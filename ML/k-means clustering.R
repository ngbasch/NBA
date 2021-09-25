# Helper packages
library(dplyr)       # for data manipulation
library(ggplot2)     # for data visualization
library(stringr)     # for string functionality

# Modeling packages
library(cluster)     # for general clustering algorithms
library(factoextra)  # for visualizing cluster results


# Full ames data set --> recode ordinal variables to numeric
ames_full <- AmesHousing::make_ames() %>%
  mutate_if(str_detect(names(.), 'Qual|Cond|QC|Qu'), as.numeric)

reg_2020 <- reg_data%>%
  ungroup()%>%
  na.omit()%>%
  filter(yearSeason == 2020)



# One-hot encode --> retain only the features and not sale price
full_rank  <- caret::dummyVars(f, data = reg_2020, 
                               fullRank = TRUE)
ames_1hot <- predict(full_rank, reg_2020)

# Scale data
ames_1hot_scaled <- scale(ames_1hot)

# New dimensions
dim(ames_1hot_scaled)
## [1] 2930  240

fviz_nbclust(
  ames_1hot_scaled, 
  kmeans, 
  method = "wss", 
  k.max = 25, 
  verbose = FALSE
)


fviz_cluster(kmeans(ames_1hot_scaled, 5, nstart=10),
             ames_1hot, ellipse.type = "norm")


# Original data minus Sale_Price
ames_full <- reg_2020%>% select(-win)

# Compute Gower distance for original data
gower_dst <- daisy(ames_full, metric = "gower")


