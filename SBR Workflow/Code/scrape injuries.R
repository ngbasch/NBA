library(rvest)
library(dplyr)
library(lubridate)
library(stringr)

AppendDateSuffix <- function(dates){
  dayy <- day(dates)
  month_name<-month.name[month(dates)]%>%tolower()
  year<-year(dates)
  
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(month_name,"-",dayy, suff,"-", year)
}

GetInjuryReport<- function(dates = Sys.Date()){
  print(dates)
  
  date_input<-AppendDateSuffix(dates)
  tbls<-tryCatch(read_html(paste0("https://www.rotoballer.com/daily-nba-injury-roundup-for-",date_input))%>%
                   html_nodes("table")%>%html_table()%>%.[[1]],
                 error = function(e){ data.frame("name" = NA, "status"=NA,"body"=NA,"team"=NA, "opponent"=NA, "home_away"=NA,"notes"=NA)})
  

  tbls_clean<-tbls%>%
    clean_names()%>%
    mutate(body = str_extract_all(status, "\\([^()]+\\)"),
           body = gsub("[()]", "", body),
           status = str_replace(status, " \\(.*\\)", "")%>%trimws(),
           date = dates)%>%
    select(date, name, status, body, team, opponent, home_away, notes)
  
  
  return(tbls_clean)
}

#dates_input<-combined%>%filter(yearSeason == 2020)%>%pull(dateGame)%>%unique()
#season_injuries<-lapply(dates_input,GetInjuryReport)                                               

#df<-bind_rows(season_injuries)

# ------------------------------------------------------------------------
#save(df, file = "Intermediate/2020_injuries.Rda")
