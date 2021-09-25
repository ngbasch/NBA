### Set Environment -------------------------

# Clear the environment 
#rm(list=ls())

#Links
## https://dreamrs-vic.shinyapps.io/shinyWidgets/

# Set working directory
path <- "C:\\Users\\nated\\Documents\\Documents_NB\\Projects\\Betting\\SBR Workflow"
options(stringsAsFactors = F)
setwd(path)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               lubridate,
               dplyr,
               tidyr,
               scales,
               ggplot2,
               readxl,
               janitor,
               shiny,
               shinyWidgets,
               ggthemes,
               plotly,
               ggimage,
               magick,
               DT,
               nbastatR,
               reticulate,
               gganimate,
               ggtext
               )

#run this guy's github which allow me to make those kirk goldsberry figures (not working)
#runGitHub("ballr", "toddwschneider")



### 1. Pull recent SBR and NBA Log Data ------------------------------------------------
if (!exists("todays_info")){
  #Update odds databases with new data
  #Update NBA game logs and cleaned log dataset
  #source("Code/update and clean sbr.R")
  
  ### 2. Load and Prep data ---------------------------------------------------------
  #Load datasets
  source("Code/Load datasets.R")
  source("Code/Functions/plotting functions.R")
  
  
  #Prepare and clean historical Data for modeling
  source("Code/regression prep.R")
  
  #Pull today's games
  source_python("Code/Functions/sbrscraper_functions.py")
  source("Code/todays lines.R")
  todays_info<-GetTodaysPicks()
  
  #Pull today's injuries
  source("Code/scrape injuries.R")
  todays_injuries<-GetInjuryReport()
}

#APP -----------------------------------------------------------------------------------------------------
ui<- fluidPage(
  titlePanel("NBA Betting Visualizations"),
  tabsetPanel(type='tabs',
              tabPanel("Today's Bets",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("bet_type",
                                 "Type:",
                                 choices = c("Money Line", "Spread", "Total"),
                                 #choices = list (Batter = unique(as.character(alcorn_pd_shiny$Name[alcorn_pd_shiny$Position != "P"])),
                                 #               Pitcher = unique(as.character(alcorn_pd_shiny$Name[alcorn_pd_shiny$Position == "P"]))),
                                 selected = "Money Line"
                                 #options = list(`live-search` = TRUE,
                                 #               `actions-box` = TRUE)
                     ),
                     dateInput("date_input",
                               "Betting Date",
                                value = Sys.Date(),
                               max = Sys.Date()
                               ),
                     h4("Graphing Options:"),
                     pickerInput("graph_var",
                                 "Variable:",
                                 selected = "dif_spread",
                                 multiple = TRUE,
                                 choices = colnames(combined),
                                 options = list(`live-search` = TRUE,
                                                `actions-box` = TRUE)
                                 ),
                     numericInput("obs", "Last number of games to graph", 3),
                     #uiOutput("checkbox"),
                     width = 2
                   ),
                   mainPanel(
                     # Create a new row for the table.
                     #h4(paste0("Today's Lines: ", unique(todays_info$dateGame ))),
                     DT::dataTableOutput("todays_info_tbl"),
                     #plotOutput("today_plot", height = '600px'),
                     plotlyOutput("today_plotly", height = '600px'),
                     DT::dataTableOutput("todays_injury_tbl"),
                     
                       #Show ggplots of specific statistics with all the players as different colors. 
                       ## Allow use to choose which statistic they wnat to see (facet wrap on statistic)
                     
                     
                     #MAKE SO THAT IF A ROW IS CLICKED IN THE DT, THE FOLLOWING WILL SHOW:
                     
                     #INSERT FIGURE Below today's bet table THAT WILL BE GGPLOTLY OF TWO TEAMS MATCHUP WITH THE FOLLOWING OPTIONS
                     #Show win probabilities over an entire game of last XX games overlapped over eachother with two different colors
                     ### Representing two different teams and they've won or lost games against opponents
                     #show injuries (pulled from basketball reference) for the games today
                     #
                     # 30 points of each statistic (highlighting the 2 two teams that are playing), while being able to  filter on last X games)
                     # Show X games against eachother and what happened (win probabilities)
                     #Maybe a separate tab where you can choose any 2 teams, and then find out a bunch of info on those teams
                     
                     
                     width = 10
                     )
                   )
              ),
              tabPanel("Vegas Trends",
                       #Where has Vegas been getting games wrong recently? Will there be overcorrection? Maybe facet_wrap three bet types
                       # Look over last XX games to see if there's any autocorrelation
                       # Show a simple W-L Report of Vegas (are they generally under or over for Totals/ML)
                       
                       fluidRow(
                         column(12, plotlyOutput("plots_stats", height = '600px'))
                         #Show ggplots of specific statistics with all the players as different colors. 
                         ## Allow use to choose which statistic they wnat to see (facet wrap on statistic)
                       )
              ),
              tabPanel("Test",
                       h4("TEST"))
              # tabPanel("Live bettting",
              #          #Where has Vegas been getting games wrong recently? Will there be overcorrection? Maybe facet_wrap three bet types
              #          # Look over last XX games to see if there's any autocorrelation
              #          
              #          
              #          fluidRow(
              #            column(12, plotlyOutput("plots_stats", height = '600px'))
              #            #Show ggplots of specific statistics with all the players as different colors. 
              #            ## Allow use to choose which statistic they wnat to see (facet wrap on statistic)
              #          )
              # ),
              # tabPanel("Team In-Depth",
              #          #Be able to choose  whether you are looking at a specific team,
              #          #Filter to last XX games
              #          #Choose which betting stats you want to focus on
              #          #Split into some performance metric (game) compared to vegas prediction vs actual outcome
              #          
              #          
              #          fluidRow(
              #            column(12, plotlyOutput("plots_stats", height = '600px'))
              #            #Show ggplots of specific statistics with all the players as different colors. 
              #            ## Allow use to choose which statistic they wnat to see (facet wrap on statistic)
              #          )
              # ),
              # tabPanel("Other Season comparison",
              #          #Choose Season compared to other seasons
              #          fluidRow(
              #            column(12, plotlyOutput("outcome_plots", height = '600px'))
              #            ## GGplot sidewise bar, facet wrap on players.
              #            #column(4, DT::dataTableOutput("table"))
              #          )
              # )
              # 
  )
 )

server<-function(input, output) {
  

  #Relevant UI elements ----------------------------------------
  output$checkbox <- renderUI({
    pickerInput(inputId = "select_var", 
                    label = "Select variables", 
                    choices = list (Pitcher = pitcher_vars,
                                    Batter = batter_vars,
                                    `Player Value` = value_vars),
                    selected = c("AVG","Salary","Yearly_WAR"),
                    multiple = TRUE,
                    options = list(`live-search` = TRUE,
                                   `actions-box` = TRUE))
  })
  
  output$PCA_var <- renderUI({
    pickerInput(inputId = "select_pca", 
                label = "Color variable", 
                choices = list (Pitcher = pitcher_vars,
                                Batter = batter_vars,
                                `Player Value` = value_vars,
                                Handedness = c("throwing_hand","batting_hand")
                                ),
                selected = "batting_hand",
                multiple = FALSE,
                options = list(`live-search` = TRUE,
                               `actions-box` = TRUE))
  })
  
  
  #Set up all reactive dfs ---------------------------------------------------------

  
  todays_info_react<-eventReactive(input$date_input, {
    
    if (Sys.Date() != input$date_input){
      todays_info <- GetTodaysPicks(as.character(input$date_input))}
    
    todays_info<-
      todays_info%>%
      mutate(group_no = match(idGame,unique(idGame)))
    
    
  return(todays_info)
  })
  
  todays_injuries_react<-eventReactive(input$date_input, {
    
    if (Sys.Date() != input$date_input){
      todays_info <- GetInjuryReport(as.character(input$date_input))}
    
    todays_injuries<-
      todays_injuries%>%
      left_join(crosswalk%>%select("team" = abbrev, idTeam)%>%distinct(.keep_all=T), by = "team")    
    
    return(todays_injuries)
  })
  
  # observeEvent(input$date_input,{
  #   todays_info_react <- 
  #     GetTodaysPicks(input$date_input)%>%
  #     mutate(group_no = match(idGame,unique(idGame)))
  # })
  
  observe(print(input$date_input))
  observe(print(todays_info_react()))
  
  # Select columns to print ----
  

  # Output all Tables ----------------------------------------------------------------------------
  
  sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, 'Team'),
        th(class = 'dt-center', colspan = 2, 'Money Line'),
        th(class = 'dt-center', colspan = 3, 'Spread'),
        th(class = 'dt-center', colspan = 3, 'Total'),
      tr(
        lapply(c('Ratio', "Consesus", rep(c("Line", "Ratio", "Consensus"),2)), th))
    )
  ))
  )
  
  output$todays_info_tbl <- renderDataTable(
    
    todays_info_react()%>%FormatTodaysInfo(),
    options = list(dom = 't',pageLength = 1000, lengthChange = FALSE,
                 columnDefs = list(list(className = 'dt-center', targets = 0:5))),
    container = sketch, selection = "single", rownames= FALSE, escape = FALSE
  )
  
  output$todays_injury_tbl <- renderDataTable({
    req(input$todays_info_tbl_rows_selected)
    
    relevant_teams = todays_info_react()%>%
      filter(group_no %in% (input$todays_info_tbl_rows_selected))%>%
      pull(idTeam)
    
    df<-
      todays_injuries_react()%>%
      filter(idTeam %in% relevant_teams)%>%
      select(name, team, status, body, notes)
    
    print(df)
    
    datatable(df, 
              options = list(dom = 't',pageLength = 1000, lengthChange = FALSE),
              selection = "single", 
              rownames= FALSE)
    
    
  })
  
  plot_results_react<- reactive({
    
    req(input$todays_info_tbl_rows_selected)
    
    relevant_teams = todays_info_react()%>%
      filter(group_no %in% (input$todays_info_tbl_rows_selected))%>%
      pull(idTeam)
    
    print(input$todays_info_tbl_rows_selected)
    print(relevant_teams)
    
    plot_results<-
      combined%>%
      group_by(idGame)%>%
      mutate(nameTeam2 = if_else(home == "Away", nameTeam[home == "Home"], nameTeam[home == "Away"]),
             nameTeam2_logo = if_else(home == "Away", logos_local[home == "Home"], logos_local[home == "Away"]))%>%
      ungroup()%>%
      filter(idTeam %in% relevant_teams, yearSeason == 2020)%>%
      select(dateGame, idTeam, nameTeam, nameTeam2, nameTeam2_logo, input$graph_var)%>%
      gather(var, val, 6:length(.))
    #Make a config file to decode what each unit is
    #mutate(unit = ifelse(grepl("percent", var), ))
    
    return(plot_results)
  })
  
  
  output$today_plot <- renderPlot({

    req(input$todays_info_tbl_rows_selected)

    ggplot(plot_results_react(),
               aes(x = dateGame, y = val))+
      # text = paste0("Team: ", nameTeam, "\n",
      #               "Value: ", fg2mTeam),size = 1.5))+
      geom_line(aes(colour = nameTeam))+
      geom_image(aes(image = nameTeam2_logo, colour = nameTeam), size = 0.03,
                 #image_fun = function(.) magick::image_convert(., colorspace = "gray"))+
                 #image_fun = function(.) magick::image_colorize(., opacity = 40,color = "blue"))+
                 image_fun = function(.) TransparentImage(image_transparent(., "white"), 0.4))+
      #image_fun = function(.) TransparentImage(., 0.5))+
      geom_image(aes(image = nameTeam2_logo), size = 0.03,image_fun = function(.) TransparentImage(., 0.5))+
      #geom_point(aes(colour = nameTeam), alpha = 0.5, size = 5)+
      #facet_wrap(~stat, scales = "free")+
      #ylab ("") +
      xlab ("") +
      #scale_colour_tableau()+
      theme_bw()+
      theme(legend.title = element_blank())+
      facet_wrap(~var, scales = "free")


  })

  output$today_plotly <- renderPlotly({ 

    req(input$todays_info_tbl_rows_selected)
    
    p<- ggplot(plot_results_react(),
               aes(x = dateGame, y = val, colour = nameTeam))+
      geom_line()+
      geom_point(aes(text = paste0("Opponent: ", nameTeam2, "\n", 
                                   "Value: ", val),size = 1.5), alpha = 0.5, size = 5)+
      #facet_wrap(~stat, scales = "free")+
      #ylab ("") +
      xlab ("") +
      #scale_colour_tableau()+
      theme_bw()+
      theme(legend.title = element_blank())+
      facet_wrap(~var, scales = "free")
    
    #https://plotly-r.com/improving-ggplotly.html
    
    ggplotly(p, 
             tooltip = 'text',
             dynamicTicks = TRUE
             )%>%
      #rangeslider() %>%
      layout(hovermode = "x")
      #plotly::config(displayModeBar = F)
    
  })

  
  #Plots ---------------------------------------------------------------------------------------
  
  output$plots_stats <- renderPlotly({ 
    
    # 
    # test<-alcorn_pd_shiny%>%filter(Name == "Fernando Abad" | Name == "David Price")%>%select(permenant_columns, ERA, Salary, HR_per_inning)
    # plot_results = test%>%
    #  gather(stat, value, -permenant_columns)%>%
    #   mutate(value = gsub("[\\$,]", "",value)%>%as.numeric())%>%
    #  bind_rows(median_shiny%>%
    #  filter(data == "Pitcher",
    #        stat%in%c("ERA","Salary","HR_per_inning")))
    
    
    
    plot_results<-data_results()%>%
      bind_rows(player_data())%>%
      gather(stat, value, -permenant_columns)%>%
      mutate(value = gsub("[\\$,]", "",value)%>%as.numeric())%>%
      bind_rows(median_shiny%>%
                 filter(data == p_or_b(),
                        stat %in% input$select_var))
                

  
  p<- ggplot(plot_results, aes(x = Name, y = value, fill = Name,
                               text = paste0("Player: ", Name, "\n", 
                                             "Value: ", value),size = 1.5))+
      geom_bar(stat ="identity")+
      facet_wrap(~stat, scales = "free")+
      ylab ("") +
      xlab ("") +
      #scale_colour_tableau()+
      theme_bw()+
      scale_y_continuous(labels = FormatNumbers)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.title=element_blank())
  
  
  ggplotly(p, tooltip = 'text')%>% 
    config(displayModeBar = F)
  
    })
  
  output$outcome_plots<-renderPlotly({
    
    # data_outcomes<-
    #   df_outcome%>%filter(retro_id %in% c("abreb001","frank001","aybae001") )%>%
    #   group_by(retro_id,Name,outcome)%>%
    #   summarise(n=sum(n))%>%
    #   bind_rows(median_outcome%>%
    #               filter(data == "Pitcher"))
    
    #Could also do this as a multiple bar chart (one graph, n-player colors)
    
    data_outcomes<-
      df_outcome%>%
      filter(retro_id %in% player_data()$retro_id | 
               retro_id %in% data_results()$retro_id
      )%>%
      group_by(retro_id,name,outcome)%>%
      summarise(n=sum(n))%>%
      bind_rows(median_outcome%>%
                  filter(data == p_or_b())) 
                         
    
    p<-ggplot(data_outcomes, aes(x = outcome, y = n))+
      geom_col()+
      facet_wrap(~name)+
      coord_flip()+
      labs(x="",y="")+
      scale_colour_tableau()+
      theme_bw()

    ggplotly(p)%>% 
      config(displayModeBar = F)
    

  })
  
  output$outcome_boxplot<-renderPlotly({
    
    data_outcomes<-
      df_outcome%>%
      filter(retro_id %in% player_data()$retro_id | 
               retro_id %in% data_results()$retro_id
      )%>%
      group_by(retro_id,name,outcome)%>%
      summarise(n=sum(n))
    
    boxplot_outcome<-
      df_outcome%>%
      filter(data == p_or_b(),
             (data == "Pitcher" & position == "P")| 
               (data =="Batter" & position != "P"))
    
    p<-ggplot(boxplot_outcome,aes(x = outcome, y = n))+
      geom_boxplot()+
      geom_point(data = data_outcomes,
                 aes(x = outcome, 
                     y = n, 
                     colour = name,
                     text = paste0("Outcome: ", outcome, "\n", 
                                   "Frequency: ", n, "\n",
                                   "Player: ", name)),size = 1.5)+
      coord_flip()+
      scale_colour_tableau()+
      theme_bw()+
      theme(legend.title=element_blank())
    
    ggplotly(p, tooltip = "text")%>% 
      config(displayModeBar = F)
    
    
  })


  output$plots_nn<-renderPlotly({
    
    player_nn<-
      nn_bind%>%
      filter(stat%in%input$select_var)%>% 
      filter(retro_id %in% player_data()$retro_id)%>%
      mutate(col = "red")
    
    gg_nn<-
      nn_bind%>%
      filter(stat%in%input$select_var)%>%
      filter(data%in%p_or_b())%>%
      left_join(alcorn_pd_shiny%>%select(retro_id,Name),by = c("retro_id"))%>%
      left_join(alcorn_pd_shiny%>%select(retro_id,Name),by = c("nn" = "retro_id"))%>%
      left_join(player_nn)%>%
      mutate(col = ifelse(is.na(col),"black",col))
    
    
    
    p<-ggplot(gg_nn, 
              aes(value,value_nn, 
                  colour = col,
                  text = paste0("Player: ", Name.x, "\n", 
                                "Neighbor: ", Name.y)))+
      geom_point()+
      geom_smooth(method="lm")+
      facet_wrap(~stat, scales = "free")+
      scale_colour_tableau()+
      theme_bw()+
      scale_x_continuous(labels = FormatNumbers)+
      scale_y_continuous(labels = FormatNumbers)+
      labs (x = "Player", y = "Nearest Neighbor")+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.text=element_text(size=12),
            legend.position = "none",
            axis.title=element_text(size=12,face="bold"),
            strip.text = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = "text")%>% 
      config(displayModeBar = F)
      #layout(xaxis=list(fixedrange=TRUE)) %>% 
      #layout(yaxis=list(fixedrange=TRUE))  
    
  })

  output$pca<-renderPlotly({
    
    # gg_pca<-
    #   pca.list%>%
    #   unnest(pca_aug)%>%
    #   mutate(data = ifelse(position == "P","Pitcher","Batter"),
    #          shape_gg = ifelse(retro_id%in%"altuj001", name,"_Other")
    #   )%>%
    #   filter(data == "Batter")
    
    gg_pca<-
      pca.list%>%
      unnest(pca_aug)%>%
      mutate(data = ifelse(position == "P","Pitcher","Batter"),
             shape_gg = ifelse(retro_id%in%player_data()$retro_id |
                               retro_id%in%data_results()$retro_id, name,"Other")
             )%>%
      filter(data == p_or_b())
    
    if (input$dimens == "2D")
    {
    p<-ggplot(gg_pca, 
              aes(x = .fittedPC1, y = .fittedPC2, colour = shape_gg,
                  text = paste0("Player: ", name, "\n",
                                "Throwing Hand: ", throwing_hand, "\n",
                                "Batting Hand: ", batting_hand))
                  )+
      geom_point()+
      theme_bw()+
      labs (x = "First Principal Component", y = "Second Principal Component")+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.text=element_text(size=12),
            #legend.position = "none",
            axis.title=element_text(size=12,face="bold"),
            strip.text = element_text(size = 12)
      )
    
    
    ggplotly(p, tooltip = "text")%>% 
      config(displayModeBar = F)
    
    }else if (input$dimens == "3D")
    {
      plot_ly(gg_pca%>%mutate(lab = paste0("Player: ", name, "\n",
                                           "Throwing Hand: ", throwing_hand, "\n",
                                           "Batting Hand: ", batting_hand)),
              type = "scatter3d",
              x = ~.fittedPC1, y = ~.fittedPC2, z = ~.fittedPC3, color = ~shape_gg,
              mode = "markers", text = ~lab,
              marker = list (size = 5, opacity = 0.4))%>%
        layout(scene = list(xaxis = list(title = 'PC 1'),
                            yaxis = list(title = 'PC 2'),
                            zaxis = list(title = 'PC 3')))
    }
  

  })
  
  output$pca_2<-renderPlotly({
    
    # gg_pca<-
    #   pca.list%>%
    #   unnest(pca_aug)%>%
    #   mutate(data = ifelse(position == "P","Pitcher","Batter"),
    #          shape_gg = ifelse(retro_id%in%"altuj001", name,"_Other")
    #   )%>%
    #   filter(data == "Batter")
    
    gg_pca<-
      pca.list%>%
      unnest(pca_aug)%>%
      mutate(data = ifelse(position == "P","Pitcher","Batter"),
             shape_gg = ifelse(retro_id%in%player_data()$retro_id |
                                 retro_id%in%data_results()$retro_id, name,"_Other")
      )%>%
      filter(data == p_or_b())
    
    pca_var = input$select_pca
    

    p<-ggplot(gg_pca,
              aes(x = .fittedPC1, y = .fittedPC2, 
                  colour = eval(as.name(pca_var)), shape = shape_gg,
                  text = paste0("Player: ", name, "\n",
                                "Variable ", pca_var, "\n",
                                "Value: ", get(pca_var)))
    )+
      geom_point()+
      theme_bw()+
      labs (x = "PC 1", y = "PC 2")+
      {if(!(pca_var %in% c("batting_hand","throwing_hand"))) scale_colour_gradient(low="white", high="black", guide = FALSE)}+
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.text=element_text(size=12),
            #legend.position = "none",
            legend.title=element_blank(),
            strip.text = element_text(size = 12)
      ) 
    
    
    ggplotly(p, tooltip = "text")%>% 
      config(displayModeBar = F)
    
  })
}

runApp(list(ui = ui, server = server),launch.browser = TRUE)

