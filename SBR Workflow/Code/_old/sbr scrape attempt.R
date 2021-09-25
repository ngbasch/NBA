#SCrape from Sportsbook review
lego_movie <- read_html("https://www.sportsbookreview.com/betting-odds/nba-basketball/pointspread/?date=20191108")

scrape2<-
  lego_movie%>%
  #html_nodes(".opener")
  html_nodes("._1QEDd span")

#html_nodes(".undefined div , ._1t1eJ span , ._3IKv4 , ._2XT2S , #bettingOddsGridContainer a , ._1kega span , ._1QEDd")
html_nodes("#bettingOddsGridContainer img , .pZWkv , ._2XT2S span , ._1kega span , .undefined div , ._3ptK- , ._3qi53")

scrape2%>%html_text()

test<-scrape2%>%
  .[[5]]%>%
  .[[2]]
scrape2%>%
  .[[400]]%>%
  html_text()
html_nodes("_3Nv_7 opener")

scrape<-
  lego_movie%>%
  html_nodes("script")%>%
  .[[5]]%>%
  html_text()%>%
  gsub("window.__INITIAL_STATE__=|             .window.__config = .*|;", "",.)%>%
  trimws()
#html_node("#bettingOddsGridContainer")


test<-fromJSON(scrape)


scrape%>% html_text() 
test<-scrape%>% html_nodes("div div")%>%
  .[[1]]