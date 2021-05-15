library(DT)
library(kableExtra)
library(bsplus)
library(collapsibleTree)
library(readxl)
library(tidyverse)
library(reactable)
library(dplyr)
library(xml2)
library(rvest)
library(htmltools)
library(tidyquant)
library(lubridate)
library(apexcharter)
library(htmltools)
library(magrittr)
library(scales)
library(data.table)
library(ggplot2)
library(shiny)
library(shinySignals)   # devtools::install_github("hadley/shinySignals")
library(dplyr)
library(shinydashboard)
library(bubbles)        # devtools::install_github("jcheng5/bubbles")
library(shinythemes)
library(shinyBS)

data(mtcars)
mtdat <- mtcars[1:10,]


news <- function(term) {
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-IN&gl=IN&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Title = html_dat %>%
      html_nodes('.DY5T1d') %>% 
      html_text(),
    Link = dat$Link,
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text()
  ) 
  
  # Extract Source and Time (To avoid missing content)
  prod <- html_nodes(html_dat, ".SVJrMe")
  Source <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "a") %>% html_text() ,
                     error=function(err) {NA})
  })
  
  time <- lapply(prod, function(x) {
    norm <- tryCatch(html_node(x, "time") %>% html_text(),
                     error=function(err) {NA})
  })
  
  mydf <- data.frame(Source = do.call(rbind, Source), Time = do.call(rbind, time), stringsAsFactors = F)
  dff <- cbind(news_dat, mydf) %>% distinct(Time, .keep_all = TRUE) %>% arrange(desc(Time))
  dff[1:10,]
  return(dff)
}

ranthresh<-function(n,min_num,max_num,mean,sd){
  repeat{
    y<-rnorm(n,mean,sd)
    if (n>1 && min(y)>min_num && max(y)<max_num &&sd(y)<=sd*1.25)break
  }
  return(y)
}
format.money  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
}
format.money1  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=1, big.mark=","))
}
format.percentage<-function(x,...){
  paste0(formatC(as.numeric(x*100),format="f",digits=4,),"%")
}

newsbc <- news('bit"%20coin')
newsbc$Title<-paste("<a href='",newsbc$Link,"'>",newsbc$Title,"</a>",sep="")
newsbc<-newsbc[1:10,-2]

newsapple <- news('apple')
newsapple$Title<-paste("<a href='",newsapple$Link,"'>",newsapple$Title,"</a>",sep="")
newsapple<-newsapple[1:10,-2]
