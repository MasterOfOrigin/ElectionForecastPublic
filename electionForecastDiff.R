library(rvest)
library(tidyverse)
library(dplyr)
library(RSelenium)
library(magrittr)

## Not run:
# start a chrome browser
rD <- rsDriver(chromever = "86.0.4240.22")
remDr <- rD[["client"]]
remDr$open()

states = c("north-carolina", "georgia", "pennsylvania", "nevada", "arizona")


projectState <- function(state) {
  remDr$navigate(paste0("https://www.nbcnews.com/politics/2020-elections/", state, "-president-results"))
                 
                 buttons <- remDr$findElements(value = '//main//div[@class="jsx-2521325328 page-content relative"]//div[@class="jsx-787784943 toggle-button"]//button')
                 if (length(buttons) > 0) {
                   buttons[[1]]$clickElement()
                 }
                 
                 contentPage <- remDr$getPageSource()[[1]] 
                 
                 html <- read_html(contentPage)
                 
                 table_nodes <- html %>%
                   html_nodes(xpath='//div[@data-testid="candidate-column"]')
                 `[`(1:2)
                 
                 bidenFirst <- table_nodes[1] %>%
                   html_nodes(".party-block") %>%
                   html_text() %>%
                   unlist() %>%
                   lapply(function(text) {
                     text == "dem"
                   }) %>%
                   extract2(1)
                 
                 
                 trumpvotes <-   table_nodes[ifelse(bidenFirst, 2, 1)] %>% 
                   html_nodes(".jsx-4268314568") %>%
                   html_nodes(".jsx-3437879980") %>%
                   html_text() %>%
                   lapply(function(x) {
                     as.numeric(gsub(",", "", x))
                   }) %>%
                   unlist()
                 
                 
                 bidenvotes <- table_nodes[ifelse(bidenFirst, 1, 2)] %>% 
                   html_nodes(".jsx-4268314568") %>%
                   html_nodes(".jsx-3437879980") %>%
                   html_text() %>%
                   lapply(function(x) {
                     as.numeric(gsub(",", "", x))
                   }) %>%
                   unlist()
                 
                 scalefac <- html %>%
                   html_nodes(".jsx-3921339373") %>%
                   html_nodes(".jsx-421939043") %>%
                   html_nodes(".jsx-998961378") %>%
                   html_text %>%
                   lapply(function(x) {
                     pct = as.numeric(gsub(",", "", sub("%", "", x)))
                     ifelse(pct == 0, 0, 100/pct)
                   }) %>%
                   unlist()
                 
                 vm <- rbind(trumpvotes, bidenvotes)
                 
                 return(vm %*% scalefac)
}

m <- map(states, projectState)
df <- data.frame(m)
colnames(df) <- states

remDr$close()
# stop the selenium server
rD[["server"]]$stop()

df_diff <- diff(as.matrix(df))
df
df_diff
df_diff/colSums(df)
