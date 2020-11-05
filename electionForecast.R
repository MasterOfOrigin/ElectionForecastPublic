library(rvest)
library(tidyverse)
library(dplyr)
library(RSelenium)
library(magrittr)

output <- file("/Users/mikemahon/Documents/projects/election-forecast/output", "a")

## Not run:
# start a chrome browser
rD <- rsDriver(chromever = "86.0.4240.22")
remDr <- rD[["client"]]
remDr$open()

states = c("georgia", "pennsylvania", "nevada", "arizona", "north-carolina")
projectState <- function(state) {
  remDr$navigate(paste0("https://www.nbcnews.com/politics/2020-elections/", state, "-president-results"))
  Sys.sleep(1)
  buttons <- remDr$findElements(value = '//div[@class="jsx-787784943 toggle-button"]//button[@class="jsx-1765211304 dib button-press founders-cond lh-none f4 fw6 ttu clear-blue bg-white w-100 pv2"]')
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
  
  
  trump <-   table_nodes[ifelse(bidenFirst, 2, 1)] %>% 
    html_nodes(".jsx-4268314568") %>%
    html_nodes(".jsx-3437879980") %>%
    html_text() %>%
    lapply(function(x) {
      as.numeric(gsub(",", "", x))
    }) %>%
    unlist()
  
  
  biden <- table_nodes[ifelse(bidenFirst, 1, 2)] %>% 
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
      ifelse(pct == 0, 0, ifelse(pct == 99, 1, 100/pct))
    }) %>%
    unlist()
  
  vm <- rbind(trump, biden)
  proj <- vm %*% scalefac
  facOnes <- c(1, rep(1,length(scalefac) - 1))
  curr <- vm %*% facOnes
  
  csc <- colSums(curr)
  csp <- colSums(proj)
  remainingVotes <- csp - csc
  winTotal <- csp/2
  neededVotes <- winTotal - curr
  projectedUncounted <- proj - curr
  needed_split <- neededVotes/remainingVotes
  proj_split <- projectedUncounted/remainingVotes
  
  
  return(cbind(curr, proj, needed_split, proj_split))
}

m <- map(states, projectState)
df <- data.frame(m)
projectedVotes <- select(df, seq(2, length(states)*4, 4))
currentVotes <- select(df, seq(1, length(states)*4, 4))
colnames(projectedVotes) <- states
colnames(currentVotes) <- states
ratios <- select(df, -c(seq(1, length(states)*4, 4), seq(2, length(states)*4, 4)))
colnames(ratios) <- unlist(map(seq(1:(length(states)*2)), 
                               function(idx){
                                 ifelse(idx %% 2 != 0, paste(states[ceiling(idx/2)], "needed"), paste(states[idx/2], 'projected'))
                                 }
                               ))

remDr$close()
# stop the selenium server
rD[["server"]]$stop()


write(c("\n", "\n", paste("Date: ", Sys.Date()), paste("Time: ", Sys.time()), "", "current number of votes:"), output, append=TRUE)
write.table(currentVotes, output, append=TRUE)
write(c("\n", "projected number of votes:"), output, append=TRUE)
write.table(projectedVotes, output, append=TRUE)
write(c("\n", "projection vs needed to win ratios of remaining votes:"), output, append=TRUE)
write.table(ratios, output, append=TRUE)
write(c("\n", "\n"), output, append=TRUE)
close(output)

ratios

remainingVotesSplit <- ratios %>%
  mutate_all(function(n) {
    paste0(round(n*100, digits=2),"%")
  }) %>%
  set_rownames(rownames(ratios))

currentVotes
projectedVotes
remainingVotesSplit
colSums(projectedVotes) - colSums(currentVotes)
diff(as.matrix(projectedVotes))

currentVotes %>% knitr::kable()
projectedVotes %>% knitr::kable()
diff(as.matrix(projectedVotes)) %>% knitr::kable()
remainingVotesSplit %>% knitr::kable()
(colSums(projectedVotes) - colSums(currentVotes)) %>% knitr::kable()
