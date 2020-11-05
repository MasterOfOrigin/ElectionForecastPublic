library(rmarkdown)
library(knitr)
library(rsconnect)
library(dotenv)
# url: https://rpubs.com/itsthejokahbaby/686723

title <- "2020_Election_Projection"

render(Sys.getenv("MD_FILE"), "all")

updateResult <-  rpubsUpload(title, Sys.getenv("HTML_FILE"), NULL, id = Sys.getenv("ID"))