library(dotenv)
load_dot_env(file = ".env")
library(cronR)
cmd <- cron_rscript(Sys.getenv("RUN_FILE"))
cron_add(command = cmd, frequency = '*/30 * * * *', user=USER,
         id = 'efMD', description = 'electionForecastMD')
cron_njobs()

# cron_clear(ask = FALSE, user = "")
# # install.packages(c("rvest", "tidyverse", "dplyr", "RSelenium", "magrittr", "rmarkdown", "knitr", "rsconnect", "cronR")
