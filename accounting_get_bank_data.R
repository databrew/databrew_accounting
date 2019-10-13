library(RSelenium)
library(yaml)
library(tidyverse)
library(XML)

# Webscraping currently on hold (too many website changes, etc.)
# Instead, getting data manually from websites and populating data/bank_value.csv manually

get_bank_money <- function(){
  cad <- read_csv('data/bank_value_cad.csv')  
  usd <- read_csv('data/bank_value_usd.csv')
  # Combine
  bank_value <- 
    bind_rows(cad %>% mutate(currency = 'CAD'),
              usd %>% mutate(currency = 'USD'))
  # Get a left side (for filling)
  left <- expand.grid(currency = unique(sort(bank_value$currency)),
                      date = seq(min(bank_value$date),
                                 max(bank_value$date),
                                 by = 1))
  # Join
  joined <- left_join(left, bank_value) %>%
    arrange(currency)
  
  # Forward fill values
  joined <- joined %>%
    group_by(currency) %>%
    tidyr::fill(value, .direction = 'down')
  # Fill NAs (ie, prior to any money) with 0
  joined$value[is.na(joined$value)] <- 0
  return(joined)
}
  

# # Define function for getting CAD bank account info
# get_cad <- function(){
#   
#   # Wrap the whole attempt in a try statement, so that it doesn't fail even if webscraping goes wrong
#   tried <- try({
#     # Read credentials
#     credentials <- yaml.load_file('credentials.yaml')
#     
#     # start a chrome browser
#     # (first run install_chromedriver.sh)
#     system('sudo docker run -d -p 4445:4444 selenium/standalone-chrome')
#     
#     remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
#     
#     # rD <- rsDriver()
#     # remDr <- rD[["client"]]
#     # remDr <- remoteDriver(browserName = "phantomjs")
#     # iconv <- base::iconv
#     
#     remDr$open()
#     # navigate to royal bank
#     remDr$navigate("https://www1.royalbank.com/cgi-bin/rbaccess/rbcgi3m01?F6=1&F7=IB&F21=IB&F22=IB&REQUEST=ClientSignin&LANGUAGE=ENGLISH&_ga=2.44854951.2041350902.1517547498-877209576.1517547498")
#     
#     # Identify where to enter user/pass and submit button
#     username_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "K1")]')
#     password_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "Q1")]') 
#     submit <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "yellowBtnLarge", " " ))]')
#     
#     username_entry$sendKeysToElement(list(credentials$card))
#     password_entry$sendKeysToElement(list(credentials$password))
#     submit$clickElement()
#     
#     # Figure out what question they are asking
#     question <- remDr$findElements(using = 'xpath', '//td')
#     question <- question[[10]]
#     question <- unlist(question$getElementText())
#     question_key <- ifelse(grepl('father', question),
#                            'father',
#                            ifelse(grepl('mother', question),
#                                   'mother',
#                                   ifelse(grepl('nephew', question),
#                                          'nephew',
#                                          NA)))
# 
#     # Identify answer field
#     answer_field <- remDr$findElement(using = 'xpath', '//*[(@id = "pvqAnswer")]')
# 
#     # Provide an answer
#     answer_field$sendKeysToElement(list(credentials[[question_key]]))
# 
#     # Identify and click the "continue" button
#     continue <- remDr$findElement(using = 'xpath', '//*[(@id = "id_btn_continue")]')
#     continue$clickElement()
#     # 
#     
#     # Find value
#     cad <- remDr$findElements(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "m3dtht", " " ))]')
#     lc <- length(cad)
#     cad_list <- list()
#     for(i in 1:lc){
#       cad_list[[i]] <- cad[[i]]$getElementText()
#     }
#     cad_list <- unlist(cad_list)
#     which_total <- which(grepl('Total', cad_list))
#     which_values <- which_total + 2
#     value <- cad_list[which_values]
#     value <- gsub(',', '', value)
#     value <- as.numeric(value)
#     value <- sum(value, na.rm = TRUE)
#     value_df <- data.frame(date = Sys.Date(),
#                            value = value)
#     
#     remDr$close()
#     # stop the selenium server
#     # rD[["server"]]$stop()
#   })
#   
#   # Read older data
#   if('bank_value.csv' %in% dir('data')){
#     old_data <- read_csv('data/bank_value.csv')
#   } else {
#     old_data <- data_frame(data = as.Date(NA),
#                            value = as.integer(NA))
#     old_data <- old_data[0,]
#   }
#   
#   
#   if(class(tried) == 'try-error'){
#     warning('---There was an error retrieving data from Royal Bank.\n---Consider retrieving manually and then updating data/bank_value.csv, or debug the web-scraping code.\n---For now, using data from ', max(old_data$date))
#     value_df <- old_data
#   } else {
#     # Combine new data with old data
#     value_df <- bind_rows(value_df, old_data)
#     value_df <- value_df %>% filter(!duplicated(date))
#     # Overwrite the old data
#     write_csv(value_df, 'data/bank_value.csv')
#   }
#   return(value_df)
# }

