library(RSelenium)
library(rvest)
library(tibble)
library(dplyr)
library(keyring)

password <- key_set('password')
# cleaning session in selenium, gc - garbage collection
rm(rD)
gc()

# https://chromedriver.chromium.org/downloads - version of chrome
rD <- rsDriver(verbose = F, chromever = '79.0.3945.36', check = T)
remDr <- rD$client

#maximize window size (full screen)
remDr$maxWindowSize()
remDr$navigate(paste('https://www.filmweb.pl/user/', user, '/films', sep = ''))
Sys.sleep(10)

# rodo button
button <- remDr$findElement(using = 'css'
  , "[class = 'fwBtn fwBtn--gold']"
)

button$clickElement()

#login button

fb_log <- remDr$findElement(using = 'css'
  , "[class = 'facebookLoginButton FacebookLoginButton']"
)

fb_log$clickElement()

fb_mail <- remDr$findElements(using = 'css'
  , "[class = 'inputtext _55r1 inputtext _1kbt inputtext _1kbt']"
)

fb_mail[[1]]$sendKeysToElement(list(login))
fb_mail[[2]]$sendKeysToElement(list(key_get('password')))

fb_log <- remDr$findElement(using = 'css'
  , "[class = '_42ft _4jy0 _52e0 _4jy6 _4jy1 selected _51sy']"
)

fb_log$clickElement()

max_page_el <- remDr$findElement(using = 'css'
                        , "[class = 'section userVotesPage userVotesPage--userVotes __Filters ribbonsContainer page__section isDefault']"
)

max_page <- as.numeric(unlist(max_page_el$getElementAttribute('data-pages-count')))

# jeszcze dodac date do pobrania, no i dodac sys.stop przed rodo

download_data = tibble()

for(i in 1 : max_page){
  paste('https://www.filmweb.pl/user/', user, '/films?page=', i, sep = "") %>%
    remDr$navigate()
  titles <- remDr$findElements(using = 'css', "[class='filmPreview__title']")
  dates <- remDr$findElements(using = 'class', 'voteCommentBox__date')
  user_rates <- remDr$findElements(using = 'class', 'userRate__rate')
  avg_rates <- remDr$findElements(using = 'class', 'rateBox__rate')
  # function findElements in JS is asynchronic 
  Sys.sleep(3)
  rows_with_data <- nrow(download_data)
  
  for(j in (rows_with_data + 1) : (rows_with_data + length(titles))){
    if (j %% 25 != 0) {
      index = j %% 25
    } else {
      index = 25
    }
    download_data[j, 1] = unlist(titles[[index]]$getElementText())
    download_data[j, 2] = unlist(dates[[index]]$getElementText())
    download_data[j, 3] = unlist(user_rates[[index]]$getElementText())
    download_data[j, 4] = unlist(avg_rates[[index]]$getElementText())%>%
      sub(',', '.', .)%>%
      as.numeric()
  }
}

download_data <- rename(download_data, 'title' = 'V1', 'date' = 'V2' ,  'rate' = 'V3',  'average_rate' = 'V4')

file_name <-  paste('Output/', user, '.csv', sep = '')

if (file.exists(file_name)){
  file.remove(file_name)
}

write.csv2(download_data, file = file_name)

