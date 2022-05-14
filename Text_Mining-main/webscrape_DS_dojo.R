
#  webscraping - datascience dojo

# https://code.datasciencedojo.com/datasciencedojo/tutorials/blob/master/web_scraping_R-master/r_web_scraping_coded_example_share.R
# https://code.datasciencedojo.com/rebeccam/tutorials/blob/master/web_scraping_R-master/r_web_scraping_meetup_share.r

library(rvest)

# 1 - get link
website = read_html(
  "https://www.marketwatch.com/story/bitcoin-jumps-after-credit-scare-2018-10-15"
  )

# 2 - get title/header
website %>% html_nodes("title") %>% html_text()

# 3 - print out the paragraph tags
website %>%
  html_nodes("p") %>% #See HTML source code for data within this tag
  html_text()


# =====================
#  time series data

library(lubridate)

#  1- link
bitcoin = read_html("https://www.marketwatch.com/search?q=bitcoin&m=Keyword&rpp=15&mp=0&bd=false&rs=false")

# 2 - grab specific url tags 
urlz = bitcoin %>% 
  html_nodes("div.searchresult a") %>% 
  html_text("href")
urlz

# 3 - grab all the datetimes 
the_dates = bitcoin %>% 
  html_nodes("#maincontent span") %>% 
  html_text()

the_dates

# Filter datetimes that do not follow a consistent format
the_dates2 <- c()
for(i in the_dates){
  correct_datetime <- grep("Today", i, invert=T, value=T)
  the_dates2 <- append(the_dates2, correct_datetime)
}

the_dates = the_dates2
the_dates

# convert str dates to date format
# First remove periods from datetime, as lubridate 
# cannot interpret a.m. and p.m. with periods
datetime_clean <- gsub("\\.","",the_dates)
datetime_clean

dates_parsed = parse_date_time(datetime_clean,
                               "%I:%M %p %m/%d/%Y")
dates_parsed

# convert timezones to local 
dates_parsed = ymd_hms(
  dates_parsed, tz="America/Edmonton"
)
dates_parsed
# ---- or -----
dates_tz = with_tz(
  dates_parsed, "America/Edmonton"
)
dates_tz




















