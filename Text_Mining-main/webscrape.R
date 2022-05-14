
# WEB SCRAPING by DataSlice YouTube

# SelectorGadget for Chrome: https://chrome.google.com/webstore/de...

library(rvest)
library(dplyr)

link = "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc"
page = read_html(link)

# html_nodes takes html tag elements
# html_text extracts text from selected nodes
header = page %>% html_nodes(".header") %>%  html_text()
header

name = page %>% html_nodes(".lister-item-header a") %>% html_text()
name

year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
year

rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
rating

synopsis =  page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
synopsis

# make a df
# movies = data.frame(name, year, rating, synopsis, stringsAsFactors = FALSE )
# movies

# save as CSV
# write.csv(movies, "imdb_movies.csv")


# ======================= nested links that have further info 
movie_links = page %>% html_nodes(".lister-item-header a") %>%
  html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
movie_links

#  function loop to get all cast members
get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)

movies = data.frame(name, year, rating, synopsis, cast, stringsAsFactors = FALSE)
# ======================



# ========================= multiple pages and the whole code
library(rvest)
library(dplyr)

get_cast = function(movie_link) {
  movie_page = read_html(movie_link)
  movie_cast = movie_page %>% html_nodes(".primary_photo+ td a") %>%
    html_text() %>% paste(collapse = ",")
  return(movie_cast)
}

movies = data.frame()

for (page_result in seq(from = 1, to = 51, by = 50)) {
  link = paste0("https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=adventure&sort=user_rating,desc&start=", 
                page_result, "&ref_=adv_nxt")
  page = read_html(link)
  
  name = page %>% html_nodes(".lister-item-header a") %>% html_text()
  movie_links = page %>% html_nodes(".lister-item-header a") %>%
    html_attr("href") %>% paste("https://www.imdb.com", ., sep="")
  year = page %>% html_nodes(".text-muted.unbold") %>% html_text()
  rating = page %>% html_nodes(".ratings-imdb-rating strong") %>% html_text()
  synopsis = page %>% html_nodes(".ratings-bar+ .text-muted") %>% html_text()
  cast = sapply(movie_links, FUN = get_cast, USE.NAMES = FALSE)
  
  
  movies = rbind(movies, data.frame(name, year, rating, synopsis, cast, stringsAsFactors = FALSE))
  
  print(paste("Page:", page_result))  
}




















