
# Title: Scraping
# by: Zane Dax
# Date: June 28, 2021

# Tutorial:
#   Scraping, iterating, purring
#   Amelia McNamara, PhD





# install dplyr for mutate function and others
library(dplyr)

# install palmer penguins dataframe
# find the means of 3 species in a dataframe
library(palmerpenguins)
penguins = palmerpenguins::penguins

penguins %>% 
  group_by(species) %>% 
  summarise(mean = mean(body_mass_g, na.rm=T))

# ----------------


# scrape GitHub to count the number of commits by a person

library(rvest)

roster = tibble(url = c(
  "https://github.com/AmeliaMN",
  "https://github.com/juliasilge",
  "https://github.com/evelinag",
  "https://github.com/massaraevi",
  "https://github.com/SatenikS",
  "https://github.com/spbail",
  "https://github.com/amberjrivera",
  "https://github.com/trallard",
  "https://github.com/lesley2958",
  "https://github.com/kjam",
  "https://github.com/PratheepaJ"
))

session = html_session("https://github.com")


commits = function(url, session){
  session %>% 
    jump_to(url) %>% 
    read_html() %>% 
    # use the SelectorGadget Chrome extension to find the html tags 
    html_nodes("h2.f4.text-normal.mb-2") %>% 
    html_text() %>% 
    purrr::pluck(2) %>% 
    readr::parse_number()
}


# iterate over the urls
library(purrr)

women_GitHub_commits = roster %>% 
  mutate(commits= map_dbl(url, commits, session = session )) %>% 
  arrange( desc(commits)) 

# check for class type
class(women_GitHub_commits)

# convert to dataframe
women_GitHub_commits = as.data.frame(women_GitHub_commits)

#  check it's a dataframe
class(women_GitHub_commits)

# commits column is a num class, it needs to be integer else you get a empty plot
library(forcats)
women_GitHub_commits = women_GitHub_commits %>% 
  mutate(women = gsub("https://github.com/","", url),
         Git_commits = as.integer(commits))



library(ggplot2)
ggplot(data = women_GitHub_commits)+
  geom_col(aes(x=women, y= Git_commits, fill= Git_commits)) +
  coord_flip() 
 














