
# Tidy Text Analysis : word frequencies 

# chapter 4 of Text Mining by R by Julia Silge

pacman::p_load(
  tidyverse,
  tidytext,
  stringr,
  dplyr,
  readtext, # reads textfiles 
  igraph,  # for network of bigrams
  ggraph, # visual network of bigrams
  gutenbergr # full books
)

gutenberg_metadata %>% 
  filter(title %in% c("Alice's Adventures in Wonderland","Grimms' Fairytales","Andersen's Fairytales"))

gutenberg_works(author =="Carroll, Lewis")

gutenberg_works(str_detect(author,"Carroll")) # find authors with Carroll name



fairytales_raw = gutenberg_download( c(11, 2591, 1597))

head(fairytales_raw, 50)

# replace the id number with the book titles
fairytales_raw = fairytales_raw %>% 
  mutate(gutenberg_id = recode(gutenberg_id,
                               "11" = "Alice's Adventures in Wonderland",
                               "2591"= "Grimms' Fairytales",
                               "1597" = "Andersen's Fairytales" ),
         gutenberg_id = as.factor(gutenberg_id))



# tidytext & word frequencies, 1 token per row, n-gram
# unnest_tokens function: converts text per row, unnest_tokens(df, new_column, old_column)
# automatically removes punctuation & converts to lowercase
# tokens: "characters","ngrams","sentences","lines","regrex","paragraphs","tweets"

(fairytales_tidy = fairytales_raw %>% 
    unnest_tokens(word, text))


# ---- keep sentence numbers
fairytales_raw %>% 
  unnest_tokens(sentences, text, token = "sentences") %>% 
  mutate(sent_nr = row_number()) %>% 
  unnest_tokens(word, sentences)
# -----


# ---- remove symbols like _ / - etc
# not run
# str_extract("_test words_ _hello", "[a-z]+") # extract 1st single word until space or char is found
# # not run
# str_remove_all("_test words_ _hello", "-") # remove only underscores

fairytales_tidy = fairytales_tidy %>% 
  mutate(word = str_extract(word, "[a-z]+"))


# ----------- stop words
head(stop_words) # tidytext stop_words

# use anti-join removes left-side df any rows in right side of df
fairytales_tidy = fairytales_tidy %>% 
  anti_join(stop_words)

fairytales_tidy


# ===== make your own stop words list
# tibble is simple df
useless_words = tibble(word = c("wacka","SMH","der","thou"))

fairytales_tidy = fairytales_tidy %>% 
  anti_join(useless_words)


# ----- stop words in other languages
# library(stopwords)
# stop_german = data.frame(word = stopwords::stopwords("de"), stringsAsFactors = F)
# head(stop_german)
# ------------------------------------



# ================= word analysis, word frequencies
# use count(), tidy data, sort= TRUE

fairytales_freq = fairytales_tidy %>% 
  group_by(gutenberg_id) %>% # keep books separated
  count(word, sort=T) 

head(fairytales_freq, 10)


#  can use filter for a conditional search
# fairytales_tidy %>% 
#   group_by(gutenberg_id) %>% 
#   count(word, sort=T) %>% 
#   filter(gutenberg_id == "Grimm's Fairytales") # look at only 1 book's word counts



# ===================== plot word freq with bar charts
library(ggplot2)

fairytales_freq %>% 
  filter(n > 90 & gutenberg_id =="Grimms' Fairytales") %>%
  ggplot( aes(x= n, 
              y= reorder(word, n),  # order in descending 
              fill=n)) +
  geom_col(show.legend = F) +
  labs(
    x= "Word",
    y= "Frequency in Grimm's Fairytales",
    title = "Word Frequencies of Grimm's Fairytales book"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))




# ----- Term Frequency for document comparisons, TF-IDF denotes which words
# are particularly strongly represented in 1 text vs another
#  low  TF-IDF == word is in many books
#  high TF-IDF == word is in few books

fairytales_idf = fairytales_freq %>% 
  bind_tf_idf(word, gutenberg_id, n)

# view(fairytales_idf)

# words that are distinctive (high tf_idf value) in each book (compared the other 2 books)
fairytales_idf %>% 
  select(gutenberg_id, word, tf_idf) %>% 
  arrange(desc(tf_idf)) # top number 

# do word tf_idf comparisons {depending on books/ sections/ chapters}
fairytales_idf %>% 
  select(gutenberg_id, word, tf_idf) %>% 
  filter(word %in% c("rabbit","mother","tea"))

# ------- plot 
fairytales_idf$word = as.factor(fairytales_idf$word)

fairytales_idf %>% 
  group_by(gutenberg_id) %>% 
  arrange( desc(tf_idf)) %>% 
  top_n(20, tf_idf) %>% 
  ggplot( aes(x= tf_idf,
              y= reorder(word, tf_idf), fill= gutenberg_id)) +
  geom_col(show.legend = F) +
  labs(x= NULL,
       y= "tf_idf") +
  facet_wrap(~gutenberg_id, # facet_wrap is what splits the books up in plot
             scales = "free") +
  theme_minimal()








# ======================
# ---------------------- n_grams, multi-words
# cumsum is a counter +=1
alice = fairytales_raw %>%
  filter(gutenberg_id == "Alice's Adventures in Wonderland") %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", # find digit Roman numerals
                                                 ignore_case = T)))) %>% 
  select(-gutenberg_id) %>% 
  filter(chapter != 0) %>% 
  mutate(chapter = as_factor(chapter),
         text = str_remove_all(text, "_"))

alice %>% 
  select(text, chapter)



# ---- n grams, unnest_tokens(), bigrams  n=2
(alice_bigrams = alice %>% 
    #            new_col  df_col, multiwords, word_length=
    unnest_tokens(bigram, text, token = "ngrams", n= 2))

# have 4-grams
alice %>% 
  #            new_col  df_col, multiwords, word_length=
  unnest_tokens("4-gram", text, token = "ngrams", n= 4)




# -------- counting n-grams
alice_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))

# -- or
alice_bigrams %>% count(bigram, sort = T)




#----- NAs most common, remove them
alice_bigrams = alice_bigrams %>% 
  drop_na(bigram)

alice_bigrams %>% 
  count(bigram) %>% 
  arrange(desc(n))


# ---- bigrams have stop words
# need to separate the bigrams into single words
(alice_bigrams = alice_bigrams %>% 
    separate(col = bigram,
             into = c("word1","word2"),
             sep = " ",
             remove = F)) # FALSE keeps the original column

# -- filter the words of stop words
(alice_bigrams_stop = alice_bigrams %>% 
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word))

# --- count the filtered words
alice_bigrams_stop %>% 
  count(bigram, sort = T)

# -- plot the filtered bigrams
alice_bigrams_stop %>% 
  count(bigram, sort = T) %>% 
  filter(n > 4) %>% 
  ggplot( aes(x= reorder(bigram, n),
              y= n,
              fill= n)) +
  geom_col(show.legend = F) +
  labs(x= NULL,
       y="Frequency",
       title = "Most frequent bigrams in Alice's Adeventures in Wonderland") +
  coord_flip() +
  theme_minimal()


# ---- search specific bigrams, extact type matching
alice_bigrams_stop %>% 
  filter(word1 == "alice" | word2 == "alice") %>% 
  count(bigram, sort = T)
  # distinct(bigram) 
  
# ---- str_detect() for variations of word
alice_bigrams_stop %>% 
  filter(str_detect(bigram, "alice")) %>% 
  distinct(bigram)


# ------ tf_idf n-grams, bigrams on different chapters
(alice_bigram_tfidf = alice_bigrams_stop %>% 
    count(chapter, bigram) %>% 
    bind_tf_idf(bigram, chapter, n))

alice_bigram_tfidf %>% 
  arrange( desc(tf_idf))

#  a high tf_idf value is per chapter, mock turtle occurs in chapter 9 and 10

# ----- plot
alice_bigram_tfidf %>% 
  group_by(chapter) %>% 
  top_n(tf_idf, n=3) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x= tf_idf,
      y= fct_reorder(bigram, tf_idf),
      fill= chapter) +
  geom_col(show.legend = F)+
  facet_wrap(~chapter, scales = "free")+
  labs(x="tf_idf", y=NULL) +
  theme_minimal()



# =====================
# --- bigrams network, words that commonly co-occur in book

# -- step 1
alice_graph = alice_bigrams_stop %>% 
  count(word1, word2) %>%  # need words separated
  filter(n>3) %>% 
  graph_from_data_frame()

alice_graph

# -- step 2
set.seed(2021)
ggraph(alice_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point()+
  geom_node_text( aes(label= name), vjust=1, hjust=1)

# -- clean it up, add arrows 'a'
a = grid::arrow(type = "closed", length = unit(.20,"inches"))

ggraph(alice_graph, layout = "fr") +
  geom_edge_link( aes(edge_alpha= n), 
                  show.legend = F, 
                  arrow = a, 
                  end_cap= circle(0.03,"inches")) +
  geom_node_point( color= "#00cc99", size= 2)+
  geom_node_text( aes(label= name), vjust=1, hjust=1) +
  theme_void()+
  labs(title = "Bigrams in Alice\'s Adventures in Wonderland")





# ====================
# Exercise: bigrams of singer's song lyrics per album  GitHub: tidytuesday

















