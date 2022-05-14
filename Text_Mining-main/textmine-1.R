
# ----------------
# Text Mining by Julia Silge & David Robinson
# ---------------


#  TIDY TEXT = 
# each var is a column, each observation is a row, each type of observation is a table

# token = single word, n-gram, sentence, or paragraph.

# tibble is a modern class of data frame


# ================ libraries
library(dplyr)
library(tidyr)
library(tidytext)
data("stop_words")
library(stringr)
library(scales)
library(janeaustenr) # all Jane Austen's books
library(gutenbergr)  # for all free ebooks on Gutenberg Press
library(ggplot2)
library(wordcloud)
library(forcats)
library(igraph)
library(ggraph)
library(topicmodels)
# ================


Text = c("Warp engines are offline.",
         "The plasma manifold is overheated again.",
         "Quark\'s bar is chaotic with Klingons.",
         "3 Klingons are in custody for fighting on the Promenade.")
Text 

#--use dplyr() to make the Text into a dataframe 

text_df = tibble(line = 1:4, WordText= Text)

text_df

# We need to convert WordText to one-token-per-document-per-row.
# need to use unnest_tokens( OUT_column_Name, IN_column_Name)
# 1 token == 1 word

text_df %>% 
  unnest_tokens(line, WordText)






# ------------------------
#  Jane Austen books
JA_books = austen_books() %>% 
  group_by(book) %>% 
  mutate(line_number = row_number(),
         chapter = cumsum(str_detect(text,  # cumulative sum function
                                     regex("^chapter [\\divxlc]", # Regular Expression, removes Roman Numerals
                                           ignore_case = TRUE)))) %>% 
  ungroup()

JA_books

# -- now need to tokenize words with unnest_tokens()
#    for 1-word-per-row format

tidy_Jane_books = JA_books %>% 
  unnest_tokens(word, text)

tidy_Jane_books

# REMOVE STOP WORDS from the data(stop_words)
# anti_join(stop_words)

tidy_Jane_books = tidy_Jane_books %>% 
  anti_join(stop_words)

tidy_Jane_books
# was: 'sense and sensibility by'
# now: 'sense sensibility jane'

# - now we want to count the words in all books
tidy_Jane_books %>% 
  count(word, sort = TRUE)

# - plot the word counts
tidy_Jane_books %>% 
  count(word, sort = T) %>%  # sort= TRUE
  filter(n > 600) %>%  # get only word counts higher than 600
  mutate(word = reorder(word, n)) %>%  # reorder by word counts, not alphabetically (default)
  ggplot( aes(n, word)) +
  geom_col() +
  labs(y= NULL, title = "Text Mining: Jane Austen books",
       subtitle = "word counts greater than 600")







#------ Gutenbergr package   
# 27827 - Kama Sutra
# 36 - War of the worlds
# 10 - Bible

# HG Wells books:
  # The Time Machine, The War of the Worlds, 
  # The Invisible Man, and The Island of Doctor Moreau. 

HGWells_books = gutenberg_download( c(35,36,5230,159))

tidy_HGWells = HGWells_books %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

# most common words / word frequencies
tidy_HGWells %>% 
  count(word, sort = T) %>% 
  filter(n > 200) %>%  # get only word counts higher than 600
  mutate(word = reorder(word, n)) %>%  # reorder by word counts, not alphabetically (default)
  ggplot( aes(n, word)) +
  geom_col() +
  labs(y= NULL)


# -----

kama_sutra = gutenberg_download(27827)
tidy_kama_sutra = kama_sutra %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = T)


tidy_kama_sutra %>% 
  filter(n > 100) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot( aes(x= n, y= word)) +
  geom_col(show.legend = F, fill="#ff4d94") +
  scale_fill_manual(values = mycolors) +
  labs(y= NULL) + theme(plot.subtitle = element_text(size = 11,
    colour = "white"), plot.caption = element_text(size = 11,
    colour = "white", hjust = 0), axis.line = element_line(colour = "gray20",
    linetype = "solid"), panel.grid.major = element_line(colour = "white",
    linetype = "blank"), panel.grid.minor = element_line(colour = "white",
    linetype = "blank"), axis.title = element_text(size = 13,
    hjust = 0), axis.text = element_text(size = 12,
    colour = "gray"), plot.title = element_text(colour = "white"),
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black",
        colour = NA)) +labs(title = "Word Frequencies of the Kama Sutra ",
    subtitle = "Kama Sutra from Gutenbergr R package",
    caption = "Made by : Zane Dax")
# 






# total word counts for 3 books

word_freq = bind_rows(
  mutate(tidy_Jane_books, author="Jane Austen"),
  mutate(tidy_HGWells, author= "HG Wells"),
  mutate(tidy_kama_sutra, author= "Kama Sutra")) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% 
  count(author, word) %>% 
  group_by(author) %>% 
  mutate(proportion = n /sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion) %>% 
  pivot_longer("Jane Austen": "Kama Sutra", names_to = "author", values_to = "proportion")

word_freq








# =============================== SENTIMENT ANALYSIS

# unigrams (words)
# sentiment (pos/neg)
# emotions (sad, happy, etc)

# bing, nrc packages in tidytext()

get_sentiments("bing")
get_sentiments("nrc") 

# find 'joy' words in the nrc, filter
nrc_joy = get_sentiments('nrc') %>% 
  filter(sentiment == 'joy')
nrc_joy

# find the words with joy association the Jane Austen book
tidy_Jane_books %>% 
  filter(book == "Emma") %>% 
  inner_join(nrc_joy) %>% 
  count(word, sort = T)



jane_austen_sentiment <- tidy_Jane_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = line_number %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# look at pos/neg sentiment
get_sentiments('nrc') %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  count(sentiment )

#  sentiment     n
#  <chr>      <int>
# 1 negative   3324
# 2 positive   2312

get_sentiments('bing') %>% 
  count(sentiment)

#  sentiment     n
#   <chr>     <int>
# 1 negative   4781
# 2 positive   2005


# Most common positive and negative words
bing_word_counts = tidy_Jane_books %>% 
  inner_join(get_sentiments('bing')) %>% 
  count(word, sentiment, sort=T) %>% 
  ungroup()

bing_word_counts

# Words that contribute to positive and negative sentiment in Jane Austen’s novels
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

#  add custom stop_word 'miss' as it is tagged negative but just a woman title
custom_stop_words <- bind_rows(tibble(word = c("miss"),  lexicon = c("custom")), stop_words)

head(custom_stop_words)





# =================== WORDCLOUDS 

tidy_Jane_books %>% 
  anti_join(custom_stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))






# ================== term frequency (tf), inverse document freq (idf)
# idf increase weights for less common words
# idf measures how important a word is to a document or corpus


book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n)) 

book_words <- left_join(book_words, total_words)

book_words

# Term frequency distribution in Jane Austen’s novels
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")



# Zipf’s law states that the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

freq_by_rank
freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

book_tf_idf <- book_words %>%
  bind_tf_idf(word, book, n)

book_tf_idf

#  0 for very common words
book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)





# -------- physics books
library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 30155), 
                              meta_fields = "author")

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

physics_words

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan", 
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

plot_physics %>% 
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free")

# what is the k ?
physics %>% 
  filter(str_detect(text, "_k_")) %>% 
  select(text)

physics %>% 
  filter(str_detect(text, "RC")) %>% 
  select(text)

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn", 
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords, 
                           by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>% 
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()





# --------------- tokenizing n-gram

library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Counting and filtering n-grams
austen_bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
#  names (whether first and last or with a salutation) are the most common pairs in Jane Austen books.

# “separate/filter/count/unite” let us find the most common bigrams not containing stop-words.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% # trigrams
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


# 4.1.2 Analyzing bigrams
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


# bigrams to provide context in sentiment analysis
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE) 

AFINN <- get_sentiments("afinn")
AFINN

# examine the most frequent words that were preceded by “not” 
# and were associated with a sentiment.
not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words


#  word with a value of +3 occurring 10 times has as much impact 
#  as a word with a sentiment value of +1 occurring 30 times
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# pick four common words (or more) that negate the subsequent term
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)





# ----------------- ggraph network 
# from: node an edge is from
# to: node edge is going to
# weight: numeric value with each edge

# library(igraph)
# make igraph object from tidy data is the graph_from_data_frame() function, 
# which takes a data frame of edges with columns for “from”, “to”, and 
# edge attributes (in this case n)

bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# library(ggraph)
set.seed(2021)

# layout = fr, kk, lgl 
# layout = "linear", circular=T
#          

ggraph(bigram_graph, layout = "fr") + #
  geom_edge_link() +
  geom_node_point(show.legend = F) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)



set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()









# -------------
set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()











# ========================================= TOPIC MODELING 

# Topic modeling is a method for unsupervised classification 
# of such documents, similar to clustering on numeric data, 
# which finds natural groups of items 

# Latent Dirichlet allocation (LDA) is a particularly popular method for 
# fitting a topic model. It treats each document as a mixture of topics, 
# and each topic as a mixture of words.

# library(topicmodels)

data("AssociatedPress")
AssociatedPress

#  can use the LDA() function from the topicmodels package, 
#  setting k = 2, to create a two-topic LDA model.
AP_lda = LDA(AssociatedPress, k=2, control = list(seed=1234))
AP_lda

# rest of the analysis will involve exploring and interpreting the model

#------------- Word-topic probabilities

# library(tidytext)
AP_topics = tidy(AP_lda, matrix="beta")
AP_topics

# the term “aaron”: 
# has a 1.69 x 10^-12 probability of being generated from topic 1
# has a 3.90 x 10^-5 probability of being generated from topic 2

# use dplyr’s slice_max() to find the 10 terms that are most common within each topic.

# library(dplyr)
# library(ggplot2)

AP_top_terms = AP_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n=10) %>% # slice rows by integer value
  ungroup() %>% 
  arrange(topic, desc(beta) )
AP_top_terms

AP_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot( aes(x= beta, y=term, fill= factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered()


tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))





# --------------- 4 books organized only by unlabeled chapters
# library(gutenbergr)
book.titles = c("Twenty Thousand Leagues under the Sea", 
                "The War of the Worlds",
                "Pride and Prejudice", 
                "Great Expectations")
books = gutenberg_works(title %in% book.titles) %>% 
  gutenberg_download(meta_fields = "title")

# head(books)
# tail(books)

# unnest_tokens() to separate them into words, then remove stop_words

# library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(
    text, regex("^chapter ", ignore_case = TRUE)
  ))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# --- LDA on chapters, Document Term Matrix
chapters.dtm = word_counts %>% 
  cast_dtm(document, word, n)

chapters.dtm

chapters.lda = LDA(chapters.dtm, k=4, control = list(seed=1234))
chapters.lda

# per topic probabilities
chapter_topics = tidy(chapters.lda, matrix="beta")
chapter_topics

# group by topic , slice_max
top_words = chapter_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n=5) %>% 
  ungroup() %>% 
  arrange(topic, desc(beta))
top_words


top_words %>% 
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# topic of “captain”, “nautilus”, “sea”, and “nemo” belongs to Twenty Thousand Leagues Under the Sea

chapters_gamma <- tidy(chapters.lda, matrix = "gamma")
chapters_gamma


chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))



chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  slice_max(gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  slice_max(n, n = 1) %>% 
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

assignments <- augment(chapters.lda, data = chapters.dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), 
           sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments


library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")






wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))


word_counts %>%
  filter(word == "flopson")









# =======================



































