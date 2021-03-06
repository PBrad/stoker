############

# EDA

############

# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gutenbergr)
library(tidytext)
library(stringr)
library(tidyr)
library(widyr)
library(wordcloud)
library(reshape2)
library(igraph)
library(ggraph)

# Load --------------------------------------------------------------------

# Find our novel
gutenberg_metadata %>% 
  filter(title == "Dracula")

# Load
dracula <- gutenberg_download(345)

# Inspect -----------------------------------------------------------------

str(dracula)

head(dracula)

tail(dracula)

# Clean Up ----------------------------------------------------------------

## Remove first 156 rows (title page, TOC) to avoid
## the TOC entries being treated as chapters

nrow(dracula)

dracula <- dracula %>% 
  filter(row_number() > 156)
  
nrow(dracula)

# Tidy --------------------------------------------------------------------

tidy_dracula <- dracula %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(
           str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word, text) %>% # one word per row
  anti_join(stop_words) # remove stop words

# Use str_extract() to remove underscores (used in
## gutenbergr to indicate emphasis). We don't want
## "_text" and "text" to be treated as two different
## words. Remove digits as well.

tidy_dracula <- tidy_dracula %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  filter(!is.na(word)) # removing any resulting NA's

# Word Frequencies --------------------------------------------------------

# Look out for "van" and "helsing"

tidy_dracula %>% 
  count(word, sort = TRUE)

tidy_dracula %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 150) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Sentiment Analysis ------------------------------------------------------

# Get change in sentiment over time
dracula_sentiment <- tidy_dracula %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber = row_number()) %>% 
  count(index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% # fill missing values with 0
  mutate(sentiment = positive - negative) %>% 
  group_by(index) 

dracula_sentiment %>% 
  ggplot(aes(index, sentiment, fill = sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  ggtitle("Dracula Sentiment Trajectory")

# Top positive and negative words

pos_neg_counts <- tidy_dracula %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup() # Looks good

pos_neg_counts %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution of Sentiment", 
       x = NULL) +
  coord_flip() + 
  theme_minimal()

## "Miss" is probably a misfire (salutation rather than a true
## negative word - at least in some cases). Add to custom stop
## words

custom_stop_words <- bind_rows(data_frame(word = c("miss"),
                                          lexicon = c("custom")),
                               stop_words)

# Wordclouds --------------------------------------------------------------

# Standard wordcloud
tidy_dracula %>% 
  anti_join(custom_stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

# Comparison cloud
tidy_dracula %>% 
  anti_join(custom_stop_words) %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  # Convert to matrix with acast() from reshape2
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# Identify most positive and negative chapters ----------------------------

bing_positive <- get_sentiments("bing") %>% 
  filter(sentiment == "positive")

bing_negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_dracula %>% # for denominator
  group_by(chapter) %>% 
  summarize(words = n())

# Most positive chapter is 5
tidy_dracula %>% 
  semi_join(bing_positive) %>% 
  group_by(chapter) %>% 
  summarize(positive_words = n()) %>% 
  left_join(wordcounts, by = "chapter") %>% 
  mutate(ratio = positive_words / words) %>% 
  filter(chapter != 0) %>% 
  arrange(desc(ratio)) %>% 
  top_n(5) %>% 
  ungroup()

tidy_dracula %>% 
  filter(chapter == 5)

# Most negative chapter is 19
tidy_dracula %>% 
  semi_join(bing_negative) %>% 
  group_by(chapter) %>% 
  summarize(negative_words = n()) %>% 
  left_join(wordcounts, by = "chapter") %>% 
  mutate(ratio = negative_words / words) %>% 
  filter(chapter != 0) %>% 
  arrange(desc(ratio)) %>% 
  top_n(5) %>% 
  ungroup()

tidy_dracula %>% 
  filter(chapter == 19)  

# N-grams -----------------------------------------------------------------

dracula_bigrams <- dracula %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

dracula_bigrams %>% 
  count(bigram, sort = TRUE)

# Not too helpful ("of the", "in the", "to the").
## Use separate() from tidyr to split and then remove
## stop words

bigrams_separated <- dracula_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>% 
  filter(!word1 %in% custom_stop_words$word) %>% 
  filter(!word2 %in% custom_stop_words$word) %>%
  # Remove _ and numbers...not sure this is the best place
  mutate(word1 = str_extract(word1, "[a-z']+")) %>%
  mutate(word2 = str_extract(word2, "[a-z']+")) %>% 
  filter(!is.na(word1),
         !is.na(word2))

# Cleaned up bigrams

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

# Put them back together with united()

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# Trigrams

dracula_trigrams <- dracula %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% custom_stop_words$word,
         !word2 %in% custom_stop_words$word,
         !word3 %in% custom_stop_words$word) %>% 
  mutate(word1 = str_extract(word1, "[a-z']+")) %>% 
  mutate(word2 = str_extract(word2, "[a-z']+")) %>% 
  mutate(word3 = str_extract(word3, "[a-z']+")) %>%
  filter(!is.na(word1),
         !is.na(word2),
         !is.na(word3)) %>% 
  count(word1, word2, word3, sort = TRUE)

dracula_trigrams
  
# Analyzing bigrams

# Streets
bigrams_filtered %>% 
  filter(word2 == "street") %>% 
  count(word1, sort = TRUE)

# Context RE: negatives
bigrams_separated %>% 
  filter(word1 == "not") %>% 
  count(word1, word2, sort = TRUE)

# Use AFINN lexicon to identify how often sentiment-
## associated words are preceded by a negating word

AFINN <- get_sentiments("afinn") # +/= ratings 

AFINN

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word2, score, sort = TRUE) %>% 
  ungroup()

# Words contributing the most in the "wrong" direction
not_words %>% 
  mutate(contribution = n * score) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(word2 = reorder(word2, contribution)) %>% 
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip() +
  theme_minimal()

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>% 
  filter(word1 %in% negation_words) %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>% 
  count(word1, word2, score, sort = TRUE) %>% 
  ungroup() %>% 
  mutate(contribution = n * score) %>% 
  group_by(word1) %>% 
  arrange(desc(abs(contribution))) %>% 
  filter(row_number() <= 20) %>% 
  ungroup()

negated_words %>% 
  group_by(word1) %>% 
  arrange(desc(contribution)) %>% 
  ungroup() %>% 
  ggplot(aes(reorder(word2, contribution), contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"negation words\"") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, ncol = 2, scales = "free") +
  coord_flip() +
  theme_minimal() 

# Network Graph -----------------------------------------------------------

bigram_counts

bigram_graph <- bigram_counts %>% 
  filter(n > 5) %>% 
  graph_from_data_frame()

bigram_graph

set.seed(2018)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()

set.seed(2018)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = "n"), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Co-occurring Words ------------------------------------------------------

# Break into 10-line sections
dracula_section <- dracula %>% 
  mutate(section = row_number() %/% 10) %>% 
  filter(section > 0) %>% 
  unnest_tokens(word, text) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>% 
  filter(!is.na(word)) %>% # removing any resulting NA's
  filter(!word %in% stop_words$word)

# Correlate
word_cors <- dracula_section %>% 
  group_by(word) %>% 
  filter(n() >= 20) %>% #  common words
  pairwise_cor(word, section, sort = TRUE) #  from widyr

word_cors

# Check out words correlated with a selection of terms
word_cors %>% 
  filter(item1 == "evil")

word_cors %>% 
  filter(item1 == "vampire")

word_cors %>% 
  filter(item1 == "night")

word_cors %>% 
  filter(item1 == "stake")

word_cors %>% 
  filter(item1 == "time")

word_cors %>% 
  filter(item1 == "blood")

# Selected terms and their correlates
word_cors %>% 
  filter(item1 %in% c("evil", "night", "dark", "time")) %>% 
  group_by(item1) %>% 
  top_n(6) %>% 
  ungroup() %>% 
  mutate(item2 = reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") + 
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  theme_minimal()

# Network graph
set.seed(2018)

word_cors %>% 
  filter(correlation > .2) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "#5F0D0A", alpha = .8, size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Vampire-themed network graph
set.seed(1897)

word_cors %>% 
  filter(correlation > .2) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, color = "white"), show.legend = FALSE) +
  geom_node_point(color = "#5F0D0A", alpha = .8, size = 5) +
  geom_node_text(aes(label = name, color = "white"), repel = TRUE, show.legend = FALSE) +
  theme(
    panel.background = element_rect(fill = "black"),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
