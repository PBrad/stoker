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
library(wordcloud)
library(reshape2)

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

# Tidy --------------------------------------------------------------------

tidy_dracula <- dracula %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(
           str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>% 
  unnest_tokens(word, text) %>% # one word per row
  anti_join(stop_words) # remove stop words

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

# Most positive chapter is 32
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
  filter(chapter == 32)

# Most negative chapter is 46
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
  filter(chapter == 46)  
## These chapter counts are off... need to check
## the regex

