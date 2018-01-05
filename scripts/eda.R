############

# EDA

############

# Packages ----------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gutenbergr)
library(tidytext)
library(stringr)

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

dracula_sentiment <- tidy_dracula %>%
  inner_join(get_sentiments("bing")) %>% 
  mutate(linenumber = row_number()) %>% 
  count(word, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

dracula_sentiment %>% 
  ggplot(aes(index, sentiment)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Dracula Sentiment Trajectory")

