# Load the necessary libraries
library(readr)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(stringr)
library(tm) 
library(wordcloud)
library(igraph)
library(ggraph)
library(topicmodels)
library(quanteda)
library(RColorBrewer)
library(ggplot2)

# Reading the dataset
df_airbnb <- read.csv("~/Desktop/Business Unstructured Data/listingsAndReviews.csv")

# Data Exploration
names(df_airbnb)
head(df_airbnb, n=2)
str(df_airbnb)
summary(df_airbnb)
sapply(df_airbnb, class)

# Data Preprocessing
# Tokenization and creating TIDY format data for descriptions
tidy_descriptions <- df_airbnb %>%
  select(description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

# Remove punctuation, numbers, and stopwords
processed_text <- df_airbnb$description %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("[[:digit:]]", "") %>%
  removeWords(stopwords("en"))

processed_text

# Tokenization and creating TIDY format data for reviews
tidy_reviews <- df_airbnb %>%
  select(reviews) %>%
  unnest_tokens(word, reviews) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)

# Remove non-textual data from reviews
extract_review_text <- function(json_str) {
  tryCatch({
    parsed_json <- fromJSON(json_str)
    return(paste(parsed_json$comments, collapse = " "))
  }, error = function(e) { return(NA) })
}
df_airbnb$reviews_cleaned <- sapply(df_airbnb$reviews, extract_review_text)


# Text Mining

# Creating a VCorpus for descriptions
corpus_descriptions <- VCorpus(VectorSource(df_airbnb$description))
corpus_descriptions <- tm_map(corpus_descriptions, content_transformer(tolower))
corpus_descriptions <- tm_map(corpus_descriptions, removePunctuation)
corpus_descriptions <- tm_map(corpus_descriptions, removeNumbers)
corpus_descriptions <- tm_map(corpus_descriptions, removeWords, stopwords("en"))

# Creating a DTM for descriptions
dtm_descriptions <- DocumentTermMatrix(corpus_descriptions)
tidy_descriptions <- as.data.frame(as.matrix(dtm_descriptions))
colnames(tidy_descriptions) <- make.names(colnames(tidy_descriptions))

# Creating a VCorpus for reviews
corpus_reviews <- VCorpus(VectorSource(df_airbnb$reviews_cleaned))
corpus_reviews <- tm_map(corpus_reviews, content_transformer(tolower))
corpus_reviews <- tm_map(corpus_reviews, removePunctuation)
corpus_reviews <- tm_map(corpus_reviews, removeNumbers)
corpus_reviews <- tm_map(corpus_reviews, removeWords, stopwords("en"))

# Creating a DTM for reviews
dtm_reviews <- DocumentTermMatrix(corpus_reviews)
tidy_reviews <- as.data.frame(as.matrix(dtm_reviews))
colnames(tidy_reviews) <- make.names(colnames(tidy_reviews))

# Visualizations

# Convert DTM to data frame for Descriptions
tidy_descriptions_df <- as.data.frame(as.matrix(dtm_descriptions))
word_counts_descriptions <- sort(colSums(tidy_descriptions_df), decreasing = TRUE)
tidy_descriptions_df <- data.frame(word = names(word_counts_descriptions), n = word_counts_descriptions)

# Convert DTM to data frame for Reviews
tidy_reviews_df <- as.data.frame(as.matrix(dtm_reviews))
word_counts_reviews <- sort(colSums(tidy_reviews_df), decreasing = TRUE)
tidy_reviews_df <- data.frame(word = names(word_counts_reviews), n = word_counts_reviews)

# Visualization for Descriptions
top_words_desc <- head(tidy_descriptions_df, 10)
ggplot(top_words_desc, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Frequent Words in Airbnb Descriptions",
       x = "Words", y = "Frequency") +
  theme_minimal()

# Visualization for Reviews
top_words_reviews <- head(tidy_reviews_df, 10)
ggplot(top_words_reviews, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Frequent Words in Airbnb Reviews",
       x = "Words", y = "Frequency") +
  theme_minimal()

# Word Cloud for Description

# Select the top N words
top_n_desc_words <- head(tidy_descriptions_df, 150)  

# Generate word cloud with limited words
wordcloud(words = top_n_desc_words$word, freq = top_n_desc_words$n, min.freq = 1, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Find correlations between words
word_cors <- findAssocs(dtm_descriptions, terms = c("bedroom", "room"), corlimit = 0.3)
word_cors

# Find associations between specific words
word_associations <- findAssocs(dtm_descriptions, terms = "airbnb", corlimit = 0.25)

# Word Cloud for Reviews

# Select the top N words
top_n_rev_words <- head(tidy_reviews_df, 150)  

# Generate word cloud with limited words
wordcloud(words = top_n_rev_words$word, freq = top_n_rev_words$n, min.freq = 1, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

# Sentiment Analysis - Reviews

# Load the sentiment lexicons
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Combine the sentiment lexicons
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing"))

# Filter by lexicon
afinn_sentiments <- sentiments %>% filter(lexicon == 'afinn')
nrc_sentiments <- sentiments %>% filter(lexicon == 'nrc')
bing_sentiments <- sentiments %>% filter(lexicon == 'bing')

# Explore unique values for each lexicon
unique(nrc_sentiments$sentiment) 
unique(bing_sentiments$sentiment)
summary(afinn_sentiments$value)   

# Prepare reviews for sentiment analysis
tidy_reviews <- df_airbnb %>%
  unnest_tokens(word, reviews_cleaned) %>%
  inner_join(sentiments, by = "word", relationship = "many-to-many")

# Visualization for Bing, Afinn, Nrc  Lexicon

bing_vis <- tidy_reviews %>% 
  filter(lexicon == 'bing') %>% 
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution for Bing Lexicon", x = "Sentiment", y = "Count") +
  theme_minimal()

print(bing_vis)

afinn_vis <- tidy_reviews %>% 
  filter(lexicon == 'afinn') %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = word)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Sentiment Scores in AFINN Lexicon", x = "Word", y = "Score") +
  theme_minimal()

print(afinn_vis)

nrc_vis <- tidy_reviews %>% 
  filter(lexicon == 'nrc') %>% 
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution for NRC Lexicon", x = "Sentiment", y = "Count") +
  theme_minimal()

print(nrc_vis)

# Sentiment Analysis - Descriptions

# Load the sentiment lexicons
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")

# Combine the sentiment lexicons
sentiments <- dplyr::bind_rows(dplyr::mutate(afinn, lexicon="afinn"),
                               dplyr::mutate(nrc, lexicon="nrc"),
                               dplyr::mutate(bing, lexicon="bing"))

# Redoing since I got error - Tokenization and creating tidy format data for descriptions
tidy_descriptions <- df_airbnb %>%
  dplyr::select(description) %>%
  unnest_tokens(word, description) %>%
  dplyr::anti_join(stop_words, by = "word") %>%
  dplyr::count(word, sort = TRUE)

# Identifying duplicate column names
dup_cols <- names(tidy_descriptions)[duplicated(names(tidy_descriptions))]

# Manually renaming duplicate columns
for (col in dup_cols) {
  cols_to_rename <- which(names(tidy_descriptions) == col)
  for (i in seq_along(cols_to_rename)) {
    new_name <- paste0(col, "_", i)
    names(tidy_descriptions)[cols_to_rename[i]] <- new_name
  }
}

# Convert 'word' column in tidy_descriptions to character
tidy_descriptions$word <- as.character(tidy_descriptions$word)

# Prepare descriptions for sentiment analysis
tidy_descriptions_sentiment <- tidy_descriptions %>%
  dplyr::inner_join(sentiments, by = "word")

# Visualization for Bing, Afinn, Nrc Lexicon on Descriptions
library(ggplot2)
bing_vis_desc <- tidy_descriptions_sentiment %>%
  dplyr::filter(lexicon == 'bing') %>%
  dplyr::count(sentiment) %>%
  ggplot(ggplot2::aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution for Bing Lexicon in Descriptions", 
       x = "Sentiment", y = "Count") +
  theme_minimal()

print(bing_vis_desc)

afinn_vis_desc <- tidy_descriptions_sentiment %>%
  dplyr::filter(lexicon == 'afinn') %>%
  dplyr::count(value, sort = TRUE) %>%
  dplyr::top_n(10) %>%
  ggplot(ggplot2::aes(x = reorder(value, n), y = n, fill = value)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 10 Sentiment Scores in AFINN Lexicon in Descriptions", 
       x = "Score", y = "Frequency") +
  theme_minimal()

print(afinn_vis_desc)

nrc_vis_desc <- tidy_descriptions_sentiment %>%
  dplyr::filter(lexicon == 'nrc') %>%
  dplyr::count(sentiment) %>%
  ggplot(ggplot2::aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution for NRC Lexicon in Descriptions", 
       x = "Sentiment", y = "Count") +
  theme_minimal()

print(nrc_vis_desc)


# Negative Words Analysis - Reviews

# Filter for negative sentiments in the Bing lexicon
negative_words <- bing %>% filter(sentiment == 'negative')

# Tokenize the cleaned reviews and join with the negative words
tidy_reviews_negative <- df_airbnb %>%
  unnest_tokens(word, reviews_cleaned) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(negative_words, by = "word")

# Count the frequency of each negative word
negative_word_counts <- tidy_reviews_negative %>%
  count(word, sort = TRUE)

# Visualization of Negative Words
ggplot(negative_word_counts[1:10, ], aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Frequent Negative Words in Airbnb Reviews",
       x = "Negative Words", y = "Frequency") +
  theme_minimal()


# Negative Words Analysis - Description


# Filter for negative sentiments in the Bing lexicon
negative_words <- bing %>% filter(sentiment == 'negative')

# Tokenize the cleaned description and join with the negative words
tidy_description_negative <- df_airbnb %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(negative_words, by = "word")

# Count the frequency of each negative word
negative_word_counts <- tidy_description_negative %>%
  count(word, sort = TRUE)

# Visualization of Negative Words
ggplot(negative_word_counts[1:10, ], aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Frequent Negative Words in Airbnb Description",
       x = "Negative Words", y = "Frequency") +
  theme_minimal()


# Td-IDF Framework - Descriptions

# Ensure tidy_descriptions is set up correctly
tidy_descriptions <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  select(document_id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

# Count words per document
word_counts <- tidy_descriptions %>%
  group_by(document_id, word) %>%
  summarize(n = n(), .groups = 'drop')

# TF-IDF for Descriptions
tidy_descriptions_tfidf <- word_counts %>%
  bind_tf_idf(word, document_id, n) %>%
  arrange(desc(tf_idf))

# Visualization of top TF-IDF scores in descriptions
top_tfidf_desc <- tidy_descriptions_tfidf %>%
  group_by(word) %>%
  summarize(avg_tf_idf = mean(tf_idf), .groups = 'drop') %>%
  top_n(10, avg_tf_idf)

ggplot(top_tfidf_desc, aes(x = reorder(word, avg_tf_idf), y = avg_tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top TF-IDF Scores in Airbnb Descriptions", x = "Words", y = "Average TF-IDF Score") +
  theme_minimal()

# Making sure tidy_descriptions has 'document' and 'word' columns
tidy_descriptions <- df_airbnb %>%
  mutate(document = row_number()) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

# Now compute TF-IDF
tf_idf <- tidy_descriptions %>%
  count(document, word) %>%
  bind_tf_idf(word, document, n)


ggplot(tf_idf, aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top TF-IDF Scores in Airbnb Data")



# Td-IDF Framework - Reviews

# Ensuring tidy_reviews is set up correctly
tidy_reviews <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  select(document_id, reviews_cleaned) %>%
  unnest_tokens(word, reviews_cleaned) %>%
  anti_join(stop_words, by = "word")

# Count words per document in reviews
word_counts_reviews <- tidy_reviews %>%
  group_by(document_id, word) %>%
  summarize(n = n(), .groups = 'drop')

# TF-IDF for Reviews
tidy_reviews_tfidf <- word_counts_reviews %>%
  bind_tf_idf(word, document_id, n) %>%
  arrange(desc(tf_idf))

# Visualization of top TF-IDF scores in reviews
top_tfidf_reviews <- tidy_reviews_tfidf %>%
  group_by(word) %>%
  summarize(avg_tf_idf = mean(tf_idf), .groups = 'drop') %>%
  top_n(10, avg_tf_idf)

ggplot(top_tfidf_reviews, aes(x = reorder(word, avg_tf_idf), y = avg_tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top TF-IDF Scores in Airbnb Reviews", x = "Words", y = "Average TF-IDF Score") +
  theme_minimal()

# Making sure tidy_reviews has 'document_id' and 'word' columns
tidy_reviews <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  unnest_tokens(word, reviews_cleaned) %>%
  anti_join(stop_words, by = "word")


# Bigrams & Quadgrams - Descriptions

# Creating Bigrams for Descriptions
bigrams_desc <- df_airbnb %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

# Counting and filtering the bigrams
bigram_counts_desc <- bigrams_desc %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

bigram_counts_desc

# Similar process for Quadgrams (n = 4)
quadgrams_desc <- df_airbnb %>%
  unnest_tokens(quadgram, description, token = "ngrams", n = 4)

# Counting and filtering the bigrams
quadgram_counts_desc <- quadgrams_desc %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

quadgram_counts_desc

# Adding document_id to bigrams_desc
bigrams_desc <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

# Unite bigram words
bigram_united <- bigrams_desc %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")

# Compute tf-idf for bigrams
bigram_tf_idf <- bigram_united %>%
  count(document_id, bigram) %>%
  bind_tf_idf(bigram, document_id, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Create a network graph for bigrams
bigram_graph <- bigram_counts_desc %>%
  filter(n > 50) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Selecting top bigrams based on TF-IDF scores
top_bigrams_tf_idf <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  head(10)

# Plotting the top bigrams
ggplot(top_bigrams_tf_idf, aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Bigrams in Airbnb Descriptions (Based on TF-IDF Scores)",
       x = "Bigram",
       y = "TF-IDF Score")

# Adding document_id to quadgrams_desc
quadgrams_desc <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  unnest_tokens(quadgram, description, token = "ngrams", n = 4)

# Unite quadgram words
quadgram_united <- quadgrams_desc %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  unite(quadgram, word1, word2, word3, word4, sep = " ")

# Compute tf-idf for quadgrams
quadgram_tf_idf <- quadgram_united %>%
  count(document_id, quadgram) %>%
  bind_tf_idf(quadgram, document_id, n) %>%
  arrange(desc(tf_idf))

quadgram_tf_idf

# Creating Trigrams
trigrams <- df_airbnb %>%
  unnest_tokens(trigram, description, token = "ngrams", n = 3)

# Count and filter trigrams
trigram_counts <- trigrams %>%
  count(trigram, sort = TRUE)

# Top Trigrams visualization
top_trigrams <- head(trigram_counts, 10)
ggplot(top_trigrams, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Trigrams in Airbnb Descriptions")


# Bigrams & Quadgrams - Reviews

# Ensure tidy_reviews is set up correctly, using reviews_cleaned column
tidy_reviews <- df_airbnb %>%
  mutate(document_id = row_number()) %>%
  select(document_id, reviews_cleaned)

# Creating Bigrams for Reviews
bigrams_reviews <- tidy_reviews %>%
  unnest_tokens(bigram, reviews_cleaned, token = "ngrams", n = 2)

# Counting and filtering the bigrams in Reviews
bigram_counts_reviews <- bigrams_reviews %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

# Output the bigram counts
bigram_counts_reviews

# Creating Quadgrams for Reviews
quadgrams_reviews <- tidy_reviews %>%
  unnest_tokens(quadgram, reviews_cleaned, token = "ngrams", n = 4)

# Counting and filtering the quadgrams in Reviews
quadgram_counts_reviews <- quadgrams_reviews %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

# Output the quadgram counts
quadgram_counts_reviews

# Adding document_id to bigrams_reviews
bigrams_reviews <- tidy_reviews %>%
  unnest_tokens(bigram, reviews_cleaned, token = "ngrams", n = 2)

# Unite bigram words
bigram_united_reviews <- bigrams_reviews %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ")

# Compute tf-idf for bigrams in Reviews
bigram_tf_idf_reviews <- bigram_united_reviews %>%
  count(document_id, bigram) %>%
  bind_tf_idf(bigram, document_id, n) %>%
  arrange(desc(tf_idf))

# Output the TF-IDF for bigrams
bigram_tf_idf_reviews

# Create a network graph for bigrams in Reviews
bigram_graph_reviews <- bigram_counts_reviews %>%
  filter(n > 50) %>%
  graph_from_data_frame()

ggraph(bigram_graph_reviews, layout = "fr") +
  geom_edge_link() +
  geom_node_point(color = "blue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Create a graph object from bigram counts
bigram_graph <- graph_from_data_frame(bigram_counts_desc)

# Plot the network graph
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  labs(title = "Network Graph of Bigrams in Airbnb Descriptions")

# Selecting top bigrams based on TF-IDF scores in Reviews
top_bigrams_tf_idf_reviews <- bigram_tf_idf_reviews %>%
  arrange(desc(tf_idf)) %>%
  head(10)

# Plotting the top bigrams in Reviews
ggplot(top_bigrams_tf_idf_reviews, aes(x = reorder(bigram, tf_idf), y = tf_idf)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Bigrams in Airbnb Reviews (Based on TF-IDF Scores)",
       x = "Bigram",
       y = "TF-IDF Score")

# Adding document_id to quadgrams_reviews
quadgrams_reviews <- tidy_reviews %>%
  unnest_tokens(quadgram, reviews_cleaned, token = "ngrams", n = 4)

# Unite quadgram words
quadgram_united_reviews <- quadgrams_reviews %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  unite(quadgram, word1, word2, word3, word4, sep = " ")

# Compute tf-idf for quadgrams in Reviews
quadgram_tf_idf_reviews <- quadgram_united_reviews %>%
  count(document_id, quadgram) %>%
  bind_tf_idf(quadgram, document_id, n) %>%
  arrange(desc(tf_idf))

# Output the TF-IDF for quadgrams
quadgram_tf_idf_reviews

# Creating Trigrams for Reviews
trigrams_reviews <- tidy_reviews %>%
  unnest_tokens(trigram, reviews_cleaned, token = "ngrams", n = 3)

# Count and filter trigrams in Reviews
trigram_counts_reviews <- trigrams_reviews %>%
  count(trigram, sort = TRUE)

# Top Trigrams visualization in Reviews
top_trigrams_reviews <- head(trigram_counts_reviews, 10)
ggplot(top_trigrams_reviews, aes(x = reorder(trigram, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Trigrams in Airbnb Reviews")

# Zipf's Law visualization - Descriptions

word_frequency <- tidy_descriptions %>%
  count(word, sort = TRUE) %>%
  mutate(rank = row_number())

ggplot(word_frequency, aes(x = log(rank), y = log(n))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Word Frequency Distribution (Zipf's Law)", x = "Log Rank", y = "Log Frequency")

word_counts <- tidy_descriptions %>%
  count(word, sort = TRUE) %>%
  mutate(rank = row_number())

# Zipf's Law Visualization
ggplot(word_counts, aes(x = log(rank), y = log(n))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Zipf's Law in Airbnb Descriptions",
       x = "Log Rank", y = "Log Frequency")


# Topic Modelling with LDA

# Remove empty documents from the DocumentTermMatrix
non_empty_docs_idx <- which(rowSums(as.matrix(dtm_descriptions)) > 0)
dtm_descriptions_non_empty <- dtm_descriptions[non_empty_docs_idx, ]

num_topics_descriptions <- 5

# Perform LDA on the non-empty DocumentTermMatrix
lda_model_descriptions <- LDA(dtm_descriptions_non_empty, k = num_topics_descriptions, control = list(seed = 123))

# Extract and visualize topics
topics_descriptions <- tidy(lda_model_descriptions, matrix = "beta")

top_terms_descriptions <- topics_descriptions %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ggplot(top_terms_descriptions, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  labs(title = "Top Terms in Each Topic (LDA Model for Descriptions)",
       x = "Terms", y = "Beta")

# Numerical Data Analysis 

# Visualization 1: Price Distribution
ggplot(df_airbnb, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Price Distribution", x = "Price", y = "Count") +
  theme_minimal()

# Visualization 2: Price vs Number of Reviews
ggplot(df_airbnb, aes(x = price, y = number_of_reviews)) +
  geom_point(aes(color = number_of_reviews)) +
  labs(title = "Price vs Number of Reviews", x = "Price", y = "Number of Reviews") +
  theme_minimal()

# Visualization 3: Boxplot of Prices by Room Type
ggplot(df_airbnb, aes(x = room_type, y = price)) +
  geom_boxplot() +
  labs(title = "Price Distribution by Room Type", x = "Room Type", y = "Price")

# Visualization 4: Bar plot for average price per availability category
df_airbnb %>%
  mutate(availability_category = ifelse(availability.availability_365 > 180, "High", "Low")) %>%
  group_by(availability_category) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  ggplot(aes(x = availability_category, y = avg_price, fill = availability_category)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Price per Availability Category", x = "Availability Category", y = "Average Price")

# Naive Bayes - Text

# Create a unique identifier for each description
df_airbnb$description_id <- seq_along(df_airbnb$description)

# Tokenization while keeping the description ID
tidy_descriptions <- df_airbnb %>%
  select(description_id, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word")

# Join with sentiment lexicons
tidy_descriptions_sentiment <- tidy_descriptions %>%
  inner_join(sentiments, by = "word", relationship = "many-to-many")


# Calculate sentiment score for each description ID
sentiment_scores <- tidy_descriptions_sentiment %>%
  group_by(description_id) %>%
  summarize(sentiment_score = sum(value, na.rm = TRUE)) %>%
  ungroup()

# Join back to the original dataframe
df_airbnb <- df_airbnb %>%
  left_join(sentiment_scores, by = "description_id")

# Check if sentiment_score column exists
if ("sentiment_score" %in% colnames(df_airbnb)) {
  # Replace NA values with 0
  df_airbnb$sentiment_score[is.na(df_airbnb$sentiment_score)] <- 0
} else {
  # If sentiment_score column does not exist, print a message
  print("Column 'sentiment_score' does not exist in the dataframe.")
}

# Fill NAs with 0 or another neutral value
df_airbnb$sentiment_score[is.na(df_airbnb$sentiment_score)] <- 0

# Create a sentiment label based on the score
positive_threshold <- 10   
negative_threshold <- -10 

# Create sentiment labels
df_airbnb$sentiment_label <- ifelse(df_airbnb$sentiment_score >= positive_threshold, 'positive',
                                  ifelse(df_airbnb$sentiment_score <= negative_threshold, 'negative', 'neutral'))


# Convert descriptions to a corpus and then to a document-feature matrix
corp <- corpus(df_airbnb$description)

# Create tokens and then a document-feature matrix
tokens <- tokens(corp, what = "word", remove_punct = TRUE, remove_numbers = TRUE)
tokens <- tokens_remove(tokens, stopwords("en"))
dfm <- dfm(tokens)

# Apply TF-IDF weighting
dfm_tfidf <- dfm_tfidf(dfm)

# Split data into training and testing
set.seed(123) 
split <- sample.int(n = nrow(df_airbnb), size = floor(0.7 * nrow(df_airbnb)))
train_data <- dfm_tfidf[split, ]
test_data <- dfm_tfidf[-split, ]

# Convert labels to factor for training Naive Bayes model
train_labels <- factor(df_airbnb$sentiment_label[split])
test_labels <- factor(df_airbnb$sentiment_label[-split])

library(quanteda.textmodels)

# Train Naive Bayes model
NB_classifier <- textmodel_nb(train_data, y = train_labels)

# Predict on test data
predictions <- predict(NB_classifier, newdata = test_data)

# Evaluate performance
performance_table <- table(Predicted = predictions, Actual = test_labels)
performance_table

