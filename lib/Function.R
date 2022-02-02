
# Author: Shanyue Zeng

#  Sentiments for each sentence
sentiment_score <- function(df){
  sentence <- df %>% 
    select(c("title", "author", "school", "sentence_str", 
             "original_publication_date")) %>%
    mutate(id = 1:nrow(df))
  
  sentence$sentence_str <- as.character(sentence$sentence_str)
  emotions <- get_nrc_sentiment(sentence$sentence_str)
  sentence <- bind_cols(sentence, emotions)
  
  output_dp <- "sentence_emotion.csv"
  write.csv(sentence, output_dp)
  return(output_dp)
}

# Find a maximum index
random_max <- function(a){
  sample(which(a == max(a)), 1)
}

# Get top emotion and its score
top_emotion <- function(processed_df){
  sentence <- read.csv(processed_df)
  
  # Get the top emotion of each sentence
  sentence$top <- sentence %>%
    select(anger:trust) %>%
    apply(1, random_max)
  
  # Get the emotion score for the top emotion
  sentence$top.a <- sentence %>%
    select(anger:trust) %>%
    apply(1, max)
  
  sentence$top[sentence$top.a == 0] <- 9
  return(sentence)
}

sentence_sentiment <- function(processed_df){
  sentence <- read.csv(processed_df)
  
  # Get the sentiment of each sentence
  sentence$sentiment <- sentence %>%
    select(negative:positive) %>%
    apply(1, random_max)
  
  # Get the sentiment score
  sentence$sentiment.a <- sentence %>%
    select(negative:positive) %>%
    apply(1, max)
  
  sentence$sentiment[sentence$sentiment.a == 0] <- 3
  return(sentence)
}


word_cloud <- function(data,select_school){
  text <- data %>% 
    select(school, author, title, sentence_lowered, original_publication_date) %>%
    filter(school == select_school )
  
  # Create a corpus  
  my_custom_stopwords <- c("one", "will", "may","things", "say",
                           "can", "now", "even", "also", "must","whether")
  corpus <- VCorpus(VectorSource(text$sentence_lowered))
  corpus<- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, my_custom_stopwords)
  tdm <- TermDocumentMatrix(corpus)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(words = names(v), freq=v)
  analytic <- d
  
  set.seed(20220202)
  wordcloud(words = d$word, freq = d$freq, min.freq = 10,
            max.words=150, random.order=FALSE, rot.per=0.3,
            colors=brewer.pal(8, "Dark2"), scale = c(4, 0.5))
}