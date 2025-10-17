library(rvest)
library(xml2)
library(textclean)
library(textstem)
library(hunspell)
library(stopwords)
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(igraph)
library(ggraph)
library(sentimentr)

url <- "https://www.thedailystar.net"
extention <- "/news/world/"
webpage <- tryCatch(read_html(paste0(url, extention)), error = function(e) {
  stop("Failed to read main page: ", e$message)
})
web_data <- webpage %>% html_nodes(".card-content.columns")

rows_list <- list()
cleaned_list <- list()
i <- 1

for (title in web_data) {
  title_node <- title %>% html_node("a")
  if (is.null(title_node)) next
  title_text <- title_node %>% html_text(trim = TRUE)
  href_value <- html_attr(title_node, "href")
  full_url <- xml2::url_absolute(href_value, url)
  
  extwebpage <- tryCatch(read_html(full_url), error = function(e) {
    message("Failed to fetch ", full_url, " : ", e$message)
    return(NULL)
  })
  if (is.null(extwebpage)) next
  Sys.sleep(0.5)
  
  description <- extwebpage %>%
    html_nodes("article p") %>%
    html_text() %>%
    paste(collapse = " ")
  if (is.na(description) || nchar(description) < 5) next
  
  text <- tolower(description)
  text <- replace_contraction(text)
  text <- replace_emoji(text)
  text <- replace_emoticon(text)
  text <- gsub("[^a-z\\s]", " ", text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  
  tokens <- unlist(strsplit(text, " "))
  tokens <- tokens[!(tokens %in% c(stopwords("en"), ""))]
  tokens <- tokens[nchar(tokens) > 1]
  
  if (length(tokens) > 0) {
    lemmatized <- lemmatize_words(tokens)
    correct <- hunspell_check(lemmatized)
    tokens_clean <- lemmatized[which(correct)]
  } else {
    tokens_clean <- character(0)
  }
  
  cleaned_text <- paste(tokens_clean, collapse = " ")
  
  rows_list[[i]] <- list(
    Title = title_text,
    Anchor_tag = href_value,
    Raw_article = description,
    Cleaned_article = cleaned_text
  )
  cleaned_list[[i]] <- data.frame(cleaned_text = cleaned_text, stringsAsFactors = FALSE)
  i <- i + 1
}

raw_data <- dplyr::bind_rows(rows_list)
if (length(cleaned_list) > 0) {
  cleaned_data <- dplyr::bind_rows(cleaned_list)
} else {
  cleaned_data <- data.frame(cleaned_text = character(0), stringsAsFactors = FALSE)
}

output_dir <- "C:/Users/maeed/OneDrive/Documents/Final Project/Files"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

raw_file <- file.path(output_dir, "Raw_Article.csv")
cleaned_file <- file.path(output_dir, "Cleaned_Article.csv")

write.csv(raw_data, file = raw_file, row.names = FALSE, fileEncoding = "UTF-8")
write.csv(cleaned_data, file = cleaned_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("Files saved in:", output_dir, "\n")

n_docs <- nrow(cleaned_data)
word_tokens_vec <- if (n_docs > 0) {
  sapply(strsplit(cleaned_data$cleaned_text, " "), function(x) {
    if (length(x) == 1 && x == "") return(0L)
    sum(nzchar(x))
  })
} else numeric(0)

total_words <- sum(word_tokens_vec)
avg_words <- ifelse(n_docs > 0, mean(word_tokens_vec), NA)

cat("Documents:", n_docs, "\n")
cat("Total cleaned tokens:", total_words, "\n")
cat("Average tokens per document:", avg_words, "\n")

if (n_docs > 0) {
  word_tokens <- cleaned_data %>%
    mutate(doc_id = row_number()) %>%
    unnest_tokens(word, cleaned_text) %>%
    filter(!word %in% stop_words$word)
  word_counts <- word_tokens %>% count(word, sort = TRUE)
  top_words <- word_counts %>% slice_max(n, n = 20)
  
  print(
    ggplot(top_words, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 20 Words in News Articles", x = "Words", y = "Frequency") +
      theme_minimal()
  )
} else {
  message("No cleaned documents available for word frequency analysis.")
}

if (n_docs > 0) {
  cleaned_data$doc_id <- 1:nrow(cleaned_data)
  doc_words <- cleaned_data %>%
    unnest_tokens(word, cleaned_text) %>%
    anti_join(stop_words, by = "word") %>%
    count(doc_id, word, sort = TRUE)
  
  doc_tfidf <- doc_words %>%
    bind_tf_idf(word, doc_id, n) %>%
    arrange(desc(tf_idf))
  
  top_tfidf <- doc_tfidf %>%
    distinct(word, .keep_all = TRUE) %>%
    slice_max(tf_idf, n = 20)
  
  print(
    ggplot(top_tfidf, aes(x = reorder(word, tf_idf), y = tf_idf)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(title = "Top 20 TF-IDF Words", x = "Words", y = "TF-IDF Score") +
      theme_minimal()
  )
} else {
  message("No cleaned documents available for TF-IDF analysis.")
}

if (n_docs > 0) {
  texts <- cleaned_data$cleaned_text
  corpus_all <- VCorpus(VectorSource(texts))
  dtm_all <- DocumentTermMatrix(corpus_all, control = list(wordLengths = c(3, Inf)))
  dtm_all_sparse <- tryCatch(removeSparseTerms(dtm_all, 0.99), error = function(e) dtm_all)
  
  dtm_mat <- as.matrix(dtm_all_sparse)
  dims <- dim(dtm_mat)
  if (is.null(dims) || dims[1] == 0 || dims[2] == 0) {
    stop("DocumentTermMatrix is empty after removeSparseTerms(). Adjust preprocessing or sparsity threshold.")
  }
  if (sum(dtm_mat) == 0) {
    stop("DTM contains only zeros. Check preprocessing; tokens may have been removed.")
  }
  
  k <- 5
  if (k >= nrow(dtm_all_sparse)) {
    warning("k >= number of documents. Reducing k to number_of_docs - 1.")
    k <- max(2, nrow(dtm_all_sparse) - 1)
  }
  
  lda_model <- tryCatch(
    LDA(dtm_all_sparse, k = k, control = list(seed = 1234)),
    error = function(e) { stop("LDA failed: ", e$message) }
  )
  
  topics <- tidy(lda_model, matrix = "beta")
  top_terms <- topics %>%
    group_by(topic) %>%
    slice_max(beta, n = 5) %>%
    arrange(topic, -beta)
  print(top_terms)
  
  gamma_df <- tidy(lda_model, matrix = "gamma") %>% mutate(document = as.integer(document))
  topic_summary <- gamma_df %>% group_by(topic) %>% summarise(avg_prop = mean(gamma))
  
  print(
    ggplot(topic_summary, aes(x = "", y = avg_prop, fill = factor(topic))) +
      geom_col() +
      coord_polar(theta = "y") +
      labs(title = "Topic Distribution Across Articles", fill = "Topic") +
      theme_void()
  )
  
  dominant_topics <- gamma_df %>%
    group_by(document) %>%
    slice_max(gamma, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  topic_labels <- top_terms %>%
    group_by(topic) %>%
    summarise(topic_label = paste(term, collapse = "_"))
  
  dominant_topics <- dominant_topics %>%
    left_join(topic_labels, by = "topic")
  
  gamma_wide <- gamma_df %>%
    pivot_wider(names_from = topic, values_from = gamma, names_prefix = "Topic_") %>%
    arrange(document) %>%
    left_join(dominant_topics %>% select(document, dominant_topic_label = topic_label), by = "document")
  
  print(gamma_wide)
} else {
  message("Skipping topic modeling: no documents available.")
}

if (n_docs > 0) {
  for (i in seq_along(texts)) {
    corpus_i <- VCorpus(VectorSource(texts[i]))
    dtm_i <- DocumentTermMatrix(corpus_i, control = list(wordLengths = c(3, Inf)))
    word_freq_i <- colSums(as.matrix(dtm_i))
    word_freq_i <- sort(word_freq_i, decreasing = TRUE)
    if (length(word_freq_i) == 0) next
    wordcloud(words = names(word_freq_i), freq = word_freq_i, min.freq = 1, max.words = 30,
              random.order = FALSE, rot.per = 0.3, colors = brewer.pal(min(8, length(word_freq_i)), "Dark2"))
  }
}

if (n_docs > 0) {
  sentiment_scores <- sentiment_by(cleaned_data$cleaned_text)
  cleaned_data$sentiment <- sentiment_scores$ave_sentiment
  
  print(
    ggplot(cleaned_data, aes(x = sentiment)) +
      geom_histogram(binwidth = 0.1, fill = "orange", color = "black") +
      labs(title = "Sentiment Distribution of Articles", x = "Sentiment Score", y = "Count") +
      theme_minimal()
  )
} else {
  message("No data for sentiment analysis.")
}

cat("Pipeline finished.\n")
