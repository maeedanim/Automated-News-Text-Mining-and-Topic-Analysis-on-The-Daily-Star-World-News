# Automated-News-Text-Mining-and-Topic-Analysis-on-The-Daily-Star-World-News

This project focuses on automating the process of collecting, cleaning, and analyzing news data from The Daily Star’s World News section. It applies advanced text mining and NLP techniques to extract meaningful patterns, discover hidden topics, and analyze sentiment from news articles.

🔹 1. Data Collection

The project scrapes real-world articles from The Daily Star website using the rvest and xml2 libraries.

It extracts each article’s title, URL, and paragraph content, storing them in a structured format.

🔹 2. Data Cleaning & Preprocessing

All text data undergoes deep preprocessing:

Converts to lowercase

Removes punctuation, symbols, emojis, and contractions

Filters stopwords and short tokens

Applies lemmatization (reduces words to their root forms)

Uses spell checking to retain valid English tokens

This ensures a clean and standardized corpus for NLP analysis.

🔹 3. Feature Extraction

The cleaned text is transformed into tokens.

Two major features are analyzed:

Word Frequency Analysis: Identifies the most common words in news articles.

TF-IDF (Term Frequency–Inverse Document Frequency): Highlights words that are most important in each article, removing generic frequent words.

🔹 4. Topic Modeling (LDA Algorithm)

The project applies Latent Dirichlet Allocation (LDA) to discover hidden themes within the articles.

Each article is represented as a mix of multiple topics, each topic being a distribution of keywords.

The top 5 topics are extracted and visualized to show dominant themes in world news.

🔹 5. Sentiment Analysis

Uses the sentimentr package to calculate average sentiment scores for each article.

Plots a histogram to visualize the emotional distribution (positive, negative, neutral) of the news dataset.

🔹 6. Visualization

Multiple plots are generated using ggplot2 and wordcloud:

Top 20 most frequent words

Top 20 TF-IDF words

Topic distribution (pie chart)

Individual word clouds per article

Sentiment histogram

🔹 7. Output Files

Two CSV files are saved:

Raw_Article.csv — contains unprocessed article text and metadata.

Cleaned_Article.csv — contains cleaned, tokenized, and ready-to-analyze text.
