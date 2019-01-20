# Load necessary packages into current R session
library(dplyr)
library(ggplot2)
library(tm)
library(stringi)
library(tidytext)
library(RWeka)

# Download Data Files
if (!file.exists("Coursera-SwiftKey.zip")){
        URL <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(URL, 
                      destfile = "Coursera-SwiftKey.zip", 
                      method = "curl", 
                      quote = "")
        rm(URL)
}

# Unzip Data
if(!file.exists("./final")){
        unzip("Coursera-SwiftKey.zip")
}

# Create function to read the data
readdata <- function(x){
        con <- file(paste("./final/en_US/", x, sep = ""), "r")
        temp <- readLines(con,
                          encoding = "UTF-8",
                          skipNul = TRUE)
        close(con)
        return(temp)
}

# Create function to sample the data
textSample <- function(temp){
        sample(temp, length(temp) * .01)
}

# Read the data from the three files
blog <- readdata("en_US.blogs.txt")
news <- readdata("en_US.news.txt")
twitter <- readdata("en_US.twitter.txt")

# Summarize the three files
file_summary <- data.frame(File = c("blog", "news", "twitter"),
                           lines = c(length(blog), length(news), length(twitter)),
                           mean_num_words = c(mean(stri_count_words(blog)),
                                              mean(stri_count_words(news)),
                                              mean(stri_count_words(twitter))),
                           total_num_words = c(sum(stri_count_words(blog)),
                                               sum(stri_count_words(news)),
                                               sum(stri_count_words(twitter))))

# Use function to sample the data
set.seed(12345)

blog <- textSample(blog)
news <- textSample(news)
twitter <- textSample(twitter)

# Create a corpus and clean it up
corpus <- c(blog, news, twitter) %>% 
        VectorSource() %>%
        VCorpus() %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(stripWhitespace) %>%
        tm_map(removeWords, stopwords("en"))

rm(list = c("blog", "news", "twitter", "textSample", "readdata", "file_summary"))

# Create function to make N-gram dataframe 
get_freq <- function(corpus, ngramcount){
        token <- function(x){
                NGramTokenizer(x,
                               Weka_control(min = ngramcount,
                                            max = ngramcount))
        }
        TermDocumentMatrix(corpus, 
                           control = list(tokenize = token)) %>%
                removeSparseTerms(0.9999) %>%
                tidy() %>%
                group_by(term) %>%
                summarize(frequency = sum(count)) %>%
                arrange(desc(frequency))
}

# Make n-gram dataframes with list of most frequent terms
unigram <- get_freq(corpus, 1)
bigram <- get_freq(corpus, 2)
trigram <- get_freq(corpus, 3)

# Save files for later use
if(!dir.exists("finaldata")){
        dir.create("finaldata")
}else{
        saveRDS(unigram, "finaldata/unigram.RData")
        saveRDS(bigram, "finaldata/bigram.RData")
        saveRDS(trigram, "finaldata/trigram.RData")
}

# Plot most common terms
myplot <- function(data, xlabel = ""){
        ggplot(data = data[1:10,],
               aes(x = reorder(term, -frequency), 
                   y = frequency)) +
                geom_bar(stat = "identity") +
                labs(x = xlabel,
                     y = "Frequency") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

myplot(unigram, "Unigram")
myplot(bigram, "Bigram")
myplot(trigram, "Trigram")

rm(list = ls())
