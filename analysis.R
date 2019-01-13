library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(stringi)

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
        readLines(paste("./final/en_US/", x, sep = ""),
                  encoding = "UTF-8",
                  skipNul = TRUE)
}

# Create function to sample the data
textSample <- function(temp){
        sample(temp, length(temp) * .01)
}

# Use functions to read and sample the data
set.seed(12345)

blog <- readdata("en_US.blogs.txt") %>% textSample()
news <- readdata("en_US.news.txt") %>% textSample()
twitter <- readdata("en_US.twitter.txt") %>% textSample()

# Create a corpus and clean it up
corpus <- c(blog, news, twitter) %>% 
        VectorSource() %>%
        VCorpus() %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        tm_map(stripWhitespace)

rm(list = c("blog", "news", "twitter", "textSample", "readdata"))

# Create term document matrix
tdm <- TermDocumentMatrix(corpus)
m <- rowSums(as.matrix(tdm))

# sum the rows and sort by frequency
term_freq <- rowSums(tdm) %>% sort(decreasing = TRUE)

# Plot frequent terms using qdap package
library(qdap)
freq <- freq_terms(corpus,
                   top = 10,
                   at.least = 3,
                   stopwords = "Top200Words")
plot(freq)