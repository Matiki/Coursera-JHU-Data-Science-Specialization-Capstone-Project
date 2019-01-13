# Load necessary packages into current R session
library(dplyr)
library(tidyr)
library(ggplot2)
library(tm)
library(stringi)
library(tidytext)

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
        tm_map(stripWhitespace) %>%
        tm_map(removeWords, stopwords("en"))

rm(list = c("blog", "news", "twitter", "textSample", "readdata"))

# Create term document matrix
tdm <- corpus %>% 
        TermDocumentMatrix() %>%
        removeSparseTerms(0.9999)

# Create term frequncy data frame
term_freq <- tidy(tdm) %>%
        group_by(term) %>%
        summarise(frequency = sum(count)) %>%
        arrange(desc(frequency))

# Plot most frequent terms 
ggplot(data = term_freq[1:10,],
       aes(x = term, y = frequency)) +
        geom_bar(stat = "identity")
