# Load necessary packages into current R session
library(dplyr)
library(ggplot2)
library(tm)
library(stringi)
library(tidytext)
library(RWeka)
library(stylo)

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
textSample <- function(temp, percent){
        sample(temp, length(temp) * percent)
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

blog <- textSample(blog, 0.01)
news <- textSample(news, 0.01)
twitter <- textSample(twitter, 0.01)

# Create a corpus and clean it up
cleantext <- function(text){
        text %>% 
        VectorSource() %>%
        VCorpus() %>%
        tm_map(content_transformer(tolower)) %>%
        tm_map(removePunctuation) %>%
        tm_map(removeNumbers) %>%
        #tm_map(removeWords, stopwords("en")) %>%
        tm_map(stripWhitespace)
}

corpus <- c(blog, news, twitter) %>%
        cleantext()
        
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
quadgram <- get_freq(corpus, 4)
pentgram <- get_freq(corpus, 5)
hexgram <- get_freq(corpus, 6)

# Clean the data further by separating each word into its own column
firstterm <- function(string){
        txt.to.words(string)[1]
}
secondterm <- function(string){
        txt.to.words(string)[2]
}
thirdterm <- function(string){
        txt.to.words(string)[3]
}
fourthterm <- function(string){
        txt.to.words(string)[4]
}
fifthterm <- function(string){
        txt.to.words(string)[5]
}
sixthterm <- function(string){
        txt.to.words(string)[6]
}

bigram$first <- apply(bigram, 1, firstterm)
bigram$second <- apply(bigram, 1, secondterm)

trigram$first <- apply(trigram, 1, firstterm)
trigram$second <- apply(trigram, 1, secondterm)
trigram$third <- apply(trigram, 1, thirdterm)

quadgram$first <- apply(quadgram, 1, firstterm)
quadgram$second <- apply(quadgram, 1, secondterm)
quadgram$third <- apply(quadgram, 1, thirdterm)
quadgram$fourth <- apply(quadgram, 1, fourthterm)

pentgram$first <- apply(pentgram, 1, firstterm)
pentgram$second <- apply(pentgram, 1, secondterm)
pentgram$third <- apply(pentgram, 1, thirdterm)
pentgram$fourth <- apply(pentgram, 1, fourthterm)
pentgram$fifth <- apply(pentgram, 1, fifthterm)

hexgram$first <- apply(hexgram, 1, firstterm)
hexgram$second <- apply(hexgram, 1, secondterm)
hexgram$third <- apply(hexgram, 1, thirdterm)
hexgram$fourth <- apply(hexgram, 1, fourthterm)
hexgram$fifth <- apply(hexgram, 1, fifthterm)
hexgram$sixth <- apply(hexgram, 1, sixthterm)

alldata <- list(unigram, bigram, trigram, quadgram, pentgram, hexgram)

# Save files for later use
if(!dir.exists("finaldata")){
        dir.create("finaldata")
}

saveRDS(unigram, "finaldata/unigram.RData")
saveRDS(bigram, "finaldata/bigram.RData")
saveRDS(trigram, "finaldata/trigram.RData")
saveRDS(quadgram, "finaldata/quadgram.RData")
saveRDS(pentgram, "finaldata/pentgram.RData")
saveRDS(hexgram, "finaldata/hexgram.RData")

saveRDS(alldata, "finaldata/alldata.RData")

rm(list = ls())

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

# Read the data 
unigram <- readRDS("finaldata/unigram.RData")
bigram <- readRDS("finaldata/bigram.RData")
trigram <- readRDS("finaldata/trigram.RData")
quadgram <- readRDS("finaldata/quadgram.RData")
pentgram <- readRDS("finaldata/pentgram.RData")
hexgram <- readRDS("finaldata/hexgram.RData")

alldata <- readRDS("finaldata/alldata.RData")

# Function for predicting the next word
nextword <- function(input){
        # Clean the input text and initialize prediction as NA
        text <- cleantext(input)[[1]][1] %>%
                as.character() %>%
                txt.to.words()
        prediction <- NA
        # Call appropriate prediction function based on input length
        while(is.na(prediction)){
                if(length(text) > 5){
                        text <- tail(text, 5)
                        cleaninput <- paste(text, collapse = " ")
                }else if(length(text) == 5){
                        prediction <- hexpred(text)
                }else if(length(text) == 4){
                        prediction <- pentpred(text)
                }else if(length(text) == 3){
                        prediction <- quadpred(text)
                }else if(length(text) == 2){
                        prediction <- tripred(text)
                }else if(length(text) == 1){
                        prediction <- bipred(text)
                }else if(length(text) <= 0){
                        stop(return("ERROR: No match found"))
                }
                # If no word is found, shorten input, try again
                if(is.na(prediction)){
                        text <- tail(text, length(text) - 1)
                        cleaninput <- paste(text, collapse = " ")
                }
        }
        return(as.character(prediction))
}

# Prediction functions
hexpred <- function(text){
        hexgram[hexgram$first == text[1] &
                hexgram$second == text[2] &
                hexgram$third == text[3] &
                hexgram$fourth == text[4] &
                hexgram$fifth == text[5],8][1,1]
}

pentpred <- function(text){
        pentgram[pentgram$first == text[1] &
                 pentgram$second == text[2] &
                 pentgram$third == text[3] &
                 pentgram$fourth == text[4],7][1,1]
}


quadpred <- function(text){
        quadgram[quadgram$first == text[1] &
                 quadgram$second == text[2] &
                 quadgram$third == text[3],6][1,1]
}

tripred <- function(text){
        trigram[trigram$first == text[1] &
                trigram$second == text[2],5][1,1]
}

bipred <- function(text){
        bigram[bigram$first == text[1],4][1,1]
}
