## Step 0: Opening and manipulating data
## Meltem YILMAZ

library(quanteda)
library(readr)
library(rio)
library(tidyverse)
library(readtext)
#library(udpipe)
#library(R.utils)

## Opening files
datadir <- 'final'
lang <- c("en_US", "de_DE", "fi_FI", "ru_RU")
paths <- vector("list", 4)
for(i in 1:4){
    li <- list.files(path = file.path(datadir, lang[i]))
    pad <- function(x){return(file.path(datadir, lang[i], x))}
    filenames <- lapply(li, pad)
    paths[[i]] <- filenames
}
paths <- setNames(paths, paste0(lang))

## Sample files
set.seed(512)
n <- 4000
con <- file(paths$en_US[[1]], open = "r")
sampdat <- readLines(con, n)
k <- n
while (length(curline <- readLines(con, 1))) {
    k <- k + 1
    if (runif(1) < n/k) {
        sampdat[sample(n, 1)] <- curline
    }
}
close(con)
text <- readLines(textConnection(sampdat))
delaysamp <- read.delim(textConnection(sampdat), header = FALSE, quote = "")

countLines(file.path(paths$en_US[[3]]))

dat <- scan(file.path(paths$en_US[[3]]), what="character", sep="\n")
summary(str_detect(dat, "A computer once beat me at chess, but it was no match for me at kickboxing"))
summary(str_detect(dat, "love"))
summary(str_detect(dat, "hate"))
which(str_detect(dat, "biostats"))
dat[556872]
max(sapply(dat, nchar))

dat <- scan(file.path(paths$en_US[[1]]), what="character", sep="\n")
max(sapply(dat, nchar))

dat <- scan(file.path(paths$en_US[[2]]), what="character", sep="\n")
max(sapply(dat, nchar))

## Making a table of summary statistics
blogs_size   <- file.size(blogs_file) / (2^20)
news_size    <- file.size(news_file) / (2^20)
twitter_size <- file.size(twitter_file) / (2^20)

blogs   <- read_lines(blogs_file)
news    <- read_lines(news_file)
twitter <- read_lines(twitter_file) 

blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

blogs_nchar   <- nchar(blogs)
news_nchar    <- nchar(news)
twitter_nchar <- nchar(twitter)

blogs_nchar_sum   <- sum(blogs_nchar)
news_nchar_sum    <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

blogs_words <- wordcount(blogs, sep = " ")
news_words  <- wordcount(news,  sep = " ")
twitter_words <- wordcount(news, sep = " ")

repo_summary <- data.frame(f_names = c("blogs", "news", "twitter"),
                           f_size  = c(blogs_size, news_size, twitter_size),
                           f_lines = c(blogs_lines, news_lines, twitter_lines),
                           n_char =  c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                           n_words = c(blogs_words, news_words, twitter_words))
repo_summary <- repo_summary %>% mutate(pct_n_char = round(n_char/sum(n_char), 2))
repo_summary <- repo_summary %>% mutate(pct_lines = round(f_lines/sum(f_lines), 2))
repo_summary <- repo_summary %>% mutate(pct_words = round(n_words/sum(n_words), 2))
kable(repo_summary)


