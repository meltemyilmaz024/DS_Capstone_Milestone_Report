## Exploratory analysis
## meltemyilmaz024@gmail.com

# Libraries
library(quanteda)
library(readr)
library(readtext)
library(stringi)
library(ggsci)
library(stringr)
library(ggplot2)

## Opening files
data <- readtext("texts/*.txt",
                   docvarsfrom = "filenames", 
                   docvarnames = c("lang", "type"),
                   dvsep = "-", 
                   encoding = "utf-8")

corp <- corpus(data)

corpsentc <- corpus_reshape(corp, to=c("sentences"), use_docvars = TRUE)
corps <- corpus_sample(corpsentc, size = 5000, replace = FALSE, by = "document")
corps2 <- corpus_reshape(corps, to = "documents")

dtm <- dfm(corps2, tolower=TRUE, stem=FALSE, remove=stopwords('en'), remove_punct=TRUE,
           remove_numbers = TRUE, remove_symbols = TRUE)
dtm <- dfm_trim(dtm, min_termfreq = 10)

# Some words are more frequent than others - what are the distributions of word frequencies? 
textplot_wordcloud(dtm, max_words = 50)     ## top 50 (most frequent) words
textplot_wordcloud(dtm, max_words = 50, comparison = TRUE)
textstat_frequency(dtm, n = 10, groups = "docid_")           ## view the frequencies 

k = kwic(corps2, 'freedom', window = 7)
head(k, 10)    ## only view first 10 results


g <- textplot_xray(
    kwic(corps2, pattern = "american"),
    kwic(corps2, pattern = "love"),
    kwic(corps2, pattern = "work"),
    kwic(corps2, pattern = "politi*"),
    kwic(corps2, pattern = "scien*")
)
g + aes(color = keyword) + 
    scale_color_jama() +
    theme_minimal() +
    theme(legend.position = "none")


# Plot estimated word keyness
type_corp <- corpus_subset(corps2, 
                             type %in% c("twitter", "news"))
typedfm <- dfm(type_corp, groups = "type", tolower=TRUE, stem=FALSE, remove=stopwords('en'), remove_punct=TRUE,
               remove_numbers = TRUE, remove_symbols = TRUE)
result_keyness <- textstat_keyness(typedfm, target = "twitter")
textplot_keyness(result_keyness) 

# What are the frequencies of 2-grams and 3-grams in the dataset? 
toks <- tokens(corps2, remove_punct=TRUE, remove_numbers = TRUE, remove_symbols = TRUE)
toks <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")

toks_ngram <- tokens_ngrams(toks, n = 2)
dfngram <- dfm(toks_ngram)
tstat_freq <- textstat_frequency(dfngram)
head(tstat_freq, 20)

dfngram %>% 
    textstat_frequency(n = 15) %>% 
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    theme_minimal() +
    ggtitle("2-gram frequencies")

toks_ngram <- tokens_ngrams(toks, n = 3)
dfngram <- dfm(toks_ngram)
tstat_freq <- textstat_frequency(dfngram)
head(tstat_freq, 20)

dfngram %>% 
    textstat_frequency(n = 15) %>% 
    ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
    geom_point() +
    coord_flip() +
    labs(x = NULL, y = "Frequency") +
    theme_minimal() +
    ggtitle("3-gram frequencies")
    

