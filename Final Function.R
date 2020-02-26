library(quanteda)
library(stringr)
library(data.table)
library(stringi)
set.seed(510)
options(download.file.method = "libcurl")
quanteda_options(threads=8)

profanity <- readLines(url("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"))
close(url("https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"))

corpblogs <- corpus (readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE))
corptwit <- corpus (readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE))
corpnews <- corpus (readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE))

#combining all the corpi together to analyze them at once
corpall <- corpblogs+corptwit+corpnews

corpallsampled <- corpus_sample(corpall, size=length(texts(corpall))*0.3)

toks0<- tokens (corpallsampled, remove_punct = TRUE, remove_numbers = TRUE)
toks11 <- tokens_remove(toks0, pattern =  profanity)

toks_ngram21 <- tokens_ngrams(toks11, n = 2, concatenator = " ")
toks_ngram31 <- tokens_ngrams(toks11, n = 3, concatenator = " ")
toks_ngram41 <- tokens_ngrams(toks11, n = 4, concatenator = " ")

dfm_toks2 <- dfm(toks_ngram21)
dfm_toks3 <- dfm(toks_ngram31)
dfm_toks4 <- dfm(toks_ngram41)

dfm_toks2 <- dfm_trim(dfm_toks2, min_termfreq = 4, verbose = TRUE)
dfm_toks3 <- dfm_trim(dfm_toks3, min_termfreq = 4, verbose = TRUE)
dfm_toks4 <- dfm_trim(dfm_toks4, min_termfreq = 4, verbose = TRUE)

toks1_freqs <- sort(featfreq(dfm_toks1), decreasing = TRUE)
toks2_freqs <- sort(featfreq(dfm_toks2), decreasing = TRUE)
toks3_freqs <- sort(featfreq(dfm_toks3), decreasing = TRUE)
toks4_freqs <- sort(featfreq(dfm_toks4), decreasing = TRUE)

toks1_freqs_normal <- toks1_freqs / sum(toks1_freqs) 
toks1_freqs_normal <- sort(toks1_freqs_normal, decreasing = TRUE)

toks2_freqs_normal <- toks2_freqs[names(toks2_freqs)] / toks1_freqs[onegram]
toks2_freqs_normal <- sort(toks2_freqs_normal, decreasing = TRUE)

toks3_freqs_normal <- toks3_freqs[names(toks3_freqs)] / toks2_freqs[twograms]
toks3_freqs_normal <- sort(toks3_freqs_normal, decreasing = TRUE)

toks4_freqs_normal <- toks4_freqs[names(toks4_freqs)] / toks3_freqs[threegrams]
toks4_freqs_normal <- sort(toks4_freqs_normal, decreasing = TRUE)

nam1 <- as.data.table(names(toks1_freqs_normal))
nam2 <- as.data.table(names(toks2_freqs_normal))
nam3 <- as.data.table(names(toks3_freqs_normal))
nam4 <- as.data.table(names(toks4_freqs_normal))

onegram_of_2toks <- word(nam2$V1, 1)
prediction_of_2toks <- word(nam2$V1, 2)

twograms_of_3toks <- word(nam3$V1, 1,2)
prediction_of_3toks <- word(nam3$V1, 3)

threegrams_of_4toks <- word(nam4$V1, 1,3)
prediction_of_4toks <- word(nam4$V1, 4)

twograms_predictions <- data.table(entry= onegram_of_2toks, prediction = prediction_of_2toks, value_normal = toks2_freqs_normal, value_firstbackoff = toks2_freqs_normal*0.4, value_secondbackoff = toks2_freqs_normal*0.4*0.4)

threegrams_predictions <- data.table(entry= twograms_of_3toks, prediction = prediction_of_3toks, value_normal = toks3_freqs_normal, value_firstbackoff = toks3_freqs_normal*0.4)

fourgrams_predictions <- data.table(entry= threegrams_of_4toks, prediction = prediction_of_4toks, value_normal = toks4_freqs_normal)

mfw <- c('the', 'on', 'a')

find_next_word <- function (strings){
      
      strings <- str_trim(tolower(gsub('[[:punct:]|[:digit:]]+','',  strings, perl = TRUE)))
      length_of_entry <- length(unlist(strsplit(strings, "\\s+")))
      
      
      if (identical(length_of_entry, integer(0))){
            length_of_entry <- 0
      }
      
      if (length_of_entry==1) {
            li <- twograms_predictions[entry == strings]$prediction[1:3]
            
                  ifelse(is.na(
                        li
                  ), mfw[1:length(which(is.na(li)))],
                  li
                  )
            
            
      }
      else if (length_of_entry==2) {
            threegramsprob <- threegrams_predictions[entry == strings]$prediction[1:3]
            if (sum(!is.na(threegramsprob))==0){
                  strings <- word(strings, -1)
                  li <- twograms_predictions[entry == strings]$prediction[1:3]
                  
                        ifelse(is.na(li),
                               mfw[1:length(which(is.na(li)))]
                               ,li
                        )
                  
                  
            }
            else{
                  
                        ifelse (is.na(threegramsprob),
                                mfw[1:length(which(is.na(threegramsprob)))]     
                                ,threegramsprob
                        )
                  
            }
      }
      
      else if (length_of_entry>=3) {
            strings <- word(strings, -3 ,-1)
            fourgramsprob <- fourgrams_predictions[entry == strings]$prediction[1:3]
            
            #if fourgrams don't exist
            if (sum(!is.na(fourgramsprob))==0){
                  strings <- word(strings, -2 ,-1)
                  threegramsprob <- threegrams_predictions[entry == strings]$prediction[1:3]
                  #two cases, three grams exist, and 3-grams don't exist then we go to 2-grams
                  
                  #if 3-grams don't exist
                  if (sum(!is.na(threegramsprob))==0){
                        #backoff to 2 gram
                        strings <- word(strings, -1)
                        twogramsprob <- twograms_predictions[entry == strings]$prediction[1:3]
                        
                              ifelse(is.na(twogramsprob),
                                     mfw[1:length(which(is.na(twogramsprob)))]
                                     ,twogramsprob
                              )
                        
                        
                  }
                  
                  #if three grams exist
                  else {
                        
                              ifelse (is.na(threegramsprob),
                                      mfw[1:length(which(is.na(threegramsprob)))]     
                                      ,threegramsprob
                              )
                        
                  }
                  
            }
            #if fourgrams exist
            else {
                  
                        ifelse(is.na(fourgramsprob ),
                               mfw[1:length(which(is.na(fourgramsprob )))]
                               ,fourgramsprob
                        )
                  
            }
            
      }
      
      else {
            mfw
      }
}