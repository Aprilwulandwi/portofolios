# Pengumpulan Data
teks<-c("Ibu membeli sayur setiap hari di Pasar Rebo.","Pasar itu berjarak kurang lebih 1 km dari rumah.",
"Ibu sangat senang belanja di pasar itu karena harganya murah.","Setiap kali belanja, ibu berangkat diantar ayah.")
teks

# Pre-processing

library(stringr)
teks <- str_to_lower(teks)
teks <- str_remove_all(teks,"\\.")
teks <- str_remove_all(teks,",")
teks <- str_remove_all(teks,"di")
teks <- str_remove_all(teks,"itu")
teks <- str_remove_all(teks,"1 km")
stopw<- readLines("stop_words_ind.txt")
teks <- removeWords(teks,stopw)

library(dplyr)
teks_tb <- tibble(line = 1:4, text = teks)
teks_tb

library(tm)
# formating corpus
teks_corpus <- VCorpus(VectorSource(teks))
teks_corpus <- tm_map(teks_corpus,removeWords, stopw)

# Matrik dokumen
teks_dtm <- DocumentTermMatrix(teks_corpus)
teks_dtm
inspect(teks_dtm)
Docs(teks_dtm)     # Access Document IDs and Terms
nDocs(teks_dtm)    # Mengetahui banyak dokumen
nTerms(teks_dtm)   # Mengetahui banyak terms
Terms(teks_dtm)    # Menampilkan Term


mat_teks_dtm <-as.matrix(teks_dtm)
mat_teks_dtm
f <- sort(colSums(mat_teks_dtm),decreasing=TRUE)
f
dat <- data.frame(word=names(f), freq=f)
head(dat, 5)

# Visualization
library(ggplot2)
ggplot(dat,aes(x=reorder(word,freq),y=freq)) +
  geom_col()  +
  coord_flip() +
  labs(y="frequency", x="kata")

library(forcats)
ggplot(dat,aes(x=fct_reorder(word,freq),y=freq)) +
  geom_col() +
  coord_flip() +
  labs(y="frequency", x="kata")

library(wordcloud)
library(RColorBrewer)
set.seed(100)
wordcloud(words = dat$word, freq = dat$freq, min.freq=1, random.order=FALSE)
wordcloud(words = dat$word, freq = dat$freq, min.freq=1, random.order=FALSE, colors=brewer.pal(4, "Dark2"))

library(wordcloud2)
wordcloud2(data=dat, size=.5, color='random-dark')

#==========================================================

# TF-IDF

library(tidytext)
teks_tf <- teks_tb %>%
  unnest_tokens(word, text) %>%
  count(line,word,sort=TRUE)
teks_tf
total_kata <- teks_tf %>% 
  group_by(line) %>% 
  summarize(total = sum(n))
total_kata

line_kata <- left_join(teks_tf, total_kata)
line_kata
line_kata<-line_kata %>% 
  mutate(tf=n/total)
line_kata

teks_df<-teks_tf %>%
  group_by(word) %>%
  count(word) %>%
  rename(df=n)
teks_df  
 
line_tf_idf<-left_join(line_kata,teks_df)  %>% 
  mutate(idf=log(length(teks)/df)) %>%
  mutate(tf_idf=tf*idf)
line_tf_idf

#bind_tf_idf
bind_tf_idf <- teks_tb %>%
  unnest_tokens(word, text) %>%
  count(line,word,sort=TRUE) %>%
  bind_tf_idf(word,line, n)
bind_tf_idf

# Bagaimana menghitung idf







teks1 = dat[!is.element(dat$word, stopw),]
