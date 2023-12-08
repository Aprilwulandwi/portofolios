#read data
library(readxl)
cleandata<- read.csv("C:/Users/wulandari/Downloads/2043201003_Aprilia Dwi W.csv")
View(cleandata)

#convert to corpus
library(tm)
corpus_text <- Corpus(VectorSource(cleandata$English))
inspect(corpus_text)

#calculate DTM
DTM <- DocumentTermMatrix(corpus_text)
inspect(DTM)

#remove empty doc
rowTotals <- apply(DTM, 1, sum)
dtm.new <- DTM[rowTotals> 0,]

#define topics
library(topicmodels)
ap_lda <- LDA(dtm.new, k=2, control=list(seed = 1234))
ap_lda

#
#word-topic probabilities
library(tidytext)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

#most term in each topic
library(ggplot2)
library(dplyr)
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#difference in β between topic 1 and topic 2
library(tidyr)
beta_wide <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))
beta_wide

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n=10) %>%
  ungroup() %>%
  mutate(term=reorder(term,log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x="Log2 ratio of beta in topic 2 / topic 1", y=NULL)

#document-topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

#check what the most common words in that document were
tidy(dtm.new) %>%
  filter(document == 10) %>%
  arrange(desc(count))

