rm(list = ls())
#install.packages("tidytext")
#install.packages("topicmodels")
#install.packages("ldatuning")
#install.packages("stringi")
#install.packages("rjson")
#install.packages('doParallel')
#install.packages('lsa')
#install.packages('tcR')
#install.packages('bursts')
#install.packages('stm')
install.packages('Rtsne')
install.packages('rsvd')
install.packages('geometry')
libraries <- c("ldatuning", "topicmodels", "ggplot2", "dplyr", "rjson", "quanteda", "lubridate", "parallel", "doParallel", "tidytext", "stringi", "tidyr")
lapply(libraries, require, character.only = TRUE)
library(quanteda)
library(quanteda.corpora)
library(dplyr)
library(lsa)
library(tcR)
library(stm)
library(factoextra)
library(bursts)
library(text2vec)
library(Rtsne)
library(rsvd)
library(geometry)
# 1.(a)
# The table that shows how many documents are associated with each newspaper:
#  ft  guardian       sun telegraph     times 
# 207       392       277       511       352
data("data_corpus_immigrationnews")
sotu <- corpus_subset(data_corpus_immigrationnews, 
                      paperName == 'telegraph'|paperName =='guardian'|paperName =='ft'|paperName == 'times'|paperName == 'sun')
paperName_table <- table(docvars(sotu, 'paperName'))
# 1.(b)
# There are 2776 features and 1739 documents in the DFM.
sotu_dfm <- dfm(sotu, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
sotu_dfm <- dfm_trim(sotu_dfm, min_termfreq = 30, min_docfreq = 20)
dim(sotu_dfm)
# 1.(c)
# 1.(d)
# the @loglikeli- hood of my topic model object is -2575792.
k <- 30
system.time(
  sotu_tm <- LDA(sotu_dfm, k = k, method = "Gibbs",  iter = 3000, control = list(seed = 10012)))
sotu_tm@loglikelihood
# 1.(e)
top_terms <- get_terms(sotu_tm, 10)
assigned_topics <- topics(sotu_tm)
ranked_topics <- sort(table(assigned_topics), decreasing = TRUE)
ranked_topics
names(ranked_topics)[1:5] <- c('ukip_30', 'parti_28', 'migrat_12', 'minist_2', 'case_15')
ranked_topics
# 1.(f)
# times
sotu_times <- corpus_subset(data_corpus_immigrationnews, 
                          paperName == 'times')
sotu_times$documents$day <- as.numeric(sotu_times$documents$day)
sotu_times <- sotu_times$documents %>% group_by(day) %>% arrange(day)
sotu_times_dfm <- dfm(sotu_times$texts, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
#sotu_times_dfm <- dfm_trim(sotu_times_dfm, min_termfreq = 30, min_docfreq = 20)
system.time(
  sotu_times_tm <- LDA(sotu_times_dfm, k = k, method = "Gibbs",  control = list(seed = 10012)))
doc_topics <- sotu_times_tm@gamma
doc_topics <- t(doc_topics)
dim(doc_topics)
doc_topics[1:5,1:5]
max <- apply(doc_topics, 2, which.max)
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)
#newspaper <- c('Sun', 'Times')
top2 <- data.frame(top_topic = max, second_topic = max2, date = sotu_times$day)
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First"))
blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) + geom_point(aes(x=date, y=top_topic, pch="First") ) + scale_shape_manual(values=c(18, 1), name = "Topic Rank")
# sun
sotu_sun <- corpus_subset(data_corpus_immigrationnews, 
                            paperName == 'sun')
sotu_sun$documents$day <- as.numeric(sotu_sun$documents$day)
sotu_sun <- sotu_sun$documents %>% group_by(day) %>% arrange(day)
sotu_sun_dfm <- dfm(sotu_sun$texts, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
#sotu_times_dfm <- dfm_trim(sotu_times_dfm, min_termfreq = 30, min_docfreq = 20)
system.time(
  sotu_sun_tm <- LDA(sotu_sun_dfm, k = k, method = "Gibbs",  control = list(seed = 10012)))
doc_topics <- sotu_sun_tm@gamma
doc_topics <- t(doc_topics)
dim(doc_topics)
doc_topics[1:5,1:5]
max <- apply(doc_topics, 2, which.max)
which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}

max2 <- apply(doc_topics, 2, which.max2)
max2 <- sapply(max2, max)
#newspaper <- c('Sun', 'Times')
top2 <- data.frame(top_topic = max, second_topic = max2, date = sotu_sun$day)
blm_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First"))
blm_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) + geom_point(aes(x=date, y=top_topic, pch="First") ) + scale_shape_manual(values=c(18, 1), name = "Topic Rank")
# 1.(g)
# time
# 0.005681818 0.113636364 0.008522727 0.014204545 0.034090909
time_topics <- topics(sotu_times_tm)
ranked_topics_time <- sort(table(time_topics), decreasing = TRUE)
ranked_topics_time
time_topics_ratio <- c(2/352, 40/352, 3/352, 5/352, 12/352)
time_topics_ratio
# sun
# 0.028880866 0.010830325 0.097472924 0.007220217 0.007220217
sun_topics <- topics(sotu_sun_tm)
ranked_topics_sun <- sort(table(sun_topics), decreasing = TRUE)
ranked_topics_sun
ranked_topics
sun_topics_ratio <- c(8/277, 3/277, 27/277, 2/277, 2/277)
sun_topics_ratio
# ft
# 0.01932367 0.02415459 0.03864734 0.01449275 0.03864734
sotu_ft <- corpus_subset(data_corpus_immigrationnews, 
                            paperName == 'ft')
sotu_ft$documents$day <- as.numeric(sotu_ft$documents$day)
sotu_ft <- sotu_ft$documents %>% group_by(day) %>% arrange(day)
sotu_ft_dfm <- dfm(sotu_ft$texts, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
#sotu_times_dfm <- dfm_trim(sotu_times_dfm, min_termfreq = 30, min_docfreq = 20)
system.time(
  sotu_ft_tm <- LDA(sotu_ft_dfm, k = k, method = "Gibbs",  control = list(seed = 10012)))
ft_topics <- topics(sotu_ft_tm)
ranked_topics_ft <- sort(table(ft_topics), decreasing = TRUE)
ranked_topics_ft
ranked_topics
ft_topics_ratio <- c(4/207, 5/207, 8/207, 3/207, 8/207)
ft_topics_ratio
# guardian
# 0.01530612 0.03316327 0.01530612 0.01785714 0.02040816
sotu_guardian <- corpus_subset(data_corpus_immigrationnews, 
                         paperName == 'guardian')
sotu_guardian$documents$day <- as.numeric(sotu_guardian$documents$day)
sotu_guardian <- sotu_guardian$documents %>% group_by(day) %>% arrange(day)
sotu_guardian_dfm <- dfm(sotu_guardian$texts, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
#sotu_times_dfm <- dfm_trim(sotu_times_dfm, min_termfreq = 30, min_docfreq = 20)
system.time(
  sotu_guardian_tm <- LDA(sotu_guardian_dfm, k = k, method = "Gibbs",  control = list(seed = 10012)))
guardian_topics <- topics(sotu_guardian_tm)
ranked_topics_guardian <- sort(table(guardian_topics), decreasing = TRUE)
ranked_topics_guardian
ranked_topics
guardian_topics_ratio <- c(6/392, 13/392, 6/392, 7/392, 8/392)
guardian_topics_ratio
# telegraph
# 0.037181996 0.005870841 0.029354207 0.007827789 0.017612524
sotu_telegraph <- corpus_subset(data_corpus_immigrationnews, 
                               paperName == 'telegraph')
sotu_telegraph$documents$day <- as.numeric(sotu_telegraph$documents$day)
sotu_telegraph <- sotu_telegraph$documents %>% group_by(day) %>% arrange(day)
sotu_telegraph_dfm <- dfm(sotu_telegraph$texts, stem = T, remove_punct = T, remove_numbers = T, tolower = T, remove = custom_stopwords)
#sotu_times_dfm <- dfm_trim(sotu_times_dfm, min_termfreq = 30, min_docfreq = 20)
system.time(
  sotu_telegraph_tm <- LDA(sotu_telegraph_dfm, k = k, method = "Gibbs",  control = list(seed = 10012)))
telegraph_topics <- topics(sotu_telegraph_tm)
ranked_topics_telegraph <- sort(table(telegraph_topics), decreasing = TRUE)
ranked_topics_telegraph
ranked_topics
telegraph_topics_ratio <- c(19/511, 3/511, 15/511, 4/511, 9/511)
telegraph_topics_ratio
# 2.(a)
# -2565838
system.time(
  sotu_tm_stable <- LDA(sotu_dfm, k = k, method = "Gibbs",  iter = 3000, control = list(seed = 1992)))
sotu_tm_stable@loglikelihood
# 2.(b)
#  28 21 22 11  1 24 20  8 25  7 11 16 10  9 13  2  5 27 27  4 15  8 18 14  1 17  3 19  6 19
calculate_cosine_similarity <- function(vec1, vec2) { 
  nominator <- vec1 %*% vec2  # %*% specifies dot product rather than entry by entry multiplication (we could also do: sum(x * y))
  denominator <- sqrt(vec1 %*% vec1)*sqrt(vec2 %*% vec2)
  return(nominator/denominator)
}
sotu_topics <- tidy(sotu_tm, matrix = 'beta')
sotu_topics_stable <- tidy(sotu_tm_stable, matrix = 'beta')
sotu_terms <- sotu_topics %>%
  group_by(topic)
sotu_terms_stable <- sotu_topics_stable %>%
  group_by(topic)
sotu_terms[sotu_terms$topic==1,]$beta
cos <- matrix(nrow = 30, ncol = 30, data=NA)
for (i in 1:30){
  for(j in 1:30){
    topic1 <- sotu_terms[sotu_terms$topic==j,]$beta;
    topic2 <- sotu_terms_stable[sotu_terms_stable$topic==i,]$beta;
    cos[i,j]=
      calculate_cosine_similarity(topic1,topic2)
  }
}
apply(cos,1,which.max)
cos <- as.data.frame.matrix(cos)
names(cos) <- seq(1:ncol(cos))
rownames(cos) <- seq(1:nrow(cos))
cos$top_match <- colnames(cos)[apply(cos,1, which.max)]
original_matches <- data.frame(cbind(seq(1:nrow(cos)), cos$top_match))
names(original_matches) <- c("original", "match")
# 2.(c)
sotu_terms <- sotu_topics %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
sotu_terms_stable <- sotu_topics_stable %>% group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
sotu_terms <- data.frame(sotu_terms)
sotu_terms_stable <- data.frame(sotu_terms_stable)
for (i in 1:nrow(original_matches)) {
  number = as.numeric(original_matches$original[i])
  words = sotu_terms[sotu_terms['topic'] == number,]
  words = words$term
  number_stable = as.numeric(original_matches$match[i])
  words_stable = sotu_terms_stable[sotu_terms_stable['topic'] == number_stable ,]
  words_stable = words_stable$term
  num_matches = length(intersect(words, words_stable))
  original_matches$average[i] = num_matches 
  
}
original_matches$average
#  [1] 0 2 0 1 0 0 0 0 3 1 1 1 1 0 0 1 0 0 0 1 1 0 0 0 1 0 0 0 2 0
# average shared: 0.05333333
16/300
# 2.(d)
sotu_tm_new1 <- LDA(sotu_dfm, k = 5, method = "Gibbs",  iter = 3000, control = list(seed = 10012))
sotu_tm_new2 <- LDA(sotu_dfm, k = 5, method = "Gibbs",  iter = 3000, control = list(seed = 1992))
sotu_topics_new <- tidy(sotu_tm_new1, matrix = 'beta')
sotu_topics_stable_new <- tidy(sotu_tm_new2, matrix = 'beta')
sotu_terms_new <- sotu_topics_new %>%
  group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
sotu_terms_stable_new <- sotu_topics_stable_new %>%
  group_by(topic) %>% top_n(10, beta) %>% ungroup() %>% arrange(topic, -beta)
sotu_terms_new <- data.frame(sotu_terms_new)
sotu_terms_stable_new <- data.frame(sotu_terms_stable_new)

cos2 <- matrix(nrow = 5, ncol = 5, data=NA)
for (i in 1:5){
  for(j in 1:5){
    topic1 <- sotu_terms_new[sotu_terms_new$topic==j,]$beta;
    topic2 <- sotu_terms_stable_new[sotu_terms_stable_new$topic==i,]$beta;
    cos2[i,j]=
      calculate_cosine_similarity(topic1,topic2)
  }
}
apply(cos2,2,which.max)
cos2 <- as.data.frame.matrix(cos2)
names(cos2) <- seq(1:ncol(cos2))
rownames(cos2) <- seq(1:nrow(cos2))
cos2$top_match <- colnames(cos2)[apply(cos2,1, which.max)]
original_matches1 <- data.frame(cbind(seq(1:nrow(cos2)), cos2$top_match))
names(original_matches1) <- c("original", "match")
for (i in 1:nrow(original_matches2)) {
  number = as.numeric(original_matches2$original[i])
  words = sotu_terms_new[sotu_terms_new['topic'] == number,]
  words = words$term
  number_stable = as.numeric(original_matches2$match[i])
  words_stable = sotu_terms_stable_new[sotu_terms_stable_new['topic'] == number_stable ,]
  words_stable = words_stable$term
  num_matches = length(intersect(words, words_stable))
  original_matches2$average[i] = num_matches 
  
}

original_matches2$average
# 8 9 8 9 9
43/50
# average shared: 0.86
# 3.(a)
sotu_sun_time <- corpus_subset(data_corpus_immigrationnews, 
                      paperName == 'times'|paperName == 'sun')
sotu_sun_time$documents$day <- as.numeric(sotu_sun_time$documents$day)
df_sun_time <- sotu_sun_time$documents
df_sun_time$day <- as.numeric(df_sun_time$day)
df_sun_time_text <- textProcessor(df_sun_time$texts, metadata = df_sun_time, language = 'english', stem = T)
# 3.(b)
# the number of topics selected in the fitted model: 103
# the number of iterations completed before the model converged: 47
df_sun_time_stm <- stm(df_sun_time_text$documents, df_sun_time_text$vocab, 0, prevalence = ~paperName + day, data = df_sun_time_text$meta, control = (lower.thresh=30))
# 3.(c)
# topic 33(news), topic 20(will), topic 50(migrant), topic 18(said), topic 15(ukip)
plot(df_sun_time_stm, type = "summary")
# 3.(d)
plot(df_sun_time_stm, type="perspectives", topics = c(33,20))
prep <- estimateEffect(~paperName + day , df_sun_time_stm, meta=df_sun_time_text$meta)
plot(prep, "day", df_sun_time_stm, topics = 33, 
     method = "continuous", xaxt = "n", xlab = "Date")
# 4.(a)
uk <- corpus_subset(data_corpus_ukmanifestos, Party == 'Con' | Party == 'Lab')
# 4.(b)
uk_dfm <- dfm(uk, stem = T, remove = stopwords('english'), remove_punct = T)
uk_dfm@docvars[c(19,20),]
uk_fish <- textmodel_wordfish(uk_dfm, c(19, 20))
# 4.(c)
textplot_scale1d(uk_fish)
textplot_scale1d(uk_fish, groups = uk$documents$Party)
plot(as.factor(uk$documents$Party), uk_fish$theta)
# 4.(d)
weights <- uk_fish$beta
words <- uk_fish$psi
plot(weights, words)
# 5.
bursty <- function(word = "sioux", DTM, date) {
  word.vec <- DTM[, which(colnames(DTM) == word)]
  if(length(word.vec) == 0) {
    print(word, " does not exist in this corpus.")
  } 
  else {
    word.times <- c(0,which(as.vector(word.vec)>0))
    
    kl <- kleinberg(word.times, gamma = 0.5)
    kl$start <- date[kl$start+1]
    kl$end <- date[kl$end]
    max_level <- max(kl$level)
    
    plot(c(kl$start[1], kl$end[1]), c(1,max_level),
         type = "n", xlab = "Time", ylab = "Level", bty = "n",
         xlim = c(kl$start[1], kl$end[1]), ylim = c(1, max_level),
         yaxt = "n")
    axis(2, at = 1:max_level)
    
    for (i in 1:nrow(kl)) {
      if (kl$start[i] != kl$end[i]) {
        arrows(kl$start[i], kl$level[i], kl$end[i], kl$level[i], code = 3, angle = 90,
               length = 0.05)
      } 
      else {
        points(kl$start[i], kl$level[i])
      }
    }
    
    print(kl)
  }
  #note deviation from standard defaults bec don't have that much data
}
news_data <- readRDS("/Users/apple/Downloads/news_data.rds")
news_data_corpus <- corpus(news_data$headline)
docvars(news_data_corpus)$date <- news_data$date

news_data_dfm <- dfm(news_data_corpus)
bursty("trump", news_data_dfm, as.numeric(gsub("-", "", news_data_dfm@docvars$date)))
bursty("korea", news_data_dfm, as.numeric(gsub("-", "", news_data_dfm@docvars$date)))
bursty("afghanistan", news_data_dfm, as.numeric(gsub("-", "", news_data_dfm@docvars$date)))
# 6.(a)
# negative: appeal, other, aziz, kale, mindi.
# positive: 3.7, poorest, much, billionair, 42.
#news_data <- readRDS("/Users/apple/Downloads/news_data.rds")
news_data <- news_data[1:1000,]
news_data_dfm <- dfm(news_data$headline, stem = T, remove = stopwords('english'), remove_punct = T, tolower = T)
news_pca <- prcomp(news_data_dfm, center = T, scale = T)
#fviz_eig(news_pca, addlabels = T)
pc_loadings <- news_pca$rotation
N <- 5
pc1_loading <- tibble(token = rownames(pc_loadings), loading = as.vector(pc_loadings[,1])) %>% arrange(-loading)
pc1_loading$loading <- scale(pc1_loading$loading, center = TRUE)
pc1_loading <- rbind(top_n(pc1_loading, N, loading),top_n(pc1_loading, -N, loading))
pc1_loading <- transform(pc1_loading, token = factor(token, levels = unique(token)))
ggplot(pc1_loading, aes(token, loading)) + 
  geom_bar(stat = "identity", fill = ifelse(pc1_loading$loading <= 0, "grey20", "grey70")) +
  coord_flip() + 
  xlab("Tokens") + ylab("Tokens with Top Loadings on PC1") +
  scale_colour_grey(start = .3, end = .7) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16),
        axis.title.y = element_text(size=18, margin = margin(t = 0, r = 15, b = 0, l = 15)),
        axis.title.x = element_text(size=18, margin = margin(t = 15, r = 0, b = 15, l = 0)),
        legend.text=element_text(size=16),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.position = "top",
        legend.spacing.x = unit(0.25, 'cm'),
        plot.margin=unit(c(1,1,0,0),"cm"))
# 6.(b)
# korea: north     south   dialogu      agre    resolv 
#       0.8541998 0.5187266 0.5142791 0.4528880 0.4528880 
# corruption:   disrupt   governor       gone     lost    ex-italian 
#              0.9435927  0.6288455  0.6288455  0.6288455  0.6168755
news_lsa <- convert(news_data_dfm, to = 'lsa')
news_lsa <- lw_logtf(news_lsa) * gw_idf(news_lsa)
news_data_lsa <- lsa(news_lsa)
news_textmatrix <- as.textmatrix(news_data_lsa)
korea <- associate(news_textmatrix, 'korea', 'cosine', threshold = .4)
korea[1:5]
corruption <- associate(news_textmatrix, 'corrupt', 'cosine', threshold = .6)
corruption[1:5]
# 6.(c)
# korea_glove
# korean    pyongyang     seoul      dprk   koreans 
# 0.7692005 0.7274451 0.6918146 0.6857666 0.6707964 
# corruption_glove
# graft      bribery     corrupt      allegations   scandals 
# 0.7507896   0.6686464   0.6202291   0.6174196   0.6155130 
pretrained <- readRDS("/Users/apple/Downloads/pretrained.rds")
korea_glove <- associate(pretrained, 'korea', 'cosine', threshold = .4)
korea_glove[1:5]
corruption_glove <- associate(pretrained, 'corruption', 'cosine', threshold = .3)
corruption_glove[1:5]
