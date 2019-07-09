library(quanteda)
library(quanteda.corpora)
library(readtext)
library(dplyr)
# 1.(a)
email <- c('republican1', 'republican2', 'republican3', 
           'democrat1', 'democrat2', 'democrat3', 'democrat4')
content <- c('immigration aliens wall emergency country', 
             'voter economy president growth security', 
             'healthcare cost socialism unfair help', 
             'immigration country diversity help security', 
             'healthcare universal preconditions unfair help', 
             'economy inequality opportunity voter help', 
             'abortion choice right women help')
test_email <- c('immigration voter aliens help economy')
train_data <- data.frame(email, content)
train_data$content <- as.character(train_data$content)
test_data <- data.frame(test_email)
test_data$test_email <- as.character(test_data$test_email)
prior_rep <- 3/7
prior_dem <- 4/7
# There are five words in the test email: 'immigration', 'voter',
# 'aliens', 'help', 'economy'.
# I will use the "kwic" to count the occurance of each word in the training set
# There are 35 words in the train_data in total
kwic(train_data$content, 'economy', 3, case_insensitive = FALSE)
kwic(train_data$content, 'immigration', 3, case_insensitive = FALSE)
kwic(train_data$content, 'voter', 3, case_insensitive = FALSE)
kwic(train_data$content, 'aliens', 3, case_insensitive = FALSE)
kwic(train_data$content, 'help', 3, case_insensitive = FALSE)
# 'immigration' occured once of each party
# 'voter' occured once of each party
# 'aliens' occured once in republican
# 'help' occured once in republican, forth in democrat
# 'economy' occured of each party
P_rep <- prior_rep * 1/35 * 1/35 * 1/35 * 1/35 * 1/35
P_dem <- prior_dem * 1/35 * 1/35 * 0 * 4/35 * 1/35
P_rep
P_dem
P_rep > P_dem
# According to the result, the probability of it sent from democrat
# is lower than republican
# It sent by the republican
# I don't trust the findings because there is a zero in P_rep (Sparsity)
# 1.(b)
# smoothing
# add one too each count
P_rep_ws <- prior_rep * 2/40 * 2/40 * 2/40 * 2/40 * 2/40
P_dem_ws <- prior_dem * 2/40 * 2/40 * 1/40 * 5/40 * 2/40
P_rep_ws < P_dem_ws
# According to the result, the probability of it sent from democrat
# is higher than republican
# It sent by the democrat

yelp <- read.csv("/Users/apple/Downloads/Text-as-Data-Lab-Spring-2019-master/Homeworks/HW2/data/yelp.csv")
head(yelp, n=5)
#2.(a) keep useful columns
yelp <- yelp %>% select(text, stars) %>% setNames(c("text", "stars"))
yelp$text <- unlist(yelp$text)
prop.table(table(yelp$stars))
# The empirical median score is 3. 
# add new column 'label' into dataset.
label <- ifelse (yelp$stars >= 3, 'positive', 'negative')
yelp$label <- label
# 2.(b) add another column named anchor
yelp$anchor <- ifelse(yelp$stars == 5, 'positive', ifelse(yelp$stars == 1, 'negative', 'neutral'))
count(yelp, anchor)
# 1 negative   749
# 2 neutral   5914
# 3 positive  3337
# 3.(a)
positive_words <- readLines("/Users/apple/Downloads/Text-as-Data-Lab-Spring-2019-master/Homeworks/HW2/data/positive-words.txt")
negative_words <- readLines("/Users/apple/Downloads/Text-as-Data-Lab-Spring-2019-master/Homeworks/HW2/data/negative-words.txt")
#positive <- dfm(yelp$text, select = positive_words)
yelp$text <- as.character(yelp$text)
class(yelp$text)
positive <- dfm(yelp$text, select = positive_words)
negative <- dfm(yelp$text, select = negative_words)
class(rowSums(positive))
positive_sum <- rowSums(positive, na.rm = TRUE)
negative_sum <- rowSums(negative, na.rm = TRUE)
score <- positive_sum - negative_sum
positive$sum <- rowSums(positive[sapply(positive, is.numeric)])
score <- as.numeric(score)
yelp$score <- score
yelp$sentiment <- ifelse(yelp$score >= 0, 'positive', 'negative')
count(yelp, sentiment)
# the percent of positive category is 9041/10000=90.41%
# the percent of negative category is 959/10000=9.59%
# 3.(b)
group <- group_by(yelp, score)
score_sum <- summarise(group, total = n())
plot(score_sum)
h <- hist(score_sum$score, ylim = c(0,12), xlim = c(-30,50))
text(h$mids, h$counts, labels = h$counts, adj = c(0.5, -0.5))
# 3.(c)
# the answer I get from the code below is:
# Baseline Accuracy:  0.8324 
# Accuracy: 0.8603 
# Recall: 0.9591543 
# Precision: 0.8830882 
# F1-score: 0.9195508
cmat <- table(yelp$label,  yelp$sentiment)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)
baseline_acc <- max(prop.table(table(yelp$label)))
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)
# 3.(d)
# the RankSum is 28198588
yelp_dictionary <- yelp
yelp_dictionary$rank <- rank(-yelp_dictionary$stars, ties.method = 'min')
yelp_dictionary_predict <- yelp
yelp_dictionary_predict$rank <- rank(-yelp_dictionary_predict$score, ties.method = 'min')
#yelp_reorder_predict <- yelp[order(-yelp$score),]
#yelp_reorder_actual <- yelp[order(-yelp$stars),]
#yelp_reorder_actual$rank <- rank(-yelp_reorder_actual$stars, ties.method = 'min')
#yelp_reorder_predict$rank <- rank(-yelp_reorder_predict$score, ties.method = 'min')
RankSum <- sum(abs(yelp_dictionary$rank - yelp_dictionary_predict$rank))
RankSum

# 4.(a)
# the answers are
# Baseline Accuracy:  0.8985 
# Accuracy: 0.905 
# Recall: 0.9744018
# Precision: 0.9240106 
# F1-score: 0.9485374
# the confusion matrics
# predicted_star_sm
# negative positive
# negative       59      144
# positive       46     1751
yelp_short <- yelp %>% select(text, sentiment) %>% setNames(c("text", "sentiment"))
set.seed(1984L)
prop_train <- 0.8
ids <- 1:nrow(yelp_short)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
ids_test <- ids[-ids_train]
train_set <- yelp_short[ids_train,]
test_set <- yelp_short[ids_test,]
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
as.matrix(train_dfm)[1:5,1:5]
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))
nb_model_sm <- textmodel_nb(train_dfm, train_set$sentiment, smooth = 1, prior = "uniform")
predicted_star_sm <- predict(nb_model_sm, newdata = test_dfm)
cmat_sm <- table(test_set$sentiment, predicted_star_sm)
baseline_acc <- max(prop.table(table(test_set$sentiment)))
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)
cmat_sm
# 4.(b)
# the answers are
# Baseline Accuracy:  0.8985 
# Accuracy: 0.908 
# Recall: 0.9883139 
# Precision: 0.915936 
# F1-score: 0.9507495
# the confusion matrics
# predicted_star_sm
# negative positive
# negative       40      163
# positive       21     1776
nb_model_sm_1 <- textmodel_nb(train_dfm, train_set$sentiment, smooth = 1, prior = "docfreq")
predicted_star_sm <- predict(nb_model_sm_1, newdata = test_dfm)
cmat_sm <- table(test_set$sentiment, predicted_star_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)
# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)
cmat_sm
# 4.(c)
# The accuracy will decrease if I fit the model without smoothing.
# Because smoothing reduces the 'weight' place on new information vis-a-vis the prior.
# Baseline Accuracy:  0.8985 
# Accuracy: 0.8315 
# Recall: 0.8903728 
# Precision: 0.9195402 
# F1-score: 0.9047215
nb_model <- textmodel_nb(train_dfm, train_set$sentiment, smooth = 0, prior = "uniform")

# evaluate on test set
predicted_class <- predict(nb_model, newdata = test_dfm)

# baseline
baseline_acc <- max(prop.table(table(test_set$sentiment)))

# get confusion matrix
cmat <- table(test_set$sentiment, predicted_class)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)

# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)
# 4.(d)

# 5.(a)
# According to the result
# The most extreme word(positive) is "great"
# The most extreme word(negative) is "minutes"
# The results are
# possitive_features[1:10]
# great      love      best    always   amazing delicious 
# 0.7280742 0.6945657 0.6795529 0.6766334 0.6754761 0.6700161 
# favorite  friendly   awesome   perfect 
# 0.6663395 0.6636674 0.6610100 0.6592150
#  negative_features[1:10]
# minutes        us     asked      told      food      said 
# 0.5654491 0.5751572 0.5790140 0.5798495 0.5816950 0.5829414 
# even       bad      back   ordered 
# 0.5838641 0.5847291 0.5865597 0.5884235
yelp_positive <- yelp[yelp$anchor == 'positive',]
yelp_negative <- yelp[yelp$anchor == 'negative',]
total <- rbind(yelp_positive, yelp_negative)
set.seed(1984L)
ids <- 1:nrow(total)
ids_test <- sample(ids, 1, replace = FALSE)
ids_train <- ids[-ids_test]
train_set <- total[ids_train,]
test_set <- total[ids_test,]
train_dfm <- dfm(train_set$text, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, remove_punct = TRUE, remove = stopwords("english"))
ws_sm <- textmodel_wordscores(train_dfm, 
                                y = (2 * as.numeric(train_set$stars == 5)) - 1, 
                                smooth = 1
)

# Look at strongest features
possitive_features <- sort(ws_sm$wordscores, decreasing = TRUE)  # for labor
possitive_features[1:10]

negative_features <- sort(ws_sm$wordscores, decreasing = FALSE)  # for conservative
negative_features[1:10]


# 5.(b)
# The RankSum is 25886447
# The RankSum of the dictionaries is 28198588
# So the wordscore did better
nonanchor <- dfm(yelp$text, remove_punct = TRUE, remove = stopwords("english"))
wordscore <- predict(ws_sm, newdata = nonanchor, rescaling = 'none', level = 0.95)
wordscore <- as.numeric(wordscore)
yelp$wordscore <- wordscore
yelp_wordscore <- yelp
yelp_wordscore$rank <- rank(-yelp_wordscore$stars, ties.method = 'min')
yelp_wordscore_predict <- yelp
yelp_wordscore_predict$rank <- rank(-yelp_wordscore_predict$wordscore, ties.method = 'min')



#yelp_reorder_predict_wordscore <- yelp[order(-yelp$wordscore),]
#yelp_reorder_actual_wordscore <- yelp[order(-yelp$stars),]
#yelp_reorder_actual_wordscore$rank <- rank(-yelp_reorder_actual_wordscore$stars, ties.method = 'min')
#yelp_reorder_predict_wordscore$rank <- rank(-yelp_reorder_predict_wordscore$wordscore, ties.method = 'min')
RankSum <- sum(abs(yelp_wordscore$rank - yelp_wordscore_predict$rank))
RankSum

# 6.(a)
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
library(caret)
yelp_samp <- yelp[c(1:1000),]
svm_dfm <- dfm(yelp_samp$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")

# A. the caret package has it's own partitioning function
# p = 0.9
# Baseline Accuracy:  0.81 
# SVM-Linear Accuracy: 0.84
set.seed(1984)
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.9, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.8
# Baseline Accuracy:  0.855 
# SVM-Linear Accuracy: 0.81
set.seed(1984)
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.8, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$sentiment[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$sentiment[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.7
# Baseline Accuracy:  0.81
# SVM-Linear Accuracy: 0.7933333
set.seed(1984)
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.7, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.6
# Baseline Accuracy:  0.81 
# SVM-Linear Accuracy: 0.8025
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.6, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.5
# Baseline Accuracy:  0.83 
# SVM-Linear Accuracy: 0.82
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.5, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.4
# Baseline Accuracy:  0.8183333 
# SVM-Linear Accuracy: 0.82
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.4, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.3
# Baseline Accuracy:  0.8257143 
# SVM-Linear Accuracy: 0.8242857
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.3, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.2
# Baseline Accuracy:  0.82125 
# SVM-Linear Accuracy: 0.79125
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.2, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)
# p = 0.1
# Baseline Accuracy:  0.8222222 
# SVM-Linear Accuracy: 0.8355556
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.1, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$label[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$label[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]]
)

# 6.(c)
# I think the linear kernels would be the best to use in this context
# The accuracy for both kernels are shown below
# Baseline Accuracy:  0.895 
# SVM-Linear Accuracy: 0.915 
# SVM-Radial Accuracy: 0.895
set.seed(1984)
ids_train <- createDataPartition(1:nrow(svm_dfm), p = 0.9, list = FALSE, times = 1)
train_x <- svm_dfm[ids_train, ] %>% as.data.frame() # train set data
train_y <- yelp_samp$sentiment[ids_train] %>% as.factor()  # train set labels
test_x <- svm_dfm[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- yelp_samp$sentiment[-ids_train] %>% as.factor() # test set labels

# baseline
baseline_acc <- max(prop.table(table(test_y)))

# B. define training options (we've done this manually above)
trctrl <- trainControl(method = "cv", number = 5)
svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "svmLinear",
                        trControl = trctrl)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y)
svm_mod_radial <- train(x = train_x,
                        y = train_y,
                        method = "svmRadial",
                        trControl = trctrl)

svm_radial_pred <- predict(svm_mod_radial, newdata = test_x)
svm_radial_cmat <- confusionMatrix(svm_radial_pred, test_y)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "SVM-Linear Accuracy:",  svm_linear_cmat$overall[["Accuracy"]], "\n",
  "SVM-Radial Accuracy:",  svm_radial_cmat$overall[["Accuracy"]]
)
# 7.(a)
yelp_tree <- yelp[1:500,]
#yelp_tree$text <- gsub(pattern = "'", "", yelp_tree$text)  # replace apostrophes
prop_train <- 0.8
ids <- 1:nrow(yelp_tree)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)
ids_test <- ids[-ids_train]
train_set <- yelp_tree[ids_train,]
test_set <- yelp_tree[ids_test,]
train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
#as.matrix(train_dfm)[1:5,1:5]
test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))
train_x <- as.matrix(train_dfm)
train_y <- as.factor(train_set$label)


#tree_dfm <- dfm(yelp_tree$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english")) %>% convert("matrix")


#ids_train <- createDataPartition(1:nrow(tree_dfm), p = 0.8, list = FALSE, times = 1)
#train_x <- tree_dfm[ids_train, ] %>% as.data.frame() # train set data
#train_y <- yelp_tree$sentiment[ids_train] %>% as.factor()  # train set labels
#test_x <- tree_dfm[-ids_train, ]  %>% as.data.frame() # test set data
#test_y <- yelp_tree$sentiment[-ids_train] %>% as.factor() # test set labels

# 7.(b)
install.packages('randomForest')
#install.packages('mlbench')
library(randomForest)
#library(mlbench)
set.seed(1984)
#mtry = sqrt(ncol(train_x))  # number of features to sample at each split
#ntree = 51  # numbre of trees to grow
system.time(rf.base <- randomForest(x = train_x, y = train_y, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)], 10)
print(rf.base)
varImpPlot(rf.base, n.var = 10, main = "Variable Importance")
# 7.(c)
#test_x <- as.matrix(test_dfm)
#test_y <- as.factor(test_set$sentiment)
confusionMatrix(predict(rf.base, newdata = test_x), test_y)
predicted_rf <- predict(rf.base, newdata = test_x)
predicted_rf
# baseline
baseline_acc <- max(prop.table(table(test_set$sentiment)))

# get confusion matrix
cmat <- table(test_y, predicted_rf)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)
cmat
# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)
# 7.(d)
mtry = 0.5 * sqrt(ncol(train_x))
system.time(rf.man1 <- randomForest(x = train_x, y = train_y, mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])
print(rf.base)
confusionMatrix(predict(rf.man1, newdata = test_x), test_y)
predicted_rf <- predict(rf.man1, newdata = test_x)
baseline_acc <- max(prop.table(table(test_set$stars)))

# get confusion matrix
cmat <- table(test_y, predicted_rf)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)
cmat
# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)

mtry = 1.5 * sqrt(ncol(train_x))
system.time(rf.man1 <- randomForest(x = train_x, y = train_y, mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])
print(rf.base)
confusionMatrix(predict(rf.man1, newdata = test_x), test_y)
predicted_rf <- predict(rf.man1, newdata = test_x)
baseline_acc <- max(prop.table(table(test_set$stars)))

# get confusion matrix
cmat <- table(test_y, predicted_rf)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)
cmat
# print
cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)
