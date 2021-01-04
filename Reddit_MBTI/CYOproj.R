# Preface ----
# Coded on a Windows machine using R version 4.0.2

# Opener -----
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

reddit <- read.csv(unzip("./Capstone/reddit_psychometric_data.zip"))


# Exploration-------
dim(reddit)
head(reddit[1:6])

# storing the types
mb <- reddit[1]

# storing the interactions
r_dat <- reddit[-1]
namez <- colnames(r_dat)


# getting an idea of number of interactions
# per person and per sub_reddit
s_col <- colSums(r_dat)
s_row <- rowSums(r_dat)

# understanding the total number of interactions per redit
max(s_col)
min(s_col)
mean(s_col)
sd(s_col)

# working with per user
max(s_row)
min(s_row)
mean(s_row)
sd(s_row)


# Visualization work ------

types_table <- reddit %>% 
  group_by(mbti_type) %>% 
  summarise(total = n()) %>% 
  arrange(total)

col_df <- as.data.frame(cbind(columns = namez, totals = as.numeric(s_col)))
head(col_df)

col_df %>% 
  group_by(totals) %>% 
  summarise(total = n()) %>% 
  arrange(totals) 


# create data frames with totals
type_totals <- data.frame(mbti = mb,total = s_row)

type_totals %>% 
  ggplot(aes(mbti_type)) +
  geom_bar()

# strip the mbti type to E for Extroversion and I for Introversion  
mb_extro <- mb %>% mutate(mbti_type = str_extract(mbti_type, "[A-Z]")) %>% select(mbti_type)

ie_totals <- data.frame(mbti = mb_extro,total = s_row)

ie_totals %>% 
  ggplot(aes(mbti_type)) +
  geom_bar()



# preprocessing ---------



# adding a column with the total interations per user.
r_dat_totals <- cbind(r_dat, totals = s_row)

# performing PCA 
# This could take ~20 minutes
pca_red_t <- prcomp(r_dat_totals)

# See the breakdown of sd's
pc <- 1:length(pca_red_t$sdev)
pt <- pca_red_t$sdev
qplot(pc, pt)

# zooming in to where it starts to flatten
pc2 <- 3:200
pt2 <- pca_red_t$sdev[3:200]
qplot(pc2, pt2)



# Taking a closer look at the proportion of the total variance is accounted for.
# summary(pca_red_t)$importance[,1:100]
# summary(pca_red_t)$importance[,100:400]
# summary(pca_red_t)$importance[,50:100]


# ML work -------
# 70% of data = 10, 75% =  21, 80% = 62, 85% = 157

fin_redd <- cbind(mb_extro, pca_red_t$x[,1:62])
# if using R 3.6 or later
set.seed(2020, sample.kind = "Rounding")   
test_index <- createDataPartition(y = fin_redd$mbti_type, times = 1, p = 0.2, list = FALSE)
test <- fin_redd[test_index,]
train <- fin_redd[-test_index,]


#  set up  ------
i <- seq(1, 8)

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "multinom", "qda", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(mbti_type ~ ., method = model, data = train)
}) 

names(fits) <- models


# See the all accuricies of each individual model.
acc_train <- sapply(fits, function(fit){
  fit$results$Accuracy
})
acc_train


# picking the lowest accuracies from the models
acc_hat <- sapply(fits, function(fit){
  min(fit$results$Accuracy)
})
which.max(acc_hat)
mean(acc_hat)

# Final Testing -----
# all models
pred_ensemble <- sapply(fits, function(object){ 
  predict(object, newdata = test)})


# First using the best performing single model. 
glm_pred <- predict(fits$glm, test)
glm_acc <- mean(glm_pred == test$mbti_type)


# Using the models that performed better than 80%
ind <- acc_hat >= 0.8
votes <- rowMeans(pred_ensemble[,ind] == "E")
y_hat <- ifelse(votes > 0.5, "E", "I")
ensem_acc <- mean(y_hat == test$mbti_type)

ensem_acc > glm_acc

