############# Random Forest
library(randomForest)
library(ranger)
library(caret)

imdb.train <- imdb %>% filter(!is.na(imdb_score))
imdb.test <- imdb %>% filter(is.na(imdb_score))

plot_missing(imdb.train)


#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=3, 
                        repeats=1)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
#Number randomely variable selected is mtry
mtry <- sqrt(ncol(imdb.train))
tunegrid <- expand.grid(mtry=10:20)
rf_default <- train(imdb_score ~ ., 
                    data=imdb.train %>% select(-Set, -movie_title), 
                    method='rf', 
                    tuneGrid=tunegrid, 
                    trControl=control)
plot(rf_default)
predict(rf_default, newdata=imdb.test)

#predict
predictions <- data.frame(Id=imdb.test$movie_title, Predicted=predict(rf_default, newdata=imdb.test))

write.csv(predictions,"/Users/graceedriggs/Documents/STAT 495/IMDB/IMDBScorePredictions.csv")


