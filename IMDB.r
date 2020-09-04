##
## Code to analyze the IMDB database
##

library(tidyverse)
library(bestglm)

## Read in Data
imdb <- read_csv("/Users/graceedriggs/Documents/Stat 495/IMDB/imdb495/IMDBTrain.csv")
imdb.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/IMDB/imdb495/IMDBTest.csv")

##
## Exploratory Data Analysis
##

## Overall summary of the Data
summary(imdb)


# get rid of all NA's
imdb.deleted.all <- na.omit(imdb) 
    ## results in 3294 variables. so 1200 rows were deleted.

## Getting rid of NA's
delete.na <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
imdb.deleted <- delete.na(imdb, 2)
    ## results in 4135 variables. So only 300 rows were deleted.


## Scatterplot of Budget vs. Score
## Uh-oh... budget is in local currency.. need to convert to common.
ggplot(data=imdb, mapping = aes(x=budget, y=imdb_score)) +
  geom_point() 
imdb %>% filter(budget>100000000, country == "USA") %>% select(movie_title)

## Scatterplot of gross vs. IMDB score
ggplot(data=imdb, mapping = aes(x=gross, y=imdb_score)) +
  geom_point() 
# is it Highly correlated? (0.202)
with(imdb, cor(gross, imdb_score, use="complete.obs"))
cor(imdb$gross, imdb$imdb_score, use="complete.obs")

## Regression Model
imdb.lm <- lm(imdb_score ~ color + director_name + num_critic_for_reviews + duration, data = imdb )
summary(imdb.lm)

##### Best Subsets method ######
best.subsets.bic <- bestglm(imdb,
                            IC = "BIC",
                            method = "exhaustive",
                            TopModels = 10)

best.subsets.bic.df <- data.frame("num.vars" = 1:dim(imdb.lm)[2], 
                                  "BIC" = best.subsets.bic$Subsets$BIC)


## explore the first 4 collumns. clean up data. etc. (color, director name, num_critics for reviees, duration)
## install git on computer.
