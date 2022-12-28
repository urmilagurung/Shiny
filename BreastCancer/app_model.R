# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(mlbench)
library(tidyverse)
library(dplyr)
library(bestglm)


####################################
# Data Preprocessing               #
####################################

# Read data
data(BreastCancer)

# Check Data
glimpse(BreastCancer)

# Remove Data
BreastCancer <- na.omit(BreastCancer)

## Create new variable for recoded data
# recode 'class' variable based
BreastCancer_conv = data.frame(BreastCancer[,-(11)],Class=as.integer(BreastCancer$Class)*2)

# Convert factors to quantitative variables
BreastCancer_conv$Id <- as.integer(BreastCancer_conv$Id)
BreastCancer_conv$Cl.thickness <- as.integer(BreastCancer_conv$Cl.thickness)
BreastCancer_conv$Cell.size <- as.integer(BreastCancer_conv$Cell.size)
BreastCancer_conv$Cell.shape <- as.integer(BreastCancer_conv$Cell.shape)
BreastCancer_conv$Marg.adhesion <- as.integer(BreastCancer_conv$Marg.adhesion)
BreastCancer_conv$Epith.c.size <- as.integer(BreastCancer_conv$Epith.c.size)
BreastCancer_conv$Bare.nuclei <- as.integer(BreastCancer_conv$Bare.nuclei)
BreastCancer_conv$Bl.cromatin <- as.integer(BreastCancer_conv$Bl.cromatin)
BreastCancer_conv$Normal.nucleoli <- as.integer(BreastCancer_conv$Normal.nucleoli)
BreastCancer_conv$Mitoses <- as.integer(BreastCancer_conv$Mitoses)


#Redundant observations
duplicated_id <- BreastCancer_conv %>% 
  filter(duplicated(.))

# Number of redundant observations
nrow(duplicated_id)

#Code to remove redundant observations and identifier column
BreastCancer_conv <- BreastCancer_conv %>% 
  filter(!duplicated(.)) 

## Pick out and scale predictor variables
X1 = BreastCancer_conv[,2:10]
# Pick out response variable
y = BreastCancer_conv[,11]
## Combine to create new data frame
BreastCancer_data = data.frame(X1, y)
BreastCancer_data$y <- as.factor(BreastCancer_data$y)

## Apply best subset selection
best_fit_AIC = bestglm(BreastCancer_data, family=binomial, IC="AIC")
best_fit_BIC = bestglm(BreastCancer_data, family=binomial, IC="BIC")

## Examine the results
best_fit_AIC$Subsets
best_fit_BIC$Subsets

## Identify best-fitting models
(best_AIC = best_fit_AIC$ModelReport$Bestk)
(best_BIC = best_fit_BIC$ModelReport$Bestk)

## Create multi-panel plotting device
par(mfrow=c(1,2))
## Store n and p
n = nrow(BreastCancer_data); p = ncol(BreastCancer_data) - 1
## Produce plots, highlighting optimal value of k
plot(0:p, best_fit_AIC$Subsets$AIC, xlab="Number of predictors", ylab="AIC", type="b")
points(best_AIC, best_fit_AIC$Subsets$AIC[best_AIC+1], col="red", pch=16)
plot(0:p, best_fit_BIC$Subsets$BIC, xlab="Number of predictors", ylab="BIC", type="b")
points(best_BIC, best_fit_BIC$Subsets$BIC[best_BIC+1], col="red", pch=16)


pstar = 6
## Check which predictors are in the 6-predictor model
best_fit_AIC$Subsets[pstar+1,]
## Construct a reduced data set containing only the selected predictor
indices = as.logical(best_fit_AIC$Subsets[pstar+1, 2:(p+1)])
BreastCancer_data_red = data.frame(X1[,indices], y)
BreastCancer_data_red$y <- as.factor(BreastCancer_data_red$y)
## Obtain regression coefficients for this model


# Create a new data frame for best dataset
BreastCancer_best <- data.frame(BreastCancer_data_red,stringsAsFactors = FALSE)

# filter dataframe
set.seed(100)
split = sample(c(rep(0, 0.8 * nrow(BreastCancer_best)), rep(1, 0.2 * nrow(BreastCancer_best))))
BreastCancer_best_train <- BreastCancer_best[split == 0,] 
BreastCancer_best_test <- BreastCancer_best[split == 1,]


write.csv(BreastCancer_best_train, "training.csv")
write.csv(BreastCancer_best_test, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

# Build model
model = glm(y~., data=BreastCancer_best_train, family="binomial")

# Save model to RDS file
 saveRDS(model, "model.rds")

# Read in the Logistic model
model <- readRDS("model.rds")
