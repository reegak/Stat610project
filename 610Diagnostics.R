
install.packages("MASS")
library(MASS)
install.packages("dplyr")
library(dplyr)
install.packages("digest")
library(digest)
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(broom)
theme_set(theme_classic())

##Downloading the Data

headers <- c("Age", "Sex", "cp", "trestbps", "chol", "fbs", "restecg",
             "thalach", "exang", "oldpeak", "slope", "ca", "thal", "num")

www <- paste0("https://archive.ics.uci.edu/ml/machine-learning-databases/",
              "heart-disease/processed.cleveland.data")

data <- read.csv(url(www), header = F, col.names = headers)
str(data)

data$ca <- as.numeric(data$ca)
data$thal <- as.numeric(data$thal)

data$Sex <- as.factor(ifelse(data$Sex == 1, "Male", "Female"))
data$fbs <- as.factor(ifelse(data$fbs == 1, "True", "False"))
data$exang <- as.factor(ifelse(data$exang == 1, "Yes", "No"))

Heart <- data
str(Heart)

#Our binary output data set:

data2 <- data

data2$num[data2$num == 0] <- 0
data2$num[data2$num == 1] <- 1
data2$num[data2$num == 2] <- 1
data2$num[data2$num == 3] <- 1
data2$num[data2$num == 4] <- 1

data2$num <- as.factor(ifelse(data2$num == 0, "No.Risk", "Risk"))

Heart2 <- data2
str(Heart2)

##Cooks Distance/ LEverage

model3 <- glm(num~Sex+cp+exang+slope, family=binomial, data=Heart2)

##Cooks Distance / Influential values are extreme individual 
##data points that can alter the quality of logistic regression
##Here we label the top 3 largest values

plot(model3, which=4, id.n=3)

model.data <- augment(model3) %>% 
  mutate(index = 1:n()) 

##Standardided Residuals / Cooks Distance
model.data %>% top_n(3, .cooksd)

##Plotting Standardized Residuals 

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = num), alpha = .5) +
  theme_bw()

##Filter Data Points with >3 Standard Deviations
model.data %>% 
  filter(abs(.std.resid) > 3)

# Code for stepwise AIC 

stepAIC(glm(num~-1+1,family=binomial,data=Heart2), scope=~Sex*cp*exang*slope)

##simple one variable case

simple <- glm(num~thalach,family=binomial,data=Heart2)
summary(simple)

