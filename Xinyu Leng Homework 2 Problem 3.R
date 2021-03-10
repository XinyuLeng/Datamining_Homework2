\section{Problem 3: Classification and retrospective sampling}

library(vcd)
library(ggplot2)
library(tidyverse)

german_credit <- read.csv("C:/Users/Administrator/Desktop/german_credit.csv", stringsAsFactors=TRUE)
summary(german_credit)
# Default：300 Not Default: 700

counts <- table(german_credit$history)
counts
# good: 89 poor:618 terrible: 293

ggplot(german_credit, aes(history, ..count..)) + 
  geom_bar(aes(fill = as.factor(Default)), position = "dodge") 
# For categories poor and terrible we see the number of nodefault are greater than the number of default.
# For categories good we see the number of default is greater than the number of nodefault.

# sampling
set.seed(1234)
train <- sample(nrow(german_credit),0.7*nrow(german_credit))
german_credittrain <- german_credit[train,]
german_credittest <- german_credit[-train,]
table(german_credittrain$history)
table(german_credittest$history)

# Q3_1 Make a bar plot of default probability by credit history

credit.glm1 <- glm(Default ~ history, family = binomial, german_credittrain )
coef(credit.glm1) %>% round (3)

DefaultProppoor <-  2.718281828459^(0.528-1.261*1) / (1+2.718281828459^(0.528-1.261*1))
DefaultProppoor
DefaultPropterrible <- 2.718281828459^(0.528-2.241*1) / (1+2.718281828459^(0.528-2.241*1))
DefaultPropterrible
DefaultPropgood <- 1-(DefaultProppoor+DefaultPropterrible)
DefaultPropgood


DefaultProp<- c(DefaultPropterrible,DefaultProppoor,DefaultPropgood)
history1<- c("terrible", "poor", "good")

Defaulthistory<- data.frame(history1,DefaultProp)
Defaulthistory

ggplot(Defaulthistory) + 
  geom_bar(stat='identity',aes(x=history1, y=DefaultProp), color='blue', alpha=0.1) +
  labs(title="default probability by credit history", x="history1", y="DefaultProp")  
  
# Q3_2 Build a logistic regression model for predicting default probability, using the variables duration + amount + installment + age + history + purpose + foreign.

fit.logit <- glm( Default ~ duration + amount + installment + age + history + purpose + foreign, family = binomial(), data=german_credittrain)

summary(fit.logit)

coef(fit.logit)
coef(fit.logit) %>% round (3)
exp(coef(fit.logit))

# What do you notice about the history variable vis-a-vis predicting defaults? What do you think is going on here? 

# According to the graph, the default probablitity will become higher as the borrower's credit rating is better.Because the bank matched each default with similar sets of loans that had not defaulted, including all reasonably close matches in the analysis.The sample of People with good credit is too small to lower the accuracy and they usually have fewer default samples. This resulted in a substantial oversampling of defaults


# use the confusion matrix to check out-of-sample performance
prob <- predict(fit.logit, german_credittest, type="response")
logit.pred <- factor(prob > 0.5, levels=c(FALSE, TRUE), 
                    labels=c("Nodefault", "default"))
logit.perf <- table(german_credittest$Default, logit.pred,
                    dnn=c("Actual", "Predicted")) 
logit.perf

# accuracy= (188+22)/300= 0.70

# an example for predict default history by using the logistic model

german_credittest[1,]
predictions_1st <- predict(fit.logit, newdata = german_credittest[1,], type = "response")
predictions_1st
# we could see that the defaulting probability for the 1st player in the test set is about 45.77%
 
# Q3_3 In light of what you see here, do you think this data set is appropriate for building a predictive model of defaults, if the purpose of the model is to screen prospective borrowers to classify them into "high" versus "low" probability of default? Why or why not---and if not, would you recommend any changes to the bank's sampling scheme?

# I think this data set is not appropriate for building a predictive model for defaults. Because the bank attempted to match each default with similar sets of loans that had not defaulted, including all reasonably close matches in the analysis. This resulted in a substantial oversampling of defaults, relative to a random sample of loans in the bank's overall portfolio.
# The bank’s sample size should be as large as possible. The more closer to the overall conditions, the more accurate the predictive outcome is. 












