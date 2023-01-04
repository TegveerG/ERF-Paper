# import packages
library(naivebayes)
library(klaR)
library(e1071)
library(caTools)
library(caret)
library(tidyverse)
library(kableExtra)
library(stargazer)
library(flextable)
library(dplyr)
library(leaps)
library(caret)
library(ggplot2)
library(ggExtra)
library(tidyr)
library(ISLR2)
library(ISLR)
library(readr)
library(reticulate)

# df import and subset
df <- read_csv('Data/REKT_Database_Clean_Python.csv') 
df <- subset(df, select = -c(...1, token_name, description, name_categories))
#df <- df %>% filter(funds_lost!=0)

# Removing dictionary values from the scam_type column
df$scam_type <- gsub("[^:]*,[^:]*", "",df$scam_type)
df$scam_type <- gsub("'id'::", "",df$scam_type)
df$scam_type <- gsub("\\{|\\}", "",df$scam_type)
df$scam_type <- gsub("'", "",df$scam_type)
df$scam_type <- gsub("type: ", "",df$scam_type)
df$scam_type <- gsub(" ", "",df$scam_type)

# Removing list brackets from the scamNetworks column
df$scamNetworks <- gsub("\\[|\\]", "", df$scamNetworks)
df$scamNetworks <- gsub("'", '', df$scamNetworks)

# Split df by scamNetworks feature that has multiple categorical values for respective rows 
#library(splitstackshape)
#df <- concat.split.multiple(data = df, split.cols = "scamNetworks", direction = "long")

# pooling together scam types into respective types 
df <- df %>% 
  mutate(scam_type_grouped = if_else(scam_type=="Honeypot" | scam_type=="Rugpull" | scam_type=="Abandoned" | project_name=="Kronos Dao" | project_name=="Genesis" | project_name=="Celsius Network" | project_name=="Voyager" | project_name=="BlockFi" | project_name=="FTX Group" | project_name=="Bitcoin Sheikh" | project_name=="Trade Coin Club" | project_name=="EmpiresX" | project_name=="Vauld", "Exit Scam", "Exploit"))
df <- subset(df, select = -c(scam_type))

#extracting month names (results in 1876 NA's and elimination of those rows)
df$months=as.factor(months(df$date))

# specify dtypes before train test split
df$scamNetworks <- as.factor(df$scamNetworks)
df$scam_type_grouped <-as.factor(df$scam_type_grouped)

df <- subset(df, select = -c(month_of_attack, day_of_week_of_attack, day_of_year_of_attack, date, project_name))  

library(caret)
train.index <- createDataPartition(df$scamNetworks, p = .7, list = FALSE)
train <- df[ train.index,]
test  <- df[-train.index,]

test$scam_type_grouped = ifelse(test$scam_type_grouped  == "Exploit", 1, 0)
logistic_model <- glm(scam_type_grouped ~ ., data = train, family = "binomial")
summary(logistic_model)
prob_pred <- predict(logistic_model, type = 'response', newdata = test)
y_pred = ifelse(prob_pred > 0.5, 1, 0)
test$scam_type_grouped <- as.factor(test$scam_type_grouped)
(cm = table(test$scam_type_grouped, y_pred)) # NAs ignored
y_pred <- as.factor(unname(y_pred)) # for cfm plot

library(scales)
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Prediction, y = Reference)) +
    geom_tile(aes(fill = log(Freq)), 
              colour = "white", show.legend = FALSE) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Prediction, y = Reference, 
                  label = Freq)) +
    ggtitle(mytitle) + 
    scale_x_discrete(limits = rev) +
    theme_minimal() 
  return(p)
}
cfm_train <- confusionMatrix(test$scam_type_grouped, y_pred)
ggplotConfusionMatrix(cfm_train)

# ---------------------------------------------------------------------------

# Evaluating model accuracy
# using confusion matrix
table(test$scam_type_grouped, predict_reg)

missing_classerr <- mean(predict_reg != test$scam_type_grouped)
print(paste('Accuracy =', 1 - missing_classerr))

# ROC-AUC Curve
ROCPred <- prediction(predict_reg, test$scam_type_grouped) 
ROCPer <- performance(ROCPred, measure = "tpr", 
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
auc

# Plotting curve
plot(ROCPer)
plot(ROCPer, colorize = TRUE, 
     print.cutoffs.at = seq(0.1, by = 0.1), 
     main = "ROC CURVE")
abline(a = 0, b = 1)

auc <- round(auc, 4)
legend(.6, .4, auc, title = "AUC", cex = 1)