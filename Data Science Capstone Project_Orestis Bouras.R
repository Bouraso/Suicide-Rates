## title: "Data Science Capstone Project: Suicide Rates Overview"
## author: "Orestis Bouras"
## date: "November 25, 2020"




#### Introduction

#This report analyzes the worldwide Suicide Rates based on the dataset named as "Suicide Rates Overview 1985 to 2016", 
#found in the website of "kaggle".In the first part of the report, a thorough analysis and visualization of the data is included while in the second part, 
#a machine learning algorithm is developed with multiple methods such as logistic regression, lda, knn and random forest, 
#to predict the Suicide Rate based on several of the provided features.

#To sum up, the report is structured as follows. 
#Chapter 1 includes the introduction of the report. 
#Chapter 2 describes the dataset and the data cleaning. 
#Chapter 3 is divided into two sectors. The first sector presents the data exploration and visualization through multiple graphs that provide useful insights for the Suicide Rates dataset. The second sector includes the multiple developed models with the respective accuracy results and discussion on each model's performance. 
#Chapter 4 summarizes the results for each model while 
#Chapter 5 concludes with a brief summary of the report, possible limitations and future work.





#### Dataset

###############
# Data download
###############

# Download and read the data from Github + install all the necessary packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(tidyr)
library(gridExtra)

suiciderates <- read.csv(file="https://raw.githubusercontent.com/Bouraso/Suicide-Rates/main/Suicide%20Rates%20Overview%201985%20to%202016.csv",
                         header = TRUE, sep = ",",fileEncoding="UTF-8-BOM")

###############
# Data cleaning
###############

# Check the raw data
head(suiciderates) %>%print.data.frame()

# Rename several columns for simplicity
suiciderates <- suiciderates%>%rename(gdp_per_capita=gdp_per_capita....,gdp_for_year=gdp_for_year....)

# Remove "HDI.for.year" column and rows including NA values
suiciderates <- suiciderates[-9]
suiciderates <- na.omit(suiciderates)

# Check the new structure of the dataset
head(suiciderates) %>%print.data.frame()

# Summarizing the dataset
summary(suiciderates)






#### Methods and Analysis

###############
# Data Analysis
###############

# In the "Data Analysis" chapter, several graphs and summary statistics are created in order to get familiarized with the filtered dataset 
# and understand how each feature can impact the outcome.The insights obtained during the investigation will help to build the machine learning model.

# Identify the unique values of the important dataset's features, such as country, sex, year
suiciderates %>%
  summarize(n_country = n_distinct(country), n_year = n_distinct(year), n_sex = n_distinct(sex), n_age = n_distinct(age), n_suiciderate = n_distinct(suicides.100k.pop), n_gdp_capita = n_distinct(gdp_per_capita), n_generation = n_distinct(generation))

# There are 5298 unique values of the suicide rate feature ("suicides.100k.pop") which is the feature of interest.
# In order to obtain a better view on the provided rates, their distribution can be visualized as below:
suiciderates %>%
  ggplot(aes(suicides.100k.pop)) +
  geom_histogram(binwidth = 4, color = "black") +
  xlab("Suicide Rate")+
  scale_x_continuous(breaks = c(seq(0, 250, 25))) +
  ylab("Number of Suicide Rates")+
  scale_y_continuous(breaks = c(seq(0, 10000, 1000))) +
  ggtitle("Suicide Rate (suicides per 100k population) distribution")

# The following code summarizes the Suicide rate dataset by calculating the complete set's average rate per 100k population 
# and the maximum rate as well as where and when it appeared:
avg_suiciderate <- mean(suiciderates$suicides.100k.pop)
message("The average Suicide rate per 100k population is ", round(avg_suiciderate,digits = 1))

sd_suiciderate <- sd(suiciderates$suicides.100k.pop)
max_suicide_rate <- max(suiciderates$suicides.100k.pop)
message("The maximum Suicide rate is ", round(max_suicide_rate,digits = 1)," and appears in ", suiciderates$country[which.max(suiciderates$suicides.100k.pop)], " in the year ",suiciderates$year[which.max(suiciderates$suicides.100k.pop)])

# An interesting insight is to investigate the countries that present the highest and the lowest average SR. 
# In order to represent this, the average rate per country is calculated:
suiciderates%>%group_by(country)%>%summarize(avg_SR=mean(suicides.100k.pop))%>%arrange(-avg_SR)%>%head(10)
suiciderates%>%group_by(country)%>%summarize(avg_SR=mean(suicides.100k.pop))%>%arrange(avg_SR)%>%head(10)

# The following graph is summarizing the average SR per 100k population for 4 countries, each one from a different continent through the years:
avg_SR_per_year <- function(x){
  suiciderates %>% filter(country%in%x)%>%group_by(year)%>%summarize(avg_SR_per_year=mean(suicides.100k.pop))
}

avg_SR_per_year_US <- sapply("United States", avg_SR_per_year)
avg_SR_per_year_Greece <- sapply("Greece", avg_SR_per_year)
avg_SR_per_year_Japan <- sapply("Japan", avg_SR_per_year)
avg_SR_per_year_Brazil <- sapply("Brazil", avg_SR_per_year)

ggplot() + 
  geom_line(aes(x = avg_SR_per_year_US[[1]], y = avg_SR_per_year_US[[2]]), color = "black",size=1.5) +
  geom_text(aes(as.data.frame(avg_SR_per_year_US)$`United States`$year[30], as.data.frame(avg_SR_per_year_US)$`United States`$avg_SR_per_year[30],   label="United States"),position = position_nudge(y = -1.2))+
  geom_line(aes(x = avg_SR_per_year_Greece[[1]], y = avg_SR_per_year_Greece[[2]]), color = "blue",size=1.5) +
  geom_text(aes(as.data.frame(avg_SR_per_year_Greece)$`Greece`$year[30], as.data.frame(avg_SR_per_year_Greece)$`Greece`$avg_SR_per_year[30], label="Greece"),position = position_nudge(y = -1),col="blue")+
  geom_line(aes(x = avg_SR_per_year_Japan[[1]], y = avg_SR_per_year_Japan[[2]]), color = "red",size=1.5) +
  geom_text(aes(as.data.frame(avg_SR_per_year_Japan)$`Japan`$year[30], as.data.frame(avg_SR_per_year_Japan)$`Japan`$avg_SR_per_year[30],label="Japan"),position = position_nudge(y = 2),col="red")+
  geom_line(aes(x = avg_SR_per_year_Brazil[[1]], y = avg_SR_per_year_Brazil[[2]]), color = "green",size=1.5) +
  geom_text(aes(as.data.frame(avg_SR_per_year_Brazil)$`Brazil`$year[30],  as.data.frame(avg_SR_per_year_Brazil)$`Brazil`$avg_SR_per_year[30],label="Brazil"),position = position_nudge(y = 1),col="green")+
  xlab('Year') +
  scale_x_continuous(breaks = c(seq(1980, 2015, 2))) +
  ylab('Average Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 30, 5))) +
  ggtitle("Average Suicide Rates per Year ")

# Another important correlation of the suicide rate is the one with the sex:
suiciderates%>%ggplot(aes(sex,suicides.100k.pop))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  xlab('Sex') +
  ylab('Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 250, 25))) +
  ggtitle("Suicide Rate vs Sex")

mean_SR_male <- suiciderates%>%group_by(sex)%>%summarize(mean(suicides.100k.pop))%>%.[2,2]
mean_SR_male
mean_SR_female <- suiciderates%>%group_by(sex)%>%summarize(mean(suicides.100k.pop))%>%.[1,2]
mean_SR_female

# Moreover, the correlation of SR vs Age can be depicted:
suiciderates$age <- factor(suiciderates$age,c("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years"))
suiciderates%>%ggplot(aes(age,suicides.100k.pop))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  xlab('Age') +
  ylab('Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 250, 25))) +
  ggtitle("Suicide Rate vs Age")

# The following graph combines the factors of "Age" and "Sex" and represents the Suicide rate for each age category for female and males respectively
suiciderates%>%ggplot(aes(age,suicides.100k.pop,col=sex))+
  geom_boxplot(outlier.colour = "black", outlier.shape = 1)+
  xlab('Age') +
  ylab('Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 250, 25))) +
  ggtitle("Suicide Rate vs Age for Female and Male separately")

# Regarding the "generation" correlation, it can be depicted as below:
suiciderates%>%ggplot(aes(generation,suicides.100k.pop))+
  geom_boxplot(outlier.colour = "black", outlier.shape = 1)+
  xlab('Generation') +
  ylab('Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 250, 25))) +
  ggtitle("Suicide Rate vs Generation")

# Finally, the correlation between the average SR and the gpd_per_capita for each country is shown in the graph below where each point represents a country. 
# A zoomed graph is also included in the area of low gpd_per_capita in order to be easier for the reader to identify the rate for a specific country.
SR_GPD_plot <- suiciderates%>%group_by(country)%>%summarize(avg_SR=mean(suicides.100k.pop),avg_gdp=mean(gdp_per_capita))%>%
  ggplot(aes(avg_gdp,avg_SR))+
  geom_point(size=2)+
  geom_text(aes(avg_gdp,avg_SR, label=country),size=2.4,position = position_nudge(y = 1.2))+
  xlab('gpd_per_capita') +
  scale_x_continuous(breaks = c(seq(0, 70000, 5000))) +
  ylab('Suicide Rate') +
  scale_y_continuous(breaks = c(seq(0, 50, 5))) +
  ggtitle("Average Suicide Rate vs Average GPD_per_capita")

SR_GPD_plot

SR_GPD_plot_zoom <- SR_GPD_plot+
  scale_x_continuous(breaks = c(seq(0, 8000, 2000)))+
  ggtitle("Zoom on low GPD")+
  coord_cartesian(xlim=c(0,8000),ylim=c(0,15))

SR_GPD_plot_zoom

# As mentioned in this Chapter, there are 5298 unique values of the suicide rate feature ("suicides.100k.pop"). 
# In order to recude this value and be able to predict more accurately the suicide rate, this feature will be rounded to its integer. 
# The new column is named as "SR":
suiciderates <- suiciderates%>%mutate(SR=round(suicides.100k.pop,digits = 0))

# Identify the unique values of the rounded Suicide rate 
suiciderates %>%
  summarize( n_SR = n_distinct(SR))

# In order to get a strong insight of the distribution of the suicide rate through the 101 different countries, 
# the cumulative distribution function is plotted, as below:
plot(ecdf(suiciderates[,"SR"]),
     xlab="Suicide Rate",
     ylab="Cumulative Proportion",
     main="Suicide Rate cumulative distribution")

# Calculation of the five main quantiles (0, 25, 50, 75 and 100%) of the "SR" 
p <- seq(0,1,0.25)
quantile(suiciderates$SR,p)

# Based on these facts and the insights gained from the data visualization plots, a new column will be added in the original dataset, 
# where each observation/row is categorized based on the rounded Suicide rate per 100k population ("suicides.100k.pop"). 
# The categorization takes place as below:
# -  SR<=5 --> "Low"
# -  SR>5 and SR<=15 --> "Medium"
# -  SR>15 and SR<=30 --> "High"
# -  SR>30 --> "Very High"
suiciderates <- suiciderates%>%mutate(SR_cat=
                                        ifelse(SR<=5,"Low",
                                               ifelse(SR>5 & SR<=15, "Medium", 
                                                      ifelse(SR>15 & SR<=30, "High", "Very High"))))

# New structure of the "suiciderates" dataset
head(suiciderates) %>%print.data.frame()



###################
# Modeling approach
###################

# Based on the insights gained through the dataset investigation, a prediction model can now be developed. 
# The main prediction target is the Suicide rate category ("SR_cat") by using multiple learning-methods that were thoroughly examined during the Data Schience course. 
# The methods that are applied belong to the Classification algorithms and are the following:
# 1) Baseline prediction by guessing the outcome
# 2) Predicting SR_cat by Sex
# 3) Predicting SR_cat by Age
# 4) Predicting SR_cat by Sex and Age
# 5) Predicting SR_cat by GDP_per_capita / LDA
# 6) Predicting SR_cat by GDP_per_capita / GLM
# 7) Predicting SR_cat by Sex, Age, GDP, Generation, Country / GLM
# 8) Predicting SR_cat by Sex, Age, GDP, Generation, Country / KNN
# 9) Predicting SR_cat by Sex, Age, GDP, Generation, Country / KNN & 10-fold cross validation
# 10) Predicting SR_cat by Sex, Age, GDP, Generation, Country / Classification tree
# 11) Predicting SR_cat by Sex, Age, GDP, Generation, Country / Random forest

# In order to reduce the size of the original dataset, only the useful predictors will be kept while the rest of them will be filtered based on the code:
suiciderates_clean<-suiciderates%>%select(SR_cat,country,year,sex,age,gdp_per_capita,generation,SR)

# Partition of the "suiciderates_clean" set into "train" and "test" set. 
# The train-set is used for the training of each model, while the test-set is used for the evaluation of each method.
# Note that test-set is set equal to 10% of the original dataset.
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = suiciderates_clean$SR_cat, times = 1, p = 0.1, list = FALSE)
train_set<-suiciderates_clean[-test_index,]
test_set<-suiciderates_clean[test_index,]


### 1. Baseline prediction by guessing the outcome

# The first simplified model is built based on guessing the outcome (Suicide Rate category) with equal probabiblities
# The Accuracy of the simple guessing is calculated in comparison to the test-set values.
guess <- sample(c("Low","Medium","High","Very High"), nrow(test_set), replace = TRUE)
accuracy_guessing<-mean(guess == test_set$SR_cat)

# Create a Summarize Table/Data-frame where the accuracies of each method are represented.
# Add the first accuracy of the simple guessing method
Accuracy_sum <- data_frame(method = "Simple Guessing", Accuracy = accuracy_guessing)
Accuracy_sum %>% knitr::kable()



### 2. Predicting SR_cat by Sex

# As a second step, the prediction will be performed based only on the sex. 
# For this reason, the average SR of the females (always corresponding to the train-set) and the males are calculated as below:

# average SR of training set for females
train_set %>%
  group_by(sex) %>%
  summarize(SR = mean(SR)) %>%
  filter(sex == "female") %>%
  pull(SR)
# >5 thus Medium

# average SR of training set for males
train_set %>%
  group_by(sex) %>%
  summarize(SR = mean(SR)) %>%
  filter(sex == "male") %>%
  pull(SR)
# >15 and <30 thus High

# Based on results above, the new prediction based only on the Sex will be:
sex_model <- ifelse(test_set$sex == "female", "Medium", "High")    # predict "Medium" if female, "0"High" if male
accuracy_by_sex <- mean(sex_model == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex",Accuracy = accuracy_by_sex))
Accuracy_sum %>% knitr::kable()



### 3. Predicting SR_cat by Age

# Similar as before, the average SR for each Age category (always for the train-set) is calculated:
train_set %>%
  group_by(age) %>%
  summarize(SR = mean(SR))

# Prediction and accuracy based on Age
age_model <- ifelse(test_set$age == "5-14 years", "Low", ifelse(test_set$age == "15-24 years","Medium", ifelse(test_set$age == "25-34 years","Medium", ifelse(test_set$age == "35-54 years","Medium", ifelse(test_set$age == "55-74 years","High", "High"))))) 
accuracy_by_age <- mean(age_model == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Age",Accuracy = accuracy_by_age))
Accuracy_sum %>% knitr::kable()



### 4. Predicting SR_cat by Sex and Age

# A combined prediction is developed for this step. 
# As a prediction example, when the the age-group is equal to "35-54 years" and the sex is "female", then the prediction will be "Medium", 
# while if the sex is equal to "male", the prediction will be "High".

# Prediction and accuracy based on Sex and Age
sex_age_model <- ifelse(test_set$age == "5-14 years", "Low", ifelse(test_set$age == "15-24 years","Medium", ifelse(test_set$age == "25-34 years" & test_set$sex == "female","Medium", ifelse(test_set$age == "25-34 years" & test_set$sex == "male","High",ifelse(test_set$age == "35-54 years" & test_set$sex == "female","Medium",ifelse(test_set$age == "35-54 years" & test_set$sex == "male","High" ,ifelse(test_set$age == "55-74 years" & test_set$sex == "female","Medium",ifelse(test_set$age == "55-74 years" & test_set$sex == "male","High",ifelse(test_set$age == "75+ years" & test_set$sex == "female","Medium", "High"))))))))) 
accuracy_by_sex_age <- mean(sex_age_model == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex and Age",Accuracy = accuracy_by_sex_age))
Accuracy_sum %>% knitr::kable()



### 5. Predicting SR_cat by GDP_per_capita - LDA

# In the fifth step of the learning algorithm, the LDA method is examined. In this case, only one predictor is used, the "gdp_per_capita".
# The training is performed with the "train" function and the corresponding arguments

# Prediction and accuracy based on GDP_per_capita and LDA method
train_lda <- train(SR_cat ~ gdp_per_capita, method = "lda", data = train_set)
lda_preds <- predict(train_lda, test_set)
accuracy_by_gdp_LDA <- mean(lda_preds == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by GDP_per_capita - LDA",Accuracy = accuracy_by_gdp_LDA))
Accuracy_sum %>% knitr::kable()



### 6. Predicting SR_cat by GDP_per_capita - Generalized Linear Model

# As the number of the predictors increase, the LDA and QDA methods cannot provide a higher accuracy. 
# Thus, in the sixth step, the Generalized Linear model (GLM) is examined. Initially, only one predictor (gdp_per_capita) is considered:

# Prediction and accuracy based on GDP_per_capita and GLM method
train_gdp_glm <- train(SR ~ gdp_per_capita, method = "glm", data = train_set)
glm_gdp_preds <- predict(train_gdp_glm, test_set)
glm_gdp_preds <- as.data.frame(glm_gdp_preds)
glm_gdp_preds <- glm_gdp_preds%>%mutate(SR_cat=
                                          ifelse(glm_gdp_preds<=5,"Low",
                                                 ifelse(glm_gdp_preds>5 & glm_gdp_preds<=15, "Medium", 
                                                        ifelse(glm_gdp_preds>15 & glm_gdp_preds<=30, "High", "Very High"))))
accuracy_by_gdp_glm <- mean(glm_gdp_preds$SR_cat == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by GDP_per_capita - GLM",Accuracy = accuracy_by_gdp_glm))
Accuracy_sum %>% knitr::kable()



### 7. Predicting SR_cat by Sex, Age, GDP, Generation, Country - GLM

# A more complex model with five predictors is examined. The"GLM" method is again applied:

# Prediction and accuracy based on Sex, Age, GDP, Generation, Country and GLM method
train_glm <- train(SR ~ sex+age+gdp_per_capita+generation+country, method = "glm", data = train_set)
glm_preds <- predict(train_glm, test_set)
glm_preds <- as.data.frame(glm_preds)
glm_preds <- glm_preds%>%mutate(SR_cat= ifelse(glm_preds<=5,"Low",
                                               ifelse(glm_preds>5 & glm_preds<=15, "Medium", 
                                                      ifelse(glm_preds>15 & glm_preds<=30, "High", "Very High"))))
accuracy_by_glm <- mean(glm_preds$SR_cat == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex, Age, GDP, Generation, Country - GLM",Accuracy = accuracy_by_glm))
Accuracy_sum %>% knitr::kable()



### 8. Predicting SR_cat by Sex, Age, GDP, Generation, Country - KNN

# As a next step, the knn method, that was throroughly explained during the Machine Learning course of the Data Science course, is applied.
# In order to estimate the value of the K that could provide the highest model's accuracy, the "tuneGrid" argument of the "train" function is activated.
# The provided range of the K is 3 to 11 with a step of 2.
# The training of the algorithm is based only on the train-set, thus the optimum K value is calculated based only on this.

###############################################################################
############# Note that this process takes a lot of time to run ###############
###############################################################################

# Training based on Sex, Age, GDP, Generation and KNN method/k optimization
train_knn <- train(SR ~ sex+age+gdp_per_capita+generation+country, method = "knn", data = train_set, tuneGrid = data.frame(k = seq(3, 11, 2)))

# Plot of the the calculated RMSE (root-mean-square deviation) for each k value.
# The optimal k is the one that minimizes the RMSEs.
ggplot(train_knn, highlight=TRUE)

# Best tune of KNN
train_knn$bestTune

# Prediction and accuracy based on Sex, Age, GDP, Generation and KNN method/optimized k
knn_preds <- predict(train_knn, test_set)
knn_preds <- as.data.frame(knn_preds)
knn_preds <- knn_preds%>%mutate(SR_cat= ifelse(knn_preds<=5,"Low",
                                               ifelse(knn_preds>5 & knn_preds<=15, "Medium", 
                                                      ifelse(knn_preds>15 & knn_preds<=30, "High", "Very High"))))
accuracy_by_knn <- mean(knn_preds$SR_cat == test_set$SR_cat)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex, Age, GDP, Generation, Country - KNN",Accuracy = accuracy_by_knn))
Accuracy_sum %>% knitr::kable()



### 9. Predicting SR_cat by Sex, Age, GDP, Generation, Country - KNN & 10-fold cross validation

# The 10-fold cross validation is examined as a next step. 
# The following code performs the training of the new model, the calculation of the new predictions and the respective accuracy. 
# The knn method is again optimized based on several k values (the range is 3 to 15 with a step of 2) for the train dataset only.

###############################################################################
############# Note that this process takes a lot of time to run ###############
###############################################################################

# Prediction, accuracy and best-tune basedon Sex, Age, GDP, Generation and KNN/10-fold cross validation method
train_knn_cv <- train(SR ~ sex+age+gdp_per_capita+generation+country,
                      method = "knn",
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 15, 2)),
                      trControl = trainControl(method = "cv", number = 10, p = 0.9))
knn_cv_preds <- predict(train_knn_cv, test_set)
knn_cv_preds <- as.data.frame(knn_cv_preds)
knn_cv_preds <- knn_cv_preds%>%mutate(SR_cat= ifelse(knn_cv_preds<=5,"Low",
                                                     ifelse(knn_cv_preds>5 & knn_cv_preds<=15, "Medium", 
                                                            ifelse(knn_cv_preds>15 & knn_cv_preds<=30, "High", "Very High"))))
accuracy_by_knn_cv <- mean(knn_cv_preds$SR_cat == test_set$SR_cat)

train_knn_cv$bestTune

# Plot of the the calculated RMSE (root-mean-square deviation) for each k value.
ggplot(train_knn_cv, highlight=TRUE)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex, Age, GDP, Generation, Country - KNN & 10-fold cross validation",Accuracy = accuracy_by_knn_cv))
Accuracy_sum %>% knitr::kable()



### 10. Predicting SR_cat by Sex, Age, GDP, Generation, Country - Classification tree

# Classification or decision tree is a predictive model that is based on an iterative process of splitting the data into partitions 
# and then keep splitting it up further on multiple branches (specific observation values). 
# The process continues until no more useful splits can be found.
# The classification tree can be optimized based on the "complexity parameter" (cp). 
# This parameter iss used to control the size of the decision tree and to select the optimal tree size.

# The following code is used to train and optimize the algorithm based on the classification tree method.
# Similar to the last steps, the same five predicors are used and the training and optimization is only based on the train-set.

###############################################################################
############# Note that this process takes a lot of time to run ###############
###############################################################################

# Training of the algorithm based on Sex, Age, GDP, Generation and Classification tree method
train_rpart <- train(SR ~ sex+age+gdp_per_capita+generation+country,
                     method = "rpart",
                     data = train_set,
                     tuneGrid = data.frame(cp = seq(0, 0.04, 0.002)))

# Prediction, accuracy and best-tune based on Sex, Age, GDP, Generation and Classification tree method
rpart_preds <- predict(train_rpart, test_set)
rpart_preds <- as.data.frame(rpart_preds)
rpart_preds <- rpart_preds%>%mutate(SR_cat= ifelse(rpart_preds<=5,"Low",
                                                   ifelse(rpart_preds>5 & rpart_preds<=15, "Medium", 
                                                          ifelse(rpart_preds>15 & rpart_preds<=30, "High", "Very High"))))
accuracy_by_rpart <- mean(rpart_preds$SR_cat == test_set$SR_cat)

train_rpart$bestTune

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex, Age, GDP, Generation, Country - Classification tree",Accuracy = accuracy_by_rpart))
Accuracy_sum %>% knitr::kable()



### 11. Predicting SR_cat by Sex, Age, GDP, Generation, Country - Random forest

# As a final step, the Random Forest model is examined. 
# The Random Forest consists of a large number of individual decision trees that operate as a total.
# The Random Forest model requires even higher CPU effort due to the multiple decision trees, thus the respective code is takes a lot of time to run.
# The code below is training the new fit based on the Random Forest model (using the same five predictors and the train-set) 
# as well as calculating the new predictions and the accuracy. 
# The parameter that is optimzied is the "mtry". This parameter refers to the number of variables available for splitting at each tree node and it have a range of 1 to 20.

###############################################################################
############# Note that this process takes a lot of time to run ###############
###############################################################################

# Prediction, accuracy and optimization based on Sex, Age, GDP, Generation and Random forest method
train_rf <- train(SR ~ sex+age+gdp_per_capita+generation+country,
                  data = train_set,
                  method = "rf",
                  ntree = 100,
                  tuneGrid = data.frame(mtry = seq(1:20)))

rf_preds <- predict(train_rf, test_set)
rf_preds <- as.data.frame(rf_preds)
rf_preds <- rf_preds%>%mutate(SR_cat= ifelse(rf_preds<=5,"Low",
                                             ifelse(rf_preds>5 & rf_preds<=15, "Medium", 
                                                    ifelse(rf_preds>15 & rf_preds<=30, "High", "Very High"))))
accuracy_by_rf <- mean(rf_preds$SR_cat == test_set$SR_cat)

# The optimal "mtry" value is shown in the following plot:
ggplot(train_rf, highlight=TRUE)

# By using the "varImp" function, the variable importance of several objects can be represented. 
# The top-10 objects with the highest importance are shown as below:
varImp(train_rf)

# Save Accuracy in the existing data frame
Accuracy_sum <- bind_rows(Accuracy_sum,data_frame(method="Predicting SR_cat by Sex, Age, GDP, Generation, Country - Random forest",Accuracy = accuracy_by_rf))
Accuracy_sum %>% knitr::kable()







##### Results

# The summarized Accuracy values of all the presented models are depicted below:
Accuracy_sum %>% knitr::kable()

# The highest accuracy on the prediction of the Suicide rates Category was achieved through the Random Forest model.
# This accuracy is satisfactory and thus chosen as the optimal solution.
print(accuracy_by_rf)







##### Conclusion

# This report presents a thorough analysis on the worldwide Suicide Rates, based on the dataset of "Suicide Rates Overview 1985 to 2016" found in the website of "kaggle". 
# Furthermore, a machine learning algorithm, able to predict the Suicide rate categorization from "Low" to "Very High" with an accuracy of 0.81 is succesfully developed. 
# During the process, multiple predictive methods are discussed and tested in order to procide a complete picture around the algorithm's development.





##### Appendix

print("Operating System:")
version

