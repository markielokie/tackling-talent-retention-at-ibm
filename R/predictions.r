### This script is to perform the prediction modeling

## Set working directory to source file location
## Ensure that ibm_clean.r has been run first before running this script

## Load libraries
library(dplyr)
library(correlationfunnel)
library(plotly)
library(caret)
library(corrplot)
library(car)
library(ROCR)
library(rpart)
library(cluster) 
library(DT)
library(forcats)
library(skimr) 
library(flexclust)

## Split data

# Split data
set.seed(1000)
split <- createDataPartition(dat_hr_final$Attrition, p=0.7, list=F, groups=100)
train <- dat_hr_final[split,]
test <- dat_hr_final[-split,]

## Logistic regression with Backward Selection - Model 1

model1 <- glm(Attrition~., family="binomial", data=train)
summary(model1)

# Backward selection
start_mod <- glm(Attrition~., family="binomial", data=train)
empty_mod <- glm(Attrition~1, family="binomial", data=train)
full_mod <- glm(Attrition~., family="binomial", data=train)
model_bw <- step(start_mod,
                 scope=list(upper=full_mod, lower=empty_mod),
                 direction="backward")
summary(model_bw)

# Check VIF
vif(model_bw)

# Drop highest VIF (JobLevel)
model_bw <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+
                  DistanceFromHome+EnvironmentSatisfaction+Gender+
                  JobInvolvement+JobSatisfaction+MonthlyIncome+
                  NumCompaniesWorked+OverTime+PercentSalaryHike+
                  RelationshipSatisfaction+StockOptionLevel+
                  TotalWorkingYears+TrainingTimesLastYear+
                  WorkLifeBalance+YearsSinceLastPromotion+
                  YearsWithCurrManager, family="binomial", data=train)

vif(model_bw) # The VIF seems good

# Predict
pred <- predict(model_bw, newdata=test, "response") # we want to reduce FN
ct <- table(attrition = test$Attrition,
            predictions = as.integer(pred>0.5))
ct
accuracy_df <- data.frame(accuracy = sum(ct[1,1],ct[2,2])/nrow(test),
                          specificity = ct[1,1]/sum(ct[1,1],ct[1,2]),
                          sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]))
accuracy_df

# Plot AUC
ROCRpred <- prediction(pred, test$Attrition)
ROCRperf <- performance(ROCRpred, "acc", "fnr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Threshold should be around 0.2
as.numeric(performance(ROCRpred,"auc")@y.values) # AUC = 0.8940799

# Use 0.2 as threshold
ct_0.2 <- table(attrition = test$Attrition,
                predictions = as.integer(pred>0.2))
ct_0.2
accuracy_df_0.2 <- data.frame(accuracy = sum(ct_0.2[1,1],ct_0.2[2,2])/nrow(test),
                              specificity = ct_0.2[1,1]/sum(ct_0.2[1,1],ct_0.2[1,2]),
                              sensitivity = ct_0.2[2,2]/sum(ct_0.2[2,1],ct_0.2[2,2]))
accuracy_df_0.2

## Logistic regression using sentiment scores as predictors (pros) - Model 2

# Logistic regression
model_senti <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+
                     DistanceFromHome+EnvironmentSatisfaction+Gender+
                     JobInvolvement+JobSatisfaction+MonthlyIncome+
                     NumCompaniesWorked+OverTime+PercentSalaryHike+
                     RelationshipSatisfaction+StockOptionLevel+
                     TotalWorkingYears+TrainingTimesLastYear+
                     WorkLifeBalance+YearsSinceLastPromotion+
                     YearsWithCurrManager+ave_sentiment_pros, 
                   family="binomial", data=train)

vif(model_senti) # VIF looks good

# Predict
pred_senti <- predict(model_senti, newdata=test, "response")
ct_senti <- table(attrition = test$Attrition,
                  predictions = as.integer(pred_senti>0.5))
ct_senti
accuracy_senti_df <- data.frame(accuracy = sum(ct_senti[1,1],ct_senti[2,2])/nrow(test),
                                specificity = ct_senti[1,1]/sum(ct_senti[1,1],ct_senti[1,2]),
                                sensitivity = ct_senti[2,2]/sum(ct_senti[2,1],ct_senti[2,2]))
accuracy_senti_df

# Plot AUC
ROCRpred_senti <- prediction(pred_senti, test$Attrition)
ROCRperf_senti <- performance(ROCRpred_senti, "acc", "fnr")
plot(ROCRperf_senti, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Threshold should be around 0.19
as.numeric(performance(ROCRpred_senti,"auc")@y.values) # AUC = 0.8982404

# Use 0.24 as threshold
ct_senti_0.24 <- table(attrition = test$Attrition,
                       predictions = as.integer(pred_senti>0.24))
ct_senti_0.24
accuracy_senti_0.24 <- data.frame(accuracy = sum(ct_senti_0.24[1,1],ct_senti_0.24[2,2])/nrow(test),
                                  specificity = ct_senti_0.24[1,1]/sum(ct_senti_0.24[1,1],ct_senti_0.24[1,2]),
                                  sensitivity = ct_senti_0.24[2,2]/sum(ct_senti_0.24[2,1],ct_senti_0.24[2,2]))
accuracy_senti_0.24

## Logistic regression using sentiment scores as predictors (cons) - Model 3

# Logistic regression
model_senti2 <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+
                      DistanceFromHome+EnvironmentSatisfaction+Gender+
                      JobInvolvement+JobSatisfaction+MonthlyIncome+
                      NumCompaniesWorked+OverTime+PercentSalaryHike+
                      RelationshipSatisfaction+StockOptionLevel+
                      TotalWorkingYears+TrainingTimesLastYear+
                      WorkLifeBalance+YearsSinceLastPromotion+
                      YearsWithCurrManager+ave_sentiment_cons, 
                    family="binomial", data=train)

vif(model_senti2) # VIF looks good

# Predict
pred_senti2 <- predict(model_senti2, newdata=test, "response")
ct_senti2 <- table(attrition = test$Attrition,
                   predictions = as.integer(pred_senti2>0.5))
ct_senti2
accuracy_senti2_df <- data.frame(accuracy = sum(ct_senti2[1,1],ct_senti2[2,2])/nrow(test),
                                 specificity = ct_senti2[1,1]/sum(ct_senti2[1,1],ct_senti2[1,2]),
                                 sensitivity = ct_senti2[2,2]/sum(ct_senti2[2,1],ct_senti2[2,2]))
accuracy_senti2_df

# Plot AUC
ROCRpred_senti2 <- prediction(pred_senti2, test$Attrition)
ROCRperf_senti2 <- performance(ROCRpred_senti2, "acc", "fnr")
plot(ROCRperf_senti2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Threshold should be around 0.19
as.numeric(performance(ROCRpred_senti2,"auc")@y.values) # AUC = 0.9003015

# Use 0.245 as threshold
ct_senti2_0.245 <- table(attrition = test$Attrition,
                         predictions = as.integer(pred_senti2>0.245))
ct_senti2_0.245
accuracy_senti2_0.245 <- data.frame(accuracy = sum(ct_senti2_0.245[1,1],ct_senti2_0.245[2,2])/nrow(test),
                                    specificity = ct_senti2_0.245[1,1]/sum(ct_senti2_0.245[1,1],ct_senti2_0.245[1,2]),
                                    sensitivity = ct_senti2_0.245[2,2]/sum(ct_senti2_0.245[2,1],ct_senti2_0.245[2,2]))
accuracy_senti2_0.245

## Logistic regression with clustering - Model 4

# Binarize continuous variables  and one hot encode factors
corr_tbl  <- dat_hr %>%
  select(-EmployeeNumber) %>%
  binarize(n_bins=5, thresh_infreq=0.01, name_infreq="OTHER", one_hot=T) %>%
  correlate(Attrition__Yes)

# Plot correlation funnel
corr_tbl %>%
  plot_correlation_funnel() %>%
  ggplotly()

# Select top correlated variables for analysis
var_selection <- c("EmployeeNumber", "Attrition", "OverTime", "JobLevel", 
                   "MonthlyIncome", "YearsAtCompany", "StockOptionLevel",
                   "YearsWithCurrManager", "TotalWorkingYears", "MaritalStatus",
                   "Age", "YearsInCurrentRole", "JobRole", "EnvironmentSatisfaction",
                   "JobInvolvement", "BusinessTravel")

dat_hr_subset <- dat_hr %>%
  select(one_of(var_selection)) %>%
  mutate_if(is.character, as.factor)

# Use Gower distance to handle both numeric and factor types
gower_dist <- daisy(dat_hr_subset[,2:13], metric="gower")
gower_mat <- as.matrix(gower_dist)

# View most similar employees to test Gower distance
similar_tbl <- dat_hr_subset[which(gower_mat==min(gower_mat[gower_mat!=min(gower_mat)]), arr.ind=T)[1,],]
similar_tbl

# Choose number of clusters using silhouette plot
silhouette_width <- sapply(2:10, FUN=function(x){
  pam(x=gower_dist, k=x)$silinfo$avg.width
})

ggplot(data=data.frame(cluster = 2:10,silhouette_width),aes(x=cluster,y=silhouette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1)) # choose 7 clusters soln

# Perform clustering using PAM
k <- 7
pam_fit <- pam(gower_dist, diss=T, k)

# Add cluster assignments to each employee
dat_hr_clusters <- dat_hr_subset %>%
  mutate(cluster=pam_fit$clustering)

# Look at medoids to understand each cluster
datatable(dat_hr_clusters[pam_fit$medoids,])

# Group by attrition rate
attrition_rate_tbl <- dat_hr_clusters %>%
  select(cluster, Attrition) %>%
  mutate(attrition_num = fct_relevel(Attrition, "No", "Yes") %>% as.numeric()-1) %>%
  group_by(cluster) %>%
  summarise(
    Cluster_Turnover_Rate = (sum(attrition_num)/length(attrition_num))*100,
    Turnover_Count = sum(attrition_num),
    Cluster_Size = length(attrition_num)
  ) %>%
  ungroup() %>%
  mutate(Population_Turnover_Rate = (Turnover_Count/sum(Turnover_Count))*100)

datatable(attrition_rate_tbl)

## Using clustering results as predictors

# Merge clusters with IBM HR data
dat_hr_full <- dat_hr_final %>%
  mutate(cluster=pam_fit$clustering)

# Split data
set.seed(1000)
split2 <- createDataPartition(dat_hr_full$Attrition, p=0.7, list=F, groups=100)
train2 <- dat_hr_full[split2,]
test2 <- dat_hr_full[-split2,]

# Logistic regression
model_clust <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+
                     DistanceFromHome+EnvironmentSatisfaction+Gender+
                     JobInvolvement+JobSatisfaction+MonthlyIncome+
                     NumCompaniesWorked+OverTime+PercentSalaryHike+
                     RelationshipSatisfaction+StockOptionLevel+
                     TotalWorkingYears+TrainingTimesLastYear+
                     WorkLifeBalance+YearsSinceLastPromotion+
                     YearsWithCurrManager+cluster, 
                   family="binomial", data=train2)

# Predict
pred_clust <- predict(model_clust, newdata=test2, "response")
ct_clust <- table(attrition = test2$Attrition,
                  predictions = as.integer(pred_clust>0.5))
ct_clust
accuracy_clust_df <- data.frame(accuracy = sum(ct_clust[1,1],ct_clust[2,2])/nrow(test2),
                                specificity = ct_clust[1,1]/sum(ct_clust[1,1],ct_clust[1,2]),
                                sensitivity = ct_clust[2,2]/sum(ct_clust[2,1],ct_clust[2,2]))
accuracy_clust_df

# Plot AUC
ROCRpred_clust <- prediction(pred_clust, test2$Attrition)
ROCRperf_clust <- performance(ROCRpred_clust, "acc", "fnr")
plot(ROCRperf_clust, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred_clust,"auc")@y.values) # AUC = 0.8963319

# Use 0.22 as threshold
ct_clust_0.22 <- table(attrition = test2$Attrition,
                       predictions = as.integer(pred_clust>0.22))
ct_clust_0.22
accuracy_clust_0.22 <- data.frame(accuracy = sum(ct_clust_0.22[1,1],ct_clust_0.22[2,2])/nrow(test2),
                                  specificity = ct_clust_0.22[1,1]/sum(ct_clust_0.22[1,1],ct_clust_0.22[1,2]),
                                  sensitivity = ct_clust_0.22[2,2]/sum(ct_clust_0.22[2,1],ct_clust_0.22[2,2]))
accuracy_clust_0.22

## Logistic regression using both clustering and sentiment scores as predictors - Model 5

# Logistic regression
model_clust_senti <- glm(Attrition~Age+BusinessTravel+DailyRate+Department+
                           DistanceFromHome+EnvironmentSatisfaction+Gender+
                           JobInvolvement+JobSatisfaction+MonthlyIncome+
                           NumCompaniesWorked+OverTime+PercentSalaryHike+
                           RelationshipSatisfaction+StockOptionLevel+
                           TotalWorkingYears+TrainingTimesLastYear+
                           WorkLifeBalance+YearsSinceLastPromotion+
                           YearsWithCurrManager+cluster+ave_sentiment_cons, 
                         family="binomial", data=train2)

# Predict
pred_clust_senti <- predict(model_clust_senti, newdata=test2, "response")
ct_clust_senti <- table(attrition = test2$Attrition,
                        predictions = as.integer(pred_clust_senti>0.5))
ct_clust_senti
accuracy_clust_senti_df <- data.frame(accuracy = sum(ct_clust_senti[1,1],ct_clust_senti[2,2])/nrow(test2),
                                      specificity = ct_clust_senti[1,1]/sum(ct_clust_senti[1,1],ct_clust_senti[1,2]),
                                      sensitivity = ct_clust_senti[2,2]/sum(ct_clust_senti[2,1],ct_clust_senti[2,2]))
accuracy_clust_senti_df

# Plot AUC
ROCRpred_clust_senti <- prediction(pred_clust_senti, test2$Attrition)
ROCRperf_clust_senti <- performance(ROCRpred_clust_senti, "acc", "fnr")
plot(ROCRperf_clust_senti, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
as.numeric(performance(ROCRpred_clust_senti,"auc")@y.values) # AUC = 0.9015611

# Use 0.26 as threshold
ct_clust_senti_0.26 <- table(attrition = test2$Attrition,
                             predictions = as.integer(pred_clust_senti>0.26))
ct_clust_senti_0.26
accuracy_clust_senti_0.26 <- data.frame(accuracy = sum(ct_clust_senti_0.26[1,1],ct_clust_senti_0.26[2,2])/nrow(test2),
                                        specificity = ct_clust_senti_0.26[1,1]/sum(ct_clust_senti_0.26[1,1],ct_clust_senti_0.26[1,2]),
                                        sensitivity = ct_clust_senti_0.26[2,2]/sum(ct_clust_senti_0.26[2,1],ct_clust_senti_0.26[2,2]))
accuracy_clust_senti_0.26

## Decision Trees

# Model using CV
trControl = trainControl(method="cv",number=10) #10-fold cross validation
tuneGrid = expand.grid(.cp=seq(0,0.1,0.001)) 

set.seed(777)
trainCV = train(Attrition~., 
                data=train,
                method="rpart", 
                trControl=trControl,
                tuneGrid=tuneGrid)

trainCV$bestTune # cp = 0.02

tree_CV <- rpart(Attrition~.,
                 data=train,
                 method="class",
                 control=rpart.control(cp=trainCV$bestTune))

# Predict
pred_CV = predict(tree_CV, newdata=test, type='class')
ct_tree = table(attrition = test$Attrition, 
                predictions = pred_CV)
ct_tree
accuracy_tree_df <- data.frame(accuracy = sum(ct_tree[1,1],ct_tree[2,2])/nrow(test),
                               specificity = ct_tree[1,1]/sum(ct_tree[1,1],ct_tree[1,2]),
                               sensitivity = ct_tree[2,2]/sum(ct_tree[2,1],ct_tree[2,2]))
accuracy_tree_df

# Plot AUC
pred_tree_auc <- predict(tree_CV, newdata=test, type="prob")
ROCRpred_tree <- prediction(pred_tree_auc[,2], test$Attrition)
ROCRperf_tree <- performance(ROCRpred_tree, "acc", "fnr")
plot(ROCRperf_tree, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7)) # Threshold should be around 0.4
as.numeric(performance(ROCRpred_tree,"auc")@y.values) # AUC = 0.6325814

## Summary of Results

rbind(accuracy_df_0.2, accuracy_senti_0.24, accuracy_senti2_0.245,
      accuracy_clust_0.22, accuracy_clust_senti_0.26, accuracy_tree_df) %>%
  as.data.frame() %>%
  dplyr::mutate(model = c("1", "2", "3", "4", "5", "6"),
                desc = c("log mod with bw select",
                         "log mod with senti (pros)",
                         "log mod with senti (cons)",
                         "log mod with clust",
                         "log mod with senti & clust",
                         "tree with 10-fold cv"),
                auc = c(as.numeric(performance(ROCRpred,"auc")@y.values),
                        as.numeric(performance(ROCRpred_senti,"auc")@y.values),
                        as.numeric(performance(ROCRpred_senti2,"auc")@y.values),
                        as.numeric(performance(ROCRpred_clust,"auc")@y.values),
                        as.numeric(performance(ROCRpred_clust_senti,"auc")@y.values),
                        as.numeric(performance(ROCRpred_tree,"auc")@y.values))) %>%
  dplyr::select(model:auc, everything())

## Discussion

summary(model_clust_senti)

var_imp <- varImp(model_clust_senti) %>%
  dplyr::arrange(desc(Overall))

var_imp %>%
  ggplot(aes(x=reorder(rownames(var_imp),Overall), y=Overall)) +
  geom_point(color="blue", size=4, alpha=0.6) +
  geom_segment(aes(x=rownames(var_imp), xend=rownames(var_imp), y=0, yend=Overall),
               color="skyblue") +
  xlab("Variable") +
  ylab("Overall Importance") +
  theme_classic() +
  coord_flip()

# Probability of attrition if employee works overtime
exp(model_clust_senti$coef[1]+model_clust_senti$coef[21])/
  (1+exp(model_clust_senti$coef[1]+model_clust_senti$coef[21]))