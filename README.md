title: "Machine learning example in R Example"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
The code below is an example of using machine learning to predict housing generally based on Kuhn (2019) guide: https://topepo.github.io/caret/

telehealth_noms_wide_noms is a data set from the national outcomes measures (NOMS), and the codebook is available here: https://drive.google.com/file/d/1BBKuV3A6tAaACF5DljuvYLDHRxr1q_iG/view?usp=sharing 

In the section below, I am sub-setting the variables that I want to include.  The variables consist of counts, ordinal, and binary variables.  These data are matched pairs from intake to 6-month (.x is at intake, and .y is 6-month).

Below I demonstrate first how I selected variables that myself and a team of content experts believed were potentially predictive of housing at 6-months.  I then identified that all the "VT" variables, school and work, and living conditions satisfaction variables were missing more significantly more than 50%.  Given these variables were missing significantly more than 50% and research generally discourages imputing variables (discussed later) with more than 50% missing data, we removed those variables (Manly & Wells, 2015).  

We then created several binary demographics from nominal variables.  First, we evaluated the top three primary diagnoses at baseline and created binary variables for those diagnoses.  We also created a sexual identity variable as another sexual identity and heterosexual and a gender variable as male and all other gender identities.  We also included housing at baseline and 6-months (i.e., the outcome variable). 
```{r}
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
telehealth_noms_wide_noms = read.csv("telehealth_noms_wide_noms_8_10_20.csv", header = TRUE)
machine_dat =  telehealth_noms_wide_noms[c("Quarter.x", "DiagnosisOne.x", "Gender.x", "HispanicLatino.x", "RaceWhite.x", "RaceBlack.x", "Agegroup.x", "SexualIdentity.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "SchoolOrWork.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "Cocaine_Use.x", "Meth_Use.x", "RxOpioids_Use.x", "StreetOpioids_Use.x", "ViolenceTrauma.x", "VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "PhysicallyHurt.x", "NightsHospitalMHC.x", "NightsDetox.x", "NightsJail.x", "TimesER.x", "Housing.x", "LivingConditionsSatisfaction.x", "Education.x", "Employment.x", "EnoughMoneyForNeeds.x", "NumTimesArrested.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x", "telehealth.x", "Housing.y")]
library(naniar)
miss_var_summary(machine_dat)
# Remove variables missing significantly more than 50%
machine_dat[,c("VT_NightmaresThoughts.x", "VT_NotThinkAboutIt.x", "VT_OnGuard.x", "VT_NumbDetached.x", "SchoolOrWork.x", "LivingConditionsSatisfaction.x")] = list(NULL)


#DiagnosisOne.x 62 = anxiety, 59 = mdd recurrent, 58 mdd single episode
machine_dat$anxiety = ifelse(machine_dat$DiagnosisOne.x == 62, 1, 0) 
machine_dat$mdd_r= ifelse(machine_dat$DiagnosisOne.x == 59, 1, 0)
machine_dat$mdd_s = ifelse(machine_dat$DiagnosisOne.x == 58, 1, 0)
machine_dat$DiagnosisOne.x = NULL
#SexualIdentity.x 1,2,3,4 create another category for 3 and 4
machine_dat$another_sex_ident =  ifelse(machine_dat$SexualIdentity.x > 1, 1, 0)
machine_dat$SexualIdentity.x = NULL

### Change gender to male or female / another gender identity
machine_dat$Gender.x = ifelse(machine_dat$Gender.x == 1,1,0)

#### Employment 
machine_dat$Employment.x = ifelse(machine_dat$Employment.x <3,1,0)

#Housing.x and y 1 = OWNED OR RENTED HOUSE, APARTMENT, TRAILER, ROOM
machine_dat$Housing.x = ifelse(machine_dat$Housing.x == 1, 1,0)
machine_dat$Housing.y = ifelse(machine_dat$Housing.y == 1, 1,0)
#1= EMPLOYED FULL TIME (35+ HOURS PER WEEK, OR WOULD HAVE BEEN)
#2 = EMPLOYED PART TIME



```
We next continued to pre-process the data by identifying near-zero variance variables.  We used the nearZeroVar function in caret to identify variables with large (i.e., higher than 19) ratios of the first to second most frequent variables.

We found two variables (Hispanic and physical hurt) the team decided were not critical, and we had no way of increasing the variance.  Therefore we removed both variables.

Next, we identified that drug use, nights/times in the hospital or ER, and times arrested and nights in jail all had low variance.  Given the importance of these variables, we summed each of them, respectively.  Although some variables had mixed types (i.e., nights and times), our goal is to predict housing not necessarily to have meaningful variables.  Also, after summation, jail/arrests and drug use were still near-zero variance.  However, because of their importance in predicting housing, we kept all the summed variables.
```{r}
library(caret)

nzv = nearZeroVar(machine_dat, saveMetrics = TRUE)
nzv

# HispanicLatino.x, PhysicallyHurt.x
machine_dat$HispanicLatino.x = NULL
machine_dat$PhysicallyHurt.x = NULL


machine_dat$drug_use = machine_dat$Cocaine_Use.x + machine_dat$Meth_Use.x + machine_dat$StreetOpioids_Use.x + machine_dat$RxOpioids_Use.x
machine_dat$Cocaine_Use.x = NULL
machine_dat$Meth_Use.x = NULL
machine_dat$StreetOpioids_Use.x = NULL
machine_dat$RxOpioids_Use.x = NULL


machine_dat$er_hos_use_base = machine_dat$NightsDetox.x + machine_dat$NightsHospitalMHC.x + machine_dat$TimesER.x
### Drop other variables
machine_dat[,c("NightsDetox.x", "NightsHospitalMHC.x", "TimesER.x")] = list(NULL)


machine_dat$jail_arrest_base = machine_dat$NumTimesArrested.x + machine_dat$NightsJail.x
machine_dat$NightsJail.x = NULL
machine_dat$NumTimesArrested.x = NULL
```
Descriptives
```{r}
apply(machine_dat, 2, function(x){describe.factor(x)})
```


Our next step is to identify if there are any high (i.e., .90 or greater) Spearman correlations between variables.  We used a Spearman correlation, as most variables are generally not normally distributed.  We found no correlation over .9.  

Additionally, we evaluated the variance inflation factors (VIFs) for a logistic regression model with housing as the outcome variable.  This logistic regression model will be similar to the final model used; therefore, the VIFs can provide some insight into potential multicollinearity.  Overall we only found two VIFs above five quarter at 6.12 and telehealth at 6.4, providing evidence that multicollinearity is generally not a concern. 

```{r}
descCor = cor(machine_dat, use = "pairwise.complete.obs", method = "spearman")
hig_corr = findCorrelation(descCor)
hig_corr
library(car)
vif_model = glm(Housing.y ~ ., data = machine_dat)
vif_list =  vif(vif_model)
vif_list = data.frame(vif_list) 
vif_list = subset(vif_list, vif_list > 5)
vif_list
```



Next, we are evaluating the missing data using the Amelia package with five imputations.  The Amelia package has advantages over other packages (e.g., MICE), because we can set the type of variable (i.e., nominal, log).  In my personal experience, I have found the noms function works for binary variables better than binary, and a nominal regression with binary data reduces to a logistic regression.  More information on data imputation is available in the Amelia package documentation: https://gking.harvard.edu/amelia
```{r}
library(Amelia)
library(prettyR)

#a.out_noms = amelia(x = machine_dat, m = 5, noms = c("Gender.x", "RaceWhite.x", "RaceBlack.x", "Employment.x", "Housing.x", "telehealth.x", "Housing.y", "anxiety", "mdd_r", "mdd_s", "another_sex_ident"), ords = c("Quarter.x", "Agegroup.x", "OverallHealth.x", "CapableManagingHealthCareNeeds.x", "HandlingDailyLife.x", "ControlLife.x", "DealWithCrisis.x", "GetsAlongWithFamily.x", "SocialSituations.x", "FunctioningHousing.x", "Symptoms.x", "Nervous.x", "Hopeless.x", "Restless.x", "Depressed.x", "EverythingEffort.x", "Worthless.x", "PsychologicalEmotionalProblems.x", "LifeQuality.x", "EnoughEnergyForEverydayLife.x", "PerformDailyActivitiesSatisfaction.x", "HealthSatisfaction.x", "RelationshipSatisfaction.x", "SelfSatisfaction.x", "Tobacco_Use.x", "Alcohol_Use.x", "Cannabis_Use.x", "ViolenceTrauma.x", "Education.x", "EnoughMoneyForNeeds.x", "Friendships.x", "EnjoyPeople.x", "BelongInCommunity.x", "SupportFromFamily.x", "SupportiveFamilyFriends.x", "GenerallyAccomplishGoal.x"), logs = c("drug_use" ,"er_hos_use_base", "jail_arrest_base"))

#saveRDS(a.out_noms, file = "a.out_noms_8_20_20.rds")
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
a.out_noms = readRDS(file = "a.out_noms_8_20_20.rds")
impute_dat_noms = a.out_noms$imputations
```
We also compared the densities of the observed data versus the model imputed.  Overall the distributions match with some imputations having flatter distributions where some variables spike.  However, drug use imputed values are about one unit higher than observed values. 
```{r}
compare.density(a.out_noms, var = "GetsAlongWithFamily.x")
compare.density(a.out_noms, var = "RelationshipSatisfaction.x")
compare.density(a.out_noms, var = "drug_use")
compare.density(a.out_noms, var = "Housing.y")
```
Now, because we have five data sets, we need to conduct all the remaining analyses five times. 
First, we need to make sure R is treating each factor variable as the correct variable type.  Therefore, we used the apply function on the factor (i.e., binary) variables to ensure they are treated as factors.
```{r}
impute_dat_noms_out_bin = list()
impute_dat_noms_out = list()
for(i in 1:length(impute_dat_noms)){
  impute_dat_noms_out_bin[[i]]= apply(impute_dat_noms[[i]][,c(2:4, 32, 34, 42:47)], 2, function(x){as.factor(x)})
impute_dat_noms_out[[i]] = data.frame(impute_dat_noms[[i]][,-c(2:4, 32, 34, 42:47)], impute_dat_noms_out_bin[[i]]) 
}
head(impute_dat_noms_out[[1]])

```

Next, we need to create the training and testing data sets.  The createDataPartition function allows us to randomly select a set percentage of the data to go into the training or testing data sets.  We selected 75% of the data for the training and the remaining 25% in the testing data set.  
```{r}
library(caret)
train_out = list()
test_out = list()
train_test_index = list()

for(i in 1:length(impute_dat_noms_out)){
  train_test_index[[i]] =  createDataPartition(impute_dat_noms_out[[i]][[46]], p = .75,list = FALSE, times = 1)
  train_out[[i]] = impute_dat_noms_out[[i]][train_test_index[[i]],]
  test_out[[i]] = impute_dat_noms_out[[i]][-train_test_index[[i]],]
}

```
Then we set the train control settings.  In this setting we conducted a repeated cross-validation where we create 10 cross validation data sets and repeat this process 10 times.  This helps prevent over fitting the data.
```{r}
set.seed(123)
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

```
Here I discuss a few models for machine learning.  These explanations are generally from the following sources: 
https://towardsdatascience.com/an-intro-to-ensemble-learning-in-machine-learning-5ed8792af72d
https://towardsdatascience.com/ensemble-methods-bagging-boosting-and-stacking-c9214a10a205
https://machinelearningmastery.com/tune-learning-rate-for-gradient-boosting-with-xgboost-in-python/
http://uc-r.github.io/gbm_regression


adaboost: This is a boosting model, which means it identifies weak (i.e., smaller trees) base (i.e., decision trees, linear regression) models interatively and makes adjustments based on the results (i.e., some loss function).  It identifies which observations it is failing to predict accurately and attempts to adjust the model and updates the weights for each observations (weights must sum to one).  Then when the decision trees (i.e., cuts in the variables and interactions between them) are being updated the weights for the observations that are most inaccurate play a larger role (i.e., it is like increasing the population).  We then continue this process until the parameters have been met (i.e., tree depth of 150 across three-way interactions). 

gbm: Another boosting technique, but instead of updating weights and training on those, it uses pseudo-residuals to identify the most problematic observations and updates the model.  Specifically, gbm uses gradient descent.  It starts off with a random set for the trees, calculates the residuals and subtracts those from the model and uses the differences to identify the observations with the highest errors.  It can make changes, within the set parameters (e.g., learning rate, interaction-depth), to the trees to improve the model in the direction (e.g., negative, positive) of the gradient (i.e., the difference in the improvement in the loss function between the models).


For bagging models like random forests, they use bootstrapping.  Bootstraping in this context means taking a random sample of participants from the population (smaller than the population) with replacement many times and creating almost identical sample distributions.  Then we run the model on each of these bootstrapped samples and take the average across the cross validation samples.  Bagging maximizes reductions in variance whereas boosting maximizes reductions in bias.


Some explanation for the output from the models:

interaction.depth: The level of interactions between the variables (i.e., two-way interactions, three-way interactions).
n.trees = The number of interactions and or splits in the variables (e.g., splitting an ordinal variable).
shrinkage = The learning rate at which the model can change (default set to .1).
n.minobsinnode = The minimum number of observations in any terminal (final) node from the tree.
kappa: It is the agreement between models accounting for random chance.

expand.grid: Although, we chose against this (we ran it and found similar results), if you wanted to expand the grid search of parameters, you could use the expand.grid function.
```{r}
set.seed(825)

gbmGrid <-  expand.grid(interaction.depth = c(3:5), 
                        n.trees = (20:30)*10, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

gbmFit_house_out = list()
for(i in 1:length(train_out)){
gbmFit_house_out[[i]] = train(Housing.y ~ ., data = train_out[[i]], 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE, 
                 tuneGrid = gbmGrid)
}
gbmFit_house_out[[1]]
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks")
saveRDS(gbmFit_house_out, file = "gbmFit_house_out.rds")
gbmFit_house_out = readRDS(file = "gbmFit_house_out.rds")

```
We can review some of the results by looking at the most influence variables and plotting accuracy across level of interactions and depth of tree.  To do this we took the average influence across the top ten influencenial variable across all five data sets.
```{r include=FALSE}
### Get the average influence from each variable
dat_rel_inf = list()
var_list = list()
rel_inf = list()
for(i in 1:length(gbmFit_house_out)){
  dat_rel_inf[[i]] =  summary(gbmFit_house_out[[i]])
  dat_rel_inf[[i]] = dat_rel_inf[[i]][order(dat_rel_inf[[i]]$var),]
  rel_inf[[i]] = dat_rel_inf[[i]]$rel.inf
}

```
Plotting the results
```{r}
rel_inf = data.frame(rel_inf)
rel_inf = apply(rel_inf, 1, mean)
rel_inf = round(rel_inf, 2)
dat_rel_inf = data.frame(var = dat_rel_inf[[1]]$var, rel_inf)
### Order by name so you can average them

#### Get the top ten and order 
dat_rel_inf$var = factor(dat_rel_inf$var, levels  = dat_rel_inf$var[order(dat_rel_inf$rel_inf)])
dat_rel_inf = dat_rel_inf[order(dat_rel_inf$rel_inf, decreasing = TRUE),]
dat_rel_inf = dat_rel_inf[1:10,]
plot_rel_inf = ggplot(dat_rel_inf, aes(x =  rel_inf,y =var,))+
  geom_bar(stat = "identity")+
  labs(title="Plot of variable relative influence", y = "Variable", x = "Relative influence")
plot_rel_inf
```


Then we took the average of the accuracy and kappa's across all five data sets for each set of parameters.
```{r}
ac_out = list()
kappa_out = list()
for(i in 1:length(gbmFit_house_out)){
  ac_out[[i]] = gbmFit_house_out[[i]]$results[5]
  kappa_out[[i]] = gbmFit_house_out[[i]]$results[6]
}
ac_out = data.frame(ac_out)
ac_out = apply(ac_out, 1, mean)
kappa_out = data.frame(kappa_out)
kappa_out = apply(kappa_out, 1, mean)
ac_kappa_out = data.frame(ac_out, kappa_out)
ac_kappa_out = round(ac_kappa_out, 2)
ac_kappa_out = data.frame(gbmFit_house_out[[1]]$results[1:4], ac_kappa_out)
ac_kappa_out
```
Next we evaluated the model on new (i.e., the test) data set and review common measures such as accuracy, sensitivity, and specificity.  Overall, the model is not as sensitive as we would like.
```{r}
plsProbs_list = list()
plsProbs_list_one = list()
plsClasses = list()
con_matrix = list()
class_list = list()
accuracy_list = list()

for(i in 1:length(gbmFit_house_out)){
  plsProbs_list[[i]] = predict(gbmFit_house_out[[i]], newdata = test_out[[i]], type = "prob")
  plsProbs_list_one[[i]] = plsProbs_list[[i]][[2]]
  plsClasses[[i]] = predict(gbmFit_house_out[[i]], newdata = test_out[[i]])
  con_matrix[[i]] = confusionMatrix(data = plsClasses[[i]], reference = as.factor(test_out[[i]][[46]]))
  class_list[[i]] = con_matrix[[i]]$byClass
  accuracy_list[[i]] = con_matrix[[i]]$overall[1]
}
value = data.frame(class_list[[1]])
measure = row.names(value)
value = data.frame(measure, value = value$class_list..1..) 

accuracy_list = data.frame(accuracy_list)
accuracy_list = round(apply(accuracy_list, 1, mean),2)
accuracy_list = data.frame(t(data.frame(t(accuracy_list))))
accuracy_list$measure =  row.names(accuracy_list)
accuracy_list = data.frame(measure =  accuracy_list$measure, value = accuracy_list$t.data.frame.t.accuracy_list...)

value = rbind(accuracy_list, value)
value$value = round(value$value,2)
value
```
Although the model is not as accurate as we would like, in theory, we could evaluate the probability of being housed and create thresholds. For example, anyone below 75% is at mild risk, below 50% is at moderate risk, and below 25% is at high risk.  Everyone above 75% would be considered minimal to no risk. 

Because I am just showing this part for demonstration, I will only use one data set.
```{r}
library(dplyr)
plsProbs_list_one = plsProbs_list_one[[1]]
plsProbs_list_one = data.frame(prob_housed = plsProbs_list_one)

plsProbs_list_one$risk_level = case_when(
  plsProbs_list_one$prob_housed < .25 ~ "high",
  plsProbs_list_one$prob_housed < .5 ~ "moderate",
  plsProbs_list_one$prob_housed < .75 ~ "mild",
  TRUE ~ "minimal to none")
describe.factor(plsProbs_list_one$risk_level)
```



