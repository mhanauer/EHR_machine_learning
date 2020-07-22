---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Variables I need

MEE_PHQ9
Client_ID
SourceClient_ID
PHQ9_Date
PHQ9_Total

MEE_ClientDemo
Gender
Race
ORG_ABBREV
PATID

MEE_DIAG
SourceClient_ID
ICD Code Description
```{r}
library(prettyR)
library(lubridate)
library(dplyr)
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/MEE_Data")
MEE_PHQ9 = read.csv("MEE_PHQ9.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry")) 

MEE_PHQ9 = MEE_PHQ9[c("Client_ID", "SourceClient_ID", "PHQ9_Date", "PHQ9_Total")]
summary(MEE_PHQ9)
MEE_PHQ9$PHQ9_Date = ymd(MEE_PHQ9$PHQ9_Date)
dim(MEE_PHQ9)
###########
# Now client demos
setwd("T:/CRI_Research/telehealth_evaluation/data_codebooks/MEE_Data")
MEE_ClientDemo_dat = read.csv("MEE_ClientDemo.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_ClientDemo = MEE_ClientDemo_dat[c("Gender", "Race", "ORG_ABBREV", "SourceClient_ID")]
MEE_ClientDemo$gender_minority = ifelse(MEE_ClientDemo$Gender != "MALE", 1, 0)
MEE_ClientDemo$IL = ifelse(MEE_ClientDemo$ORG_ABBREV == "CIL", 1, 0)
MEE_ClientDemo$FL = ifelse(MEE_ClientDemo$ORG_ABBREV == "CFL", 1, 0)
describe.factor(MEE_ClientDemo$Race)
#WHITE/CAUCASIAN, UNKNOWN, BLACK/AFRICAN AMERICAN, HISPANIC WHITE, OTHER
MEE_ClientDemo$WHITE = ifelse(MEE_ClientDemo$Race == "WHITE/CAUCASIAN", 1, 0)
MEE_ClientDemo$UNKNOWN = ifelse(MEE_ClientDemo$Race == "UNKNOWN", 1, 0)
MEE_ClientDemo$BLACK = ifelse(MEE_ClientDemo$Race == "BLACK/AFRICAN AMERICAN", 1, 0)
MEE_ClientDemo$HISPANIC_WHITE = ifelse(MEE_ClientDemo$Race == "HISPANIC WHITE", 1, 0)
MEE_ClientDemo$Another = ifelse(MEE_ClientDemo$Race == "OTHER", 1, 0)

MEE_ClientDemo_merge = MEE_ClientDemo[c("SourceClient_ID", "gender_minority", "IL", "FL", "WHITE", "UNKNOWN", "BLACK", "HISPANIC_WHITE", "Another")]


library(caret)
### Merge PHQ-9 with demos 
PHQ9_demos = merge(MEE_PHQ9, MEE_ClientDemo_merge, by = "SourceClient_ID", all.x = TRUE)
dim(PHQ9_demos) 
dim(MEE_PHQ9)
#### Now add PHQ-9 adminstration variable
PHQ9_demos = PHQ9_demos %>% dplyr::group_by(SourceClient_ID) %>% dplyr::mutate(time = row_number()-1)





```
You just want baseline diagnosis
```{r}
#MEE_DIAG_dat = read.csv("MEE_DIAG.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
head(MEE_DIAG_dat)

PHQ9_demos

### 1,2,3,4,5 or greater
MEE_DIAG_dat$Diagnosis.Episode.Number_1 = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number == 1, 1, 0)
MEE_DIAG_dat$Diagnosis.Episode.Number_2 = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number == 2, 1, 0)
MEE_DIAG_dat$Diagnosis.Episode.Number_3 = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number == 3, 1, 0)
MEE_DIAG_dat$Diagnosis.Episode.Number_4 = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number == 4, 1, 0)
MEE_DIAG_dat$Diagnosis.Episode.Number_5 = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number == 5, 1, 0)
MEE_DIAG_dat$Diagnosis.Episode.Number_6_greater = ifelse(MEE_DIAG_dat$Diagnosis.Episode.Number > 5, 1, 0)
MEE_DIAG_dat_merge = MEE_DIAG_dat[c("SourceClient_ID", "Diagnosis.Episode.Number_1", "Diagnosis.Episode.Number_2", "Diagnosis.Episode.Number_3", "Diagnosis.Episode.Number_4", "Diagnosis.Episode.Number_5", "Diagnosis.Episode.Number_6_greater")]

MEE_DIAG_dat_merge$RESOLVED = ifelse(MEE_DIAG_dat$Diagnosis.Status == "RESOLVED",1, 0)

MEE_DIAG_dat_merge$Diagnosis.Ranking = MEE_DIAG_dat$Diagnosis.Ranking

#### Code top ten diags
#t(data.frame(describe.factor(MEE_DIAG_dat$ICD.Code.Description)))
# MAJOR.DEPRESSIVE.DISORDER, ATTENTION.DEFICIT.HYPERACTIVITY.DISORDER, POST.TRAUMATIC.STRESS.DISORDER, BIPOLAR.DISORDER, GENERALIZED.ANXIETY.DISORDER, OPIOID.DEPENDENCE, ALCOHOL.DEPENDENCE, SCHIZOAFFECTIVE.DISORDER, OTHER.STIMULANT.DEPENDENCE, DISRUPTIVE.MOOD.DYSREGULATION.DISORDER
MEE_DIAG_dat_merge$MAJOR.DEPRESSIVE.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "MAJOR DEPRESSIVE DISORDER",1, 0)
MEE_DIAG_dat_merge$ATTENTION.DEFICIT.HYPERACTIVITY.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "ATTENTION DEFICIT HYPERACTIVITY DISORDER",1, 0)

MEE_DIAG_dat_merge$POST.TRAUMATIC.STRESS.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "POST TRAUMATIC STRESS DISORDER",1, 0)

MEE_DIAG_dat_merge$BIPOLAR.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "BIPOLAR DISORDER",1, 0)

MEE_DIAG_dat_merge$GENERALIZED.ANXIETY.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "GENERALIZED ANXIETY DISORDER",1, 0)

MEE_DIAG_dat_merge$OPIOID.DEPENDENCE = ifelse(MEE_DIAG_dat$ICD.Code.Description == "OPIOID DEPENDENCE",1, 0)

MEE_DIAG_dat_merge$ALCOHOL.DEPENDENCE = ifelse(MEE_DIAG_dat$ICD.Code.Description == "ALCOHOL DEPENDENCE",1, 0)

MEE_DIAG_dat_merge$SCHIZOAFFECTIVE.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "SCHIZOAFFECTIVE DISORDER",1, 0)

MEE_DIAG_dat_merge$OTHER.STIMULANT.DEPENDENCE = ifelse(MEE_DIAG_dat$ICD.Code.Description == "OTHER.STIMULANT DEPENDENCE",1, 0)

MEE_DIAG_dat_merge$DISRUPTIVE.MOOD.DYSREGULATION.DISORDER = ifelse(MEE_DIAG_dat$ICD.Code.Description == "DISRUPTIVE MOOD DYSREGULATION DISORDER",1, 0)

sum(!is.na(MEE_DIAG_dat$Diagnosis.End.Date))

PHQ9_demos_diag = merge(PHQ9_demos, MEE_DIAG_dat_merge, by = "SourceClient_ID", all.x = TRUE)
PHQ9_demos_diag
describe.factor(PHQ9_demos_diag$RESOLVED)
```
Try to get positive or negative change in PHQ-9 
```{r}

phq9_change_dat = PHQ9_demos_diag
phq9_change_dat
#### Get one PHQ-9 per date
phq9_change_dat = distinct(phq9_change_dat, SourceClient_ID, PHQ9_Date, .keep_all = TRUE)
phq9_change_dat_base = subset(phq9_change_dat, time ==0)
phq9_change_dat_one = subset(phq9_change_dat, time ==1)

phq9_change_dat_base_one_matched = merge(phq9_change_dat_base, phq9_change_dat_one, by = "SourceClient_ID")
dim(phq9_change_dat_base_one_matched)
phq9_change_dat_base_one_matched$phq9_decrease =  phq9_change_dat_base_one_matched$PHQ9_Total.y-phq9_change_dat_base_one_matched$PHQ9_Total.x
phq9_change_dat_base_one_matched$phq9_decrease = ifelse(phq9_change_dat_base_one_matched$phq9_decrease <0,1,0)
describe.factor(phq9_decrease)
dim(phq9_change_dat_base_one_matched)
phq9_change_dat_base_one_matched = phq9_change_dat_base_one_matched[c(1:31,62)]
phq9_change_dat_base_one_matched$SourceClient_ID = NULL
phq9_change_dat_base_one_matched$PHQ9_Date.x = NULL
phq9_change_dat_base_one_matched$Client_ID.x = NULL
phq9_change_dat_base_one_matched
nzv =  nearZeroVar(phq9_change_dat_base_one_matched)
## Filter out near zero variance
phq9_change_dat_base_one_matched = phq9_change_dat_base_one_matched[,-nzv]
##### Create dummies for ranking variable
describe.factor(phq9_change_dat_base_one_matched$Diagnosis.Ranking.x)
phq9_change_dat_base_one_matched$PRIMARY =  ifelse(phq9_change_dat_base_one_matched$Diagnosis.Ranking.x=="PRIMARY", 1, 0) 
phq9_change_dat_base_one_matched$SECONDARY =  ifelse(phq9_change_dat_base_one_matched$Diagnosis.Ranking.x=="SECONDARY", 1, 0) 
phq9_change_dat_base_one_matched$UNKNOWN =  ifelse(phq9_change_dat_base_one_matched$Diagnosis.Ranking.x=="UNKNOWN", 1, 0)
phq9_change_dat_base_one_matched$TERTIARY =  ifelse(phq9_change_dat_base_one_matched$Diagnosis.Ranking.x=="TERTIARY", 1, 0) 
#### Get complete data
phq9_change_dat_base_one_matched_complete = na.omit(phq9_change_dat_base_one_matched)
dim(phq9_change_dat_base_one_matched_complete)
#### Now filter out high correlations
phq9_change_dat_base_one_matched_complete$Diagnosis.Ranking.x = NULL
descrCor <- cor(phq9_change_dat_base_one_matched_complete)
### No high correlations

findLinearCombos(phq9_change_dat_base_one_matched_complete)
### No linear combos

### Change to factor for outcome
phq9_change_dat_base_one_matched_complete$phq9_decrease = as.factor(phq9_change_dat_base_one_matched_complete$phq9_decrease)


```
Try what predicts PHQ9 increases
```{r}
colnames(phq9_change_dat_base_one_matched_complete) = c("PHQ9_Total", "gender_minority", "IL", "FL", "White", "Unknown", "Black", "Another", "D1", "D2", "D3", "MDD", "BD", "GAD", "OD", "phq9_decrease", "primary", "secondary", "unkown", "tertiary")


write.csv(phq9_change_dat_base_one_matched_complete, "phq9_change_dat_base_one_matched_complete.csv", row.names =  FALSE)
phq9_change_dat_base_one_matched_complete = read.csv("phq9_change_dat_base_one_matched_complete.csv", header = TRUE)


names <- c(2:dim(phq9_change_dat_base_one_matched_complete)[2])
phq9_change_dat_base_one_matched_complete[,names] <- lapply(phq9_change_dat_base_one_matched_complete[,names] , factor)
str(phq9_change_dat_base_one_matched_complete)


inTrain = createDataPartition(y = phq9_change_dat_base_one_matched_complete$phq9_decrease, p = .75, list = FALSE)
training = phq9_change_dat_base_one_matched_complete[inTrain,]
testing = phq9_change_dat_base_one_matched_complete[-inTrain,] 
describe.factor(testing$phq9_decrease)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

set.seed(12345)
gbmFit1 <- train(phq9_decrease ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = TRUE)

summary(gbmFit1)
```



Get baseline factors so first administration no matter what even if they have multiple episodes of care
Later figure out how to predict based on multiple episodes of care
```{r}
### Need to get a baseline data set
PHQ9_demos_diag[c("SourceClient_ID", "Diagnosis.Ranking")]
### Created a resolved data frame
resolved = PHQ9_demos_diag[c("SourceClient_ID", "RESOLVED")]
resolved = subset(resolved, RESOLVED == 1)

#### Get the baseline diagnoses for primary, secondary, etc.
PHQ9_demos_diag = distinct(PHQ9_demos_diag, SourceClient_ID, Diagnosis.Ranking, .keep_all = TRUE)
PHQ9_demos_diag$RESOLVED = NULL
### Merge where you keep PHQ9_demos_diag
PHQ9_demos_diag = merge(PHQ9_demos_diag, resolved, by = "SourceClient_ID", all.x = TRUE)

### Maybe unique and resolved
PHQ9_demos_diag = distinct(PHQ9_demos_diag, SourceClient_ID, RESOLVED, .keep_all = TRUE)
describe.factor(PHQ9_demos_diag$RESOLVED)
PHQ9_demos_diag[c("SourceClient_ID", "PHQ9_Date", "RESOLVED")]
PHQ9_demos_diag$Client_ID = NULL
sum(duplicated(PHQ9_demos_diag$SourceClient_ID))

### Change NA in RESOLVED to 0
PHQ9_demos_diag$RESOLVED[is.na(PHQ9_demos_diag$RESOLVED)] <- 0
```



What factors predict a session being resolved?
```{r}
library(naniar)
miss_var_summary(PHQ9_demos_diag)
dim(PHQ9_demos_diag)
PHQ9_demos_diag_complete = na.omit(PHQ9_demos_diag)
dim(PHQ9_demos_diag_complete)
describe.factor(PHQ9_demos_diag_complete$RESOLVED)

PHQ9_demos_diag_complete$RESOLVED = as.factor(PHQ9_demos_diag_complete$RESOLVED)
PHQ9_demos_diag_complete
apply(PHQ9_demos_diag_complete[c(4:30)], 2, function(x){describe.factor(x)})

### Remove ATTENTION.DEFICIT.HYPERACTIVITY.DISORDER, POST.TRAUMATIC.STRESS.DISORDER, OTHER.STIMULANT.DEPENDENCE
PHQ9_demos_diag_complete$ATTENTION.DEFICIT.HYPERACTIVITY.DISORDER = NULL
PHQ9_demos_diag_complete$POST.TRAUMATIC.STRESS.DISORDER = NULL
PHQ9_demos_diag_complete$OTHER.STIMULANT.DEPENDENCE = NULL
apply(PHQ9_demos_diag_complete[c(4:27)], 2, function(x){describe.factor(x)})
PHQ9_demos_diag_complete$SourceClient_ID = NULL
PHQ9_demos_diag_complete$PHQ9_Date = NULL
PHQ9_demos_diag_complete$PHQ9_Date = NULL
```
Clean up variables
Remove highly correlated variables
```{r}
PHQ9_demos_diag_complete$RESOLVED = as.numeric(PHQ9_demos_diag_complete$RESOLVED)
descrCor <-  cor(PHQ9_demos_diag_complete[-c(17)])
descrCor

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
### No variables
findLinearCombos(PHQ9_demos_diag_complete[-c(17)])
## No linear combos to remove
```

Next steps of tuning
```{r}
library(doParallel)


inTrain = createDataPartition(y = PHQ9_demos_diag_complete$RESOLVED, p = .75, list = FALSE)
training = PHQ9_demos_diag_complete[inTrain,]
testing = PHQ9_demos_diag_complete[-inTrain,] 
describe.factor(training$RESOLVED)

fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10,
  classProbs = TRUE)

set.seed(12345)
gbmFit1 <- train(RESOLVED ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = TRUE, metric = "ROC")

```
Look at results
```{r}
summary(gbmFit1)
plsProbs <- predict(gbmFit1, newdata = testing, type = "prob")
plsClasses <- predict(gbmFit1, newdata = testing)

confusionMatrix(data = plsClasses, reference = testing$RESOLVED)
plsProbs
```
Just run a regression
PHQ9_Total, , Diagnosis.RankingSECONDARY, IL, WHITE, Diagnosis.RankingUNKNOWN, GENERALIZED.ANXIETY.DISORDER
```{r}
typeof(PHQ9_demos_diag_complete$RESOLVED)
PHQ9_demos_diag_complete$RESOLVED = as.factor(PHQ9_demos_diag_complete$RESOLVED)

test_glm = glm(RESOLVED ~PHQ9_Total + Diagnosis.Episode.Number_5 + IL + WHITE + GENERALIZED.ANXIETY.DISORDER,  data = PHQ9_demos_diag_complete, family = "binomial")

summary(test_glm)

```



Show that all if you drop baseline you don't drop any resolved
```{r}
### How may resolves are left 
library(dplyr)
test = MEE_DIAG_dat %>% dplyr::group_by(SourceClient_ID) %>% dplyr::mutate(time = row_number()-1)
resolved_by_time = test %>%
  dplyr::group_by(Diagnosis.Status, time)%>%
  tally()
resolved_by_time
n_resolved = subset(resolved_by_time, Diagnosis.Status == "RESOLVED")
sum(n_resolved$n)
### All resolved left
#### Do the top ten disorders

```


