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
test_resolved =  subset(test_merge, test_merge$RESOLVED ==1)
test_resolved
test_resolved[c("SourceClient_ID", "PHQ9_Date", "RESOLVED")]
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

inTrain = createDataPartition(y = PHQ9_demos_diag_complete$RESOLVED, p = .75, list = FALSE)
training = PHQ9_demos_diag_complete[inTrain,]
testing = PHQ9_demos_diag_complete[-inTrain,] 
describe.factor(training$RESOLVED)
```
Next steps of tunning
```{r}
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)

set.seed(12345)
gbmFit1 <- train(RESOLVED ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
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



Other data sets work on later
```{r}
MEE_ClientDemo = read.csv("MEE_ClientDemo.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
head(MEE_ClientDemo)
MEE_ClientDemo = MEE_ClientDemo[c("Gender", "Race", "ORG_ABBREV", "SourceClient_ID")]
#apply(MEE_ClientDemo, 2, function(x){describe.factor(x)})
MEE_ClientDemo$gender_minority = ifelse(MEE_ClientDemo$Gender != "MALE", 1, 0)
describe.factor(MEE_ClientDemo$Race)
head(MEE_ClientDemo, 10)
MEE_ClientDemo$racial_minority = ifelse(MEE_ClientDemo$Race != "WHITE/CAUCASIAN",1 , 0)
head(MEE_ClientDemo, 10)
MEE_ClientDemo$Client_ID = MEE_ClientDemo$PATID
  
MEE_DIAG_dat = read.csv("MEE_DIAG.csv", header = TRUE, na.strings = c("NO ENTRY", "NULL", "No entry"))
MEE_DIAG = MEE_DIAG_dat[c("SourceClient_ID", "ICD.Code.Description", "Diagnosis.Ranking")]
#Only including primary diagnosis
MEE_DIAG = subset(MEE_DIAG, Diagnosis.Ranking == "PRIMARY")
dim(MEE_DIAG)
MEE_DIAG$MDD = ifelse(MEE_DIAG$ICD.Code.Description == "MAJOR DEPRESSIVE DISORDER", 1, 0)
describe.factor(MEE_DIAG$MDD)

#### Remove all duplicates which I am assuming will be the
library(dplyr)

MEE_DIAG = MEE_DIAG[order(MEE_DIAG$SourceClient_ID),]
MEE_DIAG = distinct(MEE_DIAG, SourceClient_ID, .keep_all = TRUE)
sum(duplicated(MEE_DIAG$SourceClient_ID))

### Try merging on SourceClient_ID with MEE_PHQ9 and MEE_DIAG
phq9_diag = merge(MEE_PHQ9, MEE_DIAG, by = "SourceClient_ID", all.x= TRUE)
dim(phq9_diag)
dim(MEE_PHQ9)

## Now merge on Client_ID with 
phq9_diag_demo = merge(phq9_diag, MEE_ClientDemo, by = "SourceClient_ID", all.x = TRUE)
dim(phq9_diag_demo)

phq9_diag_demo = phq9_diag_demo[c("SourceClient_ID", "PHQ9_Date", "PHQ9_Total", "MDD", "ORG_ABBREV", "gender_minority", "racial_minority")]
apply(phq9_diag_demo[3:7], 2, function(x){describe.factor(x)})
### Create two dummy variables for IL and FL
phq9_diag_demo$IL = ifelse(phq9_diag_demo$ORG_ABBREV == "CIL", 1, 0)
phq9_diag_demo$FL = ifelse(phq9_diag_demo$ORG_ABBREV == "CFL", 1, 0)
phq9_diag_demo

#####
MEE_DIAG_dat_merge = MEE_DIAG_dat_merge[order(MEE_DIAG_dat_merge$SourceClient_ID),]
describe.factor(MEE_DIAG_dat_merge$MAJOR.DEPRESSIVE.DISORDER)
MEE_DIAG_dat_merge
MEE_DIAG_dat_merge = MEE_DIAG_dat_merge %>% dplyr::group_by(SourceClient_ID) %>% dplyr::mutate(time = row_number()-1)
#### Let's try subsetting them, then merging them back on ID you have duplicate IDs so that could be a problem
describe.factor(MEE_DIAG_dat_merge$Diagnosis.Ranking)
PRIMARY = subset(MEE_DIAG_dat_merge, Diagnosis.Ranking == "PRIMARY")
SECONDARY = subset(MEE_DIAG_dat_merge, Diagnosis.Ranking == "SECONDARY")
UNKNOWN = subset(MEE_DIAG_dat_merge, Diagnosis.Ranking == "UNKNOWN")
TERTIARY = subset(MEE_DIAG_dat_merge, Diagnosis.Ranking == "TERTIARY")

prim_secon = merge(PRIMARY, SECONDARY, by = "SourceClient_ID", all = TRUE)
prim_secon
```

