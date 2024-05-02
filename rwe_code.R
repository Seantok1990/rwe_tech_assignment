#This code serves to clean, organize, process and analyse the dataset provided
#by Holmusk. The code analyses the dataset to understand how the CGIS score (dependent variable)
#is influenced by the clinical features (independent variables). Several features
#are calculated as part of this study and will be elaborated in the accompanying
#slide deck

#Created by: Sean Tok
#Date created:2/5/2024
#Created using: RStudio 2023.12.1

library(tidyverse)
library(ggplot2)
library(naniar)
library(lubridate)
library(corrplot)

#Get working directory of the script
dir_name=dirname(rstudioapi::getSourceEditorContext()$path)
dir_name=gsub('//','////',dir_name) #using a windows system for Rstudio

#Change to raw data directory
setwd(paste(dir_name,'//raw_data',sep=''))

#Load data
clinical_data=read.csv('clinical_data.csv') %>% as.data.frame()
demographic_data=read.csv('demographics.csv')%>% as.data.frame()
billing_amt_data=read.csv('bill_amount.csv')%>% as.data.frame()
billing_id_data=read.csv('bill_id.csv')%>% as.data.frame()

#Inspect colnames for joining data
names(clinical_data) #inconsistent patient id name, id links to patient_id in demographic data
names(demographic_data)
names(billing_id_data) #bill_id links to bill_id in billing_amt_data 
names(billing_amt_data) #patient_id links to patient_id data

#join clinical and demographic data using patient_id after renaming column
clinical_data=clinical_data %>% rename('patient_id'='id')
patient_data=full_join(as.data.frame(clinical_data),as.data.frame(demographic_data),by='patient_id')

#perform join on billing data
#Seems there are multiple bills associated with a single patient. Perhaps best to 
#calculate summarized billing data
billing_data=full_join(billing_amt_data,billing_id_data,'bill_id')
billing_data=billing_data %>%  group_by(patient_id) %>% summarize(bill_mean=mean(amount),bill_min=min(amount),bill_max=max(amount),bill_count=n())

#perform full join on patient data and billing data
full_dataset=full_join(patient_data,billing_data,'patient_id') %>% as.data.frame()

#inspect rows and columns for inconsistent data
lapply(full_dataset,function(x) {head(unique(x))})
lapply(full_dataset,typeof)
#columns medical_history_hbp, gender, race,resident_status have discrepancies

#Resolve gender
full_dataset$gender[full_dataset$gender=='m']='Male'
full_dataset$gender[full_dataset$gender=='f']='Female'
#Resolve race
full_dataset$race[full_dataset$race=='chinese']='Chinese'
full_dataset$race[full_dataset$race=='India']='Indian'
#Resolve resident_status
full_dataset$resident_status[full_dataset$resident_status=='Singapore citizen']='Singaporean'
#Resolve medical_history_hbp
full_dataset$medical_history_hbp[full_dataset$medical_history_hbp=='No']=0
full_dataset$medical_history_hbp[full_dataset$medical_history_hbp=='Yes']=1
full_dataset$medical_history_hbp=as.integer(full_dataset$medical_history_hbp)

#columns medical_history_sud and medical_history_tum have missing values.
#check number of missing values
table(is.na(full_dataset$medical_history_tum))
table(is.na(full_dataset$medical_history_sud))

#test if values are missing at random
mcar_test(full_dataset)
#appears to be missing completely at random
#Propose to remove the rows with missing values since value does not seem to high
trimmed_dataset=na.omit(full_dataset)

#rename trimmed dataset as dataset for easy swapping if need be
dataset=trimmed_dataset

#Perform feature generation on current features
#Generate age, duration of stay as well as change in CGIS score at discharge, number of co-occuring symptoms and treatments
dataset= dataset %>% mutate(age=floor(as.integer(dmy(.$date_of_admission)-ymd(.$date_of_birth))/365))
dataset= dataset %>% mutate(stay_duration=as.integer(dmy(.$date_of_discharge)-dmy(.$date_of_admission)))
dataset= dataset %>% mutate(cgis_diff=.$cgis_dis-.$cgis_adm)
dataset$coocur_symp= dataset %>% select(matches('symptom')) %>% rowSums()
dataset$coocur_trt= dataset %>% select(matches('trt')) %>% rowSums()

#Generate descriptive statistics
age_summary= dataset %>% summarise(mean=mean(age),median=median(age),min=min(age),max=max(age),sd=sd(age))
symptom_summary=dataset %>% select(matches('symptom')) %>% summarise_all(sum)/nrow(dataset)*100
coccur_symptoms=table(dataset$coocur_symp)/nrow(dataset)*100
treatment_summary=dataset %>% select(matches('trt')) %>% summarise_all(sum)/nrow(dataset)*100
coccur_treat=table(dataset$coocur_trt)/nrow(dataset)*100
gender_info=table(dataset$gender)/nrow(dataset)*100
race_info=table(dataset$race)/nrow(dataset)*100
resident_info=table(dataset$resident_status)/nrow(dataset)*100

#generate distribution graphs
hist(dataset$age,breaks = seq(20,90,2))

#convert columns to factors
dataset=dataset %>% mutate(across(matches('trt_|symptom|race|gender|resident'),as.factor))

#Main study question and analysis: which factors influence treatment outcomes?
#We can take the CGIS score of the patient at admission as baseline and a
#successful treatment as a score that is lower than the baseline. In this sense
#you can binarize the outcome and run a binomial regression to determine which
#factors may influence the outcome.

dataset$outcome[dataset$cgis_diff<0]=1
dataset$outcome[dataset$cgis_diff>=0]=0

#select relevant variables for regression
stat_data=dataset %>% select(matches('trt|symp|age|weight|height|medical|gaf|gender|race|outcome'))
cgis_glm=glm(outcome~.,data = stat_data,family = 'binomial')
cgis_summ=summary(cgis_glm)

#Appears that the effect of adt appears to have a significant effect on treatment outcomes compared to others.
#NAs returned. Collinearity? Requires more inspection.