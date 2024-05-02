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
billing_data=billing_data %>%  group_by(patient_id) %>% summarize(mean=mean(amount),min=min(amount),max=max(amount),count=n())

#perform full join on patient data and billing data
full_dataset=full_join(patient_data,billing_data,'patient_id') %>% as.data.frame()

#inspect rows and columns for inconsistent data
#columns medical_history_tum,medical_history_sud, gender, race have discrepancies

#Resolve gender
full_dataset$gender[full_dataset$gender=='m']='Male'
