# CIND 820: Anlalyzing APS data set


# preparing 2018 APS data as the training set


# calling libraries
library(tidyverse)
library(mgsub)
library(corrgram)
library(corrplot)
library(likert)
library(party)

# reading 2018 aps data set
aps <- read.csv("/Users/ibrahimibrahim/Documents/Ryerson/820/data set/2018-aps-employee-census-dataset.csv",stringsAsFactors = TRUE, na.strings = " ")

# when I first examined the dataset, I found out there were no NAs and all unaswered questions
# were categorized as " ", so I modified the read.csv file to regard " " as NA using na.strings=" " 
# all variables are factors so I chose stringAsFators=True


# overview
#head(aps)
#tail(aps)
#str(aps)
#summary(aps)


# renaming variables and creating a new data frame with reduced number of variables

# reading new column names from a csv file
column_names_1 <- read.csv("/Users/ibrahimibrahim/Documents/Ryerson/820/data set/column_names_2018.csv",stringsAsFactors = FALSE, header = TRUE)

aps_new_column_names <- aps
names(aps_new_column_names)[1:301] <- c(column_names_1$new.column.name)
aps_reduced <- select(aps_new_column_names, -contains("disregarded"))


# On page 1 of instructions on how to complete the census, employees were told:
# 1- You are then free to skip and not answer any other questions that you may not want to answer.
# 2- If you cannot answer a question, please feel free to leave it blank.

# a few questions also had the option of answering with "I do not know" which could be 
# one of the motivations for skipping a question - so "I do not know" answers cannot be 
# treated as unique vs. skipped questions (these answers are likely a subset of skipped questions)
# therefore after initial exploration, "I do not know" answers will be treated the same
# way as skipped questions 

# Skippig questions was allowed in survey, so will check for missing values
sum(is.na(aps_reduced))       

aps_reduced$number_skipped_questions <- rowSums(is.na(aps_reduced))
table(aps_reduced$number_skipped_questions)

number_skipped_questions_above_0 <- aps_reduced$number_skipped_questions[aps_reduced$number_skipped_questions>0]
table(number_skipped_questions_above_0)
sum(table(number_skipped_questions_above_0))       # a total of 22,748 respondents who skipped questions
prop.table(table(number_skipped_questions_above_0))*100
barplot(prop.table(table(number_skipped_questions_above_0))*100, main = "Frequency of questions skipped by participants", xlab = "number of questions skipped", ylab = "% of total participants")


# Univariate analysis

# Examining the first 2 (and only demographic) variables :  org_size and employee_level

# first variable org_size

# check for unique values, re-order factor levels and rename factor levels
unique(aps_reduced$org_size)

levels(aps_reduced$org_size)
aps_reduced$org_size <- fct_relevel(aps_reduced$org_size, c("Small (Less than 250 employees)", "Medium (251 to 1,000 employees)", "Large (1,001 or more employees)"))
levels(aps_reduced$org_size)

levels(aps_reduced$org_size) <- list("1" = "Small (Less than 250 employees)", "2" = "Medium (251 to 1,000 employees)",  "3" = "Large (1,001 or more employees)")
levels(aps_reduced$org_size)

# frequency table and check for unclassified
org_size_table <- table(aps_reduced$org_size, useNA = "always")
org_size_table
prop.table(org_size_table)*100

# visualization
ggplot(aps_reduced, aes(x = org_size, fill=org_size)) + 
  geom_bar(show.legend = FALSE) + 
  labs(title = "Number of census participants by size of organization",
       subtitle = "Small(Less than 250 employees)=1,  Medium(251 to 1,000 employees)=2,  Large(1,001 or more employees)=3",
       x = "") +
  theme_bw() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)


# second variable employee_level

# check for unique values, re-order factor levels and rename factor levels
unique(aps_reduced$employee_level)

levels(aps_reduced$employee_level)
aps_reduced$employee_level <- fct_relevel(aps_reduced$employee_level, c("Trainee/Graduate/APS", "EL", "SES"))
levels(aps_reduced$employee_level)

levels(aps_reduced$employee_level) <- list("1" = "SES", "2" = "EL", "3" = "Trainee/Graduate/APS")
levels(aps_reduced$employee_level)

# frequency table and check for unclassified
employee_level_table <- table(aps_reduced$employee_level)
employee_level_table
prop.table(employee_level_table)*100

# visualization
ggplot(aps_reduced, aes(x = employee_level, fill=employee_level)) + 
  geom_bar(show.legend = FALSE) + 
  labs(title = "Employees classification level count",
       subtitle = "Senior executive=1,  executive=2,  non executive=3, skipped questions=NA",
       x = "") +
  theme_bw() +
  geom_text(stat='count', aes(label=..count..), vjust=-1)



# examining variables that will be combined to form the main scales that will be studied

# reformat_variable_group1: 
# a function to change a variable to factor, re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# job_engagement, team_engagement, supervisor_engagement, senior_manager_engagement, 
# agency_engagement, wellbeing (Q9 to Q13), team_performance_support, risk_culture,
# innovation 

reformat_variable_group1 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree"))
  
  levels(variable_to_be_used) <- list("1" = "Strongly disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly agree")
  
  return(variable_to_be_used)
}
#end reformat_variable_group1

# reformat_variable_group2: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# leadership_engagement

unique(aps_reduced$leadership_engagement_1)

reformat_variable_group2 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Strongly disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly agree", "Do not know"))
  
  levels(variable_to_be_used) <- list("1" = "Strongly disagree", "2" = "Disagree", "3" = "Neither agree nor disagree", "4" = "Agree", "5" = "Strongly agree", "Do not know" = "Do not know")
  
  return(variable_to_be_used)
}
#end reformat_variable_group2

# reformat_variable_group3: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# wellbeing_1

unique(aps_reduced$wellbeing_1)

reformat_variable_group3 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Very dissatisfied", "Dissatisfied",  "Neither satisfied or dissatisfied", "Satisfied", "Very satisfied"))
  
  levels(variable_to_be_used) <- list("1" = "Very dissatisfied", "2" = "Dissatisfied",  "3" = "Neither satisfied or dissatisfied", "4" = "Satisfied", "5" = "Very satisfied")
  
  return(variable_to_be_used)
}
#end reformat_variable_group3

# reformat_variable_group4: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# wellbeing_2 and wellbeing_6

unique(aps_reduced$wellbeing_2)
unique(aps_reduced$wellbeing_6)

reformat_variable_group4 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Always", "Often", "Sometimes", "Rarely", "Never"))
  
  levels(variable_to_be_used) <- list("1" = "Always", "2" = "Often", "3" = "Sometimes", "4" = "Rarely", "5" = "Never")
  
  return(variable_to_be_used)
}
#end reformat_variable_group4

# reformat_variable_group5: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# wellbeing_3, wellbeing_4, wellbeing_=5, wellbeing_7, wellbeing_8

unique(aps_reduced$wellbeing_3)

reformat_variable_group5 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Always", "Often", "Sometimes", "Rarely", "Never"))
  
  levels(variable_to_be_used) <- list("1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Always")
  
  return(variable_to_be_used)
}
#end reformat_variable_group5


# reformat_variable_group7: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# values

unique(aps_reduced$values_1)

reformat_variable_group7 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Never", "Rarely", "Sometimes", "Often", "Always", "Not sure"))
  
  levels(variable_to_be_used) <- list("1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Always", "Not sure" = "Not sure")
  
  return(variable_to_be_used)
}
#end reformat_variable_group7

# reformat_variable_group8: 
# a function to re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# team_performance_rating

unique(aps_reduced$team_performance_rating)
unique(aps_reduced$agency_performance_rating)

reformat_variable_group8 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
  variable_to_be_used <- fct_relevel(variable_to_be_used, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Don't know"))
  
  levels(variable_to_be_used) <- list("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9", "10" = "10", "Don't know" = "Don't know")
  
  return(variable_to_be_used)
}
#end reformat_variable_group8

# function to generate a barplot for each variable - except dependent variable

generate_barplot <- function(variable_to_be_used){
  name_to_display <- deparse(substitute(variable_to_be_used))
  
  barplot_1 <- ggplot(aps_reduced, aes(x = variable_to_be_used, fill=variable_to_be_used)) + 
    geom_bar(show.legend = FALSE) + 
    labs(title = name_to_display,  
         subtitle = "(Strongly disagree=1, Disagree=2, Neither agree nor disagree=3, Agree=2, Strongly agree=1, Skipped question=NA)",
         x = "") +
    theme_bw() +
    geom_text(stat='count', aes(label=..count..), vjust=-1) 
  
  return(barplot_1)
}

# function to generate a barplot for dependent variable

generate_barplot_dep_var <- function(variable_to_be_used){
  name_to_display <- deparse(substitute(variable_to_be_used))
  
  barplot_1 <- ggplot(aps_reduced, aes(x = variable_to_be_used, fill=variable_to_be_used)) + 
    geom_bar(show.legend = FALSE) + 
    labs(title = name_to_display,  
         subtitle = "(1=workgroup’s worst performance, 5=average workgroup performance, 10=the best your workgroup has ever worked, NA=Skipped question)",
         x = "") +
    theme_bw() +
    geom_text(stat='count', aes(label=..count..), vjust=-1) 
  
  return(barplot_1)
}


# job_engagement reformatting

aps_reduced$job_engagement_1 <- reformat_variable_group1(aps_reduced$job_engagement_1)
generate_barplot(aps_reduced$job_engagement_1)

aps_reduced$job_engagement_2 <- reformat_variable_group1(aps_reduced$job_engagement_2)
generate_barplot(aps_reduced$job_engagement_2)

# code to check that outputs of function reformat_variable_group1 are correct compared to outputs
# I got earlier on job_engagement_1 and job_engagement_2 not using the function
str(aps_reduced$job_engagement_1)
levels(aps_reduced$job_engagement_1)
summary(aps_reduced$job_engagement_1)
sum(is.na(aps_reduced$job_engagement_1))

str(aps_reduced$job_engagement_2)
levels(aps_reduced$job_engagement_2)
summary(aps_reduced$job_engagement_2)
sum(is.na(aps_reduced$job_engagement_2))

aps_reduced$job_engagement_3 <- reformat_variable_group1(aps_reduced$job_engagement_3)
generate_barplot(aps_reduced$job_engagement_3)

aps_reduced$job_engagement_4 <- reformat_variable_group1(aps_reduced$job_engagement_4)
generate_barplot(aps_reduced$job_engagement_4)

aps_reduced$job_engagement_5 <- reformat_variable_group1(aps_reduced$job_engagement_5)
generate_barplot(aps_reduced$job_engagement_5)

aps_reduced$job_engagement_6 <- reformat_variable_group1(aps_reduced$job_engagement_6)
generate_barplot(aps_reduced$job_engagement_6)

aps_reduced$job_engagement_7 <- reformat_variable_group1(aps_reduced$job_engagement_7)
generate_barplot(aps_reduced$job_engagement_7)

aps_reduced$job_engagement_8 <- reformat_variable_group1(aps_reduced$job_engagement_8)
generate_barplot(aps_reduced$job_engagement_8)

aps_reduced$job_engagement_9 <- reformat_variable_group1(aps_reduced$job_engagement_9)
generate_barplot(aps_reduced$job_engagement_9)

aps_reduced$job_engagement_10 <- reformat_variable_group1(aps_reduced$job_engagement_10)
generate_barplot(aps_reduced$job_engagement_10)

# team_engagement reformatting

aps_reduced$team_engagement_1 <- reformat_variable_group1(aps_reduced$team_engagement_1)
generate_barplot(aps_reduced$team_engagement_1)

aps_reduced$team_engagement_2 <- reformat_variable_group1(aps_reduced$team_engagement_2)
generate_barplot(aps_reduced$team_engagement_2)

aps_reduced$team_engagement_3 <- reformat_variable_group1(aps_reduced$team_engagement_3)
generate_barplot(aps_reduced$team_engagement_3)

aps_reduced$team_engagement_4 <- reformat_variable_group1(aps_reduced$team_engagement_4)
generate_barplot(aps_reduced$team_engagement_4)

# supervisor_engagement reformatting

aps_reduced$supervisor_engagement_1 <- reformat_variable_group1(aps_reduced$supervisor_engagement_1)
generate_barplot(aps_reduced$supervisor_engagement_1)

aps_reduced$supervisor_engagement_2 <- reformat_variable_group1(aps_reduced$supervisor_engagement_2)
generate_barplot(aps_reduced$supervisor_engagement_2)

aps_reduced$supervisor_engagement_3 <- reformat_variable_group1(aps_reduced$supervisor_engagement_2)
generate_barplot(aps_reduced$supervisor_engagement_2)

aps_reduced$supervisor_engagement_4 <- reformat_variable_group1(aps_reduced$supervisor_engagement_4)
generate_barplot(aps_reduced$supervisor_engagement_4)

aps_reduced$supervisor_engagement_5 <- reformat_variable_group1(aps_reduced$supervisor_engagement_5)
generate_barplot(aps_reduced$supervisor_engagement_5)

aps_reduced$supervisor_engagement_6 <- reformat_variable_group1(aps_reduced$supervisor_engagement_6)
generate_barplot(aps_reduced$supervisor_engagement_6)

aps_reduced$supervisor_engagement_7 <- reformat_variable_group1(aps_reduced$supervisor_engagement_7)
generate_barplot(aps_reduced$supervisor_engagement_7)

aps_reduced$supervisor_engagement_8 <- reformat_variable_group1(aps_reduced$supervisor_engagement_8)
generate_barplot(aps_reduced$supervisor_engagement_8)

aps_reduced$supervisor_engagement_9 <- reformat_variable_group1(aps_reduced$supervisor_engagement_9)
generate_barplot(aps_reduced$supervisor_engagement_9)

aps_reduced$supervisor_engagement_10 <- reformat_variable_group1(aps_reduced$supervisor_engagement_10)
generate_barplot(aps_reduced$supervisor_engagement_10)

aps_reduced$supervisor_engagement_11 <- reformat_variable_group1(aps_reduced$supervisor_engagement_11)
generate_barplot(aps_reduced$supervisor_engagement_11)

# senior_manager_engagement reformatting

aps_reduced$senior_manager_engagement_1 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_1)
generate_barplot(aps_reduced$senior_manager_engagement_1)

aps_reduced$senior_manager_engagement_2 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_2)
generate_barplot(aps_reduced$senior_manager_engagement_2)

aps_reduced$senior_manager_engagement_3 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_3)
generate_barplot(aps_reduced$senior_manager_engagement_3)

aps_reduced$senior_manager_engagement_4 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_4)
generate_barplot(aps_reduced$senior_manager_engagement_4)

aps_reduced$senior_manager_engagement_5 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_5)
generate_barplot(aps_reduced$senior_manager_engagement_5)

aps_reduced$senior_manager_engagement_6 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_6)
generate_barplot(aps_reduced$senior_manager_engagement_6)

aps_reduced$senior_manager_engagement_7 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_7)
generate_barplot(aps_reduced$senior_manager_engagement_7)

aps_reduced$senior_manager_engagement_8 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_8)
generate_barplot(aps_reduced$senior_manager_engagement_8)

aps_reduced$senior_manager_engagement_9 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_9)
generate_barplot(aps_reduced$senior_manager_engagement_9)

aps_reduced$senior_manager_engagement_10 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_10)
generate_barplot(aps_reduced$senior_manager_engagement_10)

aps_reduced$senior_manager_engagement_11 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_11)
generate_barplot(aps_reduced$senior_manager_engagement_11)

aps_reduced$senior_manager_engagement_12 <- reformat_variable_group1(aps_reduced$senior_manager_engagement_12)
generate_barplot(aps_reduced$senior_manager_engagement_12)

# agency_engagement reformatting

aps_reduced$agency_engagement_1 <- reformat_variable_group1(aps_reduced$agency_engagement_1)
generate_barplot(aps_reduced$agency_engagement_1)

aps_reduced$agency_engagement_2 <- reformat_variable_group1(aps_reduced$agency_engagement_2)
generate_barplot(aps_reduced$agency_engagement_2)

aps_reduced$agency_engagement_3 <- reformat_variable_group1(aps_reduced$agency_engagement_3)
generate_barplot(aps_reduced$agency_engagement_3)

aps_reduced$agency_engagement_4 <- reformat_variable_group1(aps_reduced$agency_engagement_4)
generate_barplot(aps_reduced$agency_engagement_4)

aps_reduced$agency_engagement_5 <- reformat_variable_group1(aps_reduced$agency_engagement_5)
generate_barplot(aps_reduced$agency_engagement_5)

aps_reduced$agency_engagement_6 <- reformat_variable_group1(aps_reduced$agency_engagement_6)
generate_barplot(aps_reduced$agency_engagement_6)

aps_reduced$agency_engagement_7 <- reformat_variable_group1(aps_reduced$agency_engagement_7)
generate_barplot(aps_reduced$agency_engagement_7)

aps_reduced$agency_engagement_8 <- reformat_variable_group1(aps_reduced$agency_engagement_8)
generate_barplot(aps_reduced$agency_engagement_8)

aps_reduced$agency_engagement_9 <- reformat_variable_group1(aps_reduced$agency_engagement_9)
generate_barplot(aps_reduced$agency_engagement_9)

aps_reduced$agency_engagement_10 <- reformat_variable_group1(aps_reduced$agency_engagement_10)
generate_barplot(aps_reduced$agency_engagement_10)

aps_reduced$agency_engagement_11 <- reformat_variable_group1(aps_reduced$agency_engagement_11)
generate_barplot(aps_reduced$agency_engagement_11)

aps_reduced$agency_engagement_12 <- reformat_variable_group1(aps_reduced$agency_engagement_12)
generate_barplot(aps_reduced$agency_engagement_12)

aps_reduced$agency_engagement_13 <- reformat_variable_group1(aps_reduced$agency_engagement_13)
generate_barplot(aps_reduced$agency_engagement_13)

aps_reduced$agency_engagement_14 <- reformat_variable_group1(aps_reduced$agency_engagement_14)
generate_barplot(aps_reduced$agency_engagement_14)

aps_reduced$agency_engagement_15 <- reformat_variable_group1(aps_reduced$agency_engagement_15)
generate_barplot(aps_reduced$agency_engagement_15)

aps_reduced$agency_engagement_16 <- reformat_variable_group1(aps_reduced$agency_engagement_16)
generate_barplot(aps_reduced$agency_engagement_16)

aps_reduced$agency_engagement_17 <- reformat_variable_group1(aps_reduced$agency_engagement_17)
generate_barplot(aps_reduced$agency_engagement_17)

# team_performance_support reformatting

aps_reduced$team_performance_support_1 <- reformat_variable_group1(aps_reduced$team_performance_support_1)
generate_barplot(aps_reduced$team_performance_support_1)

aps_reduced$team_performance_support_2 <- reformat_variable_group1(aps_reduced$team_performance_support_2)
generate_barplot(aps_reduced$team_performance_support_2)

aps_reduced$team_performance_support_3 <- reformat_variable_group1(aps_reduced$team_performance_support_3)
generate_barplot(aps_reduced$team_performance_support_3)

aps_reduced$team_performance_support_4 <- reformat_variable_group1(aps_reduced$team_performance_support_4)
generate_barplot(aps_reduced$team_performance_support_4)

# risk culture reformatting

aps_reduced$risk_culture_1 <- reformat_variable_group1(aps_reduced$risk_culture_1)
generate_barplot(aps_reduced$risk_culture_1)

aps_reduced$risk_culture_2 <- reformat_variable_group1(aps_reduced$risk_culture_2)
generate_barplot(aps_reduced$risk_culture_2)

aps_reduced$risk_culture_3 <- reformat_variable_group1(aps_reduced$risk_culture_3)
generate_barplot(aps_reduced$risk_culture_3)

aps_reduced$risk_culture_4 <- reformat_variable_group1(aps_reduced$risk_culture_4)
generate_barplot(aps_reduced$risk_culture_4)

aps_reduced$risk_culture_5 <- reformat_variable_group1(aps_reduced$risk_culture_5)
generate_barplot(aps_reduced$risk_culture_5)

# reformatting innovation

aps_reduced$innovation_1 <- reformat_variable_group1(aps_reduced$innovation_1)
generate_barplot(aps_reduced$innovation_1)

aps_reduced$innovation_2 <- reformat_variable_group1(aps_reduced$innovation_2)
generate_barplot(aps_reduced$innovation_2)

aps_reduced$innovation_3 <- reformat_variable_group1(aps_reduced$innovation_3)
generate_barplot(aps_reduced$innovation_3)

aps_reduced$innovation_4 <- reformat_variable_group1(aps_reduced$innovation_4)
generate_barplot(aps_reduced$innovation_4)

aps_reduced$innovation_5 <- reformat_variable_group1(aps_reduced$innovation_5)
generate_barplot(aps_reduced$innovation_5)

# leadership engagement reformatting - group2

aps_reduced$leadership_engagement_1 <- reformat_variable_group2(aps_reduced$leadership_engagement_1)
generate_barplot(aps_reduced$leadership_engagement_1)

# checking that function for reformartting variable worked
str(aps_reduced$leadership_engagement_1)
levels(aps_reduced$leadership_engagement_1)
summary(aps_reduced$leadership_engagement_1)
sum(is.na(aps_reduced$leadership_engagement_1))

aps_reduced$leadership_engagement_2 <- reformat_variable_group2(aps_reduced$leadership_engagement_2)
generate_barplot(aps_reduced$leadership_engagement_2)

aps_reduced$leadership_engagement_3 <- reformat_variable_group2(aps_reduced$leadership_engagement_3)
generate_barplot(aps_reduced$leadership_engagement_3)

aps_reduced$leadership_engagement_4 <- reformat_variable_group2(aps_reduced$leadership_engagement_4)
generate_barplot(aps_reduced$leadership_engagement_4)

aps_reduced$leadership_engagement_5 <- reformat_variable_group2(aps_reduced$leadership_engagement_5)
generate_barplot(aps_reduced$leadership_engagement_5)

aps_reduced$leadership_engagement_6 <- reformat_variable_group2(aps_reduced$leadership_engagement_6)
generate_barplot(aps_reduced$leadership_engagement_6)

aps_reduced$leadership_engagement_7 <- reformat_variable_group2(aps_reduced$leadership_engagement_7)
generate_barplot(aps_reduced$leadership_engagement_7)

# wellbeing_1 reformatting - group3

aps_reduced$wellbeing_1 <- reformat_variable_group3(aps_reduced$wellbeing_1)
generate_barplot(aps_reduced$wellbeing_1)

# checking that function for reformartting variable worked
str(aps_reduced$wellbeing_1)
levels(aps_reduced$wellbeing_1)
summary(aps_reduced$wellbeing_1)
sum(is.na(aps_reduced$wellbeing_1))

# wellbeing_2 and wellbeing_6 reformatting - group4

aps_reduced$wellbeing_2 <- reformat_variable_group4(aps_reduced$wellbeing_2)
generate_barplot(aps_reduced$wellbeing_2)

aps_reduced$wellbeing_6 <- reformat_variable_group4(aps_reduced$wellbeing_6)
generate_barplot(aps_reduced$wellbeing_6)

# checking that function for reformartting variable worked
str(aps_reduced$wellbeing_2)
levels(aps_reduced$wellbeing_2)
summary(aps_reduced$wellbeing_2)
sum(is.na(aps_reduced$wellbeing_2))

str(aps_reduced$wellbeing_6)
levels(aps_reduced$wellbeing_6)
summary(aps_reduced$wellbeing_6)
sum(is.na(aps_reduced$wellbeing_6))

# wellbeing_3, wellbeing_4, wellbeing_=5, wellbeing_7, wellbeing_8 reformating - group5

aps_reduced$wellbeing_3 <- reformat_variable_group5(aps_reduced$wellbeing_3)
generate_barplot(aps_reduced$wellbeing_3)

str(aps_reduced$wellbeing_3)
levels(aps_reduced$wellbeing_3)
summary(aps_reduced$wellbeing_3)
sum(is.na(aps_reduced$wellbeing_3))

aps_reduced$wellbeing_4 <- reformat_variable_group5(aps_reduced$wellbeing_4)
generate_barplot(aps_reduced$wellbeing_4)

aps_reduced$wellbeing_5 <- reformat_variable_group5(aps_reduced$wellbeing_5)
generate_barplot(aps_reduced$wellbeing_5)

aps_reduced$wellbeing_7 <- reformat_variable_group5(aps_reduced$wellbeing_7)
generate_barplot(aps_reduced$wellbeing_7)

aps_reduced$wellbeing_8 <- reformat_variable_group5(aps_reduced$wellbeing_8)
generate_barplot(aps_reduced$wellbeing_8)

# wellbeing_9,  wellbeing_10, wellbeing_11, wellbeing_12, wellbeing_13 reformatting - group1

aps_reduced$wellbeing_9 <- reformat_variable_group1(aps_reduced$wellbeing_9)
generate_barplot(aps_reduced$wellbeing_9)

aps_reduced$wellbeing_10 <- reformat_variable_group1(aps_reduced$wellbeing_10)
generate_barplot(aps_reduced$wellbeing_10)

aps_reduced$wellbeing_11 <- reformat_variable_group1(aps_reduced$wellbeing_11)
generate_barplot(aps_reduced$wellbeing_11)

aps_reduced$wellbeing_12 <- reformat_variable_group1(aps_reduced$wellbeing_12)
generate_barplot(aps_reduced$wellbeing_12)

aps_reduced$wellbeing_13 <- reformat_variable_group1(aps_reduced$wellbeing_13)
generate_barplot(aps_reduced$wellbeing_13)


# values reformatting - group7

aps_reduced$values_1 <- reformat_variable_group7(aps_reduced$values_1)
generate_barplot(aps_reduced$values_1)

str(aps_reduced$values_1)
levels(aps_reduced$values_1)
summary(aps_reduced$values_1)
sum(is.na(aps_reduced$values_1))

aps_reduced$values_2 <- reformat_variable_group7(aps_reduced$values_2)
generate_barplot(aps_reduced$values_2)

aps_reduced$values_3 <- reformat_variable_group7(aps_reduced$values_3)
generate_barplot(aps_reduced$values_3)

# team_performance_rating reformatting - dependent variable - group8

aps_reduced$team_performance_rating <- reformat_variable_group8(aps_reduced$team_performance_rating)
generate_barplot_dep_var(aps_reduced$team_performance_rating)

str(aps_reduced$team_performance_rating)
levels(aps_reduced$team_performance_rating)
summary(aps_reduced$team_performance_rating)
sum(is.na(aps_reduced$team_performance_rating))

# handling no response and I do not know / Not sure

# total number of skipped questions
sum(is.na(aps_reduced))         #362,030 skipped questions in 2018 vs. 355,416 in 2019 
table(rowSums((is.na(aps_reduced))>0)>0)  # 22,646 respondents in 2018 vs. 23,037 in 2019 

# out of 129 variables, only the following have "I do not know" as an option for answers:
# leadership_engagement 1:7                                
length(which(aps_reduced=="Do not know"))   #31,282 responses in 2018 vs.31,247 in 2019 
table(rowSums(aps_reduced=="Do not know")>0)  #8,092 respondents in 2018 vs.7,990 in 2019 

# team_performance_rating                                  
length(which(aps_reduced=="Don't know"))    #8,487 responses in 2018 vs. 8,726 in 2019  
table(rowSums(aps_reduced=="Don't know")>0) #6,572 respondents in 2018 vs.6,629 in 2019 

# one variable had a "not sure" option": values 1:3        
length(which(aps_reduced=="Not sure"))    #17,277 responses in 2018 vs. 15,786 in 2019 
table(rowSums(aps_reduced=="Not sure")>0) #11,764 respondents in 2018 vs. 15,786 in 2019 


# Handling of skipped questions and non standard answer options 
# a) removing all respondents with more than 13 skipped questions
# b) removing all respondents who answered the team performance question with either
# don't know or who skipped
# c) replacing all "Do not know" for leadership_engagement 1 to 7 with "3" of each variable
# d) replacing all re "not sure" for values 1 to 3 with "3" for each variable
# e) replacing all remaining skipped questions with "3" for each variable

# a) removing all respondents with more than 13 skipped questions

number_skipped_questions_above_0 <- aps_reduced$number_skipped_questions[aps_reduced$number_skipped_questions>0]
#data[data[, "Var"]<=13, ]
question_threshold <- 3
aps_reduced <- aps_reduced[aps_reduced[ , "number_skipped_questions"]<=question_threshold, ]

# b) removing all respondents who answered the team performance question with either
# Don't know or who skipped

aps_reduced <- aps_reduced[aps_reduced[ , "team_performance_rating"]!="Don't know", ]
aps_reduced <- aps_reduced[!is.na(aps_reduced$team_performance_rating) , ]
levels(aps_reduced$team_performance_rating)

# c) replacing all "Do not know" for leadership_engagement 1 to 7 with "3"

replacement_for_Do_not_know <- "3"

aps_reduced$leadership_engagement_1 <- replace(aps_reduced$leadership_engagement_1, aps_reduced$leadership_engagement_1 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_2 <- replace(aps_reduced$leadership_engagement_2, aps_reduced$leadership_engagement_2 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_3 <- replace(aps_reduced$leadership_engagement_3, aps_reduced$leadership_engagement_3 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_4 <- replace(aps_reduced$leadership_engagement_4, aps_reduced$leadership_engagement_4 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_5 <- replace(aps_reduced$leadership_engagement_5, aps_reduced$leadership_engagement_5 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_6 <- replace(aps_reduced$leadership_engagement_6, aps_reduced$leadership_engagement_6 == "Do not know", replacement_for_Do_not_know)
aps_reduced$leadership_engagement_7 <- replace(aps_reduced$leadership_engagement_7, aps_reduced$leadership_engagement_7 == "Do not know", replacement_for_Do_not_know)


# d) replacing all re "Not sure" for values 1 to 3 with "3"

replacement_for_Not_sure <- "3"

aps_reduced$values_1 <- replace(aps_reduced$values_1, aps_reduced$values_1 == "Not sure", replacement_for_Not_sure)
aps_reduced$values_2 <- replace(aps_reduced$values_2, aps_reduced$values_2 == "Not sure", replacement_for_Not_sure)
aps_reduced$values_3 <- replace(aps_reduced$values_3, aps_reduced$values_3 == "Not sure", replacement_for_Not_sure)

# e) replacing all remaining skipped questions with "3"

replacement_for_NAs <- "3"


aps_reduced$org_size[which(is.na(aps_reduced$org_size))] = replacement_for_NAs 
aps_reduced$employee_level[which(is.na(aps_reduced$employee_level))] = replacement_for_NAs

aps_reduced$job_engagement_1[which(is.na(aps_reduced$job_engagement_1))] = replacement_for_NAs
aps_reduced$job_engagement_2[which(is.na(aps_reduced$job_engagement_2))] = replacement_for_NAs
aps_reduced$job_engagement_3[which(is.na(aps_reduced$job_engagement_3))] = replacement_for_NAs 
aps_reduced$job_engagement_4[which(is.na(aps_reduced$job_engagement_4))] = replacement_for_NAs 
aps_reduced$job_engagement_5[which(is.na(aps_reduced$job_engagement_5))] = replacement_for_NAs 
aps_reduced$job_engagement_6[which(is.na(aps_reduced$job_engagement_6))] = replacement_for_NAs 
aps_reduced$job_engagement_7[which(is.na(aps_reduced$job_engagement_7))] = replacement_for_NAs 
aps_reduced$job_engagement_8[which(is.na(aps_reduced$job_engagement_8))] = replacement_for_NAs 
aps_reduced$job_engagement_9[which(is.na(aps_reduced$job_engagement_9))] = replacement_for_NAs 
aps_reduced$job_engagement_10[which(is.na(aps_reduced$job_engagement_10))] = replacement_for_NAs 

aps_reduced$team_engagement_1[which(is.na(aps_reduced$team_engagement_1))] = replacement_for_NAs 
aps_reduced$team_engagement_2[which(is.na(aps_reduced$team_engagement_2))] = replacement_for_NAs  
aps_reduced$team_engagement_3[which(is.na(aps_reduced$team_engagement_3))] = replacement_for_NAs  
aps_reduced$team_engagement_4[which(is.na(aps_reduced$team_engagement_4))] = replacement_for_NAs  

aps_reduced$supervisor_engagement_1[which(is.na(aps_reduced$supervisor_engagement_1))] = replacement_for_NAs
aps_reduced$supervisor_engagement_2[which(is.na(aps_reduced$supervisor_engagement_2))] = replacement_for_NAs
aps_reduced$supervisor_engagement_3[which(is.na(aps_reduced$supervisor_engagement_3))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_4[which(is.na(aps_reduced$supervisor_engagement_4))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_5[which(is.na(aps_reduced$supervisor_engagement_5))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_6[which(is.na(aps_reduced$supervisor_engagement_6))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_7[which(is.na(aps_reduced$supervisor_engagement_7))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_8[which(is.na(aps_reduced$supervisor_engagement_8))] = replacement_for_NAs
aps_reduced$supervisor_engagement_9[which(is.na(aps_reduced$supervisor_engagement_9))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_10[which(is.na(aps_reduced$supervisor_engagement_10))] = replacement_for_NAs 
aps_reduced$supervisor_engagement_11[which(is.na(aps_reduced$supervisor_engagement_11))] = replacement_for_NAs

aps_reduced$senior_manager_engagement_1[which(is.na(aps_reduced$senior_manager_engagement_1))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_2[which(is.na(aps_reduced$senior_manager_engagement_2))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_3[which(is.na(aps_reduced$senior_manager_engagement_3))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_4[which(is.na(aps_reduced$senior_manager_engagement_4))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_5[which(is.na(aps_reduced$senior_manager_engagement_5))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_6[which(is.na(aps_reduced$senior_manager_engagement_6))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_7[which(is.na(aps_reduced$senior_manager_engagement_7))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_8[which(is.na(aps_reduced$senior_manager_engagement_8))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_9[which(is.na(aps_reduced$senior_manager_engagement_9))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_10[which(is.na(aps_reduced$senior_manager_engagement_10))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_11[which(is.na(aps_reduced$senior_manager_engagement_11))] = replacement_for_NAs
aps_reduced$senior_manager_engagement_12[which(is.na(aps_reduced$senior_manager_engagement_12))] = replacement_for_NAs

aps_reduced$agency_engagement_1[which(is.na(aps_reduced$agency_engagement_1))] = replacement_for_NAs
aps_reduced$agency_engagement_2[which(is.na(aps_reduced$agency_engagement_2))] = replacement_for_NAs
aps_reduced$agency_engagement_3[which(is.na(aps_reduced$agency_engagement_3))] = replacement_for_NAs
aps_reduced$agency_engagement_4[which(is.na(aps_reduced$agency_engagement_4))] = replacement_for_NAs
aps_reduced$agency_engagement_5[which(is.na(aps_reduced$agency_engagement_5))] = replacement_for_NAs
aps_reduced$agency_engagement_6[which(is.na(aps_reduced$agency_engagement_6))] = replacement_for_NAs
aps_reduced$agency_engagement_7[which(is.na(aps_reduced$agency_engagement_7))] = replacement_for_NAs
aps_reduced$agency_engagement_8[which(is.na(aps_reduced$agency_engagement_8))] = replacement_for_NAs
aps_reduced$agency_engagement_9[which(is.na(aps_reduced$agency_engagement_9))] = replacement_for_NAs
aps_reduced$agency_engagement_10[which(is.na(aps_reduced$agency_engagement_10))] = replacement_for_NAs
aps_reduced$agency_engagement_11[which(is.na(aps_reduced$agency_engagement_11))] = replacement_for_NAs
aps_reduced$agency_engagement_12[which(is.na(aps_reduced$agency_engagement_12))] = replacement_for_NAs
aps_reduced$agency_engagement_13[which(is.na(aps_reduced$agency_engagement_13))] = replacement_for_NAs
aps_reduced$agency_engagement_14[which(is.na(aps_reduced$agency_engagement_14))] = replacement_for_NAs
aps_reduced$agency_engagement_15[which(is.na(aps_reduced$agency_engagement_15))] = replacement_for_NAs
aps_reduced$agency_engagement_16[which(is.na(aps_reduced$agency_engagement_16))] = replacement_for_NAs
aps_reduced$agency_engagement_17[which(is.na(aps_reduced$agency_engagement_17))] = replacement_for_NAs

aps_reduced$team_performance_support_1[which(is.na(aps_reduced$team_performance_support_1))] = replacement_for_NAs
aps_reduced$team_performance_support_2[which(is.na(aps_reduced$team_performance_support_2))] = replacement_for_NAs
aps_reduced$team_performance_support_3[which(is.na(aps_reduced$team_performance_support_3))] = replacement_for_NAs
aps_reduced$team_performance_support_4[which(is.na(aps_reduced$team_performance_support_4))] = replacement_for_NAs

aps_reduced$risk_culture_1[which(is.na(aps_reduced$risk_culture_1))] = replacement_for_NAs
aps_reduced$risk_culture_2[which(is.na(aps_reduced$risk_culture_2))] = replacement_for_NAs
aps_reduced$risk_culture_3[which(is.na(aps_reduced$risk_culture_3))] = replacement_for_NAs
aps_reduced$risk_culture_4[which(is.na(aps_reduced$risk_culture_4))] = replacement_for_NAs
aps_reduced$risk_culture_5[which(is.na(aps_reduced$risk_culture_5))] = replacement_for_NAs

aps_reduced$innovation_1[which(is.na(aps_reduced$innovation_1))] = replacement_for_NAs
aps_reduced$innovation_2[which(is.na(aps_reduced$innovation_2))] = replacement_for_NAs
aps_reduced$innovation_3[which(is.na(aps_reduced$innovation_3))] = replacement_for_NAs
aps_reduced$innovation_4[which(is.na(aps_reduced$innovation_4))] = replacement_for_NAs
aps_reduced$innovation_5[which(is.na(aps_reduced$innovation_5))] = replacement_for_NAs

aps_reduced$leadership_engagement_1[which(is.na(aps_reduced$leadership_engagement_1))] = replacement_for_NAs
aps_reduced$leadership_engagement_2[which(is.na(aps_reduced$leadership_engagement_2))] = replacement_for_NAs
aps_reduced$leadership_engagement_3[which(is.na(aps_reduced$leadership_engagement_3))] = replacement_for_NAs
aps_reduced$leadership_engagement_4[which(is.na(aps_reduced$leadership_engagement_4))] = replacement_for_NAs
aps_reduced$leadership_engagement_5[which(is.na(aps_reduced$leadership_engagement_5))] = replacement_for_NAs
aps_reduced$leadership_engagement_6[which(is.na(aps_reduced$leadership_engagement_6))] = replacement_for_NAs
aps_reduced$leadership_engagement_7[which(is.na(aps_reduced$leadership_engagement_7))] = replacement_for_NAs

aps_reduced$wellbeing_1[which(is.na(aps_reduced$wellbeing_1))] = replacement_for_NAs
aps_reduced$wellbeing_2[which(is.na(aps_reduced$wellbeing_2))] = replacement_for_NAs
aps_reduced$wellbeing_3[which(is.na(aps_reduced$wellbeing_3))] = replacement_for_NAs
aps_reduced$wellbeing_4[which(is.na(aps_reduced$wellbeing_4))] = replacement_for_NAs
aps_reduced$wellbeing_5[which(is.na(aps_reduced$wellbeing_5))] = replacement_for_NAs
aps_reduced$wellbeing_6[which(is.na(aps_reduced$wellbeing_6))] = replacement_for_NAs
aps_reduced$wellbeing_7[which(is.na(aps_reduced$wellbeing_7))] = replacement_for_NAs
aps_reduced$wellbeing_8[which(is.na(aps_reduced$wellbeing_8))] = replacement_for_NAs
aps_reduced$wellbeing_9[which(is.na(aps_reduced$wellbeing_9))] = replacement_for_NAs
aps_reduced$wellbeing_10[which(is.na(aps_reduced$wellbeing_10))] = replacement_for_NAs
aps_reduced$wellbeing_11[which(is.na(aps_reduced$wellbeing_11))] = replacement_for_NAs
aps_reduced$wellbeing_12[which(is.na(aps_reduced$wellbeing_12))] = replacement_for_NAs
aps_reduced$wellbeing_13[which(is.na(aps_reduced$wellbeing_13))] = replacement_for_NAs

aps_reduced$values_1[which(is.na(aps_reduced$values_1))] = replacement_for_NAs
aps_reduced$values_2[which(is.na(aps_reduced$values_2))] = replacement_for_NAs
aps_reduced$values_3[which(is.na(aps_reduced$values_3))] = replacement_for_NAs

# double checking data has no missing values and no answers other than 1-5

sum(is.na(aps_reduced))         
length(which(aps_reduced=="Do not know"))                  
length(which(aps_reduced=="Don't know"))            
length(which(aps_reduced=="Not sure")) 


# developing 11 data frames grouping questions as per census

job_engagement_df <- data.frame(aps_reduced$job_engagement_1, aps_reduced$job_engagement_2, aps_reduced$job_engagement_3, aps_reduced$job_engagement_4, aps_reduced$job_engagement_5, aps_reduced$job_engagement_6, aps_reduced$job_engagement_7, aps_reduced$job_engagement_8, aps_reduced$job_engagement_9, aps_reduced$job_engagement_10)
team_engagement_df <- data.frame(aps_reduced$team_engagement_1, aps_reduced$team_engagement_2, aps_reduced$team_engagement_3, aps_reduced$team_engagement_4)
supervisor_engagement_df <- data.frame(aps_reduced$supervisor_engagement_1, aps_reduced$supervisor_engagement_2, aps_reduced$supervisor_engagement_3, aps_reduced$supervisor_engagement_4, aps_reduced$supervisor_engagement_5, aps_reduced$supervisor_engagement_6, aps_reduced$supervisor_engagement_7, aps_reduced$supervisor_engagement_8, aps_reduced$supervisor_engagement_9, aps_reduced$supervisor_engagement_10, aps_reduced$supervisor_engagement_11)
senior_manager_engagement_df <- data.frame(aps_reduced$senior_manager_engagement_1, aps_reduced$senior_manager_engagement_2, aps_reduced$senior_manager_engagement_3, aps_reduced$senior_manager_engagement_4, aps_reduced$senior_manager_engagement_5, aps_reduced$senior_manager_engagement_6, aps_reduced$senior_manager_engagement_7, aps_reduced$senior_manager_engagement_8, aps_reduced$senior_manager_engagement_9, aps_reduced$senior_manager_engagement_10, aps_reduced$senior_manager_engagement_11, aps_reduced$senior_manager_engagement_12)
agency_engagement_df <- data.frame(aps_reduced$agency_engagement_1, aps_reduced$agency_engagement_2, aps_reduced$agency_engagement_3, aps_reduced$agency_engagement_4, aps_reduced$agency_engagement_5, aps_reduced$agency_engagement_6, aps_reduced$agency_engagement_7, aps_reduced$agency_engagement_8, aps_reduced$agency_engagement_9, aps_reduced$agency_engagement_10, aps_reduced$agency_engagement_11, aps_reduced$agency_engagement_12, aps_reduced$agency_engagement_13, aps_reduced$agency_engagement_14, aps_reduced$agency_engagement_15, aps_reduced$agency_engagement_16, aps_reduced$agency_engagement_17)
team_performance_support_df <- data.frame(aps_reduced$team_performance_support_1, aps_reduced$team_performance_support_2, aps_reduced$team_performance_support_3, aps_reduced$team_performance_support_4)
risk_culture_df <- data.frame(aps_reduced$risk_culture_1, aps_reduced$risk_culture_2, aps_reduced$risk_culture_3, aps_reduced$risk_culture_4, aps_reduced$risk_culture_5)
innovation_df <- data.frame(aps_reduced$innovation_1, aps_reduced$innovation_2, aps_reduced$innovation_3, aps_reduced$innovation_4, aps_reduced$innovation_5)
leadership_engagement_df <- data.frame(aps_reduced$leadership_engagement_1, aps_reduced$leadership_engagement_2, aps_reduced$leadership_engagement_3, aps_reduced$leadership_engagement_4, aps_reduced$leadership_engagement_5, aps_reduced$leadership_engagement_6, aps_reduced$leadership_engagement_7)
wellbeing_df <- data.frame(aps_reduced$wellbeing_1, aps_reduced$wellbeing_2, aps_reduced$wellbeing_3, aps_reduced$wellbeing_4, aps_reduced$wellbeing_5, aps_reduced$wellbeing_6, aps_reduced$wellbeing_7, aps_reduced$wellbeing_8, aps_reduced$wellbeing_9, aps_reduced$wellbeing_10, aps_reduced$wellbeing_11, aps_reduced$wellbeing_12, aps_reduced$wellbeing_13)
values_df <- data.frame(aps_reduced$values_1, aps_reduced$values_2, aps_reduced$values_3)


# descriptive statistics for questions within each scale

# Descriptive statistics step 1: entering descriptive names for columns

names(job_engagement_df) <- c(
  job_engagement_1 = column_names_1$full.question[5],
  job_engagement_2 = column_names_1$full.question[6],
  job_engagement_3 = column_names_1$full.question[7],
  job_engagement_4 = column_names_1$full.question[8],
  job_engagement_5 = column_names_1$full.question[9],
  job_engagement_6 = column_names_1$full.question[10],
  job_engagement_7 = column_names_1$full.question[11],
  job_engagement_8 = column_names_1$full.question[12],
  job_engagement_9 = column_names_1$full.question[13],
  job_engagement_10 = column_names_1$full.question[14]
)

names(team_engagement_df) <- c(
  team_engagement_1 = column_names_1$full.question[16],
  team_engagement_2 = column_names_1$full.question[17],
  team_engagement_3 = column_names_1$full.question[18],
  team_engagement_4 = column_names_1$full.question[19]
)

names(supervisor_engagement_df) <- c(
  supervisor_engagement_1 = column_names_1$full.question[21],
  supervisor_engagement_2 = column_names_1$full.question[22],
  supervisor_engagement_3 = column_names_1$full.question[23],
  supervisor_engagement_4 = column_names_1$full.question[24],
  supervisor_engagement_5 = column_names_1$full.question[26],
  supervisor_engagement_6 = column_names_1$full.question[27],
  supervisor_engagement_7 = column_names_1$full.question[28],
  supervisor_engagement_8 = column_names_1$full.question[29],
  supervisor_engagement_9 = column_names_1$full.question[30],
  supervisor_engagement_10 = column_names_1$full.question[31],
  supervisor_engagement_11 = column_names_1$full.question[32]
)

names(senior_manager_engagement_df) <- c(
  senior_manager_engagement_1 = column_names_1$full.question[33],
  senior_manager_engagement_2 = column_names_1$full.question[34],
  senior_manager_engagement_3 = column_names_1$full.question[35],
  senior_manager_engagement_4 = column_names_1$full.question[36],
  senior_manager_engagement_5 = column_names_1$full.question[37],
  senior_manager_engagement_6 = column_names_1$full.question[38],
  senior_manager_engagement_7 = column_names_1$full.question[39],
  senior_manager_engagement_8 = column_names_1$full.question[41],
  senior_manager_engagement_9 = column_names_1$full.question[42],
  senior_manager_engagement_10 = column_names_1$full.question[43],  
  senior_manager_engagement_11 = column_names_1$full.question[44],
  senior_manager_engagement_12 = column_names_1$full.question[46]
)

names(agency_engagement_df) <- c(
  agency_engagement_1 = column_names_1$full.question[55],
  agency_engagement_2 = column_names_1$full.question[56],
  agency_engagement_3 = column_names_1$full.question[57],
  agency_engagement_4 = column_names_1$full.question[58],
  agency_engagement_5 = column_names_1$full.question[59],
  agency_engagement_6 = column_names_1$full.question[60],
  agency_engagement_7 = column_names_1$full.question[61],
  agency_engagement_8 = column_names_1$full.question[62],
  agency_engagement_9 = column_names_1$full.question[63],
  agency_engagement_10 = column_names_1$full.question[64],
  agency_engagement_11 = column_names_1$full.question[65],
  agency_engagement_12 = column_names_1$full.question[66],
  agency_engagement_13 = column_names_1$full.question[68],
  agency_engagement_14 = column_names_1$full.question[69],
  agency_engagement_15 = column_names_1$full.question[70],
  agency_engagement_16 = column_names_1$full.question[71],
  agency_engagement_17 = column_names_1$full.question[72]
)

names(team_performance_support_df) <- c(
  team_performance_support_1 = column_names_1$full.question[229],
  team_performance_support_1 = column_names_1$full.question[230],
  team_performance_support_1 = column_names_1$full.question[231],
  team_performance_support_1 = column_names_1$full.question[232]
)

names(risk_culture_df) <- c(
  risk_culture_1 = column_names_1$full.question[200],
  risk_culture_1 = column_names_1$full.question[201],
  risk_culture_1 = column_names_1$full.question[203],
  risk_culture_1 = column_names_1$full.question[204],
  risk_culture_1 = column_names_1$full.question[205]
)

names(innovation_df) <- c(
  innovation_1 = column_names_1$full.question[221],
  innovation_1 = column_names_1$full.question[222],
  innovation_1 = column_names_1$full.question[223],
  innovation_1 = column_names_1$full.question[224],
  innovation_1 = column_names_1$full.question[225]
)

names(leadership_engagement_df) <- c(
  leadership_engagement_1 = column_names_1$full.question[47],
  leadership_engagement_2 = column_names_1$full.question[48],
  leadership_engagement_3 = column_names_1$full.question[50],
  leadership_engagement_4 = column_names_1$full.question[51],
  leadership_engagement_5 = column_names_1$full.question[52],
  leadership_engagement_6 = column_names_1$full.question[53],
  leadership_engagement_7 = column_names_1$full.question[54]
)

names(wellbeing_df) <- c(
  wellbeing_1 = column_names_1$full.question[74],
  wellbeing_2 = column_names_1$full.question[87],
  wellbeing_3 = column_names_1$full.question[88],
  wellbeing_4 = column_names_1$full.question[89],
  wellbeing_5 = column_names_1$full.question[90],
  wellbeing_6 = column_names_1$full.question[91],
  wellbeing_7 = column_names_1$full.question[92],
  wellbeing_8 = column_names_1$full.question[93],
  wellbeing_9 = column_names_1$full.question[94],
  wellbeing_10 = column_names_1$full.question[95],
  wellbeing_11 = column_names_1$full.question[96],
  wellbeing_12 = column_names_1$full.question[97],
  wellbeing_13 = column_names_1$full.question[98]
)


names(values_df) <- c(
  values_1 = column_names_1$full.question[236],
  values_1 = column_names_1$full.question[237],
  values_1 = column_names_1$full.question[238]
)

names(aps_reduced$org_size) <- c(org_size = column_names_1$full.question[1])

names(aps_reduced$employee_level) <- c(org_size = column_names_1$full.question[4])

names(aps_reduced$team_performance_rating) <- c(org_size = column_names_1$full.question[226])


# Descriptive statistics step 2: descriptive statistics for questions within each scale (for 11 scales)

# Descriptive statistics step 2
#scale 1: job_engagement analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
job_engagement_likert <- likert(job_engagement_df)
summary(job_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(job_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(job_engagement_likert, group.order = names(job_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(job_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(job_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(job_engagement_df)


# Descriptive statistics step 2
#scale 2: team_engagement analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
team_engagement_likert <- likert(team_engagement_df)
summary(team_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(team_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(team_engagement_likert, group.order = names(team_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(team_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(team_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(team_engagement_df)


# Descriptive statistics step 2
#scale 3: supervisor_engagement analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
supervisor_engagement_likert <- likert(supervisor_engagement_df)
summary(supervisor_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(supervisor_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(supervisor_engagement_likert, group.order = names(supervisor_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(supervisor_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(supervisor_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(supervisor_engagement_df)


# Descriptive statistics step 2
#scale 4: senior_manager_engagement analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
senior_manager_engagement_likert <- likert(senior_manager_engagement_df)
summary(senior_manager_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(senior_manager_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(senior_manager_engagement_likert, group.order = names(senior_manager_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(senior_manager_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(senior_manager_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(senior_manager_engagement_df)


# Descriptive statistics step 2
#scale 5: agency_engagement analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
agency_engagement_likert <- likert(agency_engagement_df)
summary(agency_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(agency_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(agency_engagement_likert, group.order = names(agency_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(agency_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(agency_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(agency_engagement_df)


# Descriptive statistics step 2
#scale 6: team_performance_support analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
team_performance_support_likert <- likert(team_performance_support_df)
summary(team_performance_support_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(team_performance_support_likert, type="bar")

# bar plot ordered by question (not centered)
plot(team_performance_support_likert, group.order = names(team_performance_support_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(team_performance_support_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(team_performance_support_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(team_performance_support_df)


# Descriptive statistics step 2
#scale 7: risk_culture analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
risk_culture_likert <- likert(risk_culture_df)
summary(risk_culture_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(risk_culture_likert, type="bar")

# bar plot ordered by question (not centered)
plot(risk_culture_likert, group.order = names(risk_culture_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(risk_culture_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(risk_culture_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(risk_culture_df)


# Descriptive statistics step 2
#scale 8: innovation analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
innovation_likert <- likert(innovation_df)
summary(innovation_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(innovation_likert, type="bar")

# bar plot ordered by question (not centered)
plot(innovation_likert, group.order = names(innovation_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(innovation_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(innovation_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(innovation_df)


# Descriptive statistics step 2
#scale 9: leadership_engagement analysis

str(leadership_engagement_df)
leadership_engagement_df <- droplevels(leadership_engagement_df)
str(leadership_engagement_df)

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
leadership_engagement_likert <- likert(leadership_engagement_df)
summary(leadership_engagement_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(leadership_engagement_likert, type="bar")

# bar plot ordered by question (not centered)
plot(leadership_engagement_likert, group.order = names(leadership_engagement_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(leadership_engagement_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(leadership_engagement_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(leadership_engagement_df)


# Descriptive statistics step 2
#scale 10: wellbeing analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
wellbeing_likert <- likert(wellbeing_df)
summary(wellbeing_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(wellbeing_likert, type="bar")

# bar plot ordered by question (not centered)
plot(wellbeing_likert, group.order = names(wellbeing_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(wellbeing_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(wellbeing_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(wellbeing_df)


# Descriptive statistics step 2
#scale 11: values analysis

str(values_df)
values_df <- droplevels(values_df)
str(values_df)

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
values_likert <- likert(values_df)
summary(values_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(values_likert, type="bar")

# bar plot ordered by question (not centered)
plot(values_likert, group.order = names(values_df), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(values_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(values_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(values_df)


# Descriptive statistics step 2
# org size analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
org_size <- data.frame(aps_reduced$org_size)
org_size_likert <- likert(org_size)
summary(org_size_likert)

# density plot (treating Likert data like numeric data)
plot(org_size_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(org_size)


# Descriptive statistics step 2
#employee level analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
employee_level <- data.frame(aps_reduced$employee_level)
employee_level_likert <- likert(employee_level)
summary(employee_level_likert)

# density plot (treating Likert data like numeric data)
plot(employee_level_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(employee_level)


# Descriptive statistics step 2
# team performance rating analysis

# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
team_performance_rating <- data.frame(aps_reduced$team_performance_rating)
team_performance_rating_likert <- likert(team_performance_rating)
summary(team_engagement_likert)

# density plot (treating Likert data like numeric data)
plot(team_performance_rating_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(team_performance_rating)


# converting all variables to numerical 
# a- questions making up 11 data frames b- org_size and employee level c- dependent var

# a- converting questions making up 11 data frames to numeric
aps_reduced$job_engagement_1 <- as.numeric(aps_reduced$job_engagement_1)
aps_reduced$job_engagement_2 <- as.numeric(aps_reduced$job_engagement_2)
aps_reduced$job_engagement_3 <- as.numeric(aps_reduced$job_engagement_3)
aps_reduced$job_engagement_4 <- as.numeric(aps_reduced$job_engagement_4)
aps_reduced$job_engagement_5 <- as.numeric(aps_reduced$job_engagement_5)
aps_reduced$job_engagement_6 <- as.numeric(aps_reduced$job_engagement_6)
aps_reduced$job_engagement_7 <- as.numeric(aps_reduced$job_engagement_7)
aps_reduced$job_engagement_8 <- as.numeric(aps_reduced$job_engagement_8)
aps_reduced$job_engagement_9 <- as.numeric(aps_reduced$job_engagement_9)
aps_reduced$job_engagement_10 <- as.numeric(aps_reduced$job_engagement_10)

aps_reduced$team_engagement_1 <- as.numeric(aps_reduced$team_engagement_1)
aps_reduced$team_engagement_2 <- as.numeric(aps_reduced$team_engagement_2)
aps_reduced$team_engagement_3 <- as.numeric(aps_reduced$team_engagement_3)
aps_reduced$team_engagement_4 <- as.numeric(aps_reduced$team_engagement_4)

aps_reduced$supervisor_engagement_1 <- as.numeric(aps_reduced$supervisor_engagement_1)
aps_reduced$supervisor_engagement_2 <- as.numeric(aps_reduced$supervisor_engagement_2)
aps_reduced$supervisor_engagement_3 <- as.numeric(aps_reduced$supervisor_engagement_3)
aps_reduced$supervisor_engagement_4 <- as.numeric(aps_reduced$supervisor_engagement_4)
aps_reduced$supervisor_engagement_5 <- as.numeric(aps_reduced$supervisor_engagement_5)
aps_reduced$supervisor_engagement_6 <- as.numeric(aps_reduced$supervisor_engagement_6)
aps_reduced$supervisor_engagement_7 <- as.numeric(aps_reduced$supervisor_engagement_7)
aps_reduced$supervisor_engagement_8 <- as.numeric(aps_reduced$supervisor_engagement_8)
aps_reduced$supervisor_engagement_9 <- as.numeric(aps_reduced$supervisor_engagement_9)
aps_reduced$supervisor_engagement_10 <- as.numeric(aps_reduced$supervisor_engagement_10)
aps_reduced$supervisor_engagement_11 <- as.numeric(aps_reduced$supervisor_engagement_11)

aps_reduced$senior_manager_engagement_1 <- as.numeric(aps_reduced$senior_manager_engagement_1)
aps_reduced$senior_manager_engagement_2 <- as.numeric(aps_reduced$senior_manager_engagement_2)
aps_reduced$senior_manager_engagement_3 <- as.numeric(aps_reduced$senior_manager_engagement_3)
aps_reduced$senior_manager_engagement_4 <- as.numeric(aps_reduced$senior_manager_engagement_4)
aps_reduced$senior_manager_engagement_5 <- as.numeric(aps_reduced$senior_manager_engagement_5)
aps_reduced$senior_manager_engagement_6 <- as.numeric(aps_reduced$senior_manager_engagement_6)
aps_reduced$senior_manager_engagement_7 <- as.numeric(aps_reduced$senior_manager_engagement_7)
aps_reduced$senior_manager_engagement_8 <- as.numeric(aps_reduced$senior_manager_engagement_8)
aps_reduced$senior_manager_engagement_9 <- as.numeric(aps_reduced$senior_manager_engagement_9)
aps_reduced$senior_manager_engagement_10 <- as.numeric(aps_reduced$senior_manager_engagement_10)
aps_reduced$senior_manager_engagement_11 <- as.numeric(aps_reduced$senior_manager_engagement_11)
aps_reduced$senior_manager_engagement_12 <- as.numeric(aps_reduced$senior_manager_engagement_12)

aps_reduced$agency_engagement_1 <- as.numeric(aps_reduced$agency_engagement_1)
aps_reduced$agency_engagement_2 <- as.numeric(aps_reduced$agency_engagement_2)
aps_reduced$agency_engagement_3 <- as.numeric(aps_reduced$agency_engagement_3)
aps_reduced$agency_engagement_4 <- as.numeric(aps_reduced$agency_engagement_4)
aps_reduced$agency_engagement_5 <- as.numeric(aps_reduced$agency_engagement_5)
aps_reduced$agency_engagement_6 <- as.numeric(aps_reduced$agency_engagement_6)
aps_reduced$agency_engagement_7 <- as.numeric(aps_reduced$agency_engagement_7)
aps_reduced$agency_engagement_8 <- as.numeric(aps_reduced$agency_engagement_8)
aps_reduced$agency_engagement_9 <- as.numeric(aps_reduced$agency_engagement_9)
aps_reduced$agency_engagement_10 <- as.numeric(aps_reduced$agency_engagement_10)
aps_reduced$agency_engagement_11 <- as.numeric(aps_reduced$agency_engagement_11)
aps_reduced$agency_engagement_12 <- as.numeric(aps_reduced$agency_engagement_12)
aps_reduced$agency_engagement_13 <- as.numeric(aps_reduced$agency_engagement_13)
aps_reduced$agency_engagement_14 <- as.numeric(aps_reduced$agency_engagement_14)
aps_reduced$agency_engagement_15 <- as.numeric(aps_reduced$agency_engagement_15)
aps_reduced$agency_engagement_16 <- as.numeric(aps_reduced$agency_engagement_16)
aps_reduced$agency_engagement_17 <- as.numeric(aps_reduced$agency_engagement_17)

aps_reduced$team_performance_support_1 <- as.numeric(aps_reduced$team_performance_support_1)
aps_reduced$team_performance_support_2 <- as.numeric(aps_reduced$team_performance_support_2)
aps_reduced$team_performance_support_3 <- as.numeric(aps_reduced$team_performance_support_3)
aps_reduced$team_performance_support_4 <- as.numeric(aps_reduced$team_performance_support_4)

aps_reduced$risk_culture_1 <- as.numeric(aps_reduced$risk_culture_1)
aps_reduced$risk_culture_2 <- as.numeric(aps_reduced$risk_culture_2)
aps_reduced$risk_culture_3 <- as.numeric(aps_reduced$risk_culture_3)
aps_reduced$risk_culture_4 <- as.numeric(aps_reduced$risk_culture_4)
aps_reduced$risk_culture_5 <- as.numeric(aps_reduced$risk_culture_5)

aps_reduced$innovation_1 <- as.numeric(aps_reduced$innovation_1)
aps_reduced$innovation_2 <- as.numeric(aps_reduced$innovation_2)
aps_reduced$innovation_3 <- as.numeric(aps_reduced$innovation_3)
aps_reduced$innovation_4 <- as.numeric(aps_reduced$innovation_4)
aps_reduced$innovation_5 <- as.numeric(aps_reduced$innovation_5)

aps_reduced$leadership_engagement_1 <- as.numeric(aps_reduced$leadership_engagement_1)
aps_reduced$leadership_engagement_2 <- as.numeric(aps_reduced$leadership_engagement_2)
aps_reduced$leadership_engagement_3 <- as.numeric(aps_reduced$leadership_engagement_3)
aps_reduced$leadership_engagement_4 <- as.numeric(aps_reduced$leadership_engagement_4)
aps_reduced$leadership_engagement_5 <- as.numeric(aps_reduced$leadership_engagement_5)
aps_reduced$leadership_engagement_6 <- as.numeric(aps_reduced$leadership_engagement_6)
aps_reduced$leadership_engagement_7 <- as.numeric(aps_reduced$leadership_engagement_7)

aps_reduced$wellbeing_1 <- as.numeric(aps_reduced$wellbeing_1)
aps_reduced$wellbeing_2 <- as.numeric(aps_reduced$wellbeing_2)
aps_reduced$wellbeing_3 <- as.numeric(aps_reduced$wellbeing_3)
aps_reduced$wellbeing_4 <- as.numeric(aps_reduced$wellbeing_4)
aps_reduced$wellbeing_5 <- as.numeric(aps_reduced$wellbeing_5)
aps_reduced$wellbeing_6 <- as.numeric(aps_reduced$wellbeing_6)
aps_reduced$wellbeing_7 <- as.numeric(aps_reduced$wellbeing_7)
aps_reduced$wellbeing_8 <- as.numeric(aps_reduced$wellbeing_8)
aps_reduced$wellbeing_9 <- as.numeric(aps_reduced$wellbeing_9)
aps_reduced$wellbeing_10 <- as.numeric(aps_reduced$wellbeing_10)
aps_reduced$wellbeing_11 <- as.numeric(aps_reduced$wellbeing_11)
aps_reduced$wellbeing_12 <- as.numeric(aps_reduced$wellbeing_12)
aps_reduced$wellbeing_13 <- as.numeric(aps_reduced$wellbeing_13)

aps_reduced$values_1 <- as.numeric(aps_reduced$values_1)
aps_reduced$values_2 <- as.numeric(aps_reduced$values_2)
aps_reduced$values_3 <- as.numeric(aps_reduced$values_3)

# b- converting org_size and employee level to numeric - to perform correlation analysis
aps_reduced$org_size <- as.numeric(aps_reduced$org_size)
aps_reduced$employee_level <- as.numeric(aps_reduced$employee_level)

# c- converting dependent var (team_performance_rating) to numeric
aps_reduced$team_performance_rating <- as.numeric(aps_reduced$team_performance_rating)


# checking that changes were successful
summary(aps_reduced)
str(aps_reduced)


# developing the scales - using the 11 data frames created earlier 

job_engagement_df <- data.frame(aps_reduced$job_engagement_1, aps_reduced$job_engagement_2, aps_reduced$job_engagement_3, aps_reduced$job_engagement_4, aps_reduced$job_engagement_5, aps_reduced$job_engagement_6, aps_reduced$job_engagement_7, aps_reduced$job_engagement_8, aps_reduced$job_engagement_9, aps_reduced$job_engagement_10)
job_engagement <- rowMeans(job_engagement_df)
summary(job_engagement)

team_engagement_df <- data.frame(aps_reduced$team_engagement_1, aps_reduced$team_engagement_2, aps_reduced$team_engagement_3, aps_reduced$team_engagement_4)
team_engagement <- rowMeans(team_engagement_df)
summary(team_engagement)

supervisor_engagement_df <- data.frame(aps_reduced$supervisor_engagement_1, aps_reduced$supervisor_engagement_2, aps_reduced$supervisor_engagement_3, aps_reduced$supervisor_engagement_4, aps_reduced$supervisor_engagement_5, aps_reduced$supervisor_engagement_6, aps_reduced$supervisor_engagement_7, aps_reduced$supervisor_engagement_8, aps_reduced$supervisor_engagement_9, aps_reduced$supervisor_engagement_10, aps_reduced$supervisor_engagement_11)
supervisor_engagement <- rowMeans(supervisor_engagement_df)
summary(supervisor_engagement)

senior_manager_engagement_df <- data.frame(aps_reduced$senior_manager_engagement_1, aps_reduced$senior_manager_engagement_2, aps_reduced$senior_manager_engagement_3, aps_reduced$senior_manager_engagement_4, aps_reduced$senior_manager_engagement_5, aps_reduced$senior_manager_engagement_6, aps_reduced$senior_manager_engagement_7, aps_reduced$senior_manager_engagement_8, aps_reduced$senior_manager_engagement_9, aps_reduced$senior_manager_engagement_10, aps_reduced$senior_manager_engagement_11, aps_reduced$senior_manager_engagement_12)
senior_manager_engagement <- rowMeans(senior_manager_engagement_df)
summary(senior_manager_engagement)

agency_engagement_df <- data.frame(aps_reduced$agency_engagement_1, aps_reduced$agency_engagement_2, aps_reduced$agency_engagement_3, aps_reduced$agency_engagement_4, aps_reduced$agency_engagement_5, aps_reduced$agency_engagement_6, aps_reduced$agency_engagement_7, aps_reduced$agency_engagement_8, aps_reduced$agency_engagement_9, aps_reduced$agency_engagement_10, aps_reduced$agency_engagement_11, aps_reduced$agency_engagement_12, aps_reduced$agency_engagement_13, aps_reduced$agency_engagement_14, aps_reduced$agency_engagement_15, aps_reduced$agency_engagement_16, aps_reduced$agency_engagement_17)
agency_engagement <- rowMeans(agency_engagement_df)
summary(agency_engagement)

team_performance_support_df <- data.frame(aps_reduced$team_performance_support_1, aps_reduced$team_performance_support_2, aps_reduced$team_performance_support_3, aps_reduced$team_performance_support_4)
team_performance_support <- rowMeans(team_performance_support_df)
summary(team_performance_support)

risk_culture_df <- data.frame(aps_reduced$risk_culture_1, aps_reduced$risk_culture_2, aps_reduced$risk_culture_3, aps_reduced$risk_culture_4, aps_reduced$risk_culture_5)
risk_culture <- rowMeans(risk_culture_df)
summary(risk_culture)

innovation_df <- data.frame(aps_reduced$innovation_1, aps_reduced$innovation_2, aps_reduced$innovation_3, aps_reduced$innovation_4, aps_reduced$innovation_5)
innovation <- rowMeans(innovation_df)
summary(innovation)

leadership_engagement_df <- data.frame(aps_reduced$leadership_engagement_1, aps_reduced$leadership_engagement_2, aps_reduced$leadership_engagement_3, aps_reduced$leadership_engagement_4, aps_reduced$leadership_engagement_5, aps_reduced$leadership_engagement_6, aps_reduced$leadership_engagement_7)
leadership_engagement <- rowMeans(leadership_engagement_df)
summary(leadership_engagement)

wellbeing_df <- data.frame(aps_reduced$wellbeing_1, aps_reduced$wellbeing_2, aps_reduced$wellbeing_3, aps_reduced$wellbeing_4, aps_reduced$wellbeing_5, aps_reduced$wellbeing_6, aps_reduced$wellbeing_7, aps_reduced$wellbeing_8, aps_reduced$wellbeing_9, aps_reduced$wellbeing_10, aps_reduced$wellbeing_11, aps_reduced$wellbeing_12, aps_reduced$wellbeing_13)
wellbeing <- rowMeans(wellbeing_df)
summary(wellbeing)

values_df <- data.frame(aps_reduced$values_1, aps_reduced$values_2, aps_reduced$values_3)
values <- rowMeans(values_df)
summary(values)


# new data frame with the 11 new scales, org_size, employee_level and dependant var
# this data frame (aps_with_scales) will be used for (numeric) regression analysis

# assigning the below 3 variables to unique variables - separate from aps_reduced data frame
org_size <- aps_reduced$org_size
employee_level <- aps_reduced$employee_level
team_performance_rating <- aps_reduced$team_performance_rating

aps_with_scales <- data.frame(org_size, employee_level, job_engagement, team_engagement, supervisor_engagement, senior_manager_engagement, agency_engagement, team_performance_support, risk_culture, innovation, leadership_engagement, wellbeing, values, team_performance_rating)
str(aps_with_scales)


# Visualizing 11 new scales vs. dependent variable

# step 1: converting 11 new scales and dep var to factors for use with likert functions
job_engagement_f <- as.factor(round(job_engagement))
team_engagement_f <- as.factor(round(team_engagement))
supervisor_engagement_f <- as.factor(round(supervisor_engagement))
senior_manager_engagement_f <- as.factor(round(senior_manager_engagement))
agency_engagement_f <- as.factor(round(agency_engagement))
team_performance_support_f <- as.factor(round(team_performance_support))
risk_culture_f <- as.factor(round(risk_culture))
innovation_f <- as.factor(round(innovation))
leadership_engagement_f <- as.factor(round(leadership_engagement))
wellbeing_f <- as.factor(round(wellbeing))
values_f <- as.factor(round(values))

# transforming team_performance_rating to 5 point scale to visualize vs. other questions 
# using likert functions 
team_performance_rating_5scale <- mgsub(aps_reduced$team_performance_rating, c(1,2,3,4,5,6,7,8,9,10), c(1,1,2,2,3,3,4,4,5,5))
str(team_performance_rating_5scale)
team_performance_rating_5scale <- as.numeric(team_performance_rating_5scale)
str(team_performance_rating_5scale)
team_performance_rating_5scale_f <- as.factor(team_performance_rating_5scale)

# Data frame to be used for likert visualizations 
aps_with_scales_factors_excl_depVar <- data.frame(job_engagement_f, team_engagement_f, supervisor_engagement_f, senior_manager_engagement_f, agency_engagement_f, team_performance_support_f, risk_culture_f, innovation_f, leadership_engagement_f, wellbeing_f, values_f)

aps_with_scales_factors <- data.frame(job_engagement_f, team_engagement_f, supervisor_engagement_f, senior_manager_engagement_f, agency_engagement_f, team_performance_support_f, risk_culture_f, innovation_f, leadership_engagement_f, wellbeing_f, values_f, team_performance_rating_5scale_f)
str(aps_with_scales_factors)


# summary of low scores (strongly disagree + disagree), neutral (neither agree nor disagree)
# high (strongly agree + agree) and mean and sd
aps_with_scales_factors_likert <- likert(aps_with_scales_factors)
summary(aps_with_scales_factors_likert)

# centered bar plot showing the percent responses for each question (order from most to least agreement)
plot(aps_with_scales_factors_likert, type="bar")

# bar plot ordered by question (not centered)
plot(aps_with_scales_factors_likert, group.order = names(aps_with_scales_factors), centered = FALSE) + theme(text = element_text(size = 14))

# heat plot (mean, standard deviation, and percent selection of responses for each question)
plot(aps_with_scales_factors_likert, 
     type="heat",
     low.color = "white", 
     high.color = "blue",
     text.color = "black", 
     text.size = 4, 
     wrap = 50)

# density plot (treating Likert data like numeric data)
plot(aps_with_scales_factors_likert,
     type="density",
     facet = TRUE, 
     bw = 0.5)

# descriptive statistics (mean, sd, median, skewness)
psych::describe(aps_with_scales_factors)


# Correlation analysis

# Pearson correlation (11 scales, org_size, employee_level and dep var)
Pcorrelation_and_significance <- Hmisc::rcorr(as.matrix(aps_with_scales), type = "pearson")
Pcorrelation_and_significance

Pcorr_aps_with_scales <- cor(aps_with_scales, method = "pearson")
Pcorr_aps_with_scales

corrgram(Pcorr_aps_with_scales, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation analysis - Pearson")

corrplot(Pcorr_aps_with_scales, method="pie", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 40, tl.cex = 0.4)

corrplot(Pcorr_aps_with_scales, method="number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 40, tl.cex = 0.4)


# Spearman correlation (11 scales, org_size, employee_level and dep var)

Scorrelation_and_significance <- Hmisc::rcorr(as.matrix(aps_with_scales), type = "spearman")  
Scorrelation_and_significance

Scorr_aps_with_scales <- cor(aps_with_scales, method = "spearman")
Scorr_aps_with_scales

corrgram(Scorr_aps_with_scales, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlation analysis - Spearman")

corrplot(Scorr_aps_with_scales, method="pie", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 40, tl.cex = 0.4)

corrplot(Scorr_aps_with_scales, method="number", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 40, tl.cex = 0.4)


# PCA analysis
aps_model_pca <- prcomp(aps_with_scales)
summary(aps_model_pca)
plot(aps_model_pca, type = "l")

aps_model_pca$rotation
varimax(aps_model_pca$rotation)
head(aps_model_pca$x)


# importing and formatting 2019 test data
aps_with_scales_lr_test <- read.csv("/Users/ibrahimibrahim/Documents/Ryerson/820/data set/aps_with_scales_lr_test.csv",stringsAsFactors = FALSE)
aps_with_scales_factors_ordReg_test <- read.csv("/Users/ibrahimibrahim/Documents/Ryerson/820/data set/aps_with_scales_factors_ordReg_test.csv",stringsAsFactors = TRUE)

str(aps_with_scales_lr_test)

aps_with_scales_lr_test <- aps_with_scales_lr_test[-1]
str(aps_with_scales_lr_test)
aps_with_scales_lr_test$team_performance_rating <- as.numeric(aps_with_scales_lr_test$team_performance_rating)
str(aps_with_scales_lr_test)
aps_with_scales_lr_test$org_size <- as.factor(aps_with_scales_lr_test$org_size)
aps_with_scales_lr_test$employee_level <- as.factor(aps_with_scales_lr_test$employee_level)

str(aps_with_scales_lr_test)


str(aps_with_scales_factors_ordReg_test)

aps_with_scales_factors_ordReg_test <- aps_with_scales_factors_ordReg_test[-1]
str(aps_with_scales_factors_ordReg_test) 

aps_with_scales_factors_ordReg_test$org_size <- as.factor(aps_with_scales_factors_ordReg_test$org_size)
aps_with_scales_factors_ordReg_test$employee_level <- as.factor(aps_with_scales_factors_ordReg_test$employee_level)
aps_with_scales_factors_ordReg_test$job_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$job_engagement_f)
aps_with_scales_factors_ordReg_test$team_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$team_engagement_f)
aps_with_scales_factors_ordReg_test$supervisor_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$supervisor_engagement_f)
aps_with_scales_factors_ordReg_test$senior_manager_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$senior_manager_engagement_f)
aps_with_scales_factors_ordReg_test$agency_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$agency_engagement_f)
aps_with_scales_factors_ordReg_test$team_performance_support_f <- as.factor(aps_with_scales_factors_ordReg_test$team_performance_support_f)
aps_with_scales_factors_ordReg_test$risk_culture_f <- as.factor(aps_with_scales_factors_ordReg_test$risk_culture_f)
aps_with_scales_factors_ordReg_test$innovation_f <- as.factor(aps_with_scales_factors_ordReg_test$innovation_f)
aps_with_scales_factors_ordReg_test$leadership_engagement_f <- as.factor(aps_with_scales_factors_ordReg_test$leadership_engagement_f)
aps_with_scales_factors_ordReg_test$wellbeing_f <- as.factor(aps_with_scales_factors_ordReg_test$wellbeing_f)
aps_with_scales_factors_ordReg_test$values_f <- as.factor(aps_with_scales_factors_ordReg_test$values_f)
aps_with_scales_factors_ordReg_test$team_performance_rating_ordReg <- as.factor(aps_with_scales_factors_ordReg_test$team_performance_rating_ordReg)

str(aps_with_scales_factors_ordReg_test)


# Logistic regression model

# preparing the 2 factor levels: org_size and employee_level

org_size <- as.factor(aps_reduced$org_size)
employee_level <- as.factor(aps_reduced$employee_level)

# transforming the values of the dependent variable to be binary

table(aps_reduced$team_performance_rating)
team_performance_rating_binary <- mgsub(aps_reduced$team_performance_rating, c(1,2,3,4,5,6,7,8,9,10), c(0,0,0,0,0,0,0,1,1,1))
team_performance_rating_binary <- as.numeric(team_performance_rating_binary)
table(team_performance_rating_binary)
str(team_performance_rating_binary)
summary(team_performance_rating_binary)

# new data frame with the new scales plus the 2 ordinal variables - for logistic regression
aps_with_scales_lr <- data.frame(org_size, employee_level, job_engagement, team_engagement, supervisor_engagement, senior_manager_engagement, agency_engagement, team_performance_support, risk_culture, innovation, leadership_engagement, wellbeing, values, team_performance_rating_binary)
str(aps_with_scales_lr)

train <- aps_with_scales_lr
test <- aps_with_scales_lr_test


lrmodel <- glm(train$team_performance_rating_binary~.,data =  train, family = "binomial") 
lrmodel
summary(lrmodel)
aov(lrmodel)

predicted <- predict(lrmodel, test, type="response")
str(predicted)
summary(predicted)
predicted_class <- ifelse(predicted>=0.562, 1, 0) 

cm <- table(actual = test$team_performance_rating_binary, predicted = predicted_class)
cm

accuracy <- sum(diag(cm))/nrow(test)
accuracy

accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2])
accuracy

sensitivity <- cm[2,2] / (cm[2,2] + cm[2,1])
sensitivity
# SN = TP / (TP + FN)
#quantifies how many diagnosis are predicted accurately

specificity <- cm[1,1] / (cm[1,1] + cm[1,2])
specificity
# SP = TN / (TN + FP)
#measures the proportion of actual negatives that are predicted correctly 

accuracy
sensitivity
specificity


# Ordinal logistic regression

# preparing dep var
team_performance_rating_ordReg <- mgsub(aps_reduced$team_performance_rating, c(1,2,3,4,5,6,7,8,9,10), c(0,0,0,0,1,1,1,2,2,2))
team_performance_rating_ordReg <- as.numeric(team_performance_rating_ordReg)
table(team_performance_rating_ordReg)
team_performance_rating_ordReg <- as.factor(team_performance_rating_ordReg)
str(team_performance_rating_ordReg)

aps_with_scales_factors_ordReg <- data.frame(org_size, employee_level, aps_with_scales_factors_excl_depVar, team_performance_rating_ordReg)
str(aps_with_scales_factors_ordReg)

train <- aps_with_scales_factors_ordReg
test <- aps_with_scales_factors_ordReg_test


ordered_logistic_regression <- MASS::polr(formula = team_performance_rating_ordReg~., data = train, Hess = TRUE)
summary(ordered_logistic_regression)
coeffs <- coef(summary(ordered_logistic_regression))
p <- pnorm(abs(coeffs[,"t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))

predicted <- predict(ordered_logistic_regression, test, type = "class")
head(predicted)
str(predicted)
summary(predicted)

cm <- table(actual = test$team_performance_rating_ordReg, predicted = predicted)
cm

accuracy <- sum(diag(cm))/nrow(test)
accuracy

# sensitivity: (TP) / (TP + FN)
sensitivity_low <- cm[1,1] / (cm[1,1] + cm[1,2] + cm[1,3])
sensitivity_low                       

sensitivity_medium <- cm[2,2] / (cm[2,2] + cm[2,1] + cm[2,3])
sensitivity_medium                    

sensitivity_high <- cm[3,3] / (cm[3,3] + cm[3,1] + cm[3,2])
sensitivity_high                    

sensitivity <- c(low = sensitivity_low, medium = sensitivity_medium, high = sensitivity_high)
sensitivity

# specificity: (TN) / (TN + FP)
specificity_low <- (cm[2,2] + cm[2,3] + cm[3,2] + cm[3,3]) / (cm[2,2] + cm[2,3] + cm[3,2] + cm[3,3] + cm[2,1] + cm[3,1])
specificity_low

specificity_medium <- (cm[1,1] + cm[1,3] + cm[3,1] + cm[3,3]) / (cm[1,1] + cm[1,3] + cm[3,1] + cm[3,3] + cm[1,2] + cm[3,2])
specificity_medium

specificity_high <- (cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2]) / (cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2] + cm[2,3] + cm[1,3])
specificity_high

specificity <- c(low = specificity_low, medium = specificity_medium, high = specificity_high)
specificity


# Decision tree using logitic regression aps_with_scales_lr data set

#simplified decision tree
aps_decision_tree <- ctree(
  team_performance_rating_binary ~., 
  data = aps_with_scales_lr,
  control = ctree_control(maxdepth = 4)
)
print(aps_decision_tree)
plot(aps_decision_tree, type="simple")

#full decision tree
aps_decision_tree <- ctree(
  team_performance_rating_binary ~., 
  data = aps_with_scales_lr)
print(aps_decision_tree)
plot(aps_decision_tree, type="simple")


aps_decision_tree_prediction <- predict(aps_decision_tree, aps_with_scales_lr_test) 
head(aps_decision_tree_prediction)
str(aps_decision_tree_prediction)
summary(aps_decision_tree_prediction)

predicted_class <- ifelse(aps_decision_tree_prediction>=0.56, 1, 0) 

cm <- table(actual = aps_with_scales_lr_test$team_performance_rating_binary, predicted = predicted_class)
cm

accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2])
accuracy

sensitivity <- cm[2,2] / (cm[2,2] + cm[2,1])
sensitivity

specificity <- cm[1,1] / (cm[1,1] + cm[1,2])
specificity

accuracy
sensitivity
specificity


#k-means clustering - analyzing APS data using binary dependent variable scale
aps_with_scales_k2 <- data.frame(job_engagement, team_engagement, supervisor_engagement, senior_manager_engagement, agency_engagement, team_performance_support, risk_culture, innovation, leadership_engagement, wellbeing, values, team_performance_rating_binary)

aps_with_scales_kmeans <- aps_with_scales_k2[-aps_with_scales_k2$team_performance_rating_binary]
aps_kmeans<- kmeans(aps_with_scales_kmeans, 6) 
aps_kmeans
aps_kmeans$centers
aps_kmeans_centres <- as.data.frame(aps_kmeans$centers)
aps_kmeans_centres
table(aps_with_scales_k2$team_performance_rating_binary,aps_kmeans$cluster)

aps_kmeans_centres %>%
  gather("Type", "Value",-team_performance_rating_binary) %>%
  ggplot(aes(team_performance_rating_binary, Value, fill = Type)) +
  geom_col(position = "dodge") +
  theme_bw()+
  facet_wrap(~team_performance_rating_binary,scales = "free_x")