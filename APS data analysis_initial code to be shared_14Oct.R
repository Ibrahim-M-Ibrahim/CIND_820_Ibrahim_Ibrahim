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


# overview
head(aps)
tail(aps)
str(aps)
summary(aps)


# renaming variables and creating a new data frame with reduced number of variables

# reading new column names from a csv file
column_names_1 <- read.csv("/Users/ibrahimibrahim/Documents/Ryerson/820/data set/column_names_2018.csv",stringsAsFactors = FALSE, header = TRUE)

aps_new_column_names <- aps
names(aps_new_column_names)[1:301] <- c(column_names_1$new.column.name)
aps_reduced <- select(aps_new_column_names, -contains("disregarded"))


# Skippig questions was allowed in survey, so will check for missing values
sum(is.na(aps_reduced))        # a total of 568,284 skipped questions

aps_reduced$number_skipped_questions <- rowSums(is.na(aps_reduced))
table(aps_reduced$number_skipped_questions)

number_skipped_questions_above_0 <- aps_reduced$number_skipped_questions[aps_reduced$number_skipped_questions>0]
table(number_skipped_questions_above_0)
sum(table(number_skipped_questions_above_0))       # a total of 22,748 respondents who skipped questions
prop.table(table(number_skipped_questions_above_0))*100
barplot(prop.table(table(number_skipped_questions_above_0))*100)


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

# reformat_variable_group6: this group was cancelled as variable was dropped
# re-order factor levels, rename factor levels:
# this function will be applied to the following variables:
# performance_barriers

#unique(aps_reduced$performance_barriers_1)

#reformat_variable_group6 <- function(variable_to_be_used){
  # re-order factor levels and rename factor levels 
#  variable_to_be_used <- fct_relevel(variable_to_be_used, c("Not at all", "Very little", "Somewhat", "To a great extent", "To a very great extent"))
  
#  levels(variable_to_be_used) <- list("1" = "Not at all", "2" = "Very little", "3" = "Somewhat", "4" = "To a great extent", "5" = "To a very great extent")
  
#  return(variable_to_be_used)
#}
#end reformat_variable_group6

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
