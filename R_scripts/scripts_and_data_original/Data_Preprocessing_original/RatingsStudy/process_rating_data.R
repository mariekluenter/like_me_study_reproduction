######
# Script for processing of raw ratings data

# Load packages

library(tidyr)
library(dplyr)
library(hms)
library(readr)
library(stringr)
library(ggplot2)
library(reshape2)
library(rjson)
library(jsonlite)
library(Matrix)
library(lme4)
library(psych)
library(lmtest)
library(sjPlot)
library(wesanderson)
library(knitr)
library(rmarkdown)
library(MuMIn)
library(pwr)

#####
# Import data
files_conjoined <- list.files(path = "./prolific_data_conjoined", pattern = "\\.csv$", full.names = T)


files_transitive <- list.files(path = "./prolific_data", pattern = "\\.csv$", full.names = T)

raw_data_conjoined <- sapply(files_conjoined, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

raw_data_transitive <- sapply(files_transitive, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

raw_data_transitive$condition <- "transitive"

raw_data <- rbind(raw_data_conjoined, raw_data_transitive)

coding = read_csv("coding.csv") #load file with regular/reverse coding info

test = raw_data %>% # Read in data
  dplyr::select(id,rt,stimulus,condition,trial_type,trial_index,subject,list,ppt_gender,sentence_type,test_part,verb,responses,left_part,right_part)%>%
  fill(stimulus, .direction = "down")%>% #make sure rating scale data has sentence marker
  dplyr::filter(trial_type == "survey-likert-extra-labels")%>%
  mutate(responses = gsub('{\"Q0\":','',
                          x=responses
                          ,fixed=T),
         responses = gsub('}','',
                          x=responses
                          ,fixed=T))%>%
  dplyr::filter(!verb %in% c("chase", "drive", "teach", "remind", "carry", "pose", "feed", "hire"))%>%#remove catch questions
  left_join(coding, by = c("list","verb"))

test$first_mention_gender <- gsub('[[:digit:]]+', '', x=test$first_mention_gender)

test$event <- test$verb
test$event_number <- test$verb
test$event <- gsub('[[:digit:]]+', '', x=test$event)
test$event_number <- gsub('\\D+','', x=test$event_number)
test$trial_condition <- ifelse(test$event_number == 1 | test$event_number == 2, "same_gender", "different_gender")

#handle reverse-coded trials


library(dplyr)

test <- test %>% 
  mutate(responses_new = case_when(
    responses == 0 ~ 1, responses == 1 ~ 2, responses == 2 ~ 3, responses == 3 ~ 4,responses == 4 ~ 5, responses == 5 ~ 6, responses == 6 ~ 7))%>%
  mutate(responses_new_reversed = case_when(
    responses_new == 1 ~ 7, responses_new == 2 ~ 6, responses_new == 3 ~ 5, responses_new == 4 ~ 4,responses_new == 5 ~ 3, responses_new == 6 ~ 2, responses_new == 7 ~ 1))%>%
  mutate(score = ifelse(
    coding == "reverse",responses_new_reversed,responses_new))%>%
  mutate(score = case_when(
    score == 1 ~ 3, score == 2 ~ 2, score == 3 ~ 1, score == 4 ~ 0,score == 5 ~ -1, score == 6 ~ -2, score == 7 ~ -3))


#####
# Process catch questions


catch = raw_data %>% # Read in data
  dplyr::select(id,rt,stimulus,condition,trial_type,trial_index,subject,list,ppt_gender,sentence_type,test_part,verb,responses,left_part,right_part)%>%
  fill(stimulus, .direction = "down")%>% #make sure rating scale data has sentence marker
  dplyr::filter(trial_type == "survey-likert-extra-labels")%>%
  mutate(responses = gsub('{\"Q0\":','',
                          x=responses
                          ,fixed=T),
         responses = gsub('}','',
                          x=responses
                          ,fixed=T))%>%
  dplyr::filter(verb %in% c("chase", "drive", "teach", "remind", "carry", "pose", "feed", "hire"))

library(car)

catch$score <- catch$responses
#recode scores so it's 1 - 7, instead of 0 - 6

catch <- catch %>% 
  mutate(score = case_when(
    responses == 0 ~ 1, responses == 1 ~ 2, responses == 2 ~ 3, responses == 3 ~ 4,responses == 4 ~ 5, responses == 5 ~ 6, responses == 6 ~ 7))


catch = catch%>%
  dplyr::mutate(catch_correct = case_when((
    score > 3 & test_part == "rating_causation") ~ "correct",
    (score < 5 & test_part == "rating_volition") ~ "correct",
    (score < 5 & test_part == "rating_causation") ~ "incorrect",
    (score > 3 & test_part == "rating_volition") ~ "incorrect"))

table(catch$catch_correct,catch$list)

#####
# Exclude based on catch questions


caught_out <- subset(catch, catch_correct == "incorrect", na.rm = T) #df for all bad trials. Use to clean data


catch_exclusions = caught_out %>%
  dplyr::group_by(subject,list,ppt_gender,condition) %>%
  dplyr::summarise(n_excl = length(catch_correct)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 2) #filter out ppts who got over 2 catch questions wrong
catch_exclusions

#Comment back in after collecting data
newdata <- subset(test, !subject %in% catch_exclusions$subject) #remove entire subjects who fell below threshold for inclusion (total = 9)

######
# Process repeated responses

test_rep = raw_data %>% # Read in data
  dplyr::select(id,rt,stimulus,trial_type,trial_index,subject,list,condition,ppt_gender,sentence_type,test_part,verb,responses,left_part,right_part)%>%
  fill(stimulus, .direction = "down")%>% #make sure rating scale data has sentence marker
  dplyr::filter(trial_type == "survey-likert-extra-labels")%>%
  mutate(responses = gsub('{\"Q0\":','',
                          x=responses
                          ,fixed=T),
         responses = gsub('}','',
                          x=responses
                          ,fixed=T))%>%
  left_join(coding, by = c("list","verb"))

test_rep$first_mention_gender <- gsub('[[:digit:]]+', '', x=test_rep$first_mention_gender)

test_rep$event <- test_rep$verb
test_rep$event_number <- test_rep$verb
test_rep$event <- gsub('[[:digit:]]+', '', x=test_rep$event)
test_rep$event_number <- gsub('\\D+','', x=test_rep$event_number)
test_rep$trial_condition <- ifelse(test_rep$event_number == 1 | test_rep$event_number == 2, "same_gender", "different_gender")

#handle reverse-coded trials

#test$score <- as.numeric(as.character(test$score), na.rm=T)

test_rep <- test_rep %>% 
  dplyr::select(subject,condition,list,ppt_gender,verb,responses)

test_rep$responses <- as.numeric(as.character(test_rep$responses), na.rm=T)
is.numeric(test_rep$responses)


#Now do this for participants who gave the same exact rating on over 80% of trials (76 trials out of 96 --- 80 critical, 16 catch)

#First look for the ppts who answered "4" (0) over 76 times


repetitive0 <- test_rep%>%
  dplyr::group_by(subject,condition,list,ppt_gender,responses) %>%
  dplyr::filter(responses == 0)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76) #filter out ppts who selected 0 over 80% of the time
repetitive0

repetitive1 <- test_rep%>%
  dplyr::group_by(subject,list,ppt_gender,responses) %>%
  dplyr::filter(responses == 1)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76)
repetitive1

#There are none 

repetitive2 <- test%>%
  dplyr::group_by(subject,list,ppt_gender,responses) %>%
  dplyr::filter(responses == 2)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76)
repetitive2
#There are none

repetitive3 <- test%>%
  dplyr::group_by(subject,list,condition,responses) %>%
  dplyr::filter(responses == 3)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76) #filter out ppts who got over 2 catch questions wrong
repetitive3

#newdata <- subset(newdata, !subject %in% repetitive3$subject) #remove entire subjects 

repetitive4 <- test%>%
  dplyr::group_by(subject,list,ppt_gender,responses) %>%
  dplyr::filter(responses == 4)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76)
repetitive4

#There are none

repetitive5 <- test%>%
  dplyr::group_by(subject,list,ppt_gender,responses) %>%
  dplyr::filter(responses == 5)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76)
repetitive5

#There are none

repetitive6 <- test%>%
  dplyr::group_by(subject,list,ppt_gender,score) %>%
  dplyr::filter(score == 6)%>%
  dplyr::summarise(n_excl = length(responses)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 76)
repetitive6

#####
# Create df for questions

question = raw_data %>% # Read in data
  dplyr::select(subject,list,trial_type,responses,ppt_gender,test_part,condition) %>% #Select columns we want
  dplyr::filter(test_part %in% c("demographics"))


all.equal(question, fromJSON(toJSON(question))) #check it's a json file, yep


question_json_file <- question$responses

question_json_file <- purrr::map(question$responses, jsonlite::fromJSON)

#convert from json string to data frame
question_json_file <- lapply(question_json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

question_json_only <- do.call("rbind", question_json_file)

question_json_full <- cbind(question_json_only, question) #bind dfs so subject number is associated with responses


#####
# Exclude based on mis-stated personal characteristics

demog = question_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics")%>%
  dplyr::select(subject,Q6,test_part,ppt_gender,list,condition)%>%
  dplyr::rename(
    reported_gender = Q6
  ) %>%
  dplyr::filter(ppt_gender =="woman") #change each time per ppt gender

#Found 1 invalid participants due to mis-reporting (from conjoined condition, woman)

newdata <- newdata %>%
  dplyr::filter(!(subject == "73893"))

## FIVE YEARS

fiveyears = raw_data %>% # Read in data
  dplyr::select(subject,button_pressed,ppt_gender,test_part,condition) %>% #Select columns we want
  dplyr::filter(test_part %in% c("fiveyears"))%>%
  dplyr::filter(button_pressed %in% c(1))

#7 ineligible (answered "No" to living in the UK for the last 5 years). Remove ineligible participants.

newdata <- subset(newdata, !subject %in% fiveyears$subject)

#Other descriptives
table(demog$list,demog$ppt_gender,demog$condition) #check you started off with the right number of participants. Yep, got all 8 lists and 8 participants in each (4 men, 4 women).


#####
# Exclude based on RT
newdata$rt <- as.numeric(as.character(newdata$rt), na.rm=T)

rt_average <- newdata %>%
  dplyr::select(id,subject,rt,list,trial_type,responses,ppt_gender,test_part) %>%
  dplyr::group_by(subject) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T),
                   rt.sd = sd(rt,na.rm=T),
                   rt.se = sd(rt,na.rm=T)/sqrt(length(subject)))

#combine rt_average df with main df

data <- merge(newdata, rt_average, by="subject")


data <- data %>%
  dplyr::filter(!(rt > (rt.mean + (rt.sd * 3))))

#####
# Determine demographics

question_eligible = raw_data %>% # Read in data
  dplyr::select(subject,list,trial_type,responses,ppt_gender,test_part) %>% #Select columns we want
  dplyr::filter(test_part %in% c("demographics"))

all.equal(question_eligible, fromJSON(toJSON(question))) #check it's a json file, yep


question_json_file_eligible <- question_eligible$responses

question_json_file_eligible <- purrr::map(question_eligible$responses, jsonlite::fromJSON)

#convert from json string to data frame
question_json_file_eligible <- lapply(question_json_file_eligible, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

question_json_only_eligible <- do.call("rbind", question_json_file_eligible)

question_json_full_eligible <- cbind(question_json_only_eligible, question_eligible)

age_eligible = question_json_full_eligible %>% # Read in data
  dplyr::filter(test_part == "demographics") %>% # Filter out demographics only
  dplyr::select(Q0,subject) %>%
  dplyr::rename(
    ppt_age = Q0 #rename column from Q0 to ppt_age
  )


age_eligible$ppt_age <- as.numeric(as.character(age_eligible$ppt_age), na.rm=T)

#is.numeric(age$ppt_age)

data <- merge(data, age_eligible, by="subject")

data$score <- as.numeric(as.character(data$score), na.rm=T)                
is.numeric(data$score)

summary(data$ppt_age)
sd(data$ppt_age, na.rm = TRUE)

ppt_total <- data %>%
  dplyr::select(id,subject,list,ppt_gender,score,condition) %>%
  #dplyr::filter(condition == "conjoined")%>%
  dplyr::group_by(subject,ppt_gender) %>%
  dplyr::summarise(score.m = mean(score,na.rm=T))


#####
# Write dataframe
write.csv(data,"rating_study.csv")

