# From Mike Frank
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #-

library(dplyr)

#### Anonymise prolific ids

files <- list.files(path = "./full_passives_data_pre", pattern = "\\.csv$", full.names = T)
for (i in files){
  file = read_csv(i)
  file$subject_id = "anomymised"
  write_csv(file, file = gsub("_pre","",x = i))
}


# Process data
files <- list.files(path = "./full_passives_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE, col_types = cols(list = col_character())) %>% 
  bind_rows(.id = "id") %>%
  mutate(condition = "passive")

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

test = test %>%
  filter(responses != "\"\"") %>% # exclude weird trials with odd responses
  mutate(responses = as.numeric(responses),
         coding = ifelse(substr(left_part,1,2) == substr(sentence_type,1,2),"standard","reversed"),
         responses_new = ifelse(coding == "reversed", responses, -1 *(responses - 6)),
         score = responses_new - 3)


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
catch$score <- catch$responses

catch <- catch %>% 
  mutate(score = case_when(
    responses == 0 ~ 1, responses == 1 ~ 2, responses == 2 ~ 3, responses == 3 ~ 4,responses == 4 ~ 5, responses == 5 ~ 6, responses == 6 ~ 7))


catch = catch%>%
  dplyr::mutate(catch_correct = case_when((
    score > 3 & test_part == "rating_causation" & verb != "drive") ~ "correct",
    (score < 5 & test_part == "rating_volition"& verb != "drive") ~ "correct",
    (score < 5 & test_part == "rating_causation"& verb != "drive") ~ "incorrect",
    (score > 3 & test_part == "rating_volition"& verb != "drive") ~ "incorrect",
    (score < 5 & test_part == "rating_causation" & verb == "drive") ~ "correct",
    (score > 3 & test_part == "rating_volition"& verb == "drive") ~ "correct",
    (score > 3 & test_part == "rating_causation"& verb == "drive") ~ "incorrect",
    (score < 5 & test_part == "rating_volition"& verb != "drive") ~ "incorrect")
    )
table(catch$catch_correct,catch$list)

# catch exclusions
caught_out <- subset(catch, catch_correct == "incorrect", na.rm = T) #df for all bad trials. Use to clean data


catch_exclusions = caught_out %>%
  dplyr::group_by(subject,list,ppt_gender,condition) %>%
  dplyr::summarise(n_excl = length(catch_correct)) %>% #count number of excluded trials
  dplyr::filter(n_excl > 2) #filter out ppts who got over 2 catch questions wrong
catch_exclusions

#Comment back in after collecting data
newdata <- subset(test, !subject %in% catch_exclusions$subject) #remove entire subjects who fell below threshold for inclusion (total = 9)



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

write_csv(data,"rating_study_passive.csv")

#####
# Analysis
data %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise(score = mean(score)) %>% 
  dplyr::group_by() %>% 
  dplyr::summarise(score = mean(score))

