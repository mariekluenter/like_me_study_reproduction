#####
#
# Code for processing the raw data for the memory studies in Experiments 1 thru 3
# You will need to reset the working directory for each study as you go thru the script
#
#####
# Load data from study 1 - need to set working directory
# set working directory to study 1
files <- list.files(path = "./prolific_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

stimuli = read_csv("picture_tags.csv") %>%
  mutate(list = tolower(list))

test = raw_data %>% # Read in data
  dplyr::select(id,subject,rt,list,trial_type,trial_index,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part) %>% #DON'T FORGET TO ADD TEST_PART in real data! Select columns we want
  dplyr::filter(trial_type == "survey-text" & !test_part %in% c("worker_id", "demographics")) %>% #Select rows we want
  mutate(verb = gsub('<img src=\"stim_listA/','',
                     x=image_type
                     ,fixed=T),
         verb = gsub('<img src=\"stim_listB/','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif\"</stim_listA>','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif\"</stim_listB>','',
                     x=verb
                     ,fixed=T),
         responses = gsub('{\"Q0\":\"','', # Find and replace the nasty characters we don't need
                          x=responses
                          ,fixed=T),
         responses = gsub('\"}','',
                          x=responses
                          ,fixed=T),
         responses = tolower(responses),
         list = tolower(list)) %>% #make all responses lower case) 
  left_join(stimuli, by = c("list","verb")) %>% #join the data to the tagged databse linking picture name to demog. characteristics
  rowwise() %>%
  mutate(left_person_mentioned = str_detect(responses,left), 
         right_person_mentioned = str_detect(responses,right), #search if the left or right person is in response
         both_people_mentioned = ifelse(left_person_mentioned == TRUE & right_person_mentioned ==TRUE,
                                        TRUE,FALSE), #TRUE if ppts have used both correct names
         first_person = ifelse(both_people_mentioned == FALSE, "cannot auto compute", 
                               ifelse(
                                 unlist(gregexpr(pattern =left,responses)) < unlist(gregexpr(pattern =right,responses)),
                                 "left","right")), #if left person is named earlier in string response, then they're mentioned first etc
         first_person_gender = ifelse(both_people_mentioned == FALSE,"cannot auto compute", #add columns for gender and race of the first mentioned person
                                      ifelse(first_person == "left",left_gender,right_gender)),
         first_person_race = ifelse(both_people_mentioned == FALSE,"cannot auto compute",
                                    ifelse(first_person == "left",left_race,right_race)),
         syntax_correct = str_detect(responses,auxiliary),
         syntax = ifelse(syntax == "intransitive", "conjoined","transitive"), #rename "intransitive to more accurate label "conjoined"
         excluded_trial_mention = ifelse(both_people_mentioned == FALSE,"excluded","included"),
         excluded_trial_syntax = ifelse(syntax_correct == FALSE,"excluded","included"),
         excluded_trial = ifelse(excluded_trial_mention == "included" & excluded_trial_syntax == "included","included","excluded")
  ) 


#create column for event to be used in glmer as item, by removing number from 'verb'
test$event <- test$verb
test$event <- gsub('[[:digit:]]+', '', x=test$event)

#create new column for 4 participant demographics

test <- test %>%
  dplyr::mutate(ppt_demog = case_when((ppt_gender == "man" & ppt_race == "white") ~
                                        "White_Man", 
                                      (ppt_gender == "man" & ppt_race == "black") ~ 
                                        "Black_Man", 
                                      (ppt_gender == "woman" & ppt_race == "black") ~ 
                                        "Black_Woman", 
                                      (ppt_gender == "woman" & ppt_race == "white") ~ 
                                        "White_Woman"))


#create new columns for man_first and white_first to analyse overal trends later

test$man_first <- ifelse(test$first_person_gender == "man", 1, 0) #create numerical column
test$white_first <- ifelse(test$first_person_race == "white", 1, 0) #create numerical column

#Create column for event valence (intimate, negative, casual)

test <- test %>%
  dplyr::mutate(valence = case_when((event == "hug" | event == "kiss" | 
                                       event == "marry" | event == "dance") ~ "intimate",
                                    (event == "shout" | event == "argue" | event == "fight") ~ "negative",
                                    (event == "meet" | event == "talk" | event == "touch") ~ "casual"))

#Creat df to check descriptives
descriptives_data <- subset(raw_data, trial_type=="survey-text")
table(descriptives_data$list,descriptives_data$ppt_gender, descriptives_data$ppt_race)
#equal number of participants in all lists across demographic conditions


#create new trial order column called trial_number, 1 - 80 for each ppt
test$trial_number <- rep(1:80, length=nrow(test))




#####
# Participant Exclusion

data_bad <- subset(test, excluded_trial == "excluded", na.rm = T) #df for all bad trials. Use to clean data
#19% trials excluded

verbal_exclusions = data_bad %>%
  dplyr::group_by(subject,syntax,list,ppt_race,ppt_gender) %>%
  dplyr::summarise(n_excl = length(excluded_trial)) %>% #count number of excluded trials in each aux condition
  tidyr::spread(key = syntax, value = n_excl) %>%
  dplyr::filter(conjoined > 20 | transitive > 20) #filter out trials on which ppt got it wrong on over 50% transitive trials and over 50% intransitive trials
verbal_exclusions


newdata <- subset(test, !subject %in% verbal_exclusions$subject) #remove entire subjects who fell below threshold for inclusion 

newdata <- subset(newdata, excluded_trial!="excluded") #remove individual invalid responses
#Now data frame "newdata" only has valid trials and valid subjects... except for those with orientation bias


# We excluded 19% of trials for incorrect responses, and we excluded 31 whole subjects, for not providing valid responses on at least 50% transitive trials and 50% conjoined trials.




#####
# Orientation Bias

test$left_bias <- ifelse(test$first_person == "left", 1, 0) # Was left person mentioned first? This will be used to determine if >90% of ptpts trials had a left bias
test$right_bias <- ifelse(test$first_person == "right",1,0) # Measure right person mentioned first? This will be used to determine if >90% of ptpts trials had a right bias


#overall means by list, ptpt gender, ptpt race  etc
data_summary <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, # Create column that shows overall orientation bias (i.e., the larger of the left/right biases)
                                                    left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(list,ppt_gender,ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T), #Summarise orientation bias by list etc
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#overall means averaging across all ppts
test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T),
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>%
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= 
                                                      right_bias.subj.mean, left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(ppt_gender, ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T),
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#Overall left bias across all 4 demographic groups is 0.57


#subject means - how are ppts responding, what's their L-R bias?
data_summary2 <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, 
                                               left_bias.subj.mean, right_bias.subj.mean)) # Create column that shows overall orientation bias for each subject (i.e., the larger of the left/right biases)

orientationbias_excluded = data_summary2 %>%
  dplyr::filter(!(orientation_bias.mean >=0.1 & orientation_bias.mean <=0.9)) #filtering out ppts with an orientation bias that meets our exclusion criteria ( > 0.9 or < 0.1 of trials described from one orientation)

#Excluded 2 participant for an orientation bias of over 0.9 (both left bias)


#####
# left_bias_plot

#Overall orientation bias mean
orientation_bias_average <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T))

summary(orientation_bias_average$left_bias.subj.mean)
sd(orientation_bias_average$left_bias.subj.mean, na.rm = TRUE)

#Mean = 0.54 (SD = 0.13)

orientation_bias_plot <- ggplot(data_summary2, aes(x = ppt_gender, y = orientation_bias.mean)) +
  geom_jitter(width = 0.3,height = 0)+
  geom_hline(yintercept = c(0.9),linetype='dotted', colour = "black")+
  geom_hline(yintercept = c(0.1),linetype='dotted', color = "black")+
  xlim(c(0,1.0)) +
  xlab("Participant gender")+
  ylab("Bias to one orientation")+
  theme_minimal()+
  scale_x_discrete(limits = c("man", "woman"),
                   labels = c("Men", "Women"))+
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"))
orientation_bias_plot
ggsave("USE_Orientation_Bias.png") #save plot as png

#Now remove participants 67824 (black man list 1a) and 68311 (black man like 2b) from "newdata" df

newdata <- subset(newdata, !subject %in% orientationbias_excluded$subject)


# We excluded 2 participants for having a strong left orientation bias.

#####
# Check participant eligibility


# Looking to check that the self-selected gender and race of Prolific academic users matches their self-reported gender and race, and that they are L1 english speakers. For example, we will exclude participants who have put "male" for gender on the post-study questionnaire when they should have been woman, or "white" when they should have been black.

#start by creating dataframe for likert responses

likert = raw_data %>% # Read in data
  dplyr::select(subject,list,trial_type,responses,ppt_race,ppt_gender,test_part) %>% #Select columns we want
  dplyr::filter(test_part %in% c("demographics","likert_race","likert_gender"))


all.equal(likert, fromJSON(toJSON(likert))) #check it's a json file, yep


likert_json_file <- likert$responses

likert_json_file <- purrr::map(likert$responses, jsonlite::fromJSON)

#convert from json string to data frame
likert_json_file <- lapply(likert_json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

likert_json_only <- do.call("rbind", likert_json_file)

likert_json_full <- cbind(likert_json_only, likert) #bind dfs so subject number is associated with responses

likert_age_raw = likert_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics") %>% # Filter out demographics only
  dplyr::select(Q0,subject) %>%
  dplyr::rename(
    ppt_age = Q0 #rename column from Q0 to ppt_age
  )

likert_age_raw$ppt_age <- as.numeric(as.character(likert_age_raw$ppt_age), na.rm=T)

summary(likert_age_raw$ppt_age)
sd(likert_age_raw$ppt_age, na.rm = TRUE)
#(120 female; mean age = 30.54 (SD = 9.77), range 18 – 70)--- ALL 240 ppts

# gender_race_eligibility

#next create dataframe to find invalid self-reported participants
#First language (english), gender (m/f), race (Black/white)

likert_demog = likert_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics") %>%
  dplyr::select(subject,Q1,Q2,Q4,Q5,ppt_race,ppt_gender)%>%
  dplyr::rename(L1 = Q1) %>% #rename column
  dplyr::rename(current_language = Q2) %>%
  dplyr::rename(reported_race = Q4) %>%
  dplyr::rename(reported_gender = Q5) %>%
  dplyr::filter(ppt_race == "black", ppt_gender =="woman") #change each time per ppt demog

# Remove participants whose reported race or gender did not match their prolific profile

newdata <- newdata %>%
  dplyr::filter(!(subject == "91610")) %>% #from black men (white)
  dplyr::filter(!(subject == "1019")) %>% #from black women (no race given)
  dplyr::filter(!(subject == "82873")) %>% #from black women (black and white)
  dplyr::filter(!(subject == "9415")) %>% #from black women (white)
  dplyr::filter(!(subject == "5732"))%>% #from white men (refuse to say)
  dplyr::filter(!(subject == "93884"))%>% #f white men (first language spanish)
  dplyr::filter(!(subject == "18280"))%>% #f white women (first language spanish)
  dplyr::filter(!(subject == "23972"))%>% #f black women (first language shona)
  dplyr::filter(!(subject == "55994"))%>% #f black women (first language Somali)
  dplyr::filter(!(subject == "87528"))%>% #f black men (first language Efik)
  dplyr::filter(!(subject == "95473"))%>% #f black women (first language German)
  dplyr::filter(!(subject == "70165"))%>% #f black women (first language Twi)
  dplyr::filter(!(subject == "74618")) #f black women (first language Twi)

#13 participants excluded (5 for race mis-reporting, 8 for not being english L1)


#####
# Trial Exclusion - Reaction Time



#Make rt numeric
newdata$rt <- as.numeric(as.character(newdata$rt), na.rm=T)

rt_average <- newdata %>%
  dplyr::select(id,subject,rt,list,trial_type,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject) %>% #removed ppt_gender,ppt_race,add back in to get race/gender split across ppts for commented out table below (oherwise we get multiple ppt_gender ppt_race columns)
  dplyr::summarize(rt.mean = mean(rt,na.rm=T),
                   rt.sd = sd(rt,na.rm=T),
                   rt.se = sd(rt,na.rm=T)/sqrt(length(subject)))

demog_post_exclusion <- newdata %>%
  dplyr::select(subject,responses,ppt_gender,ppt_race,rt) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T))

table(demog_post_exclusion$ppt_gender,demog_post_exclusion$ppt_race)
#100 Women and 96 Men in final data set for main analysis

#combine rt_average df with main df
data <- merge(newdata, rt_average, by="subject")
#Exclude trials
data <- data %>%
  dplyr::filter(!(rt > (rt.mean + (rt.sd * 3))))

#after excluding trials with a response time greater than 3 standard deviations away from the subject mean, total number of analysable trials went from 13924 to 13681. Go to ppt_age analysis for means and sd of participant pool.

(13681/13924)*100

# Process to match the data in Study 2 and Study 3
data = data %>%
  mutate(study = "study1", .before = subject) %>%
  dplyr::select(!c("left_height","right_height","height_diff","left_width","right_width","width_diff","left_volume","right_volume","volume_diff"))
# the height columns were used when we were post-hoc analysing visual differences in the images
# they might be interesting for other people to look at, but follow-on studies used matches images


study1_memory = subset(raw_data, trial_type == "html-button-response" & !is.na(first_guess)) %>%
  filter(!subject %in% verbal_exclusions$subject) %>%
  filter(!subject %in% orientationbias_excluded$subject) %>%
  mutate(Study = "Study1")


#####
# Study 2
# set working directory to study 2


files <- list.files(path = "./prolific_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

stimuli = read_csv("picture_tags.csv") %>%
  mutate(list = tolower(list))

test = raw_data %>% # Read in data
  dplyr::select(id,subject,rt,list,trial_type,trial_index,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part) %>%
  dplyr::filter(trial_type == "survey-text" & !test_part %in% c("worker_id", "demographics")) %>% #Select rows we want
  mutate(verb = gsub('<img src=\"stim_listA/','',
                     x=image_type
                     ,fixed=T),
         verb = gsub('<img src=\"stim_listB/','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif"style="height:400px;"></stim_listA>','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif"style="height:400px;"></stim_listB>','',
                     x=verb
                     ,fixed=T),
         responses = gsub('{\"Q0\":\"','', # Find and replace the nasty characters we don't need
                          x=responses
                          ,fixed=T),
         responses = gsub('\"}','',
                          x=responses
                          ,fixed=T),
         responses = tolower(responses),
         list = tolower(list)) %>% #make all responses lower case) 
  left_join(stimuli, by = c("list","verb")) %>% #join the data to the tagged databse linking picture name to demog. characteristics
  rowwise() %>%
  mutate(left_person_mentioned = str_detect(responses,left), 
         right_person_mentioned = str_detect(responses,right), #search if the left or right person is in response
         both_people_mentioned = ifelse(left_person_mentioned == TRUE & right_person_mentioned ==TRUE,
                                        TRUE,FALSE), #TRUE if ppts have used both correct names
         first_person = ifelse(both_people_mentioned == FALSE, "cannot auto compute", 
                               ifelse(
                                 unlist(gregexpr(pattern =left,responses)) < unlist(gregexpr(pattern =right,responses)),
                                 "left","right")), #if left person is named earlier in string response, then they're mentioned first etc
         first_person_gender = ifelse(both_people_mentioned == FALSE,"cannot auto compute", #add columns for gender and race of the first mentioned person
                                      ifelse(first_person == "left",left_gender,right_gender)),
         first_person_race = ifelse(both_people_mentioned == FALSE,"cannot auto compute",
                                    ifelse(first_person == "left",left_race,right_race)),
         syntax_correct = str_detect(responses,auxiliary),
         syntax = ifelse(syntax == "intransitive", "conjoined","transitive"), #rename "intransitive to more accurate label "conjoined"
         excluded_trial_mention = ifelse(both_people_mentioned == FALSE,"excluded","included"),
         excluded_trial_syntax = ifelse(syntax_correct == FALSE,"excluded","included"),
         excluded_trial = ifelse(excluded_trial_mention == "included" & excluded_trial_syntax == "included","included","excluded")
  ) 

test = test %>% # Read in data
  mutate(complete_phrase = ifelse(syntax == "transitive" | (syntax == "conjoined" & str_detect(responses, "each")), "complete", "incomplete"))


#try_subset <- subset(try, complete_phrase == "incomplete" & syntax == "conjoined", na.rm = T) #used to clean data i.e. check for misspellings of "each"


#create column for event to be used in glmer as item, by removing number from 'verb'
test$event <- test$verb
test$event <- gsub('[[:digit:]]+', '', x=test$event)

#create new column for 4 participant demographics

test <- test %>%
  dplyr::mutate(ppt_demog = case_when((ppt_gender == "man" & ppt_race == "white") ~
                                        "White_Man", 
                                      (ppt_gender == "man" & ppt_race == "black") ~ 
                                        "Black_Man", 
                                      (ppt_gender == "woman" & ppt_race == "black") ~ 
                                        "Black_Woman", 
                                      (ppt_gender == "woman" & ppt_race == "white") ~ 
                                        "White_Woman"))


#create new columns for man_first and white_first to analyse overal trends later

test$man_first <- ifelse(test$first_person_gender == "man", 1, 0) #create numerical column
test$white_first <- ifelse(test$first_person_race == "white", 1, 0) #create numerical column


#Create column for event valence (intimate, negative, casual)


test <- test %>%
  dplyr::mutate(valence = case_when((event == "flirt" | event == "kiss" | 
                                       event == "marry" | event == "dance") ~
                                      "intimate",
                                    (event == "shout" | event == "argue" | event == 
                                       "fight") ~ "negative",
                                    (event == "play" | event == "talk" |
                                       event == "touch") ~ "casual"))


#Create df to check descriptives
descriptives_data <- subset(raw_data, trial_type=="survey-text")
table(descriptives_data$list,descriptives_data$ppt_gender, descriptives_data$ppt_race)
#equal number of participants in all lists across demographic conditions


#create new trial order column called trial_number, 1 - 80 for each ppt
test$trial_number <- rep(1:80, length=nrow(test))


#####
# Data Cleaning
## Participant Exclusion

data_bad <- subset(test, excluded_trial == "excluded", na.rm = T) #df for all bad trials. Use to clean data


verbal_exclusions = data_bad %>%
  dplyr::group_by(subject,syntax,list,ppt_race,ppt_gender) %>%
  dplyr::summarise(n_excl = length(excluded_trial)) %>% #count number of excluded trials in each aux condition
  tidyr::spread(key = syntax, value = n_excl) %>%
  dplyr::filter(conjoined > 20 | transitive > 20) #filter out trials on which ppt got it wrong on over 50% transitive trials and over 50% intransitive trials
verbal_exclusions


newdata <- subset(test, !subject %in% verbal_exclusions$subject) #remove entire subjects who fell below threshold for inclusion 

newdata <- subset(newdata, excluded_trial!="excluded") #remove individual invalid responses (invalid trials)

newdata <- subset(newdata, complete_phrase!="incomplete") #remove individual invalid responses (conjoined trials missing "each other")

#Now data frame "newdata" only has valid trials and valid subjects... except for those with orientation bias

# We excluded 26 whole subjects, for not providing valid responses on at least 50% transitive trials and 50% conjoined trials.

#####
## Orientation Bias

#In order to determine the extent of an orientation bias, we will measure the proportion of responses on which speakers first mentioned the figure who appeared on the left-hand-side first, or the figure who appeared on the right-hand side first. Participants who demonstrate an orientation bias for mentioning either the figure who appears on the left first on over 90% of trials, or the figure who appears on the right first on over 90% of trials, will be excluded.

test$left_bias <- ifelse(test$first_person == "left", 1, 0) # Was left person mentioned first? This will be used to determine if >90% of ptpts trials had a left bias
test$right_bias <- ifelse(test$first_person == "right",1,0) # Measure right person mentioned first? This will be used to determine if >90% of ptpts trials had a right bias


#overall means by list, ptpt gender, ptpt race  etc
data_summary <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, # Create column that shows overall orientation bias (i.e., the larger of the left/right biases)
                                                    left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(list,ppt_gender,ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T), #Summarise orientation bias by list etc
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#overall means averaging across all ppts
test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T),
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>%
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= 
                                                      right_bias.subj.mean, left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(ppt_gender, ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T),
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#subject means - how are ppts responding, what's their L-R bias?
data_summary2 <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, 
                                               left_bias.subj.mean, right_bias.subj.mean)) # Create column that shows overall orientation bias for each subject (i.e., the larger of the left/right biases)

orientationbias_excluded = data_summary2 %>%
  dplyr::filter(!(orientation_bias.mean >=0.1 & orientation_bias.mean <=0.9)) #filtering out ppts with an orientation bias that meets our exclusion crietria ( > 0.9 of trials described from one orientation)

#Exclude 5 participants for an orientation bias of over 0.9 (all left bias)

#Overall orientation bias mean
orientation_bias_average <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T))

summary(orientation_bias_average$left_bias.subj.mean)
sd(orientation_bias_average$left_bias.subj.mean, na.rm = TRUE)

#Mean = 0.53 (SD = 0.15)



# left_bias_plot

orientation_bias_plot <- ggplot(data_summary2, aes(x = ppt_gender, y = orientation_bias.mean)) +
  geom_jitter(width = 0.3,height = 0)+
  geom_hline(yintercept = c(0.9),linetype='dotted', colour = "black")+
  geom_hline(yintercept = c(0.1),linetype='dotted', color = "black")+
  xlim(c(0,1.0)) +
  xlab("Participant gender")+
  ylab("Bias to one orientation")+
  theme_minimal()+
  scale_x_discrete(limits = c("man", "woman"),
                   labels = c("Men", "Women"))+
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"))
orientation_bias_plot
ggsave("USE_Orientation_Bias.png") #save plot as png

#Now remove participants
newdata <- subset(newdata, !subject %in% orientationbias_excluded$subject)

# We excluded 5 participants for having an orientation bias of over 0.9 (all left bias).

#####
## Participant Eligibility

#start by creating dataframe for questionnaire responses

question = raw_data %>% # Read in data
  dplyr::select(subject,list,trial_type,responses,ppt_race,ppt_gender,test_part) %>% #Select columns we want
  dplyr::filter(test_part %in% c("demographics", "likert_ESS", "likert_NSSAL_Q1",
                                 "likert_NSSAL_Q2", "likert_CGRS_Q1", "likert_CGRS_Q2",
                                 "likert_CGRS_Q3"))


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

# gender_race_eligibility

#next create dataframe to find invalid self-reported participants


demog = question_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics")%>%
  dplyr::select(subject,Q1,Q2,Q3,Q4,Q5,ppt_race,ppt_gender)%>%
  dplyr::rename(
    L1 = Q1 #rename column
  ) %>%
  dplyr::rename(
    current_language = Q2 #rename column
  ) %>%
  dplyr::rename(
    reported_race = Q4 #rename column from Q0 to reported_race
  ) %>%
  dplyr::rename(
    reported_gender = Q5
  ) %>%
  dplyr::filter(ppt_race == "white", ppt_gender =="man") #change each time per ppt demog

#Invalid participants found were:
#24950; 57920; 78747; 57082; 39035; 7643
#Remove from newdata df

newdata <- newdata %>%
  dplyr::filter(!(subject == "14790")) %>% #Black man exluded: Yoruba L1
  dplyr::filter(!(subject == "17111")) %>% #Yoruba L1
  dplyr::filter(!(subject == "70009")) %>% #Yoruba L1
  dplyr::filter(!(subject == "77270")) %>% #Arabic L1
  dplyr::filter(!(subject == "84413")) %>% #Yoruba L1
  dplyr::filter(!(subject == "39035")) %>% #Race reported as "american"
  dplyr::filter(!(subject == "7643")) %>% #Race reported as "Biracial Afro Caribbean American / White European American"
  
  dplyr::filter(!(subject == "29709")) %>% #Black woman excluded: race reported as "BRITISH"
  dplyr::filter(!(subject == "78747")) %>% #“black/white/hispanic”
  dplyr::filter(!(subject == "57082")) %>% #Age reported in gender place
  
  dplyr::filter(!(subject == "89400")) %>% #White woman excluded: French L1
  
  dplyr::filter(!(subject == "57920")) %>% #White man excluded: Reported female
  dplyr::filter(!(subject == "24950")) %>% #reported "mixed"
  dplyr::filter(!(subject == "67659")) #Zulu L1

#lost 14 participants, eligible trials went down to 13344

# Checking to see if reported participant gender and race, and L1 matches the Prolific Academic self-selected gender/race and L1 (i.e. are they who we think they are). If these factors don't match, participant must be excluded.

#Excluded the following 14 participants:

#1. white men
# 
#     + 1 for race mis-match
#     + 1 for gender mis-match
#     + 1 for other language L1
#     
# 2. black women
# 
#     + 2 for race mis-match
#     + 1 for gender mis-matched (reported age instead)
#     
# 3. black men
# 
#     + 5 for other language L1
#     + 2 for race mis-match
#     
# 4. white women
# 
#     + 1 for other language L1
# 

#how_many_brits

demog_location = question_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics")%>%
  dplyr::select(subject,Q3)%>%
  dplyr::rename(
    Current_location = Q3 #rename column
  )%>%
  dplyr::filter(!(subject == "14790")) %>% #Black man exluded: Yoruba L1
  dplyr::filter(!(subject == "17111")) %>% #Yoruba L1
  dplyr::filter(!(subject == "70009")) %>% #Yoruba L1
  dplyr::filter(!(subject == "77270")) %>% #Arabic L1
  dplyr::filter(!(subject == "84413")) %>% #Yoruba L1
  dplyr::filter(!(subject == "39035")) %>% #Race reported as "american"
  dplyr::filter(!(subject == "7643")) %>% #Race reported as "Biracial Afro Caribbean American / White European American"
  
  dplyr::filter(!(subject == "29709")) %>% #Black woman excluded: race reported as "BRITISH"
  dplyr::filter(!(subject == "78747")) %>% #“black/white/hispanic”
  dplyr::filter(!(subject == "57082")) %>% #Age reported in gender place
  
  dplyr::filter(!(subject == "89400")) %>% #White woman excluded: French L1
  
  dplyr::filter(!(subject == "57920")) %>% #White man excluded: Reported female
  dplyr::filter(!(subject == "24950")) %>% #reported "mixed"
  dplyr::filter(!(subject == "67659")) #Zulu L1

#155 from the UK before exclusions
#146 from the UK after exclusions, or 65% participants out of 226

#UK	155	146
#America	74	71
#Canada	5	4
#ireland	5	4
#south korea	1	1

#####
## Trial Exclusion - RT

#Make rt numeric
newdata$rt <- as.numeric(as.character(newdata$rt), na.rm=T)

rt_average <- newdata %>%
  dplyr::select(id,subject,rt,list,trial_type,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  #dplyr::filter(ppt_gender == "man")%>%
  dplyr::group_by(subject) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T),
                   rt.sd = sd(rt,na.rm=T),
                   rt.se = sd(rt,na.rm=T)/sqrt(length(subject)))

#combine rt_average df with main df

data <- merge(newdata, rt_average, by="subject")


data <- data %>%
  dplyr::filter(!(rt > (rt.mean + (rt.sd * 3))))

#after excluding trials with a response time greater than 3 standard deviations away from the subject mean, total number of analysable trials went from 13344 ("newdata") to 13120 ("data").
#(224/19200)*100)

demog_post_exclusion <- newdata %>%
  dplyr::select(subject,responses,ppt_gender,ppt_race,rt) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T))

table(demog_post_exclusion$ppt_gender,demog_post_exclusion$ppt_race)
#105 Women and 90 Men in final data set for main analysis


# Process to match the data in Study 1 and Study 3
data = data %>%
  mutate(study = "study2", .before = subject) %>%
  dplyr::select(!c("left_height","right_height","height_diff","left_width","right_width","width_diff","left_volume","right_volume","volume_diff","complete_phrase"))
# the height columns were used when we were post-hoc analysing visual differences in the images


study2_memory = subset(raw_data, trial_type == "html-button-response" & !is.na(first_guess)) %>%
  filter(!subject %in% verbal_exclusions$subject) %>%
  filter(!subject %in% orientationbias_excluded$subject) %>%
  mutate(Study = "study2")


#######
# study 3
# set working directory to study 1

#####
# import_and_tidy_data

files <- list.files(path = "./prolific_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")


stimuli = read_csv("picture_tags_passive.csv") %>%
  mutate(list = tolower(list))

test = raw_data %>% # Read in data
  dplyr::select(id,subject,rt,list,trial_type,trial_index,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part) %>% #DON'T FORGET TO ADD TEST_PART in real data! Select columns we want
  dplyr::filter(trial_type == "survey-text" & !test_part %in% c("worker_id", "demographics")) %>% #Select rows we want
  mutate(verb = gsub('<img src=\"stim_listA/','',
                     x=image_type
                     ,fixed=T),
         verb = gsub('<img src=\"stim_listB/','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif"style="height:400px;"></stim_listA>','',
                     x=verb
                     ,fixed=T),
         verb = gsub('.gif"style="height:400px;"></stim_listB>','',
                     x=verb
                     ,fixed=T),
         responses = gsub('{\"Q0\":\"','', # Find and replace the nasty characters we don't need
                          x=responses
                          ,fixed=T),
         responses = gsub('\"}','',
                          x=responses
                          ,fixed=T),
         responses = tolower(responses),
         list = tolower(list)) %>% #make all responses lower case) 
  left_join(stimuli, by = c("list","verb")) %>% #join the data to the tagged databse linking picture name to demog. characteristics
  rowwise() %>%
  mutate(left_person_mentioned = str_detect(responses,left), 
         right_person_mentioned = str_detect(responses,right), #search if the left or right person is in response
         both_people_mentioned = ifelse(left_person_mentioned == TRUE & right_person_mentioned ==TRUE,
                                        TRUE,FALSE), #TRUE if ppts have used both correct names
         first_person = ifelse(both_people_mentioned == FALSE, "cannot auto compute", 
                               ifelse(
                                 unlist(gregexpr(pattern =left,responses)) < unlist(gregexpr(pattern =right,responses)),
                                 "left","right")), #if left person is named earlier in string response, then they're mentioned first etc
         first_person_gender = ifelse(both_people_mentioned == FALSE,"cannot auto compute", #add columns for gender and race of the first mentioned person
                                      ifelse(first_person == "left",left_gender,right_gender)),
         first_person_race = ifelse(both_people_mentioned == FALSE,"cannot auto compute",
                                    ifelse(first_person == "left",left_race,right_race)),
         syntax_correct = str_detect(responses,phrase_type),
         excluded_trial_mention = ifelse(both_people_mentioned == FALSE,"excluded","included"),
         excluded_trial_syntax = ifelse(syntax_correct == FALSE,"excluded","included"),
         excluded_trial = ifelse(excluded_trial_mention == "included" & excluded_trial_syntax == "included","included","excluded")
  ) 


#create column for event to be used in glmer as item, by removing number from 'verb'
test$event <- test$verb
test$event <- gsub('[[:digit:]]+', '', x=test$event)

#create new column for 4 participant demographics

test <- test %>%
  dplyr::mutate(ppt_demog = case_when((ppt_gender == "man" & ppt_race == "white") ~
                                        "White_Man", 
                                      (ppt_gender == "man" & ppt_race == "black") ~ 
                                        "Black_Man", 
                                      (ppt_gender == "woman" & ppt_race == "black") ~ 
                                        "Black_Woman", 
                                      (ppt_gender == "woman" & ppt_race == "white") ~ 
                                        "White_Woman"))

#Check for participants

worker_id = raw_data %>% # Read in data
  dplyr::filter(test_part %in% c("worker_id"))%>%
  dplyr::select(subject,responses,test_part)%>%
  mutate(responses = gsub('{\"Q0\":\"','', # Find and replace the nasty characters we don't need
                          x=responses
                          ,fixed=T))%>%
  mutate(responses = gsub('\"}','',
                          x=responses
                          ,fixed=T))

is.element("6156035bce9a2ce5f3683837",unlist(worker_id))

#create new columns for man_first and white_first to analyse overal trends later

test$man_first <- ifelse(test$first_person_gender == "man", 1, 0) #create numerical column
test$white_first <- ifelse(test$first_person_race == "white", 1, 0) #create numerical column

#Create column for event valence (intimate, negative, casual)

test <- test %>%
  dplyr::mutate(valence = case_when((event == "kiss" | event == "seduce" | 
                                       event == "hug") ~ "intimate",
                                    (event == "shout" | event == "challenge" | event == "oppose") ~ "negative",
                                    (event == "greet" | event == "look" | event == "touch" | event == "copy") ~ "casual"))

#create new trial order column called trial_number, 1 - 80 for each ppt
test$trial_number <- rep(1:80, length=nrow(test))

#Create df to check descriptives
descriptives_data <- subset(raw_data, test_part == "demographics" & trial_type=="survey-text")
descriptives_table <- table(descriptives_data$list,descriptives_data$ppt_gender, descriptives_data$ppt_race)
descriptives_table

#####
# Participants

# From the initial 240 participants (60 Black women, 60 Black men, 60 white women and 60 white men), we excluded 29 participants for not providing good responses on at least 50% of active trials and 50% of passive trials, 8 participant for showing a strong orientation bias (mentioning either the figure who appears on the left first on over 90% of trials, or the figure who appears on the right first on over 90% of trials), 8 participants for misreporting their gender, race, or L1 on Prolific and 5 participant for not having lived in the USA for the last five years. Thus, our final sample included 196 participants: 46 Black Women, 49 Black Men, 55 White Women and 46 White Men (mean age = 30.63 (sd = 9.25), range = 18 - 68).

# exclusions

data_bad <- subset(test, excluded_trial == "excluded", na.rm = T) #df for all bad trials. Use to clean data
#19% trials excluded

verbal_exclusions = data_bad %>%
  dplyr::group_by(subject,syntax,list,ppt_race,ppt_gender) %>%
  dplyr::summarise(n_excl = length(excluded_trial)) %>% #count number of excluded trials in each aux condition
  tidyr::spread(key = syntax, value = n_excl) %>%
  dplyr::filter(passive > 20 | active > 20) #filter out trials on which ppt got it wrong on over 50% active trials and over 50% passive trials
verbal_exclusions


newdata <- subset(test, !subject %in% verbal_exclusions$subject) #remove entire subjects who fell below threshold for inclusion 

newdata <- subset(newdata, excluded_trial!="excluded") #remove individual invalid responses
#Now data frame "newdata" only has valid trials and valid subjects... except for those with orientation bias. Went from 16880 trials to 14932 trials. Reduction of 11% trials.

#Use this to find redeemable participants
# data_bad2 <- subset(data_bad, !subject %in% orientationbias_excluded$subject)
# data_bad2 <- subset(data_bad2, !subject %in% fiveyears$subject)
# data_bad2 <- data_bad2 %>%
#   dplyr::filter(!(subject == "34043")) %>% #BW, L1 creole
#   dplyr::filter(!(subject == "95926")) %>% #BW, L1 creole
#   dplyr::filter(!(subject == "88993")) %>% #BW, L1 igbo
#   dplyr::filter(!(subject == "29102")) %>% #BW, L1 jamaican
#   dplyr::filter(!(subject == "94990")) %>% #BW, race white
#   dplyr::filter(!(subject == "15145")) %>% #BM, race Hispanic/Latino
#   dplyr::filter(!(subject == "47195")) %>% #WM, L1 spanish
#   dplyr::filter(!(subject == "5577")) %>% #WW, L1 polish
#   mutate(name_to_scan = ifelse(both_people_mentioned == FALSE,"scan","dontscan"))%>%
#   mutate(syntax_to_scan = ifelse(syntax_correct == FALSE,"scan","dontscan"))%>%
#   select(subject,list,responses,ppt_gender,ppt_race,left,right,phrase_type,both_people_mentioned,syntax_correct,right,name_to_scan,syntax_to_scan)%>%
#   dplyr::filter(!(syntax_to_scan == "dontscan"))%>%#change each time from name_to_scan to syntax_to_scan
#   dplyr::filter(!(both_people_mentioned == "FALSE"))#then get rid of trials where both people weren't mentioned because I've already gone through these.

#BEFORE CLEANING
#13298 in data
#3858 in data_bad
#1478 in data_bad2 with syntax errors



## Orientation Bias


test$left_bias <- ifelse(test$first_person == "left", 1, 0) # Was left person mentioned first? This will be used to determine if >90% of ptpts trials had a left bias
test$right_bias <- ifelse(test$first_person == "right",1,0) # Measure right person mentioned first? This will be used to determine if >90% of ptpts trials had a right bias


#overall means by list, ptpt gender, ptpt race  etc
data_summary <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, # Create column that shows overall orientation bias (i.e., the larger of the left/right biases)
                                                    left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(list,ppt_gender,ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T), #Summarise orientation bias by list etc
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#overall means averaging across all ppts
test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T),
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>%
  dplyr::mutate(orientation_bias.subj.mean = ifelse(left_bias.subj.mean >= 
                                                      right_bias.subj.mean, left_bias.subj.mean, right_bias.subj.mean)) %>%
  dplyr::group_by(ppt_gender, ppt_race) %>%
  dplyr::summarise(orientation_bias.mean = mean(orientation_bias.subj.mean,na.rm=T),
                   orientation_bias.sd = sd(orientation_bias.subj.mean))

#Overall left bias across all 4 demographic groups is 0.57


#subject means - how are ppts responding, what's their L-R bias?
data_summary2 <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender,ppt_race,list) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T), # get ptpt mean left bias
                   right_bias.subj.mean = mean(right_bias,na.rm=T)) %>% # get ptpt mean right bias bias
  dplyr::mutate(orientation_bias.mean = ifelse(left_bias.subj.mean >= right_bias.subj.mean, 
                                               left_bias.subj.mean, right_bias.subj.mean)) # Create column that shows overall orientation bias for each subject (i.e., the larger of the left/right biases)

orientationbias_excluded = data_summary2 %>%
  dplyr::filter(!(orientation_bias.mean >=0.1 & orientation_bias.mean <=0.9)) #filtering out ppts with an orientation bias that meets our exclusion criteria ( > 0.9 or < 0.1 of trials described from one orientation)

orientation_bias_average <- test %>%
  dplyr::select(id,subject,list,trial_type,responses,image_type,phrase_type,left_bias,right_bias,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,ppt_gender) %>%
  dplyr::summarize(left_bias.subj.mean = mean(left_bias,na.rm=T))

summary(orientation_bias_average$left_bias.subj.mean)
sd(orientation_bias_average$left_bias.subj.mean, na.rm = TRUE)

# left_bias_plot

orientation_bias_plot <- ggplot(data_summary2, aes(x = ppt_gender, y = orientation_bias.mean)) +
  geom_jitter(width = 0.3,height = 0)+
  geom_hline(yintercept = c(0.9),linetype='dotted', colour = "black")+
  geom_hline(yintercept = c(0.1),linetype='dotted', color = "black")+
  xlim(c(0,1.0)) +
  xlab("Participant gender")+
  ylab("Bias to one orientation")+
  theme_minimal()+
  scale_x_discrete(limits = c("man", "woman"),
                   labels = c("Men", "Women"))+
  theme(plot.title = element_text(color="black", size=16, face="bold", hjust = 0.5),
        axis.text.x = element_text(color = "black", size = 16),
        axis.text.y = element_text(color = "black", size = 16),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"))
orientation_bias_plot


#Now remove participants 67824 (black man list 1a) and 68311 (black man like 2b) from "newdata" df

newdata <- subset(newdata, !subject %in% orientationbias_excluded$subject)


#####
# Check participant eligibility
#start by creating dataframe for post-study questionnaire responses

question = raw_data %>% # Read in data
  dplyr::select(subject,list,trial_type,responses,ppt_race,ppt_gender,test_part) %>% #Select columns we want
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
# gender_race_eligibility, include=FALSE, echo=FALSE}

#next create dataframe to find invalid self-reported participants


demog = question_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics")%>%
  dplyr::select(subject,Q1,Q5,Q6,test_part,ppt_race,ppt_gender,list)%>%
  dplyr::rename(
    L1 = Q1 #rename column from Q1 to first language
  ) %>%
  dplyr::rename(
    reported_race = Q5 #rename column from Q0 to reported_race
  ) %>%
  dplyr::rename(
    reported_gender = Q6
  ) %>%
  dplyr::filter(ppt_race == "white", ppt_gender =="man") #change each time per ppt demog and condition

#Invalid participants found in HumOr were:
#

newdata <- newdata %>%
  dplyr::filter(!(subject == "34043")) %>% #BW, L1 creole
  dplyr::filter(!(subject == "95926")) %>% #BW, L1 creole
  dplyr::filter(!(subject == "88993")) %>% #BW, L1 igbo
  dplyr::filter(!(subject == "29102")) %>% #BW, L1 jamaican
  dplyr::filter(!(subject == "94990")) %>% #BW, race white
  dplyr::filter(!(subject == "15145")) %>% #BM, race Hispanic/Latino
  dplyr::filter(!(subject == "47195")) %>% #WM, L1 spanish
  dplyr::filter(!(subject == "5577")) #WW, L1 polish

#8 participants removed for misreporting gender, race, or L1

## FIVE YEARS

fiveyears = raw_data %>% # Read in data
  dplyr::select(subject,button_pressed,list,trial_type,ppt_race,ppt_gender,test_part) %>% #Select columns we want
  dplyr::filter(test_part %in% c("fiveyears"))%>%
  dplyr::filter(button_pressed %in% c(1))


newdata <- subset(newdata, !subject %in% fiveyears$subject)

#Total analysable trials went from xxxx to xxxx
#205 participants overall

#Checking to see if reported participant gender and race matches the Prolific Academic self-selected gender/race (i.e. are they who we think they are), and if ppts had been in the USA for the last 5 years. If these factors don't match, participant must be excluded.

#Excluded 5 participants for not living in the USA for the last 5 years, and 8 participants for mis-reporting.


#####
# question_age_raw, include=FALSE}

question_age_raw = question_json_full %>% # Read in data
  dplyr::filter(test_part == "demographics") %>% # Filter out demographics only
  dplyr::select(Q0,subject) %>%
  dplyr::rename(
    ppt_age = Q0 #rename column from Q0 to ppt_age
  )

question_age_raw$ppt_age <- as.numeric(as.character(question_age_raw$ppt_age), na.rm=T)

summary(question_age_raw$ppt_age)
sd(question_age_raw$ppt_age, na.rm = TRUE)



#####
# rt_exclusions

## Trial Exclusion - RT

#Make rt numeric
newdata$rt <- as.numeric(as.character(newdata$rt), na.rm=T)

rt_average <- newdata %>%
  dplyr::select(id,subject,rt,list,trial_type,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T),
                   rt.sd = sd(rt,na.rm=T),
                   rt.se = sd(rt,na.rm=T)/sqrt(length(subject)))

#combine rt_average df with main df

data <- merge(newdata, rt_average, by="subject")


data <- data %>%
  dplyr::filter(!(rt > (rt.mean + (rt.sd * 3))))

#after excluding trials with a response time greater than 3 standard deviations away from the subject mean, total number of analysable trials went from 13839 ("newdata") to 13597 ("data").

#describe(test)

#xxx Participants overall in final dataset

rt_average_demog <- newdata %>%
  dplyr::select(id,subject,rt,list,trial_type,responses,image_type,phrase_type,ppt_gender,ppt_race,test_part,excluded_trial,event) %>%
  dplyr::group_by(subject,list,ppt_gender,ppt_race) %>%
  dplyr::summarize(rt.mean = mean(rt,na.rm=T),
                   rt.sd = sd(rt,na.rm=T),
                   rt.se = sd(rt,na.rm=T)/sqrt(length(subject)))

table(rt_average_demog$ppt_gender, rt_average_demog$ppt_race)
#49 Black Men, 46 Black Women, 46 White Men and 55 White Women in final data set


# Process to match the data in Study 1 and Study 3
data = data %>%
  mutate(study = "study4", .before = subject) 



study3_memory = subset(raw_data, trial_type == "html-button-response" & !is.na(first_guess)) %>%
  filter(!subject %in% verbal_exclusions$subject) %>%
  filter(!subject %in% orientationbias_excluded$subject) %>%
  select(!(response:survey_item)) %>%
  mutate(Study = "study3")

memory = rbind(study1_memory,study2_memory,study3_memory)
write_csv(memory, here("R_scripts", "scripts_reproduction" ,"Data_Preprocessing_reproduction", "reproduced_data", "studies1_2_3_memory.csv"))
