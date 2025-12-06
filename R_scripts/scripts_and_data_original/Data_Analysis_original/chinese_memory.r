
#####
# Code for exploratory analysis of memory game response times in the Chinese study
#
# This commented out section is the code for pulling the chinese_memory data out of the original Chinese study
#
#chinese_memory = subset(raw_data, trial_type == "html-button-response" & !is.na(first_guess)) %>%
#  filter(!subject %in% verbal_exclusions$subject) %>%
#  filter(!subject %in% orientationbias_excluded$subject)
  
#write_csv(chinese_memory, "chinese_study_chinese_memory.csv")

chinese_memory = read_csv("chinese_study_memory.csv")

chinese_memory$figure_gender = ifelse(grepl("W[1-2].jpg",chinese_memory$stimulus), "woman","man")
chinese_memory$figure_race = ifelse(grepl("W[MW][1-2].jpg",chinese_memory$stimulus), "asian","black")

chinese_memory = chinese_memory %>%
  dplyr::mutate(first_guess_num = ifelse(first_guess == "TRUE",1,0),
                correct_response = ifelse(button_pressed == correct,1,0),
                rt = as.numeric(rt))

chinese_memory %>%
  filter(first_guess == "TRUE" & correct_response == 1) %>%
  dplyr::group_by(ppt_gender,figure_gender,figure_race) %>%
  dplyr::mutate(first_guess_num = ifelse(first_guess == "TRUE",1,0),
                rt = as.numeric(rt)) %>%
  dplyr::summarise(mean(first_guess_num),
            mean((rt)))

chinese_memory$like_me_gender = ifelse(chinese_memory$ppt_gender == chinese_memory$figure_gender,1,0)
chinese_memory$like_me_race = ifelse(chinese_memory$ppt_race == chinese_memory$figure_race,1,0)
chinese_memory$OverallLikeMe = ifelse(chinese_memory$ppt_race == chinese_memory$figure_race & chinese_memory$ppt_gender == chinese_memory$figure_gender, 1, 0)
chinese_memory$LikeMeFeature = ifelse(chinese_memory$ppt_race == chinese_memory$figure_race & chinese_memory$ppt_gender == chinese_memory$figure_gender, "Gender Match Race Match", 
                              ifelse(chinese_memory$ppt_race != chinese_memory$figure_race & chinese_memory$ppt_gender == chinese_memory$figure_gender, "Gender Match Race No-Match",
                                     ifelse(chinese_memory$ppt_race == chinese_memory$figure_race & chinese_memory$ppt_gender != chinese_memory$figure_gender, "Gender No-Match Race Match","Gender No-Match Race No-Match")))

summary(lmer(log(rt) ~ like_me_race + (1+like_me_race|id), data = chinese_memory %>% 
               filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1)))
summary(lmer(log(rt) ~ like_me_gender + (1+like_me_gender|id), 
             data = chinese_memory %>% filter(first_guess == "TRUE" & rt < 10000  & correct_response == 1)))
summary(lmer(log(rt) ~ OverallLikeMe + (1+OverallLikeMe|id), data = 
               chinese_memory %>% filter(first_guess == "TRUE" & rt < 10000  & correct_response == 1)))


chinese_memory %>%
  filter(first_guess == "TRUE" & rt < 10000  & correct_response == 1) %>%
  dplyr::group_by(LikeMeFeature, id) %>%
  dplyr::summarise(rt = mean(rt)) %>%
  dplyr::group_by(LikeMeFeature) %>%
  dplyr::summarise(mean((rt)))


chinese_memory %>%
  filter(first_guess == "TRUE"  & correct_response == 1) %>%
  dplyr::group_by(ppt_gender,like_me_gender) %>%
  dplyr::summarise(mean(first_guess_num),
                   mean((rt)))

