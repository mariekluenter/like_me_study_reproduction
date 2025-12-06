#########
#
# Code for pre-registered analysis of memory game response times in experiments 1 thru 3
#
# Analysis of loaded full_memory data
library(tidyverse)
library(cowplot)
library(lme4)


# From Mike Frank
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #-



memory = read_csv("studies1_2_3_memory.csv")

memory$figure_gender = ifelse(grepl("W[1-2].jpg",memory$stimulus), "woman","man")
memory$figure_race = ifelse(grepl("W[MW][1-2].jpg",memory$stimulus), "white","black")

memory = memory %>%
  dplyr::mutate(first_guess_num = ifelse(first_guess == "TRUE",1,0),
                correct_response = ifelse(button_pressed == correct,1,0),
                rt = as.numeric(rt))

memory %>%
  filter(first_guess == "TRUE" & correct_response == 1) %>%
  dplyr::group_by(ppt_gender,figure_gender,figure_race) %>%
  dplyr::mutate(first_guess_num = ifelse(first_guess == "TRUE",1,0),
                rt = as.numeric(rt)) %>%
  dplyr::summarise(mean(first_guess_num),
                   mean((rt)))

memory$like_me_gender = ifelse(memory$ppt_gender == memory$figure_gender,1,0)
memory$like_me_race = ifelse(memory$ppt_race == memory$figure_race,1,0)
memory$OverallLikeMe = ifelse(memory$ppt_race == memory$figure_race & memory$ppt_gender == memory$figure_gender, 1, 0)

memory$LikeMeFeature = ifelse(memory$ppt_race == memory$figure_race & memory$ppt_gender == memory$figure_gender, "Gender Match\nRace Match", 
                              ifelse(memory$ppt_race != memory$figure_race & memory$ppt_gender == memory$figure_gender, "Gender Match\nRace No-Match",
                                     ifelse(memory$ppt_race == memory$figure_race & memory$ppt_gender != memory$figure_gender, "Gender No-Match\nRace Match","Gender No-Match\nRace No-Match")))

memory$LikeMeFeatureNumeric = ifelse(memory$ppt_race == memory$figure_race & memory$ppt_gender == memory$figure_gender, -1, 
                              ifelse(memory$ppt_race != memory$figure_race & memory$ppt_gender == memory$figure_gender, 0,
                                     ifelse(memory$ppt_race == memory$figure_race & memory$ppt_gender != memory$figure_gender, 0,1)))


summary(lmer(log(rt) ~ like_me_race + (1+like_me_race|id), data = memory %>% 
               filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1)))
summary(lmer(log(rt) ~ like_me_gender + (1+like_me_gender|id), 
             data = memory %>% filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1)))
summary(lmer(log(rt) ~ OverallLikeMe + (1+OverallLikeMe|id), 
             data = memory %>% filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1)))
summary(lmer(log(rt) ~ LikeMeFeatureNumeric + (1+LikeMeFeatureNumeric|id), 
             data = memory %>% filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1)))

numeric = lmer(log(rt) ~ LikeMeFeatureNumeric + (1+LikeMeFeatureNumeric|id), 
     data = memory %>% filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1))

numeric_base = lmer(log(rt) ~ 1 + (1|id), 
               data = memory %>% filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1))

anova(numeric,numeric_base)

memory_means = memory %>%
  filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1) %>%
  dplyr::group_by(id, LikeMeFeature) %>%
  dplyr::summarise(rt = median(rt,na.rm = T)) %>%
  dplyr::mutate(rt_norm = rt - mean(rt)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(LikeMeFeature) %>%
  dplyr::mutate(rt_norm = rt_norm + mean(rt)) %>%
  dplyr::summarise(rt.m = mean(rt),
                   se = sd(rt)/sqrt(n()),
                   se.norm = sd(rt_norm)/sqrt(n()), # within subject SE
                   ci.norm = 1.96 * se.norm,# within subject CI
                   rt.low = ci.low(rt),# Remember that the bootstrapped CIs do not account for within subjects design
                   rt.high = ci.high(rt)) # Remember that the bootstrapped CIs do not account for within subjects design
memory_means

memory_subj = memory %>%
  filter(first_guess == "TRUE" & rt < 10000 & correct_response == 1) %>%
  dplyr::group_by(id, LikeMeFeature) %>%
  dplyr::summarise(rt.m = mean(rt,na.rm = T))

ggplot(memory_subj,
       aes(x = LikeMeFeature,
           y = rt.m,
           color = LikeMeFeature))+
  geom_jitter(alpha = 0.05)+
  #scale_y_continuous(trans='log2')+
  coord_flip()+
  theme_cowplot()+ 
  theme(legend.position = "none")+
  #geom_point(data = memory_means, size = 3)+
  ylim(c(1800,4500))+
  scale_x_discrete(limits=rev)+
  geom_crossbar(data = memory_means,aes(ymin = rt.m, ymax = rt.m), 
                width = 0.7)+
  geom_linerange(data = memory_means,aes(ymin = rt.m - ci.norm, 
                                         ymax = rt.m + ci.norm),
                 lwd = 1)+
  ylab("Time to choose name (ms)")+xlab("")



