# From Mike Frank
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #-

library(dplyr)

files <- list.files(path = "./full_empathy_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")
test = raw_data %>% # Read in data
  dplyr::filter(trial_type == "survey-likert-extra-labels" )%>%
  mutate(responses = gsub('{\"Q0\":','',
                          x=responses
                          ,fixed=T),
         responses = gsub('}','',
                          x=responses
                          ,fixed=T),
         responses = as.numeric(responses)-3,
         responses = ifelse(is.na(name_order), responses, ifelse(name_order == "right_to_left", responses,-1 *responses))
                            ) %>%
  mutate(condition = case_when(
    grepl("_active",verb, fixed = TRUE) ~ "active",
    grepl("_passive",verb, fixed = TRUE) ~ "passive",
    grepl("_conjoined",verb, fixed = TRUE) ~ "conjoined",
    TRUE ~ "catch"
  ))

#####
# Process worker ids
ids = raw_data %>% 
  dplyr::filter(test_part == "worker_id" ) %>%
  mutate(responses = gsub('{\"Q0\":"','',
                          x=responses
                          ,fixed=T),
         responses = gsub('"}','',
                          x=responses
                          ,fixed=T))

summary(as.factor(ids$responses)) # two participants completed twice for some reason
# Need to remove ptpts 62938_W and 83249_W who are both double completions (we keep their first completion)
# 

write_csv(data.frame(cbind(ids$id,ids$responses)), file = "ids.csv")

#####
# Process catch questions

catch = test %>% # Read in data
    dplyr::filter(verb %in% c("chase", "drive", "teach", "remind", "carry", "pose", "feed", "hire")) %>%
    mutate(correct = case_when(
      verb == "drive" & responses > 0 ~ 1,
      verb != "drive" & responses < 0 ~ 1,
      TRUE ~ 0
    ))

catch_ids = catch %>% 
  group_by(id) %>% 
  dplyr::summarise(correct = mean(correct)) %>% 
  filter(correct < 0.85)



# Exclude 56517_W based on catch trials

empathy_pilot_results = test %>%
  filter(!id  %in% catch_ids$id) %>%
  group_by(id, condition) %>%
  dplyr::summarise(empathy_with_first_mentioned = mean(responses)) %>%
  group_by(condition) %>%
  dplyr::summarise(empathy_with_first_mentioned.m = mean(empathy_with_first_mentioned),
                   empathy_with_first_mentioned.sd = sd(empathy_with_first_mentioned),
                   OverallLikeMe.lo = ci.low(empathy_with_first_mentioned),
                   OverallLikeMe.hi = ci.high(empathy_with_first_mentioned)) %>%
  filter(condition != "catch") %>%
  rename(syntax_unique = condition,
        OverallLikeMe.m = empathy_with_first_mentioned.m) %>%
  mutate(data_type = "3. Empathy Intuitions",
         Participant_Background = "English speakers")

  empathy_score_pilot  = ggplot(data = empathy_pilot_results,
                              aes(x = syntax_unique, y = OverallLikeMe.m, group = data_type)) + 
  #geom_hline(yintercept = 0.5,lty =2)+
  geom_line(color = "black")+
  geom_point(size = 3, color = "black") +
  geom_errorbar(aes(ymin= OverallLikeMe.lo,ymax = OverallLikeMe.hi), width = 0) +
  facet_grid(Participant_Background ~ data_type, scales = "free")+
  theme_cowplot()+ylim(c(-0.5,2)) + geom_hline(yintercept = 0, lty =2 )+
  xlab("")+ylab("Relative Empathy for \nfirst mentioned character") +
  theme(legend.position="bottom", 
        strip.background.y = element_blank(),
        strip.text.y = element_blank()
  )

plot_grid(LikeMe_score,agency_score, empathy_score_pilot,
          nrow = 3, labels = "AUTO")  

summary(lmer(responses ~ condition + (1+condition|id), data = test %>%
               filter(!id  %in% c("./pilot_testing/56517_W.csv", "./pilot_testing/62938_W.csv", "./pilot_testing/83249_W.csv") & condition != "catch")))
