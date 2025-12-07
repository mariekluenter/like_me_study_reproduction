# From Mike Frank
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)} #  mean(x,na.rm=na.rm) -
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap::bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) } #-

library(dplyr)
#GR:
library(readr)
library(ggplot2)
library(cowplot)

files <- list.files(path = "./full_empathy_data", pattern = "\\.csv$", full.names = T)
raw_data <- sapply(files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

#GR: raw data already anonymised

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
#GR: they are already removed from data folder 

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

# ELF: As far as I can see 3 participants got all catch trials wrong
catch %>% 
  group_by(id) %>% 
  dplyr::summarise(correct = mean(correct)) |> 
  View()

catch_ids = catch %>% 
  group_by(id) %>% 
  dplyr::summarise(correct = mean(correct)) %>% 
  filter(correct < 0.85) #GR -> 8 catch trials, 0.25= 2/8 (2 out of eight), so they excluded participants with 2 OR MORE (not more than two) incorrect catch trials -> 19 participants (including the 3 who answered all catch trials wrong)

# Exclude 56517_W based on catch trials #GR: this comment is not relevant?

#GR: the following code excludes the 19 participants (based on catch trials ids) and calculates participants' response means (summarize) -> = empathy pilot results
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

#GR: added data reproduction code:
write.csv(test, file = "empathy_data_GR.csv", row.names = FALSE)
empathy_study_data_original <- read_csv("empathy_study_data_original.csv")

length(unique(test$id)) #128 participants
length(unique(empathy_study_data_original$id)) #109 participants

test_ids <- test |> distinct(id)
original_ids <- empathy_study_data_original |> distinct(id)

supposed_df <- test |> 
  filter(!id  %in% catch_ids$id)
supposed_ids <- supposed_df |> distinct(id)

write_csv(supposed_df, file = "supposed_df.csv")
#GR: Successful recreation of the dataframe the authors used in the analysis. However, this procedure confirmed that the data used in the analysis was the data from the original 128 participant minus the 19 they excluded due to catch trials. Therefore, no other exclusion criteria were applied by the authors.
