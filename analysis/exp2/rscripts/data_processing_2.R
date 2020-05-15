#setwd("C:/Users/aldos/submiterator")
# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(brms)
`%notin%` <- Negate(`%in%`)
data<-read.csv("../raw_data/satiation_exp2-trials.csv")

# color-blind-friendly colors:
cbPalette = c("#d55e00", "#009e74","#e69d00","#cc79a7", "#0071b2")

#Step 1: Filter out the participants who responded incorrectely more than once to the practice questions:
practice_data=subset(data,block_sequence == "practice")
practice_bad_data=subset(practice_data, wrong_attempts > 1)
data=subset(data, workerid %notin% practice_bad_data$workerid)


#Step 2: filter: no overlap of 95%CI of FILL and UNGRAM

filler_data = subset(data, condition == "FILL")
ungram_data = subset(data, condition == "UNGRAM")

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), mean)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high.int)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_avg"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_avg"

all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_avg < row$fill_avg){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)

#Step 3: exclude non-English speakers
non_Eng <- c(142, 171, 240)

data = subset(data, workerid %notin% non_Eng)

#Step 4: exclude people who failed the attention questions
data_attention <- subset(data, comp_question_exist == 1)
data_attention$speaker_identity <- factor(data_attention$speaker_identity, levels = c("", "Emily", "Gregory", "Iron-Head", "Jessy", "Thomas", "/", "Forgot"))
data_attention$correct <- as.numeric(data_attention$speaker_identity == data_attention$comp_answer)

agg_comp <- aggregate(data_attention[,"correct"],list(data_attention$workerid), mean)
agg_comp <- subset(agg_comp, x > 0.75)
data <- subset(data, workerid %in% agg_comp$Group.1)



#Step 5: calculate and plot trial/cumulative average
data = subset(data, block_sequence != "practice")
trial_avg <- aggregate(data[,"response"],list(data$trial_sequence_total), mean)
names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
names(trial_avg)[names(trial_avg) == "x"] <- "avg"
#trial_average plot

trial_avg <- trial_avg[order(trial_avg$trial),]
cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg) 
trial_avg$cum <- cum

ggplot(trial_avg, aes(x=trial, y=avg)) + 
  geom_smooth(method = lm, se = F) + geom_point()+
  theme_bw()

ggsave("../graphs/by_trial_avg.pdf",width=5,height=5)

#cum_average plot
ggplot(trial_avg, aes(x=trial, y=cum)) + 
  
  geom_smooth (se = F) + geom_point()+
  theme_bw()
ggsave("../graphs/cum_avg.pdf",width=5,height=5)



#Step 6Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"satiation_exp2_cleaned.csv", row.names = FALSE)
d <- read.csv("satiation_exp2_cleaned.csv")
d$condition <- factor(d$condition, levels = c("FILL", "CNPC","SUBJ","WH","UNGRAM"))
#d <- subset(d, island_tested == test_cond)

exposure_data <- subset(d, phase == "exposure")
test_data <- subset(d, phase == "test")

#step 7: stats
library(optimx)
# cnpc_exposure <- subset(exposure_data, island_tested == "CNPC")
# subj_exposure <- subset(exposure_data, island_tested == "SUBJ")
# wh_exposure <- subset(exposure_data, island_tested == "WH")
# # 
#    model_exposure <- lmer(response~trial_sequence_total*condition + 
#                                  (1 + trial_sequence_total|workerid)+
#                                (1+trial_sequence_total*condition|item_number), 
#                           data = exposure_data, verbose=100, control = lmerControl(calc.derivs = FALSE))
#    summary(model_exposure)

# model_test <- lmer(response~test_match_cond*condition + (1|workerid)+(1+test_match_cond*condition|item_number), data = test_data)
# summary(model_test)

#step8: plot
# summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
#                       conf.interval=.95, .drop=TRUE) {
#   library(plyr)
#   
#   # New version of length which can handle NA's: if na.rm==T, don't count them
#   length2 <- function (x, na.rm=FALSE) {
#     if (na.rm) sum(!is.na(x))
#     else       length(x)
#   }
#   
#   # This does the summary. For each group's data frame, return a vector with
#   # N, mean, and sd
#   datac <- ddply(data, groupvars, .drop=.drop,
#                  .fun = function(xx, col) {
#                    c(N    = length2(xx[[col]], na.rm=na.rm),
#                      mean = mean   (xx[[col]], na.rm=na.rm),
#                      sd   = sd     (xx[[col]], na.rm=na.rm)
#                    )
#                  },
#                  measurevar
#   )
#   
#   # Rename the "mean" column    
#   datac <- rename(datac, c("mean" = measurevar))
#   
#   datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
#   
#   # Confidence interval multiplier for standard error
#   # Calculate t-statistic for confidence interval: 
#   # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
#   ciMult <- qt(conf.interval/2 + .5, datac$N-1)
#   datac$ci <- datac$se * ciMult
#   
#   return(datac)
# }
# df2 <- summarySE(test_data, measurevar="response", 
#                  groupvars=c("condition", "test_match_cond"))
# head(df2)
# 
# agg_test <- aggregate(test_data[,"response"], by= list(test_data$workerid, test_data$condition, test_data$test_match_cond), FUN = mean, na.rm=TRUE)
# 
# names(agg_test)[names(agg_test) == "Group.1"] <- "workerid"
# 
# names(agg_test)[names(agg_test) == "Group.2"] <- "condition"
# 
# names(agg_test)[names(agg_test) == "Group.3"] <- "test_match_cond"
# 
# names(agg_test)[names(agg_test) == "x"] <- "response"
# 
# p<- ggplot(agg_test, aes(x=condition, y=response, fill=test_match_cond)) + 
#   geom_bar(data = df2, stat="identity", color="black", 
#            position=position_dodge()) +
#   geom_errorbar(data = df2, aes(ymin=response-se, ymax=response+se), width=.2,
#              position=position_dodge(.9)) + theme_bw() + geom_jitter(aes(x=condition, y=response, color=test_match_cond), alpha = .4)
# 
# p

test_data = test_data %>%
  mutate(sentence_type = fct_recode(condition,"grammatical"="FILL","ungrammatical"="UNGRAM")) %>%
  mutate(sentence_type = fct_relevel(sentence_type,"grammatical","WH","SUBJ","CNPC"))

means = test_data %>%
  group_by(test_match_cond,sentence_type) %>%
  summarize(Mean = mean(response), CILow = ci.low(response), CIHigh = ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

subj_means = test_data %>%
  group_by(test_match_cond,sentence_type,workerid) %>%
  summarize(Mean = mean(response)) %>%
  ungroup() 

dodge = position_dodge(.9)

ggplot(means, aes(x=sentence_type,y=Mean,fill=test_match_cond)) +
  geom_bar(stat="identity",position=dodge,color="black") +
  geom_point(data=subj_means,alpha=.2,position=dodge,color="gray30") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=dodge) +
  scale_fill_manual(values=cbPalette,name="Test condition") +
  xlab("Sentence type") +
  ylab("Mean acceptability rating")
ggsave("../graphs/test_means.pdf",width=5.5,height=3)

#overall plot:
trial_means = exposure_data %>%
  group_by(condition,trial_sequence_total) %>%
  summarize(response = mean(response)) %>%
  ungroup() 

ggplot(exposure_data, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point(data=trial_means,alpha=.9) + 
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  xlab("Presentation Order") +
  ylab("Acceptability rating")+
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

ggsave("../graphs/exposure_phase.pdf",width=5,height=2.5)


#-subject Plot
#  ggplot(exposure_data, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
#   geom_point() + 
#    geom_smooth(method=lm, aes(fill=condition))+facet_wrap(~workerid)
# ggsave("../graphs/subject_variability.pdf", width=20, height = 25)

###