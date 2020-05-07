setwd("C:/Users/aldos/submiterator")
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(brms)
`%notin%` <- Negate(`%in%`)
data<-read.csv("satiation_exp2-trials.csv")
test_cond = "CNPC"


#Step 1: Filter out the participants who responded incorrectely more than once to the practice questions:
practice_data=subset(data,block_sequence == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
data=subset(data, is.element(workerid, practice_good_data$workerid))


#Step 2: filter: no overlap of 95%CI of FILL and UNGRAM

filler_data = subset(data, condition == "FILL")
ungram_data = subset(data, condition == "UNGRAM")
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), mean)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), ci.high)

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
non_Eng <- c()

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

a= ggplot(trial_avg, aes(x=trial, y=avg)) + 
  geom_smooth(method = lm, se = F) + geom_point()+
  theme_bw()

#cum_average plot
b= ggplot(trial_avg, aes(x=trial, y=cum)) + 
  
  geom_smooth (se = F) + geom_point()+
  theme_bw()



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
model_test <- lmer(response~test_match_cond*condition + 
                       (1 + condition |workerid)+(1|item_number), data = test_data)
summary(model_test)


#step8: plot
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

df2 <- data_summary(test_data, varname="response", 
                    groupnames=c("condition", "test_match_cond"))
head(df2)

p<- ggplot(df2, aes(x=condition, y=response, fill=test_match_cond)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=response-sd, ymax=response+sd), width=.2,
                position=position_dodge(.9)) 

p
#overall plot:
c= ggplot(exposure_data, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point() + 
  geom_smooth(method=lm, aes(fill=condition))+theme_bw()

c
#by-subject Plot
ggplot(exposure_data, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point() + 
  geom_smooth(method=lm, aes(fill=condition))+facet_wrap(~workerid)
ggsave("subject_variability_2_exposure_phase.pdf", width=20, height = 25)

###
subj = subset(test_data, island_tested =="SUBJ")

cnpc = subset(test_data, island_tested =="CNPC")

wh = subset(test_data, island_tested =="WH")
