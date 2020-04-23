setwd("C:/Users/aldos/Desktop/directed_research_satiation")
library(plyr)
library(dplyr)
library(reshape)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(brms)
data<-read.csv("satiation_baseline-trials.csv")

#Step 1: Filter out the participants who responded incorrectely to the practice questions:
practice_good_data=subset(data,condition == "practice_good")
practice_good_data=subset(practice_good_data,response >= 0.75)

practice_bad_data=subset(data,condition == "practice_bad")
practice_bad_data=subset(practice_bad_data,response <= 0.25)

eligible_worker_p = intersect(practice_good_data[,"workerid"], practice_bad_data[,"workerid"])
data=subset(data, is.element(workerid, eligible_worker_p))
fill <- c("FILL", "UNGRAM")
#alternative filter: average filler lower than average ungram
filler_data = subset(data, condition == "FILL")
ungram_data = subset(data, condition == "UNGRAM")
library(bootstrap)
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
   quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}

filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), ci.low)
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

data = subset (data, workerid < 40)


#calculate cumulative average
data = subset(data, item_number != "practice_good")
data = subset(data, item_number != "practice_bad")

ggplot(data, aes(x=trial_sequence_total, y=response)) + 
    
  geom_smooth(method = lm, se = F) + 
    theme_bw()


trial_avg <- aggregate(data[,"response"],list(data$trial_sequence_total), mean)


names(trial_avg)[names(trial_avg) == "Group.1"] <- "trial"
names(trial_avg)[names(trial_avg) == "x"] <- "avg"

ggplot(trial_avg, aes(x=trial, y=avg)) + 
  
  geom_smooth(method = lm, se = F) + geom_point()
  theme_bw()
  
trial_avg <- trial_avg[order(trial_avg$trial),]
cum <- cumsum(trial_avg$avg) / seq_along(trial_avg$avg) 
trial_avg$cum <- cum


ggplot(trial_avg, aes(x=trial, y=avg)) + 
  
  geom_smooth(method = lm, se = F) + geom_point()+
theme_bw()

ggplot(trial_avg, aes(x=trial, y=cum)) + 
  
  geom_smooth (se = F) + geom_point()+
theme_bw()















#Alternative#2: the highest in word salad is higher than lowest in gram filler.

filler_data = subset(data, condition == "FILL")
ungram_data = subset(data, condition == "UNGRAM")
filler_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), min)
ungram_by_subject = aggregate(ungram_data[,"response"],list(ungram_data$workerid), max)

names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_min"

names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_max"

all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")

eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_max < row$fill_min){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)










#Step 2: Filter out the participants whose average response to fillers are below 0.75
filler_data = subset(data, condition=="FILL")
filler_data_by_subject = aggregate(filler_data[,"response"],list(filler_data$workerid), mean)
filler_data_by_subject_eligible = subset(filler_data_by_subject, x>=0.75)
eligible_worker_f = filler_data_by_subject_eligible$Group.1
data=subset(data, is.element(workerid, eligible_worker_f))
#Step 3: Filter out the participants who responded >0.25 to ungrammatical controls
ungram_control = subset(data, condition =="UNGRAM")
to_be_removed = subset(ungram_control, response >0.25)
worker_to_be_removed = to_be_removed$workerid
`%notin%` <- Negate(`%in%`)
data = subset(data, workerid %notin% worker_to_be_removed)
#Step 4: Response time filter
data=subset(data, Answer.time_in_minutes >=5)

data = data %>%
  filter(condition %in% c("CNPC", "FILL", "WH", "SUBJ")) %>%
  droplevels()

#Step 5: Clean practice trials and control trials.
data = subset(data, block_sequence != "practice")
data = subset(data, condition != "UNGRAM")
#data = subset(data, condition != "FILL")
d=transform(data, block_sequence = as.numeric(block_sequence))
write.csv(d,"satiation_baseline_cleaned.csv", row.names = FALSE)
d <- read.csv("satiation_baseline_cleaned.csv")
d$condition <- factor(d$condition, levels = c("FILL", "CNPC","SUBJ","WH"))
#Step 5.5: remove non-English speakers:
non_Eng <- {}
d = subset(d, workerid %notin% non_Eng)


#look at subset of conditions
data = subset(data, condition !="CNPC")
data = subset(data, condition !="SUBJ")
data = subset(data, condition != "UNGRAM")
#Step 6: Statistics
model_block <- lmer(response~block_sequence*condition + (1+block_sequence*condition|workerid)+(1+condition|item_number), data = d)
summary(model_block)
data$condition <- factor(data$condition, levels = c("FILL", "CNPC","SUBJ","WH"))
model_global2 <- lmer(response~trial_sequence_total*condition + 
                       (1+trial_sequence_total*condition|workerid)+(1+trial_sequence_total*condition|item_number), data = data)
summary(model_global2)

str(data)
data$trial_sequence_total = as.numeric(as.character(data$trial_sequence_total))
model_global3 <- brm(response~trial_sequence_total*condition + 
                       (1+trial_sequence_total*condition|workerid)+(1+trial_sequence_total*condition|item_number), data)
summary(model_global3)


model_ext_class <- extend(model_global2, along="workerid", n=150)
model_ext_class
p_curve_treat <- powerCurve(model_ext_class, test = fcompare(response~trial_sequence_total*condition), along="workerid", breaks=c(50,100,150))
plot(p_curve_treat)
powerSim(model_global2, test=fcompare(response~trial_sequence_total*condition))


#Step 7: Plot
ggplot(d, aes(x=trial_sequence_total, y=response, color = condition, shape = condition)) + 
  geom_point() + 
  geom_smooth(method=lm, aes(fill=condition))+facet_wrap(~workerid)
ggsave("subject_variability.pdf", width=20, height = 25)

