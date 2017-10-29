# This code provides the descriptives and permits running the linear mixed effects models for the experiments reported in 
# Galati, A., Diavastou, A., & Avraamides, M. N. (2017). Signatures of cognitive difficulty in perspective-taking: Is the egocentric perspective always the easiest to adopt?. Language, Cognition, and Neuroscience. 
# DOI: 10.1080/23273798.2017.1384029

# Written by A. Galati (UC Merced, U Cyprus)
# Last date moditfied 28 October 2017


# Load libraries
library(pander)
library(lme4)
library(gdata)

# Set your working directory
setwd("./difficulty_signatures_perspectivetaking/Code/")
source('printStats.R') # this contains some functions we'll need (print stats)

#################################
###### Prelininaries  ##########
###############################

# Load Masterfile for Experiments 1 & 2
# Check your path to the file's location
Masterfile = read.xls('./difficulty_signatures_perspectivetaking/Data_Files/GDA_Masterfile_Exp1_Exp2.xlsx', na.strings=c("NA", "#NULL!")) 

# Load Masterfile for Experiment 2B
Masterfile2B = read.xls('./difficulty_signatures_perspectivetaking/Data_Files/GDA_Masterfile_Exp2B.xlsx', na.strings=c("NA", "#NULL!")) 

# Load Masterfile for Experiment 3
Masterfile3 = read.xls('./difficulty_signatures_perspectivetaking/Data_Files/GDA_Masterfile_Exp3.xlsx', na.strings=c("NA", "#NULL!")) 


#####################################
##### SOME DATA PROCESSING #########
#####################################

# Create separate data frames for ambiguous and control trials for each experiment

#for Experiment 1
resAll_Exp1 = subset(Masterfile, Masterfile$trial_type == "ambiguous"& Exp==1) 
resAllError_Exp1 = subset(Masterfile, Masterfile$trial_type == "same"& Exp==1) 

#for Experiment 2
resAll_Exp2 = subset(Masterfile, Masterfile$trial_type == "ambiguous"& Exp==2) 
resAllError_Exp2 = subset(Masterfile, Masterfile$trial_type == "same"& Exp==2) 

#for Experiment 2B
resAll_Exp2B = subset(Masterfile2B, Masterfile2B$trial_type == "ambiguous") 
resAllError_Exp2B = subset(Masterfile2B, Masterfile2B$trial_type == "same") 

#for Experiment 3
resAll_Exp3 = subset(Masterfile3, Masterfile3$trial_type == "ambiguous") 
resAllError_Exp3 = subset(Masterfile3, Masterfile3$trial_type == "same")


# Create an aggregate variable to see distribution of egocentrism across subjects

# For Experiment 1
egoChosen_Exp1 = 1*(resAll_Exp1$Ego==1)
perspectiveDistribution_Exp1 = aggregate(egoChosen_Exp1~resAll_Exp1$Subject,FUN=mean)
# Inspect histogram of proportion of egocentric responding for Exp1
hist(perspectiveDistribution_Exp1$egoChosen_Exp1,100, main = paste("Histogram of", "proportion of egocentrism in Exp 1"), xlab='Proportion of egocentrism',ylab='Number of subjects')

# For Experiment 2
egoChosen_Exp2 = 1*(resAll_Exp2$Ego==1)
perspectiveDistribution_Exp2 = aggregate(egoChosen_Exp2~resAll_Exp2$Subject,FUN=mean)

# Inspect histogram of proportion of egocentric responding for Exp2
hist(perspectiveDistribution_Exp2$egoChosen_Exp2,100, main = paste("Histogram of", "proportion of egocentrism in Exp 2"), xlab='Proportion of egocentrism',ylab='Number of subjects')

# For Experiment 2B
egoChosen_Exp2B = 1*(resAll_Exp2B$Ego==1)
perspectiveDistribution_Exp2B = aggregate(egoChosen_Exp2B~resAll_Exp2B$Subject,FUN=mean)

# Inspect histogram of proportion of egocentric responding for Exp2B
hist(perspectiveDistribution_Exp2B$egoChosen_Exp2B,100, main = paste("Histogram of", "proportion of egocentrism in Exp 2B"), xlab='Proportion of egocentrism',ylab='Number of subjects')

# For Experiment 3
egoChosen_Exp3 = 1*(resAll_Exp3$Ego==1)
perspectiveDistribution_Exp3 = aggregate(egoChosen_Exp3~resAll_Exp3$Subject,FUN=mean)

# Inspect histogram of proportion of egocentric responding for Exp3
hist(perspectiveDistribution_Exp3$egoChosen_Exp3,100, main = paste("Histogram of", "proportion of egocentrism in Exp 3"), xlab='Proportion of egocentrism',ylab='Number of subjects')


# Construct perspective preference variables

# For Experiment 1
egoSubjects_Exp1 = perspectiveDistribution_Exp1[perspectiveDistribution_Exp1$egoChosen_Exp1>.7,]$resAll_Exp1
otherSubjects_Exp1 = perspectiveDistribution_Exp1[perspectiveDistribution_Exp1$egoChosen_Exp1<.3,]$resAll_Exp1
mixedSubjects_Exp1 = perspectiveDistribution_Exp1[(perspectiveDistribution_Exp1$egoChosen_Exp1>=.3 & perspectiveDistribution_Exp1$egoChosen_Exp1<=.7) ,]$resAll_Exp1

# Get number of each type of responder
#length(egoSubjects_Exp1)
#length(otherSubjects_Exp1)
#length(mixedSubjects_Exp1)
#length(unique(resAll_Exp1$Subject))

# For Experiment 2
egoSubjects_Exp2 = perspectiveDistribution_Exp2[perspectiveDistribution_Exp2$egoChosen_Exp2>.7,]$resAll_Exp2
otherSubjects_Exp2 = perspectiveDistribution_Exp2[perspectiveDistribution_Exp2$egoChosen_Exp2<.3,]$resAll_Exp2
mixedSubjects_Exp2 = perspectiveDistribution_Exp2[(perspectiveDistribution_Exp2$egoChosen_Exp2>=.3 & perspectiveDistribution_Exp2$egoChosen_Exp2<=.7) ,]$resAll_Exp2

# Get number of each type of responder
#length(egoSubjects_Exp2)
#length(otherSubjects_Exp2)
#length(mixedSubjects_Exp2)
#length(unique(resAll_Exp2$Subject))

# For Experiment 2B
egoSubjects_Exp2B = perspectiveDistribution_Exp2B[perspectiveDistribution_Exp2B$egoChosen_Exp2B>.7,]$resAll_Exp2B
otherSubjects_Exp2B = perspectiveDistribution_Exp2B[perspectiveDistribution_Exp2B$egoChosen_Exp2B<.3,]$resAll_Exp2B
mixedSubjects_Exp2B = perspectiveDistribution_Exp2B[(perspectiveDistribution_Exp2B$egoChosen_Exp2B>=.3 & perspectiveDistribution_Exp2B$egoChosen_Exp2B<=.7) ,]$resAll_Exp2B

# Get number of each type of responder
#length(egoSubjects_Exp2B)
#length(otherSubjects_Exp2B)
#length(mixedSubjects_Exp2B)
#length(unique(resAll_Exp2B$Subject))

# For Experiment 3
egoSubjects_Exp3 = perspectiveDistribution_Exp3[perspectiveDistribution_Exp3$egoChosen_Exp3>.7,]$resAll_Exp3
otherSubjects_Exp3 = perspectiveDistribution_Exp3[perspectiveDistribution_Exp3$egoChosen_Exp3<.3,]$resAll_Exp3
mixedSubjects_Exp3 = perspectiveDistribution_Exp3[(perspectiveDistribution_Exp3$egoChosen_Exp3>=.3 & perspectiveDistribution_Exp3$egoChosen_Exp3<=.7) ,]$resAll_Exp3

# Get number of each type of responder
#length(egoSubjects_Exp3)
#length(otherSubjects_Exp3)
#length(mixedSubjects_Exp3)
#length(unique(resAll_Exp3$Subject))


### Insert the perspective preference labels in the dataframes ###

# For Exp 1 

#Label perspectivePreference levels for Ambiguous trials
resAll_Exp1$perspectivePreference = 'mixed'
resAll_Exp1$perspectivePreference[resAll_Exp1$Subject %in% egoSubjects_Exp1]='ego'
resAll_Exp1$perspectivePreference[resAll_Exp1$Subject %in% otherSubjects_Exp1]='other'

#for Control trials let's trasfer the perspectivePreference variable over 
resAllError_Exp1$perspectivePreference = 'mixed'
resAllError_Exp1$perspectivePreference[resAllError_Exp1$Subject %in% egoSubjects_Exp1]='ego'
resAllError_Exp1$perspectivePreference[resAllError_Exp1$Subject %in% otherSubjects_Exp1]='other'

# For Exp 2 

resAll_Exp2$perspectivePreference = 'mixed'
resAll_Exp2$perspectivePreference[resAll_Exp2$Subject %in% egoSubjects_Exp2]='ego'
resAll_Exp2$perspectivePreference[resAll_Exp2$Subject %in% otherSubjects_Exp2]='other'

#for Control trials let's trasfer the perspectivePreference variable over 
resAllError_Exp2$perspectivePreference = 'mixed'
resAllError_Exp2$perspectivePreference[resAllError_Exp2$Subject %in% egoSubjects_Exp2]='ego'
resAllError_Exp2$perspectivePreference[resAllError_Exp2$Subject %in% otherSubjects_Exp2]='other'

# For Exp 2B 

resAll_Exp2B$perspectivePreference = 'mixed'
resAll_Exp2B$perspectivePreference[resAll_Exp2B$Subject %in% egoSubjects_Exp2B]='ego'
resAll_Exp2B$perspectivePreference[resAll_Exp2B$Subject %in% otherSubjects_Exp2B]='other'

#for Control trials let's trasfer the perspectivePreference variable over 
resAllError_Exp2B$perspectivePreference = 'mixed'
resAllError_Exp2B$perspectivePreference[resAllError_Exp2B$Subject %in% egoSubjects_Exp2B]='ego'
resAllError_Exp2B$perspectivePreference[resAllError_Exp2B$Subject %in% otherSubjects_Exp2B]='other'

# For Exp 3 

resAll_Exp3$perspectivePreference = 'mixed'
resAll_Exp3$perspectivePreference[resAll_Exp3$Subject %in% egoSubjects_Exp3]='ego'
resAll_Exp3$perspectivePreference[resAll_Exp3$Subject %in% otherSubjects_Exp3]='other'

#for Control trials let's trasfer the perspectivePreference variable over 
resAllError_Exp3$perspectivePreference = 'mixed'
resAllError_Exp3$perspectivePreference[resAllError_Exp3$Subject %in% egoSubjects_Exp3]='ego'
resAllError_Exp3$perspectivePreference[resAllError_Exp3$Subject %in% otherSubjects_Exp3]='other'

##############################################################
### Combine the data frames of experiments being compared ###
#############################################################

###### COMBINE Exp 1 & Exp 2 ###### 

# for ambiguous trials
resAll = rbind(resAll_Exp1, resAll_Exp2)
#head(resAll)
#tail(resAll)

# for control trials
resAllError = rbind(resAllError_Exp1, resAllError_Exp2)
#head(resAllError)
#tail(resAllError)

###### COMBINE Exp 2 & Exp 2B ###### 

#before doing that let's remove the column TrialList.Sample from Exp 2, since it's missing from Exp 2B, 
#so that we can combine the two dataframes
resAll_Exp2$TrialList.Sample <- NULL
resAllError_Exp2$TrialList.Sample <- NULL

# for ambiguous trials
resAll22B = rbind(resAll_Exp2, resAll_Exp2B)
#head(resAll22B)
#tail(resAll22B)

# for control trials
resAllError22B = rbind(resAllError_Exp2, resAllError_Exp2B)
#head(resAllError22B)
#tail(resAllError22B)

###############################################
#### VARIABLE RECODING FOR AXIS AND OFFSET ####
###############################################

#Let's create axis variable to compress front-back and left-right instuctions into a sagittal and lateral axis
#See paper for our theoretical motivation for doing so
resAll$instruction_axis = 'sagittal'
resAll$instruction_axis[resAll$instruction %in% c('right','left')]='lateral'

#Let's create an offset type variable, that captures oblique (135, 225) vs. orthogonal (90, 180, 270) offsets
resAll$offset_type = 'orthogonal'
resAll$offset_type[resAll$speaker_position %in% c('135','225')]='oblique'

#Let's do this for control trials too.
resAllError$instruction_axis = 'sagittal'
resAllError$instruction_axis[resAllError$instruction %in% c('righ','left')]='lateral'

#Let's do this for Experiment 2B too. 
resAll_Exp2B$instruction_axis = 'sagittal'
resAll_Exp2B$instruction_axis[resAll_Exp2B$instruction %in% c('right','left')]='lateral'

resAll_Exp2B$offset_type = 'orthogonal'
resAll_Exp2B$offset_type[resAll_Exp2B$speaker_position %in% c('135','225')]='oblique'

#Let's do this for Experiment 2B too. 
resAllError_Exp2B$instruction_axis = 'sagittal'
resAllError_Exp2B$instruction_axis[resAllError_Exp2B$instruction %in% c('righ','left')]='lateral'

#Let's do this for Experiment 3, but only for instruction
#Note that for Experiment 3, we don't examine the difference between oblique and orthogonal offsets
#Instead, we look at performance across all offsets

resAll_Exp3$instruction_axis = 'sagittal'
resAll_Exp3$instruction_axis[resAll_Exp3$instruction %in% c('right','left')]='lateral'

resAllError_Exp3$instruction_axis = 'sagittal'
resAllError_Exp3$instruction_axis[resAllError_Exp3$instruction %in% c('righ','left')]='lateral'


###############################################
### SOME DESCRIPTIVES for AMBIGUOUS trials ###
###############################################

#####################
##### EXP 1 & 2 #####
#####################

# Let's focus on just Exp and perspective preference
pander(aggregate(RT~perspectivePreference+Exp,data=resAll,FUN=mean))
#pander(aggregate(RT~perspectivePreference+Exp,data=resAll,FUN=sd)) #get standard deviation

#get standard errors
#st.err <- function(x) {
#  sd(x)/sqrt(length(x))
#}
#pander(aggregate(RT~perspectivePreference+Exp,data=resAll, st.err)) #get standard error

# Remove factors as needed to simplify table
pander(aggregate(RT~offset_type+instruction_axis+perspectivePreference+Exp,data=resAll,FUN=mean))
pander(aggregate(Ego~offset_type+instruction_axis+Exp,data=resAll,FUN=mean)) #proportion egocentrism across speaker positions


#####################
####### EXP 2B ######
#####################

# Remove factors as needed to simplify table
pander(aggregate(RT~objects_no+offset_type+instruction_axis+perspectivePreference,data=resAll_Exp2B,FUN=mean))
pander(aggregate(Ego~objects_no+offset_type+instruction_axis,data=resAll_Exp2B,FUN=mean)) #proportion egocentrism across speaker positions


#####################
####### EXP 3 #######
#####################

# Remove factors as needed to simplify table
pander(aggregate(RT~listener_position+instruction_axis+perspectivePreference,data=resAll_Exp3,FUN=mean))
pander(aggregate(Ego~listener_position+instruction_axis,data=resAll_Exp3,FUN=mean)) #proportion egocentrism across speaker positions

#get also other-centric responses by position for Table 3 of the manuscript
#resAll_Exp3$Other = as.numeric(as.matrix(resAll_Exp3$Other))
#pander(aggregate(Other~listener_position+perspectivePreference,data=resAll_Exp3,FUN=mean))

#get mean proportions for all participants
#egoChosen_Exp3 = 1*(resAll_Exp3$Ego==1)
#mean(egoChosen_Exp3)
#sd(egoChosen_Exp3)
#otherChosen_Exp3 = 1*(resAll_Exp3$Other==1)
#mean(otherChosen_Exp3)
#sd(otherChosen_Exp3)
#mean(resAll_Exp3$RT)
#sd(resAll_Exp3$RT)

############################################
### SOME DESCRIPTIVES for CONTROL trials ###
############################################

#####################
##### EXP 1 & 2 #####
#####################

# Remove factors as needed to simplify table
pander(aggregate(RT~instruction_axis+speaker_position+perspectivePreference+Exp,data=resAllError,FUN=mean))
pander(aggregate(Ego~instruction_axis+speaker_position+perspectivePreference+Exp,data=resAllError,FUN=mean)) #proportion correct


#####################
####### EXP 2B ######
#####################

#Set Ego back to numeric to get means (in case it it had been converted to factor for lmers)
#resAllError_Exp2B$Ego = as.numeric(as.matrix(resAllError_Exp2B$Ego))

# Remove factors as needed to simplify table
pander(aggregate(RT~instruction_axis+speaker_position+perspectivePreference,data=resAllError_Exp2B,FUN=mean))
pander(aggregate(Ego~instruction_axis+speaker_position+perspectivePreference,data=resAllError_Exp2B,FUN=mean)) #proportion correct


#####################
####### EXP 3 #######
#####################

#Set Ego back to numeric to get means (in case it it had been converted to factor for lmers)
resAllError_Exp3$Ego = as.numeric(as.matrix(resAllError_Exp3$Ego))

# Remove factors as needed to simplify table
pander(aggregate(RT~instruction_axis+speaker_position+perspectivePreference,data=resAllError_Exp3,FUN=mean))
pander(aggregate(Ego~instruction_axis+speaker_position+perspectivePreference,data=resAllError_Exp3,FUN=mean)) #proportion correct


###############################
### Experiment COMPARISONS ###
###############################

# Before getting to the LMERs Let's do some chi-squares to compare distributions across exps # 

#######################
##### EXP 1 vs. 2 #####
#######################

#compare the distribution of other, ego, mixed responders in Exp 1 vs. Exp 2
#Recall in Exp 1 there were 25 ego, 1 other, 0 mixed
#In Exp 2 there were 14 ego, 8 other, 4 mixed
preferenceCounts <- matrix(c(25, 1, 0, 14, 8, 4), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("ego", "other", "mixed")
rownames(preferenceCounts) <- c("Exp1", "Exp2")
preferenceCounts <- as.table(preferenceCounts)

summary(preferenceCounts)
chisq.test(preferenceCounts) #Same as what we reported in the text!


########################
##### EXP 2 vs. 2B #####
########################

#compare the distribution of other, ego, mixed responders in Exp 2 vs. Exp 2B
#Recall in Exp 2 there were 14 ego, 8 other, 4 mixed
#in Exp 2B there were 18 ego, 5 other, 2 mixed  
preferenceCounts <- matrix(c(14, 8, 4, 18, 5, 2), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("ego", "other", "mixed")
rownames(preferenceCounts) <- c("Exp2", "Exp2B")
preferenceCounts <- as.table(preferenceCounts)

summary(preferenceCounts)
chisq.test(preferenceCounts) 

########################
##### EXP 2B vs. 3 #####
########################

#compare the distribution of other, ego, mixed responders in Exp 2B vs. Exp 3
#Recall in Exp 2B there were 18 ego, 5 other, 2 mixed  
#In Exp 2B there were 18 ego, 3 other, 3 mixed
preferenceCounts <- matrix(c(18, 5, 2, 18, 3, 3), ncol=3, byrow=TRUE)
colnames(preferenceCounts) <- c("ego", "other", "mixed")
rownames(preferenceCounts) <- c("Exp2B", "Exp3")
preferenceCounts <- as.table(preferenceCounts)

summary(preferenceCounts)
chisq.test(preferenceCounts)

###############################
#### MIXED EFFECTS MODELS ####
##############################

##########################################################
#### LMER MODELS FOR AMBIGUOUS TRIALS FOR EXPS 1 & 2 ####
##########################################################

resAll = as.data.frame(as.matrix(resAll))

# Defining as factors in order to set reference categories next
resAll$Exp = as.factor(as.matrix(resAll$Exp))
resAll$speaker_position = as.factor(as.matrix(resAll$speaker_position))
resAll$offset_type = as.factor(as.matrix(resAll$offset_type))
resAll$instruction = as.factor(as.matrix(resAll$instruction))
resAll$instruction_axis = as.factor(as.matrix(resAll$instruction_axis))
resAll$perspectivePreference = as.factor(as.matrix(resAll$perspectivePreference))
resAll$objects_no = as.factor(as.matrix(resAll$objects_no))

# Setting reference categories
resAll$speaker_position = relevel(resAll$speaker_position, ref = "180") #we can't do the sawtooth contrast here but we can compare to 180 degrees
resAll$perspectivePreference= relevel(resAll$perspectivePreference, ref = "ego")

# Check for any "holes" in the design
with(resAll, table(Exp, speaker_position, instruction, perspectivePreference))
with(resAll, table(Exp, speaker_position, instruction, perspectivePreference, objects_no))
# This reveals that there are:
# no observations for 2-object configs at obliques (135, 225)
# no observations for 3-object configs at orthogonal (90, 180, 270)
# no observations of back instructions at 90 (these were removed/lost due to some coding or data collection error)
# no mixed responders in Exp 1 (but that is a between subs factor)

with(resAll, table(Exp, offset_type, instruction_axis, perspectivePreference))
# In contrast to the above, this matrix was no holes, except for the lack of mixed responders in Exp 1
# The factors in this table are thefore well motivated as fixed effects

#make sure DVs are of the appropriate type
resAll$RT = as.numeric(as.matrix(resAll$RT))
resAll$Ego = as.factor(as.matrix(resAll$Ego))
#resAll$Ego = as.integer(as.matrix(resAll$Ego))
#resAll$Ego = as.numeric(as.matrix(resAll$Ego))

str(resAll) #check that variables are of the correct type

contrasts(resAll$Exp) <- c(-0.5, 0.5)
contrasts(resAll$offset_type) <- c(-0.5, 0.5)
contrasts(resAll$instruction_axis) <- c(-0.5, 0.5)
#contrasts(resAll$objects_no) <- c(-0.5, 0.5)


# Considerations for model building: 
# 1. Running models that include object_no as a factor resulted in rank deficiency, so it was removed  
# (see "holes" in the matrix above)
# 2. To simplify the models, we run models with axis (with only 2 levels) vs. instruction (with 4 levels). 
# Including instruction would be problematic given the "hole" identified above, of no "back" observations at 90.

#####################################
##### RT MODELS of EXP 1 & 2 #######
####################################

RTModel1 = lmer(log(RT) ~ Exp+perspectivePreference*offset_type*instruction_axis #use log-transformed RT given visual inspection below
                + (1 | Subject) 
                + (0 + offset_type | Subject)
                + (0 + instruction_axis | Subject), 
                #+ (0 + offset_type:instruction_axis | Subject), #does not convege
                data=resAll, 
                REML=FALSE)
print('RT:'); pander(print_stats(RTModel1))

#check residuals (before and after log transform)
#there is deviation from normality and homoscedasticity, so use log(RT)
#plot(density(resid(RTModel2))) #does this look approximately normal?
#qqnorm(resid(RTModel2)) #check if they fall on a straight line
#qqline(resid(RTModel2)) #check departure from line

# Run the same model without perspective preference and do an ANOVA comparison to get its main effect
RTModel1_noPref = lmer(log(RT) ~ Exp+offset_type*instruction_axis #use log-transformed RT given visual inspection below
                + (1 | Subject) 
                + (0 + offset_type | Subject)
                + (0 + instruction_axis | Subject),
                data=resAll, 
                REML=FALSE)
print('RT:'); pander(print_stats(RTModel1_noPref))

anova(RTModel1, RTModel1_noPref)


######## LMERS Exp 1 and Exp 2, separetely #########

### RT model for Exp 1 only ###

#Let's add instruction axis and offset type as variables in the resAll_Exp1 data frame
resAll_Exp1$instruction_axis = 'sagittal'
resAll_Exp1$instruction_axis[resAll_Exp1$instruction %in% c('right','left')]='lateral'

#let's create an offset type variable, that captures oblique (135, 225) vs. orthogonal (90, 180, 270) offsets
resAll_Exp1$offset_type = 'orthogonal'
resAll_Exp1$offset_type[resAll_Exp1$speaker_position %in% c('135','225')]='oblique'

resAll_Exp1 = as.data.frame(as.matrix(resAll_Exp1))
resAll_Exp1$offset_type = as.factor(as.matrix(resAll_Exp1$offset_type))
resAll_Exp1$instruction_axis = as.factor(as.matrix(resAll_Exp1$instruction_axis))
resAll_Exp1$perspectivePreference = as.factor(as.matrix(resAll_Exp1$perspectivePreference))

resAll_Exp1$RT = as.numeric(as.matrix(resAll_Exp1$RT))
resAll_Exp1$Ego = as.factor(as.matrix(resAll_Exp1$Ego))

contrasts(resAll_Exp1$offset_type) <- c(-0.5, 0.5)
contrasts(resAll_Exp1$instruction_axis) <- c(-0.5, 0.5)

RTModel_Exp1 = lmer(log(RT) ~ perspectivePreference*offset_type*instruction_axis #use log-transformed RT given visual inspection below
                + (1 | Subject) 
                + (0 + offset_type | Subject)
                + (0 + instruction_axis | Subject),
                data=resAll_Exp1, 
                REML=FALSE)
print('RT:'); pander(print_stats(RTModel_Exp1))

### RT model for Exp 2 only ###

#Let's add instruction axis and offset type as variables in the resAll_Exp2 data frame
resAll_Exp2$instruction_axis = 'sagittal'
resAll_Exp2$instruction_axis[resAll_Exp2$instruction %in% c('right','left')]='lateral'

#let's create an offset type variable, that captures oblique (135, 225) vs. orthogonal (90, 180, 270) offsets
resAll_Exp2$offset_type = 'orthogonal'
resAll_Exp2$offset_type[resAll_Exp2$speaker_position %in% c('135','225')]='oblique'

resAll_Exp2 = as.data.frame(as.matrix(resAll_Exp2))
resAll_Exp2$offset_type = as.factor(as.matrix(resAll_Exp2$offset_type))
resAll_Exp2$instruction_axis = as.factor(as.matrix(resAll_Exp2$instruction_axis))
resAll_Exp2$perspectivePreference = as.factor(as.matrix(resAll_Exp2$perspectivePreference))

resAll_Exp2$RT = as.numeric(as.matrix(resAll_Exp2$RT))
resAll_Exp2$Ego = as.factor(as.matrix(resAll_Exp2$Ego))

contrasts(resAll_Exp2$offset_type) <- c(-0.5, 0.5)
contrasts(resAll_Exp2$instruction_axis) <- c(-0.5, 0.5)

RTModel_Exp2 = lmer(log(RT) ~ perspectivePreference*offset_type*instruction_axis #use log-transformed RT given visual inspection below
                    + (1 | Subject) 
                    + (0 + offset_type | Subject),
                    #+ (0 + instruction_axis | Subject), #model does not converge
                    data=resAll_Exp2, 
                    REML=FALSE)
print('RT:'); pander(print_stats(RTModel_Exp2))

#####################################
##### CHOICE MODELS EXP 1 & 2 #######
####################################

EgoChoiceModel1 = glmer(Ego ~ Exp + offset_type*instruction_axis #without perspectivePreference in this model
                       + (1 | Subject) 
                       + (0 + offset_type | Subject)
                       + (0 + instruction_axis | Subject),
                       #+ (0 + offset_type:instruction_axis |Subject), #does not converge
                       data=resAll, 
                       family="binomial", 
                       method="Laplace",
                       REML=FALSE)
summary(EgoChoiceModel1)


##################################################
#### LMERS FOR CONTROL TRIALS FOR EXPS 1 & 2 ####
##################################################

resAllError = as.data.frame(as.matrix(resAllError))
# Defining as factors in order to set reference categories next
resAllError$Exp = as.factor(as.matrix(resAllError$Exp))
resAllError$speaker_position = as.factor(as.matrix(resAllError$speaker_position)) #recall, there's no offset type for control trials
resAllError$instruction = as.factor(as.matrix(resAllError$instruction))
resAllError$instruction_axis = as.factor(as.matrix(resAllError$instruction_axis))
resAllError$perspectivePreference = as.factor(as.matrix(resAllError$perspectivePreference))
resAllError$objects_no = as.factor(as.matrix(resAllError$objects_no)) #note, there are only 2 object trials for control trials here

# Setting reference categories
resAllError$speaker_position = relevel(resAllError$speaker_position, ref = "0") #let's compare 90 and 270 to 0 degrees
resAllError$perspectivePreference= relevel(resAllError$perspectivePreference, ref = "ego")

# Check for any "holes" in the design
with(resAllError, table(Exp, speaker_position, instruction, perspectivePreference)) #this is fine, except for the missing mixed responders in Exp 1
with(resAllError, table(Exp, speaker_position, instruction_axis, perspectivePreference)) 
# Make sure DVs are the right format
resAllError$RT = as.numeric(as.matrix(resAllError$RT))
resAllError$Ego = as.numeric(as.matrix(resAllError$Ego))

str(resAllError) #check that variables are of the correct type


contrasts(resAllError$Exp) <- c(-0.5, 0.5)
contrasts(resAllError$instruction_axis) <- c(-0.5, 0.5)
#contrasts(resAllError$objects_no) <- c(-0.5, 0.5)

#######################################
##### CORRECT MODELS EXP 1 & 2 #######
######################################

CorrectChoiceModel1 = lmer(Ego ~ Exp+speaker_position+instruction_axis # removed interactions as model fails to converge
                       + (1 | Subject), 
                       #+ (0 + speaker_position | Subject), #failed to converge
                       #+ (0 + instruction_axis | Subject), #failed to converge
                       data=resAllError, 
                       family="binomial", 
                       method="Laplace",
                       REML=FALSE)
summary(CorrectChoiceModel1)


##########################################
##### CONTROL RT MODELS EXP 1 & 2 #######
##########################################

ControlRTModel1 = lmer(log(RT) ~ Exp+perspectivePreference*speaker_position*instruction_axis
                           + (1 | Subject) 
                           + (0 + speaker_position | Subject)
                           + (0 + instruction_axis | Subject),
                           data=resAllError, 
                           REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1))

# Let's remove speaker position from the model to assess its effect
ControlRTModel1.noSpeakerPos = lmer(log(RT) ~ Exp+perspectivePreference*instruction_axis
                       + (1 | Subject) 
                       #+ (0 + speaker_position | Subject)
                       + (0 + instruction_axis | Subject),
                       data=resAllError, 
                       REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1.noSpeakerPos))

# Let's remove perspective Preference from the model to assess its effect
ControlRTModel1.nopref = lmer(log(RT) ~ Exp+speaker_position*instruction_axis
                                    + (1 | Subject) 
                                    # + (0 + perspectivePreference | Subject) #remove this and try with interaction with position
                                    + (0 + speaker_position | Subject)
                                    + (0 + instruction_axis | Subject),
                                    data=resAllError, 
                                    REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1.nopref))

anova(ControlRTModel1, ControlRTModel1.noSpeakerPos) #no significant effect of speaker position
anova(ControlRTModel1, ControlRTModel1.nopref) #perspective difference has a significant effect

# Let's run this model for Experiment 1 only

# Let's first add instruction axis as a variables in the resAllError_Exp1 data frame
resAllError_Exp1$instruction_axis = 'sagittal'
resAllError_Exp1$instruction_axis[resAllError_Exp1$instruction %in% c('right','left')]='lateral'

ControlRTModel1_Exp1 = lmer(log(RT) ~ perspectivePreference*speaker_position*instruction_axis
                       + (1 | Subject) 
                       + (0 + speaker_position | Subject),
                       #+ (0 + instruction_axis | Subject), model did not converge
                       data=resAllError_Exp1, 
                       REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp1))

# Now let's run this model for Experiment 2 only

# Let's first add instruction axis as a variables in the resAllError_Exp2 data frame
resAllError_Exp2$instruction_axis = 'sagittal'
resAllError_Exp2$instruction_axis[resAllError_Exp2$instruction %in% c('right','left')]='lateral'

ControlRTModel1_Exp2 = lmer(log(RT) ~ perspectivePreference*speaker_position*instruction_axis
                            + (1 | Subject) 
                            + (0 + speaker_position | Subject),
                            #+ (0 + instruction_axis | Subject), model did not converge
                            data=resAllError_Exp2, 
                            REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp2))

#######################
####### EXP 2B ######## 
#######################

######################################################
#### LMER MODELS FOR AMBIGUOUS TRIALS FOR EXP 2B ####
#####################################################

resAll_Exp2B = as.data.frame(as.matrix(resAll_Exp2B))
# Defining as factors in order to set reference categories next
resAll_Exp2B$speaker_position = as.factor(as.matrix(resAll_Exp2B$speaker_position))
resAll_Exp2B$offset_type = as.factor(as.matrix(resAll_Exp2B$offset_type))
resAll_Exp2B$instruction = as.factor(as.matrix(resAll_Exp2B$instruction))
resAll_Exp2B$instruction_axis = as.factor(as.matrix(resAll_Exp2B$instruction_axis))
resAll_Exp2B$perspectivePreference = as.factor(as.matrix(resAll_Exp2B$perspectivePreference))
resAll_Exp2B$objects_no = as.factor(as.matrix(resAll_Exp2B$objects_no))

# Setting reference categories
resAll_Exp2B$speaker_position = relevel(resAll_Exp2B$speaker_position, ref = "180") #we can't do the sawtooth contrast here but we can compare to 180 degrees
resAll_Exp2B$perspectivePreference= relevel(resAll_Exp2B$perspectivePreference, ref = "ego")

# Check for any "holes" in the design
with(resAll_Exp2B, table(speaker_position, instruction, perspectivePreference))
with(resAll_Exp2B, table(speaker_position, instruction, perspectivePreference, objects_no))
with(resAll_Exp2B, table(offset_type, instruction_axis, perspectivePreference, objects_no))
#looks good

# Make sure DVs are of the right type
resAll_Exp2B$RT = as.numeric(as.matrix(resAll_Exp2B$RT))
resAll_Exp2B$Ego = as.factor(as.matrix(resAll_Exp2B$Ego))
#resAll_Exp2B$Ego = as.integer(as.matrix(resAll_Exp2B$Ego))

str(resAll_Exp2B) #check that variables are of the correct type

contrasts(resAll_Exp2B$offset_type) <- c(-0.5, 0.5)
contrasts(resAll_Exp2B$instruction_axis) <- c(-0.5, 0.5)
contrasts(resAll_Exp2B$objects_no) <- c(-0.5, 0.5)

#####################################
##### AMBIG RT MODELS EXP 2B #######
####################################

# In the manuscript we make the argument for removing interactions between object no & axis and object no & offset

RTModel_Exp2B = lmer(log(RT) ~ perspectivePreference*offset_type*instruction_axis+objects_no*perspectivePreference #use log-transformed RT given visual inspection below
                + (1 | Subject) 
                + (0 + offset_type | Subject)
                + (0 + objects_no | Subject)
                + (0 + instruction_axis | Subject),
                #+ (0 + offset_type:instruction_axis | Subject),
                data=resAll_Exp2B, 
                REML=FALSE)
print('RT:'); pander(print_stats(RTModel_Exp2B))

#check residuals (before and after log transform)
#there is deviation from normality and homoscedasticity, so use log(RT)
#plot(density(resid(RTModel_Exp2))) #does this look approximately normal?
#qqnorm(resid(RTModel_Exp2)) #check if they fall on a straight line
#qqline(resid(RTModel_Exp2)) #check departure from line


#########################################
##### AMBIG CHOICE MODELS EXP 2B #######
#######################################

EgoChoiceModel1_Exp2B = glmer(Ego ~ offset_type*instruction_axis+ objects_no #removed interactions with objects to see if it converges
                       + (1 | Subject),
                       #+ (0 + offset_type | Subject),
                       #+ (0 + instruction_axis | Subject), #does not converge
                       #+ (0 + objects_no), #does not converge
                       data=resAll_Exp2B, 
                       family="binomial", 
                       method="Laplace",
                       nAGQ= 1, 
                       REML=FALSE)
summary(EgoChoiceModel1_Exp2B)

#which(is.na(resAll_Exp2B$Ego)) #checking that there's no missing cases given errors above
#which(!complete.cases(resAll_Exp2B)) 

##################################################
#### LMERS FOR CONTROL TRIALS FOR EXP 2B ####
##################################################


resAllError_Exp2B = as.data.frame(as.matrix(resAllError_Exp2B))
# Defining as factors in order to set reference categories next
resAllError_Exp2B$speaker_position = as.factor(as.matrix(resAllError_Exp2B$speaker_position)) #recall, there's no offset type for control trials
resAllError_Exp2B$instruction = as.factor(as.matrix(resAllError_Exp2B$instruction))
resAllError_Exp2B$instruction_axis = as.factor(as.matrix(resAllError_Exp2B$instruction_axis))
resAllError_Exp2B$perspectivePreference = as.factor(as.matrix(resAllError_Exp2B$perspectivePreference))
resAllError_Exp2B$objects_no = as.factor(as.matrix(resAllError_Exp2B$objects_no)) #note, there are only 2 object trials for control trials here

# Setting reference categories
resAllError_Exp2B$speaker_position = relevel(resAllError_Exp2B$speaker_position, ref = "0") #let's compare 90 and 270 to 0 degrees
resAllError_Exp2B$perspectivePreference= relevel(resAllError_Exp2B$perspectivePreference, ref = "ego")

# Check for any "holes" in the design
with(resAllError_Exp2B, table(speaker_position, instruction, perspectivePreference, objects_no)) #this is fine, except for the missing mixed responders in Exp 1
with(resAllError_Exp2B, table(speaker_position, instruction_axis, perspectivePreference, objects_no)) 
# NOTE that there are no 3-object trials at 90 or 270
# Do not include object number as a factor in the Control models of Exp 2B

#make sure DVs are of the right type
resAllError_Exp2B$RT = as.numeric(as.matrix(resAllError_Exp2B$RT))
resAllError_Exp2B$Ego = as.factor(as.matrix(resAllError_Exp2B$Ego))

str(resAllError_Exp2B) #check that variables are of the correct type

contrasts(resAllError_Exp2B$instruction_axis) <- c(-0.5, 0.5)
contrasts(resAllError_Exp2B$objects_no) <- c(-0.5, 0.5)


##########################################
##### CORRECT CHOICE MODELS EXP 2B #######
#########################################

# We do not include objects_no in Control models of Exp 2B because it's confounded with speaker position
# 3-object configurations can only occur at 0

CorrectChoiceModel1_Exp2B = lmer(Ego ~ speaker_position*instruction_axis
                           + (1 | Subject), 
                           #+ (0 + speaker_position | Subject), #failed to converge
                           #+ (0 + instruction_axis | Subject), #failed to converge
                           data=resAllError_Exp2B, 
                           family="binomial", 
                           method="Laplace",
                           REML=FALSE)
summary(CorrectChoiceModel1_Exp2B)

# Remove speaker position to assess its effect
CorrectChoiceModel1_Exp2B_noSpeakerPos = lmer(Ego ~ instruction_axis
                                 + (1 | Subject), 
                                 #+ (0 + speaker_position | Subject), #failed to converge
                                 #+ (0 + instruction_axis | Subject), #failed to converge
                                 data=resAllError_Exp2B, 
                                 family="binomial") #, 
                                 #method="Laplace",
                                # REML=FALSE)
summary(CorrectChoiceModel1_Exp2B_noSpeakerPos)

anova(CorrectChoiceModel1_Exp2B,CorrectChoiceModel1_Exp2B_noSpeakerPos)

#######################################
##### CONTROL RT MODELS EXP 2B #######
######################################

# Do not include objects_no in Control models of Exp 2B because it's confounded with speaker position
# 3-object configurations can only occur at 0

ControlRTModel1_Exp2B = lmer(log(RT) ~ perspectivePreference*speaker_position*instruction_axis
                            + (1 | Subject) 
                            + (0 + speaker_position | Subject)
                            + (0 + instruction_axis | Subject),
                            data=resAllError_Exp2B, 
                            REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp2B))

# Remove perspective Preference
ControlRTModel1_Exp2B_noPref = lmer(log(RT) ~ speaker_position*instruction_axis
                             + (1 | Subject) 
                             + (0 + speaker_position | Subject)
                             + (0 + instruction_axis | Subject),
                             data=resAllError_Exp2B, 
                             REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp2B_noPref))

anova(ControlRTModel1_Exp2B, ControlRTModel1_Exp2B_noPref) #marginal improvement in the model (p= .10)

# Remove speaker position
ControlRTModel1_Exp2B_noSpeakerPos = lmer(log(RT) ~ perspectivePreference*instruction_axis
                             + (1 | Subject) 
                             #+ (0 + speaker_position | Subject)
                             + (0 + instruction_axis | Subject),
                             data=resAllError_Exp2B, 
                             REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp2B_noSpeakerPos))

anova(ControlRTModel1_Exp2B, ControlRTModel1_Exp2B_noSpeakerPos) #the effect of speaker position is marginal (p=.09)

###########################################
##### CORRECT CHOICE MODELS EXP 2B #######
##########################################

CorrectModel1_Exp2B = lmer(Ego ~ offset_type*objects_no+instruction_axis
                              + (1 | Subject),
                              #+ (0 + offset_type | Subject),
                              #+ (0 + instruction_axis | Subject), #does not converge
                             # + (0 + objects_no), #does not converge
                              data=resAllError_Exp2B, 
                              family="binomial", 
                              method="Laplace",
                              #nAGQ= 1, 
                              REML=FALSE)
summary(CorrectModel1_Exp2B)

##########################################################
#### LMER MODELS FOR AMBIGUOUS TRIALS FOR EXP 2 & 2B ####
#########################################################

# Let's add instruction axis and offset type as variables in the resAll_Exp2 data frame
resAll22B$instruction_axis = 'sagittal'
resAll22B$instruction_axis[resAll22B$instruction %in% c('right','left')]='lateral'

# Let's create an offset type variable, that captures oblique (135, 225) vs. orthogonal (90, 180, 270) offsets
resAll22B$offset_type = 'orthogonal'
resAll22B$offset_type[resAll22B$speaker_position %in% c('135','225')]='oblique'

resAll22B = as.data.frame(as.matrix(resAll22B))
# Defining as factors in order to set reference categories next
resAll22B$Exp = as.factor(as.matrix(resAll22B$Exp))
resAll22B$offset_type = as.factor(as.matrix(resAll22B$offset_type))
resAll22B$instruction_axis = as.factor(as.matrix(resAll22B$instruction_axis))
resAll22B$perspectivePreference = as.factor(as.matrix(resAll22B$perspectivePreference))
resAll22B$objects_no = as.factor(as.matrix(resAll22B$objects_no))

# Setting reference categories
#resAll22B$speaker_position = relevel(resAll22B$speaker_position, ref = "180") #we can't do the sawtooth contrast here but we can compare to 180 degrees
resAll22B$perspectivePreference= relevel(resAll22B$perspectivePreference, ref = "ego")

# Make sure DVs are of the right type
resAll22B$RT = as.numeric(as.matrix(resAll22B$RT))
resAll22B$Ego = as.factor(as.matrix(resAll22B$Ego))
#resAll22B$Ego = as.integer(as.matrix(resAll22B$Ego))

str(resAll22B) #check that variables are of the correct type

contrasts(resAll22B$Exp) <- c(-0.5, 0.5)
contrasts(resAll22B$offset_type) <- c(-0.5, 0.5)
contrasts(resAll22B$instruction_axis) <- c(-0.5, 0.5)
#contrasts(resAll22B$objects_no) <- c(-0.5, 0.5)

##############################################
##### AMBIG CHOICE MODELS EXP 2 & 2 B #######
#############################################

EgoChoiceModel1_Exp22B = lmer(Ego ~ Exp*offset_type+instruction_axis
                              + (1 | Subject),
                              #+ (0 + offset_type | Subject),
                              #+ (0 + instruction_axis | Subject), #does not converge
                              # + (0 + objects_no), #does not converge
                              data=resAll22B, 
                              family="binomial", 
                              method="Laplace",
                              #nAGQ= 1, 
                              REML=FALSE)
summary(EgoChoiceModel1_Exp22B)


##########################################
##### AMBIG RT MODELS EXP 2 & 2 B #######
#########################################


RTModel_Exp22B = lmer(log(RT) ~ Exp*perspectivePreference*offset_type*instruction_axis #use log-transformed RT given visual inspection below
                     + (1 | Subject) 
                     + (0 + offset_type | Subject)
                     + (0 + instruction_axis | Subject),
                     data=resAll22B, 
                     REML=FALSE)
print('RT:'); pander(print_stats(RTModel_Exp22B))

#check residuals (before and after log transform)
#there is deviation from normality and homoscedasticity, so use log(RT)
#plot(density(resid(RTModel_Exp22B))) #does this look approximately normal?
#qqnorm(resid(RTModel_Exp22B)) #check if they fall on a straight line
#qqline(resid(RTModel_Exp22B)) #check departure from line


#######################
####### EXP 3 ######## 
#######################

####################################################
#### LMER MODELS FOR AMBIGUOUS TRIALS FOR EXP 3 ####
###################################################

resAll_Exp3 = as.data.frame(as.matrix(resAll_Exp3))
# Defining as factors in order to set reference categories next
resAll_Exp3$listener_position = as.factor(as.matrix(resAll_Exp3$listener_position))
resAll_Exp3$instruction = as.factor(as.matrix(resAll_Exp3$instruction))
resAll_Exp3$instruction_axis = as.factor(as.matrix(resAll_Exp3$instruction_axis))
resAll_Exp3$perspectivePreference = as.factor(as.matrix(resAll_Exp3$perspectivePreference))
resAll_Exp3$objects_no = as.factor(as.matrix(resAll_Exp3$objects_no))

# Setting reference categories
resAll_Exp3$listener_position = relevel(resAll_Exp3$listener_position, ref = "0") #getting error still
resAll_Exp3$perspectivePreference= relevel(resAll_Exp3$perspectivePreference, ref = "ego")

# For experiment 3, let's also try with mixed responders the reference category since they are the slowest
#resAll_Exp3$perspectivePreference= relevel(resAll_Exp3$perspectivePreference, ref = "mixed")

##Check for any "holes" in the design
with(resAll_Exp3, table(listener_position, instruction, perspectivePreference))
with(resAll_Exp3, table(listener_position, instruction, perspectivePreference, objects_no))
with(resAll_Exp3, table(listener_position, instruction_axis, perspectivePreference, objects_no))
#looks good

#make sure DVs are of the right type
resAll_Exp3$RT = as.numeric(as.matrix(resAll_Exp3$RT))
resAll_Exp3$Ego = as.factor(as.matrix(resAll_Exp3$Ego))
#resAll_Exp2B$Ego = as.integer(as.matrix(resAll_Exp2B$Ego))

str(resAll_Exp3) #check that variables are of the correct type

contrasts(resAll_Exp3$instruction_axis) <- c(-0.5, 0.5)
contrasts(resAll_Exp3$objects_no) <- c(-0.5, 0.5)

########################################
##### AMBIG CHOICE MODELS EXP 3 #######
#######################################

ChoiceModel1_Exp3 = lmer(Ego ~ listener_position+objects_no+instruction_axis #removed interactions from predictors
                     + (1 | Subject),
                     #+ (0 + listener_position | Subject),#did not converge 
                     #+ (0 + objects_no | Subject) #did not converge
                     #+ (0 + instruction_axis | Subject), #did not converge
                     data=resAll_Exp3,
                     family = "binomial",
                     method = "Laplace",
                     REML = FALSE)
summary(ChoiceModel1_Exp3)

#Add perspective Preference, as per manuscript 
ChoiceModel2_Exp3 = lmer(Ego ~ perspectivePreference*listener_position #removed the other factors to achieve convergence
                         + (1 | Subject),
                         #+ (0 + listener_position | Subject), #did not converge 
                         #+ (0 + objects_no | Subject) #did not converge
                         #+ (0 + instruction_axis | Subject), #did not converge
                         data=resAll_Exp3,
                         family = "binomial",
                         method = "Laplace",
                         REML = FALSE)
summary(ChoiceModel2_Exp3)

###################################
##### AMBIG RT MODELS EXP 3 #######
###################################

#this is the first full model that converges, but it's too complicated for reporting (massive table)
RTModel1_Exp3 = lmer(log(RT) ~ perspectivePreference*listener_position*objects_no*instruction_axis #use log-transformed RT given visual inspection below
                    + (1 | Subject), 
                    #+ (0 + listener_position | Subject),
                    #+ (0 + objects_no | Subject)
                    #+ (0 + instruction_axis | Subject),
                    data=resAll_Exp3, 
                    REML = FALSE)
print('RT:'); pander(print_stats(RTModel1_Exp3))

#Let's simplify the model by removing the interactions with axis and object no
#Let's try to simplify the model further
RTModel2_Exp3 = lmer(log(RT) ~ perspectivePreference*listener_position+objects_no+instruction_axis #use log-transformed RT given visual inspection below
                     + (1 | Subject), 
                     #+ (0 + listener_position | Subject),
                     #+ (0 + objects_no | Subject),
                    # + (0 + instruction_axis | Subject),
                     data=resAll_Exp3, 
                    REML = FALSE)
print('RT:'); pander(print_stats(RTModel2_Exp3))

#Let's do a model comparison to see which is better
anova(RTModel1_Exp3, RTModel2_Exp3) #the models don't differ significantly (p = .08), let's go with RTMode3_Exp3 since it has fewer parameters


#check residuals (before and after log transform)
#there is deviation from normality and homoscedasticity, so use log(RT)
#plot(density(resid(RTModel2_Exp3))) #does this look approximately normal?
#qqnorm(resid(RTModel2_Exp3)) #check if they fall on a straight line
#qqline(resid(RTModel2_Exp3)) #check departure from line


## Remove listener position to determine its contribution#
RTModel2_Exp3.NoListenPos = lmer(log(RT) ~ perspectivePreference+objects_no+instruction_axis #use log-transformed RT given visual inspection below
                     + (1 | Subject), 
                     #+ (0 + listener_position | Subject),
                     #+ (0 + objects_no | Subject),
                     # + (0 + instruction_axis | Subject),
                     data=resAll_Exp3, 
                     REML = FALSE)
print('RT:'); pander(print_stats(RTModel2_Exp3.NoListenPos))

anova(RTModel2_Exp3,RTModel2_Exp3.NoListenPos) #significant

#Remove perspective preference to determine its contribution #

RTModel2_Exp3.noPref = lmer(log(RT) ~ listener_position+objects_no+instruction_axis #use log-transformed RT given visual inspection below
                     + (1 | Subject), 
                     #+ (0 + listener_position | Subject),
                     #+ (0 + objects_no | Subject),
                     # + (0 + instruction_axis | Subject),
                     data=resAll_Exp3, 
                     REML = FALSE)
print('RT:'); pander(print_stats(RTModel2_Exp3.noPref))


anova(RTModel2_Exp3,RTModel2_Exp3.noPref)

#let's separate egocentric responders and re-run analyses only on those
resAll_Exp3_ego = subset(resAll_Exp3, resAll_Exp3$perspectivePreference == "ego") 

RTModel2_Exp3_egoOnly = lmer(log(RT) ~ listener_position+objects_no+instruction_axis #use log-transformed RT given visual inspection below
                     + (1 | Subject), 
                     #+ (0 + listener_position | Subject),
                     #+ (0 + objects_no | Subject),
                     # + (0 + instruction_axis | Subject),
                     data=resAll_Exp3_ego, 
                     REML = FALSE)
print('RT:'); pander(print_stats(RTModel2_Exp3_egoOnly))

#remove listener position from this model to determine its contribution 

RTModel2_Exp3_egoOnly_noListPos = lmer(log(RT) ~ objects_no+instruction_axis #use log-transformed RT given visual inspection below
                             + (1 | Subject), 
                             #+ (0 + listener_position | Subject),
                             #+ (0 + objects_no | Subject),
                             # + (0 + instruction_axis | Subject),
                             data=resAll_Exp3_ego, 
                             REML = FALSE)
print('RT:'); pander(print_stats(RTModel2_Exp3_egoOnly_noListPos))

anova(RTModel2_Exp3_egoOnly, RTModel2_Exp3_egoOnly_noListPos)


#############################################
#### LMERS FOR CONTROL TRIALS FOR EXP 3 ####
############################################

resAllError_Exp3 = as.data.frame(as.matrix(resAllError_Exp3))
#Defining as factors in order to set reference categories next
resAllError_Exp3$listener_position = as.factor(as.matrix(resAllError_Exp3$listener_position)) #recall, there's no offset type for control trials
resAllError_Exp3$instruction_axis = as.factor(as.matrix(resAllError_Exp3$instruction_axis))
resAllError_Exp3$perspectivePreference = as.factor(as.matrix(resAllError_Exp3$perspectivePreference))
resAllError_Exp3$objects_no = as.factor(as.matrix(resAllError_Exp3$objects_no)) #note, there are only 2 object trials for control trials here

#Setting reference categories
resAllError_Exp3$speaker_position = relevel(resAllError_Exp3$listener_position, ref = "0") #let's compare 90 and 270 to 0 degrees
resAllError_Exp3$perspectivePreference= relevel(resAllError_Exp3$perspectivePreference, ref = "mixed")

##Check for any "holes" in the design
with(resAllError_Exp3, table(listener_position, instruction, perspectivePreference)) #this is fine, except for the missing mixed responders in Exp 1
with(resAllError_Exp3, table(listener_position, instruction_axis, perspectivePreference, objects_no)) 
#object number is problematic (Same as Exp 2B); there are only control trials for 90 (or 0 in Exp 2B)

#make sure DVs are of the right type
resAllError_Exp3$RT = as.numeric(as.matrix(resAllError_Exp3$RT))
resAllError_Exp3$Ego = as.factor(as.matrix(resAllError_Exp3$Ego))

str(resAllError_Exp3) #check that variables are of the correct type

contrasts(resAllError_Exp3$instruction_axis) <- c(-0.5, 0.5)
#contrasts(resAllError_Exp3$objects_no) <- c(-0.5, 0.5)

##########################################
##### CORRECT CHOICE MODELS EXP 3 #######
#########################################

#Do not include objects_no in Control models of Exp 3 because it's confounded with listener position
#3-object configurations can only occur at 90

CorrectChoiceModel1_Exp3 = glmer(Ego ~ listener_position+instruction_axis #removed interaction for model to converge
                                 + (1 | Subject), 
                                 #+ (0 + listener_position | Subject), #failed to converge
                                 #+ (0 + instruction_axis | Subject), #failed to converge
                                 data=resAllError_Exp3, 
                                 family="binomial", 
                                 method="Laplace",
                                 nAGQ=1, #based on recommendation from warning messages
                                 REML=FALSE)
summary(CorrectChoiceModel1_Exp3)

#Remove listener position
CorrectChoiceModel1_Exp3_noListenPos = glmer(Ego ~ instruction_axis #removed interaction for model to converge
                                 + (1 | Subject), 
                                 #+ (0 + listener_position | Subject), #failed to converge
                                 #+ (0 + instruction_axis | Subject), #failed to converge
                                 data=resAllError_Exp3, 
                                 family="binomial", 
                                 method="Laplace",
                                 nAGQ=1, #based on advice from warning messages
                                 REML=FALSE)
summary(CorrectChoiceModel1_Exp3_noListenPos)

anova(CorrectChoiceModel1_Exp3, CorrectChoiceModel1_Exp3_noListenPos)

#### Let's add perspective preference since it seems to matter for Exp 3

CorrectChoiceModel2_Exp3 = glmer(Ego ~ perspectivePreference+listener_position+instruction_axis #removed interaction for model to converge
                                 + (1 | Subject),
                                 #+ (0 + listener_position | Subject), #failed to converge
                                 #+ (0 + instruction_axis | Subject), #failed to converge
                                 data=resAllError_Exp3, 
                                 family="binomial", 
                                 method="Laplace",
                                 nAGQ=1, #based on advice from warning messages
                                 REML=FALSE)
summary(CorrectChoiceModel2_Exp3)

# Remove perspective preference to get its effect
CorrectChoiceModel2_Exp3_noPref = glmer(Ego ~ listener_position+instruction_axis #removed interaction for model to converge
                                 + (1 | Subject),
                                 #+ (0 + listener_position | Subject), #failed to converge
                                 #+ (0 + instruction_axis | Subject), #failed to converge
                                 data=resAllError_Exp3, 
                                 family="binomial", 
                                 method="Laplace",
                                 nAGQ=1, #based on advice from warning messages
                                 REML=FALSE)
summary(CorrectChoiceModel2_Exp3_noPref)

anova(CorrectChoiceModel2_Exp3, CorrectChoiceModel2_Exp3_noPref)

######################################
##### CONTROL RT MODELS EXP 3 #######
#####################################

ControlRTModel1_Exp3 = lmer(log(RT) ~ perspectivePreference*listener_position*instruction_axis
                             + (1 | Subject) 
                             + (0 + listener_position | Subject)
                             + (0 + instruction_axis | Subject),
                             data=resAllError_Exp3, 
                             REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp3))

ControlRTModel1_Exp3_noListenPos = lmer(log(RT) ~ perspectivePreference*instruction_axis
                            + (1 | Subject) 
                            #+ (0 + listener_position | Subject)
                            + (0 + instruction_axis | Subject),
                            data=resAllError_Exp3, 
                            REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp3_noListenPos))

ControlRTModel1_Exp3_noPref = lmer(log(RT) ~ listener_position*instruction_axis
                            + (1 | Subject) 
                            + (0 + listener_position | Subject)
                            + (0 + instruction_axis | Subject),
                            data=resAllError_Exp3, 
                            REML=FALSE)
print('RT:'); pander(print_stats(ControlRTModel1_Exp3_noPref))

anova(ControlRTModel1_Exp3, ControlRTModel1_Exp3_noListenPos) #the effect of speaker position is significant 
anova(ControlRTModel1_Exp3, ControlRTModel1_Exp3_noPref) #the effect of preference is not significant


