##########load the package
library(foreign)
library(lmerTest)
library(GGally)
library(tidyverse)
library(haven)
library(dplyr)
library(mokken)
library(extraoperators)
library(qacBase)

###########load the data
cognitivew2 <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew2_rel8-0-0_ALL_datasets_stata/sharew2_rel8-0-0_cf.dta")
cognitivew4 <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew4_rel8-0-0_ALL_datasets_stata/sharew4_rel8-0-0_cf.dta")

gs2 <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew2_rel8-0-0_ALL_datasets_stata/sharew2_rel8-0-0_gs.dta")
pf2 <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew2_rel8-0-0_ALL_datasets_stata/sharew2_rel8-0-0_pf.dta")

iscedw2 <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew2_rel8-0-0_ALL_datasets_stata/sharew2_rel8-0-0_gv_isced.dta")

householdfeature <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew3_rel8-0-0_ALL_datasets_stata/sharew3_rel8-0-0_cs.dta")

demo <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/sharew2_rel8-0-0_ALL_datasets_stata/sharew2_rel8-0-0_dn.dta")

life <- read_dta("~/OneDrive - Nexus365/Thesis/R directory/H_SHARE_LH_b2.dta")

####################################################################################################################################
####################################################################################################################################

###########################################       Data cleaning         #################################################

####################################################################################################################################

####################################################################################################################################
############################################cognitive data####################################################
####################################################################################################################################

###only include the relavant variables into the dataset
cognitivew2 <- dplyr::select(cognitivew2, mergeid, cf003_:cf016tot)

######cleaning the cognitive data in wave two
cognitive <- cognitivew2
###try to change the Refusal and Don't know into NA
#cf003_.x
cognitive$cf003_[cognitive$cf003_==-2] <- NA
cognitive$cf003_[cognitive$cf003_==-1] <- NA
table(cognitive$cf003_)

#cf004_.x
cognitive$cf004_[cognitive$cf004_==-2] <- NA
cognitive$cf004_[cognitive$cf004_==-1] <- NA
table(cognitive$cf004_)


#cf005_.x
cognitive$cf005_[cognitive$cf005_==-2] <- NA
cognitive$cf005_[cognitive$cf005_==-1] <- NA
table(cognitive$cf005_)

#cf006_.x
cognitive$cf006_[cognitive$cf006_==-2] <- NA
cognitive$cf006_[cognitive$cf006_==-1] <- NA
table(cognitive$cf006_)

#cf010_.x
cognitive$cf010_[cognitive$cf010_==-2] <- NA
cognitive$cf010_[cognitive$cf010_==-1] <- NA
table(cognitive$cf010_)

####################################################################################################################################################################
#######code the orientation
cognitive$cf003_[cognitive$cf003_==2] <- 0
cognitive$cf004_[cognitive$cf004_==2] <- 0
cognitive$cf005_[cognitive$cf005_==2] <- 0
cognitive$cf006_[cognitive$cf006_==2] <- 0
cognitive$orientation <- cognitive$cf003_ + cognitive$cf004_+ cognitive$cf005_+cognitive$cf006_
mean(cognitive$orientation, na.rm=TRUE)

######code the immediate recall
cognitive$cf008tot[cognitive$cf008tot<2] <- 0
cognitive$cf008tot[cognitive$cf008tot==2] <- 1
cognitive$cf008tot[cognitive$cf008tot==3] <- 2
cognitive$cf008tot[cognitive$cf008tot==4] <- 3
cognitive$cf008tot[cognitive$cf008tot>=5] <- 4
cognitive$recall <- cognitive$cf008tot
summary(cognitive$recall)

######code the delay recall
cognitive$cf016tot[cognitive$cf016tot==0] <- 0
cognitive$cf016tot[cognitive$cf016tot==1] <- 1
cognitive$cf016tot[cognitive$cf016tot==2] <- 2
cognitive$cf016tot[cognitive$cf016tot==3] <- 3
cognitive$cf016tot[cognitive$cf016tot>=4] <- 4
cognitive$recall2 <- cognitive$cf016tot

######code the Numeracy
#cf013_.x
cognitive$cf013_[cognitive$cf013_!=1] <- 0
cognitive$cf013_[is.na(cognitive$cf013_)] <- 0
table(cognitive$cf013_)
#cf014_.x
cognitive$cf014_[cognitive$cf014_!=1] <- 2
cognitive$cf014_[cognitive$cf014_==1] <- 3
cognitive$cf014_[is.na(cognitive$cf014_)] <- 0
table(cognitive$cf014_)
#cf015_.x
cognitive$cf015_[cognitive$cf015_!=1] <- 0
cognitive$cf015_[is.na(cognitive$cf015_)] <- 0
table(cognitive$cf015_)
cognitive$numeracy <- cognitive$cf013_ + cognitive$cf014_ + cognitive$cf015_

#########code the Verbal fluency
cognitive <- recodes(data=cognitive, vars="cf010_", 
              from=c("$ < 10", "$ <14", "$ <17", "$ >=17", "$ == -1", "$ == -2"), 
              to=  c(0, 1, 2, 3, NA, NA))
cognitive$verbal <- cognitive$cf010_

###select variable
cognitive <- dplyr::select(cognitive,mergeid, orientation, recall, recall2, numeracy, verbal)

####################################################################################################################################
############################################physical indicator data####################################################
####################################################################################################################################

####Grip strength recoding, my strategy is based on the following paper:
#Cross-national differences in grip strength among 50+ year-old Europeans: results from the SHARE study

#identify the na values
gs2$gs006_[gs2$gs006_==-1] <- NA
gs2$gs006_[gs2$gs006_==-2] <- NA

gs2$gs007_[gs2$gs007_==-1] <- NA
gs2$gs007_[gs2$gs007_==-2] <- NA

gs2$gs008_[gs2$gs008_==-1] <- NA
gs2$gs008_[gs2$gs008_==-2] <- NA

gs2$gs009_[gs2$gs009_==-1] <- NA
gs2$gs009_[gs2$gs009_==-2] <- NA

#Valid measurements were defined as the values of two measurements in one hand that differed 
#by less than 20 kg.
#try to find the valid GS
gs2$dif1 <- abs(gs2$gs006_-gs2$gs007_)
gs2$dif2 <- abs(gs2$gs008_-gs2$gs009_)
##Also excluded the sample if GS was only measured once in one hand
gs2 <- filter(gs2,gs2$dif1<20,gs2$dif2<20)

##The maximum value (MaxGS) was defined as the maximum GS measurement of both hands (2 × 2)
##find the maxium GS
gs2$gs2max <- pmax(gs2$gs006_,gs2$gs007_,gs2$gs008_,gs2$gs009_)

##GS measurements with values =0 kg or ≥100 kg were excluded
gs2 <- filter(gs2,gs2$gs2max!=0,gs2$gs2max<100)

###only include the maxGS in the dataset 
gs2 <- dplyr::select(gs2, mergeid, gs2max)


####Peak flow recoding, my strategy is based on the following paper:
#The longitudinal relationship of work stress with peak expiratory flow: a cohort study
###identify the NA value
pf2$pf003_[pf2$pf003_==-1] <- NA
pf2$pf003_[pf2$pf003_==-2] <- NA

pf2$pf004_[pf2$pf004_==-1] <- NA
pf2$pf004_[pf2$pf004_==-2] <- NA

###exclude the NA value
pf2 <-  drop_na(pf2,pf003_,pf004_)

##find the maxium PF
pf2$pf2max <- pmax(pf2$pf003_,pf2$pf004_)

###only include the maxPF in the dataset 
pf2 <- dplyr::select(pf2, mergeid, pf2max)

####################################################################################################################################
############################################dependent variable####################################################
####################################################################################################################################
physical <- merge(gs2,pf2,by="mergeid")
dependent <- merge(cognitive,physical,by="mergeid")

###drop all the NA values in the dependent variable
dependent <- drop_na(dependent)

####################################################################################################################################
############################################controlled variable####################################################
####################################################################################################################################

#########################################cleaning education data#################################################
#wave 2 education data
iscedw2$isced1997_r[iscedw2$isced1997_r==-2] <- NA
iscedw2$isced1997_r[iscedw2$isced1997_r==-1] <- NA
table(iscedw2$isced1997_r, useNA = "always")
#recode "Still in school " into "Other"
iscedw2$isced1997_r[iscedw2$isced1997_r==95] <- 97

###change the name into education
education <- dplyr::select(iscedw2, mergeid, isced1997_r)

#########################################Childhood Advantage Scale#################################################
###fixed bath
householdfeature$sl_cs007d1[householdfeature$sl_cs007d1==-2] <- NA
householdfeature$sl_cs007d1[householdfeature$sl_cs007d1==-1] <- NA
table(householdfeature$sl_cs007d1, useNA = "always")

###running cold water
householdfeature$sl_cs007d2[householdfeature$sl_cs007d2==-2] <- NA
householdfeature$sl_cs007d2[householdfeature$sl_cs007d2==-1] <- NA
table(householdfeature$sl_cs007d2, useNA = "always")

###running hot water
householdfeature$sl_cs007d3[householdfeature$sl_cs007d3==-2] <- NA
householdfeature$sl_cs007d3[householdfeature$sl_cs007d3==-1] <- NA
table(householdfeature$sl_cs007d3, useNA = "always")

###inside toilet
householdfeature$sl_cs007d4[householdfeature$sl_cs007d4==-2] <- NA
householdfeature$sl_cs007d4[householdfeature$sl_cs007d4==-1] <- NA
table(householdfeature$sl_cs007d4, useNA = "always")

###inside toilet
householdfeature$sl_cs007d5[householdfeature$sl_cs007d5==-2] <- NA
householdfeature$sl_cs007d5[householdfeature$sl_cs007d5==-1] <- NA
table(householdfeature$sl_cs007d5, useNA = "always")

###bookcase
householdfeature$sl_cs008_[householdfeature$sl_cs008_==-2] <- NA
householdfeature$sl_cs008_[householdfeature$sl_cs008_==-1] <- NA
table(householdfeature$sl_cs008_, useNA = "always")
householdfeature$sl_cs008_[householdfeature$sl_cs008_==2] <- 1###less than one bookcase
householdfeature$sl_cs008_[householdfeature$sl_cs008_!=1] <- 2###at least one bookcase or more

###bedroom overcrowding
householdfeature$sl_cs002_[householdfeature$sl_cs002_==-2] <- NA
householdfeature$sl_cs002_[householdfeature$sl_cs002_==-1] <- NA
table(householdfeature$sl_cs002_, useNA = "always")

householdfeature$sl_cs003_[householdfeature$sl_cs003_==-2] <- NA
householdfeature$sl_cs003_[householdfeature$sl_cs003_==-1] <- NA
table(householdfeature$sl_cs003_, useNA = "always")

householdfeature$bedperson <- householdfeature$sl_cs003_/householdfeature$sl_cs002_
summary(householdfeature$bedperson)
householdfeature$bedperson[is.nan(householdfeature$bedperson)] <- NA
householdfeature$bedperson[is.infinite(householdfeature$bedperson)] <- NA####do I need to change NaN into NA???
table(householdfeature$bedperson, useNA = "always")
#the issue of number of feature
householdfeature$bedperson[householdfeature$bedperson<=1] <- 1###Yes 
householdfeature$bedperson[householdfeature$bedperson>1] <- 0###No
table(householdfeature$bedperson, useNA = "always")

###occupation of the primary breadwinner
householdfeature$sl_cs009_[householdfeature$sl_cs009_==-2] <- NA
householdfeature$sl_cs009_[householdfeature$sl_cs009_==-1] <- NA
table(householdfeature$sl_cs009_, useNA = "always") ###Question: how to deal with there was no main breadwinner

########## Merge the data, merge childhood and then combine all the current data together
childhood <- dplyr::select(householdfeature, mergeid, sl_cs007d1, sl_cs007d2, sl_cs007d3, sl_cs007d4, sl_cs007d5,
                    sl_cs008_,bedperson,sl_cs009_)

#######Merge data together
data <- merge(dependent,education,by="mergeid")
data <- merge(data,childhood,by="mergeid")
table(rowSums(as.data.frame(is.na(data))))###test the number of missing variables

####cleaning the demographic characteristics
demo <- dplyr::select(demo, mergeid, dn042_ ,dn003_)
data <- merge(data,demo,by="mergeid")
data <- filter(data, dn003_>=1930&dn003_<=1957)

####descriptive statistics for cognitive skills
summary(data$orientation)
sd(data$orientation,na.rm=TRUE)/sqrt(length((data$orientation)))
summary(data$recall)
sd(data$recall,na.rm=TRUE)/sqrt(length((data$recall)))
summary(data$recall2)
sd(data$recall2,na.rm=TRUE)/sqrt(length((data$recall2)))
summary(data$numeracy)
sd(data$numeracy,na.rm=TRUE)/sqrt(length((data$numeracy)))
summary(data$verbal)
sd(data$verbal,na.rm=TRUE)/sqrt(length((data$verbal)))

####calculate the total score
data$cognition <- data$orientation+data$recall+data$recall2+data$numeracy+data$verbal
mean(data$cognition)
sd(data$cognition,na.rm=TRUE)/sqrt(length((data$cognition)))

####################################################################################################################################
############################################life history####################################################
####################################################################################################################################
####try to follow the loop in the questionaire







