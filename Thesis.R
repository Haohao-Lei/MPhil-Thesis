#####set the working directory
setwd("~/Onedrive/OneDrive - Nexus365/Thesis")


############################################################################################################

#############################         time of life data         ####################################

############################################################################################################
####load the data
library(haven)
life_time <- read_dta("ESS9e03_1.dta",encoding = "latin1")

table(life_time$cntry)

###############marriage age in different countries
life_time <- filter(life_time,maryr!=0&maryr!=1111)
life_time$marriage <- life_time$maryr-life_time$yrbrn
table(life_time$marriage)
life_time <- filter(life_time,marriage>0)
marriage <- c(
mean(life_time$marriage[life_time$cntry == "AT"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "BE"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "BG"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "CH"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "CY"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "CZ"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "DE"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "DK"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "ES"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "FI"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "FR"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "GB"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "HR"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "HU"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "IE"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "IS"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "IT"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "LT"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "LV"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "ME"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "NL"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "NO"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "PL"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "PT"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "RS"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "SI"], na.rm=TRUE),
mean(life_time$marriage[life_time$cntry == "SK"], na.rm=TRUE))
sd(marriage)

#####################child birth
life_time$fcldbrn
life_time$child <- life_time$fcldbrn-life_time$yrbrn
life_time <- filter(life_time,child>14&child<50)
table(life_time$child)
child<-c(
mean(life_time$child[life_time$cntry == "AT"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "BE"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "BG"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "CH"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "CY"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "CZ"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "DE"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "DK"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "ES"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "FI"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "FR"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "GB"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "HR"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "HU"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "IE"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "IS"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "IT"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "LT"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "LV"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "ME"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "NL"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "NO"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "PL"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "PT"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "RS"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "SI"], na.rm=TRUE),
mean(life_time$child[life_time$cntry == "SK"], na.rm=TRUE))
sd(child)

##################################idea marriage age
life_time$iaglptn
life_time2 <- filter(life_time,iaglptn!=111&iaglptn!=0)
life_time2$idea_marr <- life_time2$fcldbrn-life_time2$yrbrn
life_time <- filter(life_time,child>14&child<50)
table(life_time2$idea_marr)

idea_marr<- c(
mean(life_time2$idea_marr[life_time2$cntry == "AT"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "BE"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "BG"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "CH"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "CY"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "CZ"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "DE"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "DK"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "ES"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "FI"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "FR"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "GB"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "HR"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "HU"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "IE"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "IS"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "IT"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "LT"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "LV"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "ME"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "NL"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "NO"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "PL"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "PT"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "RS"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "SI"], na.rm=TRUE),
mean(life_time2$idea_marr[life_time2$cntry == "SK"], na.rm=TRUE))
sd(idea_marr)


































