#-------Load data####
library(foreign)
quant_data<-read.spss("Quant_study.sav", use.value.labels = FALSE, to.data.frame = TRUE)

#-------Data preparation####
options(scipen = 999)

#Citations packages####
citation()
citation("nlme")

#-------Data overview####
library(psych)
library(memisc)
library(car)

View(quant_data)
str(quant_data)

#-------Alpha time 1####
library(GPArotation)

#--Basic needs
basicneeds_alpha_T1 <- data.frame(quant_data$aut_T1, quant_data$comp_T1, quant_data$rel_T1)
alpha(basicneeds_alpha_T1) #raw alpha= .66
omega(basicneeds_alpha_T1) #Omega_t= .73

#--Intrinsic motivation
intrinsic_alpha_T1 <- data.frame(quant_data$intri1_T1, quant_data$intri2_T1, quant_data$intri3_T1)
alpha(intrinsic_alpha_T1) #raw alpha= .92
omega(intrinsic_alpha_T1) #Omega_t= .93

#--Integrated regulation
integrated_alpha_T1 <- data.frame(quant_data$inte1_T1, quant_data$inte2_T1, quant_data$inte3_T1)
alpha(integrated_alpha_T1) #raw alpha= .92
omega(integrated_alpha_T1) #Omega_t= .93

#--Identified regulation
identified_alpha_T1 <- data.frame(quant_data$ide1_T1, quant_data$ide2_T1, quant_data$ide3_T1)
alpha(identified_alpha_T1) #raw alpha= .71
omega(identified_alpha_T1) #Omega_t= .73

#--Introjected regulation
introjected_alpha_T1 <- data.frame(quant_data$intro1_T1, quant_data$intro2_T1, quant_data$intro3_T1)
alpha(introjected_alpha_T1) #raw alpha= .93
omega(introjected_alpha_T1) #Omega_t= .94

#--External regulation
external_alpha_T1 <- data.frame(quant_data$ext1_T1, quant_data$ext2_T1, quant_data$ext3_T1)
alpha(external_alpha_T1) #raw alpha= .51
omega(external_alpha_T1) #Omega_t= .60

#--Amotivation
amotivation_alpha_T1 <- data.frame(quant_data$amo1_T1, quant_data$amo2_T1, quant_data$amo3_T1)
alpha(amotivation_alpha_T1) #raw alpha= .81
omega(amotivation_alpha_T1) #Omega_t= .83

#--Controlled motivation
controlled_alpha_T1 <- data.frame(quant_data$ext1_T1, quant_data$ext2_T1, quant_data$ext3_T1,quant_data$intro1_T1, quant_data$intro2_T1, quant_data$intro3_T1)
alpha(controlled_alpha_T1) #raw alpha= .79
omega(controlled_alpha_T1) #Omega_t= .89

#--Autonomous motivation
autonomous_alpha_T1 <- data.frame(quant_data$ide1_T1, quant_data$ide2_T1, quant_data$ide3_T1,quant_data$inte1_T1, quant_data$inte2_T1, 
                                  quant_data$inte3_T1,quant_data$intri1_T1, quant_data$intri2_T1, quant_data$intri3_T1)
alpha(autonomous_alpha_T1) #raw alpha= .88
omega(autonomous_alpha_T1) #Omega_t= .92

#-------Alpha time 2####

#--Basic needs
basicneeds_alpha_T2 <- data.frame(quant_data$aut_T2, quant_data$comp_T2, quant_data$rel_T2)
alpha(basicneeds_alpha_T2) #raw alpha= .71
omega(basicneeds_alpha_T2) #Omega_t= .77

#--Intrinsic motivation
intrinsic_alpha_T2 <- data.frame(quant_data$intri1_T2, quant_data$intri2_T2, quant_data$intri3_T2)
alpha(intrinsic_alpha_T2) #raw alpha= .91
omega(intrinsic_alpha_T2) #Omega_t= .91

#--Integrated regulation
integrated_alpha_T2 <- data.frame(quant_data$inte1_T2, quant_data$inte2_T2, quant_data$inte3_T2)
alpha(integrated_alpha_T2) #raw alpha= .78
omega(integrated_alpha_T2) #Omega_t= .81

#--Identified regulation
identified_alpha_T2 <- data.frame(quant_data$ide1_T2, quant_data$ide2_T2, quant_data$ide3_T2)
alpha(identified_alpha_T2) #raw alpha= .76
omega(identified_alpha_T2) #Omega_t= .76

#--Introjected regulation
introjected_alpha_T2 <- data.frame(quant_data$intro1_T2, quant_data$intro2_T2, quant_data$intro3_T2)
alpha(introjected_alpha_T2) #raw alpha= .88
omega(introjected_alpha_T2) #Omega_t= .89

#--External regulation
external_alpha_T2 <- data.frame(quant_data$ext1_T2, quant_data$ext2_T2, quant_data$ext3_T2)
alpha(external_alpha_T2) #raw alpha= .55
omega(external_alpha_T2) #Omega_t= .64

#--Amotivation
amotivation_alpha_T2 <- data.frame(quant_data$amo1_T2, quant_data$amo2_T2, quant_data$amo3_T2)
alpha(amotivation_alpha_T2) #raw alpha= .83
omega(amotivation_alpha_T2) #Omega_t= .84

#--Controlled motivation
controlled_alpha_T2 <- data.frame(quant_data$ext1_T2, quant_data$ext2_T2, quant_data$ext3_T2,quant_data$intro1_T2, quant_data$intro2_T2, quant_data$intro3_T2)
alpha(controlled_alpha_T2) #raw alpha= .82
omega(controlled_alpha_T2) #Omega_t= .91

#--Autonomous motivation
autonomous_alpha_T2 <- data.frame(quant_data$ide1_T2, quant_data$ide2_T2, quant_data$ide3_T2,quant_data$inte1_T2, quant_data$inte2_T2, 
                                  quant_data$inte3_T2,quant_data$intri1_T2, quant_data$intri2_T2, quant_data$intri3_T2)
alpha(autonomous_alpha_T2) #raw alpha= .88
omega(autonomous_alpha_T2) #Omega_t= .92

#-------Alpha time 3####

#--Basic needs
basicneeds_alpha_T3 <- data.frame(quant_data$aut_T3, quant_data$comp_T3, quant_data$rel_T3)
alpha(basicneeds_alpha_T3) #raw alpha= .73
omega(basicneeds_alpha_T3) #Omega_t= .77

#--Intrinsic motivation
intrinsic_alpha_T3 <- data.frame(quant_data$intri1_T3, quant_data$intri2_T3, quant_data$intri3_T3)
alpha(intrinsic_alpha_T3) #raw alpha= .92
omega(intrinsic_alpha_T3) #Omega_t= .93

#--Integrated regulation
integrated_alpha_T3 <- data.frame(quant_data$inte1_T3, quant_data$inte2_T3, quant_data$inte3_T3)
alpha(integrated_alpha_T3) #raw alpha= .73
omega(integrated_alpha_T3) #Omega_t= .77

#--Identified regulation
identified_alpha_T3 <- data.frame(quant_data$ide1_T3, quant_data$ide2_T3, quant_data$ide3_T3)
alpha(identified_alpha_T3) #raw alpha= .76
omega(identified_alpha_T3) #Omega_t= .78

#--Introjected regulation
introjected_alpha_T3 <- data.frame(quant_data$intro1_T3, quant_data$intro2_T3, quant_data$intro3_T3)
alpha(introjected_alpha_T3) #raw alpha= .94
omega(introjected_alpha_T3) #Omega_t= .94

#--External regulation
external_alpha_T3 <- data.frame(quant_data$ext1_T3, quant_data$ext2_T3, quant_data$ext3_T3)
alpha(external_alpha_T3) #raw alpha= .49
omega(external_alpha_T3) #Omega_t= .59

#--Amotivation
amotivation_alpha_T3 <- data.frame(quant_data$amo1_T3, quant_data$amo2_T3, quant_data$amo3_T3)
alpha(amotivation_alpha_T3) #raw alpha= .87
omega(amotivation_alpha_T3) #Omega_t= .88

#--Controlled motivation
controlled_alpha_T3 <- data.frame(quant_data$ext1_T3, quant_data$ext2_T3, quant_data$ext3_T3,quant_data$intro1_T3, quant_data$intro2_T3, quant_data$intro3_T3)
alpha(controlled_alpha_T3) #raw alpha= .84
omega(controlled_alpha_T3) #Omega_t= .92

#--Autonomous motivation
autonomous_alpha_T3 <- data.frame(quant_data$ide1_T3, quant_data$ide2_T3, quant_data$ide3_T3,quant_data$inte1_T3, quant_data$inte2_T3, 
                                  quant_data$inte3_T3,quant_data$intri1_T3, quant_data$intri2_T3, quant_data$intri3_T3)
alpha(autonomous_alpha_T3) #raw alpha= .89
omega(autonomous_alpha_T3) #Omega_t= .93

#-------Creating scales and adding to dataset time 1####
#--Basic needs
quant_data$basic_needs_T1 <- rowMeans(quant_data[ ,c("aut_T1", "comp_T1", 
                                                     "rel_T1")], 
                                      na.rm = TRUE)

#--Intrinsic motivation
quant_data$intrinsic_mot_T1 <- rowMeans(quant_data[ ,c("intri1_T1", "intri2_T1", 
                                                       "intri3_T1")], 
                                        na.rm = TRUE)
#--Integrated regulation
quant_data$integrated_reg_T1 <- rowMeans(quant_data[ ,c("inte1_T1", "inte2_T1", 
                                                        "inte3_T1")], 
                                         na.rm = TRUE)

#--Identified regulation
quant_data$identified_reg_T1 <- rowMeans(quant_data[ ,c("ide1_T1", "ide2_T1", 
                                                        "ide3_T1")], 
                                         na.rm = TRUE)

#--Introjected regulation
quant_data$introjected_reg_T1 <- rowMeans(quant_data[ ,c("intro1_T1", "intro2_T1", 
                                                         "intro3_T1")], 
                                          na.rm = TRUE)

#--External regulation
quant_data$external_reg_T1 <- rowMeans(quant_data[ ,c("ext1_T1", "ext2_T1", 
                                                      "ext3_T1")], 
                                       na.rm = TRUE)

#--Amotivation
quant_data$amotivation_T1 <- rowMeans(quant_data[ ,c("amo1_T1", "amo2_T1", 
                                                     "amo3_T1")], 
                                      na.rm = TRUE)

#--Controlled motivation
quant_data$controlled_mot_T1 <- rowMeans(quant_data[ ,c("ext1_T1", "ext2_T1", 
                                                        "ext3_T1","intro1_T1", "intro2_T1", 
                                                        "intro3_T1")], 
                                         na.rm = TRUE)
#--Autonomous motivation
quant_data$autonomous_mot_T1 <- rowMeans(quant_data[ ,c("ide1_T1", "ide2_T1", 
                                                        "ide3_T1","inte1_T1", "inte2_T1", 
                                                        "inte3_T1","intri1_T1", "intri2_T1", 
                                                        "intri3_T1")], 
                                         na.rm = TRUE)

#-------Creating scales and adding to dataset time 2####
#--Basic needs
quant_data$basic_needs_T2 <- rowMeans(quant_data[ ,c("aut_T2", "comp_T2", 
                                                     "rel_T2")], 
                                      na.rm = TRUE)

#--Intrinsic motivation
quant_data$intrinsic_mot_T2 <- rowMeans(quant_data[ ,c("intri1_T2", "intri2_T2", 
                                                       "intri3_T2")], 
                                        na.rm = TRUE)
#--Integrated regulation
quant_data$integrated_reg_T2 <- rowMeans(quant_data[ ,c("inte1_T2", "inte2_T2", 
                                                        "inte3_T2")], 
                                         na.rm = TRUE)

#--Identified regulation
quant_data$identified_reg_T2 <- rowMeans(quant_data[ ,c("ide1_T2", "ide2_T2", 
                                                        "ide3_T2")], 
                                         na.rm = TRUE)

#--Introjected regulation
quant_data$introjected_reg_T2 <- rowMeans(quant_data[ ,c("intro1_T2", "intro2_T2", 
                                                         "intro3_T2")], 
                                          na.rm = TRUE)

#--External regulation
quant_data$external_reg_T2 <- rowMeans(quant_data[ ,c("ext1_T2", "ext2_T2", 
                                                      "ext3_T2")], 
                                       na.rm = TRUE)

#--Amotivation
quant_data$amotivation_T2 <- rowMeans(quant_data[ ,c("amo1_T2", "amo2_T2", 
                                                     "amo3_T2")], 
                                      na.rm = TRUE)

#--Controlled motivation
quant_data$controlled_mot_T2 <- rowMeans(quant_data[ ,c("ext1_T2", "ext2_T2", 
                                                        "ext3_T2","intro1_T2", "intro2_T2", 
                                                        "intro3_T2")], 
                                         na.rm = TRUE)
#--Autonomous motivation
quant_data$autonomous_mot_T2 <- rowMeans(quant_data[ ,c("ide1_T2", "ide2_T2", 
                                                        "ide3_T2","inte1_T2", "inte2_T2", 
                                                        "inte3_T2","intri1_T2", "intri2_T2", 
                                                        "intri3_T2")], 
                                         na.rm = TRUE)

#-------Creating scales and adding to dataset time 3####
#--Basic needs
quant_data$basic_needs_T3 <- rowMeans(quant_data[ ,c("aut_T3", "comp_T3", 
                                                     "rel_T3")], 
                                      na.rm = TRUE)

#--Intrinsic motivation
quant_data$intrinsic_mot_T3 <- rowMeans(quant_data[ ,c("intri1_T3", "intri2_T3", 
                                                       "intri3_T3")], 
                                        na.rm = TRUE)
#--Integrated regulation
quant_data$integrated_reg_T3 <- rowMeans(quant_data[ ,c("inte1_T3", "inte2_T3", 
                                                        "inte3_T3")], 
                                         na.rm = TRUE)

#--Identified regulation
quant_data$identified_reg_T3 <- rowMeans(quant_data[ ,c("ide1_T3", "ide2_T3", 
                                                        "ide3_T3")], 
                                         na.rm = TRUE)

#--Introjected regulation
quant_data$introjected_reg_T3 <- rowMeans(quant_data[ ,c("intro1_T3", "intro2_T3", 
                                                         "intro3_T3")], 
                                          na.rm = TRUE)

#--External regulation
quant_data$external_reg_T3 <- rowMeans(quant_data[ ,c("ext1_T3", "ext2_T3", 
                                                      "ext3_T3")], 
                                       na.rm = TRUE)

#--Amotivation
quant_data$amotivation_T3 <- rowMeans(quant_data[ ,c("amo1_T3", "amo2_T3", 
                                                     "amo3_T3")], 
                                      na.rm = TRUE)

#--Controlled motivation
quant_data$controlled_mot_T3 <- rowMeans(quant_data[ ,c("ext1_T3", "ext2_T3", 
                                                        "ext3_T3","intro1_T3", "intro2_T3", 
                                                        "intro3_T3")], 
                                         na.rm = TRUE)
#--Autonomous motivation
quant_data$autonomous_mot_T3 <- rowMeans(quant_data[ ,c("ide1_T3", "ide2_T3", 
                                                        "ide3_T3","inte1_T3", "inte2_T3", 
                                                        "inte3_T3","intri1_T3", "intri2_T3", 
                                                        "intri3_T3")], 
                                         na.rm = TRUE)

#-------Recoding factors####
#faculty
table(quant_data$faculty)
quant_data$faculty <- factor(quant_data$faculty, labels = c("HF", "MN")) #0=HF, 1=MN
table(quant_data$faculty) #HF=200, MN=62
quant_data$faculty <- as.numeric(quant_data$faculty) #HF=1, MN=2
quant_data$faculty <- Recode(quant_data$faculty, "1=0; 2=1", as.numeric=TRUE)
table(quant_data$faculty) 

#gender
table(quant_data$gender)
quant_data$gender <- factor(quant_data$gender, labels = c("females", "males")) #0=female, 1=male
table(quant_data$gender) #females=166, males=97
quant_data$gender <- as.numeric(quant_data$gender) 
quant_data$gender <- Recode(quant_data$gender, "1=0; 2=1", as.numeric=TRUE)
table(quant_data$gender)

#semester registration
table(quant_data$semester_reg)
quant_data$semester_reg <- factor(quant_data$semester_reg, labels = c("no", "yes")) #0=no, 1=yes
table(quant_data$semester_reg)
quant_data$semester_reg <- as.numeric(quant_data$semester_reg)
quant_data$semester_reg <- Recode(quant_data$semester_reg, "1=0; 2=1", as.numeric=TRUE)
table(quant_data$semester_reg)

#Study points
table(quant_data$study_points)
quant_data$study_points <- Recode(quant_data$study_points, "10:29= 0; 30:90= 1", as.numeric=T) #0=dropout, 1=normal progresion
table(quant_data$study_points)

#study year
table(quant_data$study_year)
quant_data$study_year <- Recode(quant_data$study_year, "0=NA", as.numeric=TRUE)
quant_data$study_year <- Recode(quant_data$study_year, "1=0; 2=1;3=2", as.numeric=TRUE)
table(quant_data$study_year)
quant_data$study_year <-  factor(quant_data$study_year, labels = c("first_year", "second_year","fourth_year")) #0=fist year, 1=second year, 2=fourth year
table(quant_data$study_year)
quant_data$study_year <- as.numeric(quant_data$study_year)
table(quant_data$study_year)
quant_data$study_year <- Recode(quant_data$study_year, "1=0; 2=1;3=2", as.numeric=TRUE)

#-------Preliminary analysis####
library(summarytools)

#--Descriptives
describe(quant_data$age)
describe(quant_data$HS_average)

corr_variables_motivationclasses <- subset(quant_data,select = c(amotivation_T1,controlled_mot_T1,autonomous_mot_T1,
                                                                 amotivation_T2,controlled_mot_T2,autonomous_mot_T2,
                                                                 amotivation_T3,controlled_mot_T3,autonomous_mot_T3))

main_variables<- subset(quant_data, select = c(basic_needs_T1, basic_needs_T2, basic_needs_T3,
                                               amotivation_T1, amotivation_T2,amotivation_T3,
                                               controlled_mot_T1,controlled_mot_T2,controlled_mot_T3,
                                               autonomous_mot_T1,autonomous_mot_T2,autonomous_mot_T3))


corr_mot_DV<- subset(quant_data, select = c(basic_needs_T1, basic_needs_T2, basic_needs_T3,
                                            amotivation_T1, amotivation_T2,amotivation_T3,
                                            controlled_mot_T1,controlled_mot_T2,controlled_mot_T3,
                                            autonomous_mot_T1,autonomous_mot_T2,autonomous_mot_T3, study_points, grades))

descriptives_mainvariables <- dfSummary(main_variables, round.digits = 2)
view(descriptives_mainvariables)

descriptives_motvariables<-dfSummary(corr_mot_DV, round.digits = 2)
view(descriptives_motvariables)

describe(main_variables)
describe(corr_variables_motivationclasses)

table(quant_data$study_year)

#-Dropout descriptives
table(quant_data$semester_reg)
table(quant_data$study_points)

table_drop1 <- xtabs(~ semester_reg + study_year, data = quant_data)
table_drop1

table_drop2 <- xtabs(~ study_points + study_year, data=quant_data)
table_drop2

table_drop3 <- xtabs(~ semester_reg + study_points, data = quant_data)
table_drop3

#-------Primary analysis####

#----Wide to long data approach####
#--All main variables
library(reshape2)
library(dplyr)

#Creating individual long format datasets
data_long1 <-  melt(quant_data, id=c("ID", "faculty","gender","age","study_points","grades","semester_reg","study_year","HS_average"),
                    measure.vars=c("basic_needs_T1","basic_needs_T2", "basic_needs_T3"),
                    variable.name = "Basic_needs_measurement", value.name = "Basic_needs")

data_long2 <-  melt(quant_data, id=c("ID", "faculty","gender","age","study_points","grades","semester_reg","study_year","HS_average"),
                    measure.vars=c("amotivation_T1","amotivation_T2", "amotivation_T3"),
                    variable.name = "Amotivation_measurement", value.name = "Amotivation")

data_long3 <-  melt(quant_data, id=c("ID", "faculty","gender","age","study_points","grades","semester_reg","study_year","HS_average"),
                    measure.vars=c("controlled_mot_T1","controlled_mot_T2", "controlled_mot_T3"),
                    variable.name = "Controlled_measurement", value.name = "Controlled_motivation")

data_long4 <-  melt(quant_data, id=c("ID", "faculty","gender","age","study_points","grades","semester_reg","study_year","HS_average"),
                    measure.vars=c("autonomous_mot_T1","autonomous_mot_T2", "autonomous_mot_T3"),
                    variable.name = "Autonomous_measurement", value.name = "Autonomous_motivation")

#Combining the individual datasets but removing the unesseary columns
quant_data_long <- cbind(data_long1,data_long2,data_long3,data_long4)

quant_data_long2 <-  quant_data_long[c(1,2,3,4,5,6,7,8,9,10,11,22,33,44)]
View(quant_data_long2)

#Renaming timepoints and factor to time
quant_data_long2 <- quant_data_long2 %>% rename(Timepoints = Basic_needs_measurement)

quant_data_long2 <- quant_data_long2 %>% mutate(Timepoints = recode(Timepoints, "basic_needs_T1" = "Time1",
                                                                    "basic_needs_T2" = "Time2",
                                                                    "basic_needs_T3" = "Time3"))
View(quant_data_long2)

#-------Missing analyses####
library(summarytools)

quant_data[sapply(quant_data, is.nan)] <- NA

main_variables<- subset(quant_data, select = c(basic_needs_T1, basic_needs_T2, basic_needs_T3,
                                               amotivation_T1, amotivation_T2,amotivation_T3,
                                               controlled_mot_T1,controlled_mot_T2,controlled_mot_T3,
                                               autonomous_mot_T1,autonomous_mot_T2,autonomous_mot_T3))

incomplete_data <- quant_data_long2[is.na(quant_data_long2$Basic_needs) | is.na(quant_data_long2$Amotivation) | is.na(quant_data_long2$Controlled_motivation)
                                    | is.na(quant_data_long2$Autonomous_motivation), ]

complete_data_count <- length(unique(quant_data_long2$ID)) - length(unique(incomplete_data$ID))
incomplete_data_count <- length(unique(incomplete_data$ID))

complete_data_count #83 complete cases
incomplete_data_count #188 incomplete cases


Missingsummary<- dfSummary(main_variables, round.digits = 2)

view(Missingsummary)

#---Correlational analyses#### 
#4) Psychological need satisfaction will positively correlate with autonomous motivation, and negatively with controlled motivation and amotivation

#--Main variables
main_variables_corr_long <- subset(quant_data_long2,select = c(Basic_needs,Controlled_motivation, Autonomous_motivation))

cor.plot(main_variables_corr_long, main = "Correlation of main study variables", stars = TRUE, upper=FALSE)

#--Between and within group correlations (individual)
main_variables_corr_desc_individ <- subset(quant_data_long2,select = c(Basic_needs,Amotivation, Controlled_motivation, Autonomous_motivation, ID))

corr_BG_WH_individual <- statsBy(main_variables_corr_desc_individ, group=main_variables_corr_desc_individ$ID, cors = TRUE, 
                                 cor="cor", method="pearson")

print(corr_BG_WH_individual, short=FALSE)
corr_BG_WH_individual$pbg
corr_BG_WH_individual$pwg

#--Between and within group correlations (timepoints)
main_variables_corr_desc_time <- subset(quant_data_long2,select = c(Basic_needs,Amotivation, Controlled_motivation, Autonomous_motivation, Timepoints))

corr_BG_WH_time<- statsBy(main_variables_corr_desc_time, group=main_variables_corr_desc_time$Timepoints, cors = TRUE, 
                          cor="cor", method="pearson")

print(corr_BG_WH_time, short=FALSE)
corr_BG_WH_time$pwg


#-Create APA table
library(apaTables)

apa.cor.table(main_variables_corr_long, filename = "Table2.doc", table.number = 2,
              show.conf.interval = TRUE, landscape = TRUE)

#---Linear mixed effects model for grades####
library(lme4)
library(nlme)
library(lmerTest) 
library(lmtest)
library(sandwich)
library(report)
library(performance)
library(modelsummary)
library(pscl)
library(r2mlm) 
library(ggplot2)

#Recode for intereaction between DV and activity
table(quant_data_long2$faculty)
table(quant_data_long2$gender)
table(quant_data_long2$study_year)

quant_data_long2$faculty <- factor(quant_data_long2$faculty, labels = c("HF", "MN")) #0=HF, 1=MN
quant_data_long2$gender <- factor(quant_data_long2$gender, labels = c("females", "males")) #0=female, 1=male
quant_data_long2$study_year <-  factor(quant_data_long2$study_year, labels = c("first_year", "second_year","fourth_year")) #0=fist year, 1=second year, 2=fourth year

table(quant_data_long2$faculty)
table(quant_data_long2$gender)
table(quant_data_long2$study_year)

#1a) autonomous motivation will predict higher grades and lower dropout. 
#1b) controlled motivation and amotivation will predict lower grades and higher dropout. 
#1c) Males will have higher dropout rates than females, whereas females will have higher grades. 
#2) Students in the fourth year will have higher autonomous motivation and lower controlled motivation compared to first-year students.
#3) The effect of motivations on grades and dropout will vary across individuals. 


#-------Autonomous motivation####
#2) Students in the fourth year will have higher autonomous motivation and lower controlled motivation compared to first-year students.


#ICC check Autonomous motivation
null_model_Autonomous_motivation <- lmer(Autonomous_motivation ~ 1 + (1|ID), data = quant_data_long2, REML = FALSE) # note that REML = FALSE
performance::icc(null_model_Autonomous_motivation)

#Visualizing changes over time Autonomous motivation
quant_data_long2 %>% 
  ggplot(aes(Timepoints, Autonomous_motivation, group=1))  + 
  stat_summary(fun.data = mean_cl_boot, geom="ribbon", alpha=.3) + 
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Autonomous motivation", 
       title="Autonomous motivation across semesters", subtitle = "Mean and Boostrapped 95% Confidence Intervals")
ggsave(filename="quant_fig_Autonomous_motivation.jpg", width=36, height=22, units="cm", dpi=200)

#Autonomous motivation by factors

#-Study year
quant_data_long2 %>% 
  ggplot(aes(Timepoints, Autonomous_motivation, group=study_year, fill=study_year))  + 
  stat_summary(fun.data = mean_se, geom="ribbon", alpha=.1) +
  stat_summary(fun.y = mean, geom="line") + 
  labs(x="Time interval", y = "Autonomous motivation", 
       title="Autonomous motivation across semesters as a function of Study year",
       subtitle = "Mean +/- 1 Standard Error", color="Study year", fill="Study year")
ggsave(filename="quant_fig_Autonomous_motivation_studyyear.jpg", width=36, height=22, units="cm", dpi=200)

#-Adding predictors random intercept
autmot_model1 <- lme(Autonomous_motivation ~ 1 + study_year + Timepoints,
                     data = quant_data_long2,
                     method="ML",
                     na.action = "na.omit",
                     random = ~ 1|ID)
summary(autmot_model1)

autmot_model1_report <- report(autmot_model1)
as.data.frame(autmot_model1_report)
autmot_model1_report
VarCorr(autmot_model1)

#-Adding interaction random intercept
autmot_model2 <- lme(Autonomous_motivation ~ 1 + study_year + Timepoints +
                       study_year:Timepoints,
                     data = quant_data_long2,
                     method="ML",
                     na.action = "na.omit",
                     random = ~ 1|ID)
summary(autmot_model2)

#-Comparing models #model 1 better than 2
anova(autmot_model1, autmot_model2)

autmot_model1_report <- report(autmot_model1)

autmot_model1_report
as.data.frame(autmot_model1_report)

#-Adding study year as random intercept and random slope 
autmot_model3 <- lme(Autonomous_motivation ~ 1 + study_year + Timepoints,
                     data = quant_data_long2,
                     method="ML",
                     na.action = "na.omit",
                     random = ~ study_year|ID)

summary(autmot_model3)

#-Comparing models #model 1 better than 3
anova(autmot_model1, autmot_model3)

#-Creating a table for all models
modelsummary(list("Null model"=null_model_Autonomous_motivation,"Random intercept model" = autmot_model1, "Random intercept/interaction model"=autmot_model2,
                  "Random intercept and randome slope"=autmot_model3 ), stars = TRUE, gof_omit = 'RMSE|BIC|Obs',
             notes = "Standard errors within parentheses", output = 'Autmixed.docx') 

#-------Grades####

#ICC check Grades #97%
null_model_grades <- lmer(grades ~ 1 + (1|ID), data = quant_data_long2, REML = FALSE) # note that REML = FALSE
performance::icc(null_model_grades)


#Regression model (centered)
Gradesreg <- lm(grades ~ scale(Amotivation, scale = FALSE) + scale(Controlled_motivation, scale = FALSE) + 
                  scale(Autonomous_motivation, scale = FALSE) + gender + scale(HS_average, scale = FALSE),
                data = quant_data_long2)

summary(Gradesreg)

Gradesreg_report<- report(Gradesreg)
Gradesreg_report
as.data.frame(Gradesreg_report)
coef(Gradesreg)

model_performance(Gradesreg_report)

modelsummary(list("Multiple regression model" = Gradesreg_report), stars = TRUE, gof_omit = 'RMSE|BIC|Obs',
             notes = "Standard errors within parentheses", output = 'results.docx')

#Regression model cluster robust standard error (centered)
Gradesreg_crse <- coeftest(Gradesreg, vcov. = vcovHC, cluster = ~ ID)
Gradesreg_crse
confint(Gradesreg_crse)
logLik(Gradesreg_crse)
AIC(Gradesreg_crse)
BIC(Gradesreg_crse)
performance(Gradesreg_crse)

#-------Dropout####

#1a) autonomous motivation will predict lower dropout. 
#1b) controlled motivation and amotivation will predict higher dropout. 
#1c) Males will have higher dropout rates than females
#3) The effect of motivations on dropout will vary across individuals. 

#Descriptives
#---gender differences on dropout
xtabs(~semester_reg+gender, data = quant_data)
xtabs(~study_points+gender, data = quant_data)

#---logistic regression model for study points
#Standard logistic regression model (null model)
logreg_studypoints_null <- glm(study_points ~  1, family = binomial(link = "logit"),
                               data=quant_data_long2)

summary(logreg_studypoints_null)


#Random intercept
logreg_studypoints_randomintercept <-  glmer(study_points ~ 1 + (1|ID), data=quant_data_long2,family = binomial(link = "logit"),
                                             control = glmerControl(optimizer = "bobyqa"),
                                             nAGQ=1)

summary(logreg_studypoints_randomintercept)
performance::icc(logreg_studypoints_randomintercept) #ICC 99.9%

#Standard logistic regression 
logreg_studypoints <- glm(study_points ~ Amotivation  + Controlled_motivation + Autonomous_motivation +
                             gender, 
                          family=binomial(link = "logit"), data=quant_data_long2)
summary(logreg_studypoints)

exp(logreg_studypoints$coefficients)
exp(confint(logreg_studypoints))

logreg_studypoints_report <- report(logreg_studypoints)
logreg_studypoints_report
as.data.frame(logreg_studypoints_report)

#Preudo R2 of the model
pR2(logreg_studypoints) #McFadden= .01, Cox & Snell= .01, Nagelkerke= .02

#---logistic regression model for semester registration (null model)
logreg_semreg_null <- glm(semester_reg ~  1,
                          data=quant_data_long2)
summary(logreg_semreg_null)

#Random intercept
logreg_semreg_randomintercept <-  glmer(semester_reg ~ 1 + (1|ID), data=quant_data_long2,family = binomial(link = "logit"),
                                        control = glmerControl(optimizer = "bobyqa"),
                                        nAGQ=0)

summary(logreg_semreg_randomintercept)

performance::icc(logreg_semreg_randomintercept) #ICC 94.6%

#Standard logistic regression
logreg_semreg <- glm(semester_reg ~ Amotivation + Controlled_motivation + Autonomous_motivation +
                           gender, 
                           family=binomial(link = "logit"), data=quant_data_long2)

summary(logreg_semreg)
exp(logreg_semreg$coefficients)
exp(confint(logreg_semreg))

logreg_semreg_report <- report(logreg_semreg)
logreg_semreg_report
as.data.frame(logreg_semreg_report)

#Preudo R2 of the model
pR2(logreg_semreg)["McFadden"] 
pR2(logreg_semreg) #McFadden= 0.256, Cox & Snell= 0.101, Nagelkerke= 0.297

#-------Path analysis grades####

library(lavaan)
library(semPlot)
library(parameters)
#5) Psychological need satisfaction will mediate the effects of motivation on grades and dropout across time.

model_grades1 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3
  controlled_motivation=~ controlled_mot_T1 + controlled_mot_T2 + controlled_mot_T3
  
  # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  controlled_mot_T1 ~~ controlled_mot_T2
  controlled_mot_T1 ~~ controlled_mot_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*controlled_motivation + c*autonomous_motivation 
  grades ~ d*basic_needs
  
  # Mediators
  indirect1:= a*d 
  indirect2:= b*d
  indirect3:= c*d

'

fit_model_grades1 <- sem(model_grades1, data = quant_data, missing="FIML")

# Summarize results
summary(fit_model_grades1, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

# Removing controlled motivation
model_grades2 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3

  # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*autonomous_motivation 
  grades ~ c*basic_needs
  
  # Mediators
  indirect1:= a*c 
  indirect2:= b*c

'

fit_model_grades2 <- sem(model_grades2, data = quant_data, missing="FIML")

# Summarize results
summary(fit_model_grades2, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

#Comparing models
lavTestLRT(fit_model_grades1, fit_model_grades2, type = "Chisq", model.names = NULL) #Chi-square test

cbind(model1=inspect(fit_model_grades1, 'fit.measures'), model2=inspect(fit_model_grades2, 'fit.measures')) #Compare fit measures 


#-------Path analysis semester registration####

model_semreg1 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3
  controlled_motivation=~ controlled_mot_T1 + controlled_mot_T2 +controlled_mot_T3
  
   # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  controlled_mot_T1 ~~ controlled_mot_T2
  controlled_mot_T1 ~~ controlled_mot_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*controlled_motivation + c*autonomous_motivation 
  semester_reg ~ d*basic_needs 
  
  # Mediators
  indirect1:= a*d 
  indirect2:= b*d
  indirect3:= c*d
  
'

fit_model_model_semreg1 <- sem(model_semreg1, data = quant_data,estimator = "WLSMV")

# Summarize results
summary(fit_model_model_semreg1, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

#Removing controlled motivation
model_semreg2 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3

   # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*autonomous_motivation 
  semester_reg ~ c*basic_needs 
  
  # Mediators
  indirect1:= a*c 
  indirect2:= b*c

'

fit_model_model_semreg2 <- sem(model_semreg2, data = quant_data,estimator = "WLSMV")

summary(fit_model_model_semreg2, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

fitMeasures(fit_model_model_semreg2)

#Comparing models
cbind(model1=inspect(fit_model_model_semreg1, 'fit.measures'), model2=inspect(fit_model_model_semreg2, 'fit.measures')) #Compare fit measures 

#-------Path analysis study points#### 
model_studypoints1 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  controlled_motivation=~ controlled_mot_T1 + controlled_mot_T2 + controlled_mot_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3
  
   # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  controlled_mot_T1 ~~ controlled_mot_T2
  controlled_mot_T2 ~~ controlled_mot_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*controlled_motivation + c*autonomous_motivation 
  study_points ~ d*basic_needs 
  
  # Mediators
  indirect1:= a*d 
  indirect2:= b*d
  indirect3:= c*d
  
'

fit_model_studypoints1 <- sem(model_studypoints1, data = quant_data,estimator = "WLSMV")

# Summarize results
summary(fit_model_studypoints1, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

#Removing controlled motivation
model_studypoints2 <- '
  # Define latent variables
  basic_needs =~ basic_needs_T1 + basic_needs_T2 + basic_needs_T3
  amotivation=~ amotivation_T1 + amotivation_T2 + amotivation_T3
  autonomous_motivation =~ autonomous_mot_T1 + autonomous_mot_T2 + autonomous_mot_T3
  
   # Residual covariances
  basic_needs_T1 ~~ basic_needs_T2
  basic_needs_T2 ~~ basic_needs_T3
  
  amotivation_T1 ~~ amotivation_T2
  amotivation_T2 ~~ amotivation_T3
  
  autonomous_mot_T1 ~~ autonomous_mot_T2
  autonomous_mot_T2 ~~ autonomous_mot_T3
  
  # Define observed variables
  basic_needs ~ a*amotivation + b*autonomous_motivation 
  study_points ~ c*basic_needs 
  
  # Mediators
  indirect1:= a*c 
  indirect2:= b*c
  
'

fit_model_studypoints2 <- sem(model_studypoints2, data = quant_data,estimator = "WLSMV")

# Summarize results
summary(fit_model_studypoints2, standardized = TRUE, estimates = TRUE, 
        header = TRUE, ci = TRUE, fit.measures = TRUE, rsq = TRUE)

