#####
#Analyses of Study 3A
#
#On OSF the data have been de-identified before uploading. The steps taken to achieve that are also described in the code.
#For the data file with the full information please contact Jens Lange.
#
#First parts of the code refer to the full file. It's commented out.
#####
#####

setwd("X:/OneDrive/Envy/Entitlement, Status, and Envy/Schriftliches/Documentation for OSF/Studies 3A, 3B, & 3C")

library(rio)
library(psych)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)
library(GPArotation)
library(paran)

# data <- import("Study3A.sav", header = TRUE)
# 
# #####
# #Editing the data set
# #####
# 
# #delete useless variables
# 
# data <- subset(data, select = -c(V2,V3,V4,V5,V6,V7,V8,V9,LocationLatitude,LocationLongitude,LocationAccuracy))
# 
# #exclude participants who did not finish the study (no actual participations)
# 
# data <- data[data$V10 == 1, ]
# 
# data <- subset(data, select = -c(V10))
# 
# #During data collection, thirteen dyads encountered technical difficulties, preventing participation.
# #These are no real participations. I therefore deleted them,
# 
# data <- data[data$dyad != 6 & data$dyad != 27 & data$dyad != 35 & data$dyad != 39 & data$dyad != 44
#              & data$dyad != 46 & data$dyad != 65 & data$dyad != 135 & data$dyad != 137
#              & data$dyad != 138 & data$dyad != 139 & data$dyad != 149 & data$dyad != 183, ]
# 
# #In two dyads, participant had problems handling the tablet. I deleted them.
# 
# data <- data[data$dyad != 30 & data$dyad != 118, ]
# 
# #In one dyad, one participant stopped for no reason. I deleted them.
# 
# data <- data[data$dyad != 101, ]
# 
# #One dyad number was given two times and it was impossible to determine who rated who. I deleted both dyads.
# 
# data <- data[data$dyad != 152, ]
# 
# ###recoding variables
# 
# ##correcting some errors
# 
# #correcting age errors to the best of my knowledge
# data[data$age == ",63", "age"] <- "63"
# data[data$age == "49,9", "age"] <- "49"
# data[data$age == "142", "age"] <- NA
# data[data$V1 == "R_xhllENHtTMboqvT", "age"] <- NA
# data[data$V1 == "R_bphUIuJOFETuFZD", "age"] <- "42"
# 
# #delete non-complete data set of a person who started anew after technical problems
# data <- data[data$V1 != "R_3h3bSoJQzIPa6Ug", ]
# 
# #For two dyads, partner values did not have one 1 and one 2. I recoded one of these values so that it is correct.
# data[data$V1 == "R_rcZNoA6oQ6rvmFj", "partner"] <- 1
# data[data$V1 == "R_29pj0WeMikCi4eV", "partner"] <- 1
# 
# ##demographic variables
# data$sex <- factor(data$sex, levels = c(1,2), labels = c("male","female"))
# data$native_language <- factor(data$native_language, levels = c(1,2), labels = c("German","Other"))
# data$age <- as.numeric(as.character(data$age))
# 
# #pes1 - "Ehrlich gesagt habe ich das Gefühl, Anrecht auf mehr als andere zu haben."
# #pes2 - "Mir sollte Tolles widerfahren."
# #pes3 - "Wenn ich auf der Titanic wäre, würde ich es verdienen, auf dem ersten Rettungsboot zu sein."
# #pes4 - "Ich verlange das Beste, weil ich es wert bin."
# #pes5 - "Ich verdiene nicht unbedingt eine besondere Behandlung."
# #pes6 - "Ich verdiene mehr Dinge in meinem Leben."
# #pes7 - "Menschen wie ich verdienen es, ab und zu mehr Glück zu haben als andere."
# #pes8 - "Dinge sollten in meinem Interesse ablaufen."
# #pes9 - "Ich fühle mich berechtigt, mehr von allem zu erhalten."
# #Responses were given on a scale from 1 (do not agree at all) to 7 (agree completely).
# #
# #p1_self - "Ich werde von anderen bewundert."
# #p2_self - "Ich bekomme Komplimente."
# #p3_self - "Ich erreiche meine hochgesteckten Ziele."
# #p4_self - "Ich bin ein Vorbild für andere."
# #p5_self - "Ich helfe anderen durch mein Wissen und meine Erfahrung."
# #d1_self - "Ich lästere über andere."
# #d2_self - "Ich bin schadenfroh."
# #d3_self - "Ich zwinge andere zu tun was ich möchte."
# #d4_self - "Ich werde von anderen gefürchtet."
# #d5_self - "Andere trauen sich nicht mir zu widersprechen."
# #
# #p1_other - "Mein/e Bekannte/r wird von anderen bewundert."
# #p2_other - "Mein/e Bekannte/r bekommt Komplimente."
# #p3_other - "Mein/e Bekannte/r erreicht seine/ihre hochgesteckten Ziele."
# #p4_other - "Mein/e Bekannte/r ist ein Vorbild für andere."
# #p5_other - "Mein/e Bekannte/r hilft anderen durch sein/ihr Wissen und seine/ihre Erfahrung."
# #d1_other - "Mein/e Bekannte/r lästert über andere."
# #d2_other - "Mein/e Bekannte/r ist schadenfroh."
# #d3_other - "Mein/e Bekannte/r zwingt andere zu tun was er/sie möchte."
# #d4_other - "Mein/e Bekannte/r wird von anderen gefürchtet."
# #d5_other - "Andere trauen sich nicht meinem/r Bekannten zu widersprechen."
# #Responsen were given on a scale from 1 (never), 2 (rarely), 3 (occasionally), 4 (often), 5 (very often)
# #to 6 (extremely often).
# #
# #closeness - "Ich kenne die andere Person ..."
# #Responsen were given on a scale from 1 (not at all), 2 (fleeting), 3 (a little bit), 4 (pretty well),
# #to 5 (extremely well).
# 
# ###From here on, you find the steps to de-identify the data to reach the version on OSF.
# 
# #Several ages occurred only once. I formed groups.
# 
# quantile(data$age, c(.33,.66), na.rm = TRUE)
# 
# data$age <- ifelse(data$age <= 24, 1, ifelse(data$age > 24 & data$age <= 37, 2, 3))
# 
# #age 1 'bottom third' 2 'middle third' 3 'upper third'
# 
# #As dyads of acquainted participants took part in the study, one partner could identify the other with his/her responses.
# #Therefore, it is reasonable to report only mean values of the scales.
# #Thus, below you will find all steps towards this point (i.e., reliability analysis, mean calculation, descriptives).
# 
# #####
# #Entitlement
# #####
# 
# data$pes5r <- 8 - data$pes5
# 
# psych::alpha(data.frame(data$pes1,data$pes2,data$pes3,data$pes4,data$pes5r,data$pes6,data$pes7,data$pes8,data$pes9))
# 
# data$pes <- rowMeans(data[c("pes1","pes2","pes3","pes4","pes5r","pes6","pes7","pes8","pes9")], na.rm = TRUE)
# 
# #####
# #Status
# #####
# 
# ###Self-ratings
# 
# paran(na.omit(data.frame(data$p1_self,data$p2_self,data$p3_self,data$p4_self,data$p5_self,
#                          data$d1_self,data$d2_self,data$d3_self,data$d4_self,data$d5_self)),
#       iterations = 1000, centile = 95, status = TRUE, all = TRUE, seed = 41)
# #parallel analysis suggest two components
# 
# pca_self <- principal(data.frame(data$p1_self,data$p2_self,data$p3_self,data$p4_self,data$p5_self,
#                                  data$d1_self,data$d2_self,data$d3_self,data$d4_self,data$d5_self),
#                       nfactors = 2, rotate = "oblimin")
# pca_self
# 
# rm(pca_self)
# 
# #self prestige
# 
# psych::alpha(data.frame(data$p1_self,data$p2_self,data$p3_self,data$p4_self,data$p5_self))
# 
# data$p_self <- rowMeans(data[c("p1_self","p2_self","p3_self","p4_self","p5_self")], na.rm = TRUE)
# 
# #self dominance
# 
# psych::alpha(data.frame(data$d1_self,data$d2_self,data$d3_self,data$d4_self,data$d5_self))
# 
# data$d_self <- rowMeans(data[c("d1_self","d2_self","d3_self","d4_self","d5_self")], na.rm = TRUE)
# 
# ###other-ratings
# 
# paran(na.omit(data.frame(data$p1_other,data$p2_other,data$p3_other,data$p4_other,data$p5_other,
#                          data$d1_other,data$d2_other,data$d3_other,data$d4_other,data$d5_other)),
#       iterations = 1000, centile = 95, status = TRUE, all = TRUE, seed = 41)
# #parallel analysis suggest two components
# 
# pca_other <- principal(data.frame(data$p1_other,data$p2_other,data$p3_other,data$p4_other,data$p5_other,
#                                   data$d1_other,data$d2_other,data$d3_other,data$d4_other,data$d5_other),
#                        nfactors = 2, rotate = "oblimin")
# pca_other
# 
# rm(pca_other)
# 
# #other prestige
# 
# psych::alpha(data.frame(data$p1_other,data$p2_other,data$p3_other,data$p4_other,data$p5_other))
# 
# data$p_other <- rowMeans(data[c("p1_other","p2_other","p3_other","p4_other","p5_other")], na.rm = TRUE)
# 
# #other dominance
# 
# psych::alpha(data.frame(data$d1_other,data$d2_other,data$d3_other,data$d4_other,data$d5_other))
# 
# data$d_other <- rowMeans(data[c("d1_other","d2_other","d3_other","d4_other","d5_other")], na.rm = TRUE)
# 
# ###selecting only the aggregated variables
# 
# data <- subset(data, select = c(dyad,partner,age,sex,native_language,closeness,pes:d_other))
# 
# #Final OSF version reached. Code below saves file. This is the data set on OSF.
# 
# export(data, "Study3A.csv")
# 
# #The code below allows you to run all the analyses from the paper as long as the respective variables are unchanged.

data <- import("Study3A.csv", header = TRUE)

#####
#Demographics
#####

data$sex <- factor(data$sex, levels = c("male","female"), labels = c("male","female"))
data$native_language <- factor(data$native_language, levels = c("German","Other"), labels = c("German","Other"))

describe(data.frame(data$age))
describe(data$closeness)

#####
#Analyses
#####

describe(data.frame(data$pes,data$p_self,data$d_self,data$p_other,data$d_other))

#####
###creating the dyadic data set

data_trans <- data
data_trans$sex <- as.numeric(data_trans$sex)
data_trans$native_language <- as.numeric(data_trans$native_language)

data_dyadic <- data_trans %>%
  mutate(partner = ifelse(partner == 1, "1", "2")) %>%
  gather(variable, value, age:d_other) %>%
  unite(var_partner, variable, partner) %>%
  spread(var_partner, value)

rm(data_trans)

###gender composition of dyads

data_dyadic$gender_comp <- ifelse(data_dyadic$sex_1 == data_dyadic$sex_2 & data_dyadic$sex_1 == 1, 1,
                                  ifelse(data_dyadic$sex_1 == data_dyadic$sex_2 & data_dyadic$sex_1 == 2, 2, 3))

data_dyadic$gender_comp <- factor(data_dyadic$gender_comp, levels = c(1,2,3), labels = c("same-gender male","same-gender female","mixed"))

summary(data.frame(data_dyadic$gender_comp))

###within-dyad correlations (in every other cell on the diagonal)

chart.Correlation(data_dyadic[, c("pes_1","pes_2","p_self_1","p_self_2","d_self_1","d_self_2",
                                  "p_other_1","p_other_2","d_other_1","d_other_2")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

###correlations separately for each partner

#partner 1
chart.Correlation(data_dyadic[ , c("pes_1","p_self_1","d_self_1","p_other_1","d_other_1")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

#partner 2
chart.Correlation(data_dyadic[ , c("pes_2","p_self_2","d_self_2","p_other_2","d_other_2")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

###convergence of self- and peer-ratings

#partner 1
cor.test(data_dyadic$p_self_1,data_dyadic$p_other_2)
cor.test(data_dyadic$d_self_1,data_dyadic$d_other_2)

#partner 2
cor.test(data_dyadic$p_self_2,data_dyadic$p_other_1)
cor.test(data_dyadic$d_self_2,data_dyadic$d_other_1)

export(data_dyadic, "Study3A_dyadic.csv")
