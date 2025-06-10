#####
#Meta-Analysis for dyadic effects of Studies 3A, 3B, & 3C
#####

library(rio)
library(lavaan)
library(metafor)

setwd("X:/OneDrive/Envy/Entitlement, Status, and Envy/Schriftliches/Documentation for OSF/Studies 3A, 3B, & 3C")

data_3A <- import("Study3A_dyadic.csv")
data_3B <- import("Study3B_dyadic.csv")
data_3C <- import("Study3C_dyadic.csv")

#####
#Path Models
#####

###Self-ratings

model_self <-
'
##regressions

p_self_1 ~ ent1p1*pes_1 + ent2p1*pes_2
d_self_1 ~ ent1d1*pes_1 + ent2d1*pes_2

p_self_2 ~ ent1p2*pes_1 + ent2p2*pes_2
d_self_2 ~ ent1d2*pes_1 + ent2d2*pes_2

##covariances

pes_1 ~~ ent1ent2*pes_2

p_self_1 ~~ p1d1*d_self_1
p_self_1 ~~ p1p2*p_self_2
p_self_1 ~~ p1d2*d_self_2
d_self_1 ~~ d1p2*p_self_2
d_self_1 ~~ d1d2*d_self_2
p_self_2 ~~ p2d2*d_self_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

#covariances

p1d1 == p2d2
p1d2 == d1p2

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_self_1 ~~ varp*p_self_1
p_self_2 ~~ varp*p_self_2

d_self_1 ~~ vard*d_self_1
d_self_2 ~~ vard*d_self_2

#intercepts

p_self_1 + p_self_2 ~ intp*1
d_self_1 + d_self_2 ~ intd*1
'

###Peer-ratings

model_peer <-
'
##regressions

p_other_1 ~ ent1p1*pes_1 + ent2p1*pes_2
d_other_1 ~ ent1d1*pes_1 + ent2d1*pes_2

p_other_2 ~ ent1p2*pes_1 + ent2p2*pes_2
d_other_2 ~ ent1d2*pes_1 + ent2d2*pes_2

##covariances

pes_1 ~~ ent1ent2*pes_2

p_other_1 ~~ p1d1*d_other_1
p_other_1 ~~ p1p2*p_other_2
p_other_1 ~~ p1d2*d_other_2
d_other_1 ~~ d1p2*p_other_2
d_other_1 ~~ d1d2*d_other_2
p_other_2 ~~ p2d2*d_other_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

#covariances

p1d1 == p2d2
p1d2 == d1p2

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_other_1 ~~ varp*p_other_1
p_other_2 ~~ varp*p_other_2

d_other_1 ~~ vard*d_other_1
d_other_2 ~~ vard*d_other_2

#intercepts

p_other_1 + p_other_2 ~ intp*1
d_other_1 + d_other_2 ~ intd*1
'

###Fitting models for self-ratings

fit_model_self_3A <- sem(model_self, likelihood = "wishart", data = data_3A)
fit_model_self_3B <- sem(model_self, likelihood = "wishart", data = data_3B)
fit_model_self_3C <- sem(model_self, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_self_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_3A)

summary(fit_model_self_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_3B)

summary(fit_model_self_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_3C)

###Fitting models for peer-ratings

fit_model_peer_3A <- sem(model_peer, likelihood = "wishart", data = data_3A)
fit_model_peer_3B <- sem(model_peer, likelihood = "wishart", data = data_3B)
fit_model_peer_3C <- sem(model_peer, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_peer_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_3A)

summary(fit_model_peer_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_3B)

summary(fit_model_peer_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_3C)

#####
#Meta-Analysis
#####

###Multivariate random-effects meta-analysis with correlated outcomes (Berkeley et al., 1998; see metafor Homepage)

###constructing the data set
#As effect size, I take the standardized regression coefficients, because outcomes were measured with different scales across studies.
#Standardized regression coefficient as effect size taken from multiple regressions equations is controversial, but simulations support that it is adequate (e.g., Kim, 2011) and we even tested the exact same model across studies, rendering the standardized regression coefficients directly comparable.
#I extract the standardized regression weights directly from lavaan objects.
#For each study, I need the standardized regression weight from the model for both prestige and dominance.
#To control for the correlated nature of these effects, I need the variance-covariance matrix of the effects.
#That is, two variables in the data frame form a matrix with the variance of the respective paths denoted by the row on the diagonal and the covariance of the effects on the other diagonal (see metafor homepage).
#I take the variance-covariance parameters directly from the lavaan objects.
#In the meta-analysis I assume an unstructured variance-covariance matrix (different variances per regression weight and different covariances).
#All the above needs to be done for both self- and peer-ratings separately.

data_meta <- data.frame(Study = rep(c("3A","3B","3C"), each = 2),
                        Variable = rep(c("prestige","dominance"), 3),
                        Beta_self = c(parameterEstimates(fit_model_self_3A, standardized = TRUE)[[1,12]],
                                      parameterEstimates(fit_model_self_3A, standardized = TRUE)[[3,12]],
                                      parameterEstimates(fit_model_self_3B, standardized = TRUE)[[1,12]],
                                      parameterEstimates(fit_model_self_3B, standardized = TRUE)[[3,12]],
                                      parameterEstimates(fit_model_self_3C, standardized = TRUE)[[1,12]],
                                      parameterEstimates(fit_model_self_3C, standardized = TRUE)[[3,12]]),
                        v1_self = c(lavInspect(fit_model_self_3A, "vcov.std.all")[[1,1]],
                                    lavInspect(fit_model_self_3A, "vcov.std.all")[[1,3]],
                                    lavInspect(fit_model_self_3B, "vcov.std.all")[[1,1]],
                                    lavInspect(fit_model_self_3B, "vcov.std.all")[[1,3]],
                                    lavInspect(fit_model_self_3C, "vcov.std.all")[[1,1]],
                                    lavInspect(fit_model_self_3C, "vcov.std.all")[[1,3]]),
                        v2_self = c(lavInspect(fit_model_self_3A, "vcov.std.all")[[1,3]],
                                    lavInspect(fit_model_self_3A, "vcov.std.all")[[3,3]],
                                    lavInspect(fit_model_self_3B, "vcov.std.all")[[1,3]],
                                    lavInspect(fit_model_self_3B, "vcov.std.all")[[3,3]],
                                    lavInspect(fit_model_self_3C, "vcov.std.all")[[1,3]],
                                    lavInspect(fit_model_self_3C, "vcov.std.all")[[3,3]]),
                        Beta_peer = c(parameterEstimates(fit_model_peer_3A, standardized = TRUE)[[5,12]],
                                      parameterEstimates(fit_model_peer_3A, standardized = TRUE)[[7,12]],
                                      parameterEstimates(fit_model_peer_3B, standardized = TRUE)[[5,12]],
                                      parameterEstimates(fit_model_peer_3B, standardized = TRUE)[[7,12]],
                                      parameterEstimates(fit_model_peer_3C, standardized = TRUE)[[5,12]],
                                      parameterEstimates(fit_model_peer_3C, standardized = TRUE)[[7,12]]),
                        v1_peer = c(lavInspect(fit_model_peer_3A, "vcov.std.all")[[2,2]],
                                    lavInspect(fit_model_peer_3A, "vcov.std.all")[[2,4]],
                                    lavInspect(fit_model_peer_3B, "vcov.std.all")[[2,2]],
                                    lavInspect(fit_model_peer_3B, "vcov.std.all")[[2,4]],
                                    lavInspect(fit_model_peer_3C, "vcov.std.all")[[2,2]],
                                    lavInspect(fit_model_peer_3C, "vcov.std.all")[[2,4]]),
                        v2_peer = c(lavInspect(fit_model_peer_3A, "vcov.std.all")[[2,4]],
                                    lavInspect(fit_model_peer_3A, "vcov.std.all")[[4,4]],
                                    lavInspect(fit_model_peer_3B, "vcov.std.all")[[2,4]],
                                    lavInspect(fit_model_peer_3B, "vcov.std.all")[[4,4]],
                                    lavInspect(fit_model_peer_3C, "vcov.std.all")[[2,4]],
                                    lavInspect(fit_model_peer_3C, "vcov.std.all")[[4,4]]))

###building entire variance-covariance matrix for the three studies

#self-ratings
V_self <- bldiag(lapply(split(data_meta[,c("v1_self", "v2_self")], data_meta$Study), as.matrix))

#peer-ratings
V_peer <- bldiag(lapply(split(data_meta[,c("v1_peer", "v2_peer")], data_meta$Study), as.matrix))

###meta-analysis for self-ratings

meta_self <- rma.mv(Beta_self, V_self, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta, method="ML")
meta_self

###meta-analysis for peer-ratings

meta_peer <- rma.mv(Beta_peer, V_peer, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta, method="ML")
meta_peer

##########
###Repeating the analyses with closeness as covariate

cor.test(data_3A$closeness_1,data_3A$closeness_2)
cor.test(data_3B$closeness_1,data_3B$closeness_2)
cor.test(data_3C$closeness_1,data_3C$closeness_2)

#given the high correlations, I average the partners' closeness ratings.

data_3A$closeness <- rowMeans(data_3A[c("closeness_1","closeness_2")], na.rm = TRUE)
data_3B$closeness <- rowMeans(data_3B[c("closeness_1","closeness_2")], na.rm = TRUE)
data_3C$closeness <- rowMeans(data_3C[c("closeness_1","closeness_2")], na.rm = TRUE)

###Self-ratings

model_self_closeness <-
'
##regressions

p_self_1 ~ ent1p1*pes_1 + ent2p1*pes_2 + covp1*closeness
d_self_1 ~ ent1d1*pes_1 + ent2d1*pes_2 + covd1*closeness

p_self_2 ~ ent1p2*pes_1 + ent2p2*pes_2 + covp2*closeness
d_self_2 ~ ent1d2*pes_1 + ent2d2*pes_2 + covd2*closeness

##covariances

pes_1 ~~ ent1ent2*pes_2
pes_1 ~~ ent1cov*closeness
pes_2 ~~ ent2cov*closeness

p_self_1 ~~ p1d1*d_self_1
p_self_1 ~~ p1p2*p_self_2
p_self_1 ~~ p1d2*d_self_2
d_self_1 ~~ d1p2*p_self_2
d_self_1 ~~ d1d2*d_self_2
p_self_2 ~~ p2d2*d_self_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

covp1 == covp2
covd1 == covd2

#covariances

p1d1 == p2d2
p1d2 == d1p2

ent1cov == ent2cov

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_self_1 ~~ varp*p_self_1
p_self_2 ~~ varp*p_self_2

d_self_1 ~~ vard*d_self_1
d_self_2 ~~ vard*d_self_2

#intercepts

p_self_1 + p_self_2 ~ intp*1
d_self_1 + d_self_2 ~ intd*1
'

###Peer-ratings

model_peer_closeness <-
'
##regressions

p_other_1 ~ ent1p1*pes_1 + ent2p1*pes_2 + covp1*closeness
d_other_1 ~ ent1d1*pes_1 + ent2d1*pes_2 + covd1*closeness

p_other_2 ~ ent1p2*pes_1 + ent2p2*pes_2 + covp2*closeness
d_other_2 ~ ent1d2*pes_1 + ent2d2*pes_2 + covd2*closeness

##covariances

pes_1 ~~ ent1ent2*pes_2
pes_1 ~~ ent1cov*closeness
pes_2 ~~ ent2cov*closeness

p_other_1 ~~ p1d1*d_other_1
p_other_1 ~~ p1p2*p_other_2
p_other_1 ~~ p1d2*d_other_2
d_other_1 ~~ d1p2*p_other_2
d_other_1 ~~ d1d2*d_other_2
p_other_2 ~~ p2d2*d_other_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

covp1 == covp2
covd1 == covd2

#covariances

p1d1 == p2d2
p1d2 == d1p2

ent1cov == ent2cov

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_other_1 ~~ varp*p_other_1
p_other_2 ~~ varp*p_other_2

d_other_1 ~~ vard*d_other_1
d_other_2 ~~ vard*d_other_2

#intercepts

p_other_1 + p_other_2 ~ intp*1
d_other_1 + d_other_2 ~ intd*1
'

###Fitting models for self-ratings

fit_model_self_closeness_3A <- sem(model_self_closeness, likelihood = "wishart", data = data_3A)
fit_model_self_closeness_3B <- sem(model_self_closeness, likelihood = "wishart", data = data_3B)
fit_model_self_closeness_3C <- sem(model_self_closeness, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_self_closeness_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_closeness_3A)

summary(fit_model_self_closeness_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_closeness_3B)

summary(fit_model_self_closeness_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_closeness_3C)

###Fitting models for peer-ratings

fit_model_peer_closeness_3A <- sem(model_peer_closeness, likelihood = "wishart", data = data_3A)
fit_model_peer_closeness_3B <- sem(model_peer_closeness, likelihood = "wishart", data = data_3B)
fit_model_peer_closeness_3C <- sem(model_peer_closeness, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_peer_closeness_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_closeness_3A)

summary(fit_model_peer_closeness_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_closeness_3B)

summary(fit_model_peer_closeness_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_closeness_3C)

#####
#Meta-Analysis - closeness as covariate
#####

###Multivariate random-effects meta-analysis with correlated outcomes (Berkeley et al., 1998; see metafor Homepage)

###constructing the data set
#same as for meta-analysis without closeness as covariate

data_meta_closeness <- data.frame(Study = rep(c("3A","3B","3C"), each = 2),
                                  Variable = rep(c("prestige","dominance"), 3),
                                  Beta_self = c(parameterEstimates(fit_model_self_closeness_3A, standardized = TRUE)[[1,12]],
                                                parameterEstimates(fit_model_self_closeness_3A, standardized = TRUE)[[4,12]],
                                                parameterEstimates(fit_model_self_closeness_3B, standardized = TRUE)[[1,12]],
                                                parameterEstimates(fit_model_self_closeness_3B, standardized = TRUE)[[4,12]],
                                                parameterEstimates(fit_model_self_closeness_3C, standardized = TRUE)[[1,12]],
                                                parameterEstimates(fit_model_self_closeness_3C, standardized = TRUE)[[4,12]]),
                                  v1_self = c(lavInspect(fit_model_self_closeness_3A, "vcov.std.all")[[1,1]],
                                              lavInspect(fit_model_self_closeness_3A, "vcov.std.all")[[1,4]],
                                              lavInspect(fit_model_self_closeness_3B, "vcov.std.all")[[1,1]],
                                              lavInspect(fit_model_self_closeness_3B, "vcov.std.all")[[1,4]],
                                              lavInspect(fit_model_self_closeness_3C, "vcov.std.all")[[1,1]],
                                              lavInspect(fit_model_self_closeness_3C, "vcov.std.all")[[1,4]]),
                                  v2_self = c(lavInspect(fit_model_self_closeness_3A, "vcov.std.all")[[1,4]],
                                              lavInspect(fit_model_self_closeness_3A, "vcov.std.all")[[4,4]],
                                              lavInspect(fit_model_self_closeness_3B, "vcov.std.all")[[1,4]],
                                              lavInspect(fit_model_self_closeness_3B, "vcov.std.all")[[4,4]],
                                              lavInspect(fit_model_self_closeness_3C, "vcov.std.all")[[1,4]],
                                              lavInspect(fit_model_self_closeness_3C, "vcov.std.all")[[4,4]]),
                                  Beta_peer = c(parameterEstimates(fit_model_peer_closeness_3A, standardized = TRUE)[[7,12]],
                                                parameterEstimates(fit_model_peer_closeness_3A, standardized = TRUE)[[10,12]],
                                                parameterEstimates(fit_model_peer_closeness_3B, standardized = TRUE)[[7,12]],
                                                parameterEstimates(fit_model_peer_closeness_3B, standardized = TRUE)[[10,12]],
                                                parameterEstimates(fit_model_peer_closeness_3C, standardized = TRUE)[[7,12]],
                                                parameterEstimates(fit_model_peer_closeness_3C, standardized = TRUE)[[10,12]]),
                                  v1_peer = c(lavInspect(fit_model_peer_closeness_3A, "vcov.std.all")[[2,2]],
                                              lavInspect(fit_model_peer_closeness_3A, "vcov.std.all")[[2,5]],
                                              lavInspect(fit_model_peer_closeness_3B, "vcov.std.all")[[2,2]],
                                              lavInspect(fit_model_peer_closeness_3B, "vcov.std.all")[[2,5]],
                                              lavInspect(fit_model_peer_closeness_3C, "vcov.std.all")[[2,2]],
                                              lavInspect(fit_model_peer_closeness_3C, "vcov.std.all")[[2,5]]),
                                  v2_peer = c(lavInspect(fit_model_peer_closeness_3A, "vcov.std.all")[[2,5]],
                                              lavInspect(fit_model_peer_closeness_3A, "vcov.std.all")[[5,5]],
                                              lavInspect(fit_model_peer_closeness_3B, "vcov.std.all")[[2,5]],
                                              lavInspect(fit_model_peer_closeness_3B, "vcov.std.all")[[5,5]],
                                              lavInspect(fit_model_peer_closeness_3C, "vcov.std.all")[[2,5]],
                                              lavInspect(fit_model_peer_closeness_3C, "vcov.std.all")[[5,5]]))

###building entire variance-covariance matrix for the three studies

#self-ratings
V_self_closeness <- bldiag(lapply(split(data_meta_closeness[,c("v1_self", "v2_self")], data_meta_closeness$Study), as.matrix))

#peer-ratings
V_peer_closeness <- bldiag(lapply(split(data_meta_closeness[,c("v1_peer", "v2_peer")], data_meta_closeness$Study), as.matrix))

###meta-analysis for self-ratings

meta_self_closeness <- rma.mv(Beta_self, V_self, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta_closeness, method="ML")
meta_self_closeness

###meta-analysis for peer-ratings

meta_peer_closeness <- rma.mv(Beta_peer, V_peer, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta_closeness, method="ML")
meta_peer_closeness

##########
###Repeating the analyses with gender composition as covariate

data_3A$gender_comp <- factor(data_3A$gender_comp, levels = c("same-gender male","same-gender female","mixed"), labels = c("same-gender male","same-gender female","mixed"))
data_3B$gender_comp <- factor(data_3B$gender_comp, levels = c("same-gender male","same-gender female","mixed"), labels = c("same-gender male","same-gender female","mixed"))
data_3C$gender_comp <- factor(data_3C$gender_comp, levels = c("same-gender male","same-gender female","mixed"), labels = c("same-gender male","same-gender female","mixed"))

data_3A$gender_comp <- as.numeric(data_3A$gender_comp)
data_3B$gender_comp <- as.numeric(data_3B$gender_comp)
data_3C$gender_comp <- as.numeric(data_3C$gender_comp)

#forming two dummy variables
#d1) same-gender vs. mixed-gender
#d2) same-gender male vs. same-gender female

data_3A$gender_d1 <- ifelse(data_3A$gender_comp == 3, 1, -0.5)
data_3A$gender_d2 <- ifelse(data_3A$gender_comp == 3, 0, ifelse(data_3A$gender_comp == 1, 0.5, -0.5))

data_3B$gender_d1 <- ifelse(data_3B$gender_comp == 3, 1, -0.5)
data_3B$gender_d2 <- ifelse(data_3B$gender_comp == 3, 0, ifelse(data_3B$gender_comp == 1, 0.5, -0.5))

data_3C$gender_d1 <- ifelse(data_3C$gender_comp == 3, 1, -0.5)
data_3C$gender_d2 <- ifelse(data_3C$gender_comp == 3, 0, ifelse(data_3C$gender_comp == 1, 0.5, -0.5))

###Self-ratings

model_self_gender <-
'
##regressions

p_self_1 ~ ent1p1*pes_1 + ent2p1*pes_2 + cov1p1*gender_d1 + cov2p1*gender_d2
d_self_1 ~ ent1d1*pes_1 + ent2d1*pes_2 + cov1d1*gender_d1 + cov2d1*gender_d2

p_self_2 ~ ent1p2*pes_1 + ent2p2*pes_2 + cov1p2*gender_d1 + cov2p2*gender_d2
d_self_2 ~ ent1d2*pes_1 + ent2d2*pes_2 + cov1d2*gender_d1 + cov2d2*gender_d2

##covariances

pes_1 ~~ ent1ent2*pes_2
pes_1 ~~ ent1cov1*gender_d1
pes_1 ~~ ent1cov2*gender_d2
pes_2 ~~ ent2cov1*gender_d1
pes_2 ~~ ent2cov2*gender_d2
gender_d1 ~~ cov1cov2*gender_d2

p_self_1 ~~ p1d1*d_self_1
p_self_1 ~~ p1p2*p_self_2
p_self_1 ~~ p1d2*d_self_2
d_self_1 ~~ d1p2*p_self_2
d_self_1 ~~ d1d2*d_self_2
p_self_2 ~~ p2d2*d_self_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

cov1p1 == cov1p2
cov1d1 == cov1d2

cov2p1 == cov2p2
cov2d1 == cov2d2

#covariances

p1d1 == p2d2
p1d2 == d1p2

ent1cov1 == ent2cov1
ent1cov2 == ent2cov2

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_self_1 ~~ varp*p_self_1
p_self_2 ~~ varp*p_self_2

d_self_1 ~~ vard*d_self_1
d_self_2 ~~ vard*d_self_2

#intercepts

p_self_1 + p_self_2 ~ intp*1
d_self_1 + d_self_2 ~ intd*1
'

###Peer-ratings

model_peer_gender <-
'
##regressions

p_other_1 ~ ent1p1*pes_1 + ent2p1*pes_2 + cov1p1*gender_d1 + cov2p1*gender_d2
d_other_1 ~ ent1d1*pes_1 + ent2d1*pes_2 + cov1d1*gender_d1 + cov2d1*gender_d2

p_other_2 ~ ent1p2*pes_1 + ent2p2*pes_2 + cov1p2*gender_d1 + cov2p2*gender_d2
d_other_2 ~ ent1d2*pes_1 + ent2d2*pes_2 + cov1d2*gender_d1 + cov2d2*gender_d2

##covariances

pes_1 ~~ ent1ent2*pes_2
pes_1 ~~ ent1cov1*gender_d1
pes_1 ~~ ent1cov2*gender_d2
pes_2 ~~ ent2cov1*gender_d1
pes_2 ~~ ent2cov2*gender_d2
gender_d1 ~~ cov1cov2*gender_d2

p_other_1 ~~ p1d1*d_other_1
p_other_1 ~~ p1p2*p_other_2
p_other_1 ~~ p1d2*d_other_2
d_other_1 ~~ d1p2*p_other_2
d_other_1 ~~ d1d2*d_other_2
p_other_2 ~~ p2d2*d_other_2

##constraints

#paths

ent1p1 == ent2p2
ent1d1 == ent2d2

ent1p2 == ent2p1
ent1d2 == ent2d1

cov1p1 == cov1p2
cov1d1 == cov1d2

cov2p1 == cov2p2
cov2d1 == cov2d2

#covariances

p1d1 == p2d2
p1d2 == d1p2

ent1cov1 == ent2cov1
ent1cov2 == ent2cov2

#means

pes_1 + pes_2 ~ meansent*1

#variances

pes_1 ~~ varent*pes_1
pes_2 ~~ varent*pes_2

p_other_1 ~~ varp*p_other_1
p_other_2 ~~ varp*p_other_2

d_other_1 ~~ vard*d_other_1
d_other_2 ~~ vard*d_other_2

#intercepts

p_other_1 + p_other_2 ~ intp*1
d_other_1 + d_other_2 ~ intd*1
'

###Fitting models for self-ratings

fit_model_self_gender_3A <- sem(model_self_gender, likelihood = "wishart", data = data_3A)
fit_model_self_gender_3B <- sem(model_self_gender, likelihood = "wishart", data = data_3B)
fit_model_self_gender_3C <- sem(model_self_gender, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_self_gender_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_gender_3A)

summary(fit_model_self_gender_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_gender_3B)

summary(fit_model_self_gender_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_self_gender_3C)

###Fitting models for peer-ratings

fit_model_peer_gender_3A <- sem(model_peer_gender, likelihood = "wishart", data = data_3A)
fit_model_peer_gender_3B <- sem(model_peer_gender, likelihood = "wishart", data = data_3B)
fit_model_peer_gender_3C <- sem(model_peer_gender, likelihood = "wishart", data = data_3C)

#results
summary(fit_model_peer_gender_3A, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_gender_3A)

summary(fit_model_peer_gender_3B, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_gender_3B)

summary(fit_model_peer_gender_3C, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_peer_gender_3C)

#####
#Meta-Analysis - gender as covariate
#####

###Multivariate random-effects meta-analysis with correlated outcomes (Berkeley et al., 1998; see metafor Homepage)

###constructing the data set
#same as for meta-analysis without gender as covariate

data_meta_gender <- data.frame(Study = rep(c("3A","3B","3C"), each = 2),
                               Variable = rep(c("prestige","dominance"), 3),
                               Beta_self = c(parameterEstimates(fit_model_self_gender_3A, standardized = TRUE)[[1,12]],
                                             parameterEstimates(fit_model_self_gender_3A, standardized = TRUE)[[5,12]],
                                             parameterEstimates(fit_model_self_gender_3B, standardized = TRUE)[[1,12]],
                                             parameterEstimates(fit_model_self_gender_3B, standardized = TRUE)[[5,12]],
                                             parameterEstimates(fit_model_self_gender_3C, standardized = TRUE)[[1,12]],
                                             parameterEstimates(fit_model_self_gender_3C, standardized = TRUE)[[5,12]]),
                               v1_self = c(lavInspect(fit_model_self_gender_3A, "vcov.std.all")[[1,1]],
                                           lavInspect(fit_model_self_gender_3A, "vcov.std.all")[[1,5]],
                                           lavInspect(fit_model_self_gender_3B, "vcov.std.all")[[1,1]],
                                           lavInspect(fit_model_self_gender_3B, "vcov.std.all")[[1,5]],
                                           lavInspect(fit_model_self_gender_3C, "vcov.std.all")[[1,1]],
                                           lavInspect(fit_model_self_gender_3C, "vcov.std.all")[[1,5]]),
                               v2_self = c(lavInspect(fit_model_self_gender_3A, "vcov.std.all")[[1,5]],
                                           lavInspect(fit_model_self_gender_3A, "vcov.std.all")[[5,5]],
                                           lavInspect(fit_model_self_gender_3B, "vcov.std.all")[[1,5]],
                                           lavInspect(fit_model_self_gender_3B, "vcov.std.all")[[5,5]],
                                           lavInspect(fit_model_self_gender_3C, "vcov.std.all")[[1,5]],
                                           lavInspect(fit_model_self_gender_3C, "vcov.std.all")[[5,5]]),
                               Beta_peer = c(parameterEstimates(fit_model_peer_gender_3A, standardized = TRUE)[[9,12]],
                                             parameterEstimates(fit_model_peer_gender_3A, standardized = TRUE)[[13,12]],
                                             parameterEstimates(fit_model_peer_gender_3B, standardized = TRUE)[[9,12]],
                                             parameterEstimates(fit_model_peer_gender_3B, standardized = TRUE)[[13,12]],
                                             parameterEstimates(fit_model_peer_gender_3C, standardized = TRUE)[[9,12]],
                                             parameterEstimates(fit_model_peer_gender_3C, standardized = TRUE)[[13,12]]),
                               v1_peer = c(lavInspect(fit_model_peer_gender_3A, "vcov.std.all")[[2,2]],
                                           lavInspect(fit_model_peer_gender_3A, "vcov.std.all")[[2,6]],
                                           lavInspect(fit_model_peer_gender_3B, "vcov.std.all")[[2,2]],
                                           lavInspect(fit_model_peer_gender_3B, "vcov.std.all")[[2,6]],
                                           lavInspect(fit_model_peer_gender_3C, "vcov.std.all")[[2,2]],
                                           lavInspect(fit_model_peer_gender_3C, "vcov.std.all")[[2,6]]),
                               v2_peer = c(lavInspect(fit_model_peer_gender_3A, "vcov.std.all")[[2,6]],
                                           lavInspect(fit_model_peer_gender_3A, "vcov.std.all")[[6,6]],
                                           lavInspect(fit_model_peer_gender_3B, "vcov.std.all")[[2,6]],
                                           lavInspect(fit_model_peer_gender_3B, "vcov.std.all")[[6,6]],
                                           lavInspect(fit_model_peer_gender_3C, "vcov.std.all")[[2,6]],
                                           lavInspect(fit_model_peer_gender_3C, "vcov.std.all")[[6,6]]))

###building entire variance-covariance matrix for the three studies

#self-ratings
V_self_gender <- bldiag(lapply(split(data_meta_gender[,c("v1_self", "v2_self")], data_meta_gender$Study), as.matrix))

#peer-ratings
V_peer_gender <- bldiag(lapply(split(data_meta_gender[,c("v1_peer", "v2_peer")], data_meta_gender$Study), as.matrix))

###meta-analysis for self-ratings

meta_self_gender <- rma.mv(Beta_self, V_self, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta_gender, method="ML")
meta_self_gender

###meta-analysis for peer-ratings

meta_peer_gender <- rma.mv(Beta_peer, V_peer, mods = ~ Variable - 1, random = ~ Variable | Study, struct="UN", data=data_meta_gender, method="ML")
meta_peer_gender
