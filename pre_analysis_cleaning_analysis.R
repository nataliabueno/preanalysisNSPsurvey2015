#############################################################
# Analysis of Fake Data (Pre-Analysis Plan)
# To generate this data, I used both data from a pilot with undergraduate 
# students at UFPE, collected in July 2015
# This data sole purpose is to ilustrate data analysis and this 
# data does not reflect my theoretical expectations.
# Also, it should be noted that I changed aspects of the design and 
# implementation after the pilot. However, the pilot and the survey
# are similar enough to exemplify the type of analyses I will
# conducting with the real survey data
#############################################################

rm(list=ls())

library(foreign)
library(cjoint)
library(plyr)
library(dgof)

########################################################
#Cleaning
#Recoding
#Conjoint Analysis
#Vignette Analysis
########################################################

#Function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


######################################################

#Analysis of Pilot data

#Reading data from Qualtrics
#Not made available at this moment 
pilot <- read.csv("~/Dropbox/Dissertation/survey_experiment/data_testing/pilot.csv",
                  header=T, as.is=T)

#Eliminating those who did not make it pass filter
pilot <- pilot[-1,]
pilot <- pilot[!pilot$nsp1=="",]

#Eliminating unnecessary variables
pilot <- pilot[,-c(176:236)] #random order tracking, comments
pilot <- pilot[,-c(2:10)] # ID
pilot <- pilot[,-c(107:118)] # Survey markup

#Separating Conjoint, Vignette and Observational data

conjoint <- pilot[,c(1:121, 148:154)]
obs <- pilot[ ,c("V1", "rocha","santa", "check_pref", "check_ngos", "check_sc",       
                "fund_NGO", "Fund_hosp", "Fund_local", "gender", "age", "city.uf_1", "religion", "MW", 
                "schooling", "vote2014")]
vignette <- pilot[, c("V1", "cred_nsp", "resp_cnsp", "rate_cnsp2_1", "cred_pref", "resp_cpref",     
                        "rate_cpref_1", "cred_nsp2", "resp_cnsp2", "rate_cnsp2_1.1",  "cred_pref2",      
                        "resp_cpre2", "rate_cpre2_1", "blame_nsp", "resp_bnsp", "rate_bnsp_1",    
                        "blame_pref", "resp_bpref", "rate_bpref_1", "gender", "age", "city.uf_1", "religion", "MW", 
                        "schooling", "vote2014")]

####### Checking randomization in Conjoint

#Row randomization
table(pilot$Row1)
table(pilot$Row2)
table(pilot$Row3)
table(pilot$Row4)
table(pilot$Row5)
table(pilot$Row6)


#Making Conjoint data in individual-profile choice format as row:

conjoint <- conjoint[,-c(104:106)]

# For each respondent
RespId <- unique(conjoint$V1) 
list_c <- list()
for (i in 1:length(RespId)){
  temp <- conjoint[conjoint$V1==RespId[i],]
  col_attr <- as.matrix(temp[,c("Row1", "Row2", "Row3", "Row4", "Row5", "Row6")])
  
  profile1 <- temp[,c("V1", "Row1_A", "Row2_A", "Row3_A", "Row4_A", "Row5_A", "Row6_A", "rate1_1", "forced1",
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile1$panel <- "A" 
  profile1$task <- 1
  names(profile1) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile2 <- temp[,c("V1", "Row1_B", "Row2_B", "Row3_B", "Row4_B", "Row5_B", "Row6_B", "rate1_2", "forced1", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile2$panel <- "B" 
  profile2$task <- 1
  names(profile2) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile3 <- temp[,c("V1", "Row1_2_A", "Row2_2_A", "Row3_2_A", "Row4_2_A", "Row5_2_A", "Row6_2_A", "rate2_1", "forced2", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile3$panel <- "A" 
  profile3$task <- 2
  names(profile3) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice",
                       "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile4 <- temp[,c("V1", "Row1_2_B", "Row2_2_B", "Row3_2_B", "Row4_2_B", "Row5_2_B", "Row6_2_B", "rate2_2", "forced2", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile4$panel <- "B" 
  profile4$task <- 2
  names(profile4) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile5 <- temp[,c("V1", "Row1_3_A", "Row2_3_A", "Row3_3_A", "Row4_3_A", "Row5_3_A", "Row6_3_A", "rate3_1", "forced3", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile5$panel <- "A" 
  profile5$task <- 3
  names(profile5) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile6 <- temp[,c("V1", "Row1_3_B", "Row2_3_B", "Row3_3_B", "Row4_3_B", "Row5_3_B", "Row6_3_B", "rate3_2", "forced3", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile6$panel <- "B" 
  profile6$task <- 3
  names(profile6) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", 
                       "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile7 <- temp[,c("V1", "Row1_4_A", "Row2_4_A", "Row3_4_A", "Row4_4_A", "Row5_4_A", "Row6_4_A", "rate4_1", "forced4", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile7$panel <- "A" 
  profile7$task <- 4
  names(profile7) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  profile8 <- temp[,c("V1", "Row1_4_B", "Row2_4_B", "Row3_4_B", "Row4_4_B", "Row5_4_B", "Row6_4_B", "rate4_2", "forced4", 
                      "gender", "age", "city.uf_1", "religion", "MW", 
                      "schooling", "vote2014")]
  profile8$panel <- "B" 
  profile8$task <- 4
  names(profile8) <- c("respID", col_attr[1], col_attr[2], col_attr[3], col_attr[4], col_attr[5], col_attr[6],
                       "rate", "choice", "gender", "age", "city.uf_1", "religion", "MW", 
                       "schooling", "vote2014", "panel", "task")
  
  resp <- rbind(profile1, profile2, profile3, profile4, profile5, profile6, profile7, profile8)
  
  list_c[[i]] <- resp 
  
}

conjointf <- rbind.fill(list_c)
conjointf$d_choice <- ifelse(substrRight(as.character(conjointf$choice), 1)==conjointf$panel, 1, 0)

#Checking Feature randomization
table(conjointf$partisanship) #check here acento
table(conjointf$city)
table(conjointf$nsp)
table(conjointf$federal)
table(conjointf$nsp_city) #check here spacing?
table(conjointf$nsp_city_fed) #check here spacing?


#Creating fake data (making it more similar to final survey)
conjoint.f <- conjointf[c("respID", "partisanship", "federal", "nsp_city", "rate", "choice", "panel", "task", "d_choice", 
                          "gender", "age", "city.uf_1", "religion", "MW", 
                          "schooling", "vote2014")]

#Fixing mistakes found in pilot
conjoint.f <- conjoint.f[!conjoint.f$federal=="",]

conjoint.f$partisanship[conjoint.f$partisanship=="O prefeito pertence ao PMDB (numero 15)."] <- "O prefeito pertence ao PMDB (número 15)."
conjoint.f$nsp_city[conjoint.f$nsp_city=="Uma ORGANIZAÇÃO NÃO-GOVERNAMENTAL  (ONG) é responsável pelo abrigo para moradores de rua."] <- "Uma ORGANIZAÇÃO NÃO-GOVERNAMENTAL (ONG) é responsável pelo abrigo para moradores de rua."

#Creating variables

#Treats
conjoint.f$partisanshipf <- as.factor(conjoint.f$partisanship)
conjoint.f$federalf <- as.factor(conjoint.f$federal)
conjoint.f$nsp_cityf <- as.factor(conjoint.f$nsp_city)

#pre-treat attributes
conjoint.f$genderf <- ifelse(conjoint.f$gender=="Feminino", 1, 0)

conjoint.f$religionf[conjoint.f$religion=="Católica"] <- 1
conjoint.f$religionf[conjoint.f$religion=="Espírita"] <- 2
conjoint.f$religionf[conjoint.f$religion=="Evangélica"] <- 3
conjoint.f$religionf[conjoint.f$religion=="Não tenho religião"] <- 4
conjoint.f$religionf[conjoint.f$religion=="Outra"] <- 5
conjoint.f$religionf[conjoint.f$religion=="Protestant"] <- 6


conjoint.f$schoolingf[conjoint.f$schooling=="Ensino médio completo e superior incompleto (colegial completo e superior incompleto)"] <- 1
conjoint.f$schoolingf[conjoint.f$schooling=="Ensino superior completo"] <- 2
conjoint.f$schoolingf[conjoint.f$schooling=="Pós-graduação, mestrado ou doutorado "] <- 3


conjoint.f$vote2014f[conjoint.f$vote2014=="Aécio Neves (PSDB, 45)"] <- 1
conjoint.f$vote2014f[conjoint.f$vote2014=="Branco/Nulo"] <- 2
conjoint.f$vote2014f[conjoint.f$vote2014=="Dilma Rousseff (PT, 13)"] <- 3

conjoint.f$MWf[conjoint.f$MW=="0 a 2 salários mínimos (0 a 1.244,00 reais)"] <- 1
conjoint.f$MWf[conjoint.f$MW=="2 a 5 salários mínimos (1.245 a 3.110,00 reais)"] <- 2
conjoint.f$MWf[conjoint.f$MW=="5 a 8 salários mínimos (3.111,00 a 4.976,00 reais)"] <- 3
conjoint.f$MWf[conjoint.f$MW=="8 a 15 salários mínimos (4.976,00 a 9.330,00 reais)"] <- 4
conjoint.f$MWf[conjoint.f$MW=="Mais de 15 salários mínimos (mais de 9.331,00 reais)"] <- 5


save(conjoint.f, file="~/Dropbox/PreAnalysisPlans/conjoint.f.Rda") # this data is available

#Checking randomization (examples)
#Simple checks
table(conjoint.f$partisanship)
table(conjoint.f$federal)
table(conjoint.f$nsp_city)

b.gender <- amce(genderf ~ partisanshipf + federalf + nsp_cityf,
                data=conjoint.f,
                cluster=TRUE, respondent.id="respID")

b.vote <- amce(vote2014f ~ partisanshipf + federalf + nsp_cityf,
                 data=conjoint.f,
                 cluster=TRUE, respondent.id="respID")

b.MW <- amce(MWf ~ partisanshipf + federalf + nsp_cityf,
               data=conjoint.f,
               cluster=TRUE, respondent.id="respID")

#Main Analysis
results <- amce(d_choice ~ partisanshipf + federalf + nsp_cityf,
                data=conjoint.f,
                cluster=TRUE, respondent.id="respID")

#Small sample size yields a warning for block bootstrap for variance estimation
#Analysis will be done with block bootstrap and through clustering in amce's function.
#That warning comes up in both balance and main analyses

#######################Vignette survey

###################### Checking randomization in Vignette

#Blame recoding
#Treat
vignette$treat_b[vignette$blame_nsp==1] <- "treatb_nsp"
vignette$treat_b[vignette$blame_pref==1] <- "treatb_pref"

table(vignette$treat_b)

#Outcome
vignette$outcome_blame[vignette$treat_b=="treatb_nsp"] <- vignette$resp_bnsp[vignette$treat_b=="treatb_nsp"]
vignette$outcome_blame[vignette$treat_b=="treatb_pref"] <- vignette$resp_bpref[vignette$treat_b=="treatb_pref"]

#Credit recoding
vignette$treat_c[vignette$cred_nsp==1] <- "treatc_nsp"
vignette$treat_c[vignette$cred_nsp2==1] <- "treatc_nsp2"
vignette$treat_c[vignette$cred_pref==1] <- "treatc_pref"
vignette$treat_c[vignette$cred_pref2==1] <- "treatc_pref2"

table(vignette$treat_c)

vignette.f <- vignette

#Recoding
vignette$genderf <- ifelse(vignette$gender=="Feminino", 1, 0)

vignette$religionf[vignette$religion=="Católica"] <- 1
vignette$religionf[vignette$religion=="Espírita"] <- 2
vignette$religionf[vignette$religion=="Evangélica"] <- 3
vignette$religionf[vignette$religion=="Não tenho religião"] <- 4
vignette$religionf[vignette$religion=="Outra"] <- 5
vignette$religionf[vignette$religion=="Protestant"] <- 6


vignette$schoolingf[vignette$schooling=="Ensino médio completo e superior incompleto (colegial completo e superior incompleto)"] <- 1
vignette$schoolingf[vignette$schooling=="Ensino superior completo"] <- 2
vignette$schoolingf[vignette$schooling=="Pós-graduação, mestrado ou doutorado "] <- 3


vignette$vote2014f[vignette$vote2014=="Aécio Neves (PSDB, 45)"] <- 1
vignette$vote2014f[vignette$vote2014=="Branco/Nulo"] <- 2
vignette$vote2014f[vignette$vote2014=="Dilma Rousseff (PT, 13)"] <- 3

vignette$MWf[vignette$MW=="0 a 2 salários mínimos (0 a 1.244,00 reais)"] <- 1
vignette$MWf[vignette$MW=="2 a 5 salários mínimos (1.245 a 3.110,00 reais)"] <- 2
vignette$MWf[vignette$MW=="5 a 8 salários mínimos (3.111,00 a 4.976,00 reais)"] <- 3
vignette$MWf[vignette$MW=="8 a 15 salários mínimos (4.976,00 a 9.330,00 reais)"] <- 4
vignette$MWf[vignette$MW=="Mais de 15 salários mínimos (mais de 9.331,00 reais)"] <- 5


save(vignette.f, file="~/Dropbox/PreAnalysisPlans/vignette.f.Rda")

#Balance tests (TO DO)
#Credit
prop.table(table(vignette$genderf, vignette$treat_c))

#Blame
t.test(vignette$genderf ~ vignette$treat_b)
t.test(vignette$vote2014f ~ vignette$treat_b)
t.test(vignette$MWf ~ vignette$treat_b)
t.test(vignette$schoolingf ~ vignette$treat_b)

#Analysis (TO DO)

#Discrete KS
ksstat <- dgof::ks.test(d1, ecdf(d2), simulate.p.value=sim, B=10000)

#Chisqure
m <- matrix(c(rep(0, length(d1)), rep(1, length(d2)), d1, d2), length(d1) + length(d2), 2)
mtab <- table(m[,1], m[,2])

#Difference of proportions
pvalf <- fisher.test(mt, alternative="two.sided")$p.value

chisqstat <- chisq.test(mtab, correct = TRUE)


