## HBA Procedure on Braver Data.
## Batch - running baseline, proactive, reactive iteratively. 


# Install packages if not owned; then load packages
packages <- c("foreach", "tidyr", "broom", "ggplot2", "knitr", "irr", "rstan", "hBayesDM", "psych", "dplyr",
              "tidyverse", "reshape2", "plyr", "beepr", "MVN", "brms", "ggmcmc", "RColorBrewer", "mcmcplots", "telegram")
install.packages(setdiff(packages, rownames(installed.packages())))
for (i in packages){
  library(i, character.only = TRUE)
}

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Load stroop model stan file from dropbox
diffScore <- stan_model(file = "models/logNormal_sigmaDiff.stan", verbose = TRUE)
modelName <- "stroop_delta"

rm(list=setdiff(ls(), c("diffScore", "modelName")))

## CHANGE THIS #########################################################
## param_trialType ->  PC50 || biased 
param_itemType <- c("biased") # PC50 is control trials, biased are important trials
param_session <- c("baseline","proactive", "reactive") # choose which sessions to run
## param_cutOff -> seconds in seconds (e.g., 2 for 2 seconds//2000ms)
param_cutOff <- 4 # minimum number of trials, data quality control
options(max.print=100000)
## //CHANGE THIS #######################################################

for (s in param_session) {
  for (t in param_itemType) {
  
    rm(list=setdiff(ls(), c("diffScore", "s", "t", "param_cutOff", "param_itemType", "param_session", "modelName")))
    
dat <- read.csv("data/destroop-raw.csv")


#############
# Pre-processing & Preparing trial-level data for HBA
name_n <- paste0(modelName, "_", s)

tt <- table(dat$ID)
dat <- dat %>%
  subset(session == s) %>%
  subset(ID %in% names(tt[tt == 2112])) %>%
  subset(RT <= param_cutOff*1000)

del <- ifelse(t == "PC50", dat <- subset(dat, itemType == 2), dat <- subset(dat, itemType != 2))
rm(del)

# Number of subjects
length(unique(dat$ID))
# Can't contain NA
length(dat$RT[is.na(dat$RT)])

# Cleaning Functions
source("https://raw.githubusercontent.com/jpsnijder/whistleR/master/whistle.R")

RT.win(data = dat,
       subjVar = "ID",
       vars = c("phase", "session", "trialType"), 
       value = "RT", 
       new_df = "dat",
       minRT = 250)

# Bivariate pre-processing
datRT <-  dcast(dat, ID + phase + session ~ trialType, value.var = "RTwin", mean)
datRT <-  mutate(datRT, SEff = datRT$`2`-datRT$`1`)
datRT <- dcast(datRT, ID ~ phase, value.var = "SEff", mean)  

#mvn(datRT[-1], multivariatePlot = "qq", showOutliers = T, showNewData = T)

Sx <- cov(datRT[-1])
mux <- colMeans(datRT[-1]) 
datRT <- mutate(datRT, MD = mahalanobis(datRT[-1], mux, Sx))

# Based on Chi-square distribution with alpha .001 and df = 1, cutoff is 10.828
dat <- subset(dat, ID %in% datRT$ID[datRT$MD <= 10.828])

dat <- subset(dat, ACC == 1)

# Data Check Station (DCS): number of trials to be at least 10 per subject/time/condition
dat <- ddply(dat, c("ID", "phase", "trialType"), function(x){
    mutate(x, tCount = length(x$RTwin))
})

# CHANGE THIS NUMBER
dat <- subset(dat, tCount >= 10)

dat <- ddply(dat, c("ID"), function(x){
  tt <- table(x$phase, x$trialType)
  mutate(x, condCount = length(tt[tt != 0]))
})

dat <- subset(dat, condCount == 4)

dat <- dat[with(dat, order(ID, phase, trialType)),] %>%
  mutate(RT = RTwin / 1000) %>%  
  mutate(time = ifelse(phase == "test", 1, 2)) 

listID <- unique(factor(dat$ID))

dat <- ddply(dat, c("ID"), function(x){
  mutate(x, subj_num = which(x$ID[1] == listID))
})

HBAdat <- dat %>% select(subj_num, time, trialType, RT, ID)

#############
# Frequentist Analyses (from Haines-lab)
#############
sum_stroop <- dcast(HBAdat, subj_num + time  ~ trialType, value.var = "RT", mean)
sum_stroop <- mutate(sum_stroop, stroop_eff = sum_stroop$`2` - sum_stroop$'1') %>%
  select(subj_num, time, stroop_eff)
sd_stroop <- dcast(HBAdat, subj_num + time  ~ trialType, value.var = "RT", sd)
sd_stroop <- rename(sd_stroop, c("1" = "stroop_sd_congruent",
                                 "2" = "stroop_sd_incongruent")) %>%
  pivot_wider(names_from = time, values_from = c(stroop_sd_congruent, stroop_sd_incongruent))

# Format for test-retest analysis
stroop_unpooled <- sum_stroop %>%
  ungroup() %>%
  mutate(time = ifelse(time==1, "stroop_T1", "stroop_T2"),
         pooled = "Frequentist", 
         Replication = "Sample Mean") %>% # these "pooled" and Replication variables will come in later
  spread(key = time, value = stroop_eff)


# add original ID and save MPE as 
resetID <- unique(select(HBAdat, c("ID", "subj_num")))

stroop_unpooled <- merge(stroop_unpooled, resetID, by = "subj_num")[-1]
sd_stroop <- merge(sd_stroop, resetID, by = "subj_num")[-1]

stroop_unpooled <- stroop_unpooled %>%
rename_with(~str_replace(., "stroop_", paste0("stroop_", s, "_")))

mpe <- stroop_unpooled %>%
  select(-c("pooled", "Replication")) %>%
  merge(sd_stroop, by = "ID")

write.csv(mpe, paste0("output/", name_n, "MPE", ".csv"), row.names = F)


#################
# Enter Bayesian - building a generative model
#################

# Number of subjects
n_subj <- length(unique(HBAdat$subj_num))
# Number of conditions
n_trialType <- 2
# Number of timepoints
n_time <- 2


# Create indivT array = number of trials per subject, per time, per condition
indivT <- array(NA, dim = c(n_subj, n_trialType, n_time))
for (i in 1:n_subj) {
  # Number of trials for congruent condition at time 1
  indivT[i, 1, 1] = length(with(HBAdat, RT[subj_num==i & trialType==1 & time==1]))
  # Number of trials for incongruent condition at time 1
  indivT[i, 2, 1] = length(with(HBAdat, RT[subj_num==i & trialType==2 & time==1]))
  # Number of trials for congruent condition at time 2
  indivT[i, 1, 2] = length(with(HBAdat, RT[subj_num==i & trialType==1 & time==2]))
  # Number of trials for incongruent condition at time 2
  indivT[i, 2, 2] = length(with(HBAdat, RT[subj_num==i & trialType==2 & time==2]))
}

# Highest number of trials within each condition within each timepoint
T_max <- max(indivT)

# Create RT data array for stan; dims = (subject, condition, time, trial)
RT <- array(0, dim = c(n_subj, n_trialType, n_time, T_max))
for (i in 1:n_subj) {
  # RTs for congruent condition at time 1
  RT[i, 1, 1, 1:indivT[i,1,1]] = with(HBAdat, RT[subj_num==i & trialType==1 & time==1])
  # RTs for incongruent condition at time 1
  RT[i, 2, 1, 1:indivT[i,2,1]] = with(HBAdat, RT[subj_num==i & trialType==2 & time==1])
  # RTs for congruent condition at time 2
  RT[i, 1, 2, 1:indivT[i,1,2]] = with(HBAdat, RT[subj_num==i & trialType==1 & time==2])
  # RTs for incongruent condition at time 2
  RT[i, 2, 2, 1:indivT[i,2,2]] = with(HBAdat, RT[subj_num==i & trialType==2 & time==2])
}

# Stan-ready data list
stan_dat <- list(N      = n_subj,
                 N_cond = n_trialType,
                 N_time = n_time,
                 T_max  = T_max,
                 indivT = indivT,
                 RT     = RT)

# Fit the hierarchical model
fit <- sampling(diffScore,
                      data    = stan_dat,
                      iter    = 3000,
                      warmup  = 1000,
                      chains  = 3,
                      cores   = 3)

#################
# HBA Parameter extraction
#################

# Save and Extract parameters from model
fit@stanmodel@dso <- new("cxxdso")
saveRDS(fit, file = paste0("fits/", name_n,"_fit.rds"))
# fit <- readRDS("D:/Seafile/fits/Stroop_logNormal_proactive_fit.rds")

pars <- rstan::extract(fit)

options(scipen=999)

# Extracting posterior modes of individual-level stroop effect estimates
mu <- as.data.frame(apply(pars$mu, c(2,3), mean))
mu <- rename(mu, c("V1" = "mu_con_1",
                   "V2" = "mu_int_1",
                   "V3" = "mu_con_2",
                   "V4" = "mu_int_2"))

delta <- as.data.frame(apply(pars$delta, c(2,3), mean))
delta <- rename(delta, c("V1" = "delta_1",
                         "V2" = "delta_2"))

sigma <- as.data.frame(apply(pars$sigma_i, c(2,3), mean))
sigma <- rename(sigma, c("V1" = "sigma_con_1",
                         "V2" = "sigma_int_1",
                         "V3" = "sigma_con_2",
                         "V4" = "sigma_int_2"))

stroop <- bind_cols(mu, delta, sigma)
colnames(stroop) <- paste("stroop", s, colnames(stroop), sep = "_")


# Reset the ID 1:n_subj to original subject identification
resetID <- unique(select(HBAdat, c("ID", "subj_num")))
stroop <- mutate(stroop, subj_num = 1:n_subj)
stroop <- merge(stroop, resetID, by = "subj_num")[-1]

write.csv(stroop, paste0("output/", name_n, ".csv"), row.names = F)
capture.output(print(fit), file = paste0(name_n, "_", "runStats", ".txt"))


  }

}

