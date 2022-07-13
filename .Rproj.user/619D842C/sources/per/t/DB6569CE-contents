# Load packages -----------------------------------------------------------
library(ggplot2)
library(scales)
library(lme4)
library(lmerTest)
library(tidyverse)
library(readxl)
library(cowplot)
library(ez)





### NOTE: "Abweichung" or "Abw" means deviation in German and indicates 
###       the absolute deviation of received from expected feedback, i.e., the unsigned PE.






# Settings ----------------------------------------------------------------
nSims <- 200

# Get data ----------------------------------------------------------------
pupildata <- readxl::read_xlsx('Data_fmri_LOOP.xlsx')

# Remove data for feedback for other person
pupildata <- pupildata[pupildata$SelfvsOther == 1,]


# Get basic information such as subject IDs and LR biases -----------------
VPCodes <- unique(pupildata$VPCode)
nDataPoints <- length(pupildata$PE)
LRBiasSelf <- unique(pupildata$LRBiasSelf)
nSubs <- length(LRBiasSelf)


# Replicate models run in jamovi to assure equivalence --------------------
model0 <- lmer(pupildata$SlopesTrialPupil ~ 1 + (1|pupildata$VPCode), REML = TRUE)
model0 %>% summary()

modelPErfx <- lmer(pupildata$SlopesTrialPupil ~ 1 + pupildata$PE + pupildata$Abweichung + (1 + pupildata$PE|pupildata$VPCode), REML = TRUE)
modelPErfx %>% summary()

modelPEAbwrfx <- lmer(pupildata$SlopesTrialPupil ~ 1 + pupildata$PE + pupildata$Abweichung + (1 + pupildata$PE + pupildata$Abweichung|pupildata$VPCode), REML = TRUE)
modelPEAbwrfx %>% summary()

modelbiasAsInPaper  <- lmer(pupildata$SlopesTrialPupil ~ 1 + pupildata$PE*pupildata$LRBiasSelf + pupildata$Abweichung + (1 + pupildata$PE|pupildata$VPCode), REML = TRUE)
modelbiasAsInPaper %>% summary()


# Run model with additional RFX for Abweichung (i.e., unsigned PE) --------
modelbias <- lmer(pupildata$SlopesTrialPupil ~ 1 + pupildata$PE*pupildata$LRBiasSelf + pupildata$Abweichung + (1 + pupildata$PE + pupildata$Abweichung|pupildata$VPCode), REML = TRUE)
modelbias %>% summary()


# Get CIs for FFX (these will later be used to generate new FFX) ----------
CIs <- confint.merMod(modelbias)


# Get empirical (i.e., study data) FFX ------------------------------------
FFX <- fixef(modelbias)


# Get residual characteristics --------------------------------------------
residsd <- sd(resid(modelbias))


########################################### CORE SIMULATION/RECOVERY ROUTINE ##########################################################
########## COMMENTED OUT SO THAT PREVIOUSLY RUN SIMULATIONS AS REPORTED IN RESPONSE TO REVIEW ARE NOT ALTERED #########################
# 
# # Init variables ----------------------------------------------------------
# simdata_0 <- matrix(nrow = nSims, ncol = nDataPoints)
# simdata_25 <- matrix(nrow = nSims, ncol = nDataPoints)
# simdata_50 <- matrix(nrow = nSims, ncol = nDataPoints)
# simdata_75 <- matrix(nrow = nSims, ncol = nDataPoints)
# simdata_100 <- matrix(nrow = nSims, ncol = nDataPoints)
# 
# estFFX_0 <- matrix(nrow = nSims, ncol = length(fixef(modelbias)))
# estFFX_25 <- matrix(nrow = nSims, ncol = length(fixef(modelbias)))
# estFFX_50 <- matrix(nrow = nSims, ncol = length(fixef(modelbias)))
# estFFX_75 <- matrix(nrow = nSims, ncol = length(fixef(modelbias)))
# estFFX_100 <- matrix(nrow = nSims, ncol = length(fixef(modelbias)))
# 
# new_LRbiasSelf <- matrix(nrow = nSims, ncol = nSubs)
# 
# new_intercepts <- matrix(nrow = nSims, ncol = nSubs)
# new_PEweights <- matrix(nrow = nSims, ncol = nSubs)
# new_Abwweights <- matrix(nrow = nSims, ncol = nSubs)
# new_weightLRbias <- matrix(nrow = nSims, ncol = nSubs)
# new_CLI <- matrix(nrow = nSims, ncol = nSubs)
# 
# simFFX <- matrix(nrow = nSims, ncol = 5)
# 
# # Loop through simulations ------------------------------------------------
# for(simRunner in 1:nSims){
#   
#   # Generate datasets with different levels of odd tiling -------------------
#   # Fully odd tiling (i.e., size of signed and unsigned PEs always matches)
#   data0 <- pupildata
#   
#   # Mostly odd tiling (for 25% percent of data points, Abweichung (unsigned PEs) are randomized to reduce the correspondence of their sizes to those of signed PEs)
#   data25 <- pupildata
#   whichDataPoints <- rbinom(nDataPoints, 1, .25)
#   permuteDataPoints <- data25$Abweichung[whichDataPoints == 1]
#   data25$Abweichung[whichDataPoints == 1] <- permuteDataPoints[sample(1:length(permuteDataPoints), length(permuteDataPoints))]
#   
#   # Balanced tiling (for 50% percent of data points, Abweichung (unsigned PEs) are randomized)
#   data50 <- pupildata
#   whichDataPoints <- rbinom(nDataPoints, 1, .5)
#   permuteDataPoints <- data50$Abweichung[whichDataPoints == 1]
#   data50$Abweichung[whichDataPoints == 1] <- permuteDataPoints[sample(1:length(permuteDataPoints), length(permuteDataPoints))]
#   
#   # Mostly non-odd tiling (for 75% percent of data points, Abweichung (unsigned PEs) are randomized
#   data75 <- pupildata
#   whichDataPoints <- rbinom(nDataPoints, 1, .75)
#   permuteDataPoints <- data75$Abweichung[whichDataPoints == 1]
#   data75$Abweichung[whichDataPoints == 1] <- permuteDataPoints[sample(1:length(permuteDataPoints), length(permuteDataPoints))]
#   
#   # Fully non-odd tiling (for 100% percent of data points, Abweichung (unsigned PEs) are randomized
#   data100 <- pupildata
#   whichDataPoints <- rbinom(nDataPoints, 1, 1)
#   permuteDataPoints <- data100$Abweichung[whichDataPoints == 1]
#   data100$Abweichung[whichDataPoints == 1] <- permuteDataPoints[sample(1:length(permuteDataPoints), length(permuteDataPoints))]
#   
#   
#   # Generate new regression weights based on new LRbiases and the empirical associations between LRbias and intercepts / PEweights ---------
#   new_intercepts[simRunner,]    <- FFX[1] + rnorm(nSubs, mean = 0, sd = sd(ranef(modelbias)$`pupildata$VPCode`$'(Intercept)'))
#   new_PEweights[simRunner,]     <- FFX[2] + rnorm(nSubs, mean = 0, sd = sd(ranef(modelbias)$`pupildata$VPCode`$'pupildata$PE'))
#   new_weightLRbias[simRunner,]  <- FFX[3] + rnorm(nSubs, mean = 0, sd = diff(CIs[10,])/2)
#   new_Abwweights[simRunner,]    <- FFX[4] + rnorm(nSubs, mean = 0, sd = sd(ranef(modelbias)$`pupildata$VPCode`$'pupildata$Abweichung'))
#   new_CLI[simRunner,]           <- FFX[5] + rnorm(nSubs, mean = 0, sd = diff(CIs[12,])/2)
#   
#   
#   # Get means of simulation parameters for later plotting ------------------
#   simFFX[simRunner,] <- c(mean(new_intercepts[simRunner,]),
#                           mean(new_PEweights[simRunner,]),
#                           mean(new_weightLRbias[simRunner,]),
#                           mean(new_Abwweights[simRunner,]),
#                           mean(new_CLI[simRunner,]))
#   
#   
#   # Loop through subjects to simulate new data -----------------------------
#   for (subRunner in 1:nSubs){
#     PE <- data0$PE[data0$VPCode == VPCodes[subRunner]]
#     Abweichung <- data0$Abweichung[data0$VPCode == VPCodes[subRunner]]
#     simdata_0[simRunner,data0$VPCode == VPCodes[subRunner]] <- 
#       new_intercepts[simRunner, subRunner] + 
#       new_weightLRbias[simRunner,subRunner] * LRBiasSelf[subRunner] + 
#       new_PEweights[simRunner, subRunner] * PE + 
#       new_Abwweights[simRunner, subRunner] * Abweichung + 
#       new_CLI[simRunner, subRunner] * PE * LRBiasSelf[subRunner] + 
#       rnorm(length(PE), mean = 0, sd = residsd)
#     
#     PE <- data25$PE[data0$VPCode == VPCodes[subRunner]]
#     Abweichung <- data25$Abweichung[data0$VPCode == VPCodes[subRunner]]
#     simdata_25[simRunner,data0$VPCode == VPCodes[subRunner]] <- 
#       new_intercepts[simRunner, subRunner] + 
#       new_weightLRbias[simRunner,subRunner] * LRBiasSelf[subRunner] + 
#       new_PEweights[simRunner, subRunner] * PE + 
#       new_Abwweights[simRunner, subRunner] * Abweichung + 
#       new_CLI[simRunner, subRunner] * PE * LRBiasSelf[subRunner] + 
#       rnorm(length(PE), mean = 0, sd = residsd)
#     
#     PE <- data50$PE[data0$VPCode == VPCodes[subRunner]]
#     Abweichung <- data50$Abweichung[data0$VPCode == VPCodes[subRunner]]
#     simdata_50[simRunner,data0$VPCode == VPCodes[subRunner]] <- 
#       new_intercepts[simRunner, subRunner] + 
#       new_weightLRbias[simRunner,subRunner] * LRBiasSelf[subRunner] + 
#       new_PEweights[simRunner, subRunner] * PE + 
#       new_Abwweights[simRunner, subRunner] * Abweichung + 
#       new_CLI[simRunner, subRunner] * PE * LRBiasSelf[subRunner] + 
#       rnorm(length(PE), mean = 0, sd = residsd)
#     
#     PE <- data75$PE[data0$VPCode == VPCodes[subRunner]]
#     Abweichung <- data75$Abweichung[data0$VPCode == VPCodes[subRunner]]
#     simdata_75[simRunner,data0$VPCode == VPCodes[subRunner]] <- 
#       new_intercepts[simRunner, subRunner] + 
#       new_weightLRbias[simRunner,subRunner] * LRBiasSelf[subRunner] + 
#       new_PEweights[simRunner, subRunner] * PE + 
#       new_Abwweights[simRunner, subRunner] * Abweichung + 
#       new_CLI[simRunner, subRunner] * PE * LRBiasSelf[subRunner] + 
#       rnorm(length(PE), mean = 0, sd = residsd)
#     
#     PE <- data100$PE[data0$VPCode == VPCodes[subRunner]]
#     Abweichung <- data100$Abweichung[data0$VPCode == VPCodes[subRunner]]
#     simdata_100[simRunner,data0$VPCode == VPCodes[subRunner]] <- 
#       new_intercepts[simRunner, subRunner] + 
#       new_weightLRbias[simRunner,subRunner] * LRBiasSelf[subRunner] + 
#       new_PEweights[simRunner, subRunner] * PE + 
#       new_Abwweights[simRunner, subRunner] * Abweichung + 
#       new_CLI[simRunner, subRunner] * PE * LRBiasSelf[subRunner] + 
#       rnorm(length(PE), mean = 0, sd = residsd)
#     
#   }
#   
#   
#   # Estimate mixed models and save estimated FFX to matrices
#   estFFX_0[simRunner,] <- fixef(lmer(simdata_0[simRunner,] ~ 1 + data0$PE*data0$LRBiasSelf + data0$Abweichung + (1 + data0$PE + data0$Abweichung|data0$VPCode), REML = TRUE))
#   estFFX_25[simRunner,] <- fixef(lmer(simdata_25[simRunner,] ~ 1 + data25$PE*data25$LRBiasSelf + data25$Abweichung + (1 + data25$PE + data25$Abweichung|data25$VPCode), REML = TRUE))
#   estFFX_50[simRunner,] <- fixef(lmer(simdata_50[simRunner,] ~ 1 + data50$PE*data50$LRBiasSelf + data50$Abweichung + (1 + data50$PE + data50$Abweichung|data50$VPCode), REML = TRUE))
#   estFFX_75[simRunner,] <- fixef(lmer(simdata_75[simRunner,] ~ 1 + data75$PE*data75$LRBiasSelf + data75$Abweichung + (1 + data75$PE + data75$Abweichung|data75$VPCode), REML = TRUE))
#   estFFX_100[simRunner,] <- fixef(lmer(simdata_100[simRunner,] ~ 1 + data100$PE*data100$LRBiasSelf + data100$Abweichung + (1 + data100$PE + data100$Abweichung|data100$VPCode), REML = TRUE))
# }
############################################################################################################
############################################################################################################


################ Distributions of FFX recovered from simulated data ##################
# First, create data frames
estFFX_intercepts <- data.frame(FFX0 = estFFX_0[,1], FFX25 = estFFX_25[,1], FFX50 = estFFX_50[,1], FFX75 = estFFX_75[,1], FFX100 = estFFX_100[,1], simRun = c(1:nrow(estFFX_100)))
estFFX_PE         <- data.frame(FFX0 = estFFX_0[,2], FFX25 = estFFX_25[,2], FFX50 = estFFX_50[,2], FFX75 = estFFX_75[,2], FFX100 = estFFX_100[,2], simRun = c(1:nrow(estFFX_100)))
estFFX_LRbias         <- data.frame(FFX0 = estFFX_0[,3], FFX25 = estFFX_25[,3], FFX50 = estFFX_50[,3], FFX75 = estFFX_75[,3], FFX100 = estFFX_100[,3], simRun = c(1:nrow(estFFX_100)))
estFFX_Abw        <- data.frame(FFX0 = estFFX_0[,4], FFX25 = estFFX_25[,4], FFX50 = estFFX_50[,4], FFX75 = estFFX_75[,4], FFX100 = estFFX_100[,4], simRun = c(1:nrow(estFFX_100)))
estFFX_LRbiasByPE     <- data.frame(FFX0 = estFFX_0[,5], FFX25 = estFFX_25[,5], FFX50 = estFFX_50[,5], FFX75 = estFFX_75[,5], FFX100 = estFFX_100[,5], simRun = c(1:nrow(estFFX_100)))

# Pivot into long format
estFFX_intercepts <- pivot_longer(estFFX_intercepts, cols = starts_with("FFX"))
estFFX_PE <- pivot_longer(estFFX_PE, cols = starts_with("FFX"))
estFFX_LRbias <- pivot_longer(estFFX_LRbias, cols = starts_with("FFX"))
estFFX_Abw <- pivot_longer(estFFX_Abw, cols = starts_with("FFX"))
estFFX_LRbiasByPE <- pivot_longer(estFFX_LRbiasByPE, cols = starts_with("FFX"))

# Add a factor named tiling for correct ordering of boxplots below, and for ANOVAs
estFFX_intercepts$tiling <- factor(estFFX_intercepts$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
estFFX_PE$tiling <- factor(estFFX_PE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
estFFX_LRbias$tiling <- factor(estFFX_LRbias$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
estFFX_Abw$tiling <- factor(estFFX_Abw$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
estFFX_LRbiasByPE$tiling <- factor(estFFX_LRbiasByPE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))

# Turn simRun into factor
estFFX_intercepts$simRun <- factor(estFFX_intercepts$simRun)
estFFX_PE$simRun <- factor(estFFX_intercepts$simRun)
estFFX_LRbias$simRun <- factor(estFFX_intercepts$simRun)
estFFX_Abw$simRun <- factor(estFFX_intercepts$simRun)
estFFX_LRbiasByPE$simRun <- factor(estFFX_intercepts$simRun)

# Run Anovas to test whether FFX estimates form sim data is affected by tiling
ezANOVA(estFFX_intercepts, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_PE, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_LRbias, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_Abw, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_LRbiasByPE, dv = value, within = tiling, wid = simRun)

# Compute difference between recovered FFX and empirical FFX
estFFX_intercepts_minusEmpirical <- estFFX_intercepts
estFFX_intercepts_minusEmpirical$value <- estFFX_intercepts_minusEmpirical$value - fixef(modelbias)[1]

estFFX_PE_minusEmpirical <- estFFX_PE
estFFX_PE_minusEmpirical$value <- estFFX_PE_minusEmpirical$value - fixef(modelbias)[2]

estFFX_LRbias_minusEmpirical <- estFFX_LRbias
estFFX_LRbias_minusEmpirical$value <- estFFX_LRbias_minusEmpirical$value - fixef(modelbias)[3]

estFFX_Abw_minusEmpirical <- estFFX_Abw
estFFX_Abw_minusEmpirical$value <- estFFX_Abw_minusEmpirical$value - fixef(modelbias)[4]

estFFX_LRbiasByPE_minusEmpirical <- estFFX_LRbiasByPE
estFFX_LRbiasByPE_minusEmpirical$value <- estFFX_LRbiasByPE_minusEmpirical$value - fixef(modelbias)[5]


# Check whether recovered FFX differ from empirical FFX
t.test(estFFX_intercepts_minusEmpirical$value[estFFX_intercepts_minusEmpirical$tiling == "FFX0"])
t.test(estFFX_intercepts_minusEmpirical$value[estFFX_intercepts_minusEmpirical$tiling == "FFX25"])
t.test(estFFX_intercepts_minusEmpirical$value[estFFX_intercepts_minusEmpirical$tiling == "FFX50"])
t.test(estFFX_intercepts_minusEmpirical$value[estFFX_intercepts_minusEmpirical$tiling == "FFX75"])
t.test(estFFX_intercepts_minusEmpirical$value[estFFX_intercepts_minusEmpirical$tiling == "FFX100"])

t.test(estFFX_PE_minusEmpirical$value[estFFX_PE_minusEmpirical$tiling == "FFX0"])
t.test(estFFX_PE_minusEmpirical$value[estFFX_PE_minusEmpirical$tiling == "FFX25"])
t.test(estFFX_PE_minusEmpirical$value[estFFX_PE_minusEmpirical$tiling == "FFX50"])
t.test(estFFX_PE_minusEmpirical$value[estFFX_PE_minusEmpirical$tiling == "FFX75"])
t.test(estFFX_PE_minusEmpirical$value[estFFX_PE_minusEmpirical$tiling == "FFX100"])

t.test(estFFX_LRbias_minusEmpirical$value[estFFX_LRbias_minusEmpirical$tiling == "FFX0"])
t.test(estFFX_LRbias_minusEmpirical$value[estFFX_LRbias_minusEmpirical$tiling == "FFX25"])
t.test(estFFX_LRbias_minusEmpirical$value[estFFX_LRbias_minusEmpirical$tiling == "FFX50"])
t.test(estFFX_LRbias_minusEmpirical$value[estFFX_LRbias_minusEmpirical$tiling == "FFX75"])
t.test(estFFX_LRbias_minusEmpirical$value[estFFX_LRbias_minusEmpirical$tiling == "FFX100"])

t.test(estFFX_Abw_minusEmpirical$value[estFFX_Abw_minusEmpirical$tiling == "FFX0"])
t.test(estFFX_Abw_minusEmpirical$value[estFFX_Abw_minusEmpirical$tiling == "FFX25"])
t.test(estFFX_Abw_minusEmpirical$value[estFFX_Abw_minusEmpirical$tiling == "FFX50"])
t.test(estFFX_Abw_minusEmpirical$value[estFFX_Abw_minusEmpirical$tiling == "FFX75"])
t.test(estFFX_Abw_minusEmpirical$value[estFFX_Abw_minusEmpirical$tiling == "FFX100"])

t.test(estFFX_LRbiasByPE_minusEmpirical$value[estFFX_LRbiasByPE_minusEmpirical$tiling == "FFX0"])
t.test(estFFX_LRbiasByPE_minusEmpirical$value[estFFX_LRbiasByPE_minusEmpirical$tiling == "FFX25"])
t.test(estFFX_LRbiasByPE_minusEmpirical$value[estFFX_LRbiasByPE_minusEmpirical$tiling == "FFX50"])
t.test(estFFX_LRbiasByPE_minusEmpirical$value[estFFX_LRbiasByPE_minusEmpirical$tiling == "FFX75"])
t.test(estFFX_LRbiasByPE_minusEmpirical$value[estFFX_LRbiasByPE_minusEmpirical$tiling == "FFX100"])


# Run Anovas to test whether deviations between recovered and empirical FFX are driven by tiling
ezANOVA(estFFX_intercepts_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_PE_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_LRbias_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_Abw_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(estFFX_LRbiasByPE_minusEmpirical, dv = value, within = tiling, wid = simRun)



# Compute absolute difference between recovered FFX and empirical FFX
abs_estFFX_intercepts_minusEmpirical <- estFFX_intercepts
abs_estFFX_intercepts_minusEmpirical$value <- abs(abs_estFFX_intercepts_minusEmpirical$value - fixef(modelbias)[1])

abs_estFFX_PE_minusEmpirical <- estFFX_PE
abs_estFFX_PE_minusEmpirical$value <- abs(abs_estFFX_PE_minusEmpirical$value - fixef(modelbias)[2])

abs_estFFX_LRbias_minusEmpirical <- estFFX_LRbias
abs_estFFX_LRbias_minusEmpirical$value <- abs(abs_estFFX_LRbias_minusEmpirical$value - fixef(modelbias)[3])

abs_estFFX_Abw_minusEmpirical <- estFFX_Abw
abs_estFFX_Abw_minusEmpirical$value <- abs(abs_estFFX_Abw_minusEmpirical$value - fixef(modelbias)[4])

abs_estFFX_LRbiasByPE_minusEmpirical <- estFFX_LRbiasByPE
abs_estFFX_LRbiasByPE_minusEmpirical$value <- abs(abs_estFFX_LRbiasByPE_minusEmpirical$value - fixef(modelbias)[5])

# Run Anovas to test whether absolute deviation between recovered and empirical FFX is driven by tiling
ezANOVA(abs_estFFX_intercepts_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(abs_estFFX_PE_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(abs_estFFX_LRbias_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(abs_estFFX_Abw_minusEmpirical, dv = value, within = tiling, wid = simRun)
ezANOVA(abs_estFFX_LRbiasByPE_minusEmpirical, dv = value, within = tiling, wid = simRun)


# Plot empirical parameters (as red dots) in front of violin plots of parameters estimated from simulated data --------
plotdata0 <- as.data.frame(estFFX_0)
colnames(plotdata0) <- parnames
plotdata0 <- pivot_longer(plotdata0, cols = everything())
plot0 <- list()
for (plotRunner in 1:5){
  trueFixedParameter <- data.frame(name = 1, value = fixef(modelbias)[plotRunner])
  meanSimParameters <- data.frame(name = 1, value = mean(plotdata0$value[plotdata0$name == unique(plotdata0$name)[plotRunner]]))
  plot0[[plotRunner]] <- ggplot(data = plotdata0[plotdata0$name == unique(plotdata0$name)[plotRunner],], aes(x = name, y = value)) + geom_violin() + 
    geom_point(data = meanSimParameters, size = 3.5, shape = 21) + # Add mean of 'artificial' FFX as empty circle
    geom_point(data = trueFixedParameter, color = "red") + theme_bw(base_size = 8) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  plot0[[plotRunner]]}
fullplot0 <- plot_grid(plot0[[1]], plot0[[2]], plot0[[3]], plot0[[4]], plot0[[5]], nrow = 1)



plotdata25 <- as.data.frame(estFFX_25)
colnames(plotdata25) <- parnames
plotdata25 <- pivot_longer(plotdata25, cols = everything())
plot25 <- list()
for (plotRunner in 1:5){
  trueFixedParameter <- data.frame(name = 1, value = fixef(modelbias)[plotRunner])
  meanSimParameters <- data.frame(name = 1, value = mean(plotdata25$value[plotdata25$name == unique(plotdata25$name)[plotRunner]]))
  plot25[[plotRunner]] <- ggplot(data = plotdata25[plotdata25$name == unique(plotdata25$name)[plotRunner],], aes(x = name, y = value)) + geom_violin() + 
    geom_point(data = meanSimParameters, size = 3.5, shape = 21) + # Add mean of 'artificial' FFX as empty circle
    geom_point(data = trueFixedParameter, color = "red") + theme_bw(base_size = 8) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  plot25[[plotRunner]]}
fullplot25 <- plot_grid(plot25[[1]], plot25[[2]], plot25[[3]], plot25[[4]], plot25[[5]], nrow = 1)



plotdata50 <- as.data.frame(estFFX_50)
colnames(plotdata50) <- parnames
plotdata50 <- pivot_longer(plotdata50, cols = everything())
plot50 <- list()
for (plotRunner in 1:5){
  trueFixedParameter <- data.frame(name = 1, value = fixef(modelbias)[plotRunner])
  meanSimParameters <- data.frame(name = 1, value = mean(plotdata50$value[plotdata50$name == unique(plotdata50$name)[plotRunner]]))
  plot50[[plotRunner]] <- ggplot(data = plotdata50[plotdata50$name == unique(plotdata50$name)[plotRunner],], aes(x = name, y = value)) + geom_violin() + 
    geom_point(data = meanSimParameters, size = 3.5, shape = 21) + # Add mean of 'artificial' FFX as empty circle
    geom_point(data = trueFixedParameter, color = "red") + theme_bw(base_size = 8) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  plot50[[plotRunner]]}
fullplot50 <- plot_grid(plot50[[1]], plot50[[2]], plot50[[3]], plot50[[4]], plot50[[5]], nrow = 1)



plotdata75 <- as.data.frame(estFFX_75)
colnames(plotdata75) <- parnames
plotdata75 <- pivot_longer(plotdata75, cols = everything())
plot75 <- list()
for (plotRunner in 1:5){
  trueFixedParameter <- data.frame(name = 1, value = fixef(modelbias)[plotRunner])
  meanSimParameters <- data.frame(name = 1, value = mean(plotdata75$value[plotdata75$name == unique(plotdata75$name)[plotRunner]]))
  plot75[[plotRunner]] <- ggplot(data = plotdata75[plotdata75$name == unique(plotdata75$name)[plotRunner],], aes(x = name, y = value)) + geom_violin() + 
    geom_point(data = meanSimParameters, size = 3.5, shape = 21) + # Add mean of 'artificial' FFX as empty circle
    geom_point(data = trueFixedParameter, color = "red") + theme_bw(base_size = 8) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  plot75[[plotRunner]]}
fullplot75 <- plot_grid(plot75[[1]], plot75[[2]], plot75[[3]], plot75[[4]], plot75[[5]], nrow = 1)



plotdata100 <- as.data.frame(estFFX_100)
colnames(plotdata100) <- parnames
plotdata100 <- pivot_longer(plotdata100, cols = everything())
plot100 <- list()
for (plotRunner in 1:5){
  trueFixedParameter <- data.frame(name = 1, value = fixef(modelbias)[plotRunner])
  meanSimParameters <- data.frame(name = 1, value = mean(plotdata100$value[plotdata100$name == unique(plotdata100$name)[plotRunner]]))
  plot100[[plotRunner]] <- ggplot(data = plotdata100[plotdata100$name == unique(plotdata100$name)[plotRunner],], aes(x = name, y = value)) + geom_violin() + 
    geom_point(data = meanSimParameters, size = 3.5, shape = 21) + # Add mean of 'artificial' FFX as empty circle
    geom_point(data = trueFixedParameter, color = "red") + theme_bw(base_size = 8) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  plot100[[plotRunner]]}
fullplot100 <- plot_grid(plot100[[1]], plot100[[2]], plot100[[3]], plot100[[4]], plot100[[5]], nrow = 1)


# Aggregate plots -----------------------------------------------------
allEstPlots <- plot_grid(fullplot0, fullplot25, fullplot50, fullplot75, fullplot100, nrow = 5)
allEstPlots
ggsave(file="allEstPlots.svg", plot=allEstPlots, width=16, height=16, units = "cm")















######################## Association between recovered and underlying FFX ######################
# Get correlations between simulated and recovered FFX --------------------
simeststatistic0 <- array()
simestpvalues0 <- array()
simestestimate0 <- array()

simeststatistic25 <- array()
simestpvalues25 <- array()
simestestimate25 <- array()

simeststatistic50 <- array()
simestpvalues50 <- array()
simestestimate50 <- array()

simeststatistic75 <- array()
simestpvalues75 <- array()
simestestimate75 <- array()

simeststatistic100 <- array()
simestpvalues100 <- array()
simestestimate100 <- array()



for (parRunner in 1:5){
  simeststatistic0[parRunner] <- cor.test(simFFX[,parRunner], estFFX_0[,parRunner], alternative = "greater")$statistic
  simestpvalues0[parRunner] <- cor.test(simFFX[,parRunner], estFFX_0[,parRunner], alternative = "greater")$p.value
  simestestimate0[parRunner] <- cor.test(simFFX[,parRunner], estFFX_0[,parRunner], alternative = "greater")$estimate
  
  simeststatistic25[parRunner] <- cor.test(simFFX[,parRunner], estFFX_25[,parRunner], alternative = "greater")$statistic
  simestpvalues25[parRunner] <- cor.test(simFFX[,parRunner], estFFX_25[,parRunner], alternative = "greater")$p.value
  simestestimate25[parRunner] <- cor.test(simFFX[,parRunner], estFFX_25[,parRunner], alternative = "greater")$estimate
  
  simeststatistic50[parRunner] <- cor.test(simFFX[,parRunner], estFFX_50[,parRunner], alternative = "greater")$statistic
  simestpvalues50[parRunner] <- cor.test(simFFX[,parRunner], estFFX_50[,parRunner], alternative = "greater")$p.value
  simestestimate50[parRunner] <- cor.test(simFFX[,parRunner], estFFX_50[,parRunner], alternative = "greater")$estimate
  
  simeststatistic75[parRunner] <- cor.test(simFFX[,parRunner], estFFX_75[,parRunner], alternative = "greater")$statistic
  simestpvalues75[parRunner] <- cor.test(simFFX[,parRunner], estFFX_75[,parRunner], alternative = "greater")$p.value
  simestestimate75[parRunner] <- cor.test(simFFX[,parRunner], estFFX_75[,parRunner], alternative = "greater")$estimate
  
  simeststatistic100[parRunner] <- cor.test(simFFX[,parRunner], estFFX_100[,parRunner], alternative = "greater")$statistic
  simestpvalues100[parRunner] <- cor.test(simFFX[,parRunner], estFFX_100[,parRunner], alternative = "greater")$p.value
  simestestimate100[parRunner] <- cor.test(simFFX[,parRunner], estFFX_100[,parRunner], alternative = "greater")$estimate
  
}

# Correct correlations for Intercept
p.adjust(c(simestpvalues0[1], simestpvalues25[1], simestpvalues50[1], simestpvalues75[1], simestpvalues100[1]), method = "holm")

# Correct correlations for PE
p.adjust(c(simestpvalues0[2], simestpvalues25[2], simestpvalues50[2], simestpvalues75[2], simestpvalues100[2]), method = "holm")

# Correct correlations for LRbias
p.adjust(c(simestpvalues0[3], simestpvalues25[3], simestpvalues50[3], simestpvalues75[3], simestpvalues100[3]), method = "holm")

# Correct correlations for Abw
p.adjust(c(simestpvalues0[4], simestpvalues25[4], simestpvalues50[4], simestpvalues75[4], simestpvalues100[4]), method = "holm")

# Correct correlations for LRbias:PE
p.adjust(c(simestpvalues0[5], simestpvalues25[5], simestpvalues50[5], simestpvalues75[5], simestpvalues100[5]), method = "holm")

# Get parameter names --------------------------------------
parnames <- colnames(coef(modelbias)$`pupildata$VPCode`)
parnames <- str_replace(parnames, "pupildata", "")
parnames <- str_replace(parnames, "pupildata", "")
parnames <- str_replace(parnames, "\\$", "")
parnames <- str_replace(parnames, "\\$", "")
parnames

# Initiate plot lists
plot0 <- list()
plot25 <- list()
plot50 <- list()
plot75 <- list()
plot100 <- list()

for (parRunner in 1:5){
  
  plotdata <- data.frame(sim = simFFX[,parRunner], est = estFFX_0[,parRunner])
  plot0[[parRunner]] <- ggplot(data = plotdata, aes(x = sim, y = est)) + geom_point(size = .1) + geom_smooth(method='lm') + theme_bw(base_size = 8) + scale_x_continuous(breaks = trans_breaks(identity, identity, n = 2)) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  
  plotdata <- data.frame(sim = simFFX[,parRunner], est = estFFX_25[,parRunner])
  plot25[[parRunner]] <- ggplot(data = plotdata, aes(x = sim, y = est)) + geom_point(size = .1) + geom_smooth(method='lm') + theme_bw(base_size = 8) + scale_x_continuous(breaks = trans_breaks(identity, identity, n = 2)) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  
  plotdata <- data.frame(sim = simFFX[,parRunner], est = estFFX_50[,parRunner])
  plot50[[parRunner]] <- ggplot(data = plotdata, aes(x = sim, y = est)) + geom_point(size = .1) + geom_smooth(method='lm') + theme_bw(base_size = 8) + scale_x_continuous(breaks = trans_breaks(identity, identity, n = 2)) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  
  plotdata <- data.frame(sim = simFFX[,parRunner], est = estFFX_75[,parRunner])
  plot75[[parRunner]] <- ggplot(data = plotdata, aes(x = sim, y = est)) + geom_point(size = .1) + geom_smooth(method='lm') + theme_bw(base_size = 8) + scale_x_continuous(breaks = trans_breaks(identity, identity, n = 2)) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
  
  plotdata <- data.frame(sim = simFFX[,parRunner], est = estFFX_100[,parRunner])
  plot100[[parRunner]] <- ggplot(data = plotdata, aes(x = sim, y = est)) + geom_point(size = .1) + geom_smooth(method='lm') + theme_bw(base_size = 8) + scale_x_continuous(breaks = trans_breaks(identity, identity, n = 2)) + scale_y_continuous(breaks = trans_breaks(identity, identity, n = 3)) + xlab("") + ylab("")
}

# Aggregate plots ------------------------------------------------------
fullplot0 <- plot_grid(plot0[[1]], plot0[[2]], plot0[[3]], plot0[[4]], plot0[[5]], nrow = 1)
fullplot25 <- plot_grid(plot25[[1]], plot25[[2]], plot25[[3]], plot25[[4]], plot25[[5]], nrow = 1)
fullplot50 <- plot_grid(plot50[[1]], plot50[[2]], plot50[[3]], plot50[[4]], plot50[[5]], nrow = 1)
fullplot75<- plot_grid(plot75[[1]], plot75[[2]], plot75[[3]], plot75[[4]], plot75[[5]], nrow = 1)
fullplot100 <- plot_grid(plot100[[1]], plot100[[2]], plot100[[3]], plot100[[4]], plot100[[5]], nrow = 1)
allCorrPlots <- plot_grid(fullplot0, fullplot25, fullplot50, fullplot75, fullplot100, nrow = 5)
allCorrPlots 
ggsave(file="allCorrPlots.svg", plot=allCorrPlots, width=16, height=16, units = "cm")











########################## absolute FFX recovery errors #########################
# First, get deviation between recovered FFX and underlying FFX for each simulation to quantify quality of recovery
esterrFFX_0   <- estFFX_0 - simFFX
esterrFFX_25  <- estFFX_25 - simFFX
esterrFFX_50  <- estFFX_50 - simFFX
esterrFFX_75  <- estFFX_75 - simFFX
esterrFFX_100 <- estFFX_100 - simFFX

esterrFFX_intercepts <- data.frame(FFX0 = esterrFFX_0[,1], FFX25 = esterrFFX_25[,1], FFX50 = esterrFFX_50[,1], FFX75 = esterrFFX_75[,1], FFX100 = esterrFFX_100[,1], simRun = c(1:nrow(esterrFFX_100)))
esterrFFX_PE         <- data.frame(FFX0 = esterrFFX_0[,2], FFX25 = esterrFFX_25[,2], FFX50 = esterrFFX_50[,2], FFX75 = esterrFFX_75[,2], FFX100 = esterrFFX_100[,2], simRun = c(1:nrow(esterrFFX_100)))
esterrFFX_LR         <- data.frame(FFX0 = esterrFFX_0[,3], FFX25 = esterrFFX_25[,3], FFX50 = esterrFFX_50[,3], FFX75 = esterrFFX_75[,3], FFX100 = esterrFFX_100[,3], simRun = c(1:nrow(esterrFFX_100)))
esterrFFX_Abw        <- data.frame(FFX0 = esterrFFX_0[,4], FFX25 = esterrFFX_25[,4], FFX50 = esterrFFX_50[,4], FFX75 = esterrFFX_75[,4], FFX100 = esterrFFX_100[,4], simRun = c(1:nrow(esterrFFX_100)))
esterrFFX_LRbiasByPE <- data.frame(FFX0 = esterrFFX_0[,5], FFX25 = esterrFFX_25[,5], FFX50 = esterrFFX_50[,5], FFX75 = esterrFFX_75[,5], FFX100 = esterrFFX_100[,5], simRun = c(1:nrow(esterrFFX_100)))

# Pivot into long format
esterrFFX_intercepts <- pivot_longer(esterrFFX_intercepts, cols = starts_with("FFX"))
esterrFFX_PE <- pivot_longer(esterrFFX_PE, cols = starts_with("FFX"))
esterrFFX_LR <- pivot_longer(esterrFFX_LR, cols = starts_with("FFX"))
esterrFFX_Abw <- pivot_longer(esterrFFX_Abw, cols = starts_with("FFX"))
esterrFFX_LRbiasByPE <- pivot_longer(esterrFFX_LRbiasByPE, cols = starts_with("FFX"))

# Add a factor named tiling for correct ordering of boxplots below, and for ANOVAs
esterrFFX_intercepts$tiling <- factor(esterrFFX_intercepts$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
esterrFFX_PE$tiling <- factor(esterrFFX_PE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
esterrFFX_LR$tiling <- factor(esterrFFX_LR$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
esterrFFX_Abw$tiling <- factor(esterrFFX_Abw$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
esterrFFX_LRbiasByPE$tiling <- factor(esterrFFX_LRbiasByPE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))

# Turn simRun into factor
esterrFFX_intercepts$simRun <- factor(esterrFFX_intercepts$simRun)
esterrFFX_PE$simRun <- factor(esterrFFX_PE$simRun)
esterrFFX_LR$simRun <- factor(esterrFFX_LR$simRun)
esterrFFX_Abw$simRun <- factor(esterrFFX_Abw$simRun)
esterrFFX_LRbiasByPE$simRun <- factor(esterrFFX_LRbiasByPE$simRun)

# Run Anovas
ezANOVA(esterrFFX_intercepts, dv = value, within = tiling, wid = simRun)
ezANOVA(esterrFFX_PE, dv = value, within = tiling, wid = simRun)
ezANOVA(esterrFFX_LR, dv = value, within = tiling, wid = simRun)
ezANOVA(esterrFFX_Abw, dv = value, within = tiling, wid = simRun)
ezANOVA(esterrFFX_LRbiasByPE, dv = value, within = tiling, wid = simRun)

# Make some more plots to show estimation errors for each parameter
esterrplot_intercepts <- ggplot(data = esterrFFX_intercepts, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "Intercept") +
                         scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
esterrplot_PE <- ggplot(data = esterrFFX_PE, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "PE") +
                         scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
esterrplot_LR <- ggplot(data = esterrFFX_LR, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "LRbias") +
                         scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
esterrplot_Abw <- ggplot(data = esterrFFX_Abw, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "|PE|") +
                         scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
esterrplot_LRbyPE <- ggplot(data = esterrFFX_LRbiasByPE, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "LRbias * PE") +
                         scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)

esterrplot <- plot_grid(esterrplot_intercepts, esterrplot_PE, esterrplot_LR, esterrplot_Abw, esterrplot_LRbyPE, ncol = 5)
esterrplot
ggsave(file="esterrplot.svg", plot=esterrplot, width=16, height=6, units = "cm")


# Next, get absolute deviation between recovered FFX and underlying FFX for each simulation to quantify quality of recovery
absEstErrFFX_0   <- abs(estFFX_0 - simFFX)
absEstErrFFX_25  <- abs(estFFX_25 - simFFX)
absEstErrFFX_50  <- abs(estFFX_50 - simFFX)
absEstErrFFX_75  <- abs(estFFX_75 - simFFX)
absEstErrFFX_100 <- abs(estFFX_100 - simFFX)

absEstErrFFX_intercepts <- data.frame(FFX0 = absEstErrFFX_0[,1], FFX25 = absEstErrFFX_25[,1], FFX50 = absEstErrFFX_50[,1], FFX75 = absEstErrFFX_75[,1], FFX100 = absEstErrFFX_100[,1], simRun = c(1:nrow(absEstErrFFX_100)))
absEstErrFFX_PE         <- data.frame(FFX0 = absEstErrFFX_0[,2], FFX25 = absEstErrFFX_25[,2], FFX50 = absEstErrFFX_50[,2], FFX75 = absEstErrFFX_75[,2], FFX100 = absEstErrFFX_100[,2], simRun = c(1:nrow(absEstErrFFX_100)))
absEstErrFFX_LR         <- data.frame(FFX0 = absEstErrFFX_0[,3], FFX25 = absEstErrFFX_25[,3], FFX50 = absEstErrFFX_50[,3], FFX75 = absEstErrFFX_75[,3], FFX100 = absEstErrFFX_100[,3], simRun = c(1:nrow(absEstErrFFX_100)))
absEstErrFFX_Abw        <- data.frame(FFX0 = absEstErrFFX_0[,4], FFX25 = absEstErrFFX_25[,4], FFX50 = absEstErrFFX_50[,4], FFX75 = absEstErrFFX_75[,4], FFX100 = absEstErrFFX_100[,4], simRun = c(1:nrow(absEstErrFFX_100)))
absEstErrFFX_LRbiasByPE <- data.frame(FFX0 = absEstErrFFX_0[,5], FFX25 = absEstErrFFX_25[,5], FFX50 = absEstErrFFX_50[,5], FFX75 = absEstErrFFX_75[,5], FFX100 = absEstErrFFX_100[,5], simRun = c(1:nrow(absEstErrFFX_100)))

# Pivot into long format
absEstErrFFX_intercepts <- pivot_longer(absEstErrFFX_intercepts, cols = starts_with("FFX"))
absEstErrFFX_PE <- pivot_longer(absEstErrFFX_PE, cols = starts_with("FFX"))
absEstErrFFX_LR <- pivot_longer(absEstErrFFX_LR, cols = starts_with("FFX"))
absEstErrFFX_Abw <- pivot_longer(absEstErrFFX_Abw, cols = starts_with("FFX"))
absEstErrFFX_LRbiasByPE <- pivot_longer(absEstErrFFX_LRbiasByPE, cols = starts_with("FFX"))

# Add a factor named tiling for correct ordering of boxplots below, and for ANOVAs
absEstErrFFX_intercepts$tiling <- factor(absEstErrFFX_intercepts$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
absEstErrFFX_PE$tiling <- factor(absEstErrFFX_PE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
absEstErrFFX_LR$tiling <- factor(absEstErrFFX_LR$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
absEstErrFFX_Abw$tiling <- factor(absEstErrFFX_Abw$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))
absEstErrFFX_LRbiasByPE$tiling <- factor(absEstErrFFX_LRbiasByPE$name, levels = c("FFX0", "FFX25", "FFX50", "FFX75", "FFX100"))

# Turn simRun into factor
absEstErrFFX_intercepts$simRun <- factor(absEstErrFFX_intercepts$simRun)
absEstErrFFX_PE$simRun <- factor(absEstErrFFX_PE$simRun)
absEstErrFFX_LR$simRun <- factor(absEstErrFFX_LR$simRun)
absEstErrFFX_Abw$simRun <- factor(absEstErrFFX_Abw$simRun)
absEstErrFFX_LRbiasByPE$simRun <- factor(absEstErrFFX_LRbiasByPE$simRun)

# Run Anovas
ezANOVA(absEstErrFFX_intercepts, dv = value, within = tiling, wid = simRun)
ezANOVA(absEstErrFFX_PE, dv = value, within = tiling, wid = simRun)
ezANOVA(absEstErrFFX_LR, dv = value, within = tiling, wid = simRun)
ezANOVA(absEstErrFFX_Abw, dv = value, within = tiling, wid = simRun)
ezANOVA(absEstErrFFX_LRbiasByPE, dv = value, within = tiling, wid = simRun)

# Make some more plots to show estimation errors for each parameter
absEstErrPlot_intercepts <- ggplot(data = absEstErrFFX_intercepts, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "Intercept") +
  scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
absEstErrPlot_PE <- ggplot(data = absEstErrFFX_PE, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "PE") +
  scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
absEstErrPlot_LR <- ggplot(data = absEstErrFFX_LR, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "LRbias") +
  scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
absEstErrPlot_Abw <- ggplot(data = absEstErrFFX_Abw, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "|PE|") +
  scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)
absEstErrPlot_LRbyPE <- ggplot(data = absEstErrFFX_LRbiasByPE, aes(x = tiling, y = value)) + geom_boxplot() + xlab("") + ylab("") + labs(title = "LRbias * PE") +
  scale_x_discrete(breaks=c("FFX0", "FFX50", "FFX100"), labels=c("FFX0" = "0", "FFX50" = "50", "FFX100" = "100")) + theme_bw(base_size = 8)

absEstErrPlot <- plot_grid(absEstErrPlot_intercepts, absEstErrPlot_PE, absEstErrPlot_LR, absEstErrPlot_Abw, absEstErrPlot_LRbyPE, ncol = 5)
absEstErrPlot
ggsave(file="absEstErrPlot.svg", plot=absEstErrPlot, width=16, height=6, units = "cm")