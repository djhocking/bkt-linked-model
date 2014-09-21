
#####################
#
# Simulate data for "linked" DM and CJS data
#
#####################

library(abind) # abind()

source("Fn_simulation_model.R")

#####################
# Parameter values
#####################

# Biological parameters
M = c(0.5,0.1) #  Annual (not instantaneous!) mortality rate: YOY, adult
MaxAge = 20 # Assumed maximum age, after which all individuals are dead
P = c(0.2,0.4) # YOY, adult
DiffusionSD_m = 50 # in meters
Size_DM_m = rep(100,11)      # Size of DM simulation area, divided into different blocks (only using data from block 11)
Size_CJS_m = rep(100,10)  # Size of CJS sampling area, divided into different blocks
RecRate = 30 # Expected number of YOY per year (density independent rate, NOT per capita)

# Data availability
Nyears = 10 # Years with available data
Nrep = 3 # Replicate samples per year for DM data
Nsite_DM = 100 # Sites for DM data
Nsite_CJS = 10  # Sizes for CJS data

# Other parmeters
Nburnin = 10 # Years for burn-in of dynamics prior to data (can't be zero in current implementation)

# Derived values
M_a = c(M[1], rep(M[2],MaxAge-2), 1 ) # Annual mortality rate (M) at age            #
P_a = c(P[1], rep(P[2],MaxAge-2), 1 ) # Detection probabilitty (P) at age            #
MoveProb_DM = dnorm(x=cumsum(Size_DM_m)-sum(Size_DM_m)/2-50, mean=0, sd=DiffusionSD_m )
  MoveProb_DM = MoveProb_DM / sum(MoveProb_DM)

####################
# Simulate data
####################
for( s in 1:Nsite_DM){
  # Run DM simulator
  Tmp = Sim_DM()
  # Aggregate counts for different sites
  if(s==1){
    y = array(NA, dim=c(Nyears,Nsite_DM,2,Nrep), dimnames=list(c(paste0("Year_",1:Nyears)), paste0('Site_',1:Nsite_DM), c("YOY",'adult'), paste0("Rep_",1:Nrep)) )
    y[,1,,] = Tmp$y
  }else{
    y[,s,,] = Tmp$y  
  }
}
