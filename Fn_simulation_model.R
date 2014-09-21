
#####################
# Simulate Dail-Madsen data at size i
# This code sort-of cheats by only tracking abundance at subsite j=1, and assuming abundance at other subsites is equal to its expected value (i.e., not accounting for demographic stochasticity for subsites contributing to immigration)
#####################
Sim_DM = function(){
  # Expected age distribution
  Exp_Nstable = RecRate * cumprod( 1-c(0,M_a[1:(MaxAge-1)]) )
  # Object to track simulated abundance-at-age
  N = array(NA, dim=c(Nyears+Nburnin,MaxAge), dimnames=list(c(paste0("Burn_",1:Nburnin),paste0("Year_",1:Nyears)), c("YOY",paste0("Age_",2:MaxAge))) )
  # Initial abundance
  N[1,1:MaxAge] = rpois(MaxAge, Exp_Nstable)
  # Project forward
  for(t in 2:(Nburnin+Nyears)) {
    # Recruits
    N[t,'YOY'] = rpois(1, RecRate)
    # Survivors
    N[t,2:MaxAge] = rbinom(n=MaxAge-1, size=N[t-1,1:(MaxAge-1)], prob=1-M_a[1:(MaxAge-1)] )
    # Emigration
    N[t,1:MaxAge] = N[t,1:MaxAge] - rbinom(n=MaxAge, size=N[t,1:MaxAge], prob=1-max(MoveProb_DM) )
    # Immigration
    N[t,1:MaxAge] = N[t,1:MaxAge] + rpois(n=MaxAge, Exp_Nstable*(1-max(MoveProb_DM)) )
  }
  # Detection probability
  y = array(NA, dim=c(Nyears,MaxAge,Nrep), dimnames=list(c(paste0("Year_",1:Nyears)), c("YOY",paste0("Age_",2:MaxAge)), paste0("Rep_",1:Nrep)) )
  for(t in 1:Nyears){
  for(a in 1:MaxAge){
    y[t,a,1:Nrep] = rbinom(Nrep, size=N[t+Nburnin,a], prob=P_a[a])
  }}
  # Aggregate count for adults
  y = aperm( abind('YOY'=y[,1,], 'adult'=apply(y[,-1,],MARGIN=c(1,3),FUN=sum), along=3), c(1,3,2))
  # Return stuff
  Return = list( "N"=N, "y"=y )
  return( Return )
}

#####################
# Simulate CJS data
#####################
Sim_CJS = function(){
}

