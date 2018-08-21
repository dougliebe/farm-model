###### BROILERS #######
animal <- 'broiler'

# Start a day count
fDay <- 1000 #Length of Simulation
iDay = 1
nDay = iDay
# Set chick numbers
iChicks <- 25000

kMortality <- 5 #Mortality, % over cycle

switch_feed <- 22 #Age at which diet is switched


# Feed Characteristics
ME_young <- 3050 #Energy of diet, kcal / kg
ME_mature <- 3180 

CPy <- 21.5 #Crude protein for growing, %
CPo <- 19 #Crude protein for finishing, %

nP_young <- 0.25 #non-phytate P in diet for growing, %
nP_mature <- 0.25 #non-phytate P in diet for finishing, %

P_young <- 500 #added Phytate in diet for growing, U/kg diet
P_mature <- 500 #added Phytate in diet for finishing, U/kg diet

# Weather
Temp <- 31 #Degrees, C

nChicks <- iChicks #Set starting count of young
wtChicks <- 0.025*nChicks #each chick = 25g
nBroilers <- 0
wtBroilers <- 0

# Flux rates
kMature <- 1 #when chicks mature, they all do at once
kBirth <- 1 #When chicks hatch, get all new chicks
kCull <- 1 #when we sell, all sell
kMortality <- kMortality*iChicks/switch_feed*2/100
Temp <- if(Temp<=23) {23} else {if(Temp>31) {31} else{Temp}}



################ FUNCTIONS #########################


broiler_ADG <- function(Temp) {
  # # Use this for ADG calculations
  # # For use as BW in ADG eq.
  wt_per_bird <- sum(wtChicks,wtBroilers)/sum(nChicks,nBroilers)
  wt_per_bird <- wt_per_bird*1000
  
  #May et al 1998
  ADG <- -31.797 + (1.2071*Temp) + (0.21457*wt_per_bird) - (8.852E-5*wt_per_bird^2) +
    (1.51E-8*wt_per_bird^3) - (2.0772E-3*Temp*wt_per_bird)
  
  return(ADG/1000)
}

#Zuidhof et al 2014
broiler_Intake <- function(ADG) {
  I_ME <- (196*(sum(wtChicks,wtBroilers)/sum(nChicks,nBroilers))^0.7543)+2.4833*ADG*1000 #Convert to g
  ME <- if(nChicks > 0) {ME_young} else {ME_mature}
  Intake <- I_ME/ME
  return(Intake)
}

broiler_N_excretion <- function(ADG) {
  CP <- ifelse(nChicks>0, CPy,CPo)
  FI <- broiler_Intake(ADG)
  # For Mature Birds
  if(nBroilers > 0) {
    Nintake <- FI*1000 * nBroilers * (CP/100)/6.25
    Nret <- 29 * (ADG*nBroilers) # Constant 29 g/kg of BW gain according to ITAVI, 2013
    Nexc <- Nintake - Nret # Formula from Belloir et al. 2017
    return(Nexc/1000) # Returns N in kg
  }
  # For young birds
  else {
    Nintake <- FI * nChicks * (CP/100)/6.25 #N in kg
    Nexc <- 0.589*Nintake*1000 - 5.004 #Bregendahl et al. 2002 using N, grams
    return(Nexc/1000)  # Returns N in kg
  }
}

# Gale and Gilmour 1986
broiler_N_volitilization <- function(Nexc) {
  #percent N lost from litter, removal,
  # and storage per day - Moore et al 2011
  return((0.362+0.116+0.002)*Nexc)
}
# Beegle et al 2008
broiler_N_mineralization <- function(Nexc, Nvol) {
  #percent N mineralized in field
  #minus what has already been volitilized
  return((Nexc-Nvol)*0.49)
}

broiler_P_excretion <- function(ADG) {
  nP <- ifelse(nChicks > 0, nP_young, nP_mature)
  P <- ifelse(nChicks > 0, P_young, P_mature)
  FI <- broiler_Intake(ADG)
  P_exc <- exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  #Kornegay et al. 1996
                 ((0.4088*log(nP))^2)+((-0.0087*log(P))^2)+
                 (0.0012*log(P)*log(nP))) #g/kg intake
  Pexc <- FI*P_exc*sum(nBroilers, nChicks)
  return(Pexc/1000) # Return P in kg
}

# Eghball 2002
broiler_P_mineralization <- function(Pexc) {
  #percent P mineralized in broiler litter
  return((Pexc)*0.9) # about 90% inorganic
}


############### MODEL ##############################
val <- array(1, dim = c(length(seq(iDay,fDay, by = 1)),10))


while (nDay <= fDay) {
  
  # Set t for broilers, know switch times
  t <- ifelse(nDay%%(switch_feed*2) == 0,2, ifelse(nDay%%switch_feed ==0,1,0))
  
  # Rates
  born <- (t==2)*kBirth*iChicks
  maturing <- (t==1)*kMature*nChicks #t switches for broilers
  culling <- (t==2)*kCull*nBroilers
  # laying <- kLaying*nLayers
  
  ADG <- broiler_ADG(Temp)
  Nexc <- broiler_N_excretion(ADG)
  Nvol <- broiler_N_volitilization(Nexc)
  Pexc <- broiler_P_excretion(ADG)
  Nmin <- broiler_N_mineralization(Nexc, Nvol) # in kilograms
  Norg <- (Nexc-Nvol)-Nmin # in kilograms
  Pmin <- broiler_P_mineralization(Pexc) # in kilograms
  Porg <- Pexc - Pmin
  
  
  
  # Update Numbers of Animals
  nBroilers <- nBroilers+(maturing-culling)-(kMortality*(nBroilers > 0))
  nChicks <- nChicks+(born-maturing)-(kMortality*(nChicks > 0))
  meat_produced <- culling*(wtBroilers)
  
  wtBroilers <- (nBroilers > 0)*(wtBroilers + ADG*nBroilers + (t==1)*wtChicks)
  wtChicks <- (nChicks > 0)*(wtChicks + ADG*nChicks)
  if(t==2) {wtChicks <- 0.025*nChicks} #reset new chicks

  # # Update values
  # val[1,nDay] <- nCalf
  # val[2,nDay] <- nHeifer.first.lact
  # val[3,nDay] <- nLact
  # val[4,nDay] <- nLaying.hens.brown+nPullets.brown+nMolt.hens.brown
  val[nDay,5] <- Porg
  # val[6,nDay] <- total.drypen
  val[nDay,7] <- Norg
  val[nDay,8] <- Pmin
  val[nDay,9] <- Nmin
  # val[10,nDay] <- P
  
  nDay = nDay+1
  
}


#################### Plot results #######################

par(mfrow = c(1,4))
# plot(val[1,], main = 'calves', xlab = 'Day', ylab = 'Count')
# plot(val[2,], main = 'steer.grow', xlab = 'Day', ylab = 'Count')
# plot(val[3,], main = 'steer.finish', xlab = 'Day', ylab = 'Count')
# plot(val[4,], main = 'Broilers', xlab = 'Day', ylab = 'Count')
plot(val[,5], main = "Organic P", xlab = 'Day', ylab = 'kg')
plot(val[,9], main = "Mineralized N", xlab = 'Day', ylab = 'kg')
plot(val[,7], main = 'Organic N', xlab = 'Day', ylab = 'kg')
plot(val[,8], main = 'Mineralized P', xlab = 'Day', ylab = 'kg')
# plot(val[5,], main = 'Dry Cows', xlab = 'Day', ylab = 'Count')

# plot(val[10,], main = "P Excretion", xlab = 'Day', ylab = 'Wt, kg')