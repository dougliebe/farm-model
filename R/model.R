# setwd("C:/Users/Doug/Documents/PhD Papers/Farm Models/farm-model")

#####################  FOR LOOP #######################
#Edit source of dairy file
source('R/Broiler.R')

#Write an array
val <- array(1, dim = c(10, length(seq(iDay,fDay, by = 1))))
flow <- array(1, dim = c(9, length(seq(iDay,fDay, by = 1))))
cumManure = 0

while(nDay <= fDay) {
  if(animal == 'broiler') {

    # Set t for broilers, know switch times
    t <- ifelse(nDay%%(switch_feed*2) == 0,2, ifelse(nDay%%switch_feed ==0,1,0))

    # # Rates
    # born <- (t==2)*kBirth*iChicks
    # maturing <- (t==1)*kMature*nChicks #t switches for broilers
    # culling <- (t==2)*kCull*nBroilers
    # laying <- kLaying*nLayers

    ADG <- broiler_ADG(Temp)
    DMI <- broiler_Intake(ADG)
    DIGIntake <- DMI *0.7
    cumManure = cumManure + (DMI - DIGIntake)*(nChicks+nBroilers)
    Nexc <- broiler_N_excretion(ADG)
    Nvol <- broiler_N_volitilization(Nexc)
    Pexc <- broiler_P_excretion(ADG)
    Nmin <- broiler_N_mineralization(Nexc, Nvol) # in kilograms
    Norg <- (Nexc-Nvol)-Nmin # in kilograms
    Pmin <- broiler_P_mineralization(Pexc) # in kilograms
    Porg <- Pexc - Pmin

    if(t == 2) {
      nChicks = iChicks
      wtChicks = 0.025*nChicks
      culling = kCull*nBroilers
      nBroilers = 0
      wtBroilers = 0
    } else if (t == 1) {
      nBroilers = nChicks
      wtBroilers = wtChicks
      nChicks = 0
      wtChicks = 0
    } else {
      wtBroilers <- (nBroilers > 0)*(wtBroilers + ADG*nBroilers)
      wtChicks <- (nChicks > 0)*(wtChicks + ADG*nChicks)
    }

    # Update Numbers of Animals
    nBroilers <- nBroilers-(kMortality*(nBroilers > 0))
    nChicks <- nChicks-(kMortality*(nChicks > 0))
    
    meat_produced <- culling*(wtBroilers)
  }

  if(animal == 'layer') {

    # Set t for broilers, know switch times
    molt = sum(runif(1,0,1) < Pmolt)
    t <- if((day_since_new_pullets == 90*7)*(molt == 1)) {2
      } else if(day_since_new_pullets == 17*7) {1
        }else if((sum(nMolt.hens.brown, nMolt.hens.white) > 0)*(day_since_new_pullets == 120*7) |
            day_since_new_pullets == 90*7) {3
          }else {0}

    # Fluxes
    new.pullets <- (t==3)*(iPullets.brown+iPullets.white)
    new.layers <- (t==1)*(nPullets.brown+nPullets.white)*((nPullets.brown+nPullets.white)>0)
    molting <- (t==2)*(nLaying.hens.brown+nLaying.hens.white)
    culling <- (t==3)*(nLaying.hens.brown+nLaying.hens.white+nMolt.hens.brown+nMolt.hens.white)



    # Updating Counts
    nLaying.hens.brown <- nLaying.hens.brown + (new.layers-culling*(nLaying.hens.brown>0)-molting)*(iPullets.brown > 0)
    nPullets.brown <- nPullets.brown + (new.pullets-new.layers)
    nBreed.hens.brown <- nBreed.hens.brown
    nMolt.hens.brown <- nMolt.hens.brown + (molting-culling)*(iPullets.brown > 0)*(t==2 | (t==3)*(nMolt.hens.brown>0))

    nLaying.hens.white <- nLaying.hens.white + (new.layers-culling*(nLaying.hens.white>0)-molting)*(iPullets.white > 0)
    nPullets.white <- nPullets.white + (new.pullets - new.layers)
    nBreed.hens.white <- nBreed.hens.white
    nMolt.hens.white <- nMolt.hens.white + (molting-culling)*(iPullets.white > 0)*(t==2 | (t==3)*(nMolt.hens.white>0))

    #Excretions
    Nexc <- layer_N_excretion(layer_ADG())

    meat_produced <- meat_produced + culling
    day_since_new_pullets = (day_since_new_pullets + 1)*(t != 3)
  }

  if(animal == 'dairy') {
    born <- ((kFreshening*nDry*0.5)+(kFreshening*nHeifer.first.dry*0.5)+
               +(kMature*nCalf*0.5)+
      (kFreshening*nHeifer.second.dry*0.5)+(kFreshening*nHeifer.third.dry*0.5))*kMortality/100
    new.lact.herd <- (kFreshening*nDry)*(100-kMortality)/100
    new.first.lact <- kMature*nCalf*(100-kMortality)/100
    new.second.lact <- kFreshening*nHeifer.first.dry*(100-kMortality)/100
    new.third.lact <- kFreshening*nHeifer.second.dry*(100-kMortality)/100
    new.first.dry <- kDry*nHeifer.first.lact*(100-kMortality)/100
    new.second.dry <- kDry*nHeifer.second.lact*(100-kMortality)/100
    new.third.dry <- kDry*nHeifer.third.lact*(100-kMortality)/100
    new.dry.herd <- kDry*nLact*(100-kMortality)/100
    new.lact.from.third <- kFreshening*nHeifer.third.dry*(100-kMortality)/100
    culling <- (kCull/100*nLact)/365

    # Excretions
    # Nexc <- calf.N()+heifer.first.lact.N()+heifer.second.lact.N()+heifer.third.lact.N()+
    #   heifer.first.dry.N()+heifer.second.dry.N()+heifer.third.dry.N()+cow.lact.N()+cow.dry.N()
    # Pexc <- calf.P()+heifer.first.lact.P()+heifer.second.lact.P()+heifer.third.lact.P()+
    #   heifer.first.dry.P()+heifer.second.dry.P()+heifer.third.dry.P()+cow.lact.P()+cow.dry.P()

    #Update Numbers of Animals
    nCalf <- nCalf + born - new.first.lact + 1
    nHeifer.first.dry <- nHeifer.first.dry + new.first.dry - new.second.lact
    nHeifer.second.dry <- nHeifer.second.dry + new.second.dry - new.third.lact
    nHeifer.third.dry <- nHeifer.third.dry + new.third.dry - new.lact.from.third
    nHeifer.first.lact <- nHeifer.first.lact + new.first.lact - new.first.dry
    nHeifer.second.lact <- nHeifer.second.lact + new.second.lact - new.second.dry
    nHeifer.third.lact <- nHeifer.third.lact + new.third.lact - new.third.dry
    nLact <- nLact + new.lact.herd - culling - new.dry.herd + new.lact.from.third
    nDry <- nDry + new.dry.herd - new.lact.herd
    nCalf <- ifelse(nCalf < 0, 0, nCalf)

    meat_produced <- culling*(wtLact)
  }

  if(animal == 'beef') {

    calves <- kCow.birth * nCow.dry # same as new lactating animals
    lactating <- kCow.birth * nCow.dry
    bred.cows <- kCow.bred * nCow.lact
    dry.cows <- kCow.dry * nCow.bred
    slaughter.lact.cows <- kCow.slaughter * nCow.lact

    growing.steers <- kProd.steer.grow * nProd.bull.calf
    growing.cows <- kProd.cow.grow * nProd.cow.calf
    finishing.steers <- kProd.steer.finish * nProd.steer.grow
    finishing.cows <- kProd.cow.finish * nProd.cow.grow
    slaughter.steers <- kProd.slaughter * nProd.steer.finish
    slaughter.cows <- kProd.slaughter * nProd.cow.finish

    growing.bred.cows <- kBreed.cow.grow * nBreed.cow.calf
    growing.bred.bulls <- kBreed.bull.grow * nBreed.bull.calf
    mature.bred.bulls <- kBreed.cow.mature * nBreed.bull.grow
    mature.bred.cows <- kBreed.bull.mature * nBreed.cow.grow
    slaughter.bred.bulls <- kBreed.bull.slaughter * nBreed.bull.mature

    # Excretions
    # Nexc <-   Prod.calf.N()+Prod.steer.grow.N()+Prod.cow.grow.N()+Breed.bull.grow.N()+Breed.cow.grow.N()+Breed.bull.mature.N()
    # Pexc <- Prod.calf.P()+Prod.steer.grow.P()+Prod.cow.grow.P()+Breed.bull.grow.P()+Breed.cow.grow.P()+Breed.bull.mature.P()

    # Update Numbers of Animals
    nCow.bred <- nCow.bred + bred.cows - dry.cows - slaughter.lact.cows
    nCow.dry <- nCow.dry + dry.cows + mature.bred.cows - lactating
    nCow.lact <- nCow.lact + lactating - mature.bred.cows

    nProd.bull.calf <- nProd.bull.calf + (calves*male.birth.rate) - growing.steers
    nProd.cow.calf <- nProd.cow.calf + (calves*(1-male.birth.rate)) - growing.cows
    nProd.steer.grow <- nProd.steer.grow + growing.steers - finishing.steers
    nProd.cow.grow <- nProd.cow.grow + growing.cows - finishing.cows
    nProd.steer.finish <- nProd.steer.finish + finishing.steers - slaughter.steers
    nProd.cow.finish <- nProd.cow.finish + finishing.cows - slaughter.cows

    nBreed.cow.calf <- nBreed.cow.calf + (calves*(1-male.birth.rate)*breedstock.rate) - growing.bred.cows
    nBreed.bull.calf <- nBreed.bull.calf + (calves*(male.birth.rate)*breedstock.rate) - growing.bred.bulls
    nBreed.cow.grow <- nBreed.cow.grow + growing.bred.cows - mature.bred.cows
    nBreed.bull.grow <- nBreed.bull.grow + growing.bred.bulls - mature.bred.bulls
    nBreed.bull.mature <- nBreed.bull.mature + mature.bred.bulls - slaughter.bred.bulls

    meat_produced <- sum((slaughter.bred.bulls*breed.bull.wt),(slaughter.lact.cows*lact.cow.wt),
                         (slaughter.cows*cow.max.wt),(slaughter.bred.bulls*steer.max.wt))
  }

  # New Totals
  Meat <- Meat + meat_produced
  # Eggs <- Eggs + laying*nLayers
  # Milk <- Milk + kMilk*nLact
  # N <- N + Nexc
  # P <- P + Pexc
  # P.day <- Pexc

  # # Update Numbers of Animals
  flow[1,nDay] <- nChicks
  flow[2,nDay] <- nBroilers
  # flow[3,nDay] <- nHeifer.first.dry
  # flow[4,nDay] <- nLact
  # flow[5,nDay] <- nDry
  # flow[6,nDay] <- nHeifer.second.lact
  # flow[7,nDay] <- nHeifer.third.dry
  # flow[8,nDay] <- nHeifer.third.lact
  # flow[9,nDay] <- nHeifer.second.dry

  # # Update values
  # val[1,nDay] <- nCalf
  # val[2,nDay] <- nHeifer.first.lact
  # val[3,nDay] <- nLact
  # val[4,nDay] <- nLaying.hens.brown+nPullets.brown+nMolt.hens.brown
  val[5,nDay] <- Porg
  # val[6,nDay] <- total.drypen
  val[7,nDay] <- Norg
  val[8,nDay] <- Pmin
  val[9,nDay] <- Nmin
  # val[10,nDay] <- P

  nDay = nDay+1
}
##############################################

# Plot results
par(mfrow = c(1,4))
# plot(val[1,], main = 'calves', xlab = 'Day', ylab = 'Count')
# plot(val[2,], main = 'steer.grow', xlab = 'Day', ylab = 'Count')
# plot(val[3,], main = 'steer.finish', xlab = 'Day', ylab = 'Count')
# plot(val[4,], main = 'Broilers', xlab = 'Day', ylab = 'Count')
plot(flow[1,], main = "Organic P", xlab = 'Day', ylab = 'kg')
plot(flow[2,], main = "Mineralized N", xlab = 'Day', ylab = 'kg')
plot(val[7,], main = 'Organic N', xlab = 'Day', ylab = 'kg')
plot(val[8,], main = 'Mineralized P', xlab = 'Day', ylab = 'kg')
# plot(val[5,], main = 'Dry Cows', xlab = 'Day', ylab = 'Count')

# plot(val[10,], main = "P Excretion", xlab = 'Day', ylab = 'Wt, kg')
