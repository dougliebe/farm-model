# Individual dairy
setwd("C:/Users/Doug/Documents/PhD Papers/Farm Models")
start_time <- Sys.time()
# Read input file
source('dairy.ind.text')

iDay <- 1
nDay <- iDay


################ Predicting DMI (Fox et al. 2004) ###############

# CETI (Fox & Tylutki 1998)
CETI <- 27.88 - (0.456 * Temp) + (0.010754 * Temp^2)- (0.4905 * RH) + (0.00088 * RH^2)+ (1.1507 * WS) - (0.126447 * WS^2)+ (0.019876 * Temp * RH)- (0.046313 * Temp * WS)+ (0.4167 * HRS)
DMINC <- (119.62 - 0.9708 * CETI)/100
DMIAF_temp <- if(Temp > 20) {
  DMINC
} else {1.0433 - (0.0044 * Temp) + (0.0001 * Temp^2)}

calf_NEma <- (1.37 * calf_ME) - (0.138 * calf_ME^2) + (0.0105 * calf_ME^3) - 1.12
heifer_NEma <- (1.37 * heifer_ME) - (0.138 * heifer_ME^2) + (0.0105 * heifer_ME^3) - 1.12

#correction for breed index (fox et al 2004)
BI <- ifelse(BI == 1, 1.08,1)

#Calves
calf_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma^2)-0.1128)/calf_NEma)*DMIAF_temp*BI
  return(DMI)
}
#Yearlings
heifer_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (SBW^0.75)*(((0.2435*heifer_NEma)*(0.0466*heifer_NEma^2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
  return(DMI)
}
#Lactating Cows
cow_lact_DMI <- function(BW) {
  DMI <- ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
  return(DMI)
}
#Dry Cows
cow_dry_DMI <- function(BW) {
  SBW <- 0.94*BW
  DMI <- (0.0185 * SBW)*DMIAF_temp*BI
  return(DMI)
}
####################### Excretion Calculations ############

# Nitrogen excreted (Nennich et al. 2005)
calf.N <- function(DMI) {
  return((DMI*calf_CP*112.55))
}

heifer.N <- function(DMI) {
  return((DMI*heifer_CP*78.39+51.4))
}

cow.lact.N<- function(DMI,BW) {
  return(((DMI*lact_CP*84.1)+(BW*0.196)))
}

#Use beef equation
cow.dry.N <- function(DMI) {
  return((DMI*dry_CP*78.39+51.4))
}

#Return adg as function of time for female cows 0 - 2500 days
dairy_ADG <- function(t) { #From Perotto et al, 1992
  A <- 619 #asymptotic weight, kg
  k <- 0.0020 #Rate parameter
  b <- 0.905 # integration constant
  M <- 1.2386 # inflection parameter
  # return((M*A*k*b*exp(-k*t))*((1-(b*exp(-k*t)))^M)*((1-(b*exp(-k*t)))^-1)) #returns ADG per cow, kg/d
  return((t<= 2500)*((A*(1-(b*exp(-k*t)))^M)/1000))
}

#For cumulative BW
area <- function(days) integrate(dairy_ADG, lower = 0, upper = ifelse(days<2500, days, 2500))$value
v.area <- Vectorize(area)


# Combine all equations to predict DMI
dairy_DMI <- function(df) {
  df$DMI = calf_DMI(df$BW)*(df$days < 365) + heifer_DMI(df$BW)*(df$days > 365 & df$days <= 730) +
    cow_lact_DMI(df$BW)*(df$days >=730 & df$days%%365 <= 305) +
    cow_dry_DMI(df$BW)*(df$days >= 730 & df$days%%365 > 305)
  return(df$DMI)
}

# Predict MY # Fox et al. 2004
dairy_MY <- function(df) {
  df$wol = (df$DIM/7)*(df$DIM < 305 & df$days <= df$DIM.last)
  PKYDa = (0.125*5+0.375)*PKYD
  A = 1/(PKYDa*(1/8.5)*exp(1))
  Milk = df$wol/(A*exp((1/8.5)*df$wol))
  Milk = Milk*(df$days < 365*3)*0.74 + Milk*(df$days < 365*4 & df$days >= 365*3)*0.88+ Milk*(df$days >= 365*4)
  df$MY = Milk*(0.122*df$Fat+0.077*df$Protein+0.249)*df$cECM
  return(df$MY)
}

# Combine all equations to predict N
dairy_N <- function(df) {
  df$N <- calf.N(df$DMI)*(df$days < 365) + heifer.N(df$DMI)*(df$days > 365 & df$lactation == 0) +
    cow.lact.N(df$DMI, df$BW)*(df$lactation > 0 & df$dup >= 56) +
    cow.dry.N(df$DMI)*(df$lactation > 0 & df$dup < 56)
  return(df$N)
}

# Predict pregnancy
# takes rate of preg success / # of days they will try
# if shes marked to cull, won't get preg
# If shes on day 0, give birth
dairy_preg <- function(df) {
  if('preg' %in% names(df) == F) {df$preg = 0}
  df[df$preg == 0 & df$days >= 365 & (df$DIM >= 30 | df$lactation == 0) & df$days %% 30 == 0, 'preg'] <-
    (runif(n = nrow(df[df$preg == 0 & df$days >= 365 & (df$DIM >= 30 | df$lactation == 0) & df$days %% 30 == 0,]), 0,1)<0.233)*1
  df[df$out == 1, 'preg'] <- 0
  return(df$preg)
}

# Parameters for milk yield
PKYDa = (0.125*5+0.375)*PKYD
A = 1/(PKYDa*(1/8.5)*exp(1))

integrate.milk <- function(days, Fat, Protein, cECM) {
  PKYDa = (0.125*5+0.375)*PKYD
  A = 1/(PKYDa*(1/8.5)*exp(1))
  my = sum(((days%%365/7)*(dup >= 56))/(A*exp((1/8.5)*(days%%365/7))))*
    (0.122*Fat+0.077*Protein+0.249)*cECM
  return(max(my,0))
  # wol = (days > 730 & days %% 365 <= 305)*(days%%365/7)
  # PKYDa = (0.125*5+0.375)*PKYD
  # A = 1/(PKYDa*(1/8.5)*exp(1))
  # Milk = wol/(A*exp((1/8.5)*wol))
  # Milk = Milk*(days < 365*3)*0.74 + Milk*(days < 365*4 & days >= 365*3)*0.88+ Milk
  # MY = Milk*(0.122*Fat+0.077*Protein+0.249)*cECM
  # return(MY)
}
#Things for Production Above Replacement (PAR)
milk.price = 0.31; protein.price = 3.61; fat.price = 5.22 # all $/kg


## Function for correlation adjustment
coradj <- function(value, cor, years) {
  return(((value-1)*cor^years)+1)
}

#Future milk yield
FMY <- function(df) {
  # Only account for subsequent lactations
  herd.avereage.milk = sum((seq(1,305,1)%%365/7)/(A*exp((1/8.5)*(seq(1,305,1)%%365/7))))
  preg.now = (df$preg == 0)*(pmax(1-(0.71^floor((280-df$DIM)/30)),0))+(df$preg == 1)
  m1 = coradj(df$cECM, cor, 1)*herd.avereage.milk*(preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 0)
  m2 = coradj(df$cECM, cor, 2)*herd.avereage.milk*((preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 1)+(df$lactation != 1)*(kPreg/100))*(df$lactation <= 1)
  m3 = coradj(df$cECM, cor, 3)*herd.avereage.milk*((preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 2)+(df$lactation != 2)*(kPreg/100))*(df$lactation <= 2)
  m4 = coradj(df$cECM, cor, 4)*herd.avereage.milk*((preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 3)+(df$lactation != 3)*(kPreg/100))*(df$lactation <= 3)
  m5 = coradj(df$cECM, cor, 5)*herd.avereage.milk*((preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 4)+(df$lactation != 4)*(kPreg/100))*(df$lactation <= 4)
  m6 = coradj(df$cECM, cor, 6)*herd.avereage.milk*((preg.now*(df$preg == 0)+(df$preg == 1))*(df$lactation == 5)+(df$lactation != 5)*(kPreg/100))*(df$lactation <= 5)
  # survival odds
  s1 = 1-survival(1)
  s2 = 1-survival(2)
  s3 = 1-survival(3)
  s4 = 1-survival(4)
  s5 = 1-survival(5)
  s6 = 1-survival(6)
  return(m1*s1+m2*s2+m3*s3+m4*s4+m5*s5+m6*s6)
}


survival <- function(lactation) {
  a = (((1-dweibull(x = pmax(0,(lactation+1)), shape = 1.5, scale = 1.0))-
         (1-dweibull(x = pmax(0,lactation), shape = 1.5, scale = 1.0)))/
    (1-(1-dweibull(x = pmax(0,lactation), shape = 1.5, scale = 1.0))))/4+(1*(lactation > 7))
  a[is.nan(a)] <- 1
  a[is.infinite(a)] <- 0
  return(a)
}

new.group <- function(ids, ages,weeks, prog.scc = 1, prog.ecm = 1) {
  #Make initial data frame - lactating
  df.cows <- data.frame(CowID = ids,
                        var = rnorm(n = length(ids),mean = 0, sd = 10),
                        lactation = ages,
                        tSCC = rnorm(n = length(ids), mean = ((prog.scc-1)*cor)+1, 0.4),
                        cECM = rnorm(n = length(ids), mean = ((prog.ecm-1)*cor)+1, 0.4),
                        Fat = rnorm(n = length(ids), mean = MilkFat, sd = 0.1),
                        Protein = rnorm(n = length(ids), mean = MilkProtein, sd = 0.1),
                        out = rep(0,length(ids)),
                        dup = rep(283, length(ids)))
  df.cows$days = (365*df.cows$lactation+365)*(df.cows$lactation > 0)+
    (df.cows$lactation == 0)*365*(sample(c(0,1), length(ids), replace = T))+weeks*7
  df.cows$DIM = weeks*7
  df.cows$DIM.last <- df.cows$dup+df.cows$days -56
  df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
  df.cows$BW <- v.area(df.cows$days)*(((df.cows$var)/100)+1)
  df.cows$DMI <- dairy_DMI(df.cows)
  df.cows$MY <- dairy_MY(df.cows)
  df.cows$N <- dairy_N(df.cows)
  df.cows$preg <- 0
  return(df.cows)
}

## Funciton to collect data
collect_data <- function(nDay) {
  outputs = list()
  outputs[1] <- sum(df.cows$MY)
  outputs[2] <- sum(df.cows$N)
  outputs[3] <- nrow(df.cows[df.cows$days < 365,])
  outputs[4] <- nrow(df.cows[df.cows$lactation > 0 &  df.cows$days <= df.cows$DIM.last,])
  outputs[5] <- nrow(df.cows[df.cows$lactation > 0 & df.cows$days > df.cows$DIM.last,])
  outputs[6] <- sum(df.cows$DMI)
  outputs[7] <- nrow(df.cows[df.cows$days >= 365 & df.cows$days < 730,])
  outputs[8] <- mean(df.cows[df.cows$lactation > 1,"out"])
  outputs[9] <- mean(df.cows[df.cows$lactation > 0 & df.cows$DIM <= 305,"preg"])
  outputs[10] <- round(mean(df.cows[df.cows$lactation > 0,'lactation']),2)
  return(unlist(outputs))
}


### Start
# set.seed(1)
total.start.cows = 150

lactating_ages <- c(1:5)
# correlation of cECM to young
cor = 0.3

#Make initial data frame - lactating
df.cows <- new.group(ids = seq(1,round(total.start.cows*1.2),1),
                     ages = sample(lactating_ages, round(total.start.cows*1.2), replace = T),
                     weeks = sample(1:365, round(total.start.cows*1.2), replace = T)/7)
df.cows[1:round(total.start.cows*1), 'dup'] <- sample(1:283, size = round(round(total.start.cows)*1), replace = T)
df.cows[1:round(total.start.cows*1), 'preg'] <- 1
df.cows$DIM.last <- df.cows$dup+df.cows$days -56
df.cows$DIM <- 282-df.cows$dup

# non lactating
df.cows2 <- new.group(ids = seq(round(total.start.cows*1.2)+1,round(total.start.cows*1.2)+round(total.start.cows*0.17),1),
                      ages = sample(0, round(total.start.cows*0.17), replace = T),
                      weeks = sample(1:365, round(total.start.cows*0.17), replace = T)/7)
df.cows[1:round(nrow(df.cows2)*0.40), 'dup'] <- sample(1:283, size = round(nrow(df.cows2)*0.40), replace = T)
df.cows[1:round(nrow(df.cows2)*0.40), 'preg'] <- 1
df.cows$DIM.last <- df.cows$dup+df.cows$days -56
df.cows$DIM <- 282-df.cows$dup
df.cows <- rbind(df.cows, df.cows2)

# Calculate mean MY for herd
mean_MY = mean(df.cows[df.cows$lactation != 0,]$MY)*365

# # Plot Stats
par(mfrow= c(2,2))
par(mar = c(3,4,3,4))
# plot(df.cows$days, df.cows$ADG, ylab = 'ADG')
# plot(df.cows$days, df.cows$BW, ylab = 'BW')
# plot(df.cows$days, df.cows$DMI, ylab = 'DMI')
# plot(df.cows$days, df.cows$N, ylab = 'N, per day')

### Running Simulation
n = rep(0, fDay)

# DFs to collect info during run
dead <- data.frame()
# outputs <- data.frame(Milk = rep(0,fDay), N = rep(0,fDay),
#                       calves = rep(0,fDay), lact = rep(0,fDay), dry = rep(0,fDay),
#                       age_lact = rep(0, fDay),heifer = rep(0, fDay))
outputs <- matrix(nrow = fDay, ncol = 10)
off = 0
target = 0

# ETA
paste('projected completion time:',(total.start.cows*fDay*60/1000*0.008)+start_time)

# Loop
while(nDay <= fDay) {
  # Collect data
  outputs[nDay,] <- collect_data(nDay)

  ###### Marking/Culling

  currentMark <- mean(df.cows[df.cows$lactation > 0 & df.cows$preg == 0,"out"])
  heifers <- nrow(df.cows[df.cows$lactation == 0 & df.cows$dup < markFreq,])
  dry <- nrow(df.cows[df.cows$lactation > 0 & df.cows$dup < markFreq,])
  lastOff = off
  off = (nrow(df.cows[df.cows$lactation > 0 &  df.cows$days <= df.cows$DIM.last,]) -
           total.start.cows)
  target = round(max(0,off + heifers + dry))

  #Mark every x days
  if(nDay %% markFreq == 0) {

    over = max(0,target)
    possible_cull = df.cows[(df.cows$preg == 0 & df.cows$lactation > 0),]

    # Culling strats
    if(cullStrat == 1) {
      idsCull <- possible_cull[order(possible_cull$cECM),][1:over,'CowID']
      df.cows[df.cows$CowID %in% idsCull, 'out'] <- 1
    } else if(cullStrat == 2) {
      idsCull <- possible_cull[order(possible_cull$days, decreasing = T),][1:over,'CowID']
      df.cows[df.cows$CowID %in% idsCull, 'out'] <- 1
    } else {
      idsCull <- possible_cull[order(possible_cull$FV, decreasing = T),][1:over,'CowID']
      possible_cull[order(possible_cull$cECM),][1:over,'out'] <- 1 # Needs changed
    }
  }

  # Decide which cows will be dead before next lactation

  df.cows[(df.cows$lactation > 0 &
             df.cows$dup == 0 & runif(nrow(df.cows),0,1) <
             survival(df.cows$lactation)),'out'] <- 1

##### Cows to cull now

  ### Heifers
  new_cows <- df.cows[df.cows$lactation ==0 & df.cows$days > 500 & df.cows$preg == 0,]

  # if cows dry w/o getting pregnant or are marked, cull now
  new_cows <- rbind(new_cows, df.cows[df.cows$lactation > 0 &
                                        ((df.cows$out == 1 & df.cows$DIM == 305) |
                        (df.cows$preg == 0 & df.cows$DIM >= 305)),])
  #Keep track of who leaves herd
  dead <- rbind(dead, new_cows)


  # remove culled cows from list
  df.cows <- if(nrow(new_cows) > 0) {df.cows[!(df.cows$CowID %in% new_cows$CowID),]} else {df.cows}

  # if any cows culled and total herd is below strating #, add more heifers
  if(off < 0 & nrow(new_cows) > 0) {
    df.cows.new <- new.group(new_cows$CowID+total.start.cows,
                             rep(1, nrow(new_cows)),
                             weeks = sample(0, nrow(new_cows), replace = T)/7)
    df.cows <- rbind(df.cows, df.cows.new)
  }

  #### Calving

  #If new female calves
  # Check to see if any cows calved
  new_calves <- df.cows[df.cows$preg == 1 & df.cows$dup == 0 &
                          runif(1,0,1) < (rateFemale/100),]

  # make new df for those calves related to their mothers traits
  if(nrow(new_calves)>0) {
    df.calves.new <- new.group(ids = new_calves$CowID+(total.start.cows*10),
                               ages = rep(0, nrow(new_calves)),
                               weeks = sample(0, nrow(new_calves), replace = T)/7,
                               prog.ecm = new_calves$cECM,
                               prog.scc =  new_calves$tSCC)

    # add calves to full group
    df.cows <- rbind(df.cows, df.calves.new)
  }

 ##### Updating

  #Update other fields
  df.cows$ADG <- mapply(dairy_ADG, df.cows$days)
  df.cows$BW <- v.area(df.cows$days)
  df.cows$DMI <- dairy_DMI(df.cows)
  df.cows$MY <- dairy_MY(df.cows)
  df.cows$N <- dairy_N(df.cows)

  # If she gave birth
  df.cows$cECM <- rnorm(n = nrow(df.cows), mean = coradj(df.cows$cECM, cor = cor, years = 1),sd = 0.4)*(df.cows$dup == 0) + (df.cows$cECM)*(df.cows$dup != 0)
  df.cows[df.cows$dup == 0, 'lactation'] <- df.cows[df.cows$dup == 0, 'lactation'] +1
  df.cows$DIM <- (df.cows$DIM + 1)*(df.cows$dup != 0)*(df.cows$lactation != 0)
  df.cows[df.cows$dup == 282 | df.cows$dup == 283, 'DIM.last'] <-
    df.cows[df.cows$dup == 282 | df.cows$dup == 283, 'days'] +
    df.cows[df.cows$dup == 282 | df.cows$dup == 283, 'dup'] - 56
  df.cows[df.cows$dup == 0, 'preg'] <- 0
  df.cows[df.cows$dup == 0, 'dup'] <- 283
  df.cows$preg <- dairy_preg(df.cows)
  df.cows[df.cows$preg == 1 & df.cows$dup != 0, 'dup'] <- df.cows[df.cows$preg == 1 & df.cows$dup != 0, 'dup'] - 1


  #change day # for cows and counter
  df.cows$days <- df.cows$days + 1
  nDay = nDay+1

  if(outputs[nDay-1,4] > total.start.cows*2) {break}
}
# Burn off the first 20% or 1000 days
outputs[0:(min(1000,round(fDay*0.20))),] <- NA
end_time <- Sys.time()



## Plotting results

moving_avg <- function(x, n) {
  cx <- c(rep(NA,365),cumsum(x))
  return((cx[(365+1):length(cx)] - cx[1:(length(cx) - 365)]) / 365)
}

rsum <- moving_avg(x = outputs[(min(1000,round(fDay*0.20))):fDay,8], n = 365)
plot(outputs[,8], type = 'l', ylab = '% Marked in Lact', xlim = c(0,fDay))
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs[(min(1001,round(fDay*0.20)+1)):fDay,1]/total.start.cows, n = 365)
plot(outputs[,1]/total.start.cows, type = 'l',
     xlim = c(0,fDay),ylab = 'Milk/Lactating Herd Size')
lines(rsum, col = 'red', lwd = 2, lty = 2, xlim = c((min(1001,round(fDay*0.20)+1)),fDay))

rsum <- moving_avg(x = outputs[(min(1000,round(fDay*0.20))):fDay,2]/total.start.cows, n = 365)
plot(outputs[,2]/total.start.cows, type = 'l',
     xlim = c(0,fDay), ylab = 'N/Lactating Herd Size')
lines(rsum, col = 'red', lwd = 2, lty = 2)

rsum <- moving_avg(x = outputs[(min(1000,round(fDay*0.20))):fDay,4], n = 365)
plot(outputs[,4], type = 'l', ylim = c(0,total.start.cows*1.2),
     xlim = c(0,fDay), ylab = '# of Cows')
lines(rsum, col = 'orange', lwd = 2, lty = 2)
lines(outputs[,5], col = 'red') #dry
lines(outputs[,3], col = 'blue') #calves
lines(outputs[,7], col = 'green') #heifers
abline(h = total.start.cows)

paste(round((end_time - start_time)/total.start.cows/fDay*1000,5), "minutes per 1000 cow days,",
      round((end_time - start_time),2),'Mins for', (fDay*total.start.cows), 'Total cow days')

paste('average lactation:',round(mean(df.cows[df.cows$lactation > 0,'lactation']),2))
