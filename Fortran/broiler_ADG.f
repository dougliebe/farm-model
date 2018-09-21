




C broiler_N_excretion <- function(ADG) {
C   CP <- ifelse(nChicks>0, CPy,CPo)
C   FI <- broiler_Intake(ADG)
C   # For Mature Birds
C   if(nBroilers > 0) {
C     Nintake <- FI*1000 * nBroilers * (CP/100)/6.25
C     Nret <- 29 * (ADG*nBroilers) # Constant 29 g/kg of BW gain according to ITAVI, 2013
C     Nexc <- Nintake - Nret # Formula from Belloir et al. 2017
C     return(Nexc/1000) # Returns N in kg
C   }

C       function brlNex(ADG, CPy, CPo, nChicks, nBroilers) return(Nexc)
C       implicit none
C       if (nChicks .gt. 0) then CP = CPy
C       else CP = CPo
C       end if
C       FI - brlDMI()

C   # For young birds
C   else {
C     Nintake <- FI * nChicks * (CP/100)/6.25 #N in kg
C     Nexc <- 0.589*Nintake*1000 - 5.004 #Bregendahl et al. 2002 using N, grams
C     return(Nexc/1000)  # Returns N in kg
C   }
C }

C # Gale and Gilmour 1986
C broiler_N_volitilization <- function(Nexc) {
C   #percent N lost from litter, removal,
C   # and storage per day - Moore et al 2011
C   return((0.362+0.116+0.002)*Nexc)
C }
C # Beegle et al 2008
C broiler_N_mineralization <- function(Nexc, Nvol) {
C   #percent N mineralized in field
C   #minus what has already been volitilized
C   return((Nexc-Nvol)*0.49)
C }

C broiler_P_excretion <- function(ADG) {
C   nP <- ifelse(nChicks > 0, nP_young, nP_mature)
C   P <- ifelse(nChicks > 0, P_young, P_mature)
C   FI <- broiler_Intake(ADG)
C   P_exc <- exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  #Kornegay et al. 1996
C                  ((0.4088*log(nP))^2)+((-0.0087*log(P))^2)+
C                  (0.0012*log(P)*log(nP))) #g/kg intake
C   Pexc <- FI*P_exc*sum(nBroilers, nChicks)
C   return(Pexc/1000) # Return P in kg
C }

C # Eghball 2002
C broiler_P_mineralization <- function(Pexc) {
C   #percent P mineralized in broiler litter
C   return((Pexc)*0.9) # about 90% inorganic
C }




      program broiler 
      implicit none
      ! make array for outputs
      real outputs(45)

      real :: ADG, born, CPo, CPy, culling, CumSumWt, fDay, iChicks
      real :: iDay, kBirth, kCull, kLaying, kMature, kMortality
      real :: maturing, ME_mature, ME_young, N, nBroilers, nChicks
      real :: nDay, Nexc, Nmin, Norg, nP_mature, nP_young, Nvol, P 
      real :: P_mature, P_young, Pexc, Pmin, Porg, switch_feed
      real :: Temp, wtBroilers, wtChicks, meat_produced
      real :: wt_per_bird
      integer :: t

      ! Start a day count
      fDay = 45 !Length of Simulation
      iDay = 1
      nDay = iDay
      switch_feed = 22

      iChicks = 10
      nChicks = iChicks !Set starting count of young
      wtChicks = 0.025*nChicks !each chick = 25g
      nBroilers = 0
      wtBroilers = 0
      wt_per_bird = wtChicks/nChicks
      Temp = 31

      ! Flux rates
      kMature = 1 ! when chicks mature, they all do at once
      kBirth = 1 ! When chicks hatch, get all new chicks
      kCull = 1 ! when we sell, all sell
      kMortality = 5 ! expressed as a percentage over lifespan
      kMortality = kMortality*iChicks/switch_feed*2/100
      maturing = 0
      culling = 0
      born = 0

      ! Feed characteristics
      ME_young = 3050 !Energy of diet, kcal / kg
      ME_mature = 3180 

      CPy = 21.5 !Crude protein for growing, %
      CPo = 19 !Crude protein for finishing, %

      nP_young = 0.25 !non-phytate P in diet for growing, %
      nP_mature = 0.25 !non-phytate P in diet for finishing, %

      P_young = 500 !added Phytate in diet for growing, U/kg diet
      P_mature = 500 !added Phytate in diet for finishing, U/kg diet


      ! put ceiling on temp effects
      if(Temp .le. 23) Temp = 23

      if (Temp .ge. 31) Temp = 31
      
      

      ! Make a while loop 
      do while (nDay .le. fDay)
            if (mod(nDay,(switch_feed*2)) .eq. 0) then
                  nChicks = iChicks
                  wtChicks = 0.025*nChicks
                  culling = nBroilers
                  nBroilers = 0
                  wtBroilers = 0
                  t = 2
            else if (mod(nDay,(switch_feed)) .eq. 0) then
                  nBroilers = nChicks
                  wtBroilers = wtChicks
                  nChicks = 0
                  wtChicks = 0
                  t = 1
            else
                  if (nBroilers .gt. 0) wtBroilers=
     &            (wtBroilers+ADG*nBroilers)
                  if (nChicks .gt. 0) wtChicks = (wtChicks +
     &             ADG*nChicks)
                  t = 0
            end if

            ! calculations of outputs
            wt_per_bird = (wtChicks+wtBroilers)/(nChicks+nBroilers)*1000
            ADG = brlADG()

      

            
            outputs(int(nDay)) = brlNex()
            ()


            !update animal numbers
            if (nBroilers .gt. 0) nBroilers = nBroilers-(kMortality)
            if (nChicks .gt. 0) nChicks = nChicks-(kMortality)
            
            !wt updating
            meat_produced = culling*(wtBroilers)

            

C             if (nBroilers .gt. 0) wtBroilers=(wtBroilers+ADG*nBroilers)
C             if (nChicks .gt. 0) wtChicks = (wtChicks + ADG*nChicks)


      nDay = nDay + 1

      end do
      print *, outputs
      contains
            function brlADG() result(ADG)
               implicit none
               real :: ADG
               ADG = -31.797 + (1.2071*Temp) + (0.21457*wt_per_bird) - 
     &         (8.852E-5*wt_per_bird**2) 
     &         + (1.51E-8*wt_per_bird**3) 
     &         - (2.0772E-3*Temp*wt_per_bird)
               ADG = ADG/1000
            return
            end function brlADG
      !Zuidhof et al 2014
      function brlDMI() result(DMI)
            implicit none
            real :: DMI, intake_ME, fed_ME
            intake_ME = (196*wt_per_bird**0.7543)+2.4833*ADG*1000
            if (nChicks .gt. 0) then
                  fed_ME = ME_young
            else 
                  fed_ME = ME_mature
            end if
            DMI = intake_ME/fed_ME
      return
      end function brlDMI
      
      function brlNex() result(Nexc)
            implicit none
            real :: Nexc, CP, FI, Nintake, Nret
            if (nChicks .gt. 0) then
                  CP = CPy
            else 
                  CP = CPo
            end if
            FI = brlDMI(ADG)
            if (nBroilers .gt. 0) then
                  Nintake = FI*1000 * nBroilers * (CP/100)/6.25
                  Nret = 29 * (ADG*nBroilers) ! Constant 29 g/kg of BW gain according to ITAVI, 2013
                  Nexc = (Nintake - Nret)/1000 ! Formula from Belloir et al. 2017
            else 
                  Nintake = FI * nChicks * (CP/100)/6.25 !N in kg
                  Nexc = (0.589*Nintake*1000 - 5.004)/1000 !Bregendahl et al. 2002 using N, grams
            end if      
            return ! Returns N in kg
      end function brlNex





      end program broiler


