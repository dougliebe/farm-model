      program dairy_total
      implicit none


      real :: nDay
      real :: fDay

      ! Set starting animal numbers
      real :: iCalf
      real :: iHeifer.first.lact
      real :: iHeifer.second.lact
      real :: iHeifer.third.lact
      real :: iHeifer.first.dry
      real :: iHeifer.second.dry
      real :: iHeifer.third.dry
      real :: iLact
      real :: iDry

      ! Animal Characteristics
      real :: kCull
      real :: kMortality
      real :: BI
      real :: Breed
      real :: PKYD
      real :: MW
      real :: kPreg
      real :: MilkFat
      real :: MilkProtein
      real :: FCM

      !###########################################
      ! Feed Characteristics


      ! Calf Feeding
      real :: calf_ME
      real :: calf_CP

      !Yearling Feed
      real :: heifer_ME
      real :: heifer_CP

      !Lactating Cow Feed
      real :: lact_CP

      !Dry Cow Diet
      real :: dry_CP


      !###########################################
      ! Environmental Conditions
      real :: HRS

      ! Perceived Temeprature
      real :: Temp
      real :: RH
      real :: WS
      real :: DMIAF_temp
      real :: DMINC


      ! Set all rates to 0 before reading files
      real :: kCull
      real :: kMature
      real :: kDry
      real :: kFreshening
      real :: kMilk
      real :: kMortality

      ! Set outputs to 0
      real :: Meat
      real :: Milk
      real :: N
      real :: P

      ! Set animal numbers to start
      real :: nCalf
      real :: nHeifer.first.lact
      real :: nHeifer.first.dry
      real :: nHeifer.second.lact
      real :: nHeifer.second.dry
      real :: nHeifer.third.lact
      real :: nHeifer.third.dry
      real :: nLact
      real :: nDry

      real :: wtCalf
      real :: wtHeifer.first.lact
      real :: wtHeifer.second.lact
      real :: wtHEifer.third.lact
      real :: wtHeifer.first.dry
      real :: wtHeifer.second.dry
      real :: wtHeifer.third.dry
      real :: wtLact
      real :: wtDry




      ! Start a day count
      nDay = 1
      fDay = 4000 !Length of Simulation

      ! Set starting animal numbers
      iCalf = 220
      iHeifer.first.lact = 70
      iHeifer.second.lact = 70
      iHeifer.third.lact =70
      iHeifer.first.dry = 15
      iHeifer.second.dry = 15
      iHeifer.third.dry = 15
      iLact = 500
      iDry = 100

      ! Animal Characteristics
      kCull = 80 !rate of cows leaving milking herd, %
      kMortality = 5 !rate of cows lost to disease, etc., %
      Breed = 1 !if Holsteins 1, else 0
      PKYD = 43 !peak milk yield, kg/d
      MW = 600 !mature weight in kg
      kPreg = 40
      MilkFat = 3.5 !average MF% of herd
      MilkProtein = 3.5 !average Protein% of herd
      FCM = 3 !Fat corrected milk, %

      !###########################################
      ! Feed Characteristics


      ! Calf Feeding
      calf_ME = 5 !calf ME of diet
      calf_CP = 1 !CP, %

      !Yearling Feed
      heifer_ME = 5 !yearling ME of diet
      heifer_CP = 2.5 !CP, %

      !Lactating Cow Feed
      lact_CP = 10 !CP, %

      !Dry Cow Diet
      dry_CP = 10 !CP, %


      !###########################################
      ! Environmental Conditions
      HRS = 12 !Hours perceived sunlight

      ! Perceived Temeprature
      Temp = 24 !Indoor temp, C
      RH = 75 !Relative Humidity, %
      WS = 1 !Average wind speed, kph


      ! Set all rates to 0 before reading files
      kCull = 0
      kMature = 1/365*2
      kDry = 1/305
      kFreshening = 1/60
      kMilk = 0
      kMortality = 7 !as whole number percentage

      ! Set outputs to 0
      Meat = 0
      Milk = 0
      N = 0
      P = 0

      ! Set animal numbers to start
      nCalf = iCalf 
      nHeifer.first.lact = iHeifer.first.lact
      nHeifer.first.dry = iHeifer.first.dry
      nHeifer.second.lact = iHeifer.second.lact
      nHeifer.second.dry = iHeifer.second.dry
      nHeifer.third.lact = iHeifer.third.lact
      nHeifer.third.dry = iHeifer.third.dry
      nLact = iLact
      nDry = iDry

      wtCalf = 0
      wtHeifer.first.lact = 0
      wtHeifer.second.lact = 0
      wtHEifer.third.lact = 0
      wtHeifer.first.dry = 0
      wtHeifer.second.dry = 0
      wtHeifer.third.dry = 0
      wtLact = 0
      wtDry = 0


      ! put ceiling on temp effects
      if(Temp .le. 23) then Temp = 23
      if (Temp .ge. 31) then Temp = 31
      
      
     do nDay = 1, fDay

      CETI = 27.88 - (0.456 * Temp) + (0.010754 * Temp**2)- &
     &  (0.4905 * RH) + (0.00088 * RH^2)+ (1.1507 * WS) - &
     & (0.126447 * WS^2)+ (0.019876 * Temp * RH)- &
     & (0.046313 * Temp * WS)+ (0.4167 * HRS)

      DMINC = (119.62 - 0.9708 * CETI)/100
      if(Temp .ge. 20) then DMIAF_temp = DMINC
      if(Temp .lt. 20) then DMIAF_temp = 1.0433 - (0.0044 * Temp) + (0.0001 * Temp**2)
      
      calf_NEma = (1.37 * calf_ME) - (0.138 * calf_ME**2) + (0.0105 * calf_ME**3) - 1.12
      heifer_NEma = (1.37 * heifer_ME) - (0.138 * heifer_ME**2) + (0.0105 * heifer_ME**3) - 1.12

      !correction for breed index (fox et al 2004)
      if(Breed == 1) then BI = 1.08
      else BI = 1

      ! calculations of weights 
  
      !print *, Porg

      ! Get fractions
      cumNorg = cumNorg + Norg
      cumNmin = cumNmin + Nmin
      cumPorg = cumPorg + Porg
      cumPmin = cumPmin + Pmin


      !update animal numbers
      if (nBroilers .gt. 0) nBroilers = nBroilers-(kMortality*iChicks/(switch_feed*2)/100)
      if (nChicks .gt. 0) nChicks = nChicks-(kMortality*iChicks/(switch_feed*2)/100)
      
      !wt updating
      meat_produced = culling*(wtBroilers)

      if (mod(nDay,(switch_feed*2)) .eq. 0) then
            manure_out = cumManure
            Pmin_frac = cumPmin/manure_out
            Porg_frac = cumPorg/manure_out
            Norg_frac = cumNorg/manure_out
            Nmin_frac = cumNmin/manure_out
      else
            manure_out = 0
            Pmin_frac = 0
            Porg_frac = 0
            Norg_frac = 0
            Nmin_frac = 0
      end if

      ! add a storage var for manure
      manureStore = manureStore + manure_out
      !print *,nDay, Porg,Nmin,Pmin,cumManure,manureStore
        print *, nDay,nChicks,nBroilers,manureStore,intake,cumManure
      end do
     ! print *,nDay, Porg,Nmin,Pmin,cumManure,manureStore
       !!nDay = nDay + 1
!             if (nBroilers .gt. 0) wtBroilers=(wtBroilers+ADG*nBroilers)
!             if (nChicks .gt. 0) wtChicks = (wtChicks + ADG*nChicks)


      ! subroutines 
      contains
            function calfDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.1128)/calf_NEma)*DMIAF_temp*BI
                  return
            end function calfDMI

            function heiferDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = (SBW**0.75)*(((0.2435*calf_NEma)-(0.0466*calf_NEma**2)-0.0869)/heifer_NEma)*DMIAF_temp*BI
                  return
            end function heiferDMI

            function lactDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = ((0.0185 * BW) + (0.305 * FCM))*DMIAF_temp*BI
                  return
            end function lactDMI

            function dryDMI(BW) result(DMI)
                  implicit none
                  real, intent(in) :: BW
                  real :: DMI, SBW
                  SBW = 0.94*BW
                  DMI = (0.0185 * SBW)*DMIAF_temp*BI
                  return
            end function dryDMI

            ! Nitrogen Equations

            function heiferNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (DMI*heifer_CP*78.39+51.4) !per cow
                  return
            end function heiferNexc

            function calfNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (DMI*calf_CP*112.55)
                  return
            end function calfNexc

            function lactNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (((DMI*lact_CP*84.1)+(wtLact/nLact*0.196))
                  return
            end function lactNexc

            function dryNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (DMI*dry_CP*78.39+51.4)
                  return
            end function dryNexc

            ! Phosphorus Equations

            function calfPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = (DMI*calf_CP*112.55)
                  return
            end function calfPexc

            function cowPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = ((DMI*lact_P)-(2*(wtLact/nLact)/1000)-0.02743* &
           &            exp(((0.05527-0.000075*DIM)*DIM))- &
           &            0.02743*exp(((0.05527-0.000075*(DIM-1))*(DIM-1)))* &
           &            (1.2+4.635*MW^0.22*(wtLact/nLact)^-0.22)*ADG/0.96)
                  return
            end function cowPexc

            function dryPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = (DMI*dry_CP*78.39+51.4)
                  return
            end function calfPexc
            





      end program dairy_total
