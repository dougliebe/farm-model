      program dairy_total
      implicit none


      integer :: nDay, fDay

      ! Set starting animal numbers
      real :: iCalf
      real :: iHeifer_first_lact
      real :: iHeifer_second_lact
      real :: iHeifer_third_lact
      real :: iHeifer_first_dry
      real :: iHeifer_second_dry
      real :: iHeifer_third_dry
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
      real :: born, calf_NEma, CETI, culling, heifer_NEma, meat_produced
      real :: new_dry_herd, new_first_dry, new_first_lact, new_lact_from_third
      real :: new_lact_herd, new_second_lact, new_second_dry, new_third_dry
      real :: new_third_lact, ADG, DIM, lact_P

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
      real :: kMature
      real :: kDry
      real :: kFreshening
      real :: kMilk

      ! Set outputs to 0
      real :: Meat
      real :: Milk
      real :: N
      real :: P

      ! Set animal numbers to start
      real :: nCalf
      real :: nHeifer_first_lact
      real :: nHeifer_first_dry
      real :: nHeifer_second_lact
      real :: nHeifer_second_dry
      real :: nHeifer_third_lact
      real :: nHeifer_third_dry
      real :: nLact
      real :: nDry

      real :: wtCalf
      real :: wtHeifer_first_lact
      real :: wtHeifer_second_lact
      real :: wtHEifer_third_lact
      real :: wtHeifer_first_dry
      real :: wtHeifer_second_dry
      real :: wtHeifer_third_dry
      real :: wtLact
      real :: wtDry




      ! Start a day count
      nDay = 1
      fDay = 10 !Length of Simulation

      ! Set starting animal numbers
      iCalf = 220
      iHeifer_first_lact = 70
      iHeifer_second_lact = 70
      iHeifer_third_lact =70
      iHeifer_first_dry = 15
      iHeifer_second_dry = 15
      iHeifer_third_dry = 15
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
      kMature = (1/365.0)*2
      kDry = (1/305.0)
      kFreshening = (1/60.0)
      kMilk = 0
      kMortality = 7 !as whole number percentage

      ! Set outputs to 0
      Meat = 0
      Milk = 0
      N = 0
      P = 0

      ! Set animal numbers to start
      nCalf = iCalf 
      nHeifer_first_lact = iHeifer_first_lact
      nHeifer_first_dry = iHeifer_first_dry
      nHeifer_second_lact = iHeifer_second_lact
      nHeifer_second_dry = iHeifer_second_dry
      nHeifer_third_lact = iHeifer_third_lact
      nHeifer_third_dry = iHeifer_third_dry
      nLact = iLact
      nDry = iDry

      wtCalf = 0
      wtHeifer_first_lact = 0
      wtHeifer_second_lact = 0
      wtHEifer_third_lact = 0
      wtHeifer_first_dry = 0
      wtHeifer_second_dry = 0
      wtHeifer_third_dry = 0
      wtLact = 0
      wtDry = 0
      
     do nDay = 1, fDay

      CETI = 27.88 - (0.456 * Temp) + (0.010754 * Temp**2)- &
     &  (0.4905 * RH) + (0.00088 * RH**2)+ (1.1507 * WS) - &
     & (0.126447 * WS**2)+ (0.019876 * Temp * RH)- &
     & (0.046313 * Temp * WS)+ (0.4167 * HRS)

      DMINC = (119.62 - 0.9708 * CETI)/100
      if(Temp .ge. 20) then 
      	DMIAF_temp = DMINC
      end if 
      if(Temp .lt. 20) then 
      	DMIAF_temp = 1.0433 - (0.0044 * Temp) + (0.0001 * Temp**2)
      end if
      calf_NEma = (1.37 * calf_ME) - (0.138 * calf_ME**2) + (0.0105 * calf_ME**3) - 1.12
      heifer_NEma = (1.37 * heifer_ME) - (0.138 * heifer_ME**2) + (0.0105 * heifer_ME**3) - 1.12

      !correction for breed index (fox et al 2004)
      if(Breed == 1) then 
      	BI = 1.08
      else 
      	BI = 1
      end if

      ! calculations of weights 
  
      !print *, Porg


      !new animal numbers

      born = ((kFreshening*nDry*0.5)+(kFreshening*nHeifer_first_dry*0.5)+ &
   &           +(kMature*nCalf*0.5)+ &
   &  (kFreshening*nHeifer_second_dry*0.5)+(kFreshening*nHeifer_third_dry*0.5))*kMortality/100 
      new_lact_herd = (kFreshening*nDry)*(100-kMortality)/100
      new_first_lact = kMature*nCalf*(100-kMortality)/100
      new_second_lact = kFreshening*nHeifer_first_dry*(100-kMortality)/100
      new_third_lact = kFreshening*nHeifer_second_dry*(100-kMortality)/100
      new_first_dry = kDry*nHeifer_first_lact*(100-kMortality)/100
      new_second_dry = kDry*nHeifer_second_lact*(100-kMortality)/100
      new_third_dry = kDry*nHeifer_third_lact*(100-kMortality)/100
      new_dry_herd = kDry*nLact*(100-kMortality)/100
      new_lact_from_third = kFreshening*nHeifer_third_dry*(100-kMortality)/100
      culling = (kCull/100*nLact)/365
      !wt updating


      ! update animal numbers
      nCalf = nCalf + born - new_first_lact + 1
      nHeifer_first_dry = nHeifer_first_dry + new_first_dry - new_second_lact
      nHeifer_second_dry = nHeifer_second_dry + new_second_dry - new_third_lact
      nHeifer_third_dry = nHeifer_third_dry + new_third_dry - new_lact_from_third
      nHeifer_first_lact = nHeifer_first_lact + new_first_lact - new_first_dry
      nHeifer_second_lact = nHeifer_second_lact + new_second_lact - new_second_dry
      nHeifer_third_lact = nHeifer_third_lact + new_third_lact - new_third_dry
      nLact = nLact + new_lact_herd - culling - new_dry_herd + new_lact_from_third
      nDry = nDry + new_dry_herd - new_lact_herd
	  if (nCalf .lt. 0) then
        nCalf = 0
      endif 

      meat_produced = culling*(wtLact)



      !print *,nDay, Porg,Nmin,Pmin,cumManure,manureStore
        print *, nLact
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
                  Nexc = ((DMI*lact_CP*84.1)+(wtLact/nLact*0.196))
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
           &            (1.2+4.635*MW**(0.22)*(wtLact/nLact)**(-0.22))*ADG/0.96)
                  return
            end function cowPexc

            function dryPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = (DMI*dry_CP*78.39+51.4)
                  return
            end function dryPexc
      end program dairy_total
