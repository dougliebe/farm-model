      program dairy_total
      implicit none


!     ~ ~ ~ PURPOSE ~ ~ ~
!     this subroutine computes the lake hydrologic pesticide balance.
!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!     variable          |definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     nDay              | day in cycle 
!     nCalf             | number of calves
!     nHeifer_first_lact| first lactation cows           
!     nHeifer_first_dry | cows dry after first lactation 
!     nHeifer_second_lact| second lactation cows           
!     nHeifer_second_dry| number of cows dry after sec lactation           
!     nHeifer_third_lact| third lactation cows           
!     nHeifer_third_dry | number of third lactation dry      
!     nLact             | number of fourth lactation or greater, cows
!     nDry              | cow number fourth lactation or greater, dry cows
!     calf_ME           | calf metabolizable energy of feed, kcal/kg       
!     calf_CP           | calf feed crude protein, %
!     heifer_ME         | heifer ME of feed, kcal/kg
!     heifer_CP         | heifer crude protein, % 
!     lact_CP           | lactating crude protein, %
!     dry_CP            | dry cow crude protein, %
!     lact_target       | target lactating herd size   
!     kCull             | culling target year-over-year, %
!     kMortality        | mortality rate in herd, %
!     Breed             |Breed index, 1 if holstein - 0 otherwise             
!     PKYD              | peak milk yield, kg/d       
!     MW                | mature weight of lactating animal, kg      
!     kPreg             | pregnancy rate
!     MilkFat           | milk fat, %
!     MilkProtein       | milk protein, %
!     FCM               | fat corrected milk
!     Temp              | temperature, C       
!     RH                | Relative Humidity, %
!     WS                | wind speed, kph
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!     variable                | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     Nmin                    | mineralized Nitrogen in manure, g
!     Norg                    | organic Nitrogen in manure, g 
!     Pmin                    | mineralized Phosphorus in manure, g
!     Porg                    | organic Phosphorus in manure, g
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!     name                    | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


      ! Set animal numbers to start
     
!     age_out          
!     first_age_out          
!     second_age_out         
!     third_age_out          
!     fourth_age_out         
!     calves_death, calves_cull          
!     first_lact_death, first_lact_cull        
!     second_lact_death, second_lact_cull      
!     third_lact_death, third_lact_cull        
!     fourth_lact_death, fourth_lact_cull      
!     firstCull, secondCull, thirdCull, fourthCull, kHeiferCull        

!     wtCalf           
!     wtHeifer_first_lact          
!     wtHeifer_second_lact         
!     wtHEifer_third_lact          
!     wtHeifer_first_dry           
!     wtHeifer_second_dry          
!     wtHeifer_third_dry           
!     wtLact           
!     wtDry      
!     lact_DMI         
!     dry_DMI          
!     calf_DMI         
!     heifer_first_lact_DMI        
!     heifer_second_lact_DMI       
!     heifer_third_lact_DMI        
!     heifer_first_dry_DMI         
!     heifer_second_dry_DMI        
!     heifer_third_dry_DMI         
!     lact_N           
!     dry_N      
!     calf_N           
!     heifer_first_lact_N          
!     heifer_second_lact_N         
!     heifer_third_lact_N          
!     heifer_first_dry_N           
!     heifer_second_dry_N          
!     heifer_third_dry_N           
!     lact_P           
!     dry_P      
!     calf_P           
!     heifer_first_lact_P          
!     heifer_second_lact_P         
!     heifer_third_lact_P          
!     heifer_first_dry_P           
!     heifer_second_dry_P          
!     heifer_third_dry_P           
! ##########################################


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
      real :: lact_target
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
      real :: new_third_lact, ADG, DIM

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
      real :: age_out
      real :: first_age_out
      real :: second_age_out
      real :: third_age_out
      real :: fourth_age_out
      real :: calves_death, calves_cull
      real :: first_lact_death, first_lact_cull
      real :: second_lact_death, second_lact_cull
      real :: third_lact_death, third_lact_cull
      real :: fourth_lact_death, fourth_lact_cull
      real :: firstCull, secondCull, thirdCull, fourthCull, kHeiferCull

      real :: wtCalf
      real :: wtHeifer_first_lact
      real :: wtHeifer_second_lact
      real :: wtHEifer_third_lact
      real :: wtHeifer_first_dry
      real :: wtHeifer_second_dry
      real :: wtHeifer_third_dry
      real :: wtLact
      real :: wtDry
      real :: lact_DMI
      real :: dry_DMI
      real :: calf_DMI
      real :: heifer_first_lact_DMI
      real :: heifer_second_lact_DMI
      real :: heifer_third_lact_DMI
      real :: heifer_first_dry_DMI
      real :: heifer_second_dry_DMI
      real :: heifer_third_dry_DMI
      real :: lact_N
      real :: dry_N
      real :: calf_N
      real :: heifer_first_lact_N
      real :: heifer_second_lact_N
      real :: heifer_third_lact_N
      real :: heifer_first_dry_N
      real :: heifer_second_dry_N
      real :: heifer_third_dry_N
      real :: lact_P
      real :: dry_P
      real :: calf_P
      real :: heifer_first_lact_P
      real :: heifer_second_lact_P
      real :: heifer_third_lact_P
      real :: heifer_first_dry_P
      real :: heifer_second_dry_P
      real :: heifer_third_dry_P




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


      lact_target = 100
      kHeiferCull = 0.22
      born = 0

      firstCull = 0.1
      secondCull = 0.2
      thirdCull = 0.3
      fourthCull = 0.5
      ! Animal Characteristics
      kCull = 80.0 !rate of cows leaving milking herd, %
      kMortality = 7.0 !rate of cows lost to disease, etc., %
      Breed = 1 !if Holsteins 1, else 0
      PKYD = 43.0 !peak milk yield, kg/d
      MW = 600.0 !mature weight in kg
      kPreg = 40.0
      MilkFat = 3.5 !average MF% of herd
      MilkProtein = 3.5 !average Protein% of herd
      FCM = 3.0 !Fat corrected milk, %

      !###########################################
      ! Feed Characteristics


      ! Calf Feeding
      calf_ME = 5.0 !calf ME of diet
      calf_CP = 16.0 !CP, %

      !Yearling Feed
      heifer_ME = 5.0 !yearling ME of diet
      heifer_CP = 2.5 !CP, %

      !Lactating Cow Feed
      lact_CP = 10.0 !CP, %

      !Dry Cow Diet
      dry_CP = 10.0 !CP, %


      !###########################################
      ! Environmental Conditions
      HRS = 12.0 !Hours perceived sunlight

      ! Perceived Temeprature
      Temp = 24.0 !Indoor temp, C
      RH = 75.0 !Relative Humidity, %
      WS = 1.0 !Average wind speed, kph


      ! Set all rates to 0 before reading files
      kMature = 1/(365.0*2)
      kDry = (1/305.0)
      kFreshening = (1/60.0)
      kMilk = 0.0
      kMortality = 0.07 !as whole number percentage

      ! Set outputs to 0
      Meat = 0
      Milk = 0
      N = 0
      P = 0

      ! Set animal numbers to start
      nCalf = lact_target + born
      born = (lact_target*0.5)/365
      calves_death = nCalf*kMortality/365
      calves_cull = nCalf*kHeiferCull/365
      age_out = (nCalf )*(1/(365.0*2))-calves_death*0.5-calves_cull*0.5
      nHeifer_first_lact = age_out*365
      first_lact_death = nHeifer_first_lact*kMortality/365
      first_lact_cull = nHeifer_first_lact*firstCull/365
      first_age_out = (nHeifer_first_lact/365)-first_lact_death-first_lact_cull

      nHeifer_second_lact = first_age_out*365
      second_lact_death = nHeifer_second_lact*kMortality/365
      second_lact_cull = nHeifer_second_lact*secondCull/365
      second_age_out = (nHeifer_second_lact/365)-second_lact_death-second_lact_cull

      nHeifer_third_lact = second_age_out*365
      third_lact_death = nHeifer_third_lact*kMortality/365
      third_lact_cull = nHeifer_third_lact*thirdCull/365
      third_age_out = (nHeifer_third_lact/365)-third_lact_death-third_lact_cull

      nLact = third_age_out*365
      fourth_lact_death = nLact*kMortality/365
      fourth_lact_cull = nLact*fourthCull/365
      fourth_age_out = (nLact/365)-fourth_lact_death-fourth_lact_cull

            ! update animal numbers
      nHeifer_first_dry = nHeifer_first_lact * (60/365.0)
      nHeifer_second_dry = nHeifer_second_lact * (60/365.0)
      nHeifer_third_dry = nHeifer_third_lact * (60/365.0)
      nHeifer_first_lact = nHeifer_first_lact * (305/365.0)
      nHeifer_second_lact = nHeifer_second_lact * (305/365.0)
      nHeifer_third_lact = nHeifer_third_lact * (305/365.0)
      nDry = nLact * (60/365.0)
      nLact = nLact * (305/365.0)


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
   !    born = ((kFreshening*nDry*0.5)+(kFreshening*nHeifer_first_dry*0.5)+ &
   ! &           (kMature*nCalf*0.5)+ &
   ! &  (kFreshening*nHeifer_second_dry*0.5)+(kFreshening*nHeifer_third_dry*0.5))*((1-kMortality)/100) 
   !    new_lact_herd = (kFreshening*nDry)*(100.0-kMortality)/100.0
   !    new_first_lact = kMature*nCalf*(100.0-kMortality)/100.0
   !    new_second_lact = kFreshening*nHeifer_first_dry*(100.0-kMortality)/100.0
   !    new_third_lact = kFreshening*nHeifer_second_dry*(100.0-kMortality)/100.0
   !    new_first_dry = kDry*nHeifer_first_lact*(100.0-kMortality)/100.0
   !    new_second_dry = kDry*nHeifer_second_lact*(100.0-kMortality)/100.0
   !    new_third_dry = kDry*nHeifer_third_lact*(100.0-kMortality)/100.0
   !    new_dry_herd = kDry*nLact*(100.0-kMortality)/100.0
   !    new_lact_from_third = kFreshening*nHeifer_third_dry*(100.0-kMortality)/100.0
   !    culling = (kCull/100.0*nLact)/365.0
      

      !wt updating
      wtLact = dairy_ADG(2500)
      wtDry = dairy_ADG(2500)
      wtCalf = dairy_ADG(365)
      wtHeifer_first_lact = dairy_ADG(365*2)
      wtHeifer_second_lact = dairy_ADG(365*3)
      wtHeifer_third_lact = dairy_ADG(365*4)
      wtHeifer_first_dry = dairy_ADG(365*2+305)
      wtHeifer_second_dry = dairy_ADG(365*3+305)
      wtHeifer_third_dry = dairy_ADG(365*4+305)
      
      ! cow DMIs
      lact_DMI = lactDMI(wtLact)
      calf_DMI = calfDMI(wtCalf)
      heifer_first_lact_DMI = heiferDMI(wtHeifer_first_lact)
      heifer_second_lact_DMI = heiferDMI(wtHeifer_second_lact)
      heifer_third_lact_DMI = heiferDMI(wtHeifer_third_lact)
      heifer_first_dry_DMI = heiferDMI(wtHeifer_first_dry)
      heifer_second_dry_DMI = heiferDMI(wtHeifer_second_dry)
      heifer_third_dry_DMI = heiferDMI(wtHeifer_third_dry)
      dry_DMI = dryDMI(wtDry)

      !N and P updates
      lact_N = lactNexc(lact_DMI)
      dry_N = dryNexc(dry_DMI)
      calf_N = calfNexc(calf_DMI)
      heifer_first_lact_N = heiferNexc(heifer_first_lact_DMI)
      heifer_second_lact_N = heiferNexc(heifer_second_lact_DMI)
      heifer_third_lact_N = heiferNexc(heifer_third_lact_DMI)
      heifer_first_dry_N = heiferNexc(heifer_first_dry_DMI)
      heifer_second_dry_N = heiferNexc(heifer_second_dry_DMI)
      heifer_third_dry_N = heiferNexc(heifer_third_dry_DMI)
      lact_P = cowPexc(lact_DMI)
      dry_P = dryPexc(dry_DMI)
      calf_P = calfPexc(calf_DMI)
      heifer_first_lact_P = cowPexc(heifer_first_lact_DMI)
      heifer_second_lact_P = cowPexc(heifer_second_lact_DMI)
      heifer_third_lact_P = cowPexc(heifer_third_lact_DMI)
      heifer_first_dry_P = cowPexc(heifer_first_dry_DMI)
      heifer_second_dry_P = cowPexc(heifer_second_dry_DMI)
      heifer_third_dry_P = cowPexc(heifer_third_dry_DMI)

      total_N = lact_N+dry_N+calf_N+heifer_first_lact_N+heifer_second_lact_N+heifer_third_lact_N+heifer_first_dry_N+heifer_second_dry_N+heifer_third_dry_N
      total_N_P = lact_P+dry_P+calf_P+heifer_first_lact_P+heifer_second_lact_P+heifer_third_lact_P+heifer_first_dry_P+heifer_second_dry_P+heifer_third_dry_P
      !eghball 2002 
      Pmin = total_P * 0.75
      Porg = total_P * 0.25
      ! Van Kessel 2002 
      Nmin = total_N * 0.4
      Norg = total_N * 0.6
      ! update animal numbers
      ! nCalf = nCalf + born - new_first_lact + 1
      ! nHeifer_first_dry = nHeifer_first_dry + new_first_dry - new_second_lact
      ! nHeifer_second_dry = nHeifer_second_dry + new_second_dry - new_third_lact
      ! nHeifer_third_dry = nHeifer_third_dry + new_third_dry - new_lact_from_third
      ! nHeifer_first_lact = nHeifer_first_lact + new_first_lact - new_first_dry
      ! nHeifer_second_lact = nHeifer_second_lact + new_second_lact - new_second_dry
      ! nHeifer_third_lact = nHeifer_third_lact + new_third_lact - new_third_dry
      ! nLact = nLact + new_lact_herd - culling - new_dry_herd + new_lact_from_third
      ! nDry = nDry + new_dry_herd - new_lact_herd
      ! if (nCalf .lt. 0) then
      !   nCalf = 0
      ! endif 

      meat_produced = culling*(wtLact)



      !print *,nDay, Porg,Nmin,Pmin,cumManure,manureStore
        print *, Nmin, Norg, Pmin, Porg, 
      end do
     ! print *,nDay, Porg,Nmin,Pmin,cumManure,manureStore
       !!nDay = nDay + 1
!             if (nBroilers .gt. 0) wtBroilers=(wtBroilers+ADG*nBroilers)
!             if (nChicks .gt. 0) wtChicks = (wtChicks + ADG*nChicks)


      ! subroutines 
      contains
            function dairy_ADG(t) result(BW)
                implicit none
                integer, intent(in) :: t
                real :: BW, A, k, b, M
                A = 619 !asymptotic weight, kg
                k = 0.0020 !Rate parameter
                b = 0.905 ! integration constant
                M = 1.2386 ! inflection parameter
                BW = A*(1-(b*exp(-k*t)))**M
                return 
            end function dairy_ADG

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

            ! Nitrogen Equations per cow

            function heiferNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((heifer_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4) !per cow
                  return
            end function heiferNexc

            function calfNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((calf_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*112.55)
                  return
            end function calfNexc

            function lactNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = (((((lact_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*84.1)+(wtLact*0.196))
                  return
            end function lactNexc

            function dryNexc(DMI) result(Nexc)
                  implicit none
                  real, intent(in) :: DMI
                  real :: Nexc
                  Nexc = ((((dry_CP/100)*(DMI*1000))/(DMI*1000)*DMI)*78.39+51.4)
                  return
            end function dryNexc

            ! Phosphorus Equations

            function calfPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = ((((calf_P/100)*(DMI*1000))/(DMI*1000)*DMI)*622.03)
                  return
            end function calfPexc

            function cowPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = ((DMI*lact_P)-(2*(wtLact)/1000)-0.02743* &
           &            exp(((0.05527-0.000075*DIM)*DIM))- &
           &            0.02743*exp(((0.05527-0.000075*(DIM-1))*(DIM-1)))* &
           &            (1.2+4.635*MW**(0.22)*(wtLact)**(-0.22))*ADG/0.96)
                  return
            end function cowPexc

            function dryPexc(DMI) result(Pexc)
                  real, intent(in) :: DMI
                  real :: Pexc
                  Pexc = (DMI*dry_CP*78.39+51.4)
                  return
            end function dryPexc
      end program dairy_total
