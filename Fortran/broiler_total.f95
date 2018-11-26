      program broiler_total
      implicit none



!     ~ ~ ~ PURPOSE ~ ~ ~
!     this subroutine computes the lake hydrologic pesticide balance.
!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!     variable          |definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     nDay              | day in cycle (1-full cycle)
!     nChicks           | number of chicks, less than 1/2 of life
!     nBroilers         | number of birds, older than 1/2 of life
!     wtChicks          | weight of chicks
!     wtBroilers        | weight of mature birds
!     ME_young          | Energy of diet, kcal / kg
!     ME_mature         | Energy of diet, kcal / kg
!     CPy               | Crude protein for growing, %
!     CPo               | Crude protein for finishing, %
!     nP_young          | non-phytate P in diet for growing, %
!     nP_mature         | non-phytate P in diet for finishing, %
!     P_young           | added Phytate in diet for growing, U/kg diet
!     P_mature          | added Phytate in diet for finishing, U/kg diet
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!     variable                | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     nChicks                 | number of chicks, less than 1/2 of life
!     nBroilers               | number of birds, older than 1/2 of life
!     wtChicks                | weight of chicks
!     wtBroilers              | weight of mature birds
!     intake                  | feed intake, total kg
!     manure_out              | manure output, total kg
!     Nmin_frac               | Mineralized N excretion, as frac
!     Norg_frac               | Organic N excretion, as frac
!     Pmin_frac               | Mineralized P excretion, as frac
!     Porg_frac               | Organic P excretion, as frac
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!     name                    | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     kMature                 | rate of maturity
!     kBirth                  | rate of birthing
!     kCull                   | culling off rate
!     kMortality              | martality rate
!     maturing                | how many birds maturing
!     culling                 | how many culling
!     born                    | how many born
!     cumNorg                 | cumulative organic N of excreted N, kg
!     cumNmin                 | cum. mineralized N of excreted N, kg
!     cumPorg                 | cum. organic P of excreted P, kg
!     cumPmin                 | cum. mineralized P of excreted P, kg
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      real :: kMature
      real :: kBirth
      real :: kCull
      real :: kMortality
      real :: maturing 
      real :: culling
      real :: born
      real :: cumNorg
      real :: cumNmin
      real :: cumPorg
      real :: cumPmin
!     added vars
      real :: ADG
      real :: cumManure
      real :: iChicks
      real :: intake
      real :: manure_out
      real :: manureStore
      real :: meat_produced
      real :: Nexc
      real :: Nmin
      real :: Norg
      real :: Nmin_frac
      real :: Norg_frac
      real :: Pexc
      real :: Pmin
      real :: Porg
      real :: Porg_frac
      real :: Pmin_frac
      real :: Temp,DIGIntake
      real :: wt_per_bird


     integer:: nDay,fDay,switch_feed
      real :: nChicks
      real :: nBroilers
      real :: wtChicks
      real :: wtBroilers
      real :: ME_young
      real :: ME_mature
      real :: CPy
      real :: CPo
      real :: nP_young
      real :: nP_mature
      real :: P_young
      real :: P_mature, digest
      !real :: switch_feed

      nDay = 1
      fDay = 44
      digest = 0.7
      iChicks = 100
      nChicks = iChicks
      nBroilers = 0
      wtChicks = iChicks*0.025
      wtBroilers = 0
      ME_young = 3050
      ME_mature = 3180
      CPy = 21.5
      CPo = 19
      nP_young = 0.25
      nP_mature = 0.25
      P_young = 500
      P_mature = 500
      switch_feed = 22

      kMature = 1
      kBirth = 1
      kCull = 1
      kMortality = 5
      maturing = 0 
      culling = 0
      born = 0
      cumNorg = 0
      cumNmin = 0
      cumPorg = 0
      cumPmin = 0

!      added vars
      ADG = 0
      cumManure = 0
      intake = 0
      manure_out = 0
      manureStore = 0
      meat_produced = 0
      Nexc = 0
      Nmin = 0
      Norg = 0
      Nmin_frac = 0
      Norg_frac = 0
      Pexc = 0
      Pmin = 0
      Porg = 0
      Porg_frac = 0
      Pmin_frac = 0
      Temp = 31
      wt_per_bird = 0

      ! put ceiling on temp effects
      if(Temp .le. 23) Temp = 23
      if (Temp .ge. 31) Temp = 31
      
      
     do nDay = 1, fDay
      wt_per_bird =(wtChicks+wtBroilers)/(nChicks+nBroilers)
      ADG = brlADG()
      intake = brlDMI(ADG)
      !cumManure = cumManure + (intake - ADG)
      DIGIntake = intake * digest
      cumManure = cumManure + (intake - DIGIntake)*(nChicks+ nBroilers)
      ! Calc N and P
      Nexc = brlNex()
      call brlN(Nexc, Norg, Nmin)
      Pexc = brlPex()
      call brlP(Pexc,Porg, Pmin)
      if (mod(nDay,(switch_feed*2)) .eq. 0) then
            nChicks = iChicks
            wtChicks = 0.025*nChicks
            culling = nBroilers
            nBroilers = 0
            wtBroilers = 0
      else if (mod(nDay,(switch_feed)) .eq. 0) then
            nBroilers = nChicks
            wtBroilers = wtChicks
            nChicks = 0
            wtChicks = 0
      else
            if (nBroilers .gt. 0) wtBroilers= &
     &       (wtBroilers+ADG*nBroilers)

            if (nChicks .gt. 0) wtChicks = (wtChicks + &
     &       ADG*nChicks)

      end if

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
            function brlADG() result(ADG)
               implicit none
      real :: ADG
            ADG = (-31.797 + (1.2071*Temp) + &
     &            (0.21457*(wt_per_bird*1000.0)) - &
     &         (8.852E-5*(wt_per_bird*1000.0)**2) &
     &         + (1.51E-8*(wt_per_bird*1000.0)**3) &
     &         - (2.0772E-3*Temp*(wt_per_bird*1000.0)))
            ADG = ADG/1000.0
      return
            end function brlADG

      !Zuidhof et al 2014
      function brlDMI(ADGg) result(DMI)
            implicit none
            real, intent(in) :: ADGg
            real :: DMI, intake_ME, fed_ME
            intake_ME = (196.0*(wt_per_bird**0.7543))+2.4833*ADGg*1000
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

      subroutine brlN(Nexc, Norg, Nmin)
            real :: Nexc, Nvol, Nmin, Norg
            Nvol = ((0.362+0.116+0.002)*Nexc)
            Nmin = ((Nexc-Nvol)*0.17)
            Norg = (Nexc-Nvol)-Nmin
      end subroutine brlN

      function brlPex() result(Pexc)
            real :: Pexc, FI, P, nP
            if (nChicks .gt. 0) then
                  P = P_young
                  nP = nP_young
            else 
                  P = P_mature
                  nP = nP_mature
            end if
            FI = brlDMI(ADG)
            !Kornegay et al. 1996
            Pexc = exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  &
     &           ((0.4088*log(nP))**2)+((-0.0087*log(P))**2)+ &
     &           (0.0012*log(P)*log(nP)))
            Pexc = FI*Pexc*(nBroilers + nChicks)/1000
            return
      end function brlPex
      
      !Eghball 2002
      subroutine brlP(Pexc, Porg, Pmin)
            real :: Pexc, Porg, Pmin
            Pmin = (0.9*Pexc)
            Porg = ((Pexc-Pmin))
      end subroutine brlP      


      end program broiler_total
