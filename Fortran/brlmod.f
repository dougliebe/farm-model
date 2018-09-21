      program broiler 
      implicit none
C       ! make array for outputs
      real outputs(45)

      real :: ADG, born, CPo, CPy, culling, CumSumWt, fDay, iChicks
      real :: iDay, kBirth, kCull, kLaying, kMature, kMortality
      real :: maturing, ME_mature, ME_young, N, nBroilers, nChicks
      real :: nDay, Nexc, Nmin, Norg, nP_mature, nP_young, Nvol, P 
      real :: P_mature, P_young, Pexc, Porg, Pmin, Pvol, switch_feed
      real :: Temp, wtBroilers, wtChicks, meat_produced
      real :: wt_per_bird, intake
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
            wt_per_bird=(wtChicks+wtBroilers)/(nChicks+nBroilers)
            ADG = brlADG()
            intake = brlDMI(ADG)
            Nexc = brlNex()
            call brlN(Nexc, Norg, Nmin)
            Pexc = brlPex()
            call brlP(Pexc,Porg, Pmin)
            outputs(int(nDay)) = Porg

            !update animal numbers
            if (nBroilers .gt. 0) nBroilers = nBroilers-(kMortality)
            if (nChicks .gt. 0) nChicks = nChicks-(kMortality)
            
            !wt updating
            meat_produced = culling*(wtBroilers)

            

C             if (nBroilers .gt. 0) wtBroilers=(wtBroilers+ADG*nBroilers)
C             if (nChicks .gt. 0) wtChicks = (wtChicks + ADG*nChicks)


      nDay = nDay + 1

      end do
      print *, sum(outputs)
      contains
            function brlADG() result(ADG)
               implicit none
      real :: ADG
            ADG = (-31.797 + (1.2071*Temp) + 
     &            (0.21457*(wt_per_bird*1000.0)) - 
     &         (8.852E-5*(wt_per_bird*1000.0)**2) 
     &         + (1.51E-8*(wt_per_bird*1000.0)**3) 
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
C             real, intent(in) :: Nexc
C             real, intent(out) :: Nvol, Nmin, Norg
            real :: Nexc, Nvol, Nmin, Norg
            Nvol = ((0.362+0.116+0.002)*Nexc)
            Nmin = ((Nexc-Nvol)*0.49)
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
            Pexc = exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  
     &           ((0.4088*log(nP))**2)+((-0.0087*log(P))**2)+
     &           (0.0012*log(P)*log(nP)))
            Pexc = FI*Pexc*(nBroilers + nChicks)/1000
            return
      end function brlPex
      
      !Eghball 2002
      subroutine brlP(Pexc, Porg, Pmin)
C             real, intent(in) :: Pexc
C             real, intent(out) :: Porg, Pmin
            real :: Pexc, Porg, Pmin
            Pmin = (0.9*Pexc)
            Porg = ((Pexc-Pmin))
      end subroutine brlP      

      end program broiler


