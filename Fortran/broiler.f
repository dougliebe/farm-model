      subroutine broiler

C     ~ ~ ~ PURPOSE ~ ~ ~
C     this subroutine computes the lake hydrologic pesticide balance.
C     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
C     variable          |definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     nDay              | day in cycle (1-full cycle)
C     nChicks           | number of chicks, less than 1/2 of life
C     nBroilers         | number of birds, older than 1/2 of life
C     wtChicks          | weight of chicks
C     wtBroilers        | weight of mature birds
C     ME_young          | Energy of diet, kcal / kg
C     ME_mature         | Energy of diet, kcal / kg
C     CPy               | Crude protein for growing, %
C     CPo               | Crude protein for finishing, %
C     nP_young          | non-phytate P in diet for growing, %
C     nP_mature         | non-phytate P in diet for finishing, %
C     P_young           | added Phytate in diet for growing, U/kg diet
C     P_mature          | added Phytate in diet for finishing, U/kg diet
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
C     variable                | definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     nChicks                 | number of chicks, less than 1/2 of life
C     nBroilers               | number of birds, older than 1/2 of life
C     wtChicks                | weight of chicks
C     wtBroilers              | weight of mature birds
C     intake                  | feed intake, total kg
C     manure                  | manure output, total kg
C     Nexc                    | Total N excretion, kg
C     Pexc                    | Total P excretion, kg
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
C     name                    | definition
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
C     kMature                 | rate of maturity
C     kBirth                  | rate of birthing
C     kCull                   | culling off rate
C     kMortality              | martality rate
C     maturing                | how many birds maturing
C     culling                 | how many culling
C     born                    | how many born
C     Norg_frac - 26.5%       | organic N fraction of excreted N
C     Ninorg_frac - 25.5%     | inorganic N fraction of excreted N
C     Porg_frac - 10%         | organic P fraction of excreted P
C     Pinorg_frac - 90%       | inorganic P fraction of excreted P
C     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

C     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      real :: kMature
      real :: kBirth
      real :: kCull
      real :: kMortality
      real :: maturing 
      real :: culling
      real :: born
      real :: Norg_frac
      real :: Ninorg_frac
      real :: Porg_frac
      real :: Pinorg_frac

      kMature = 1
      kBirth = 1
      kCull = 1
      kMortality = 5
      maturing = 0 
      culling = 0
      born = 0
      Norg_frac = 0.265
      Ninorg_frac = 0.255
      Porg_frac = 0.10
      Pinorg_frac = 0.90

      ! put ceiling on temp effects
      if(Temp .le. 23) Temp = 23
      if (Temp .ge. 31) Temp = 31
      
      


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
     &       (wtBroilers+ADG*nBroilers)
            if (nChicks .gt. 0) wtChicks = (wtChicks +
     &       ADG*nChicks)
            t = 0
      end if

      ! calculations of outputs
      wt_per_bird=(wtChicks+wtBroilers)/(nChicks+nBroilers)
      ADG = brlADG()
      intake = brlDMI(ADG)
      manure = manure + (intake - ADG)
C       Nexc = brlNex()
C       call brlN(Nexc, Norg, Nmin)
C       Pexc = brlPex()
C       call brlP(Pexc,Porg, Pmin)

      !update animal numbers
      if (nBroilers .gt. 0) nBroilers = nBroilers-(kMortality)
      if (nChicks .gt. 0) nChicks = nChicks-(kMortality)
      
      !wt updating
      meat_produced = culling*(wtBroilers)

      if (mod(nDay,(switch_feed*2)) .eq. 0) then
            manure_out = manure
      else
            manure_out = 0

C             if (nBroilers .gt. 0) wtBroilers=(wtBroilers+ADG*nBroilers)
C             if (nChicks .gt. 0) wtChicks = (wtChicks + ADG*nChicks)


      nDay = nDay + 1

      end do

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

C       function brlNex() result(Nexc)
C             implicit none
C             real :: Nexc, CP, FI, Nintake, Nret
C             if (nChicks .gt. 0) then
C                   CP = CPy
C             else 
C                   CP = CPo
C             end if
C             FI = brlDMI(ADG)
C             if (nBroilers .gt. 0) then
C                   Nintake = FI*1000 * nBroilers * (CP/100)/6.25
C                   Nret = 29 * (ADG*nBroilers) ! Constant 29 g/kg of BW gain according to ITAVI, 2013
C                   Nexc = (Nintake - Nret)/1000 ! Formula from Belloir et al. 2017
C             else 
C                   Nintake = FI * nChicks * (CP/100)/6.25 !N in kg
C                   Nexc = (0.589*Nintake*1000 - 5.004)/1000 !Bregendahl et al. 2002 using N, grams
C             end if      
C             return ! Returns N in kg
C       end function brlNex

C       subroutine brlN(Nexc, Norg, Nmin)
C             real, intent(in) :: Nexc
C             real, intent(out) :: Nvol, Nmin, Norg
C             Nvol = ((0.362+0.116+0.002)*Nexc)
C             Norg = (Nexc-Nvol)-Nmin
C             Nmin = ((Nexc-Nvol)*0.49)
C       end subroutine brlN

C       function brlPex() result(Pexc)
C             real :: Pexc, FI, P, nP
C             if (nChicks .gt. 0) then
C                   P = P_young
C                   nP = nP_young
C             else 
C                   P = P_mature
C                   nP = nP_mature
C             end if
C             FI = brlDMI(ADG)
C             !Kornegay et al. 1996
C             Pexc = exp(1.058+(-0.2100*log(nP))+(-0.0160*log(P))+  
C      &           ((0.4088*log(nP))**2)+((-0.0087*log(P))**2)+
C      &           (0.0012*log(P)*log(nP)))
C             Pexc = FI*Pexc*(nBroilers + nChicks)/1000
C             return
C       end function brlPex
      
C       !Eghball 2002
C       subroutine brlP(Pexc, Porg, Pmin)
C             real, intent(in) :: Pexc
C             real, intent(out) :: Porg, Pmin
C             Pmin = (0.9*Pexc)
C             Porg = ((Pexc-Pmin))
C       end subroutine brlP      


      end