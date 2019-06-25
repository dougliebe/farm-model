      program layer_total
      implicit none


!     ~ ~ ~ PURPOSE ~ ~ ~
!     this subroutine computes the lake hydrologic pesticide balance.
!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!     variable          |definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!     nDay              | day in cycle (1 full cycle)

!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!     variable                | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!     name                    | definition
!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      real :: iPullets_brown
      real :: iLaying_hens_brown
      real :: iBreed_hens_brown
      real :: iMolt_hens_brown
      real :: iPullets_white
      real :: iLaying_hens_white
      real :: iBreed_hens_white
      real :: iMolt_hens_white

      integer :: day_since_new_pullets
      real :: Pmolt
      integer :: nDay
      integer :: fDay

      real :: CP_white
      real :: CP_brown
      real :: Temp
      real :: kMature
      real :: kSpent_Hens
      real :: kMolt
      real :: kBreed_eggs
      real :: kLaying_eggs
      real :: kLaying_spent
      real :: kMortality
      real :: kVolitilization
      real :: kBirth 
      real :: kCull 


      real :: nChicks_brown
      real :: nPullets_brown
      real :: nLaying_hens_brown
      real :: nBreed_hens_brown
      real :: nBreed_males_brown
      real :: nMolt_hens_brown
      real :: nChicks_white
      real :: nPullets_white
      real :: nLaying_hens_white
      real :: nBreed_hens_white
      real :: nBreed_males_white
      real :: nMolt_hens_white

      real :: Eggs
      real :: N
      real :: P
      real :: meat_produced
      real :: Meat
      integer :: molt

      real :: new_pullets
      real :: new_layers
      real :: culling
      real :: molting
      integer :: t
      

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

      nDay = 1
      fDay = 10

      iPullets_brown = 100
      iLaying_hens_brown = 0.0
      iBreed_hens_brown = 0.0
      iMolt_hens_brown = 0.0
      iPullets_white = 0.0
      iLaying_hens_white = 0.0
      iBreed_hens_white = 0.0
      iMolt_hens_white = 0.0

      day_since_new_pullets = 0 
      Pmolt = 0.4

      CP_white = 16.5
      CP_brown = 17.0
      Temp = 25 
      kSpent_Hens = 0.0
      kMolt = 0.0
      kBreed_eggs = 0.0
      kLaying_eggs = 0.0
      kLaying_spent = 0.0
      kMortality = 0.0
      kVolitilization = 0.0

      nChicks_brown = 0.0
      nPullets_brown = 0.0
      nLaying_hens_brown = 0.0
      nBreed_hens_brown = 0.0
      nBreed_males_brown = 0.0
      nMolt_hens_brown = 0.0
      nChicks_white = 0.0
      nPullets_white = 0.0
      nLaying_hens_white = 0.0
      nBreed_hens_white = 0.0
      nBreed_males_white = 0.0
      nMolt_hens_white = 0.0

      Eggs = 0.0
      N = 0.0
      P = 0.0
      Meat = 0.0


      nPullets_brown = iPullets_brown
      nLaying_hens_brown = iLaying_hens_brown
      nBreed_hens_brown = iBreed_hens_brown
      nMolt_hens_brown = iMolt_hens_brown
      nPullets_white = iPullets_white
      nLaying_hens_white = iLaying_hens_white
      nBreed_hens_white = iBreed_hens_white
      nMolt_hens_white = iMolt_hens_white
      meat_produced = 0

      !fluxes
      kMature = 1.0 
      kBirth = 1.0 
      kCull = 1.0


      ! put ceiling on temp effects
      if(Temp .le. 23) Temp = 23
      if (Temp .ge. 31) Temp = 31


      

      do   nDay = 1, fDay

            if(day_since_new_pullets == 90*7 .AND. molt == 1) then
                  t = 2
            else if(day_since_new_pullets == 17*7) then
                  t = 1
            else if(nMolt_hens_brown + nMolt_hens_white .gt. 0 .AND. &
     &      day_since_new_pullets == 120*7 .OR. day_since_new_pullets == 90*7) then
                  t = 3
            else 
                  t = 0 
            end if

      ! Fluxes
      new_pullets = (t==3)*(iPullets_brown+iPullets_white)
      new_layers = (t==1)*(nPullets_brown+nPullets_white)*((nPullets_brown+nPullets_white)>0)
      molting = (t==2)*(nLaying_hens_brown+nLaying_hens_white)
      culling = (t==3)*(nLaying_hens_brown+nLaying_hens_white+nMolt_hens_brown+nMolt_hens_white)



      ! Updating Counts
      nLaying_hens_brown = nLaying_hens_brown + (new_layers-culling*(nLaying_hens_brown>0)-molting)*(iPullets_brown > 0)
      nPullets_brown = nPullets_brown + (new_pullets*(iPullets_brown>0)-new_layers*(iPullets_brown>0))
      nBreed_hens_brown = nBreed_hens_brown
      nMolt_hens_brown = nMolt_hens_brown + (molting-culling)*(iPullets_brown > 0)*(t==2 .OR. (t==3)*(nMolt_hens_brown>0))

      nLaying_hens_white = nLaying_hens_white + (new_layers-culling*(nLaying_hens_white>0)-molting)*(iPullets_white > 0)
      nPullets_white = nPullets_white + (new_pullets*(iPullets_white>0) - new_layers*(iPullets_white>0))
      nBreed_hens_white = nBreed_hens_white
      nMolt_hens_white = nMolt_hens_white + (molting-culling)*(iPullets_white > 0)*(t==2 .OR. (t==3)*(nMolt_hens_white>0))
      print *, new_pullets
      end do 

      !       call grand(molt)
      !       ! is_true = r < Pmolt
      !       print *, molt
      ! end do 
      ! molt = grand()
      ! print *, molt


      ! function grand() result(r)
      !       real :: r
      !       call random_seed()
      !       call random_number(r)
      ! end function grand
end program layer_total
