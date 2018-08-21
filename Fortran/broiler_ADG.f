      function broiler_ADG(Temp) result(ADG)
         implicit none
         real, intent(in) :: Temp
         real :: ADG
         ADG = Temp**2
      end function broiler_ADG

      program dude 
         implicit none
         real   :: Temp
         real   :: adgg
         real   :: broiler_ADG
         Temp = 500
         adgg = broiler_ADG(Temp)
         print*, adgg 
      end program dude
