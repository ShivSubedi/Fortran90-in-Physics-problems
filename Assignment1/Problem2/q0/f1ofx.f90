
      ! Code provided by Dr. Elster, modified by S.Subedi
      function f1ofx (x) !just assign function, double precision is assigned in
      
                          !in the main function

!
      implicit none

      double precision :: x
      double precision :: f1ofx
      
      
      f1ofx = 0.5d0*(log((1+x)/(1-x)))
      return  
      end 
