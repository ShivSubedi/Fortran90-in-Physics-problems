
 ! Code provided by Dr. Elster, modified by S.Subedi
      double precision function f1ofx (x)
!

!
      implicit none

      double precision :: x
      double precision :: f1ofx
      
      

f1ofx = ((5*x*x*x-3*x)/4)*(log((1+x)/(1-x)))-(2.5*x*x) + (2/3)
      return  
      end 
