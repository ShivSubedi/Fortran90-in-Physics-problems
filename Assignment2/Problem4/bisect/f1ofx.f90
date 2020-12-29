      double precision function f1ofx (x)
!
!       function cos x - x
!
      implicit none

      double precision x
      !double precision f1ofx

      f1ofx = x*x - 7.d0*x -log(x)
     !f1ofx = x - 2
      return  
      end 
