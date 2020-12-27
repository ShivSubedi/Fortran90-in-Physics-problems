! Code provided by Dr. Elster, modified by S. Subedi

      double precision function f1ofx (x)
!
!       function sin x / x
!
      implicit none

      double precision :: x
      double precision :: f1ofx
      double precision :: ep
      ep = 0.001d0   !taking a small value to avoid singluatity at x tends to 0

if (x<=ep)then
f1ofx = 1-(x*x)/6 !taylor expansion taking two terms to avoid singularity
else
f1ofx = sin (x) / x
end if   
      return  
      end 
