      
!code provided by Dr. Elster, modified by S.Subedi
program diff

!        calculate second differentiation of given function

      implicit none



      real :: fcos, h, h2, hd4, x, xmin, xmax, xstep,ii,ana,error,ploep,pi
      real :: result(1)
      integer :: kwrite,ploh

      data kwrite/6/

      open(kwrite, File = 'diff.dat', Status = 'Unknown')
      
      x=.1
      ana=-0.995004
      pi = acos(0.d0)*2
      h=pi/10.

      write (kwrite,*) 'SECOND ORDER DERIVATIVE FOR SINGLE PRECISION VIA THREE POINT FORMULA'
      write(kwrite,*) 'For x=0.1'
      write (kwrite,*) 'Taken analytical value: cos''''(0.1) =- 0.995004 (Wolfram alpha)'
      write (kwrite,*)
      write (kwrite,*) '***********************************************************************'
      write (kwrite,*) '          h            cos''''(1)     error   -log10(h)   log10(error)'
      write (kwrite,*) '************************************************************************'
      do ii = 1,100
      
      
       result(1) = (fcos(x-h)+fcos(x+h))/(h**2) - (2*fcos(x))/(h**2)
      error=abs((ana-result(1))/ana)*100
!       
     ploh=-log10(h)
     ploep=log10(error)
      
     

      write (kwrite,10000) h, result(1),error,ploh,ploep
    
     h=h*.90
      end do

10000 format(F16.9,2x,f16.9,2x,f8.2,2x,i3,6x,f5.2) 

      close(kwrite)

      stop 'data saved in diffplot.dat'
      
      end program diff
!--------------------------------------------------------------------------

!      double precision function fcos(x)
     function fcos(x)

      implicit none

!      double precision ::  x, fcos
     real ::  x, fcos

      fcos = cos(x)

      return
      end
!--------------------------------------------------------------------------

!      double precision function fexp(x)
     ! function fexp(x)

      !implicit none

!      double precision ::  x, fexp
      !real ::  x, fexp

      !fexp = exp(x)

      !return
      !end
!--------------------------------------------------------------------------

!      double precision function fsq(x)
      !function fsq(x)

      !implicit none

!      double precision ::  x, fsq
      !real ::  x, fsq

      !fsq = sqrt(x)

     ! return
      !end
