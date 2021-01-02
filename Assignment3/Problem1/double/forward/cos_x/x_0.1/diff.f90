     

!code provided by Dr. Elster, modified by S.Subedi

 program diff

!        calculate forward differentiation of given function

      implicit none



      double precision :: fcos, h, h2, hd4, x, xmin, xmax, xstep,ii,ana,error,ploep
      double precision :: result(1)
      integer :: kwrite,ploh
      

      data kwrite/6/

      open(kwrite, File = 'diff.dat', Status = 'Unknown')
      
      x=.1d0
      ana=-0.099983341
     

!        forward
      write (kwrite,*) 'DERIVATIVE VIA FORWARD DIFFERENTIATION METHOD FOR SINGLE PRECISION'
      write(kwrite,*) 'For x=0.1'
      write (kwrite,*) 'Taken analytical value: cos''(0.1) = -0.099983341 (Wolfram alpha)'
      write (kwrite,*)
      write (kwrite,*) '************************************************************************'
      write (kwrite,*) '          h            cos''(.1)     error   -log10(h)   log10(error)'
      write (kwrite,*) '*************************************************************************'
      do ii = 20,1,-1
      h2 = 1E-20*10**ii
      
      result(1) = (fcos(x+h2) - fcos(x))/h2
      error=abs((ana-result(1))/ana)*100
      
     ploh=-log10(h2)
     ploep=log10(error)

      write (kwrite,10000) h2, result(1),error,ploh,ploep
      end do

10000 format(F16.9,2x,f16.9,3x,f10.2,4x,i3,6x,f5.2) 

      close(kwrite)

      stop 'data saved in diffplot.out'
      
      end program diff
!--------------------------------------------------------------------------

    double precision function fcos(x)
     !function fcos(x)

      implicit none

   double precision ::  x, fcos
     !real ::  x, fcos

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
