     

!code provided by Dr. Elster, modified by S.Subedi
 program diff

!        calculate extrapolated differentiation of given function

      implicit none

!      double precision :: fcos, h, hd2, hd4, x, xmin, xmax, xstep
!      double precision :: result(3)

      real :: fcos, h, hd2, hd4, x, xmin, xmax, xstep,ii,ana,error,ploep
      real :: result(3)
      integer :: kwrite,ploh

      data kwrite/6/

      open(kwrite, File = 'diff.dat', Status = 'Unknown')
      
      x=30
      ana=0.988032
     


      write (kwrite,*) 'DERIVATIVE VIA EXTRAPOLATED DIFFERENTIATION METHOD FOR SINGLE PRECISION'
      write(kwrite,*) 'For x=30'
      write (kwrite,*) 'Taken analytical value: cos''(30) = 0.988032 (Wolfram alpha)'
      write (kwrite,*)
      write (kwrite,*) '************************************************************************'
      write (kwrite,*) '          h            cos''(30)     error   -log10(h)   log10(error)'
      write (kwrite,*) '*************************************************************************'
      do ii = 10,1,-1
      h = 1E-10*10**ii
      hd2=0.5*h
      hd4=0.25*h

      
      !result(1) = (fcos(x+h2) - fcos(x))/h2
      
!        !central
      !result(2) = (fcos(x+hd2) - fcos(x-hd2))/h
!        extrapolated
      result(3) = (8. *(fcos(x+hd4)-fcos(x-hd4)) - &
     &                 (fcos(x+hd2)-fcos(x-hd2)))/(3.*h)
      error=abs((ana-result(3))/ana)*100

     ploh=-log10(h)
     ploep=log10(error)

      write (kwrite,10000) h, result(3),error,ploh,ploep
      end do

10000 format(F16.9,2x,f16.9,3x,f7.2,2x,i3,8x,f5.2) 

      close(kwrite)

      stop 'data saved in diffplot.out'
      
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
