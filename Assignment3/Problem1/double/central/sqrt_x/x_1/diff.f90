      
!code provided by Dr. Elster, modified by S.Subedi

program diff

!        calculate central differentiation of given function

      implicit none

!      double precision :: fcos, h, hd2, hd4, x, xmin, xmax, xstep
!      double precision :: result(3)

      double precision :: fsq, h, h2, hd4, x, xmin, xmax, xstep,ii,ana,error,ploep
      double precision :: result(1)
      integer :: kwrite,ploh

      data kwrite/6/

      open(kwrite, File = 'diffplot.dat', Status = 'Unknown')
      
      x=1
      ana=0.5d0
     

!        forward
      !write (kwrite,*) 'DERIVATIVE OF SQRT(X) VIA CENTRAL DIFFERENTIATION METHOD FOR DOUBLE PRECISION'
      !write(kwrite,*) 'For x=1'
      !write (kwrite,*) 'Taken analytical value: 1/(2*sqrt(1)) = 0.5 (Wolfram alpha)'
      !write (kwrite,*)
      !write (kwrite,*) '************************************************************************'
      !write (kwrite,*) '          h          1/(2*sqrt(1))     error   -log10(h)   log10(error)'
      !write (kwrite,*) '*************************************************************************'
      do ii = 20,1,-1
      h = 1E-20*10**ii
      h2=0.5*h
      
      result(1) = (fsq(x+h2) - fsq(x-h2))/h
      error=abs((ana-result(1))/ana)*100
!        !central
      !result(2) = (fcos(x+hd2) - fcos(x-hd2))/h
!        extrapolated
      !result(3) = (8.d0 *(fcos(x+hd4)-fcos(x-hd4)) - &
     !&                 (fcos(x+hd2)-fcos(x-hd2)))/(3.d0*h)
     ploh=-log10(h)
     ploep=log10(error)

      write (kwrite,10000) h, result(1),error,ploh,ploep
      end do

10000 format(F16.9,2x,f16.9,4x,f7.2,5x,i3,6x,f7.2) 

      close(kwrite)

      stop 'data saved in diffplot.out'
      
      end program diff
!--------------------------------------------------------------------------

!      double precision function fcos(x)
     !function fcos(x)

      !implicit none

!      double precision ::  x, fcos
     !real ::  x, fcos

      !fcos = cos(x)

      !return
      !end
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

      !function fsq(x)
      double precision function fsq(x)

      implicit none

    double precision ::  x, fsq
    !  real ::  x, fsq

      fsq = sqrt(x)

      return
      end
