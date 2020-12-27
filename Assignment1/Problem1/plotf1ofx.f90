
 !Data file path ~/New/sample/hw2/prob1/f1ofx_data.dat
  ! Code provided by Dr. elster, modified by S.Subedi for the function sinx/x       
     include 'f1ofx.f90'
     program plotf1ofx
       
!
!     plot function given in as f1ofx
!
      implicit none

      double precision :: x, deltax, xa, xb
      double precision :: f1ofx
      double precision :: xx(0: 200), fxx(0:200) !starts with 0 value
      double precision :: dnumber
      integer :: i

      integer :: kread, kwrite

      data kread/5/, kwrite/6/
!
      deltax = 0.1d0 
      xa = 0.0d0
      xb = 10.0d0

      xx(0) = xa ! calls value of xx at 0 as well
      fxx(0) = f1ofx(xa)

      open (kwrite, File = 'f1ofx_data.dat', Status = 'Unknown')

      do i=1,100 !taking 100 looping points in interval 0 to 10
        xx(i) = xa + i * deltax
        fxx(i) = f1ofx(xx(i))
      end do

     !write (kwrite,*) 'plot output from f1ofx, with 20 points.'
      do i=0,100
        write (kwrite,10000) xx(i),fxx(i)
      end do

      close(kwrite)

10000 format (1x,f6.3,2x,d14.6)
  
      stop 'data saved in f1ofx_data.dat'
      end 
