

  !   Code provided by Dr. elster, modified by S.Subedi      
      include 'f1ofx.f90'
      program plotf1ofx
       
!
!     plot function given in as f1ofx
!
      implicit none

      double precision :: x, deltax, xa, xb
      double precision :: f1ofx
      double precision :: xx(200), fxx(200)
      double precision :: dnumber
      integer :: i

      integer :: kread, kwrite

      data kread/5/, kwrite/6/
!
      deltax = 0.05d0 
      xa = -0.95d0
      xb = 0.95d0

      xx(0) = xa
      fxx(0) = f1ofx(xa)

      open (kwrite, File = 'f1ofx_data.dat2', Status = 'Unknown')

      do i=1,38
        xx(i) = xa + i * deltax
        fxx(i) = f1ofx(xx(i))
      end do

     
      do i=0,38
        write (kwrite,10000) xx(i),fxx(i)
      end do

      close(kwrite)

10000 format (1x,f6.3,2x,d14.6)
  
      stop 'data saved in f1ofx_data2.dat'
      end 
