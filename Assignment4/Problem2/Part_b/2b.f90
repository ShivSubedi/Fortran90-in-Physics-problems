

!code provided by Dr. Elster, modified by S. Subedi
include 'gauleg.f90'

program gaulegendre

!This program integrates does the integation of x^3/(e^x-1) from 0 to inf via
! Gauss-Legenrde integration using gauss points 2,4,6,8 in double precision.

  implicit none

      integer :: nxmt,nxms,ng,nsxx,ngp
      integer :: kread, kwrite,ii,jj
      integer :: i,nxmx,nxmx1,nxmx2
      double precision :: pi,sxx,wxx,u,w, exact
      double precision :: xival,dx, sumlag,alf, sumleg,error
      
      

      parameter (nsxx=1000000)
      parameter (ngp=1000000)
 
      parameter (kread=5,kwrite=6)
 
      dimension sxx(nsxx),wxx(nsxx)
      dimension u(ngp),w(ngp)


       pi = acos(0.d0)*2.d0
       exact=(pi**4)/15.d0 !exact value of integration, used in deriving planck blackbody formula
      open(kwrite,file='2bf2.dat',status='unknown')
       write(kwrite,*) '#***************************************************'
      write(kwrite,*) '  #      N       sumlegendre        error'
       write(kwrite,*) '#***************************************************'
      do ii=1, 5
      sumleg=0.d0



	jj=2*ii

        


!      Gauss-legendre integration

 
     call gauleg (0.d0,1.d0,u,w,jj)
 

 
      do i=1,jj
     sxx(i)=u(i)
     wxx(i)=w(i)
     sumleg=sumleg + (((cos(sxx(i)*pi/2.d0))**(-2))*((6.d0*tan(sxx(i)*pi/2.d0))**3)/(exp(6.d0*tan(sxx(i)*pi/2.d0))-1.d0))*wxx(i)
     end do


       sumleg=6.d0*sumleg*pi/2.d0 
	error=abs((sumleg-exact)/exact)
	
	write(kwrite,1001) jj, sumleg, error
	


      end do
    1001 format(i10,1x,2(f16.8))
    
end program gaulegendre

