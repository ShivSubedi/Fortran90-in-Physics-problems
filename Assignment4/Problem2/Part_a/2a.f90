


!code provided by Dr. Elster, modified by S. Subedi
include 'gaulag.f90'
program gaulaguerre

!This program integrates does the integation of x^3/(e^x-1) from 0 to inf via
! Gauss-Laguerre integration using gauss points 2,4,6,8 in double precision.

  implicit none

      integer :: nxmt,nxms,ng,nsxx,ngp
      integer :: kread, kwrite,ii,jj,pp
      integer :: i,nxmx,nxmx1,nxmx2
      double precision :: pi,sxx,wxx,u,w
      double precision :: xival,dx, sumlagu,alf, sumleg,exact,error,plo1,plo2
      
      

      parameter (nsxx=1000000)
      parameter (ngp=1000000)
 
      parameter (kread=5,kwrite=6)
 
      dimension sxx(nsxx),wxx(nsxx)
      dimension u(ngp),w(ngp)


       pi = acos(0.d0)*2.d0
      exact=(pi**4)/15.d0 !exact value of integration, used in deriving planck blackbody formula
      xival=1.d0 
      alf = 3.d0 !shown in the write-up how it is evaluated
       
      open(kwrite,file='2aplot.dat',status='unknown')
     ! write(kwrite,*) '***************************************************'
      !write(kwrite,*) '        N       sumlaguerre        error'
      ! write(kwrite,*) '***************************************************'
       
      do ii=1, 5
       sumlagu=0.d0

!      Gauss-lagueree integration

	jj=2*ii !taking into account of 5 different points 2,4,6,8,10

        call gaulag(u,w,jj,alf)

       
	do i=1,jj
        
	sxx(i)=u(i)
	wxx(i)=w(i)
        sumlagu=sumlagu + ((exp(sxx(i)))/(exp(sxx(i))-1))*wxx(i) !equivalent to g(x)*wxx(i) as shown in write up
        error=abs((sumlagu-exact)/exact)
        
        plo2= Log10(error)
	end do
       
	
	write(kwrite,1001) jj,sumlagu,error
        end do
        1001 format(i10,1x,2(f16.8))

        end 
