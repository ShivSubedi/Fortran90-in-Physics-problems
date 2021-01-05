

!code provided by Dr. Elster, modified by S. Subedi

include 'gaulegsp.f90'
program integ1

!This program integrates does the integation of 2/(1+x^2) from -1 to +1 via
! Trapezoidal, Simpson and GAussian Quadrature in single precision.

      	implicit none
      	 integer :: kread, kwrite
      	integer, parameter ::nsxx =10000000
         integer,parameter:: ngp=10000000
         integer,parameter:: n=1500 !Taking 1000 no of step points/gauss points
      	
      	
      	integer :: i,jj,nxmx,nxmx1,nxmx2,nxmt,ng
      	real :: pi,dx,sum,d43,d23
        real:: sxx,wxx,u,w,errortrap, errorsimp,errorgau,sumtrap,sumsimp,sumgau,step
        
	dimension sxx(nsxx),wxx(nsxx)
        dimension u(n),w(n),errortrap(n), errorsimp(n),errorgau(n),sumtrap(n),sumsimp(n),sumgau(n),step(n)
        parameter (pi=3.14159265)
        parameter (kread=5,kwrite=6)

        open(kwrite, file ='q1d.dat', status = 'unknown')

     
        write(kwrite,*)'#*******************************************************************************************'
	write(kwrite,*)'#N     h       T-Rule.      S-Rule        G-Quad    Error_trap     Error_simp   Error_gau'
	write(kwrite,*)'#*******************************************************************************************'

      	

       do jj= 10, n,10
		

!************************************************************************************
!       Integration via Trapezoidal rule
!************************************************************************************

	nxmt=jj
	nxmx=nxmt/2
	nxmx=nxmx*2+1
			 
	dx=2.0/float(nxmx-1)
			 
	wxx(1)=dx*0.5
	wxx(nxmx)=dx*0.5

	sxx(1)=-1.0
	sxx(nxmx)=1.0
			 
	nxmx1=nxmx-1
 
      	do  i=2,nxmx1
      		sxx(i)=sxx(1) + float(i-1)*dx
      	    wxx(i)=dx
	end do


      	!sumtrap=0.0d0
         sum=0.0

      	do  i=1,nxmx
          sum=sum + ( 2.0/( 1.0 +sxx(i)*sxx(i) ) )*wxx(i)
	end do

	
	sumtrap(jj)=sum	
	errortrap(jj) =abs((sumtrap(jj) - pi)/pi ) 

!**********************************************************************************************
!	Integration via Simpson Rule
 !**************************************************************************************************
	     
     
              d43=4.0/3.0
	      d23=2.0/3.0

	      wxx(1)=dx/3.0
	      wxx(nxmx)=dx/3.0
	 
	      nxmx1=nxmx-1
	      nxmx2=nxmx-2
 
      do  i=2,nxmx1,2
      	      sxx(i)=sxx(1) + float(i-1)*dx
              wxx(i)=d43*dx
      end do
 
      do  i=3,nxmx2,2
              sxx(i)=sxx(1) + float(i-1)*dx
              wxx(i)=d23*dx
      end do

!       integrate

      sum=0.0d0
      do  i=1,nxmx
             sum=sum + ( 2.0/( 1.0 +sxx(i)*sxx(i) ) )*wxx(i)
      end do

	        sumsimp(jj)=sum
		errorsimp(jj) =abs((sumsimp(jj) - pi )/pi ) 


!*************************************************************************************
!Integration via Gaussian Quadrature
!****************************************************************************************

	ng= nxmt

     call gauleg (-1.0,1.0,u,w,ng)
 
 
 
      do i=1,ng
      sxx(i)=u(i)
      wxx(i)=w(i)
      end do
!
!
!       integrate
!
      sum=0.0
      do  i=1,ng
      sum=sum + ( 2.0/( 1.0 +sxx(i)*sxx(i) ) )*wxx(i)
      end do

	sumgau(jj)=sum	
        errorgau(jj) =abs( ( sumgau(jj) - pi )/pi ) 

	step(jj)=dx

	end do 


	do jj=10,n,10
	
	write(kwrite,10003) jj, step(jj),sumtrap(jj),sumsimp(jj),sumgau(jj), errortrap(jj),errorsimp(jj),errorgau(jj)
	
	end do
 
 
       10003   format (i7,2x,f7.4, 2x, 6(f12.8))     
 

 
      stop 
      end
