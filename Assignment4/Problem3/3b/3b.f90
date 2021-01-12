include 'gauleg.f90'

program integ1

!       

      	implicit none
         integer :: kread, kwrite
      	integer, parameter ::nsxx =10000000
         integer,parameter:: ngp=10000000
         integer,parameter:: n=500
      	  double precision, parameter :: a=5.0d0
           double precision, parameter:: sigma=2.1d0
           double precision, parameter:: final=20.0d0
      	
      	integer :: i, j,kk, nxmt, ng
      	double precision ::pi,d43,d23, isum
	double complex :: sum 

         double precision:: sxx,wxx,u,w,error
        double complex, dimension(n) :: fsum
	dimension sxx(nsxx),wxx(nsxx)
         dimension u(n),w(n),fsum(n),error(n)

	parameter (pi=3.141592654d0)
        parameter (kread=5,kwrite=6)
        open(kwrite, file ='q3b.dat', status = 'unknown')
        write(kwrite,*)'#************************************************************************************'
	write(kwrite,*)'#       N             Real_part                      Img_part               Error'
	write(kwrite,*)'#*************************************************************************************'



	kk =0
	do j= 5, n,5
		
       call gauleg (-1.0d0,1.0d0,u,w,j)
 
      do i=1,j
      sxx(i)=a*( (1.0d0 +u(i) )/( 1.0d0 - u(i) ) )
      wxx(i)=( (2.0d0*a)/(1.0d0-u(i))**2 )*w(i)
      end do
!
       sum=0.d0
      do  i=1,j
	isum = (( exp(-2.0d0*sxx(i)) -exp(-2.0d0*sigma) )/(sxx(i)*sxx(i)-sigma*sigma))*wxx(i)
        
	sum=sum + isum
      end do


	isum = ( pi/(2.0d0*sigma) )*( exp(-2.0d0*sigma) )
	sum = sum - dcmplx(0.d0, isum)


        isum = ( (exp(-2.0d0*sigma))/(2.0d0*sigma) )*log((1.d0+sigma/final)/(1.d0-sigma/final))
	sum = sum + dcmplx(isum, 0.d0)

        fsum(j)=sum

	if(kk .ne. 0)then
	
        error(j) =abs( ( fsum(j) - fsum(kk) )/fsum(j) ) 
	endif

	kk = j
	

	end do 

	do j=5,n,5
	
	write(kwrite,1000) J, dreal(fsum(j)), dimag(fsum(j)), error(j)
	
	end do
 
 
 1000   format (i10,4x, 4(f25.16))     
 

 
      stop 
      end
