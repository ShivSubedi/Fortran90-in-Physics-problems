  
   !code written by S.Subedi as suggested by TA
   program bessel

   implicit none

   integer::kread,kwrite,ii,jj,kk
   double precision::jasc(0:25),jdes(0:51),ratio, rescale, error
   ! the role of the ratio parameter here is to rescale the values of the bessel function
   ! ratio can be used as the bessel function possess linearity
   real::x
   !Need to assign three values of x = 0.1, 1.0, 10'
   x=0.1d0
   data kread/5/,kwrite/6/
   open (kwrite, file="bessel.dat",status="Unknown")

   write (kwrite,*) 'NB: Here we are finding the values of first 25l values of spherical bessel'
   write (kwrite,*)'     function (j(x)) for   x=0.1,1.0,10'

   do kk=1,3
    
   
       write (kwrite,*)
       write (kwrite,*)
       write(kwrite,*)'When x=',x
       write (kwrite,*) '*************************************************************************'
       write(kwrite,*)"l          jup(x)                jdown(x)            error "
       write (kwrite,*) '*************************************************************************'
 
   
        jasc(0)=(sin(x))/(x)
        jasc(1)=(sin(x)-(x*cos(x)))/(x**2)
 
       do ii=1,24
                    jasc(ii+1)=((2*ii+1)*jasc(ii))/dfloat(x)-jasc(ii-1)

       end do

        jdes(50)=2.0 !arbitrary value taken
        jdes(49)=1.0 !arbitrary value taken


        do jj=50,1,-1
                     jdes(jj-1)=((2*jj+1))*jdes(jj)/dfloat(x)-jdes(jj+1)
        end do
 
        ratio=jasc(0)/jdes(0)

        do ii=0,25
                    rescale = jdes(ii)*ratio
                    error=(abs(jasc(ii)-jdes(ii)*ratio)/(abs(jasc(ii))+abs(jdes(ii)*ratio))) 
                    write (kwrite,996)ii,jasc(ii),rescale, error
        end do

         x=x*10.0d0
   
   end do

   996 format(i3,5x,d16.6,5x,d16.6,5x,d16.6)
   stop
   end program bessel

      
   
