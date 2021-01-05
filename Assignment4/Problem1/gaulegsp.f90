      subroutine gauleg (x1,x2,x,w,n) 
!
!        calculate gauss points for gauss-legendre integration
!                 (numerical recipes)  
!
      implicit real (a-h,o-z) 
!
      parameter (eps=3.d-6) 
      dimension x(n),w(n)
!
      m=(n+1)/2 
      xm=0.5*(x2+x1) 
      xl=0.5*(x2-x1) 

      do 12 i=1,m 
        z=cos(3.14159265*(i-.25)/(n+.5)) 
   1    continue 
          p1=1.0 
          p2=0.0 
          do 11 j=1,n 
            p3=p2 
            p2=p1 
            p1=((2.0*j-1.0)*z*p2-(j-1.0)*p3)/j 
  11   continue 
       pp=n*(z*p1-p2)/(z*z-1.0) 
       z1=z 
       z=z1-p1/pp 

       if (abs(z-z1).gt.eps) go to 1 

       x(i)=xm-xl*z 
       x(n+1-i)=xm+xl*z 
       w(i)=2.0*xl/((1.0-z*z)*pp*pp) 
       w(n+1-i)=w(i) 
  12  continue 

      return 
      end 
