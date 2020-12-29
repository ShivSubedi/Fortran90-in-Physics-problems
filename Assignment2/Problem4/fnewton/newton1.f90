      

!code provided by Dr. Elster, modified by S. Subedi

 include 'sec1.f90'
 program newrap
 implicit none		 

 double precision x,f1ofx, comp, pre, xx
 integer ii, kread, kwrite
 data kread/5/, kwrite/6/
 open(kwrite,file='newton.dat',status='Unknown')

 comp = 0.0001
 pre = .01
 xx = 0.01  !for finding first root
 ii=0
 call newton  (pre, ii,comp,xx)

 write(kwrite,*)
 write(kwrite,996)'As can seen from above iteration process, the no of iterations:',ii
 write(kwrite,997)'And first root:',xx
 write(kwrite,*)
   
 xx=7.0d0   !for finding second root
 pre = .001
 ii=0
 call newton  (pre, ii,comp,xx)

 write(kwrite,*)
 write(kwrite,*)
 write(kwrite,996)'As can seen from above iteration process, the no of iterations:',ii
 write(kwrite,997)'And second root:',xx
 write(kwrite,*)
    
  996 format(A,i3)    
  997 format(A,f8.4)

      
 stop
 end program newrap
