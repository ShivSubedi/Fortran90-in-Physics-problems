
!Code provided by Dr.Elster, modified by S.Subedi

 include 'f1ofx.f90'     

 program bisect1

 implicit none

 

 double precision tol
 parameter (tol=.0001)
 double precision x,epi
 double precision f1ofx
 double precision left,right,middle
 double precision fleft,fright,fmiddle
 double precision error
 integer kread, kwrite, ii

  data kread/5/, kwrite/6/
  open (kwrite, File = 'bisect_data.dat', Status = 'Unknown')

!initializing variables
!to find first root
  left = .1d0
  right= 5.d0
  fleft = f1ofx(left)
  fright = f1ofx(right)
  ii=1.

  write (kwrite,*) 'Search Root via Bisection Method: '
  write (kwrite,*) '***********************************************'
  write (kwrite,*) 'it.no lowerbound upperbound   root      error'
  write (kwrite,*)  '**********************************************'
  100  continue

   middle = (left+right)*0.5d0
   fmiddle = f1ofx(middle)

!determine which half contains the root

    if (fleft*fmiddle .le. 0) then

!      root is located in the left subinterval

        right=middle
        fright=fmiddle
        epi= abs ((middle-left)/middle)*100

     else

!        root is located in the right subinterval

         left=middle
         fleft=fmiddle
         epi= abs ((middle-right)/middle)*100
      endif
      
     
 write (kwrite,999) ii,left,right,middle,epi
 999 format (i3,1x,4(f10.4,1x))

!check for relative error
 error = abs ( (right-left)/middle )

      if (error .gt. tol ) then
          ii=ii+1
          go to 100
      end if

 write (kwrite,*)
 write (kwrite,*)
 write(kwrite,996)'As can seen from above iteration process, the no of iterations:',ii
 write(kwrite,997)'And first root:',middle
 write(kwrite,*)
 996 format(A,i3)    
 997 format(A,f8.4)
 


 !for second root point
 !initializing variables
 left = 5.d0
 right= 10.d0
 fleft = f1ofx(left)
 fright = f1ofx(right)
 ii=1.

  write (kwrite,*) 'Search Root via Bisection Method: '
  write (kwrite,*) '***********************************************'
  write (kwrite,*) 'it.no lowerbound upperbound   root      error'
  write (kwrite,*)  '**********************************************'
  200  continue

  middle = (left+right)*0.5d0
  fmiddle = f1ofx(middle)

!determine which half contains the root

     if (fleft*fmiddle .le. 0) then

!        root is located in the left subinterval
         right=middle
         fright=fmiddle
         epi= abs ((middle-left)/middle)*100

     else

!        root is located in the right subinterval
         left=middle
         fleft=fmiddle
         epi= abs ((middle-right)/middle)*100
     endif
      
     
 write (kwrite,1001) ii,left,right,middle,epi
 1001 format (i3,1x,4(f10.4,1x))

 !check for relative error
 error = abs ( (right-left)/middle )

      if (error .gt. tol ) then
           ii=ii+1
           go to 200
      end if

 write (kwrite,*)
 write (kwrite,*)
 write(kwrite,1002)'As can seen from above iteration process, the no of iterations:',ii
 write(kwrite,1003)'And second root:',middle
 write(kwrite,*)
 1002 format(A,i3)    
 1003 format(A,f8.4)
    
 end program bisect1
