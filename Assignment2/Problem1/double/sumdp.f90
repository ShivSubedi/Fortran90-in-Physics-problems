      
  !Code provided by Dr. Elster, modified by S.Subedi 
  !Program to find the sumup and sumdown of given function by recursion method using double precision

      program precision 
!
!     sum of 1/n
!
      implicit none

      double precision :: sumupdp, sumdowndp
      double precision :: onedp
      double precision :: reldiffdp, relsumdp, relerrdp
      !real :: one
      integer :: nend, nendm1, nn, n, ntot
      integer :: ii
      integer :: kread, kwrite
      parameter (ntot=10) 
     
      data kread/5/, kwrite/6/
      data onedp/1.d0/
      open (kwrite, File = 'sumdp1_data.dat', Status = 'Unknown')
      
!     initialize

 
     
!
      sumupdp=0.d0
      sumdowndp=0.d0
      !write (kwrite,*) '                    Output for Double precision'
      !write (kwrite, *) '************************************************************************************'
      !write (kwrite, *) '           nend           relerrdp          reldiffdp        sumupdp   sumdowndp'
      !write (kwrite, *) '************************************************************************************'

      do ii=1,ntot
            nend = 10**ii
            nendm1=nend-1
!
!
!           sum upward
!
            do n=1,nend

                  sumupdp=sumupdp + onedp/float(n)

            end do

!
!           sum downward
!

            do n=0,nendm1

                   nn=nend-n
                   sumdowndp=sumdowndp + onedp/float(nn) 

            end do

            reldiffdp = abs(sumupdp-sumdowndp)
            relsumdp  = sumupdp+sumdowndp
            relerrdp  = reldiffdp/abs(sumdowndp)

      
             write (kwrite,20000) nend,relerrdp,reldiffdp,sumupdp,sumdowndp

        end do
      

        20000 format(i15,4(1x,d20.10))  
 
 
         stop 
         end program precision
