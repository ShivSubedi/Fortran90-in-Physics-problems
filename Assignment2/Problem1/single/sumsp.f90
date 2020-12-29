  !Code provided by Dr. Elster, modified by S.Subedi    
  !Program to find the sumup and sumdown of given function by recursion method using single precision

      program precision 

      !sum of 1/n

      implicit none

     
      real :: onesp
      real :: sumupsp, sumdownsp
      real :: reldiffsp, relsumsp, relerrsp
      integer :: nend, nendm1, nn, n, ntot
      integer :: ii
      integer :: kread, kwrite
      parameter (ntot=10) 
     
      data kread/5/, kwrite/6/
      data onesp/1./   
      open (kwrite, File = 'sumsp_data.dat', Status = 'Unknown')
      
!     initialize

 
      sumupsp=0.
      sumdownsp=0.
      
      write (kwrite,*) '                    Output for single precision'
      write (kwrite, *) '***************************************************************'
      write (kwrite, *) '           nend  relerrsp   reldiffsp    sumupsp    sumdownsp'
      write (kwrite, *) '****************************************************************'

      do ii=1,ntot
          nend = 10**ii
          nendm1=nend-1
!
!
!         sum upward
!
          do n=1,nend

                sumupsp=sumupsp + onesp/float(n)

          end do

!
!         sum downward
!

          do n=0,nendm1

                 nn=nend-n
                 sumdownsp=sumdownsp + onesp/float(nn) 

          end do

          reldiffsp = abs(sumupsp-sumdownsp)
          relsumsp  = sumupsp+sumdownsp
          relerrsp  = reldiffsp/abs(sumdownsp)
      

      
          write (kwrite,10000) nend,relerrsp,reldiffsp,sumupsp,sumdownsp

      end do
      

      10000 format(i15,6(1x,d11.6))
!
      
 
      stop 
      end 
