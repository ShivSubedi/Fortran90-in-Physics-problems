! code provided by Dr. Elster, modified by S.Subedi
program euler
implicit none

real :: r,To,Ts,t,Tt,delt,Tlower,tdiff,texact,diff
real :: ndelt,nTlower,ntdiff,ntexact,ndiff
 integer :: kread, kwrite,ii

 data kread/5/, kwrite/6/
open(kwrite, File = 'cool.dat', Status = 'Unknown')

r=1. !as suggested in problem
To= 82.3 !initial temp of black coffee
Ts=17. !initial temperature of surrounding
t= 5.  
delt=.1
ndelt=.1/2. !decreasing value of delt by factor of 2

write(kwrite,*) '********************************************************************'
write(kwrite,*) 'delt         exact      numerical     diff        newdelt     newdiff'
write(kwrite,*) '********************************************************************'

do ii=1,10,1

!Analytic solution(provided in equation 4
Tt=Ts-(Ts-To)*exp(-r*t) 

!write (kwrite,*) 'Temperature after first',t, 'mins is ', Tt, 'degrees'
!Taking the derivative of Tt
!Tp=r*(Ts-To)*exp(-r*t)

!variation in our exact solution
Tlower=Ts-(Ts-To)*exp(-r*(t+delt))
tdiff=Tt-Tlower

!variation in our exact solution for new delta
nTlower=Ts-(Ts-To)*exp(-r*(t+ndelt))
ntdiff=Tt-nTlower

!variation in our numerical solution
texact= r*delt*(Tt-Ts)

!variation in our numerical solution for new delta
ntexact= r*ndelt*(Tt-Ts)

!difference between exact and numerical solution
diff=abs(texact-tdiff)

!difference between exact and numerical solution for new delta
ndiff=abs(ntexact-ntdiff)

write (kwrite,1000)  delt,tdiff,texact,diff,ndelt,ndiff

delt=delt/2.
ndelt=ndelt/2.
end do



1000 format(6(1f10.6,2x))
stop 'Data saved in cool.dat'
end program euler

