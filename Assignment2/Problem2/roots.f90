
!Code written by S.Subedi
!program to find roots by iteration process
program roots
implicit none

integer kread, kwrite,n
data kread/5/, kwrite/6/
double precision :: x(0:20)
open (kwrite, File = 'roots_data.dat', Status = 'Unknown')

write (kwrite, *) '***********************'
write (kwrite, *) ' n          x(n)'
write (kwrite, *)  '***********************'

x(0)=0.d0

do n=0,10
    x(n+1)=1/(4+x(n))
    write (kwrite,100) n, x(n)
end do


100 format (i3,1x,1f17.9)
end program roots
