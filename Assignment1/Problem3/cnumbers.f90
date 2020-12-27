
      ! Code as provided by Dr. elster, modified by S.Subedi
      program cnumbers
!
!     explore complex numbers 
!
      implicit none

      integer nphi, ii 
      integer kread, kwrite

      double precision pi
      double precision xx,yy,r,phi,pp,qq,y,p
      double complex   zz,sqzz,lnzz

      data kread/5/, kwrite/6/

!        calculate pi

      pi = acos(0.d0)*2.d0

      write (kwrite,*) pi

      r=1.d0

      open (kwrite, File = 'a.dat', Status = 'Unknown')

      ! write (kwrite,*) 'complex numbers for r=1'

       do ii=-16,16,1
          if (mod (ii,4) .eq. 0) then  !identifying points multiple of pi/2
             do p= (ii-0.3),(ii+0.3),.1 !taking some neighbourhood points

                phi = float(p)*0.25d0*pi
                xx=r*cos(phi)
                yy=r*sin(phi)

                zz=dcmplx(xx,yy)
                sqzz=csqrt(zz)
                lnzz=log(zz)
                pp = atan(yy/xx)
                qq = atan2(yy,xx)
                 !excluding points being precisely at multiple of pi/2
                 if (mod (p,4) .ne. 0) then 
                   write (kwrite,10000) ii,phi,xx,yy,sqzz,lnzz,pp,qq
                 end if
             end do
 
            else
                !for all points which are not multiple of pi/2
                phi=float(ii)*0.25d0*pi 
                xx=r*cos(phi)
                yy=r*sin(phi)

                zz=dcmplx(xx,yy)
                sqzz=csqrt(zz)
                lnzz=log(zz)
                pp = atan(yy/xx)
                qq = atan2(yy,xx)
                write (kwrite,10000) ii,phi,xx,yy,sqzz,lnzz,pp,qq
           end if
         end do

10000 format(i3,1x,3f8.4,1x,2f8.4,1x,2f8.4,1x,2f8.4,1x,2f8.4,1x,2f8.4,1x,2f8.4)


      stop 
      end


