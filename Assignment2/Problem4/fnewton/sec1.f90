 
 !Code provided by Dr. Elster, modified by S.Subedi

      subroutine newton (pre, ii,comp,xx)
      implicit none

      double precision f1ofx,pre, comp
      double precision xx,yy,zz, epi
      double precision x
      Integer ii
      integer ii, kread, kwrite
      data kread/5/, kwrite/6/
      


      ii = 0
      yy = xx + pre
      write (kwrite,*) 'Search Root via Hybrid method: '
      write (kwrite,*) '***********************************************'
      write (kwrite,*) 'it.no lowerbound upperbound   root      error'
      write (kwrite,*)  '**********************************************'
      do    20  while (abs(pre).gt.comp)

               zz = yy - f1ofx(yy)*(yy-xx)/(f1ofx(yy) - f1ofx(xx))
               xx = yy
               yy = zz
               pre = yy - xx
               ii = ii + 1
               epi= abs ((yy-xx)/zz)*100
               write(kwrite,999) ii, xx, yy, zz, epi
      20 end do
      999 format (i3,4(1f10.4,1x))

      return
      end

      
        

      double precision function f1ofx (x)

      implicit none

      double precision x
      double precision f1ofx

      f1ofx = x**2-7*x-log (x) 
     
      return  
      end 


