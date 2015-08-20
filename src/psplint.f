      subroutine psplint(ncat,x,y)
      include "interp.h"
      integer ncat,klo,khi,k
      real*8 h,a,b,y,x
c
c  this started out as numerical recipies splint, uses
c  derivitives and points passed in splinearray through interp.h
c      
      if(x .lt. splinearray(ncat,1,1) .or.
     $     x .gt. splinearray(ncat,1,npoints)) then
         print *,'interpolation out of range, for category ',ncat,
     $        ' asking for: ',x,'range is: ',splinearray(ncat,1,1),
     $        splinearray(ncat,1,npoints)
         stop
      endif

      klo=1
      khi=npoints
 1    if (khi-klo.gt.1) then
         k=(khi+klo)/2
         if(splinearray(ncat,1,k).gt.x)then
            khi=k
         else
            klo=k
         endif
         goto 1
      endif
      h=splinearray(ncat,1,khi)-splinearray(ncat,1,klo)
c     if (h.eq.0.) pause 'bad splinearray input.'
      a=(splinearray(ncat,1,khi)-x)/h
      b=(x-splinearray(ncat,1,klo))/h
      y=a*splinearray(ncat,2,klo)+b*splinearray(ncat,2,khi)+
     *     ((a**3-a)*splinearray(ncat,3,klo)+(b**3-b)
     $     *splinearray(ncat,3,khi))*(h**2)/6.
      return
      end
