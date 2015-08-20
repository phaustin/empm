      subroutine msd(dsize,rmean,sd,rnum) 
      character*100 id,revision,revdate
      data id/'$RCSfile: msd.f,v $'/
      data revision/'$Revision: 1.4 $'/
      data revdate/'$Date: 1994/03/12 17:58:57 $'/
      include "arrays.h"
      real*8 rmean,sd,rnum,sum,sq,dsize(*)
      
      integer i
c     
c     
c     
      sum=0. 
      sq=0. 
      rnum=0. 
      do i=1,dmax
         if(dsize(i) .gt. 1.e-6) then 
            rnum=rnum+rnr(i)
            sum=sum + dsize(i)*rnr(i)
            sq=sq + rnr(i)*dsize(i)**2. 
         endif 
      end do
      rmean=sum/rnum 
      sd=(sq/rnum - rmean**2.)**0.5 
      return 
      end 
