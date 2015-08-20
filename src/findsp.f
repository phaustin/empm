      subroutine findsp(dsize,ts,ps)
      character*100 id,revision,revdate
      data id/'$RCSfile: findsp.f,v $'/
      data revision/'$Revision: 1.4 $'/
      data revdate/'$Date: 1994/03/12 17:58:53 $'/
      include "arrays.h"
      real*8 dsize(*)
      real*8 ts,ps,esa,thetaq 
      integer ig
      include "sendit.h"
      include "const.h"
      include "satpoint.h"
c     
c     fill the common block sendit for the thetaq calculation 
c     
      qtota=dsize(dmax+1) + dsize(dmax+7) 
      qc=qtota 
      qsa=dsize(dmax+1) 
      esa=dsize(dmax+4)*qsa/(eps + qsa) 
c     
c     need pressure in mb 
c     
      pda=(dsize(dmax+4) - esa)/100. 
      tea=dsize(dmax+2) 
      if(dsize(dmax+7) .gt. 1.e-6) then 
         ig=1 
      else 
         ig=0 
      endif 
      tqc=thetaq(ig) 
      call spfind(ts,ps) 
      return
      end
