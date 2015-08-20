      real*8 function water(dsize)
      character*100 id,revision,revdate
      data id/'$RCSfile: water.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/13 01:01:51 $'/
      integer i
      include "arrays.h"
      include "const.h"
      real*8 dsize(idimen)
c     
c  calculate the liquid water mixing ratio (kg/kg) by
c  summing the spectra     
c     
      water=0.
      do i=1,dmax
         water=water + rnr(i)*dsize(i)**3
      end do
      water=pi43*rhow*water
      return 
      end 
