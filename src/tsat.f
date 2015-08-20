      real*8 function tsat(p,t,q)                                              
      character*100 id,revision,revdate
      data id/'$RCSfile: tsat.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/12 17:59:04 $'/
c                                                                       
c                                                                       
c     this function calculates the temperature at which a               
c     parcel becomes saturated using the formula from bolton            
c     (1980),mwr, vol.108,no.7, 1046-1053.                              
c                    
      include "const.h"
      real*8 q,p,t,e
c                                                                       
      e=(q*p/(eps+q))                                                   
      tsat=(2840./(3.5*dlog(t)-dlog(e)-4.805))+55.                      
      return                                                            
      end                                                               
