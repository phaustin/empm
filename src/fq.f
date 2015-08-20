      real*8 function fq(z)                                                    
      character*100 id,revision,revdate
      data id/'$RCSfile: fq.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/12 17:58:54 $'/
c                                                                       
c                                                                       
c     this function calculates the ratio (1+0.608*z)/(1+0.87*z)         
c            

      real*8 z,zcon
                                                           
      include "const.h"
c 
      zcon=(cv/cpd-1.)                                                 
      fq=(1.+0.608*z)/(1.+zcon*z)                                       
      return                                                            
      end                                                               
