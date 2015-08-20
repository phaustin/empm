      real*8 function thetaq(iwhere)                                           
      character*100 id,revision,revdate
      data id/'$RCSfile: thetaq.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/12 17:59:03 $'/
      real*8 factor
      real*8 tl,tsat,ppow,tpow,alph,fq,theta,pfac
      integer iwhere
      
c     
      include "const.h"
      include "sendit.h"
c     
      if(iwhere.eq.1) then                                            
         alph=cpd+(cw*qtota)                                               
         tpow=rd/alph                                                      
         factor=dexp(qsa*lv/(tea*alph))                                   
         thetaq=tea*((1000./pda)**tpow)*factor                             
      else                                                            
         alph=cpd+(cw*qtota)                                               
         tl=tsat(pda,tea,qtota)                                            
         tpow=con2*fq(qtota)                                               
         theta=tea*((1000./pda)**tpow)                                     
         ppow=1./tpow                                                      
         pfac=(theta/tl)**ppow                                             
         factor=(pfac**(-1.12*qtota))*dexp(qtota*lv/(tl*alph))            
         thetaq=factor*theta                                               
      end if                                                          
c     
      return                                                            
      end                                                               




