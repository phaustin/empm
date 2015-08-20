      subroutine spfind(ts,ps) 
      character*100 id,revision,revdate
      data id/'$RCSfile: spfind.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/12 17:59:02 $'/
      real*8 tsat,ppow,fq,ts,ps,e
      real*8 tqbase,tqold,t,deltat,pb,es,ew,qlw,tfunc,thetaq
      real*8 tfac,r

      integer j,l,ig

c                                                                        
c                                                                       
c     
      include "const.h"
      include "satpoint.h"                                                            
      include "sendit.h"
      
c                                                                       
c                                                                       
c 
      tqbase=tqc
      tqold=tqc 
      qtota=qc 
      t=200.0                                                           
      deltat=10.0                                                       
      pb=1000. 
      do 200 j=1,100                                                    
               t=t+deltat                                               
               es=ew(t)                                                 
                 tea=t                                                  
                 pda=pb-es                                              
                 qsa=eps*es/pda                                         
               qlw=qtota-qsa                                            
               if(pda.le.0.) then                                        
               write(6,495)                                             
495   format(/,15x,' pd.le.0 in qtcalc.  l,pbar,j,t es,pd,qs,qlw')       
               write(6,500) l,j,t,es,pda,qsa,qlw 
500   format(10x,i5,f10.2,2i5,f10.3,2f10.2,2f10.5,/)                    
               stop                                                     
               end if                                                   
c 
               if(qlw.gt.0.0) then 
                       ig=1 
               else 
                       ig=2 
                       e=pb*qtota/(eps+qtota) 
                       pda=pb-e 
               end if 
               tfunc=thetaq(ig)                                         
               tfac=((tfunc/tqbase)-1.000)                              
               if(abs(tfac).lt. 1.e-10) go to 300                       
               if(j.eq.1) go to 225                                     
               r=(tfunc-tqbase)/(tqold-tqbase)                          
               if(r.lt.0.00) deltat=-0.5*deltat                         
225            tqold=tfunc                                              
200            continue                                                 
c                                                                       
300   ts=tsat(1000.d0,t,qtota)                                          c 
      ppow=1./(con2*fq(qtota))                                          
      ps=1000.*(ts/t)**ppow                                             
      return                                                            
      end                                                               
