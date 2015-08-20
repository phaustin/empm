      real*8 function ew(tz)                                                   
      character*100 id,revision,revdate
      data id/'$RCSfile: ew.f,v $'/
      data revision/'$Revision: 1.4 $'/
      data revdate/'$Date: 1994/03/12 17:58:52 $'/
c                                                                       
c     this function calculates the saturated partial pressure           
c     of water vapor over liquid water (ew) and ice (ei).  the          
c     formulas are from pruppacher and klett, p.625                     
c 
c     tz is absolute temperature, ew is in mb 
c 
c                                                                       
c     these formulas are valid from -50 c to +50 c                      
c     for water and -50c to 0 c for ice.                                
c     if the temperature is outside these ranges, the                   
c     partial pressure is calculated at the valid temperature           
c     limit for t.gt.50 c, and the partial pressure is                  
c     calculated with an exponential decay from -50 c.                  
c                                                                       
c                                                                       
c            

      real*8 a0,a1,a2,a3,a4,a5,a6,ta,t,tz,ei,et
                                                           
      a0=6.107799961                                                    
      a1=4.436518521e-01                                                
      a2=1.428945805e-02                                                
      a3=2.650648471e-04                                                
      a4=3.031240396e-06                                                
      a5=2.034080948e-08                                                
      a6=6.136820929e-11                                                
c                                                                       
      t=tz-273.16                                                       
      ta=1.0                                                            
      if(t.gt.50.) t=50.0                                               
      if(t.lt.-50.) then                                                
               ta=t                                                     
               t=-50.0                                                  
      end if                                                            
      ew=a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+t*a6)))))                     
      if(ta.lt.-50.0) then                                              
               ew=ew*dexp((ta+50D0)/10.)                                 
      end if                                                            
c                                                                       
      return                                                            
c 
c                                                                       
      entry ei(tz)                                                      
c                                                                       
      a0=6.109177956                                                    
      a1=5.034698970e-01                                                
      a2=1.886013408e-02                                                
      a3=4.176223716e-04                                                
      a4=5.824720280e-06                                                
      a5=4.838803174e-08                                                
      a6=1.838826904e-10                                                
c                                                                       
      t=tz-273.16                                                       
      ta=1.0                                                            
      if(t.lt.-50.0) then                                               
               ta=t                                                     
               t=-50.0                                                  
      end if                                                            
      if(t.gt.0.0) t=0.0                                                
      et=a0+t*(a1+t*(a2+t*(a3+t*(a4+t*(a5+t*a6)))))                     
      if(ta.lt.-50.0) then                                              
               et=et*dexp((ta+50.)/10.)                                 
      end if                                                            
c                                                                       
      ei=et                                                             
      return                                                            
      end                                                               
