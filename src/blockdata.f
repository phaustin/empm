	block data 
c character*100 id,revision,revdate
c     data id/'$RCSfile: blockdata.f,v $'/
c     data revision/'$Revision: 1.3 $'/
c     data revdate/'$Date: 1994/03/12 17:58:49 $'/
      include "const.h"
      include "salcon.h"
      include "const.data.h"

c$$$     
c$$$      specify the constants 
c$$$     
c$$$      
c$$$      
c$$$c     
c$$$c     
c$$$c     
c$$$c     
c$$$
c$$$      aedens=1.774e3
c$$$      salden=2.165e3
c$$$      con2=rd/cpd  
c
c molecular weights in kg/Kmole
c
c$$$      amsufm=132.14
c$$$      ambim=115.11
c$$$      naclm=58.44
c$$$      waterm=18.016
c         Md=28.97
c
c  use these derived constants from const.eval.f
c
c$$$c     
c$$$c     choose van hoff=3  molecular weight=132.14 
c$$$c     for ammonium sulphate   vh=3 molecular weight=115.11 
c$$$c     for ammonium bisulphate and vh=2  molecular weight=58.44 
c$$$c     for nacl 
c$$$c     
c$$$      if(ambi) then 
c$$$         c2am=3.*waterm/ambim 
c$$$      else 
c$$$         c2am=3.000*waterm/amsufm 
c$$$      endif 
c$$$      c2nacl=2.*waterm/naclm 
c$$$c      
c$$$c     densities of amsul and ambisul=1.78, 1.77, consider 
c$$$c     them equal 
c$$$c     
c$$$      c7am=(aedens-1.e3)/aedens 
c$$$      c7nacl=(salden-1.e3)/salden 
c$$$c     
c$$$c     
c$$$c     
c$$$
      end
