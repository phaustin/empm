aedens=1.774e3
salden=2.165e3
con2=rd/cpd  
# c
# c molecular weights in kg/Kmole
# c
amsufm=132.14
ambim=115.11
naclm=58.44
waterm=18.016
Md=28.97
# c
# c  use these derived constants from const.eval.f
# c
# c$$$c     
# c$$$c     choose van hoff=3  molecular weight=132.14 
# c$$$c     for ammonium sulphate   vh=3 molecular weight=115.11 
# c$$$c     for ammonium bisulphate and vh=2  molecular weight=58.44 
# c$$$c     for nacl 
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
