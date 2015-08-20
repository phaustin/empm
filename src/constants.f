      subroutine constants(tc,ppa)
      character*100 id,revision,revdate
      data id/'$RCSfile: constants.f,v $'/
      data revision/'$Revision: 1.3 $'/
      data revdate/'$Date: 1994/03/12 17:58:49 $'/
      real*8 tc,ppa
      include "const.h"
c
c  tc=temperture in deg c, ppa= pressure in pascals
c
c  compute latent heat and diffusivities and send
c  out through common block const
c
c
c  from Bolton MWR, 1980
c
      lv=(2.501 - 0.00237*tc)*1.e6
c
c from micro notes, between -10 and 30 deg C
c and Rogers, p103
c
      Ktemp=7.7e-5*tc + 0.02399
      D=1.57e-7*tc + 2.211e-5
      D=D*1.e5/ppa
      return
      end
