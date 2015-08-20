      subroutine fcnkb(t,dsize,drdt)
      character*100 id,revision,revdate
      data id/'$RCSfile: fcnkb.f,v $'/
      data revision/'$Revision: 1.7 $'/
      data revdate/'$Date: 1994/03/14 00:34:07 $'/
      include "const.h"
      include "salcon.h"
      include "arrays.h"      
      include "interp.h"

      real*8 dsize(*),drdt(*),t
      real*8 sw,es,ew,qs,temp,falpha,fbeta,s,cr
      real*8 rhol,c,e,rho,ck,c2,c7,denom,dqvdt
      
      integer i
      
c     
c     initialize diffusivities D,Ktemp and lv through 
c     common block const.h
c     
      nrhs=nrhs+1
      call constants(dsize(dmax+2)-273.15,dsize(dmax+4))
      sw=0. 
      temp=dsize(dmax+2) 
      es=ew(dsize(dmax+2))*100. 
      qs=eps*es/(dsize(dmax+4) - es) 
      dqvdt=0.
      do 10 i=1,dmax 
         falpha=dsize(i)/(dsize(i) + lalpha)
         fbeta=dsize(i)/(dsize(i) + lbeta)
         if(i .gt. dmaxa) then 
            c2=c2nacl 
            c7=c7nacl 
         else 
            c2=c2am 
            c7=c7am 
         endif 
c     
c     check to see whether droplet is < 0.2 um, has a radius
c     less than rcrit and the saturation is less than scrit.
c     then get the equilibrium radius using the interpolator
c     
         S=dsize(dmax+3) 
c        if((dsize(i) .lt. 2.e-7) .and.
c    $        (dsize(i) .lt. splinearray(i,2,npoints)) .and.
c    $        (1+S) .lt. splinearray(i,1,npoints)) then
c           call psplint(i,1+S,dsize(i))
c write(*,*)i,dsize(i),inter
c           drdt(i)=0.
c        else
            rhol=(dsize(i)**3*pi43*1.e3+csize(i)*c7)/(dsize(i)**3*pi43) 
            Ck= (2*sigma/(Rv*dsize(dmax+2)*rhol*dsize(i))) 
            Cr= c2*csize(i)/(pi43*dsize(i)**3*rhol-csize(i)) 
            denom=rhol*
     $           (Rv*temp/(fbeta*D*es)+Lv**2/(falpha*Ktemp*Rv*temp**2))
            drdt(i)=1./dsize(i)*(S - Ck + Cr)/denom
c        endif
         dqvdt=dqvdt-pi4*rnr(i)*dsize(i)**2*drdt(i)*1.e3 
 10   continue 
c     
c     
c     
      e=dsize(dmax+1)*dsize(dmax+4)/(eps + dsize(dmax+1)) 
      rho=(dsize(dmax+4) - e)/(rd*dsize(dmax+2))*(1.+ 
     c     dsize(dmax+7)) + e/(rv*dsize(dmax+2)) 
c     c=cpd+dsize(dmax+1)*cv+dsize(dmax+7)*cw 
	c=1005.
      drdt(dmax+1)=dqvdt
      drdt(dmax+2)=-lv/c*drdt(dmax+1) 
c
c try to take out the velocity effect
c
c     -g/c*dsize(dmax+5)
c

c
c old version
c
c$$$      drdt(dmax+3)=dsize(dmax+4)*rv/(es*rd)*drdt(dmax+1)
c$$$     c     -(1.+dsize(dmax+3))
c$$$     c     *(lv/(rv*dsize(dmax+2)**2)*drdt(dmax+2)
c$$$     c     +g/(rd*dsize(dmax+2))*dsize(dmax+5))
c
c new version  (94/10/26)
c
      drdt(dmax+3)=(1.+dsize(dmax+3))*(Rd/(dsize(dmax+1)*
     c (Rv*dsize(dmax+1)+Rd))*drdt(dmax+1)
     c     -g*rho*dsize(dmax+5)/dsize(dmax+4)
     c     -lv/(Rv*dsize(dmax+2)**2)*drdt(dmax+2))
      drdt(dmax+4)= -1.*rho*g*dsize(dmax+5) 
      drdt(dmax+5)=0. 
      drdt(dmax+6)=dsize(dmax+5) 
      drdt(dmax+7)= -1.*dqvdt
      return 
      end 





