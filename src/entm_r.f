

      subroutine DGM(radius,qv,Temp,dm3,dm4,dm5,dm6,ql,
     $weight,concer,ndmax,t_use,ite,iteration1,ncat,iicat)

      
c     
c     array dsize:  1--dmax contain the radii of each droplet category
c     
c     the  other variables:
c     dmax+1=wv water vapor (kg/kg)
c     dmax+2=temp (K)
c     dmax+3=s (fraction)
c     dmax+4=p pressure (pa)
c     dmax+5=w constant vertical velocity (m/s)
c     dmax+6=z 
c     dmax+7=wl liquid water (kg/kg)
c     
c     
c     files needed
c     
c     
      external fcnkb,rkqs      
c     parameter(idimen=100)
      integer iloop,kk
      integer nok,nbad,kount
      integer ll
      
      real*8 amp(3),sig(3)
      real*8 t1,t2,h1,hmin
      real*8 vamp,qs,delt
      real*8 mu(3),lm1(3),lm2(3),dlm(3)
      real*8 dens,zstart,tmax,tprint
      real*8 sout,pbase,tbase
      real*8 es,ew,ql,qtot0,water,epserr
      real*8 rh,waterdiff,totdiff
      real*8 ts,ps,rmean,sd,rnum

      real*8 radius,dm3,dm4,dm5,dm6,
     $weight,concer,t_use

      real*8   qv, Temp
      integer ite, iteration1, ndmax
      integer ncat,iicat
c     
      include "salcon.h"
      include "const.h"
      include "sendit.h"
      include "satpoint.h"
      include "arrays.h"

	real*8 dsize(idimen)
        real*8 wy(idimen),w1d(idimen),w2d(idimen)
	real*8 ql2,ql_new
 
c dm3: Supersaturation
c dm4: pressure (pa)
c dm5: w, constant velocity
c dm6: height
c ql : liquid water (Kg/Ke)
 
	dmax=ndmax
	dmaxa=ncat

	dcat(1)=iicat
       open(unit=16,file='dgm.dat')
       dsize(1)=radius
       rnr(1)=concer
       csize(1)=weight
C      lalpha=9.2658731705498098E-07
C      lbeta=4.4710370273167390E-06
C      sigma=7.2498199999999988E-02

       lalpha=8.96686e-7
       lbeta=4.33193e-6
       sigma=7.392730e-2

       dsize(ndmax+1)=qv
       dsize(ndmax+2)=Temp
       dsize(5)=dm4
       dsize(6)=dm5
       dsize(7)=dm6

	if(ite.gt.1)goto 93
Cif(ite.gt.10)goto 93
       ql=water(dsize)
 93    dsize(8)=ql
 
       es=ew(dsize(ndmax+2))*100.
       qs=eps*es/(dsize(ndmax+4) - es)

       SS=dsize(ndmax+1)/qs-1.
Cif(ite.eq.1)then
Css=-7.76e-3
Cendif

cwrite(11,*)'ite,ss',ite-1,ss
cwrite(11,*)dsize(ndmax+2),dsize(ndmax+1),qs
cwrite(11,111)dsize(ndmax+2),dsize(ndmax+1),
c    c  ss,dsize(ndmax+4)
c111   format(4(1x,e25.14))
       dsize(4)=SS
c      es=ew(tbase)*100. 
c      qs=eps*es/(pbase-es) 

       qtot0=dsize(ndmax+1) + dsize(ndmax+7)
C	iloop=int(tmax/tprint) !was generating error

      t1=0.
       t2=0.
       epserr=1.0e-4
       hmin=0.0
       nrhs=0.
c
c	add time steps for odeint
c
Ch1=t_use*10.
Ch1=t_use*1000.
	h1=t_use*1.
c
c do ll=1,iloop
c        t1=t2
Ct1=(ite-4.)*t_use
Ct1=(ite-9.)*t_use
	t1=ite*t_use
         t2=t1 +h1
cwrite(*,*)'t1,t2,ite=,ndmax+7',t1,t2,ite,ndmax+7
c write(*,*)'dmax=',dmax
         call odeint(dsize,ndmax+7,t1,t2,epserr,h1,hmin,nok,
     $     nbad,fcnkb,rkqs)
	 
cwrite(*,*)'ite',ite
c        write(*,'(/1x,a,t30,i8)') 'Successful steps:',nok
c        write(*,'(1x,a,t30,i8)') 'Bad steps:',nbad
c        write(*,'(1x,a,t30,i8)') 'Function evaluations:',nrhs
c        write(*,'(1x,a,t30,i8)') 'Stored intermediate values:',kount
c     
c     write out the variables every tprint seconds
c     
c     
c        call findsp(dsize,ts,ps)
c        ql=water(dsize)
c        waterdiff=(dsize(ndmax+7) - ql)/ql
c        totdiff=(qtot0 - (dsize(ndmax+1) + dsize(ndmax+7)))/qtot0
c call msd(dsize,rmean,sd,rnum) 
c call output(dsize,rmean,sd,rnum,ps,
c $     t1,waterdiff,totdiff)

        qv=dsize(ndmax+1)
        Temp=dsize(ndmax+2)
        dm3=dsize(4)
        dm4=dsize(5)
        dm5=dsize(6)
        dm6=dsize(7)
        ql=dsize(8)
Cql_new=water(dsize(1))
Cql=ql_new
C       if (ql.lt.0.)then
Cwrite(85,*)'---------------------------------------------------'
Cwrite(85,*)'in entm_r.f'
C       write(85,*)
C    1  ql,ql_new
C    2 ,dsize(1)
C       write(85,*)'---------------------------------------------------'
C       endif
C ql=water(dsize)
Cwaterdiff=(dsize(8) - ql2)/ql2
Cc        totdiff=(qtot0 - (dsize(ndmax+1) + dsize(ndmax+7)))/qtot0
Cif(mod(ite,4167).gt.0)goto 881
Cwrite(35,880)ite,ql,ql2,waterdiff, dsize(1)
C880    format(1I5,4(1x,e12.6))
 881	continue
        radius=dsize(1)

        return
        end
