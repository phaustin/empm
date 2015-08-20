      SUBROUTINE rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
      character*100 id,revision,revdate
      data id/'$RCSfile: rkqs.f,v $'/
      data revision/'$Revision: 1.4 $'/
      data revdate/'$Date: 1994/03/12 17:59:01 $'/
      INTEGER n
      include "arrays.h"
      REAL*8 eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivs
CU    USES derivs,rkck
      INTEGER i
      REAL*8 errmax,h,xnew,yerr(IDIMEN),ytemp(IDIMEN),SAFETY,
     $  PGROW,PSHRNK,ERRCON
      PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4)
      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr,derivs)
      errmax=0.
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        h=SAFETY*h*(errmax**PSHRNK)
        if(h.lt.0.1*h)then
          h=.1*h
        endif
        xnew=x+h
        if(xnew.eq.x)pause 'stepsize underflow in rkqs'
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
	if((i.le.dmax).and.(y(i).le.0.))y(i)=1.d-8
12      continue
        return
      endif
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 5"#@-130Rk#3#)KB.
