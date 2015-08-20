      character*100 arrayid,arrayrevision,arrayrevdate
Cdata arrayid/'$RCSfile: arrays.h,v $'/
C     data arrayrevision/'$Revision: 1.3 $'/
C     data arrayrevdate/'$Date: 1994/03/14 00:34:47 $'/
      integer idimen,dmax,nrhs,dmaxa,dmaxs
      parameter(idimen=100)
c
c modified 04-01-95
c add dcat(idimen)
c
	integer dcat(idimen)
      real csize(idimen),rnr(idimen),csizes(idimen),rnrs(idimen)
      real stepsize
      COMMON /array/ csize,rnr,csizes,rnrs,dmax,nrhs,dmaxa,dmaxs,
     $      stepsize


