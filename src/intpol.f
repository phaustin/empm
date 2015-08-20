c     z = height to interpolate to
c     z1(k) = array of sounding coordinates (heights or pressures)
c      with n1 elements
c     arranged in either ascending or descending order
c     with respect to z (works either way EXACTLY as coded below).
c     t1(k) = array of sounding temperatures with n1 elements
c     corresponding to coordinate z1(k).
c     t = (lineraly) interpolated temperature at coordinate z.
  
	subroutine intpol(time0,w,NI,P1,T1,pe,T)

        real*8 P1(NI)
        real*8 T1(NI)
	real*8 T,fintrp,pe,w
	logical LF

Cwrite(*,*)NI
Cdo i=1,NI
Cwrite(*,*)p1(i),T1(i)
Cenddo

	pe=963.95-w*time0/10.

      K1 = INDEXR ( pe, NI, P1, LF )
      K2 = K1 + 1
Cwrite(*,*)'K1,K2=',k1,k2
c	write(*,*)
      T = FINTRP ( 1, pe, P1(K1), T1(K1), P1(K2), T1(K2) )
Cwrite(*,*)'T',T
	return
	end

