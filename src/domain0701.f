c.
c. To find spoint and fpoint of the entrained parcel
c. To decide location to put this parcel into the linear
c. eddy domain.
c.
	subroutine domain(psigma,ngrid,Xn,spoint,fpoint,iseed6, 
     &             n_blob, n_blob_final)

        integer n_max
        parameter (n_max = 30)
c integer Xn, spoint,fpoint
        integer Xn, spoint(n_max), fpoint(n_max)
	real*8 psigma
	integer*4 iseed6
        integer n_blob, n_blob_final
        integer n_s(n_max), n_f(n_max), k, i

c.
c. psigma: portion of the entrainment parcel
c. Xn    : grid points of (1-parcel)
c. spoint: start point of this parcel
c. fpoint: final point of this parcel
c.
c. calculate how many grid points (1-parcel) has
c.
        Xn=int( (1.- psigma * n_blob ) * ngrid)
        write(*,*)psigma,ngrid,Xn
c       Xn=int((1.-psigma)*float(ngrid))
c       write(*,*)psigma,ngrid,Xn

c.
c. randomly choosing the spoint and fpoint
c. shown by grid point relative to this parcel.
c.
C	open(27,file='domain.dat1')
C	read(27,*)rand_no
	
	rand_no = RAND1(iseed6)

c spoint=int(rand_no*Xn)+1
c fpoint=spoint+(ngrid-Xn)-1
c write(*,*)spoint,ngrid,Xn
 
        n_s (1) = int(rand_no * Xn) + 1
        n_f (1) = n_s(1) + psigma * ngrid - 1
        write(*,*)'n_s(1), n_f(1) = ', n_s(1), n_f(1)
        do i = 2, n_blob
           n_s (i) = n_f(i-1) + int( Xn / n_blob) 
           n_f (i) = n_s(i) + psigma * ngrid - 1
           write(*,*)'grid point of entrained blob',i, n_s(i),
     &                n_f(i), Xn
        enddo
        
* check if n_s or n_f great than ngrid, if it is it needs to re-establish period
* condition
        k = 0
        do i = 1, n_blob
           k = k + 1
           if( (n_s(i) .le. ngrid) .and. (n_f(i) .gt. ngrid) ) then
                spoint (k) = n_s(i)
                fpoint (k) = ngrid
                spoint (k+1) = 1
                fpoint (k+1) = n_f(i) - ngrid
                k = k + 1 ! increase k by 1
           else if ((n_s(i) .gt. ngrid) .and. 
     &              (n_f(i) .gt. ngrid) ) then
                spoint (k) = n_s(i) - ngrid
                fpoint (k) = n_f(i) - ngrid
           else
                spoint (k) = n_s(i)
                fpoint (k) = n_f(i)
           endif
           print *, 'k sp, fp =', k, spoint(k), fpoint(k)
        enddo
        n_blob_final = k     
        print *, 'n_blob_final = ', n_blob_final

        return
        end 
