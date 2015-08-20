
c
c This progran calculate the max and min of input variables
c

	subroutine max_min(datamax,datamin,data,m1,m4,igrid)
	integer*4 igrid
	real*8 data(igrid)
c
c maximum
c    
        do j=m1,m4
        data_diff=data(j)-datamax
        if(data_diff.gt.0.)then
        datamax=data(j)
	end if
	enddo
c
c calculate minimum
c
	do j=m1,m4
        data_diff=data(j)-datamin
        if(data_diff.lt.0.)then
        datamin=data(j)
        end if
        enddo

	return
	end
