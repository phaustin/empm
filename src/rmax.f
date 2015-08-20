
c
c This progran calculate the max of droplet radius
c

        subroutine rmax(datamax,data,ndrop,index_rmax)
        integer ndrop
        real*8 data(ndrop)
c
c maximum
c
        do j=1,ndrop
        data_diff=data(j)*1.e6-datamax
        if(data_diff.gt.0.)then
        datamax=data(j)*1.e6
	index_rmax=j
        end if
        enddo
        return
        end
