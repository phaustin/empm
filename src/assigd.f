
c. This subroutine randomly assigns the droplet position

	subroutine assigd(ndrop,ngrid,jcell_pop,delta_x
     1  ,m1,x,BL,igrid,iseed4)

	parameter(idimen=5000)
	
	integer*4 igrid

	dimension jcell_pop(0:10,igrid)
	real*8  x(idimen)
	real*8 delta_x
	real*8 pjw
	integer*4 iseed4

c. initialize jcell_pop(0,j) which is the index
c. used in drop_map.f

	do j=1,ngrid
	  jcell_pop(0,j)=0
	end do

	do i=1,ndrop

Copen(25,file='assigd.dat') --060796 -earlier one
Copen(25,file='assigd.dat1')

C 	 open(25,file='assigd_100m.dat')
C	 read(25,*)rand_no
	 rand_no = RAND1(iseed4)

	  x(i)=rand_no * BL
	  pjw=x(i)/delta_x
	  j=INT(pjw)+M1
	  if(j.gt.ngrid)j = j-ngrid
	  jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
	  jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i

	end do

c write out the cell and index where has more than
c 2 droplet.
	 n_gt4 = 0
	 nis4 = 0
	 nis3 = 0
	 nis2 = 0
	 nis1 = 0
	
        do j = 1, ngrid
	 if (jcell_pop(0,j-m1+1).gt.4)then
	  n_gt4 = n_gt4 + 1
          else if (jcell_pop(0,j-m1+1).gt.3)then
	    nis4 = nis4 + 1
	    else if (jcell_pop(0,j-m1+1).gt.2)then
	      nis3 = nis3 + 1
	      else if (jcell_pop(0,j-m1+1).gt.1)then
	        nis2 = nis2 + 1
	        else if (jcell_pop(0,j-m1+1).gt.0)then
	           nis1 = nis1 + 1
	  end if
	          
        end do

            write(*,*)'j has >4 drops are', n_gt4
	    write(*,*)'j has 4 drops are', nis4
            write(*,*)'j has 3  drops are', nis3
            write(*,*)'j has 2  drops are', nis2
            write(*,*)'j has 1  drops are', nis1


	return
	end

