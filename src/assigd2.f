c. This subroutine randomly assigns the entrained ccn
c locations within m2 and m3, which are the 1st and last
c grid cells for the newly entrained blob

	subroutine assigd2(ndrop_used,nccn,m2,m3,jcell_pop,delta_x
     1  ,m1,x,BL,igrid, iseed5)

	integer*4 igrid
	parameter(idimen=5000)
	
	dimension jcell_pop(0:10,igrid)
        real*8 x(idimen)
	real*8 delta_x
	real*8 pjw
	integer*4 iseed5

c. initialize jcell_pop(0,j) which is the index
c. used in drop_map.f
	
	do j=m2,m3
	  jcell_pop(0,j-m1+1)=0
	end do

	do i=ndrop_used+1,ndrop_used+nccn

c. randomly assign x(i);
c. find corresponding j_cell for x(i)
c

 330    continue

C	  open(28,file='assigd2_100m.dat')
C	  read(28,*)rand_no
	  rand_no = RAND1(iseed5)

	  x(i)=rand_no*(m3-m2+1)*delta_x+(m2-1)*delta_x
 	  pjw=x(i)/delta_x
	  j=INT(pjw)+M1
          jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
 	  jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i

	  if (j.gt.m3)then
	     print *, 'j>m3 j m3 ', j, m3
	     else if (j.lt.m2)then
	       print *, 'j < m2, j m2', j, m2
	  end if

	end do
	
c write out the cell and index where has more than
c 2 droplet.
         n_gt3 = 0
         nis3 = 0
         nis2 = 0
         nis1 = 0

         do j = m2, m3
          if (jcell_pop(0,j-m1+1).gt.3)then
            n_gt3 = n_gt3 + 1
            else if (jcell_pop(0,j-m1+1).gt.2)then
              nis3 = nis3 + 1
              else if (jcell_pop(0,j-m1+1).gt.1)then
                nis2 = nis2 + 1
                else if (jcell_pop(0,j-m1+1).gt.0)then
                   nis1 = nis1 + 1
          end if

        end do

            write(*,*)'j has >3 drops are', n_gt3
            write(*,*)'j has 3  drops are', nis3
            write(*,*)'j has 2  drops are', nis2
            write(*,*)'j has 1  drops are', nis1


	return
	end












