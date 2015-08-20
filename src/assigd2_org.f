
c. This subroutine randomly assigns the droplet position and
c. remove the drops when they are in the parcel. (j.ge.M2) 
c. and (j.le.M3)

	subroutine assign2(ndrop_used,nccn,m2,m3,jcell_pop,delta_x
     1  ,m1,x,BL,igrid)
	integer*4 igrid
	parameter(idimen=5000)
	
	double precision rand

	dimension jcell_pop(0:10,igrid)
        real*8 x(idimen)
	real*8 delta_x
	real*8 pjw

c. initialize jcell_pop(0,j) which is the index
c. used in drop_map.f

	
	do j=m2,m3
	jcell_pop(0,j-m1+1)=0
	end do

	icount=0
	do i=ndrop_used+1,ndrop_used+nccn

c. randomly assign j_cell for drop(i)
c. also find x(i) corrosponding to this j_cell for drop(i)
c. note: in here x(i)=0 when j=M1, this is for consistent
c. the x(i) definition in d.f
c
 330    continue
Copen(28,file='assigd2_new2.dat')
Copen(28,file='assigd2_new2c.dat') -- old one--060796

	open(28,file='assigd2_100m.dat')
	read(28,*)rand_no
	icount=icount+1
	j=int(rand_no*(m3-m2+1))+m2
cj=int(rand()*(m3-m2+1))+m2
	if((j.gt.m3).or.(j.lt.m2))then
	write(79,*)'j exceed m2m3'
	endif
	x(i)=(j-m1)*delta_x+0.5*delta_x
	if(x(i).gt.BL)then
	  x(i)=x(i)-BL
	endif
	pjw=x(i)/delta_x
Cj=INT(pjw)+M1
	j=INT(pjw)+M1
	jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
c
c
c
c 12-27-96 comment out the drops assigning at different cell
c

	 if(jcell_pop(0,j-m1+1).ge.2)then
	 jcell_pop(0,j-m1+1)=1
	 goto 330
	 endif
	jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
	end do
	
	write(*,*)'icount in assigd2.f is', icount
	return
	end












