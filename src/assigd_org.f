
c. This subroutine randomly assigns the droplet position and
c. remove the drops when they are in the parcel. (j.ge.M2) 
c. and (j.le.M3)

	subroutine assign(ndrop,ngrid,jcell_pop,delta_x
     1  ,m1,x,dsize,csize,icat,M2,M3,BL,igrid)
	parameter(idimen=5000)
	
	integer*4 igrid
	double precision rand

	dimension jcell_pop(0:10,igrid)
        real*8 dsize(idimen),csize(idimen),rnr(idimen)
	real*8  x(idimen)
	real*8 delta_x
	integer icat(idimen)
	real*8 pjw

c. initialize jcell_pop(0,j) which is the index
c. used in drop_map.f

	do j=1,ngrid
	jcell_pop(0,j)=0
	end do

	ncut=0
	icount=0

c. ncut : a active iteration index to update the droplet
c. index when they fall in the diluted parcel
	do k=1,ndrop

c. ndrop: original droplet input number

 	ichange=k
	i=k-ncut
	
c. randomly assign j_cell for drop(i)
c. also find x(i) corrosponding to this j_cell for drop(i)
c. note: in here x(i)=0 when j=M1, this is for consistent
c. the x(i) definition in d.f
c
 330    continue
Copen(25,file='assigd.dat') --060796 -earlier one
Copen(25,file='assigd.dat1')
	open(25,file='assigd_100m.dat')

Copen(25,file='a2.dat')
	read(25,*)rand_no
	icount=icount+1
	j=int(rand_no*ngrid)+m1
	if((j.ge.m2).and.(j.le.m3))goto 330
	pjw0=float(j-m1)
	x(i)=pjw0*delta_x+0.5*delta_x
	if(x(i).gt.BL)then
	  x(i)=x(i)-BL
	endif
	j_old=j
	dsize(i)=dsize(ichange)
	csize(i)=csize(ichange)
	rnr(i)=rnr(ichange)
	icat(i)=icat(ichange)
c.
c. make the drop's index jcell_pop
c. note check this: j become j-m1 in d.f
	pjw=x(i)/delta_x
	j=INT(pjw)+M1
	jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1

c
c 12-27-96 comment out the drops assigning at different cell
c

	if(jcell_pop(0,j-m1+1).ge.2)then
 	jcell_pop(0,j-m1+1)=1
	goto 330
	endif

	jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
	end do
	write(*,*)'icount=',icount

	ndrop=ndrop-ncut

	
	return
	end

