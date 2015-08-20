c. This program calcualte the accumulated concerntration
c. of the initial droplet distribution

	subroutine accu(DX,BL,dis,www,new_con,mass_drop,ndrop)

        parameter(NE=49,NE1=5000)
        real*8  mass_drop(NE1),dis(NE),v,TE(NE),new_con(NE1)
	real*8  con_drop(NE1)
        real*8      www
	integer ndrop
        double precision rand

        logical LF

        
	open (unit=81,file='rnr_data_new')
Copen (unit=81,file='rnr_data900')
Copen (unit=81,file='rnr_data_new2')
        do i=1,NE
        read(81,*)mass_drop(i),con_drop(i)
        end do
        do i=1,NE
        con_drop(i)=con_drop(i)*1.24
        enddo

c. calculate the culmulative distribution of rnr(i)
        TE(1)=con_drop(1)
        do i=2,NE
        TE(i)=TE(i-1)+con_drop(i)
        end do

        do i=1,NE
        write(36,*)i,con_drop(i),TE(I)
        end do

c. calsulate the total drop number in the LEM domain.

c	BL=1. !m
c	DX=.01!m
c	volume=DX*DX*BL
	volume=.001*.001*BL
	write(36,*)volume, TE(NE)
	www=volume*TE(NE)
	ndrop=idint(www)
	write(36,*)'number of drop is', ndrop, www,www/ndrop
	if((www/ndrop).gt.2.)then
	write(*,*)'too many drops, check acc.f'
	stop
	endif


c. use indexr to find where is the index for v
c. also find this distribution
        do i=1,NE
        dis(i)=0.
        end do
        
        do i=1,ndrop
c v=rand()*TE(NE)
	v=(float(i)-0.5)/float(ndrop)*TE(NE)
        in=indexr(v,NE,TE,LF)
Cwrite(*,*)i,'v=',v,'index=',in,LF
        dis(in)=dis(in)+1.
        end do

c	open(unit=9,file='acc.dat7')
        do i=1,NE
	new_con(i)=dis(i)/volume
        write(76,*)i,dis(i),new_con(i)
	
        end do
c	write(9,*)   
c	stop
c	end

	close(81)
	return
	end
