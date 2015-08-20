
c
c THis subprogram is to find the mean and rms of the qv,
c Qc, S, and Temperature
c

	subroutine mean_rms(m1,m4,TH1,Th2,TH3,Th4,Th1_mean,TH2_mean,
     $        TH3_mean,Th4_mean,Th1_rms,Th2_rms,TH3_rms,TH4_rms,ip,
     $          mip,igrid)

	integer*4 igrid
        real*8 Th1(igrid),Th2(igrid),Th3(igrid),Th4(igrid)
	real*8 th1_mean(0:mip),th2_mean(0:mip),th3_mean(0:mip)
     $        ,th4_mean(0:mip)
	real*8 th1_rms(0:mip),th2_rms(0:mip),th3_rms(0:mip),
     $         th4_rms(0:mip)

	sum1=0.
	sum2=0.
	sum3=0.
	sum4=0.
	
	DO i=m1,m4
	sum1=sum1+TH1(i)
	sum2=sum2+TH2(i)
	sum3=sum3+Th3(i)
	sum4=sum4+TH4(i)
	end do
	
	N=m4-m1+1
	th1_mean(ip)=sum1/float(N)
	th2_mean(ip)=sum2/float(n)
	th3_mean(ip)=sum3/float(N)
	th4_mean(ip)=sum4/float(N)

	prim1=0.
	prim2=0.
	prim3=0.
	prim4=0.

	Do i=m1,m4
	prim1=prim1+(TH1(i)-th1_mean(ip))**2.
	prim2=prim2+(TH2(i)-th2_mean(ip))**2.
	prim3=prim3+(TH3(i)-TH3_mean(ip))**2.
	prim4=prim4+(TH4(i)-TH4_mean(ip))**2.
	end do

	TH1_rms(ip)=(prim1/float(n))**.5
	TH2_rms(ip)=(prim2/float(n))**.5
	TH3_rms(ip)=(prim3/float(n))**.5
	Th4_rms(ip)=(prim4/float(n))**.5
c	write(*,*)'mean and rms'
c	write(*,*)th1_mean(ip),Th2_mean(ip),Th3_mean(ip),Th4_mean(ip)
c	write(*,*)TH1_rms(ip),TH2_rms(ip),Th3_rms(ip),Th4_rms(ip)
	return 
	end
