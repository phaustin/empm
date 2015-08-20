
c
c This subprogram calculate the averaged rms and mean of TH1,
c TH2,Th3,Th4 at different realizations
c

	subroutine average_2(Th1ave,TH1int,Th2ave,Th2int
     $  ,TH3ave,Th3int,TH4ave,th4int,ip,mr,mip)

	real*8 Th1ave(0:mip),Th2ave(0:mip),Th3ave(0:mip),Th4ave(0:mip)
	real*8 Th1int(0:mip),Th2int(0:mip),Th3int(0:mip),Th4int(0:mip)

        TH1ave(ip)=TH1ave(ip)*(MR-1)/MR+TH1int(ip)/MR
        Th2ave(ip)=TH2ave(ip)*(MR-1)/MR+TH2int(ip)/MR
        TH3ave(ip)=TH3ave(ip)*(MR-1)/MR+TH3int(ip)/(MR)
        TH4ave(ip)=TH4ave(ip)*(MR-1)/MR+Th4int(ip)/(MR)
        RETURN
        END
