
C	This program sorting the droplet distributions 
C	into a certain number of catergories.

	subroutine rspec(NE,radius_drop,con_drop,rdr,fcon,ip,NBIN,NARRAY)
	
	real*8 fcon(0:20,101),rdr(101)
        real*8 radius_drop(NARRAY),con_drop(NARRAY)
c	real*8 Ra(NARRAY)

c	ntot=489
c	NE=46
c	NE=489

c       OPEN (UNIT=7,FILE='drop.data900')
     
c       do i=1,52
c       read(7,*)radius_drop(i)
c       end do

C

c601    format(x,e12.6)
c       read(7,*)(mass_drop(i),con_drop(i),i=1,NE)
c       write(*,602)(mass_drop(i),con_drop(i),i=1,NE)
c602    format(x,2(x,e12.6))
c       close(7)

cNE=489
c
Copen(unit=20,file='sort.dat')
Cdo i=1,NE
CCread(20,*)i,Ra(i)
Cwrite(*,*)i,Ra(i)
Cenddo
C
C
C       DX=1./300.
c
ccDo i=1,NE
cradius_drop(i)=Ra(i)
ccon_drop(i)=1./(BL*DX*DX)
cenddo


cIp=1

	Ra_max=35.e-6
	Ra_min=0.
c	NE=30
	w0=0.
	NBIN=35

	call DROPPROB(1,NE,Ra_min,Ra_max,radius_drop,con_drop,
     1  fcon,rdr,NBIN,Ip,DDX,NARRAY)
	

	dRa=(Ra_max-Ra_min)/(NBIN-1.)
c	write(*,*)'dRa=',dRa
c	Do i=1,30
	
c	enddo
	
c	write(*,*)(i,TE(i),i=1,NE)

c	do i=1,NE
c	dis(i)=0
c	enddo
	
	
c	do i=1,ntot
c	v=Ra(i)
c	in=indexr(v,NE,TE,LF)
c	dis(in)=dis(in)+1
c	end do
c	BL=1.
c	DX=1./300.

c	do i=1,NE
c	discon(i)=dis(i)/(BL*DX*DX)
c	enddo
	
c	w=0
c	do i=1,NE
c	w=w+discon(i)
c	enddo

c	do i=1,NE
c	write(*,*)i,(i-1)*dRa,dis(i),discon(i)
c	enddo
	
c	do i=1,46
c	con(i)=0.
c	dis(i)=0.
c	enddo

C calculate the initial droplet distributions

C	do i=1,46
c	v=radius_drop(i)
c	in=indexr(v,30,TE,LF)
c	dis(in)=dis(in)+1
c	con(in)=con_drop(i)+con(in)
c	enddo
	w0=0
	do i=1,NBIN
	
	w0=w0+fcon(ip,i)
	enddo

c	do i=1,NBIN
c	write(70,70)i,rdr(i),fcon(ip,i)
c	enddo
c	write(70,*)w0*DDX
c70	FORMAT(i3,2(e12.4))

	return
	end
	
c***************************************************************************
c
c       This is a subroutine to calculate the pdf distribution
c       of the scalar. There are 100 bins.
c
c       PDF: pwer density function
c       SCALAR: scalar values divided by 100 intervals
c       NBIN: bin numbers for PDF
c       Ipe: every period of one realization, if periods=25 iteration
c       then they are 25, 50, 75, 100...... iteration for periods 1,
c       2, 3, 4, ......
c
c***************************************************************************
        SUBROUTINE DROPPROB(M1,M4,pNmin,pNmax,A,con_drop,
     1  fcon,SCALAR,NBIN,Ipe,DDX,NARRAY)
        real*8 A(NARRAY), PDF(0:20,101),SCALAR(101)
	real*8 con_drop(NARRAY),fcon(0:20,101)
C	NBIN=100
c	write(*,*)'nbin=',nbin
        DO K=1,NBIN
c
c set initail PDF values zero
c
	fcon(ipe,k)=0.
c	PDF(Ipe,K)=0.
        END DO

	If((M4.GE.Narray).or.(M1.GE.Narray))then
	write(*,*)'M4 or M1 is > Narray',M4,M1,Narray
	endif



c
c       PDFAREA: the total area of PDF
c       JJ: locate the A(K) into approciate bin #
c       pNmin: min. of scalar value
c       pNmax: max. of scalar value
c
	

        pNlength=pNmax-pNmin
        DDX=(pNlength)/float(NBIN)*1.e6
        PDFAREA=0.
        DO K=M1,M4
        JJ=INT(NBIN*((A(K)-pNmin)/pNlength))+1
        if(jj.ge.60.or. jj.le.0)then
        write(*,*)'In sort, jj,a(k)',jj,a(k)
	jj=1
        endif
c
c accumulate the count # at each interval of JJ
c
c	write(*,*)fcon(Ipe,jj),con_drop(k),jj,DDX
c       PDF(Ipe,JJ)=PDF(Ipe,JJ)+1.
	fcon(Ipe,jj)=(fcon(Ipe,jj)+con_drop(k))
c	write(*,*)fcon(Ipe,jj),con_drop(k)
c	write(*,*)
        END DO
c
c       calculate the total PDFAREA
c
c	DDX=FLOAT(pNlength)/NBIN
c	DO K=1,NBIN
c	PDFAREA=PDFAREA+PDF(Ipe,K)*DDX
c	END DO
c
c       normlaized the PDF and calculate the real scalar
c       value at each bin#
c
	
c	volume=(1./300.)*(1/300.)
	DO K=1,NBIN+1
c       PDF(Ipe,K)=PDF(Ipe,K)/volume*(1./DDX)
	fcon(Ipe,k)=fcon(Ipe,k)/(1.0e6)
        SCALAR(K)=(K-1.)*DDX*1.e-6+pNmin
c	write(*,*)scalar(k),fcon(Ipe,k)
        END DO

        RETURN
        END



