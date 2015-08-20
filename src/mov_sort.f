
C	This program sorting the droplet distributions 
C	into a certain number of catergories.

	subroutine mov_rsp(NE,radius_drop,con_drop,rdr,mov_fcon
     $   ,NBIN,NARRAY, r_mean, r2_mean, r3_mean)
	
	real*8 rdr(101)
        real*8 radius_drop(NARRAY),con_drop(NARRAY)
	real   mov_fcon(36)
        real   r_mean(36), r2_mean(36), r3_mean(36)
     
	Ra_max=35.e-6
	Ra_min=0.
	w0=0.
	NBIN=35

	call DROPPROB2(1,NE,Ra_min,Ra_max,radius_drop,con_drop,
     1  mov_fcon,rdr,NBIN,DDX,NARRAY,
     2  r_mean, r2_mean, r3_mean)

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
        SUBROUTINE DROPPROB2(M1,M4,pNmin,pNmax,A,con_drop,
     1  mov_fcon,SCALAR,NBIN,DDX,NARRAY, r_mean, r2_mean, r3_mean)

        real*8 A(NARRAY), PDF(101),SCALAR(101)
	real*8 con_drop(NARRAY)
	real mov_fcon(36)
* add variables for r_mean, r2_mean and r3_mean
        real r_sum(36), r2_sum(36), r3_sum(36)
        real r_mean(36), r2_mean(36), r3_mean(36)
        integer n_r(36)
        
        DO K=1,NBIN
c
c set initail PDF values zero
c
        mov_fcon(k)=0.
* initialize r_sum and r_mean, and n_r
        r_sum(k)  = 0.
        r2_sum(k) = 0.
        r3_sum(k) = 0.
        n_r(k)    = 0
       
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
        
        if(jj.ge.36.or. jj.le.0)then
           write(*,*)'In sort, jj,a(k)',jj,a(k)
           jj=1
         endif
c
c accumulate the count # at each interval of JJ
c
         mov_fcon(jj)=(mov_fcon(jj)+con_drop(k))
* also accumulate for r, r2, and r3
         r_sum (jj) = a(k) + r_sum(jj)
         r2_sum(jj) = a(k) * a(K) + r2_sum(jj)
         r3_sum(jj) = a(k) * a(k) * a(k) + r3_sum(jj)
         n_r(jj) = n_r(jj) + 1 
        END DO
c
c       calculate the total PDFAREA
c
	
	DO K=1,NBIN+1
	  mov_fcon(k)=mov_fcon(k)/(1.0e6)
          SCALAR(K)=(K-1.)*DDX*1.e-6+pNmin
        END DO
* add calculations on r, r2 and r3
        do k = 1, NBIN + 1
          if( n_r(k) .eq. 0) then
            r_mean(k) = 0.
            r2_mean(k) = 0.
            r3_mean(k) = 0.
          else
            r_mean(k) = r_sum(k) / float( n_r(k))
            r2_mean(k) = r2_sum(k) / float( n_r(k))
            r3_mean(k) = r3_sum(k) / float( n_r(k))
          endif
        enddo

        RETURN
        END



