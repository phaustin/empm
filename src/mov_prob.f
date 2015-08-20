
        SUBROUTINE MOV_PROB(M1,M4,pNmin,pNmax
     $      ,A,PDF,SCALAR,qwcell,qwbin)
	integer qwcell, qwbin
        real A(qwcell), PDF(qwbin),SCALAR(qwbin)
	real pNmin, pNmax

        NBIN=60
        DO K=1,NBIN
         PDF(K)=0.
        END DO
c
c       PDFAREA: the total area of PDF
c       JJ: locate the A(K) into approciate bin #
c       pNmin: min. of scalar value
c       pNmax: max. of scalar value
c
        pNlength=pNmax-pNmin
        PDFAREA=0.
        DO K=M1,M4
          JJ=INT(NBIN*((A(K)-pNmin)/pNlength))+1
          if(jj.gt.NBIN.or. jj.le.0)then
            write(6,*)'mov_prob.f jj,a(k)=',jj,a(k)
Ccall flush(6)
            JJ=NBIN
          endif
c
c accumulate the count # at each interval of JJ
c
          PDF(JJ)=PDF(JJ)+1.
        END DO
c
c       calculate the total PDFAREA
c
        DDX=(pNlength)/float(NBIN)
        DO K=1,NBIN
        PDFAREA=PDFAREA+PDF(K)*DDX
        END DO
c
c       normlaized the PDF and calculate the real scalar
c       value at each bin#
c
        DO K=1,NBIN+1
        PDF(K)=PDF(K)/PDFAREA
        SCALAR(K)=(K-1.)*DDX+pNmin
        END DO

        RETURN
        END

