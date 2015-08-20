c***************************************************************************
c
c This is a subroutine to average the  droplet spectrum at realizations
c In here it is the PDF.
c STATint: the input value of instaneous value at current realization
c STATave: the ave. value at different real.
c MR: number of realization
c
c***************************************************************************
        SUBROUTINE RAVERAGE( STATave, STATint, K,Ipe, MR)
        real*8 STATave(0:20,101), STATint(0:20,101)
        DO JJ=1,K+1
        STATave(Ipe,JJ)=STATave(Ipe,JJ)*(MR-1)/MR+(STATint(IPe,JJ)/MR)
        END DO
        RETURN
        END
