 
        real*8 FUNCTION FINTRP ( MODE, X, X1, F1, X2, F2 )

	real*8 X,X1, F1, X2, F2
C
C         *FINTRP* INTERPOLATES BETWEEN X1 AND X2 USING A FUNCTIONAL
C         FORM THAT DEPENDS UPON THE SPECIFIED MODE. MODES - -
C     1 = LINEAR - - A * X + B
C     2 = EXPONENTIAL - - A * EXP( B * X )
C     3 = POWER LAW - - A * X**B
C

c       write(*,*)'X, X1, F1, X2, F2',X, X1, F1, X2, F2

      GO TO ( 10, 20, 30 ), MODE
C
C         LINEAR INTERPOLATION
C
  10  F = F1 + ( F1 - F2 ) * ( X - X1 ) / ( X1 - X2 )
      GO TO 40
C
C         EXPONENTIAL INTERPOLATION
C
  20  F = F1 * ( F1 / F2 )**( ( X - X1 ) / ( X1 - X2 ) )
      GO TO 40
C
C         POWER LAW INTERPOLATION
C
  30  IF ( X1 .LE. 0. ) GO TO 20
      F = F1 * ( X / X1 )**( DLOG( F1 / F2 ) / DLOG( X1 / X2 ) )
C
  40  FINTRP = F
c	write(*,*)'FINTRP',FINTRP
C
      RETURN
      END
