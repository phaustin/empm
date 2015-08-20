
      INTEGER FUNCTION INDEXR( V, NE, TE, LF )
C
C         *INDEXR* FINDS THE INDEX OF THE ELEMENT OF TABLE TE THAT IS
C         JUST LESS THAN OR EQUAL TO V.
C
C         GLOSSARY
C     V  = VARIABLE
C     NE = NUMBER OF ELEMENTS IN TABLE TE
C     TE = TABLE OF ELEMENTS ARRANGED IN ASCENDING OR DESCENDING ORDER
C     LF = LOGICAL FLAG
C
C     DIMENSION TE(1 )
	real*8 TE(NE),v
	LOGICAL LF


c	do i=1,NE
c	write(*,*)TE(i)
c	enddo



C
C
C         ORDER TEST
      IF ( TE(1) .GT. TE(2) ) GO TO 7
C
C         EXTREME TESTS
      IF ( V .GE. TE(1) ) GO TO 1
      LF = .TRUE.
      I = 1
      GO TO 14
C
   1  IF ( V .LT. TE(NE) ) GO TO 2
      LF = .TRUE.
      I = NE
      GO TO 14
C
C         INITIALIZATIONS
   2  LF = .FALSE.
      ND = 1
C
   3  ND = ND + ND
      IF ( ND .GE. NE ) GO TO 4
      GO TO 3
C
   4  ND = ND / 2
      I = ND
C
C         BISECTION LOOP
   5  ND = ND / 2
      IF ( ND .LE. 0 ) GO TO 6
      J = MIN0( NE, I )
      IF ( V .GT. TE(J) ) I = I + ND
      IF ( V .LT. TE(J) ) I = I - ND
      GO TO 5
C
   6  IF ( I .GE. NE ) GO TO 14
      IF ( V .GT. TE(I+1) ) I = I + 1
      IF ( V .LT. TE(I) ) I = I - 1
C
      GO TO 14
C
C         EXTREME TESTS
   7  IF ( V .GE. TE(NE) ) GO TO 8
      LF = .TRUE.
      I = NE
      GO TO 14
C
   8  IF ( V .LT. TE(1) ) GO TO 9
      LF = .TRUE.
      I = 1
      GO TO 14
C
C         INITIALIZATIONS
   9  LF = .FALSE.
      ND = 1
C
  10  ND = ND + ND
      IF ( ND .GE. NE ) GO TO 11
      GO TO 10
C
  11  ND = ND / 2
      I = ND
C
C         BISECTION LOOP
  12  ND = ND / 2
      IF ( ND .LE. 0 ) GO TO 13
      J = MIN0( NE, I )
      IF ( V .GT. TE(J) ) I = I - ND
      IF ( V .LT. TE(J) ) I = I + ND
      GO TO 12
C
  13  IF ( I .GE. NE ) GO TO 14
      IF ( V .GT. TE(I-1) ) I = I - 1
      IF ( V .LT. TE(I) ) I = I + 1
      I = MAX0( 1, I - 1 )
C
  14  INDEXR = MIN0( NE - 1, I )
C
      RETURN
      END
C$  FINTRP
c     real*8 FUNCTION FINTRP ( MODE, X, X1, F1, X2, F2 )
C
C         *FINTRP* INTERPOLATES BETWEEN X1 AND X2 USING A FUNCTIONAL
C         FORM THAT DEPENDS UPON THE SPECIFIED MODE. MODES - -
C     1 = LINEAR - - A * X + B
C     2 = EXPONENTIAL - - A * EXP( B * X )
C     3 = POWER LAW - - A * X**B
C
c     GO TO ( 10, 20, 30 ), MODE
C
C         LINEAR INTERPOLATION
C
c 10  F = F1 + ( F1 - F2 ) * ( X - X1 ) / ( X1 - X2 )
c     GO TO 40
C
C         EXPONENTIAL INTERPOLATION
C
c 20  F = F1 * ( F1 / F2 )**( ( X - X1 ) / ( X1 - X2 ) )
c     GO TO 40
C
C         POWER LAW INTERPOLATION
C
c 30  IF ( X1 .LE. 0. ) GO TO 20
c     F = F1 * ( X / X1 )**( ALOG( F1 / F2 ) / ALOG( X1 / X2 ) )
C
c 40  FINTRP = F
C
c     RETURN
c     END

