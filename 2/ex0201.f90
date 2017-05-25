PROGRAM EX0201
IMPLICIT NONE
INTEGER(KIND=4) :: a
REAL(KIND=4) :: b
REAL(KIND=8) :: c
CHARACTER(LEN=10) :: d
READ(*,*) a,b,c,d
WRITE(*,*) a,b,c
WRITE(*,'(F5.3,TR3,F10.8)') b,c
WRITE(*,'(I5,2X,I2,2X,I1)') a,a,a
WRITE(*,'(E10.3,2X,E15.8)') b,c
WRITE(*,'(ES10.3,2X,ES15.8)') b,c
WRITE(*,'(A)') d
WRITE(*,'(A1,/,A2,/,A10)') d,d,d
END
