PROGRAM EX0403
IMPLICIT NONE

INTEGER :: a,b
DO a=1,9,1
  DO b=1,a,1
    write(*,'(I1,A1,I1,A1,I2,A2,$)') b, '*', a, '=', a*b,"  "
  END DO
  write(*,*) ''
END DO
END

