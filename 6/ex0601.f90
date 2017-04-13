PROGRAM EX0601
IMPLICIT NONE
INTEGER :: a(2,2,2,2) = (/0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15/)
INTEGER :: i,j,k,l
DO i=1,2,1 
  DO j=1,2,1 
    DO k=1,2,1 
      DO l=1,2,1 
        WRITE(*,'(A,I1,I1,I1,I1,3X,I2)') 'at ',l,k,j,i,a(l,k,j,i)
      END DO
    END DO
  END DO
END DO
END
