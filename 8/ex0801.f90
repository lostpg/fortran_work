SUBROUTINE gauss_elimination(mat,n)
! Gauss elimination
! to change a matrix to an uppeeeeeeeeer triangular one.
! zero_judge: when a column is all-zero, jump to the next column
! swap,l: when a diagonal element is zero, swap this raw with another 
!         raw whose corresponding element not zero.
! swap_time: times doing the raw-swap.
IMPLICIT NONE
INTEGER :: n, i, j, k, l, swap, all_zero, swap_time=0
REAL :: mat(n,n), c, zero_judge
DO i=1, n-1
  zero_judge = 0
  DO all_zero=i+1,n
    zero_judge = zero_judge +  mat(all_zero,i)
  END DO
  IF (zero_judge == 0) CYCLE
  DO j=i+1,n
    l = i
    DO WHILE (mat(l,i)==0)
      l = l + 1
    END DO
    IF (l/=i) THEN
      swap_time = swap_time + 1
      DO swap=1,n
        c = mat(i,swap)
        mat(i,swap) = mat(l,swap)
        mat(l,swap) = c
      END DO
    END IF
    c = mat(j,i) / mat(i,i)
    DO k=i,n
      mat(j,k) = mat(j,k) - c * mat(i,k)
    END DO
  END DO
END DO
IF (MOD(swap_time, 2) == 1) mat = -mat
END SUBROUTINE

PROGRAM EX0801
IMPLICIT NONE
INTEGER :: n=4
INTEGER :: p, q
REAL :: mat(4,4)=(/ 1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16 /)

DO q=1,n,1
  DO p=1,n,1
    WRITE(*,'(1X,F7.3)',advance='no') mat(q,p)
  END DO
  WRITE(*,*) ""
END DO
WRITE(*,*) "Output"

CALL gauss_elimination(mat,n)

DO q=1,n,1
  DO p=1,n,1
    WRITE(*,'(1X,F7.3)',advance='no') mat(q,p)
  END DO
  WRITE(*,*) ""
END DO
END PROGRAM
