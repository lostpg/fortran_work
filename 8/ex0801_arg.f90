SUBROUTINE my_son(mat,n)
! Gauss elimination
! to change a matrix to an uppeeeeeeeeer triangular one.
! zero_judge: when elements in a column that are under the upper triangle are
! all-zero,
! jump to the next column.
! swap,l: when a diagonal element is zero, swap this raw with another 
!         raw whose corresponding element not zero.
! swap_time: times doing the raw-swap. A swap means a minor sign multiplied to
! matrix.???? (maybe)
IMPLICIT NONE
INTEGER :: n, i, j, l, swap, all_zero, swap_time=0
REAL :: mat(n,n), c, zero_judge
DO i=1, n-1
  zero_judge = 0.0
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
!     Swap two raws.
      DO swap=1,n
        c = mat(i,swap)
        mat(i,swap) = mat(l,swap)
        mat(l,swap) = c
      END DO
    END IF
    c = mat(j,i) / mat(i,i)
    mat(j,i:n) = mat(j,i:n) - c * mat(i,i:n)
  END DO
END DO
! IF (MOD(swap_time, 2) == 1) mat = -mat
END SUBROUTINE

PROGRAM EX0801_ARG
IMPLICIT NONE
INTEGER :: n = 0
INTEGER :: p
REAL, ALLOCATABLE :: mat(:,:), mat_input(:)
REAL :: rand
CHARACTER(LEN=128) :: arg

! Input length of a matrix, 0 for a fixed sample of 4*4'
CALL GETARG(1,arg)
READ(arg,'(I2)')n

IF (n==0) THEN
  n = 4
  ALLOCATE(mat_input(n*n))
  mat_input = (/ 1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16 /)
ELSE
  ! generate a seed and some random number then multiply and floor them
  ! use them as matrix element
  ALLOCATE(mat_input(n*n))
  CALL RANDOM_SEED()
  DO p=1,n*n
    CALL RANDOM_NUMBER(rand)
    mat_input(p) = FLOOR(20 * rand)
  END DO
END IF
ALLOCATE(mat(n,n))
mat = RESHAPE(mat_input,(/n,n/))
DO p=1,n
  WRITE(*,'(20(1X,F8.3))') mat(p,1:n)
END DO

CALL my_son(mat,n)

WRITE(*,'(A)') "Output"
DO p=1,n
  WRITE(*,'(20(1X,F8.3))') mat(p,1:n)
END DO
DEALLOCATE(mat)
DEALLOCATE(mat_input)
END PROGRAM
