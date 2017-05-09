SUBROUTINE my_son(mat,n)
! Gauss elimination
! to change a matrix to an uppeeeeeeeeer triangular one.
! zero_judge: when elements in a column that are under the upper triangle are all-zero,
! jump to the next column.
! swap,l: when a diagonal element is zero, swap this raw with another 
!         raw whose corresponding element not zero.
! swap_time: times doing the raw-swap. A swap means a minor sign multiplied to
! matrix.???? (maybe)
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
! IF (MOD(swap_time, 2) == 1) mat = -mat
END SUBROUTINE

PROGRAM EX0801
IMPLICIT NONE
CHARACTER(LEN=128) :: arg
INTEGER :: n
INTEGER :: p, q, i
REAL, ALLOCATABLE :: mat(:,:), mat_input(:)
REAL :: rand

! WRITE(*,'(A,/,A)') 'Input length of a matrix, 0 for a fixed sample of 4*4:'
CALL GETARG(1,arg)
READ(arg,'(I2)')n

IF (n==0) THEN
  n = 4
  ALLOCATE(mat(n,n))
  ALLOCATE(mat_input(n*n))
  mat_input = (/ 1,5,9,13,2,6,10,14,3,7,11,15,4,8,12,16 /)
  GOTO 10
END IF

! generate a seed and some random number then multiply and floor them
! use them as matrix element
ALLOCATE(mat(n,n))
ALLOCATE(mat_input(n*n))
CALL RANDOM_SEED()
DO i=1,n*n
  CALL RANDOM_NUMBER(rand)
  mat_input(i) = FLOOR(20 * rand)
END DO
GOTO 10

10    mat = RESHAPE(mat_input,(/n,n/))
DO q=1,n,1
  DO p=1,n,1
    WRITE(*,'(1X,F7.3)',advance='no') mat(q,p)
  END DO
  WRITE(*,*) ""
END DO
WRITE(*,'(A)') "Output"

CALL my_son(mat,n)

DO q=1,n,1
  DO p=1,n,1
    WRITE(*,'(1X,F8.3)',advance='no') mat(q,p)
  END DO
  WRITE(*,*) ""
END DO
DEALLOCATE(mat)
DEALLOCATE(mat_input)
END PROGRAM
