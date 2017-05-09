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
REAL :: mat(n,n), c, zero_judge, mat_swap(n,n)
mat_swap = RESHAPE((/(0,swap=1,n*n)/),(/n,n/))
FORALL(swap=1:n) mat_swap(swap,swap) = 1
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
!      Swap two raws.
!      DO swap=1,n
!        c = mat(i,swap)
!        mat(i,swap) = mat(l,swap)
!        mat(l,swap) = c
!      END DO
!      A new way to swap to raws?
      mat_swap(i,i) = 0
      mat_swap(l,l) = 0
      mat_swap(i,l) = 1
      mat_swap(l,i) = 1
      mat = matmul(mat,mat_swap)
      mat_swap(i,i) = 1 
      mat_swap(l,l) = 1
      mat_swap(i,l) = 0
      mat_swap(l,i) = 0
    END IF
    c = mat(j,i) / mat(i,i)
!    DO k=i,n
!      mat(j,k) = mat(j,k) - c * mat(i,k)
!    END DO
    ! use implicit cycle! We eliminate the k!
    mat(j,i:n) = mat(j,i:n) - c * mat(i,i:n)
  END DO
END DO
IF (MOD(swap_time, 2) == 1) mat = -mat
END SUBROUTINE

PROGRAM EX0801
IMPLICIT NONE
INTEGER :: n=4
INTEGER :: p, q, i
REAL, ALLOCATABLE :: mat(:,:), mat_input(:)
REAL :: rand
WRITE(*,'(A)',ADVANCE='no') 'Input dim of a matrix, 0 for a fixed sample of 4*4: '
READ(*,*) n

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

10    mat = RESHAPE(mat_input,(/n,n/))
DO q=1,n 
  WRITE(*,'(20(1X,F8.3))',advance='no') (mat(q,p),p=1,n)
  WRITE(*,*) ""
END DO

CALL my_son(mat,n)

WRITE(*,'(A)') "Output"
DO q=1,n
  WRITE(*,'(20(1X,F8.3))',advance='no') (mat(q,p),p=1,n)
  WRITE(*,*) ""
END DO
DEALLOCATE(mat)
DEALLOCATE(mat_input)
END PROGRAM
