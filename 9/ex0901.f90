MODULE WTF
CONTAINS
  SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, ALLOCATABLE :: seed(:)
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
    CALL SYSTEM_CLOCK(COUNT=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(put = seed)
    DEALLOCATE(seed)
  END SUBROUTINE init_random_seed
  SUBROUTINE my_son_4(n)
    IMPLICIT NONE
    INTEGER ::  i, inside, n
    REAL(KIND=4) :: a(2), prob
    inside = 0
    prob = 0.0
    CALL init_random_seed()
    DO i=1,n
      CALL random_number(a)
      IF (a(1)*a(1)+a(2)*a(2)<=1) inside = inside + 1
    END DO
    prob = REAL(inside) / n
    WRITE(*,'(F10.7)') prob * 4
  END SUBROUTINE
  SUBROUTINE my_son_8(n)
    IMPLICIT NONE
    INTEGER ::  i, inside, n
    REAL(KIND=8) :: a(2), prob
    inside = 0
    prob = 0.0
    CALL init_random_seed()
    DO i=1,n
      CALL random_number(a)
      IF (a(1)*a(1)+a(2)*a(2)<=1) inside = inside + 1
    END DO
    prob = REAL(inside) / n
    WRITE(*,'(F15.12)') prob * 4
  END SUBROUTINE
END MODULE

PROGRAM EX0901
USE WTF
IMPLICIT NONE
INTEGER(KIND=4) :: type,n
type = 4
READ(*,*)type, n
IF (type==4) THEN
  CALL my_son_4(n)
ELSE IF (type==8) THEN
  CALL my_son_8(n)
END IF
END
