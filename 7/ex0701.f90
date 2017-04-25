PROGRAM EX0701
IMPLICIT NONE

INTEGER :: n, i, j,ib, jb, m, p, q
INTEGER, ALLOCATABLE:: a(:,:)
READ(*,*) n
ALLOCATE(a(n,n))
a = RESHAPE((/(0,i=1,n*n,1)/),(/n,n/))
a(1,(n+1)/2) = 1
j = (n + 1) / 2
i = 1
DO m=2,n*n,1
  ib = i
  jb = j
  IF (i-1 < 1) THEN
    i = n
  ELSE
    i = i - 1
  END IF
  IF (j+1 > n) THEN
    j = 1
  ELSE
    j = j + 1
  END IF
  IF (a(i,j) /= 0) THEN
    j = jb  
    i = ib + 1
  END IF
  a(i,j) = m
END DO
DO q=1,n,1
  DO p=1,n,1
    WRITE(*,'(1X,I3)',advance='no') a(q,p)
  END DO
  WRITE(*,*) ""
END DO
END
