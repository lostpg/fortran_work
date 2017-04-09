PROGRAM TEST
IMPLICIT NONE

INTEGER(KIND=8) :: ii, i, n
REAL(KIND=4) :: a, b, x, integral, h
a = -2.0
b = 2.0

DO ii=1000,10000,1000
  n = ii
  integral = 0.0
  x = a
  h = (b - a)/n
  DO i=1,n-1,1
    x = x + h
    integral = integral + x * x + sin(x)
  END DO
  integral = h * ( a*a + sin(a) + b*b + sin(b) + 2 * integral) / 2
  WRITE(*,'(A,I7,A,F12.9)') 'n = ', n, ' , error is ', integral - 5.333333333
END DO

DO ii=10000,100000,10000
  n = ii
  integral = 0.0
  x = a
  h = (b - a)/n
  DO i=1,n-1,1
    x = x + h
    integral = integral + x * x + sin(x)
  END DO
  integral = h * ( a*a + sin(a) + b*b + sin(b) + 2 * integral) / 2
  WRITE(*,'(A,I7,A,F15.12)') 'n = ', n, ' , error is ', integral - 5.333333333
END DO

DO ii=100000,1000000,100000
  n = ii
  integral = 0.0
  x = a
  h = (b - a)/n
  DO i=1,n-1,1
    x = x + h
    integral = integral + x * x + sin(x)
  END DO
  integral = h * ( a*a + sin(a) + b*b + sin(b) + 2 * integral) / 2
  WRITE(*,'(A,I7,A,F15.12)') 'n = ', n, ' , error is ', integral - 5.333333333
END DO

END
