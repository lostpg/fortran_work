PROGRAM EX0501
IMPLICIT NONE

! integrate (x**2 + sin(x))dx from -2 to 2,
! using Composite Trapezoidal Integration Method:
! integrate f(x) dx from a to b =
! h / 2 *(f(a) + f(b) + 2 Sigma(i from 1 to i-1)(f(a + i*h)))
! part integral region into n pieces of trapezoids
! which have the same height h = (b-a)/n.
! choose different n, compare the result with the
! accurate result: 16/3, approx: 5.3333333333333...

INTEGER(KIND=8) :: nn(6) = (/ 100,1000,5000,10000,1000000,10000000 /), i, n
INTEGER(KIND=4) :: ii
REAL(KIND=16) :: a, b, x, integral, h
a = -2.0
b = 2.0

DO ii=1,6,1
  n = nn(ii)
  integral = 0.0
  x = a
  h = (b - a)/n
  DO i=1,n-1,1
    x = x + h
    integral = integral + x * x + sin(x)
  END DO
  integral = h * ( a*a + sin(a) + b*b + sin(b) + 2 * integral) / 2
  WRITE(*,'(A,I8,A,F15.12)') 'n = ', n, ' , result is', integral
END DO
END
