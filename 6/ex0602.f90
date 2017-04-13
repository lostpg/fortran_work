PROGRAM EX0602
INTEGER :: a(0:9) = (/1,2,3,4,5,6,7,8,9,10/),i
REAL :: xbar=0,xbar2=0
DO i=0,9,1
  xbar = xbar + REAL(a(i))
  xbar2 = xbar2 + REAL(a(i)) * REAL(a(i))
END DO
WRITE(*,*) 'The variance is', xbar2/10 - xbar*xbar/100
END
  



