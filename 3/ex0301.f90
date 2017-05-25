PROGRAM EX0301
IMPLICIT NONE

INTEGER(KIND=4) :: y,m,d,date

10    WRITE(*,*) 'Enter the date of interest. Format as "2014 2 28" for "year &
&month day"...' 
READ(*,*) y,m,d
WRITE(*,'(A,I4,A,I2,A,I2)') 'You have inputted year ',y,', month ',m,', day ',d
SELECT CASE(m)
CASE(1)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = d
  END IF
CASE(2)
  IF (d < 0 .OR. d > 29) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 31 + d
  END IF
CASE(3)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 59 + d
 END IF
CASE(4)
  IF (d < 0 .OR. d > 30) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 90 + d
  END IF
CASE(5)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 120 + d
  END IF
CASE(6)
  IF (d < 0 .OR. d > 30) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 151 + d
  END IF
CASE(7)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 181 + d
  END IF
CASE(8)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 212 + d
  END IF
CASE(9)
  IF (d < 0 .OR. d > 30) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 243 + d
  END IF
CASE(10)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 273 + d
 END IF
CASE(11)
  IF (d < 0 .OR. d > 30) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 304 + d
  END IF
CASE(12)
  IF (d < 0 .OR. d > 31) THEN
    WRITE(*,*) 'Input data error, try again...'
    GOTO 10
  ELSE
    date = 334 + d
  END IF
CASE DEFAULT
  WRITE(*,*) 'Input data error, try again... '
  GOTO 10
END SELECT

IF ((m == 2 .AND. d ==29) .AND. .NOT.(MOD(y,400) == 0 .OR. (MOD(y,4) == &
&0 .AND. MOD(y,100) /= 0))) THEN
  WRITE(*,*) 'Input data error, try again... '
  GOTO 10
END IF

IF (m > 2 .AND. (MOD(y,400) == 0 .OR. (MOD(y,4) == 0 .AND. MOD(y,100) /= 0))) THEN
  date = date + 1
END IF
WRITE(*,'(A,I3,A)') 'It is the ', date,'th day of the year.'
100    STOP
END
