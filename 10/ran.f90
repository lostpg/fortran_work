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

PROGRAM EX1001
IMPLICIT NONE
INTEGER :: data_len, loop, tik,loop2
LOGICAL :: bin, dec
REAL(KIND=4) :: time_start, time_stop, position_
REAL(KIND=8) :: sum_bin, sum_dec, get_num

loop=1000
loop2=2000
INQUIRE(IOLENGTH=data_len) get_num
CALL init_random_seed()

! search for data file. if not existed, create one.
INQUIRE(FILE='data.txt',exist=dec)
INQUIRE(FILE='data.dat',exist=bin)
sum_dec = 0.0
sum_bin = 0.0

IF(bin=.FAUSE.) THEN
  WRITE(*,'(A)')'Unformatted data file doesn''t exist! Creating...'
  OPEN(UNIT=9,FILE='data.dat',STATUS='NEW',FORM='UNFORMATTED',&
&      ACCESS='DIRECT',RECL=data_len)
  DO tik=1,loop
    CALL random_number(get_num)
    WRITE(UNIT=9,REC=tik) get_num
  END DO
  CLOSE(8)
  WRITE(*,'(A)')'data.dat created...'
END IF

IF(dec=.FAUSE.) THEN
  WRITE(*,'(A)')'Formatted data file doesn''t exist! Creating...'
  OPEN(UNIT=10,FILE='data.txt',STATUS='NEW',FORM='FORMATTED',&
&      ACCESS='DIRECT',RECL=20)
  DO tik=1,loop
    CALL random_number(get_num)
    WRITE(UNIT=9,'(F17.14)',REC=tik) get_num
  END DO
  CLOSE(9)
  WRITE(*,'(A)')'data.txt created...'
END IF

WRITE(*,'(A)')'Handling unformatted file...'
CALL CPU_TIME(time_start)
OPEN(UNIT=9,FILE='data.dat',STATUS='OLD',FORM='UNFORMATTED',&
 32 &      ACCESS='DIRECT',RECL=data_len)
DO tik=1,loop2
  CALL RANDOM_NUMBER(position_)
  READ(UNIT=9,REC=INT(position_ * loop)) get_num
  sum_bin =sum_bin + get_num
END DO
CALL CPU_TIME(time_stop)
WRITE(*,FMT=100)'time use: ',time_start-time_stop,', average: ',sum_dec/loop2,'.'
100 FORMAT(A,F10.7,A,F17.14,A)

WRITE(*,'(A)')'Handling formatted file...'
CALL CPU_TIME(time_start)
OPEN(UNIT=10,FILE='data.txt',STATUS='OLD',FORM='FORMATTED',&
 32 &      ACCESS='DIRECT',RECL=40)
DO tik=1,loop2
  CALL RANDOM_NUMBER(position_)
  READ(UNIT=10,REC=INT(position_ * loop)) get_num
  sum_dec = sum_dec + get_num
END DO
CALL CPU_TIME(time_stop)
WRITE(*,FMT=100)'time use: ',time_start-time_stop,', average: ',sum_dec/loop2,'.'
100 FORMAT(A,F10.7,A,F17.14,A)

END
