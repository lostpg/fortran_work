PROGRAM EX0303
CHARACTER(LEN=3) :: a,b
CHARACTER(LEN=6) :: c
a = "one"
b = "two"

c = a//b
WRITE(*,*) c

c = a(1:2)//b(3:3)
WRITE(*,*) c

END

