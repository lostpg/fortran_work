program main
implicit none
integer :: a(10)=(/1,2,3,4,5,6,7,8,9,0/)
write(*,'(10i2)') a
a(3:5)=a(8:10)
write(*,'(10i2)') a
end 
