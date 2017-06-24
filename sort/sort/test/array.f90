program amin
implicit none
integer :: a(4)=(/1,2,3,4/)
print *, a
a(1:3)=a(2:4)
print *, a
end
