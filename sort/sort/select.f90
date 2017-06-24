subroutine selectsort(a,n)
implicit none
integer :: n, i, j, key, temp
integer :: a(n)
do i=1,n-1
  key = i
  do j=i+1,n
    if (a(key) > a(j)) key = j
  end do
  if (key /= i) then
    temp = a(key)
    a(key) = a(i)
    a(i) = temp
  end if
end do
end subroutine

subroutine runit(a,na,b,nb,t1,t2)
implicit none
integer :: na, nb
integer :: a(na),b(nb)
real :: t1,t2
call selectsort(a,na)
call cpu_time(t1)
call selectsort(b,nb)
call cpu_time(t2)
end subroutine

program main
write(*,'(a)')'Selection sort'
call run()
end 

include 'help.f90' 
