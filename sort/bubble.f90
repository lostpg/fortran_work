subroutine bubblesort(a,n)
integer :: n
integer :: a(n)
integer :: i, j, temp

do i=1,n
  do j=n,i+1,-1
    if (a(j) < a(j-1)) then
      temp = a(j)
      a(j) = a(j-1)
      a(j-1) = temp
    end if
  end do
end do
end subroutine

program main
write(*,'(a)')'Bubble sort'
call run()
end program 

subroutine runit(a,na,b,nb,t1,t2)
implicit none
integer :: na,nb
integer :: a(na),b(nb)
real t1,t2
call bubblesort(a,na)
call cpu_time(t1)
call bubblesort(b,nb)
call cpu_time(t2)
end subroutine

include 'help.f90'
