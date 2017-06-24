! insert sort
subroutine insertsort(a,n)
implicit none
integer :: n, i, j, key
integer :: a(n)
do i=2,n
  key = a(i)
  j = i - 1
  do while( j > 0 .and. a(j) > key)
    a(j+1) = a(j)
    j = j - 1
  end do
  a(j+1) = key
end do
end subroutine

subroutine runit(a,na,b,nb,t1,t2)
implicit none
integer :: na,nb
integer :: a(na),b(nb)
real :: t1,t2
call insertsort(a,na)
call cpu_time(t1)
call insertsort(b,nb)
call cpu_time(t2)
end subroutine

program main
write(*,'(a)')'Insert sort'
call run()
end

include 'help.f90'
