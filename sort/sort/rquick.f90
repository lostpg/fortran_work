recursive subroutine quicksort(a,n,low,high)
implicit none
integer :: low, high, n, w
integer :: a(n)

if (low < high) then
  call split(a,n,high,low,w)
  call quicksort(a,n,low,w-1)
  call quicksort(a,n,w+1,high)
end if
end subroutine

subroutine split(a,n,high,low,w)
implicit none
integer :: w, n, i, j, tmp, x, high, low
integer :: a(n)
i = low
x = a(low)
do j=low+1,high
  if (a(j) <= x) then
    i = i + 1
    if (i /= j) then
      tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    end if
  end if
end do
tmp = a(low)
a(low) = a(i)
a(i) = tmp
w = i
end subroutine

subroutine runit(a,na,b,nb,t1,t2)
implicit none
integer :: na,nb
integer :: a(na),b(nb)
real :: t1, t2
call quicksort(a,na,1,na)
call cpu_time(t1)
call quicksort(b,nb,1,nb)
call cpu_time(t2)
end subroutine

program main
write(*,'(a)')'Recursive quick sort'
call run()
end

include 'help.f90'
