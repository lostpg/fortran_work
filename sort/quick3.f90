! Recursive quick 3 sort


! TODO: reduce process in split under condition a(k)==x
!       too many 'if's and swaps slow the algorithm.



subroutine split(a,n,lo,hi,place,num)
implicit none
! place: place of x in rearranged array
! num  : number of x
integer :: n, i, j, k, hi, lo, x, swp, place, num
integer :: a(n)
i = lo
j = lo
x = a(lo)
do k = lo+1,hi
  if ( a(k)<x ) then
    j = j + 1
    if ( j/=k ) then
      swp = a(j)
      a(j) = a(k)
      a(k) = swp
    end if
  else if ( a(k)==x ) then 
    j = j + 1
    i = i + 1
    if ( j/=k ) then
      a(k) = a(j)
      a(j) = a(i)
    end if
  end if
end do
! swap x to the right place
a(lo:i) = a(j-(i-lo):j)
a(j-(i-lo):j) = x
place = j - (i - lo)
num = i + 1 - lo
end

recursive subroutine quick3(a,n,lo,hi)
implicit none
integer :: n, lo, hi, place, num
integer :: a(n)
if ( lo < hi ) then
  call split(a,n,lo,hi,place,num)
  call quick3(a,n,lo,place)
  call quick3(a,n,place+num,hi)
end if
end subroutine

subroutine runit(a,na,b,nb,t1,t2)
implicit none
integer :: na,nb
integer :: a(na),b(nb)
real :: t1, t2
call quick3(a,na,1,na)
call cpu_time(t1)
call quick3(b,nb,1,nb)
call cpu_time(t2)
end subroutine

program main
write(*,'(a)')'Recursive quick 3 sort'
call run()
end

include 'help.f90'
