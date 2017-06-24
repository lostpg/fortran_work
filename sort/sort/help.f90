subroutine init_random_seed()
integer :: i, n, clock
integer, allocatable :: seed(:)
call random_seed(size = n)
allocate(seed(n))
call system_clock(count=clock)
seed = clock + 37 * (/(i-1, i=1, n)/)
call random_seed(put=seed)
deallocate(seed)
end subroutine

subroutine run()
implicit none
integer :: na,nb, i
real :: get_num, time_start, time_stop
integer, allocatable :: a(:), b(:)
call init_random_seed()

na=20
nb=100000
allocate(a(na))
do i=1,na
  call random_number(get_num)
  a(i) = int(get_num * 20)
end do
write(*,100) a

100 format(100(i3))


allocate(b(nb))
write(*,'(a,i10,a)') 'Initiate an array with ',nb,' elements...'
do i=1,nb
  call random_number(get_num)
  b(i) = int(get_num * nb)
end do
write(*,'(a)')'Start sorting..'
call runit(a,na,b,nb,time_start,time_stop)
write(*,100) a
write(*,'(a,f10.6)')'Time spend: ', time_stop - time_start
deallocate(a)
deallocate(b)
end
