program aoc2023
  use iso_fortran_env, only : real64
  use day2301_mod, only: day2301
  implicit none
  real(real64) :: time(0:25)
  integer :: i

  call cpu_time(time(0))

  01 call day2301('inp/01/input.txt')
  call cpu_time(time(1))
  time(1) = time(1) - time(0)

  02 continue
  03 continue
  04 continue

  do i=1,1
    print '("Time ",i2,1x,f8.3," ms")', i, time(i)*1000
  end do
end program aoc2023
