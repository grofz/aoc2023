program aoc2023
  use iso_fortran_env, only : real64
  use day2301_mod, only: day2301
  use day2302_mod, only: day2302
  use day2303_mod, only: day2303
  use day2304_mod, only: day2304
  use day2305_mod, only: day2305
  use day2306_mod, only: day2306
  use day2307_mod, only: day2307
  use day2308_mod, only: day2308
  implicit none

  real(real64) :: time(0:25)
  integer :: i

  call cpu_time(time(0))

  01 call day2301('inp/01/input.txt')
  call cpu_time(time(1))
  time(1) = time(1) - time(0)

  02 call day2302('inp/02/input.txt')
  call cpu_time(time(2))
  time(2) = time(2) - time(1)

  03 call day2303('inp/03/input.txt')
  call cpu_time(time(3))
  time(3) = time(3) - time(2)

  04 call day2304('inp/04/input.txt')
  call cpu_time(time(4))
  time(4) = time(4) - time(3)

  05 call day2305('inp/05/input.txt')
  call cpu_time(time(5))
  time(5) = time(5) - time(4)

  06 call day2306('inp/06/input.txt')
  call cpu_time(time(6))
  time(6) = time(6) - time(5)

  07 call day2307('inp/07/input.txt')
  call cpu_time(time(7))
  time(7) = time(7) - time(6)

  08 call day2308('inp/08/input.txt')
  call cpu_time(time(8))
  time(8) = time(8) - time(7)

  do i=1,8
    print '("Time ",i2,1x,f8.3," ms")', i, time(i)*1000
  end do
end program aoc2023
