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
  use day2309_mod, only: day2309
  use day2310_mod, only: day2310
  use day2312_mod, only: day2312
  use day2313_mod, only: day2313
  implicit none

  real(real64) :: time(0:25)
  integer :: i

  goto 12
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

  09 call day2309('inp/09/input.txt')
  call cpu_time(time(9))
  time(9) = time(9) - time(8)

  10 call day2310('inp/10/input.txt')
  call cpu_time(time(10))
  time(10) = time(10) - time(9)

  !11
  call cpu_time(time(11))

  12 call day2312('inp/12/input.txt')
  call cpu_time(time(12))
  time(12) = time(12) - time(11)

  13 call day2313('inp/13/input.txt')
  stop 0

  do i=1,10
    print '("Time ",i2,1x,f8.3," ms")', i, time(i)*1000
  end do
end program aoc2023
