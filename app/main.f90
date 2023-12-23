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
  use day2311_mod, only: day2311
  use day2312_mod, only: day2312
  use day2313_mod, only: day2313
  use day2314_mod, only: day2314
  use day2315_mod, only: day2315
  use day2316_mod, only: day2316
  use day2317_mod, only: day2317
  use day2318_mod, only: day2318
  use day2319_mod, only: day2319
  use day2320_mod, only: day2320
  use day2321_mod, only: day2321
  use day2322_mod, only: day2322
  use day2323_mod, only: day2323
  implicit none

  real(real64) :: time(0:25)
  integer :: i

  call cpu_time(time(0))
  goto 23

  01 call day2301('inp/01/input.txt')
  call cpu_time(time(1))

  02 call day2302('inp/02/input.txt')
  call cpu_time(time(2))

  03 call day2303('inp/03/input.txt')
  call cpu_time(time(3))

  04 call day2304('inp/04/input.txt')
  call cpu_time(time(4))

  05 call day2305('inp/05/input.txt')
  call cpu_time(time(5))

  06 call day2306('inp/06/input.txt')
  call cpu_time(time(6))

  07 call day2307('inp/07/input.txt')
  call cpu_time(time(7))

  08 call day2308('inp/08/input.txt')
  call cpu_time(time(8))

  09 call day2309('inp/09/input.txt')
  call cpu_time(time(9))

  10 call day2310('inp/10/input.txt')
  call cpu_time(time(10))

  11 call day2311('inp/11/input.txt')
  call cpu_time(time(11))

  12 call day2312('inp/12/input.txt')
  call cpu_time(time(12))

  13 call day2313('inp/13/input.txt')
  call cpu_time(time(13))

  14 call day2314('inp/14/input.txt')
  call cpu_time(time(14))

  15 call day2315('inp/15/input.txt')
  call cpu_time(time(15))

  16 call day2316('inp/16/input.txt')
  call cpu_time(time(16))

  17 call day2317('inp/17/input.txt')
  call cpu_time(time(17))

  18 call day2318('inp/18/input.txt')
  call cpu_time(time(18))

  19 call day2319('inp/19/input.txt')
  call cpu_time(time(19))

  20 call day2320('inp/20/input.txt')
  call cpu_time(time(20))

  21 call day2321('inp/21/input.txt')
  call cpu_time(time(21))

  22 call day2322('inp/22/input.txt')
  call cpu_time(time(22))

  23 call day2323('inp/23/input.txt')
  !23 call day2323('inp/23/test.txt')
  call cpu_time(time(23))
  stop 0

  print *
  do i=1,23
    print '("Time ",i2,1x,f8.3," ms")', i, (time(i)-time(i-1))*1000
  end do
end program aoc2023
