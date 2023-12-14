module day2314_mod
  use parse_mod, only : read_pattern
  implicit none

  integer, parameter :: DIRS(2,4) = reshape([-1,0,0,-1,1,0,0,1],[2,4])
  integer, parameter :: DIR_NORTH=1, DIR_EAST=4, DIR_SOUTH=3, DIR_WEST=2
  character(len=1), parameter :: CH_ROCK='O', CH_SPACE='.', CH_PIVOT='#'

  type map_t
    character(len=1), allocatable :: map(:,:)
    integer :: ncycles = 0
  end type map_t

contains

  subroutine day2314(file)
    character(len=*), intent(in) :: file

    type(map_t) :: map
    integer :: ans1, ans2, i, load, k, period
    integer, parameter :: RES_CYCLES=200, CYCLES=1000000000
    integer :: serie(2,RES_CYCLES)

    ! Part 1
    map%map = read_pattern(file)
    call turn_map(map%map, DIR_NORTH)
    !call print_pattern(map%map)
    ans1 = calc_load(map%map)
    print '("Answer 14/1 ",i0,l2)', ans1, ans1==109939

    ! Part 2
    map%map = read_pattern(file)
    map%ncycles = 0
    serie = 0
    ! Make some cycles to stabilize and determine the period
    do i=1,RES_CYCLES   
      call cycle_map(map)
      load = calc_load(map%map)
      serie(1,i) = load ! 1 = the actual load
      do k=i-1,1,-1
        if (serie(1,k)==serie(1,i)) exit
      end do
      serie(2,i) = i-k ! 2 = distance to the same load value in the past 
    end do
    period = maxval(serie(2,RES_CYCLES/2:))

    ! Shift time forward and make the remaining steps
    map%ncycles = map%ncycles + period*((CYCLES-map%ncycles)/period-1)
    do
      call cycle_map(map)
      if (map%ncycles==CYCLES) exit
    end do
    ans2 = calc_load(map%map)
    print '("Answer 14/2 ", i0, l2)', ans2, ans2==101010
  end subroutine day2314


  function calc_load(map) result(cnt)
    character(len=1), intent(in) :: map(:,:)
    integer :: cnt

    integer :: i

    cnt = 0
    do i=1,size(map,1)
      cnt = cnt + (size(map,1)-i+1)*count(map(i,:)==CH_ROCK)
    end do
  end function calc_load


  subroutine cycle_map(map)
    type(map_t), intent(inout) :: map

    integer :: j

    do j=1,4
      call turn_map(map%map,j)
    end do
    map%ncycles = map%ncycles+1
  end subroutine cycle_map


  subroutine turn_map(map, dir)
    character(len=1), intent(inout) :: map(:,:)
    integer, intent(in) :: dir

    character(len=1) :: mapext(0:size(map,1)+1, 0:size(map,2)+1)
    integer :: i, j, nmoves, ijn(2)

    mapext = CH_PIVOT
    mapext(1:size(map,1), 1:size(map,2)) = map

    do
      nmoves = 0

      do i=1, size(map,1)
        do j=1, size(map,2)
          ijn = [i, j] + DIRS(:, dir)
          if (mapext(i,j)/=CH_ROCK) cycle
          if (mapext(ijn(1),ijn(2))==CH_SPACE) then
            mapext(i,j) = CH_SPACE
            mapext(ijn(1),ijn(2)) = CH_ROCK
            nmoves = nmoves + 1
          end if
        end do
      end do
      if (nmoves==0) exit
    end do
    map = mapext(1:size(map,1), 1:size(map,2))

  end subroutine turn_map


  subroutine print_pattern(map)
    character(len=1), intent(in) :: map(:,:)

    integer :: i

    do i=1, size(map,1)
      print '(*(a2))', map(i,:)
    end do
  end subroutine print_pattern

end module day2314_mod