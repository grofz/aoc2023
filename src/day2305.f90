module day2305_mod
  use parse_mod, only : string_t, read_strings
  use iso_fortran_env, only : int64
  implicit none

  type range_t
    integer(int64) :: src, dst, size
  end type range_t
  interface range_t
    module procedure range_new
  end interface

  type map_t
    type(string_t) :: srcname, dstname
    type(range_t), allocatable :: ranges(:)
  end type map_t
  interface map_t
    module procedure map_new
  end interface

  type interval_t
    integer(int64) :: beg
    integer(int64) :: size
  end type

contains

  subroutine day2305(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(map_t), allocatable :: maps(:)
    integer(int64), allocatable :: seeds(:)
    integer :: ibeg, iend, inext, i, j
    integer(int64) :: ans1
    type(interval_t), allocatable :: seeds2(:)

    ! Parsing the input
    lines = read_strings(file)
    inext = 1
    allocate(maps(0))
    do
      call mark_block(lines, ibeg, iend, inext)
      if (ibeg==1) then
        ! seeds
        i = scan(lines(1)%str,':')
        if (i==0) error stop 'error reading line of seeds'
        seeds = fill_array(lines(1)%str(i+1:))
      else
        ! maps
        maps = [maps, map_t(lines(ibeg:iend))]
      end if
      if (inext > size(lines)) exit
    end do
    print *, 'seeds: ',seeds

    ! Prepare for Part 2
    allocate(seeds2(size(seeds)/2))
    do i=1, size(seeds)/2
      seeds2(i)%beg = seeds(2*i-1)
      seeds2(i)%size = seeds(2*i)
print *, seeds2(i)%beg, seeds2(i)%beg+seeds2(i)%size-1
    end do


    ! Part 1
    do i=1, size(maps)
      do j=1, size(seeds)
        seeds(j) = map_src2dst(seeds(j), maps(i))
      end do
    end do
    ans1 = minval(seeds)
    print '("Answer 05/1 ",i0,l2)', ans1, ans1==57075758_int64

    ! Part 2


   !do i=1, size(maps)
   !  print *, maps(i)%srcname%str,'-',maps(i)%dstname%str, size(maps(i)%ranges)
   !end do
  end subroutine day2305


  subroutine map_range(srci, map, dsti)
    type(interval_t), intent(in) :: srci
    type(map_t), intent(in) :: map
    type(interval_t), allocatable :: dsti(:)

    integer(int64), allocatable :: spts(:)
    integer(int64) :: bp, ep
    integer :: i

    ! Find separation points
    !    |MAP-R1|     |MAP_R2|   |MAP_R3|
    ! |--a------b-----c------d---e-|  srci
    !
    ! |-||------||---||-----||--||-|  dsti - before shifting

    allocate(spts(0))
    do i=1, size(map%ranges)
      bp = map%ranges(i)%src
      ep = map%ranges(i)%src + map%ranges(i)%size-1

      if (bp >= srci%beg .and. bp <= srci%beg+srci%size-1) then
        spts = [spts, bp]
      end if
      if (ep >= srci%beg .and. ep <= srci%beg+srci%size-1) then
      end if

    end do
  end subroutine map_range

  ! ==============
  ! Part 1 mapping
  ! ==============

  function map_src2dst(src, map) result(dst)
    integer(int64), intent(in) :: src
    type(map_t), intent(in) :: map
    integer(int64) :: dst

    integer :: i

    dst = src
    do i=1, size(map%ranges)
      associate(j => in_range_map(src,map%ranges(i)))
        if (j/=-1) then
          dst = j
          exit
        end if
      end associate
    end do

   !print *, map%srcname%str, src, '-->', map%dstname%str, dst
   !stop
  end function map_src2dst


  function in_range_map(src, range) result(dst)
    integer(int64), intent(in) :: src
    type(range_t), intent(in) :: range
    integer(int64) :: dst

    integer(int64) :: lb, ub

    dst = -1_int64
    lb = range%src
    ub = lb + range%size - 1
    if (src>=lb .and. src<=ub) dst = range%dst + (src-lb)

   !print *, src, dst, range%src, range%dst, range%size
  end function in_range_map

  ! =============
  ! Input parsing
  ! =============

  type(range_t) function range_new(str) result(new)
    character(len=*), intent(in) :: str
    read(str, *) new%dst, new%src, new%size
  end function range_new

  type(map_t) function map_new(lines) result(new)
    type(string_t), intent(in) :: lines(:)

    integer :: dash1, dash2, colon, i

    associate(fl => lines(1)%str)
      dash1 = scan(fl,'-')
      dash2 = scan(fl,'-',back=.true.)
      colon = index(fl,' map:')
      if (dash1==0 .or. dash2==0 .or. dash2==dash1 .or. colon==0) &
          error stop 'first line is not first line'
      new%srcname = string_t(fl(1:dash1-1))
      new%dstname = string_t(fl(dash2+1:colon-1))
    end associate

    allocate(new%ranges(size(lines)-1))
    do i=2, size(lines)
      new%ranges(i-1) = range_t(lines(i)%str)
    end do
  end function map_new


  subroutine mark_block(lines, ibeg, iend, inext)
    type(string_t), intent(in) :: lines(:)
    integer, intent(out) :: ibeg, iend
    integer, intent(inout) :: inext

    integer :: i

    ! find the next blank line after "inext" or end-of-file
    do i = inext, size(lines)
      if (len_trim(lines(i)%str)==0) exit
    end do
    ibeg = inext
    iend = i-1
    inext = i+1

    !print *, 'mark_block ', ibeg, iend
  end subroutine mark_block


  function fill_array(str) result(arr)
    character(len=*), intent(in) :: str
    integer(int64), allocatable :: arr(:)

    integer, parameter :: MAX_N = 100
    integer(int64) :: tmp(MAX_N)
    integer ::  i, ios

    do i=1, MAX_N
      read(str,*,iostat=ios) tmp(1:i)
      if (ios /=0) exit
    end do
    if (i==MAX_N+1) error stop 'increase MAX_N'
    arr = tmp(1:i-1)
  end function fill_array

end module day2305_mod