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
    integer(int64) :: size=1_int64
  contains
    procedure :: print=>interval_print
  end type

contains

  subroutine day2305(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(map_t), allocatable :: maps(:)
    integer(int64), allocatable :: nums(:)
    type(interval_t), allocatable :: seeds1(:), seeds2(:)
    integer(int64) :: ans1, ans2
    integer :: ibeg, iend, inext, i

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
        nums = fill_array(lines(1)%str(i+1:))
        if (mod(size(nums),2)/=0) error stop 'un-even number of seeds'
        allocate(seeds1(size(nums)), seeds2(size(nums)/2))
      else
        ! maps
        maps = [maps, map_t(lines(ibeg:iend))]
      end if
      if (inext > size(lines)) exit
    end do

    ! Prepare seeds intervals
    do i=1, size(nums)
      seeds1(i)%beg = nums(i)
      if (mod(i,2)==0) then
        seeds2(i/2)%beg = nums(i-1)
        seeds2(i/2)%size = nums(i)
      end if
    end do

    ! Solve Parts 1 and 2
    ans1 = minimum_location(seeds1, maps)
    print '("Answer 05/1 ",i0,l2)', ans1, ans1==57075758_int64
    ans2 = minimum_location(seeds2, maps)
    print '("Answer 05/2 ",i0,l2)', ans2, ans2==31161857_int64
  end subroutine day2305


  function minimum_location(seeds, maps) result(ans)
    type(interval_t), intent(in) :: seeds(:)
    type(map_t), intent(in) :: maps(:)
    integer(int64) :: ans

    integer :: i, j
    type(interval_t), allocatable :: ints(:), newints(:)

    ints = seeds
    do i=1, size(maps)
      allocate(newints(0))
      do j=1, size(ints)
        newints = [newints, map_range(ints(j), maps(i))]
      end do
      call move_alloc(newints, ints)
    end do
    if (size(ints)<1) error stop 'solution not found'
    call interval_sort(ints)
    ans = ints(1)%beg
  end function minimum_location


  function map_range(srci, map) result(dsti)
    type(interval_t), intent(in) :: srci
    type(map_t), intent(in) :: map
    type(interval_t), allocatable :: dsti(:)
!
! "map%ranges(:)" must be sorted and without overlaps!
!
    type(interval_t) :: wrki, newi
    integer(int64) :: bp, ep
    integer :: i

    allocate(dsti(0))
    wrki = srci
    i = 1
    do
      if (i > size(map%ranges)) exit
      if (wrki%size==0) exit

      bp = map%ranges(i)%src
      ep = map%ranges(i)%src + map%ranges(i)%size-1

      ! do nothing
      ! b----e
      !       |-wrki-|
      if (ep < wrki%beg) then
        i = i + 1

      ! chop off new interval from left and move it
      !
      !  b------e            (case A)
      !  b----------------e  (case B)
      !   |----wrki-----|
      !
      !   |-newi||-wrki-|           (after A)
      !   |-----newi----| + wrki==0 (after B)
      else if (bp <= wrki%beg) then
        newi%beg = wrki%beg
        newi%size = min(ep - wrki%beg + 1, wrki%size)
        wrki%beg = ep + 1
        wrki%size = wrki%size - newi%size

        ! shift the "newi" and add it to desitnation intervals
        newi%beg = newi%beg + (map%ranges(i)%dst-map%ranges(i)%src)
        dsti = [dsti, newi]
        i = i + 1

      ! chop off new interval from left and leave it
      !
      !         b----e          (case C)
      !                  b----e (case D)
      !   |----wrki-----|
      !
      !   |newi||-wrki--|          (after C)
      !   |----newi-----| + wrk==0 (after D)
      else
        newi%beg = wrki%beg
        newi%size = min(bp - wrki%beg, wrki%size)
        wrki%beg = bp
        wrki%size = wrki%size - newi%size

        dsti = [dsti, newi]
        i = i + 0
      end if
    end do

    if (wrki%size > 0) error stop 'interval not completely processed'
    if (size(dsti) < 1) error stop 'returning null'
  end function map_range


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
    integer(int64) :: last

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

    ! Sort and check that ranges do not overlap
    call map_sort(new%ranges)
    last = -1_int64
    do i=1, size(new%ranges)
      if (new%ranges(i)%src <= last) error stop 'overlapping interval'
      last = new%ranges(i)%src+new%ranges(i)%size-1
    end do
  end function map_new


  subroutine map_sort(arr)
    type(range_t), intent(inout) :: arr(:)

    integer :: i, j
    type(range_t) :: tmp

    do i = 2, size(arr)
      tmp = arr(i)
      do j = i-1, 1, -1
        if (arr(j)%src > tmp%src) then
          arr(j+1) = arr(j)
        else
          exit
        end if
      end do
      arr(j+1) = tmp
    end do
  end subroutine map_sort


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


  ! =====================
  ! interval_t procedures
  ! =====================

  function interval_print(this) result(str)
    class(interval_t), intent(in) :: this
    character(len=:), allocatable :: str

    character(len=200) :: tmp

    tmp = ''
    write(tmp,'(i0,"-",i0)') this%beg, this%beg+this%size-1
    str = trim(tmp)
  end function interval_print


  subroutine interval_sort(arr)
    type(interval_t), intent(inout) :: arr(:)

    integer :: i, j
    type(interval_t) :: tmp

    do i = 2, size(arr)
      tmp = arr(i)
      do j = i-1, 1, -1
        if (arr(j)%beg > tmp%beg) then
          arr(j+1) = arr(j)
        else
          exit
        end if
      end do
      arr(j+1) = tmp
    end do
  end subroutine interval_sort

end module day2305_mod
