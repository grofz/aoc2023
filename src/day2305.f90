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
  contains
    procedure :: print=>interval_print
  end type

contains

  subroutine day2305(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(map_t), allocatable :: maps(:)
    integer(int64), allocatable :: seeds(:)
    integer :: ibeg, iend, inext, i, j
    integer(int64) :: ans1, ans2
    type(interval_t), allocatable :: seeds2(:), seeds2new(:), dsti(:)

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
!   print *, 'seeds: ',seeds

    ! Prepare for Part 2
    allocate(seeds2(size(seeds)/2))
    do i=1, size(seeds)/2
      seeds2(i)%beg = seeds(2*i-1)
      seeds2(i)%size = seeds(2*i)
    end do
    call interval_sort(seeds2)

    ! Part 1
    do i=1, size(maps)
      do j=1, size(seeds)
        seeds(j) = map_src2dst(seeds(j), maps(i))
      end do
    end do
    ans1 = minval(seeds)
    print '("Answer 05/1 ",i0,l2)', ans1, ans1==57075758_int64

    ! Part 2
    do i=1, size(maps)
      print *, 'MAP = ', maps(i)%srcname%str//'->'//maps(i)%dstname%str, size(seeds2)
!     do j=1, size(seeds2)
!       print *, '  SRC = ', seeds2(j)%print()
!     end do

      allocate(seeds2new(0))
      do j=1, size(seeds2)
        call map_range(seeds2(j), maps(i), dsti)
        seeds2new = [seeds2new, dsti]
      end do
      call move_alloc(seeds2new, seeds2)
      call interval_sort(seeds2)

!     do j=1, size(seeds2)
!       print *, '  DST = ', seeds2(j)%print()
!     end do
    end do

!   do i=1, size(seeds2)
!     print *, seeds2(i)%print()
!   end do

    if (size(seeds2)<1) error stop 'solution not found'
    ans2 = seeds2(1)%beg
    print '("Answer 04/2 ",i0,l2)', ans2, ans2==31161857_int64


   !do i=1, size(maps)
   !  print *, maps(i)%srcname%str,'-',maps(i)%dstname%str, size(maps(i)%ranges)
   !end do
  end subroutine day2305


  subroutine map_range(srci, map, dsti)
    type(interval_t), intent(in) :: srci
    type(map_t), intent(in) :: map
    type(interval_t), intent(out), allocatable :: dsti(:)
!
! "map%ranges(:)" must be sorted!
!
    type(interval_t) :: wrki, movi
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
      !   0123456789012
      ! b----------e
      !   |---wrki----|
      !
      !   |--eE--------| (after)
      else if (bp <= wrki%beg) then
        movi%beg = wrki%beg
        movi%size= min(ep - wrki%beg + 1, wrki%size)
        wrki%beg = ep + 1
        wrki%size = wrki%size - movi%size

        ! shift the "movi" and add it to desitnation intervals
        movi%beg = movi%beg + (map%ranges(i)%dst-map%ranges(i)%src)
        dsti = [dsti, movi]

        i = i + 1

      ! chop off new interval from left and leave it
      !   01234567890
      !      b----e
      !   |----wrki-----|
      !
      !   |-|b-wrki----| (after)
      else
        movi%beg = wrki%beg
        movi%size = min(bp - wrki%beg, wrki%size)
        wrki%beg = bp
        wrki%size = wrki%size - movi%size

        dsti = [dsti, movi]

        i = i + 0
      end if
    end do

    if (wrki%size > 0) dsti = [dsti, wrki]
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
