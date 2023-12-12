module day2312_mod
  use parse_mod, only : read_strings, string_t
  implicit none

  type :: state_t
    character(len=1), allocatable :: word(:)
    integer, allocatable :: groups(:)
  end type state_t
  interface state_t
    module procedure state_new
  end interface state_t

  integer, parameter :: IS_INVALID=1, IS_INCOMPLETE=2, IS_VALID=0

contains

  subroutine day2312(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(state_t), allocatable :: rows(:)
    integer :: i, j

    lines = read_strings(file)
    allocate(rows(size(lines)))
    do i=1, size(rows)
      rows(i) = state_t(lines(i)%str)
  print '(*(a1))', rows(i)%word
  print '(*(i0,1x))', rows(i)%groups
!     j = state_eval(rows(i))
      call complete(rows(i), j)
      print *, 'count =',j

      print *
    end do

    print *, size(lines)
  end subroutine day2312


  recursive subroutine complete(state, count)
    type(state_t), intent(in) :: state
    integer, intent(out) :: count

    integer :: i
    type(state_t) :: state1, state2
    integer :: count1, count2

    select case(state_eval(state))
    case(IS_INCOMPLETE)
      do i=1, size(state%word)
        if (state%word(i)=='?') exit
      end do
      if (i==size(state%word)+1) error stop 'no more ?'

      state1 = state
      state2 = state
      state1%word(i)='#'
      state2%word(i)='.'
      call complete(state1, count1)
      call complete(state2, count2)
      count = count1 + count2
    case(IS_VALID)
      count = 1
    case(IS_INVALID)
      count = 0
    case default
      error stop 'invalid state'
    end select
  end subroutine complete



  integer function state_eval(this) result(res)
    type(state_t), intent(in) :: this

    integer :: i, j, cnt, state
    integer, parameter :: COUNTING=1, SKIPING=0

    i = 0
    state = SKIPING
    res = IS_INCOMPLETE
    do j=1,size(this%word)
      if (this%word(j)=='?') exit
      if (this%word(j)=='#') then
        if (state==COUNTING) then
          cnt = cnt + 1
        else if (state==SKIPING) then
          cnt = 1
          i = i + 1
          state = COUNTING
        end if
      else if (this%word(j)=='.') then
        if (state==COUNTING) then
          state = SKIPING
          if (cnt==this%groups(i)) then
            ! agrees with pattern, continue scanning
          else
            ! different than required number
            res = IS_INVALID
            exit
          end if
        else if (state==SKIPING) then
          continue
        end if
      end if
    end do

    if (j==size(this%word)+1) then
      ! reached the end
      if (state==COUNTING) then
        if (cnt==this%groups(i)) then
          ! agrees with pattern
          res = IS_VALID
        else
          ! different than required number
          res = IS_INVALID
        end if
      end if
    else
      ! exit due to '?' or invalidation
      if (state==COUNTING) then
        if (cnt==this%groups(i)) then
          ! agrees with pattern
          res = IS_VALID
        elseif (cnt>this%groups(i)) then
          ! larger than required number
          res = IS_INVALID
        end if
      end if
    end if
 print *, 'eval', i==size(this%groups), res, size(this%word)-j

  end function state_eval


  type(state_t) function state_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i, j

    i = verify(str,'.#?')
    allocate(new%word(i-1))
    do j=1,i-1
      new%word(j) = str(j:j)
    end do
    new%groups = fill_array(str(i+1:))
  end function state_new


  function fill_array(str) result(arr)
    character(len=*), intent(in) :: str
    integer, allocatable :: arr(:)

    integer, parameter :: MAX_N = 500
    integer :: tmp(MAX_N)
    integer ::  i, ios

    do i=1, MAX_N
      read(str,*,iostat=ios) tmp(1:i)
      if (ios /=0) exit
    end do
    if (i==MAX_N+1) error stop 'increase MAX_N'
    arr = tmp(1:i-1)
  end function fill_array

end module day2312_mod
