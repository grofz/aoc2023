module day2312_mod
  use iso_fortran_env, only : int64
  use parse_mod, only : read_strings, string_t
  implicit none

  type :: state_t
    character(len=1), allocatable :: word(:)
    integer, allocatable :: groups(:)
  end type state_t
  interface state_t
    module procedure state_new
    module procedure state_unfold
  end interface state_t

  integer, parameter :: IS_INVALID=1, IS_INCOMPLETE=2, IS_VALID=0

contains

  subroutine day2312(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(state_t), allocatable :: rows(:), rows2(:)
    integer :: i, j
    integer(int64) :: ans1, ans2

    lines = read_strings(file)
    allocate(rows(size(lines)))
    allocate(rows2(size(lines)))
    ans1 = 0
    ans2 = 0
    do i=1, size(rows)
      rows(i) = state_t(lines(i)%str)
      call complete(rows(i), j)
      ans1 = ans1 + j
    end do
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==7857

    do i=1, size(rows)
      rows2(i) = state_t(rows(i))
  print '(*(a1))', rows(i)%word
  print '(*(i0,1x))', rows(i)%groups
  print *, 'unfold to'
  print '(*(a1))', rows2(i)%word
  print '(*(i0,1x))', rows2(i)%groups

      call complete(rows2(i), j)
      ans2 = ans2 + j
      print *, 'count =',j
      print *
    end do

    print '("Answer 12/1 ",i0,l2)', ans2, ans2==1
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
!     print *, 'valid =',state%word
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
    integer :: nfree, nleft

    nleft = sum(this%groups)-count(this%word=='#')
    nfree = count(this%word=='?')
    if (nleft > nfree) then
      res = IS_INVALID
      return
    end if


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
          if (i<=size(this%groups)) then
            if (cnt/=this%groups(i)) then
              res = IS_INVALID
              return
            end if
          else
            res = IS_INVALID
            return
          end if
        else if (state==SKIPING) then
          continue
        end if
      end if
    end do

    if (j==size(this%word)+1) then
      ! reached the end
      if (state==COUNTING) then
        if (i==size(this%groups)) then
          if (cnt==this%groups(i)) then
            ! agrees with pattern
            res = IS_VALID
          else
            ! different than required number
            res = IS_INVALID
          end if
        else
          res = IS_INVALID
        end if
      else if (state==SKIPING) then
        if (i==size(this%groups)) then
          res = IS_VALID
        else
          res = IS_INVALID
        end if
      end if
    else
      ! exit due to '?' or invalidation
      if (state==COUNTING) then
        if (i<=size(this%groups)) then
          if (cnt>this%groups(i)) then
            ! agrees with pattern
            res = IS_INVALID
          end if
        end if
      end if
    end if
!print *, 'eval', i==size(this%groups), res, size(this%word)-j

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


  type(state_t) function state_unfold(this) result(new)
    type(state_t), intent(in) :: this

    integer, parameter :: NCOPIES=5
    integer :: i, j

    allocate(new%word(5*size(this%word)+4))
    allocate(new%groups(5*size(this%groups)))
    do i=1, NCOPIES
      associate(n=>size(this%word))
        new%word((i-1)*(n+1)+1:(i-1)*(n+1)+n)=this%word
        if (i<NCOPIES) new%word(i*(n+1)) = '?'
      end associate
      associate(k=>size(this%groups))
        new%groups((i-1)*k+1:i*k) = this%groups
      end associate
    end do
  end function state_unfold

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
