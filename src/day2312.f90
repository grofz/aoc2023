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

  integer, parameter :: RES_WIN=10, RES_LOSE=11, RES_DEFAULT=12
  integer(int64), parameter :: NOTAVAIL = -1_int64

contains

  subroutine day2312(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i
    integer(int64) :: ans1, ans2, j

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      call count_paths(state_t(lines(i)%str), j)
      ans1 = ans1 + j
 print *, j
    end do
    print '("Answer 12/1 ",i0,l2)', ans1, ans1==7857

    do i=1, size(lines)
      call count_paths(state_t(state_t(lines(i)%str)), j)
      ans2 = ans2 + j
    end do
    print '("Answer 12/2 ",i0,l2)', ans2, ans2==28606137449920_int64
  end subroutine day2312


  recursive subroutine count_paths(state, cnt, iw, ig)
    type(state_t), intent(in) :: state
    integer(int64), intent(out) :: cnt
    integer, intent(in), optional :: iw, ig

    integer :: iw0, ig0, res, iw1, ig1, iw2, ig2
    integer(int64) :: cnt0, cnt1

    integer(int64), allocatable, save :: memo(:,:)

    ! Top level is recognized by not providing optional arguments
    iw0 = 1
    ig0 = 1
    if (present(iw)) iw0 = iw
    if (present(ig)) ig0 = ig
    if (.not. present(iw)) then
      if (allocated(memo)) deallocate(memo)
      allocate(memo(size(state%word)+2, size(state%groups)+1), &
               source=NOTAVAIL)
    end if

    ! Make moves until the choice must be made
    do
      call next_state(state, iw0, ig0, res)
      if (res==RES_WIN) then
        cnt = 1
        return
      else if (res==RES_LOSE) then
        cnt = 0
        return
      else if (iw0 <= size(state%word)) then
        if (state%word(iw0)=='?') exit
      end if
    end do

    iw2 = iw0
    ig2 = ig0
    if (memo(iw2,ig2)==NOTAVAIL) then
      ! Branch the solution
      iw1 = iw0
      ig1 = ig0

      ! Left branch
      call next_state(state, iw0, ig0, res, next_move='.')
      if (res==RES_LOSE) then
        cnt0 = 0
      else if (res==RES_WIN) then
        cnt0 = 1
      else
        call count_paths(state, cnt0, iw0, ig0)
      end if

      ! Right branch
      call next_state(state, iw1, ig1, res, next_move='#')
      if (res==RES_LOSE) then
        cnt1 = 0
      else if (res==RES_WIN) then
        cnt1 = 1
      else
        call count_paths(state, cnt1, iw1, ig1)
      end if
      cnt = cnt0 + cnt1

      ! Store result for next use
      memo(iw2,ig2) = cnt
    else
      ! Use memorized answer
      cnt = memo(iw2,ig2)
    end if
  end subroutine count_paths


  subroutine next_state(state, iw, ig, res, next_move)
    type(state_t), intent(in) :: state
    integer, intent(inout) :: iw, ig
    integer, intent(out)   :: res
    character(len=1), intent(in), optional :: next_move

    integer :: i, iend
    character(len=1) :: next_char

    res = RES_DEFAULT

    if (iw > size(state%word) .and. ig==size(state%groups)+1) then
      res = RES_WIN
    else if (iw > size(state%word)) then
      res = RES_LOSE
    else
      next_char = state%word(iw)
      if (next_char=='?') then
        if (present(next_move)) then
          next_char = next_move
        else
          return
        end if
      else
        if (present(next_move)) error stop 'not ready to make a choice'
      end if

      select case(next_char)
      case('.')
        iw = iw + 1
      case('#')
        if (ig > size(state%groups)) then
          res = RES_LOSE
          return
        end if
        iend = iw+state%groups(ig)-1
        do i=iw, min(iend, size(state%word))
          ! Make sure the group contains '#" or '?'
          if (state%word(i)=='.') exit
        end do
        if (i==iend+1) then
          ! group is complete, check the following character is '.' or '?'
          if (i <= size(state%word)) then
              if (state%word(i)=='#') res = RES_LOSE
          end if
        else
          ! the group could not be completed
          res = RES_LOSE
        end if
        iw = iend+2
        ig = ig+1
      case default
        error stop 'next_state - invalid letter'
      end select
    end if
  end subroutine next_state


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
