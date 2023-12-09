module day2309_mod
  use parse_mod, only : read_strings, string_t
  implicit none
  private
  public day2309

contains

  subroutine day2309(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, allocatable :: arr(:)
    integer :: i, ans1, ans2, next, prev

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      arr = fill_array(lines(i)%str)
      next = backfill(arr,.false.)
      prev = backfill(arr,.true.)
      ans1 = ans1 + next
      ans2 = ans2 + prev
    end do
    print '("Answer 09/1 ",i0,l2)', ans1, ans1==1725987467
    print '("Answer 09/1 ",i0,l2)', ans2, ans2==971

  end subroutine day2309


  function diff(u) result (du)
    integer, intent(in) :: u(:)
    integer :: du(size(u)-1)

    integer :: i

    do i=1, size(u)-1
      du(i) = u(i+1)-u(i)
    end do
  end function diff


  recursive function backfill(u, is_prev) result(next)
    integer, intent(in) :: u(:)
    logical, intent(in) :: is_prev
    integer :: next

    integer, allocatable :: du(:)

    du = diff(u)
    if (count(du==0)==size(du)) then
      next = u(1)
    else
      if (is_prev) then
        next = u(1) - backfill(du, is_prev)
      else
        next = u(size(u)) + backfill(du, is_prev)
      end if
    end if
  end function backfill


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

end module day2309_mod