module day2304_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2304

  type card_t
    integer, allocatable :: win(:), act(:)
    integer :: n=1
  end type

contains

  subroutine day2304(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(card_t), allocatable :: cards(:)
    integer :: i, j, ans1, ans2

    lines = read_strings(file)
    allocate(cards(size(lines)))
    do i=1, size(cards)
      cards(i) = card_new(lines(i)%str)
    end do

    ans1 = 0
    do i=1, size(cards)
      associate(n=>count_matches(cards(i)))
        if (n>0) then
          ans1 = ans1 + 2**(n-1)
          do j = i+1, i+n
            cards(j)%n = cards(j)%n + cards(i)%n
          end do
        end if
      end associate
    end do
    ans2 = sum(cards(:)%n)

    print '("Answer 04/1 ",i0,l2)', ans1, ans1==21138
    print '("Answer 04/2 ",i0,l2)', ans2, ans2==7185540
  end subroutine day2304


  function count_matches(this) result(n)
    type(card_t), intent(in) :: this
    integer :: n

    integer :: i, j

    n = 0
    do i=1, size(this%win)
      j = findloc(this%act, this%win(i), dim=1)
      if (j/=0) n = n+1
    end do
  end function count_matches


  function card_new(line) result(new)
    character(len=*), intent(in) :: line
    type(card_t) :: new

    integer :: col, sep

    col = scan(line,':')
    sep = scan(line,'|')
    new%win = fill_array(line(col+1:sep-1))
    new%act = fill_array(line(sep+1:))
  end function card_new


  function fill_array(str) result(arr)
    character(len=*), intent(in) :: str
    integer, allocatable :: arr(:)

    integer, parameter :: MAX_N = 100
    integer :: tmp(MAX_N), i, ios

    do i=1, MAX_N
      read(str,*,iostat=ios) tmp(1:i)
      if (ios /=0) exit
    end do
    if (i==MAX_N+1) error stop 'increase MAX_N'
    arr = tmp(1:i-1)
  end function fill_array

end module day2304_mod