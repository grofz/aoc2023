module day2301_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2301

contains

  subroutine day2301(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      ans1 = ans1 + value(lines(i)%str,.false.)
      ans2 = ans2 + value(lines(i)%str,.true.)
    end do
    print '("Answer 01 Part 1 ",i0, l2)', ans1, ans1==55447
    print '("Answer 01 Part 2 ",i0, l2)', ans2, ans2==54706
  end subroutine day2301


  function value(str, part_two)
    character(len=*), intent(in) :: str
    integer :: value
    logical, intent(in) :: part_two

    integer :: z1, z2

    z1 = strip(str, .false., part_two)
    z2 = strip(str, .true.,  part_two)

    if (z1==-1 .or. z2==-1) error stop 'invalid string'
    value = 10*z1 + z2
  end function


  function todigit(ch) result(d)
    character(len=1), intent(in) :: ch
    integer :: d
    if (iachar(ch(1:1))<iachar('0') .or. iachar(ch(1:1))>iachar('9')) then
      d = -1
    else
      d = ichar(ch(1:1))-iachar('0')
    end if
  end function


  function strip(str, back, part_two) result(z)
    character(len=*), intent(in) :: str
    logical, intent(in) :: back, part_two
    integer :: z

    integer :: i, zpos, from, to, step
    character(len=5), parameter, dimension(0:9) :: words = &
      [ 'zero ', 'one  ', 'two  ', 'three', 'four ', &
        'five ', 'six  ', 'seven', 'eight', 'nine ' ]
    z = -1
    if (back) then
      zpos = 0
    else
      zpos = huge(zpos)
    end if

    ! Search for the first/last "1", "2"
    if (.not. back) then
      from = 1
      to = len(str)
      step = 1
    else
      from = len(str)
      to = 1
      step = -1
    end if
    do i = from, to, step
      associate(dig=>todigit(str(i:i)))
        if (dig == -1) cycle
        zpos = i
        z = dig
        exit
      end associate
    end do

    ! Search for the first/last "one", "two", ...
    if (part_two) then
      do i = 0, 9
        associate(p => index(str, trim(words(i)), back=back))
          if (p==0) cycle
          if ((.not. back .and. p > zpos) .or. &
               (     back .and. p < zpos)) cycle
          if (p==zpos) error stop 'not possible'
          zpos = p
          z = i
        end associate
      end do
    end if
  end function strip

end module day2301_mod
