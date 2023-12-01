module day2301_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2301

contains

  subroutine day2301(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), help(:)
    integer :: i, val, ans1, ans2, val_help

    lines = read_strings(file)
help = read_strings('inp/01/solution2.txt')
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      val = getvalue2(lines(i)%str,.false.)
      ans1 = ans1 + val
      val = getvalue2(lines(i)%str,.true.)
read(help(i)%str,*) val_help
if (val_help/=val) print *, lines(i)%str, val, val_help
      ans2 = ans2 + val
    end do
    print '("Answer 01 Part 1 ",i0, l2)', ans1, ans1==55447
    print '("Answer 01 Part 2 ",i0, l2, l2)', ans2, ans2==54718, ans2==54706 ! too hight
  end subroutine day2301


  function getvalue(str) result(value)
    character(len=*), intent(in) :: str
    integer :: value
    integer :: i, d(2)

    do i=1, len(str)
      associate(j=>todigit(str(i:i)))
        if (j == -1) cycle
        d(1) = j
        exit
      end associate
    end do

    do i=len(str), 1, -1
      associate(j2=>todigit(str(i:i)))
        if (j2 == -1) cycle
        d(2) = j2
        exit
      end associate
    end do
    print *, str, d
    value = d(1)*10+d(2)
  end function


  function getvalue2(str, part_two) result(val)
    character(len=*), intent(in) :: str
    integer :: val
    logical, intent(in) :: part_two

    character(len=:), allocatable :: old, new
    integer, allocatable :: digits(:)
    integer :: z

    allocate(digits(0))
    allocate(character(len=len(str)):: old)
    old = str
    do
      call strip(old, new, z, part_two)
      if (z==-1) exit
      digits = [digits, z]
      deallocate(old)
      allocate(character(len=len(new)) :: old)
      old = new
    end do
    if (size(digits)==0) then
      print *, '#'//str//'#'
      if (part_two) error stop 'zero digits in string'
      val = 0
    else
      val = 10*digits(1) + digits(size(digits))
      if (val<11 .or. val>99) error stop 'value not'
    end if

!   if (part_two) &
!     print '(i3,1x,a,*(i1,1x))', val, '$'//str//'$', digits
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


  subroutine strip(old, new, z, part_two)
    character(len=*), intent(in) :: old
    character(len=:), allocatable, intent(out) :: new
    integer, intent(out) :: z
    logical, intent(in) :: part_two

    integer :: i, zpos, tmp, zlen
    character(len=10), parameter, dimension(0:9) :: words = &
      [ 'zero      ', &
        'one       ', &
        'two       ', &
        'three     ', &
        'four      ', &
        'five      ', &
        'six       ', &
        'seven     ', &
        'eight     ', &
        'nine      ' ]

    z = -1
    zpos = huge(zpos)

    ! Search for the first arabic digit
    do i=1, len(old)
      associate(dig=>todigit(old(i:i)))
        if (dig == -1) cycle
        zpos = i
        z = dig
        exit
      end associate
    end do
    if (i/=len(old)+1) zlen = 1

    ! Search for the first string "one", "two", ...
    if (part_two) then
      do i = 0, 9
        tmp = index(old, trim(words(i)))
        if (tmp==0) cycle
        if (tmp > zpos) cycle
        if (tmp==zpos) error stop 'not possible'
        zpos = tmp
        z = i
        zlen = len_trim(words(i))
      end do
    end if

    if (zpos==huge(zpos)) then
      allocate(character(len=0) :: new)
    else
      allocate(character(len=(len(old)-(zpos+zlen)+1)) :: new)
      new = old(zpos+zlen:len(old))
    end if
  end subroutine

end module day2301_mod
