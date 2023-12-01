module day2301_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2301

contains

  subroutine day2301(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i, val, ans1, ans2
    character(len=:), allocatable :: old, new
    integer, allocatable :: digits(:)

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      val = getvalue(lines(i)%str)
      ans1 = ans1 + val
    end do
    print '("Answer 01 Part 1 ",i0, l2)', ans1, ans1==55447

    do i=1, size(lines)
      if (allocated(digits)) deallocate(digits)
      if (allocated(old)) deallocate(old)
      allocate(digits(0))
      allocate(character(len=len(lines(i)%str)) :: old)
      old = lines(i)%str
      do
        call strip(old, new, val)
        !if (len(new)==0) exit
        if (val==-1) exit
        digits = [digits, val]
        deallocate(old)
        allocate(character(len=len(new)) :: old)
        old = new
      end do
      if (size(digits)==0) then
        print *, 'error? ', lines(i)%str
        cycle
      end if
      val = digits(1)*10 + digits(size(digits))
      print *, lines(i)%str, digits, val
      ans2 = ans2 + val
    end do
    print '("Answer 01 Part 2 ",i0, l2)', ans2, ans2==54718 ! too hight

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

  function todigit(ch) result(d)
    character(len=1), intent(in) :: ch
    integer :: d
    if (iachar(ch(1:1))<iachar('0') .or. iachar(ch(1:1))>iachar('9')) then
      d = -1
    else
      d = ichar(ch(1:1))-iachar('0')
    end if
  end function


  subroutine strip(old, new, digit)
    character(len=*), intent(in) :: old
    character(len=:), allocatable, intent(out) :: new
    integer, intent(out) :: digit

    integer :: i, di, j, z, zpos, tmp, zlen
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

    zpos = huge(zpos)
    z = -1
    zlen = 0
    digit = -1

    do i=1, len(old)
      associate(j=>todigit(old(i:i)))
        if (j == -1) cycle
        di = j
        exit
      end associate
    end do
    if (i/=len(old)+1) then
      zpos = i
      zlen = 1
      z = di
    end if

    do j=0, 9
      tmp = index(old, trim(words(j)))
      if (tmp==0) cycle
      if (tmp < zpos) then
        zpos = tmp
        zlen = len_trim(words(j))
        z = j
      end if
    end do

    if (zpos==huge(zpos)) then
      allocate(character(len=0) :: new)
      digit = -1
    else
      allocate(character(len=len(old)-(zpos+zlen)+1) :: new)
      new = old(zpos+zlen:len(old))
      digit = z
    end if

!   print *, old, digit, '*'//new//'*'
  end subroutine

end module day2301_mod
