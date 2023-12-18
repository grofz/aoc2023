module day2318_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I8=>int64
  implicit none

contains

  subroutine day2318(file)
    character(len=*), intent(in) :: file

    integer(I8), allocatable :: xy(:,:)
    integer(I8) :: area, circ, ans1, ans2

    call read_path(file, 1, xy, circ)
    area = trapezoid(xy)
    ans1 = area + circ/2 + 1
    print '("Answer 18/1 ",i0,l2)', ans1, ans1==47527

    call read_path(file, 2, xy, circ)
    area = trapezoid(xy)
    ans2 = area + circ/2 + 1
    print '("Answer 18/2 ",i0,l2)', ans2, ans2==52240187443190_I8 
  end subroutine day2318


  subroutine read_path(file, which_part, xy, circ)
    character(len=*), intent(in) :: file
    integer, intent(in) :: which_part
    integer(I8), allocatable, intent(out) :: xy(:,:)
    integer(I8), intent(out) :: circ
!
! Read and parse the polygon encircling the digging site.
! Also calculate the circumfence.
!
    type(string_t), allocatable :: lines(:), words(:)
    integer :: i, d, n

    character(len=*), parameter :: CHARS='RDLU'
    integer, parameter :: DIRS(2,4)=reshape([0,1,1,0,0,-1,-1,0],[2,4])

    lines = read_strings(file)
    allocate(xy(2, size(lines)+1))

    circ = 0
    xy(:,1) = [1, 1]
    do i=2, size(xy,dim=2)
      call split(lines(i-1)%str,' ',words)
      if (size(words)/=3) error stop 'parsing error - not three groups'
      ! Example of valid line: "R 6 (#70c710)"

      select case(which_part)
      case(1)
        ! Part 1: use "R" as dig direction and "6" as the length
        d = scan(CHARS, words(1)%str(1:1))
        if (d==0) error stop 'parsing error - direction not recognized'
        read(words(2)%str,*) n
      case(2)
        ! Part 2: use "(#AAAAAB)" - AAAAA is the length, B is the direction
        !                ^   ^^
        !                3   78
        !
        n = hexstr2int(words(3)%str(3:7))
        read(words(3)%str(8:8), *) d
        d = d+1
        if (d<1 .or. d>4) error stop 'parsing error - direction out of range'
      case default
        error stop 'parsing error - which_part argument is invalid'
      end select

      xy(:,i) = xy(:,i-1) + DIRS(:,d)*n
      circ = circ + n
    end do

    if (mod(circ,2)/=0) error stop 'circ must be even'
    if (any(xy(:,1)/=xy(:,size(xy,2)))) error stop 'starting and ending points must match'
  end subroutine read_path


  integer function hexstr2int(str) result(res)
    character(len=*), intent(in) :: str
!
! Convert a hexadecimal string to integer
!
    integer, parameter :: N = 5, BASE=16
    character(len=*), parameter :: DIGITS="0123456789abcdef"
    integer :: i, j

    if (len(str)/=N) error stop 'hexstr2int - unexpected length of input'
    res = 0
    do i = N, 1, -1
      j = scan(DIGITS, str(i:i))
      if (j==0) error stop 'hexstr2int - unexpected character in string'
      res = res + BASE**(N-i)*(j-1)
    end do
  end function hexstr2int


  function trapezoid(xy) result(area)
    integer(I8), intent(in) :: xy(:,:)
    integer(I8) :: area
!
! Area of the polygon
! Cf: https://en.wikipedia.org/wiki/Shoelace_formula
!
    integer :: i

    area = 0
    do i=1,size(xy,2)-1
      area = area + (xy(2,i)+xy(2,i+1))*(xy(1,i)-xy(1,i+1))
    end do
    if (mod(area,2)/=0) error stop 'trapezoid - cannot divide area by two'
    area = abs(area)/2
  end function trapezoid

end module day2318_mod