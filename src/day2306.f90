module day2306_mod
  use iso_fortran_env, only : int64, real64
  implicit none

contains

  subroutine day2306(file)
    character(len=*), intent(in) :: file

    integer, allocatable :: tim(:), dis(:)
    integer :: i
    integer(int64) :: ans1, ans2

    call read_inputs(file, tim, dis)
    ans1 = 1
    do i=1, size(tim)
      associate(tmp => count_wins(tim(i), dis(i)))
       !print *, i, tmp, tmp==count_wins2(int(tim(i),int64), int(dis(i),int64))
        ans1 = ans1 * tmp
      end associate
    end do
    ans2 = count_wins2(compound_number(tim), compound_number(dis))
    print '("Answer 06/1 ",i0,l2)', ans1, ans1==741000
    print '("Answer 06/2 ",i0,l2)', ans2, ans2==38220708_int64
  end subroutine day2306


  function flown_distance(c, r) result(d)
    integer, intent(in) :: c ! charging time
    integer, intent(in) :: r ! race time
    integer :: d
    d = c * (r-c)
  end function flown_distance


  function count_wins2(t, d) result(cnt)
    integer(int64), intent(in)  :: t, d
    integer(int64) :: cnt

    integer(int64) :: disc, c1, c2

    disc = t**2 - 4*d
    c1 = (t+sqrt(real(disc,real64)))/2
    c2 = (t-sqrt(real(disc,real64)))/2
    cnt = c1-c2
  end function count_wins2


  function count_wins(t, d) result(cnt)
    integer, intent(in)  :: t, d
    integer :: cnt

    integer :: i

    cnt = 0
    do i=0, t
      if (flown_distance(i, t)>d) cnt = cnt + 1
    end do
  end function count_wins


  subroutine read_inputs(file, tim, dis)
    use parse_mod, only : string_t, read_strings
    character(len=*), intent(in) :: file
    integer, allocatable, intent(out) :: tim(:), dis(:)

    type(string_t), allocatable :: lines(:)
    integer :: icol, i

    lines = read_strings(file)
    do i=1,2
      icol = scan(lines(i)%str,':')
      if (icol==0) error stop 'error reading input'
      select case (lines(i)%str(:icol-1))
      case('Distance')
        dis = fill_array(lines(i)%str(icol+1:))
      case('Time')
        tim = fill_array(lines(i)%str(icol+1:))
      case default
        error stop 'not recognized line'
      end select
    end do
  end subroutine read_inputs


  function fill_array(str) result(arr)
    character(len=*), intent(in) :: str
    integer, allocatable :: arr(:)

    integer, parameter :: MAX_N = 100
    integer :: tmp(MAX_N)
    integer ::  i, ios

    do i=1, MAX_N
      read(str,*,iostat=ios) tmp(1:i)
      if (ios /=0) exit
    end do
    if (i==MAX_N+1) error stop 'increase MAX_N'
    arr = tmp(1:i-1)
  end function fill_array


  function compound_number(arr) result (n)
    integer, intent(in) :: arr(:)
    integer(int64) :: n

    character(len=20) :: buffer

    write(buffer,'(*(i0))') arr
    read(buffer, *) n
  end function compound_number

end module day2306_mod