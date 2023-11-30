module day2301_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2301

contains

  subroutine day2301(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: i

    lines = read_strings(file)
    do i=1, size(lines)
      print *, lines(i)%str
    end do
    print '(i0," lines read")', size(lines)
  end subroutine day2301

end module day2301_mod
