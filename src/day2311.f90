module day2311_mod
  use parse_mod, only : read_pattern
  use iso_fortran_env, only : int64
  implicit none

  character(len=1), parameter :: CH_GALAXY='#', CH_EMPTY='.'

contains

  subroutine day2311(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: galaxy(:,:)

    integer(int64), allocatable :: xg(:), yg(:)
    integer(int64) :: ans1, ans2
    integer(int64), parameter :: NSHIFT=1000000_int64

    galaxy = read_pattern(file)
    call expand_galaxy(galaxy, xg, yg, 2_int64)
    ans1 = count_distsum(xg, yg)
    print '("Answer 11/1 ",i0,l2)', ans1, ans1==9724940

    call expand_galaxy(galaxy, xg, yg, NSHIFT)
    ans2 = count_distsum(xg, yg)
    print '("Answer 11/2 ",i0,l2)', ans2, ans2==569052586852_int64

  end subroutine day2311


  function count_distsum(x, y) result(cnt)
    integer(int64), intent(in) :: x(:), y(:)
    integer(int64) :: cnt

    integer :: ngal, i, j

    ngal = size(x)
    if (size(y)/=ngal) error stop 'arrays x and y must have same size'
    cnt = 0
    do i=1, ngal-1
      do j=i+1, ngal
        cnt = cnt + abs(x(j)-x(i)) + abs(y(j)-y(i))
      end do
    end do
  end function count_distsum


  subroutine expand_galaxy(gal, xg, yg, nshift)
    character(len=1), intent(in) :: gal(:,:)
    integer(int64), allocatable, intent(out) :: xg(:), yg(:)
    integer(int64), intent(in) :: nshift
    ! each empty row/column replaced by "nshift" rows/columns

    logical :: empty_cols(size(gal,2)), empty_rows(size(gal,1))
    integer(int64) :: i, j, i1, j1 
    integer :: k, ngal

    empty_rows = .true.
    do i=1, size(gal,1)
      if (count(gal(i,:)==CH_GALAXY)>0) empty_rows(i)=.false.
    end do

    empty_cols = .true.
    do j=1, size(gal,2)
      if (count(gal(:,j)==CH_GALAXY)>0) empty_cols(j)=.false.
    end do

    ngal = count(gal==CH_GALAXY)
    allocate(xg(ngal), yg(ngal))

    k=0
    do i=1, size(gal,1)
      do j=1, size(gal,2)
        if (gal(i,j)/=CH_GALAXY) cycle
        i1 = i + (nshift-1)*count(empty_rows(1:i))
        j1 = j + (nshift-1)*count(empty_cols(1:j))
        k=k+1
        yg(k) = i1
        xg(k) = j1
      end do
    end do
    if (k/=ngal) error stop 'galaxies missing'
  end subroutine expand_galaxy


  subroutine print_pattern(map)
    character(len=1), intent(in) :: map(:,:)

    integer :: i

    do i=1, size(map,1)
      print '(*(a2))', map(i,:)
    end do
  end subroutine print_pattern

end module day2311_mod