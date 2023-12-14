module day2311_mod
  use parse_mod, only : read_pattern
  implicit none

  character(len=1), parameter :: CH_GALAXY='#', CH_EMPTY='.'

contains

  subroutine day2311(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: galaxy(:,:)
    integer :: ans1

    galaxy = read_pattern(file)
    call expand_galaxy(galaxy)
    ans1 = count_distsum(galaxy)
    !call print_pattern(galaxy)
    print '("Answer 11/1 ",i0,l2)', ans1, ans1==9724940


  end subroutine day2311


  function count_distsum(gal) result(cnt)
    character(len=1), intent(in) :: gal(:,:)
    integer :: cnt

    integer :: ngal, i, j, k
    integer, allocatable :: x(:), y(:)

    ngal = count(gal==CH_GALAXY)
    allocate(x(ngal), y(ngal))
    k = 0
    do i=1, size(gal,1)
      do j=1, size(gal,2)
        if (gal(i,j)/=CH_GALAXY) cycle
        k = k+1
        x(k) = i
        y(k) = j
      end do
    end do
    if (k/=ngal) error stop 'some galaxies missed'

    cnt = 0
    do i=1, ngal-1
      do j=i+1, ngal
        cnt = cnt + abs(x(j)-x(i)) + abs(y(j)-y(i))
      end do
    end do
  end function count_distsum


  subroutine expand_galaxy(gal)
    character(len=1), intent(inout), allocatable :: gal(:,:)

    character(len=1), allocatable :: galexp(:,:)
    logical :: empty_cols(size(gal,2)), empty_rows(size(gal,1))
    integer :: i, j, i1, j1

    empty_rows = .true.
    do i=1, size(gal,1)
      if (count(gal(i,:)==CH_GALAXY)>0) empty_rows(i)=.false.
    end do

    empty_cols = .true.
    do j=1, size(gal,2)
      if (count(gal(:,j)==CH_GALAXY)>0) empty_cols(j)=.false.
    end do

    allocate(galexp(size(gal,1)+count(empty_rows), size(gal,2)+count(empty_cols)))
    galexp = CH_EMPTY
    do i=1, size(gal,1)
      do j=1, size(gal,2)
        if (gal(i,j)/=CH_GALAXY) cycle
        i1 = i + count(empty_rows(1:i))
        j1 = j + count(empty_cols(1:j))
        galexp(i1,j1) = CH_GALAXY
      end do
    end do
    call move_alloc(galexp, gal)
  end subroutine expand_galaxy


  subroutine print_pattern(map)
    character(len=1), intent(in) :: map(:,:)

    integer :: i

    do i=1, size(map,1)
      print '(*(a2))', map(i,:)
    end do
  end subroutine print_pattern

end module day2311_mod