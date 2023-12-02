module day2302_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

contains
  subroutine day2302(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer, parameter :: MAX_CUBES(3) = [12,13,14]
    integer, allocatable :: cubes(:,:)
    integer :: i, ans1, ans2, tmp

    lines = read_strings(file)
    ans1 = 0
    ans2 = 0
    do i=1, size(lines)
      associate(pos => scan(lines(i)%str, ':'))
        call parse_game(lines(i)%str(pos+1:), cubes)
      end associate
      if (valid_game(cubes,MAX_CUBES)) ans1 = ans1 + i
      ans2 = ans2 + product(maxval(cubes, dim=2))
    end do
    print '("Answer 02/1 ",i0, l2)', ans1, ans1==2265
    print '("Answer 02/2 ",i0, l2)', ans2, ans2==64097
  end subroutine day2302


  logical function valid_game(cubes, max_cubes)
    integer, intent(in) :: cubes(:,:), max_cubes(:)

    integer :: iround

    valid_game  = .true.
    do iround=1, size(cubes,2) ! TODO this may be rewritten without the loop
      if (all(cubes(:,iround)<=max_cubes)) cycle
      valid_game = .false.
      exit
    end do
  end function valid_game


  subroutine parse_game(line, cubes)
    character(len=*), intent(in) :: line
    integer, intent(out), allocatable :: cubes(:,:)

    type(string_t), allocatable :: rounds(:), colors(:)
    integer :: i, j, k, kpos

    character(len=5), parameter, dimension(3) :: names = &
      [ 'red  ', 'green', 'blue ']

    call split(line, ';', rounds)
    allocate(cubes(3,size(rounds)))
    cubes = 0
    ROUND: do i=1, size(rounds)
      call split(rounds(i)%str, ',', colors)
      COLOR: do j=1, size(colors)
        ! recognize the color within a round
        do k=1,3
          kpos = index(colors(j)%str, trim(names(k)), .true.)
          if (kpos==0) cycle
          if (cubes(k,i)/=0) error stop 'same color defined in one round'
          read(colors(j)%str(1:kpos-1), *) cubes(k,i)
          exit
        end do
      end do COLOR
    end do ROUND
  end subroutine parse_game

end module day2302_mod