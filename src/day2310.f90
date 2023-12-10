module day2310_mod
  use parse_mod, only : read_pattern
  implicit none

  ! "read_pattern" makes an array with [row, column] indexing, therefore:
  ! east(1), south(2), west(3), north(4)
  integer, parameter :: DIRS(2,4) = reshape([0,1, 1,0, 0,-1, -1,0],[2,4])
  ! DIRS indexing: [index, heading]

  ! Characters representing the pipes
  character(len=*), parameter :: CHARS='|-LJ7F'
  ! | = north / south
  ! - = east / west
  ! L = north / east
  ! J = north / west
  ! 7 = south / west
  ! F = south / east
  integer, parameter :: CONDIRS(2,6) = reshape([2,4, 1,3, 1,4, 3,4, 2,3, 1,2],[2,6])
  ! CONDIR indexing: [heading direction, character position]

contains

  subroutine day2310(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: map(:,:)
    integer :: i, ierr, ans1, ans2
    integer, dimension(2) :: xstart, x, xprev, xnext
    integer, allocatable :: path(:)

    ! Find the starting tile pairs that is next to 'S'
    map = read_pattern(file)
    xstart = findloc(map, 'S')
    do i=1,4
      xprev = xstart+DIRS(:,i)
      x = next_pos(xprev, xstart, map, ierr)
      if (ierr==0) exit
    end do
    path = [xstart(1), xstart(2), xprev(1), xprev(2), x(1), x(2)]

    ! Follow the pipe until 'S'
    ans1 = 2
    do
      xnext = next_pos(x, xprev, map, ierr)
      if (ierr==2) error stop 'loop lost'
      if (ierr==1) exit
      ans1 = ans1 + 1
      path = [path, xnext(1), xnext(2)]
      xprev = x
      x = xnext
    end do
    if (mod(ans1,2)/=0) error stop 'odd number of steps'
    ans1 = ans1/2
    print '("Answer 10/1 ",i0,l2)', ans1, ans1==7097

    ! Part 2
    ! Using https://en.wikipedia.org/wiki/Shoelace_formula
    ! Remove the area of pipe squares
    ! Add one - I do not know why, but it seems to work!
    ans2 = abs(trapezoid(path))-ans1+1
    print '("Answer 10/2 ",i0,l2)', ans2, ans2==355
  end subroutine day2310


  function trapezoid(arr) result(area)
    integer, intent(in) :: arr(:)
    integer :: area

    integer :: i
    integer :: xy(2,size(arr)/2)

    area = 0
    if (mod(size(arr),2)/=0) error stop 'trapezoid - size of array is odd'
    xy = reshape(arr,[2,size(arr)/2])
    do i=1,size(xy,2)-1
      area = area + (xy(2,i)+xy(2,i+1))*(xy(1,i)-xy(1,i+1))
    end do
    if (mod(area,2)/=0) error stop 'trapezoid - cannot divide by two'
    area = area/2
  end function trapezoid


  function next_pos(x, xprev, map, ierr) result(xnext)
    integer, intent(in) :: x(2), xprev(2)
    character(len=1), intent(in) :: map(:,:)
    integer, intent(out), optional :: ierr
    integer :: xnext(2)

    integer :: x1(2), x2(2), p, ierr0

    ierr0 = 0
    p = scan(CHARS,map(x(1),x(2)))
    if (p==0) then
      ierr0 = 1
    else
      x1 = x + DIRS(:,CONDIRS(1,p))
      x2 = x + DIRS(:,CONDIRS(2,p))
      if (all(x1==xprev)) then
        xnext = x2
      else if (all(x2==xprev)) then
        xnext = x1
      else
        ierr0 = 2
      end if
    end if

    select case(ierr0)
    case(0) ! OK
      continue
    case(1) ! Found 'S' probably
      if (.not. present(ierr)) &
          error stop 'next_pos: ch is not a valid pipe tile'
    case(2)
      if (.not. present(ierr)) &
          error stop 'next_pos: tile is not connected to xprev'
    case default
      error stop 'impossible'
    end select
    if (present(ierr)) ierr = ierr0
  end function next_pos

end module day2310_mod