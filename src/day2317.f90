module day2317_mod
  use parse_mod, only : read_pattern
  use iso_fortran_env, only : int64
  use djikstra_mod
  implicit none

  integer, parameter :: DIRS(2,4)=reshape([0,1,1,0,0,-1,-1,0],[2,4])
  integer, parameter :: MAX_STRIKES = 0

  integer, allocatable :: heatmap(:,:)

  type, extends(djikstra_node_at) :: state_t
    integer :: dir
    integer :: strike=0
    integer :: pos(2)
  contains
    procedure :: nextngb
    procedure :: isequal, istarget
    procedure, private :: is_gt
    generic :: operator(>) => is_gt
  end type state_t

  type statestack_t
    type(state_t), allocatable :: arr(:)
    integer :: n=0
  end type

contains

  subroutine day2317(file)
    character(len=*), intent(in) :: file

    integer :: i, shortest
    type(djikstra_node_ptr), allocatable :: nodes(:)
    type(state_t) :: start

    heatmap = read_numbers(file)

    start%pos = [2,1]
    start%strike = 1
    start%dir = 2
    call djikstra_search(nodes, start, shortest)
print *, size(nodes), shortest+heatmap(start%pos(1),start%pos(2))
deallocate(nodes)

    start%pos = [1,2]
    start%strike = 1
    start%dir = 1
    call djikstra_search(nodes, start, shortest)
print *, size(nodes), shortest+heatmap(start%pos(1),start%pos(2))

  end subroutine day2317


  subroutine nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    type(state_t) :: next
!print *, 'node ',node%pos, node%strike, node%dir, flag

    do
      flag = flag + 1
      if (flag > 3) then
        flag = 0
        exit
      end if
      next = next_state(node, flag)

      if (next%pos(1)<1 .or. next%pos(2)<1 .or. next%pos(1)>size(heatmap,1) .or. &
          next%pos(2)>size(heatmap,2)) then
            cycle
      else if (next%strike > 3) then
        cycle
      else
!print *, 'next ',next%pos, next%strike, next%dir, flag
        distance = heatmap(next%pos(1),next%pos(2))
        exit
      end if
    end do

    if (flag/=0) then
      allocate(node_ngb, source=next)
    end if

select type(node_ngb)
class is (state_t)
!  print *, 'next ngb ',flag, node_ngb%pos
class default
  if (flag/=0) error stop 'class not expected'
end select
  end subroutine nextngb


  function next_state(this, choice) result(next)
    class(state_t), intent(in) :: this
    integer, intent(in) :: choice
    type(state_t) :: next

    select case(choice)
    case(1) ! turn left and move one step
      next%dir = mod(this%dir,4)+1
      next%strike = 1
    case(2) ! turn right and move one step
      next%dir = this%dir-1
      if (next%dir==0) next%dir=4 ! TODO (??)
      next%strike = 1
    case(3) ! move one step
      next%dir = this%dir
      next%strike = this%strike+1
    case default
      error stop 'invalid choice'
    end select

    next%pos = this%pos + DIRS(:,next%dir)

  end function next_state


  logical function isequal(anode, bnode)
    class(state_t), intent(in) :: anode
    class(djikstra_node_at), intent(in) :: bnode

    select type (bnode)
    class is (state_t)
      isequal = all(anode%pos==bnode%pos) .and. &
        anode%strike==bnode%strike .and. &
        anode%dir==bnode%dir
    class default
      error stop 'unexpected class'
    end select
  end function isequal


  logical function istarget(node)
    class(state_t), intent(in) :: node
    istarget = node%pos(1)==size(heatmap,1) .and. node%pos(2)==size(heatmap,2)
  end function istarget


  function read_numbers(file) result(numbers)
    character(len=*), intent(in) :: file
    integer, allocatable :: numbers(:,:)

    character(len=1), allocatable :: map(:,:)
    integer :: i, j

    map = read_pattern(file)
    allocate(numbers(size(map,1), size(map,2)))
    do i=1,size(map,1)
      do j=1,size(map,2)
        read(map(i,j),*) numbers(i,j)
      end do
    end do
  end function read_numbers


  subroutine statestack_add(this, new)
    class(statestack_t), intent(inout) :: this
    type(state_t), intent(in) :: new

    type(state_t), allocatable :: wrks(:)
    integer :: i

    if (.not. allocated(this%arr)) allocate(this%arr(10))
    if (size(this%arr)==this%n) then
      allocate(wrks(2*size(this%arr)))
      wrks(1:size(this%arr)) = this%arr
      call move_alloc(wrks, this%arr)
    end if

    do i=1, this%n
      if (this%arr(i)>new) then
        this%arr(i+1:this%n+1) = this%arr(i:this%n)
        exit
      end if
    end do
    this%arr(i) = new

    this%n = this%n + 1
  end subroutine statestack_add


  subroutine statestack_remove(this, removed)
    class(statestack_t), intent(inout) :: this
    type(state_t), intent(out) :: removed

    if (this%n==0) error stop 'statestack is empty'
    removed = this%arr(this%n)
    this%n = this%n-1
  end subroutine statestack_remove


  logical function is_gt(a, b)
    class(state_t), intent(in) :: a, b

    if (a%pos(1)+a%pos(2)>b%pos(1)+b%pos(2)) then
      is_gt = .true.
    else if (a%pos(1)+a%pos(2)<b%pos(1)+b%pos(2)) then
      is_gt = .false.
    else
      is_gt = .false.
    end if
  end function is_gt



end module day2317_mod