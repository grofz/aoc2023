module day2317_mod
  use parse_mod, only : read_pattern
  use djikstra_mod
  implicit none

  integer, parameter :: DIRS(2,4)=reshape([0,1,1,0,0,-1,-1,0],[2,4])

  ! Global variables for Djikstra functions
  integer :: MIN_STRIKE = 1, MAX_STRIKE=3
  integer, allocatable :: heatmap(:,:)

  type, extends(djikstra_node_at) :: state_t
    integer :: dir
    integer :: strike=0
    integer :: pos(2)
  contains
    procedure :: nextngb, calchash
    procedure :: isequal, istarget
  end type state_t

contains

  subroutine day2317(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2

    heatmap = read_numbers(file)
    ans1 = get_answer(1,3)
    ans2 = get_answer(4,10)
    print '("Answer 17/1 ",i0,l2)', ans1, ans1==851
    print '("Answer 17/2 ",i0,l2)', ans2, ans2==982
  end subroutine day2317


  function get_answer(a, b) result(ans)
    integer, intent(in) :: a, b
    integer :: ans

    integer :: shortest, hash_size
    type(djikstra_node_ptr), allocatable :: nodes(:)
    type(state_t) :: start

    MIN_STRIKE = a
    MAX_STRIKE = b
    hash_size = size(heatmap)*4*(MAX_STRIKE-MIN_STRIKE+1)
    start%strike = MIN_STRIKE

    ! down
    start%dir = 2
    start%pos = [1,1] + DIRS(:,start%dir)*MIN_STRIKE
    call djikstra_search(nodes, start, shortest, hash_size)
print *, size(nodes), shortest+sum(heatmap(2:start%pos(1),start%pos(2)))
    ans = shortest+sum(heatmap(2:start%pos(1),start%pos(2)))

    ! right
    deallocate(nodes)
    start%dir = 1
    start%pos = [1,1] + DIRS(:,start%dir)*MIN_STRIKE
    call djikstra_search(nodes, start, shortest, hash_size)
print *, size(nodes), shortest+sum(heatmap(start%pos(1),2:start%pos(2)))
    ans = min(ans, shortest+sum(heatmap(start%pos(1),2:start%pos(2))))
  end function get_answer


  subroutine nextngb(node, flag, node_ngb, distance)
    class(state_t), intent(in) :: node
    integer, intent(inout) :: flag
    class(djikstra_node_at), intent(out), allocatable :: node_ngb
    integer, intent(out) :: distance

    type(state_t) :: next
    integer :: i1, i2, j1, j2

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
      else if (next%strike > MAX_STRIKE) then
        cycle
      else
        i1 = min(node%pos(1), next%pos(1))
        i2 = max(node%pos(1), next%pos(1))
        j1 = min(node%pos(2), next%pos(2))
        j2 = max(node%pos(2), next%pos(2))
        distance = sum(heatmap(i1:i2,j1:j2))-heatmap(node%pos(1),node%pos(2))
        exit
      end if
    end do
    if (flag/=0) allocate(node_ngb, source=next)

  end subroutine nextngb


  function next_state(this, choice) result(next)
    class(state_t), intent(in) :: this
    integer, intent(in) :: choice
    type(state_t) :: next

    select case(choice)
    case(1) ! turn left and move minimum steps
      next%dir = mod(this%dir,4)+1
      next%strike = MIN_STRIKE
    next%pos = this%pos + MIN_STRIKE*DIRS(:,next%dir)
    case(2) ! turn right and move minimum steps
      next%dir = this%dir-1
      if (next%dir==0) next%dir=4 ! TODO (??)
      next%strike = MIN_STRIKE
      next%pos = this%pos + MIN_STRIKE*DIRS(:,next%dir)
    case(3) ! move one step
      next%dir = this%dir
      next%strike = this%strike+1
      next%pos = this%pos + DIRS(:,next%dir)
    case default
      error stop 'invalid choice'
    end select

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


  integer function calchash(node) result(hash)
    class(state_t), intent(in) :: node

    integer :: nx, ny
    integer, parameter :: ndir=4

    nx = size(heatmap,1)
    ny = size(heatmap,2)

    hash = node%pos(1)
    hash = hash + (node%pos(2)-1)*nx
    hash = hash + (node%dir-1)*nx*ny
    hash = hash + (node%strike-MIN_STRIKE)*nx*ny*ndir

    if (hash<1 .or. hash > nx*ny*ndir*(MAX_STRIKE-MIN_STRIKE+1)) then
      print *, node%pos, node%dir, node%strike
      error stop 'invalid hash'
    end if
  end function calchash


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

end module day2317_mod