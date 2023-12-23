module day2323_mod
  use parse_mod, only : read_pattern
  implicit none

  type vertex_t
    integer :: id
    integer :: adj(4)=0
    integer :: n=1
  end type

  type state_t
    logical, allocatable :: visited(:)
    type(vertex_t), pointer :: paths(:)=>null()
    integer :: ipos
  contains
    procedure :: value => state_value
  end type

contains

  subroutine day2323(file)
    character(len=*), intent(in) :: file

    integer :: ans1, ans2

    call solve_problem(file, .false., ans1)
    call solve_problem(file, .true., ans2)
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==2018
    print '("Answer 23/2 ",i0,l2)', ans2, ans2==6406
  end subroutine day2323


  subroutine solve_problem(file, ispart2, ans)
    character(len=*), intent(in) :: file
    logical, intent(in) :: ispart2
    integer, intent(out) :: ans

    type(vertex_t), allocatable, target :: paths(:)
    type(state_t) :: s0, sn
    integer :: ibeg, iend, i
    integer, allocatable :: hash(:,:)
    character(len=1), allocatable :: map(:,:)

    call parse_paths(file, ispart2, paths, ibeg, iend, hash)
    call reduce_paths(paths, hash, ibeg, iend)

    s0%paths => paths
    allocate(s0%visited(size(paths)), source=.false.)
    s0%ipos = ibeg
    call dws(s0, iend, sn)
    ans = sn%value()-1 ! the starting square should not be counted

    ! visualize the path
    allocate(map(lbound(hash,1):ubound(hash,1), lbound(hash,2):ubound(hash,2)))
    map = ' '
    do i=1,size(paths)
      if (.not. sn%visited(i)) then
        where(hash==i) hash=-1
      end if
    end do
    where(hash>0) 
      map = 'O' ! best path
    else where(hash==-1)
      map = 'X' ! not part of the best part
    end where
    do i=lbound(map,1), ubound(map,1)
      print '(*(a2))', map(i,:)
    end do
  end subroutine solve_problem


  subroutine parse_paths(file, ispart2, paths, ibeg, iend, hash)
    character(len=*), intent(in) :: file
    logical, intent(in) :: ispart2
    type(vertex_t), intent(out), allocatable :: paths(:)
    integer, intent(out) :: ibeg, iend
    integer, allocatable, intent(out) :: hash(:,:)

    character(len=1), allocatable :: map0(:,:), map(:,:)
    character(len=*), parameter :: CH_PATH='.', CH_FOREST='#', CH_LEFT='<', &
      CH_RIGHT='>', CH_UP='^', CH_DOWN='v'
    integer, parameter :: DIRS(2,4)=reshape([0,1,1,0,0,-1,-1,0],[2,4])
    integer :: i, j, k, d, npaths

    map0 = read_pattern(file)
    if (ispart2) then
      where(map0==CH_LEFT) map0=CH_PATH
      where(map0==CH_RIGHT) map0=CH_PATH
      where(map0==CH_UP) map0=CH_PATH
      where(map0==CH_DOWN) map0=CH_PATH
    end if

    allocate(map(0:size(map0,1)+1, 0:size(map0,2)+1), source=CH_FOREST)
    map(1:size(map0,1), 1:size(map0,2)) = map0
    npaths = count(map/=CH_FOREST)

    allocate(paths(npaths))
    do i=1, npaths
      paths(i)%id = i
      paths(i)%adj = 0
      paths(i)%n = 1
    end do
    allocate(hash(0:size(map,1)-1, 0:size(map,2)-1), source=0)
    ibeg = 0
    iend = 0

    ! Count accessible squares, assign them an unique label "k"
    ! Identify the starting and target squares
    k = 0
    do i=1,ubound(map,1)-1
      do j=1,ubound(map,2)-1
        if (map(i,j)==CH_FOREST) cycle
        ! square can be walked
        k = k + 1
        hash(i,j) = k

        if (i==1) then
          if (map(i,j)/=CH_PATH) error stop 'path not at first line'
          if (ibeg/=0) error stop 'more than one path at first line'
          ibeg = k
        end if

        if (i==size(map0,1)) then
          if (map(i,j)/=CH_PATH) error stop 'path not at last line'
          if (iend/=0) error stop 'more than one path at last line'
          iend = k
        end if

      end do
    end do
    if (k/=size(paths)) error stop 'some squares missed'

    ! Mark adjacent accessible squares
    ! Slope squares have just one next neighbour
    do i=1,ubound(map,1)-1
      do j=1,ubound(map,2)-1
        select case(map(i,j))
        case(CH_PATH)
          k = 0
          do d=1,4
            if (hash(i+DIRS(1,d), j+DIRS(2,d))==0) cycle
            k = k+1
            paths(hash(i,j))%adj(k) = hash(i+DIRS(1,d), j+DIRS(2,d))
          end do
        case(CH_RIGHT)
          if (hash(i+DIRS(1,1), j+DIRS(2,1))/=0) &
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,1), j+DIRS(2,1))
        case(CH_DOWN)
          if (hash(i+DIRS(1,2), j+DIRS(2,2))/=0) &
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,2), j+DIRS(2,2))
        case(CH_LEFT)
          if (hash(i+DIRS(1,3), j+DIRS(2,3))/=0) &
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,3), j+DIRS(2,3))
        case(CH_UP)
          if (hash(i+DIRS(1,4), j+DIRS(2,4))/=0) &
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,4), j+DIRS(2,4))
        case default
          continue
        end select
      end do
    end do
  end subroutine parse_paths


  subroutine reduce_paths(arr, hash, ibeg, iend)
    type(vertex_t), allocatable, intent(inout) :: arr(:)
    integer, intent(inout) :: hash(:,:), ibeg, iend

    integer :: i, j, last

    MAIN: do
      last = size(arr)
      print *, 'Reducing...', size(arr)
      i=0
      OUT: do
        i=i+1
        if (i>size(arr)-1) exit OUT
        j = i
        IN: do
          j=j+1
          if (j>size(arr)) exit IN

          ! Pair "i" | "j" can be condensed in one if:
          ! - their connectivity is "2"
          if (.not.(count(arr(i)%adj>0)==2 .and. count(arr(j)%adj>0)==2)) cycle 
          ! - their are next to each other
          if (.not.(any(arr(i)%adj==j) .and. any(arr(j)%adj==i) )) cycle

          call join_vertices(arr,i,j,hash)
          j = j-1
        end do IN
      end do OUT
      if (last==size(arr)) exit MAIN
    end do MAIN

    ! Labels were changed, up-date them
    ibeg = findloc(arr%id, ibeg, dim=1)
    iend = findloc(arr%id, iend, dim=1)
    do i=1, size(arr)
      arr(i)%id = i
    end do
  end subroutine reduce_paths


  subroutine join_vertices(arr, i, j, hash)
    type(vertex_t), allocatable :: arr(:)
    integer, intent(in) :: i, j
    integer, intent(inout) :: hash(:,:)

    integer :: ind
    type(vertex_t), allocatable :: wrk(:)

    ! check vertices can be joined
    if (.not.(count(arr(i)%adj>0)==2 .and. count(arr(j)%adj>0)==2)) &
        error stop 'join_vertices - connectivity must be two'
    if (.not. (any(arr(i)%adj==j) .and. any(arr(j)%adj==i))) &
        error stop 'join_vertices - not adjacent'

    ! add "j"'s neighbour to the adjacency list of the "i"
    if (arr(i)%adj(1)==j) then
      arr(i)%adj(1) = other(arr(j), i)
    else if (arr(i)%adj(2)==j) then
      arr(i)%adj(2) = other(arr(j), i)
    else
      error stop 'impossible'
    end if

    ! absorb "j" by "i"
    arr(i)%n = arr(i)%n + arr(j)%n

    ! everything that pointed to "j" will now point to "i"
    do ind=1, size(arr)
      where(arr(ind)%adj==j) arr(ind)%adj=i
    end do
    where(hash==j) hash=i

    ! all labels larger than "j" will be decreased by one
    do ind=1, size(arr)
      where(arr(ind)%adj>j) arr(ind)%adj=arr(ind)%adj-1
    end do
    where(hash>j) hash=hash-1

    ! remove vertex "j" from table
    allocate(wrk(size(arr)-1))
    wrk(:j-1) = arr(:j-1)
    wrk(j:) = arr(j+1:)
    call move_alloc(wrk, arr)
  
  contains

    integer function other(this, ii) result(jj)
      class(vertex_t), intent(in) :: this
      integer, intent(in) :: ii
      if (this%adj(1)==ii) then
        jj=this%adj(2)
      else if (this%adj(2)==ii) then
        jj=this%adj(1)
      else
        error stop 'join_vertices'
      end if
    end function other
  end subroutine join_vertices


  recursive subroutine dws(s0, iend, sn)
    type(state_t), intent(in) :: s0
    integer, intent(in) :: iend
    type(state_t), intent(out) :: sn

    integer :: i, nbest
    type(state_t) :: s1, s2

    if (s0%ipos==iend) then
      ! target reached
      sn = s0
      sn%visited(s0%ipos) = .true.
      return
    end if

    ! nullify pointer to mark a death-end path
    nbest = -huge(nbest)
    sn%paths=>null()

    ! branch-off to different directions and return the longest path that
    ! ends at the target
    do i=1, 4
      associate(next=>s0%paths(s0%ipos)%adj(i))
        if (next==0) exit
        if (s0%visited(next)) cycle
        s1 = s0
        s1%visited(s0%ipos) = .true.
        s1%ipos = next
      end associate

      call dws(s1, iend, s2)

      associate(nn=>s2%value())
        if (nn > nbest) then
          nbest = nn
          sn = s2
        end if
      end associate
    end do
  end subroutine dws


  integer function state_value(this) result(n)
    class(state_t), intent(in) :: this
!
! The number of visited squares or "-Inf" if no path to target
!
    if (.not. associated(this%paths)) then
      n = -huge(n)
    else
      n = sum(this%paths%n, mask=this%visited)
    end if
  end function state_value
 
end module day2323_mod