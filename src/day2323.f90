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

    type(vertex_t), allocatable, target :: paths(:)
    type(state_t) :: s0, sn
    integer :: ibeg, iend, i, ans1

    call parse_path_network(file, paths, ibeg, iend, .true.)
    !call parse_path_network(file, paths, ibeg, iend, .true.)
    call reduce_network(paths)
    ibeg = findloc(paths%id, ibeg, dim=1)
    iend = findloc(paths%id, iend, dim=1)
    print *, size(paths), ibeg, iend
    do i=1, size(paths)
      paths(i)%id = i
      print '(i4,": ",4(i4,1x),"[",i3,"]")', paths(i)%id, paths(i)%adj, paths(i)%n
    end do

    s0%paths => paths
    allocate(s0%visited(size(paths)), source=.false.)
    s0%ipos = ibeg
    call dws(s0, iend, ans1, sn)
  print *, sn%value()
  print *, sn%visited
    ans1=ans1-1 ! ignore the first square 6609"too high"
    print '("Answer 23/1 ",i0,l2)', ans1, ans1==2018

  end subroutine day2323


  subroutine parse_path_network(file, paths, ibeg, iend, ispart2)
    character(len=*), intent(in) :: file
    type(vertex_t), intent(out), allocatable :: paths(:)
    integer, intent(out) :: ibeg, iend
    logical, intent(in) :: ispart2

    character(len=1), allocatable :: map0(:,:), map(:,:)
    character(len=*), parameter :: CH_PATH='.', CH_FOREST='#', CH_LEFT='<', &
      CH_RIGHT='>', CH_UP='^', CH_DOWN='v'
    integer, parameter :: DIRS(2,4)=reshape([0,1,1,0,0,-1,-1,0],[2,4])
    integer :: i, j, k, d, npaths
    integer, allocatable :: hash(:,:)

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

    ! Mark all accessible squares
    do i=1,ubound(map,1)-1
      do j=1,ubound(map,2)-1
        select case(map(i,j))
        case(CH_PATH)
          k = 0
          do d=1,4
            if (hash(i+DIRS(1,d), j+DIRS(2,d))==0) cycle
            k = k+1
            !paths(k,hash(i,j)) = hash(i+DIRS(1,d), j+DIRS(2,d))
            paths(hash(i,j))%adj(k) = hash(i+DIRS(1,d), j+DIRS(2,d))
          end do
        case(CH_RIGHT)
          if (hash(i+DIRS(1,1), j+DIRS(2,1))/=0) &
              !paths(1,hash(i,j)) = hash(i+DIRS(1,1), j+DIRS(2,1))
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,1), j+DIRS(2,1))
        case(CH_DOWN)
          if (hash(i+DIRS(1,2), j+DIRS(2,2))/=0) &
              !paths(1,hash(i,j)) = hash(i+DIRS(1,2), j+DIRS(2,2))
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,2), j+DIRS(2,2))
        case(CH_LEFT)
          if (hash(i+DIRS(1,3), j+DIRS(2,3))/=0) &
              !paths(1,hash(i,j)) = hash(i+DIRS(1,3), j+DIRS(2,3))
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,3), j+DIRS(2,3))
        case(CH_UP)
          if (hash(i+DIRS(1,4), j+DIRS(2,4))/=0) &
              !paths(1,hash(i,j)) = hash(i+DIRS(1,4), j+DIRS(2,4))
              paths(hash(i,j))%adj(1) = hash(i+DIRS(1,4), j+DIRS(2,4))
        case default
          continue
        end select
      end do
    end do

    return
    do i=1, ubound(hash,1)-1
      print '(*(i4,1x))', hash(i,:)
    end do
    do i=1, size(paths)
      print '(i4,": ",*(i4,1x))', i, paths(i)%adj
    end do
  end subroutine parse_path_network


  subroutine reduce_network(arr)
    type(vertex_t), allocatable, intent(inout) :: arr(:)

    integer :: i, j, last

    do
      last = size(arr)
      print *, 'redicing...', size(arr)
      i=0
      OUT: do
        i=i+1
        if (i>size(arr)-1) exit OUT
        j = i
        IN: do
          j=j+1
          if (j>size(arr)) exit IN

          if (count(arr(i)%adj>0)/=2 .or. count(arr(j)%adj>0)/=2) cycle 
          if (.not.(any(arr(i)%adj==j) .and. any(arr(j)%adj==0) )) cycle
          call join_vertices(arr,i,j)
          j = j-1
        end do IN
      end do OUT
      if (last==size(arr)) exit
    end do
  end subroutine


  subroutine join_vertices(arr,i,j)
    type(vertex_t), allocatable :: arr(:)
    integer, intent(in) :: i, j

    integer :: ind
    type(vertex_t), allocatable :: wrk(:)

    ! check vertices can be joined
    if (count(arr(i)%adj>0)==2 .and. count(arr(j)%adj>0)==2) then
      if (any(arr(i)%adj==j) .and. any(arr(j)%adj==i)) then
        if (arr(i)%adj(1)==j) then
          arr(i)%adj(1) = other(arr(j), i)
        else if (arr(i)%adj(2)==j) then
          arr(i)%adj(2) = other(arr(j), i)
        else
          error stop 'aaa'
        end if
        ! absorb "j" by "i"
        arr(i)%n = arr(i)%n + arr(j)%n
        ! everything that pointed to "j" will now point to "i"
        do ind=1, size(arr)
          where(arr(ind)%adj==j) arr(ind)%adj=i
        end do
        ! all labels larger than "j" will be decreased by one
        do ind=1, size(arr)
          where(arr(ind)%adj>j) arr(ind)%adj=arr(ind)%adj-1
        end do
        ! remove vertex "j" from table
        allocate(wrk(size(arr)-1))
        wrk(:j-1) = arr(:j-1)
        wrk(j:) = arr(j+1:)
        call move_alloc(wrk, arr)
      else
        error stop 'join_vertices - not adjacent'
      end if
    else
      error stop 'join_vertices - connectivity must be two'
    end if
  
  contains

    integer function other(this,ii) result(jj)
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


  recursive subroutine dws(s0, iend, n, sn)
    type(state_t), intent(in) :: s0
    integer, intent(in) :: iend
    integer, intent(out) :: n
    type(state_t), intent(out) :: sn

    integer :: i, k, n1
    type(state_t) :: s1, s2

    if (s0%ipos==iend) then
      n = s0%paths(s0%ipos)%n
      sn = s0
      sn%visited(s0%ipos) = .true.
      return
    end if
    sn = s0

    k = 0
    n = 0
    do i=1, 4
      associate(next=>s0%paths(s0%ipos)%adj(i))
        if (next==0) exit
        if (s0%visited(next)) cycle
        k = k + 1
        s1 = s0
        s1%visited(s0%ipos) = .true.
        s1%ipos = next
        call dws(s1, iend, n1, s2)
        if (n1 > n) then
          n = n1
          sn = s2
        end if

      end associate
    end do

    if (k>0) then
      n = n + s0%paths(s0%ipos)%n
    else
      sn = s0
      n = 0
    end if
  end subroutine dws


  integer function state_value(this) result(n)
    class(state_t), intent(in) :: this

    if (.not. associated(this%paths)) error stop 'null pointer'
    n = sum(this%paths%n, mask=this%visited)
  end function state_value
 
end module day2323_mod