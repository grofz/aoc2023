module day2322_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type block_t
    integer :: x0(3), x1(3), id
  contains
    procedure, private :: block_eq
    generic :: operator(==) => block_eq
  end type 
  interface block_t
    module procedure block_new
  end interface

  integer, parameter :: NULL=0, OP_ADD=1, OP_REM=2

contains

  subroutine day2322(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(block_t), allocatable :: blocks(:), new(:), old(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    allocate(blocks(size(lines)))
    do i=1, size(lines)
      blocks(i) = block_t(lines(i)%str, id=i)
    end do

    ! Let the blocks fall down
    blocks = block_falldown(blocks)

    ! Test which blocks can be removed
    ans1 = 0
    ans2 = 0
    allocate(new(size(blocks)-1), old(size(blocks)-1))
    do i=1, size(blocks)
      old(1:i-1) = blocks(1:i-1)
      old(i:) = blocks(i+1:)
      new = block_falldown(old)
      if (all(new==old)) ans1 = ans1 + 1
      ans2 = ans2 + count(.not.(new==old))
    end do
    print '("Answer 22/1 ",i0,l2)', ans1, ans1==463
    print '("Answer 22/2 ",i0,l2)', ans2, ans2==89727
  end subroutine day2322


  function block_falldown(oldblocks) result(newblocks)
    class(block_t), intent(in) :: oldblocks(:)
    type(block_t) :: newblocks(size(oldblocks))

    integer, allocatable :: map(:,:,:)
    integer :: i, lb(3), ub(3)
    logical :: moved

    do i=1,3
      lb(i) = minval(oldblocks%x0(i))
      ub(i) = maxval(oldblocks%x1(i))
    end do
    allocate(map(lb(1):ub(1), lb(2):ub(2), 1:ub(3)), source=NULL)
    do i=1, size(oldblocks)
      call block_addremove(oldblocks(i), map, OP_ADD)
    end do

    ! Move everything down until steady state reached
    newblocks = oldblocks
    do
      moved = .false.
      do i=1,size(newblocks)
        if (.not. block_canmove(newblocks(i), map)) cycle
        call block_addremove(newblocks(i), map, OP_REM)
        newblocks(i)%x0(3) = newblocks(i)%x0(3)-1
        newblocks(i)%x1(3) = newblocks(i)%x1(3)-1
        call block_addremove(newblocks(i), map, OP_ADD)
        moved = .true.
      end do
      if (.not. moved) exit
    end do 
  end function block_falldown


  function block_colides(this, map) result(colides)
    class(block_t), intent(in) :: this
    integer, intent(in), allocatable :: map(:,:,:)
    logical colides

    integer :: i, j, k

    colides = .false.
    OUT: do i=this%x0(1), this%x1(1)
      do j=this%x0(2), this%x1(2)
        do k=this%x0(3), this%x1(3)
          if (map(i,j,k)/=NULL .and. map(i,j,k)/=this%id) then
            colides = .true.
            exit OUT
          end if
        end do
      end do
    end do OUT
  end function block_colides


  subroutine block_addremove(this, map, operation)
    class(block_t), intent(in) :: this
    integer, intent(inout), allocatable :: map(:,:,:)
    integer, intent(in) :: operation

    integer :: i, j, k

    OUT: do i=this%x0(1), this%x1(1)
      do j=this%x0(2), this%x1(2)
        do k=this%x0(3), this%x1(3)
          select case(operation)
          case(OP_ADD)
            if (map(i,j,k)/=NULL) error stop 'space occupied'
            map(i,j,k) = this%id
          case(OP_REM)
            if (map(i,j,k)/=this%id) error stop 'block at wrong position'
            map(i,j,k) = NULL
          case default 
            error stop 'block_addremove - wrong operation type'
          end select
        end do
      end do
    end do OUT
  end subroutine block_addremove


  function block_canmove(this, map) result(canmove)
    class(block_t), intent(in) :: this
    integer, intent(in), allocatable :: map(:,:,:)
    logical :: canmove
    
    type(block_t) :: moved

    if (this%x0(3)==1) then
      canmove = .false.
    else
      moved = this
      moved%x0(3) = moved%x0(3)-1
      moved%x1(3) = moved%x1(3)-1
      canmove = .not. block_colides(moved, map)
    end if
  end function block_canmove


  type(block_t) function block_new(str, id) result(new)
    character(len=*), intent(in) :: str
    integer, intent(in) :: id

    type(string_t), allocatable :: word0(:), word1(:)
    integer :: is, i

    is = scan(str,'~')
    if (is==0) error stop 'parsing error (~ not found)'
    call split(str(:is-1), ',', word0)
    call split(str(is+1:), ',', word1)
    if (size(word0)/=3 .or. size(word1)/=3) error stop 'parsing error (not 3 numbers)'
    do i=1, 3
      read(word0(i)%str,*) new%x0(i)
      read(word1(i)%str,*) new%x1(i)
      if (new%x0(i)>new%x1(i)) error stop 'x0 must be lower than x1'
    end do
    new%id = id
  end function block_new


  elemental function block_eq(a, b) result(eq)
    class(block_t), intent(in) :: a, b
    logical :: eq

    eq = all(a%x0==b%x0) .and. all(a%x1==b%x1)
  end function block_eq

end module day2322_mod