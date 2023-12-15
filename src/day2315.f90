module day2315_mod
  use parse_mod, only : string_t, read_strings, split
  implicit none

  type len_t
    type(string_t) :: label
    integer :: focus
  end type len_t
  interface len_t
    module procedure len_new
  end interface
  ! TODO add defined assignment for len_t

  type box_t
    type(len_t), allocatable :: lens(:)
    integer :: nlen=0
  end type

  integer, parameter :: TYPE_REMOVE=1, TYPE_REPLACE=2

contains

  subroutine day2315(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:), steps(:)
    integer :: ans1, ans2, i, type, focus, boxid
    type(string_t) :: label
    type(box_t) :: boxes(0:255)

    lines = read_strings(file)
    if (size(lines)/=1) error stop 'just one line expected'
    call split(lines(1)%str,',',steps)
    ans1 = 0
    do i=1, size(steps)
      ans1 = ans1 + hash(steps(i)%str)
    end do
    print '("Answer 15/1 ",i0,l2)', ans1, ans1==511498

    do i=1, size(steps)
      call parse_step(steps(i)%str, type, label, focus)
      boxid = hash(label%str)
      select case(type)
      case(TYPE_REMOVE)
        call box_remove(boxes(boxid), label)
      case(TYPE_REPLACE)
        call box_addlen(boxes(boxid), len_t(label%str, focus))
      end select
    end do
    ans2 = countall(boxes)
    print '("Answer 15/2 ",i0,l2)', ans2, ans2==284674
  end subroutine day2315


  function countall(boxes) result(cnt)
    type(box_t), intent(in) :: boxes(0:)
    integer :: cnt

    integer :: i, j, k

    cnt = 0
    do i=0, size(boxes)-1
      do j=1, boxes(i)%nlen
        k = boxes(i)%lens(j)%focus
        cnt = cnt + (i+1)*j*k
      end do
    end do
  end function countall


  subroutine box_addlen(this, len)
    type(box_t), intent(inout) :: this
    type(len_t), intent(in) :: len

    type(len_t), allocatable :: wrklens(:)
    integer :: i

    if (.not. allocated(this%lens)) then
      allocate(this%lens(10))
      this%nlen = 0
    else if (size(this%lens)==this%nlen) then
      allocate(wrklens(size(this%lens)*2))
      do i=1, this%nlen
        wrklens(i)%label = string_t(this%lens(i)%label%str)
        wrklens(i)%focus = this%lens(i)%focus
      end do
      call move_alloc(wrklens, this%lens)
    end if

    ! scan lens, if the len with the same label found, then replace
    do i=1, this%nlen
      if (len%label%str==this%lens(i)%label%str) then
        this%lens(i)%focus = len%focus
        exit
      end if
    end do

    ! add the len at the end
    if (i==this%nlen+1) then
      this%lens(i)%label = string_t(len%label%str)
      this%lens(i)%focus = len%focus
      this%nlen = i
    end if
  end subroutine box_addlen


  subroutine box_remove(this, label)
    type(box_t), intent(inout) :: this
    type(string_t), intent(in) :: label

    integer :: i, j

    do i=1, this%nlen
      if (this%lens(i)%label%str==label%str) exit
    end do

    if (i==this%nlen+1) return

    do j=i, this%nlen-1
      this%lens(j)%label = string_t(this%lens(j+1)%label%str)
      this%lens(j)%focus = this%lens(j+1)%focus
    end do
    this%nlen = this%nlen-1
  end subroutine box_remove


  function hash(str) result(res)
    character(len=*), intent(in) :: str
    integer :: res

    integer :: i

    res = 0
    do i=1,len(str)
      res = res + iachar(str(i:i))
      res = res*17
      res = mod(res,256)
    end do
  end function hash


  subroutine parse_step(str, type, label, focus)
    character(len=*), intent(in) :: str
    integer, intent(out) :: type
    type(string_t), intent(out) :: label
    integer, intent(out) :: focus

    integer :: i, j

    i = scan(str, '-')
    j = scan(str, '=')
    if (i==0 .and. j/=0) then
      type = TYPE_REPLACE
      label = string_t(str(:j-1))
      read(str(j+1:),*) focus
    else if (i/=0 .and. j==0) then
      type = TYPE_REMOVE
      label = string_t(str(:i-1))
      focus = 0
    else
      error stop 'parsing error'
    end if
  end subroutine parse_step


  type(len_t) function len_new(label,focus) result(new)
    character(len=*), intent(in) :: label
    integer, intent(in) :: focus

    new%label = string_t(label)
    new%focus = focus
  end function len_new

end module day2315_mod