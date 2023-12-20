module day2320_mod
  use parse_mod, only : read_strings, string_t, split
  implicit none

  integer, parameter :: LO_PULSE=0, HI_PULSE=1, STATE_OFF=0, STATE_ON=1
  character(len=*), parameter :: CH_TYPE='%&'
  integer, parameter :: TYPE_OTHER=0, TYPE_FLIP=1, TYPE_CONJ=2

  type pulse_t
    type(string_t) :: orig, dest
    integer :: magn
  end type

  type module_t
    type(string_t) :: label
    type(string_t), allocatable :: inps(:), outs(:)
    integer :: type
    integer, allocatable :: memo(:)
    integer :: state=STATE_OFF
  end type
  interface module_t
    module procedure module_parse
  end interface
! Flip-flop (%)
! - on/off-default
! - ignore hi-pulse
! - flip on lo-pulse, send hi/lo if it is now on/off
!
! Conjunction (&)
! - state for every input (lo-default/hi)
! - update this input's memory
! - then send lo-pulse if all are hi
! - otherwise send high-pulse

contains

  subroutine day2320(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(module_t), allocatable :: mods(:)
    integer :: i

    lines = read_strings(file)
    allocate(mods(size(lines)))
    do i=1, size(lines)
      mods(i) = module_t(lines(i)%str)
!call module_print(mods(i))
    end do
    call module_connect_inputs(mods)

    print *, 'modules = ',size(mods)

  end subroutine day2320


  subroutine module_connect_inputs(mods)
    type(module_t), intent(inout) :: mods(:)

    integer :: i, j, k

    do i=1, size(mods)
      do j=1, size(mods(i)%outs)
        k = find_module(mods, mods(i)%outs(j))
        if (k==0) then
          print *, 'module not found ', mods(i)%outs(j)%str
          cycle
        end if
        call module_addinp(mods(k), mods(i)%label)
      end do
    end do
  end subroutine module_connect_inputs


  subroutine module_addinp(this, label)
    class(module_t), intent(inout) :: this
    type(string_t), intent(in) :: label

    integer :: i

    if (.not. allocated(this%inps)) allocate(this%inps(0))
    do i=1, size(this%inps)
      if (this%inps(i)%str==label%str) then
        error stop 'double input'
      end if
    end do
    this%inps = [this%inps, label]
    if (allocated(this%memo)) deallocate(this%memo)
    allocate(this%memo(size(this%inps)), source=LO_PULSE)
  end subroutine module_addinp


  integer function find_module(mods, label) result(ind)
    type(module_t), intent(in) :: mods(:)
    type(string_t), intent(in) :: label

    integer :: i

    do i=1, size(mods)
      if (mods(i)%label%str==label%str) exit
    end do
    ind = i
    if (ind==size(mods)+1) ind = 0
  end function find_module


  type(module_t) function module_parse(str) result(new)
    character(len=*), intent(in) :: str

    type(string_t), allocatable :: tokens(:)
    integer :: i

    call split(str, ' ', tokens)

    select case(scan(CH_TYPE, tokens(1)%str(1:1)))
    case(TYPE_FLIP)
      new%label = string_t(tokens(1)%str(2:))
      new%type = TYPE_FLIP
    case(TYPE_CONJ)
      new%label = string_t(tokens(1)%str(2:))
      new%type = TYPE_CONJ
    case(TYPE_OTHER)
      new%label = string_t(tokens(1)%str)
      new%type = TYPE_OTHER
    end select

    if (tokens(2)%str /= '->') error stop '-> expected'
    allocate(new%outs(size(tokens)-2))
    do i=1, size(new%outs)
      associate(labout=>tokens(i+2)%str)
        if (labout(len(labout):len(labout))==',') then
          new%outs(i) = string_t(labout(:len(labout)-1))
        else
          new%outs(i) = string_t(labout)
        end if
      end associate
    end do
    new%state = STATE_OFF
  end function module_parse


  subroutine module_print(this)
    class(module_t), intent(in) :: this

    integer :: k

!   type(string_t) :: label
!   type(string_t), allocatable :: inps(:), outs(:)
!   integer :: type
!   integer, allocatable :: memo(:)
!   integer :: state=STATE_OFF

    print '(i1,a,"{ ",*(a,:,","))', this%type, this%label%str, (this%outs(k)%str,k=1,size(this%outs))
  end subroutine module_print

end module day2320_mod