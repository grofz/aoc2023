module day2320_mod
  use parse_mod, only : read_strings, string_t, split
  use iso_fortran_env, only : I8 => int64
  implicit none

  integer, parameter :: LO_PULSE=0, HI_PULSE=1, STATE_OFF=0, STATE_ON=1
  character(len=*), parameter :: CH_TYPE='%&'
  integer, parameter :: TYPE_OTHER=0, TYPE_FLIP=1, TYPE_CONJ=2

  type pulse_t
    integer :: orig, dest, magn
    type(pulse_t), pointer :: prev => null()
  end type

  type pulsequeue_t
    integer :: n=0
    type(pulse_t), pointer :: rear => null()
    type(pulse_t), pointer :: front => null()
  end type

  type module_t
    type(string_t) :: label
    integer, allocatable :: inps(:), outs(:)
    integer :: type
    integer, allocatable :: memo(:)
    integer :: state=STATE_OFF
  end type
  interface module_t
    module procedure module_parse
  end interface

contains

  subroutine day2320(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(module_t), allocatable :: mods(:)
    type(pulsequeue_t) :: q
    integer :: i, istart, irx, hi_cnt, lo_cnt, ans1
    integer(I8), allocatable :: pers(:)
    integer(I8) :: ans2
    integer, parameter :: NPRESS=1000

    lines = read_strings(file)
    allocate(mods(size(lines)))
    do i=1, size(lines)
      mods(i) = module_t(lines(i)%str, lines)
    end do
    call module_connect_inputs(mods)
    istart = find_module(lines, 'broadcaster')
    irx = find_module(lines, 'kl') ! sends to "rx"
    allocate(pers(size(mods(irx)%inps)), source=0_I8)

    hi_cnt = 0
    lo_cnt = 0
    do i=1,NPRESS*10
      call pulsequeue_add(q, pulse_t(orig=istart, dest=istart, magn=LO_PULSE))
      do while(q%n/=0)
        call process_pulse(mods, q, lo_cnt, hi_cnt, irx, pers, i)
      end do
      if (i==NPRESS) ans1 = lo_cnt*hi_cnt
      if (all(pers/=0) .and. i>NPRESS) exit
    end do
    print '("Answer 20/1 ",i0,l2)', ans1, ans1==980457412

    ans2 = 1_I8
    do i=1, size(pers)
      ans2 = lcm(ans2, pers(i))
    end do
    print '("Answer 20/1 ",i0,l2)', ans2, ans2==232774988886497_I8

  end subroutine day2320


  subroutine process_pulse(mods, q, lo_cnt, hi_cnt, irx, irx_pers, ipres)
    type(module_t), intent(inout) :: mods(:)
    type(pulsequeue_t), intent(inout) :: q
    integer, intent(inout) :: lo_cnt, hi_cnt
    integer, intent(in) :: irx, ipres
    integer(I8), intent(inout) :: irx_pers(:)

    type(pulse_t) :: pulse
    integer :: i, magn, origpos

    pulse = pulsequeue_remove(q)

    ! Search for the period for each of inputs to "kl" module for Part 2
    if (pulse%dest==irx .and. any(mods(irx)%memo/=0)) then
      do i=1, size(irx_pers)
        if (irx_pers(i)==0 .and. mods(irx)%memo(i)/=0) then
          irx_pers(i) = ipres
        end if
      end do
    end if

    ! Count pulses for Part 1
    if (pulse%magn==LO_PULSE) then
      lo_cnt = lo_cnt + 1
    else
      hi_cnt = hi_cnt + 1
    end if

    ! Send the pulse to the destination module
    if (pulse%dest /= 0) then
      select case(mods(pulse%dest)%type)
      case(TYPE_FLIP)
        ! ignore hi-pulse
        if (pulse%magn==LO_PULSE) then
          ! flip state on low-pulse, send hi/lo if it is now on/off
          if (mods(pulse%dest)%state==STATE_OFF) then
            mods(pulse%dest)%state=STATE_ON
            magn = HI_PULSE
          else
            mods(pulse%dest)%state=STATE_OFF
            magn = LO_PULSE
          end if

          do i=1, size(mods(pulse%dest)%outs)
            call pulsequeue_add(q, pulse_t(orig=pulse%dest, dest=mods(pulse%dest)%outs(i), magn=magn))
          end do
        end if

      case(TYPE_CONJ)
        origpos = findloc(mods(pulse%dest)%inps, pulse%orig, dim=1)
        if (origpos==0) error stop 'pulse arrived from unknonw location'
        mods(pulse%dest)%memo(origpos) = pulse%magn
        if (all(mods(pulse%dest)%memo==HI_PULSE)) then
          magn = LO_PULSE
        else
          magn = HI_PULSE
        end if
        do i=1, size(mods(pulse%dest)%outs)
          call pulsequeue_add(q, pulse_t(orig=pulse%dest, dest=mods(pulse%dest)%outs(i), magn=magn))
        end do
! Conjunction (&)
! - state for every input (lo-default/hi)
! - update this input's memory
! - then send lo-pulse if all are hi
! - otherwise send high-pulse

      case(TYPE_OTHER)
        ! Just propagate to outputs
        do i=1, size(mods(pulse%dest)%outs)
          call pulsequeue_add(q, pulse_t(orig=pulse%dest, dest=mods(pulse%dest)%outs(i), magn=pulse%magn))
        end do

      case default
        error stop 'unknown type'
      end select
    end if
  end subroutine process_pulse


  subroutine module_connect_inputs(mods)
    type(module_t), intent(inout) :: mods(:)

    integer :: i, j, k

    do i=1, size(mods)
      do j=1, size(mods(i)%outs)
        k = mods(i)%outs(j)
        if (k==0) then
          !print *, 'module not found ', j, mods(i)%label%str
          cycle
        end if
        call module_addinp(mods(k), i)
      end do
    end do
  end subroutine module_connect_inputs


  subroutine module_addinp(this, label)
    class(module_t), intent(inout) :: this
    integer, intent(in) :: label

    integer :: i

    if (.not. allocated(this%inps)) allocate(this%inps(0))
    do i=1, size(this%inps)
      if (this%inps(i)==label) then
        error stop 'double input'
      end if
    end do
    this%inps = [this%inps, label]
    if (allocated(this%memo)) deallocate(this%memo)
    allocate(this%memo(size(this%inps)), source=LO_PULSE)
  end subroutine module_addinp


  integer function find_module(lines, key) result(ind)
    type(string_t), intent(in) :: lines(:)
    character(len=*), intent(in) :: key

    integer :: i, j
    type(string_t) :: current

    do i=1, size(lines)
      j = scan(lines(i)%str,' ')
      select case(scan(CH_TYPE, lines(i)%str(1:1)))
      case(TYPE_FLIP, TYPE_CONJ)
        current = string_t(lines(i)%str(2:j-1))
      case(TYPE_OTHER)
        current = string_t(lines(i)%str(1:j-1))
      end select

      if (current%str==key) exit
    end do
    ind = i
    if (ind==size(lines)+1) ind = 0
  end function find_module


  type(module_t) function module_parse(str, lines) result(new)
    character(len=*), intent(in) :: str
    type(string_t), allocatable, intent(in) :: lines(:)

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
          new%outs(i) = find_module(lines, labout(:len(labout)-1))
        else
          new%outs(i) = find_module(lines, labout)
        end if
      end associate
    end do
    new%state = STATE_OFF
  end function module_parse


  subroutine pulsequeue_add(this, pulse)
    class(pulsequeue_t), intent(inout) :: this
    type(pulse_t), intent(in) :: pulse

    type(pulse_t), pointer :: new

    allocate(new)
    new = pulse
    if (associated(this%rear)) this%rear%prev => new
    this%rear => new
    if (.not. associated(this%front)) this%front => new
    this%n = this%n + 1
  end subroutine pulsequeue_add


  function pulsequeue_remove(this) result(pulse)
    class(pulsequeue_t), intent(inout) :: this
    type(pulse_t) :: pulse

    type(pulse_t), pointer :: rem

    if (this%n==0) error stop 'pulsequeue_remove - queue is empty'
    rem => this%front
    this%front => this%front%prev
    if (.not. associated(this%front)) this%rear => null()
    this%n = this%n - 1

    pulse = rem
    deallocate(rem)
  end function pulsequeue_remove


  integer(I8) function lcm(a,b)
    integer(I8), intent(in) :: a, b

    integer(I8) :: greater, smallest, i

    greater = max(a,b)
    smallest = min(a,b)
    i = greater
    do
      if (mod(i,smallest)==0) then
        lcm = i
        exit
      end if
      i = i + greater
    end do
  end function

end module day2320_mod