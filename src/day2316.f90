module day2316_mod
  use parse_mod, only : read_pattern
  implicit none
  private
  public day2316

  character(len=1), parameter :: CH_EMPT='.', CH_LMIR='/', CH_RMIR='\', &
      CH_HSPL='|', CH_VSPL='-'

  integer, parameter :: DIRS(2,4) = reshape([-1,0,0,1,1,0,0,-1],[2,4])
    ! UP, RIGHT, DOWN, LEFT

  type beam_t
    integer :: xy(2), d=0
  end type beam_t

  type beamstack_t
    type(beam_t), allocatable :: arr(:)
    integer :: n=0
  contains
    procedure :: push => beamstack_push
    procedure :: pop => beamstack_pop
    procedure :: isempty => beamstack_isempty
  end type

contains

  subroutine day2316(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: map(:,:)
    type(beam_t) :: beam1, beam2
    integer :: i, ans1, ans2

    map = read_pattern(file)

    beam1%xy=[1,1]
    beam1%d = 2
    ans1 = energize_map(beam1, map)
    print '("Answer 16/1 ",i0,l2)', ans1, ans1==7185

    ans2 = 0
    beam1%d = 3
    beam2%d = 1
    do i=1, size(map,2) ! TOP / BOTTOM
      beam1%xy = [1,i]
      beam2%xy = [size(map,1),i]
      ans2 = max(ans2, energize_map(beam1, map), energize_map(beam2, map))
    end do

    beam1%d = 2
    beam2%d = 4
    do i=1, size(map,1) ! LEFT / RIGHT
      beam1%xy = [i,1]
      beam2%xy = [i,size(map,2)]
      ans2 = max(ans2, energize_map(beam1, map), energize_map(beam2, map))
    end do
    print '("Answer 16/2 ",i0,l2)', ans2, ans2==7616

  end subroutine day2316


  pure function energize_map(init_beam, map) result(cnt_energized)
    type(beam_t), intent(in) :: init_beam
    character(len=1), intent(in) :: map(:,:)
    integer :: cnt_energized

    logical :: energized(4, size(map,1), size(map,2))
    type(beamstack_t) :: beams
    type(beam_t) :: newbeams(2), curbeam
    integer :: i, n

    energized = .false.
    call beams%push(init_beam)
    do while (.not. beams%isempty())
      call beams%pop(curbeam)
      if (.not. energized(curbeam%d, curbeam%xy(1), curbeam%xy(2))) then
        energized(curbeam%d, curbeam%xy(1), curbeam%xy(2)) = .true.
        call beam_step(curbeam, map, newbeams, n)
        do i=1,n
          call beams%push(newbeams(i))
        end do
      end if
    end do

    cnt_energized = count(any(energized,dim=1))
  end function energize_map


  pure subroutine beam_step(this, map, new, n)
    class(beam_t), intent(in) :: this
    character(len=1), intent(in) :: map(:,:)
    type(beam_t), intent(out) :: new(2)
    integer, intent(out) :: n

    type(beam_t) :: tmp1, tmp2

    select case(map(this%xy(1), this%xy(2)))
    case(CH_EMPT)
      tmp1%d  = this%d
      tmp1%xy = this%xy + DIRS(:,tmp1%d)
      tmp2%d = 0

    case(CH_LMIR)
      if (mod(this%d,2)==0) then
        tmp1%d = this%d-1
        if (tmp1%d==0) tmp1%d = 4
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      else
        tmp1%d = this%d+1
        if (tmp1%d==5) tmp1%d = 1
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      end if

    case(CH_RMIR)
      if (mod(this%d,2)==0) then
        tmp1%d = this%d+1
        if (tmp1%d==5) tmp1%d = 1
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      else
        tmp1%d = this%d-1
        if (tmp1%d==0) tmp1%d = 4
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      end if

    case(CH_HSPL)
      if (mod(this%d,2)==0) then
        tmp1%d = 1
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 3
        tmp2%xy = this%xy + DIRS(:,tmp2%d)
      else
        tmp1%d  = this%d
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      end if

    case(CH_VSPL)
      if (mod(this%d,2)/=0) then
        tmp1%d = 2
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 4
        tmp2%xy = this%xy + DIRS(:,tmp2%d)
      else
        tmp1%d  = this%d
        tmp1%xy = this%xy + DIRS(:,tmp1%d)
        tmp2%d = 0
      end if

    case default
      error stop 'unknonw character'
    end select

    if (tmp1%d/=0) then
      if (out_of_map(tmp1%xy)) tmp1%d = 0
    end if
    if (tmp2%d/=0) then
      if (out_of_map(tmp2%xy)) tmp2%d = 0
    end if

    if (tmp1%d==0 .and. tmp2%d==0) then
      n = 0
    else if (tmp1%d==0 .and. tmp2%d/=0) then
      n = 1
      new(1) = tmp2
    else if (tmp1%d/=0 .and. tmp2%d==0) then
      n = 1
      new(1) = tmp1
    else
      n = 2
      new(1) = tmp1
      new(2) = tmp2
    end if

  contains

    pure function out_of_map(xy)
      integer, intent(in) :: xy(2)
      logical out_of_map

      if (xy(1)<1 .or. xy(2)<1 .or. xy(1)>size(map,1) .or. xy(2)>size(map,2)) then
        out_of_map = .true.
      else
        out_of_map = .false.
      end if
    end function

  end subroutine beam_step


  subroutine print_arr(arr)
    logical, intent(in) :: arr(:,:)

    integer :: i, j

    do i=1,size(arr,1)
      do j=1,size(arr,2)
        if (arr(i,j)) then
          write(*,'(a2)',advance='no') '#'
        else
          write(*,'(a2)',advance='no') '.'
        end if
      end do
      write(*,*)
    enddo
  end subroutine


  pure subroutine beamstack_push(this, beam)
    class(beamstack_t), intent(inout) :: this
    type(beam_t), intent(in) :: beam

    type(beam_t), allocatable :: wrk(:)

    if (.not. allocated(this%arr)) allocate(this%arr(2))
    if (size(this%arr)==this%n) then
      ! array must be extended
      allocate(wrk(size(this%arr)*2))
      wrk(1:size(this%arr)) = this%arr
      call move_alloc(wrk, this%arr)
    end if

    ! push beam to the stack
    this%n = this%n + 1
    this%arr(this%n) = beam
  end subroutine beamstack_push


  pure subroutine beamstack_pop(this, beam)
    class(beamstack_t), intent(inout) :: this
    type(beam_t), intent(out) :: beam

    if (this%isempty()) error stop 'stack is empty'
    beam = this%arr(this%n)
    this%n = this%n - 1
  end subroutine beamstack_pop


  pure logical function beamstack_isempty(this) result(isempty)
    class(beamstack_t), intent(in) :: this
    isempty = this%n == 0
  end function beamstack_isempty

end module day2316_mod