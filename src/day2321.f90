module day2321_mod
  use parse_mod, only : read_pattern, string_t, read_strings
  use iso_fortran_env, only : I1=>int32, I8=>int64
  implicit none

  character(len=1), parameter :: CH_ROCK='#', CH_PATH='.', CH_START='S'

contains

  subroutine day2321(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: garden(:,:)
    integer :: pos(2), i, ans1, nxorig, nyorig
    integer, parameter :: NSTEPS=450, NCOPIES=7, NDERS=3, NPART1=64
    logical, allocatable :: able(:,:,:)
    integer :: sums(-NDERS:NSTEPS,NDERS)

    garden = read_pattern(file)
    nxorig = size(garden,1)
    nyorig = size(garden,2)
    pos = findloc(garden,CH_START)
    garden(pos(1),pos(2)) = CH_PATH
    call make_copies(garden, NCOPIES)
    allocate(able(0:1, size(garden,1), size(garden,2)), source=.false.)

    able(0, (NCOPIES/2+1-1)*nxorig+pos(1), &
            (NCOPIES/2+1-1)*nyorig+pos(2)) = .true.
    sums= 0
    do i=1, NSTEPS
      call fill_able(able(0:1,:,:), garden)
      if (at_edge(able(1,:,:))) error stop 'edge reached'
      sums(i,1) = count(able(1,:,:))
print *, i, sums(i,1), sums(i,1)-sums(i-1,1), &
  (sums(i,1)-sums(i-1,1))-(sums(i-1,1)-sums(i-2,1)), &
  (sums(i,1)-sums(i-1,1))-(sums(i-1,1)-sums(i-2,1)) - ((sums(i-1,1)-sums(i-2,1))-(sums(i-2,1)-sums(i-3,1)))
      able(0,:,:) = able(1,:,:)
    end do

    do i=1,NDERS-1
      sums(i+1:,i+1) = diff(sums(i:,i))
    end do
    do i=1, ubound(sums,1)
      print '(*(i6,1x))', i, sums(i,:)
    end do
    ans1 = sums(NPART1,1)
    print '("Anwer 21/1 ",i0,l2)', ans1, ans1==3820
    call part2('inp/21/deriv.out')
  end subroutine day2321


  subroutine part2(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer(I8), allocatable :: y(:,:)
    integer(I8) :: x, res, ans2, yprev(0:3)
    integer :: i
    integer(I8), parameter :: PER=131, NMAX=26501365

    lines = read_strings(file)
    allocate(y(0:3,size(lines))) ! ind2=time, ind1=derivative
    do i=1, size(lines)
      read(lines(i)%str,*) x, y(0,i), y(1,i), y(2,i), y(3,i)
      if (x/=i) error stop 'wrong data'
    enddo

    ! Back-fill from 3rd derivation
    yprev = y(:,4)
    do i=5,NMAX
      res = integrate(int(i,I8),PER, y(3,2+2*PER:1+3*PER)-y(3,2+PER:1+2*PER), 2+PER, y(3,2+PER:1+2*PER))
      yprev(3) = res
      yprev(2) = yprev(2) + yprev(3)
      yprev(1) = yprev(1) + yprev(2)
      yprev(0) = yprev(0) + yprev(1)
    end do
    ans2 = yprev(0)
    print '("Answer 21/2 ",i0,l2)', ans2, ans2==632421652138917_I8
  end subroutine part2


  integer(I8) function integrate(x, per, d, x0, y0) result(y)
    integer(I8), intent(in) :: x, per, d(:), x0, y0(:)

    integer(I8) :: np, xd, i, np0

    xd = mod(x-1-1, per)+1
    np = (x-2)/per
    np0 = (x0-2)/per
    y = y0(xd) + d(xd)*(np-np0)
  end function


  subroutine fill_able(slice, garden)
    logical, intent(inout) :: slice(:,:,:)
    character(len=*), intent(in) :: garden(:,:)

    integer :: x, y, nx, ny, i, j

    ny = size(garden,1)
    nx = size(garden,2)
    slice(2,:,:) = .false.
    do y=1, ny
      do x=1, nx
        if (.not. slice(1,y,x)) cycle
        ! [y,x] is a possible position in the previous step
        do i=-1,1
          do j=-1,1
            if (abs(i)+abs(j)/=1) cycle
            if (y+i<1 .or. y+i>ny .or. x+j<1 .or. x+j>nx) cycle
            if (garden(y+i,x+j)/=CH_PATH) cycle
            ! [y+i,x+j] is a path next to [y,x]
            slice(2,y+i,x+j) = .true.
          end do
        end do
      end do
    end do
  end subroutine fill_able


  subroutine make_copies(arr, n)
    character(len=*), intent(inout), allocatable :: arr(:,:)
    integer, intent(in) :: n

    integer :: i, j
    character(len=1), allocatable :: wrk(:,:)

    allocate(wrk(size(arr,1)*n, size(arr,2)*n))
    do i=1, n
      do j=1, n
        wrk((i-1)*size(arr,1)+1 : i*size(arr,1), &
            (j-1)*size(arr,2)+1 : j*size(arr,2)) = arr
      end do
    end do
    call move_alloc(wrk, arr)
  end subroutine make_copies


  logical function at_edge(arr)
    logical, intent(in) :: arr(:,:)

    at_edge = count(arr(1,:))>0 .or. count(arr(size(arr,1),:))>0 .or. &
              count(arr(:,1))>0 .or. count(arr(:,size(arr,2)))>0
  end function at_edge


    function diff(u) result (du)
    integer, intent(in) :: u(:)
    integer :: du(size(u)-1)

    integer :: i

    do i=1, size(u)-1
      du(i) = u(i+1)-u(i)
    end do
  end function diff


  recursive function backfill(u, is_prev) result(next)
    integer, intent(in) :: u(:)
    logical, intent(in) :: is_prev
    integer :: next

    integer, allocatable :: du(:)

    du = diff(u)
    if (count(du==0)==size(du)) then
      next = u(1)
    else
      if (is_prev) then
        next = u(1) - backfill(du, is_prev)
      else
        next = u(size(u)) + backfill(du, is_prev)
      end if
    end if
  end function backfill


end module day2321_mod