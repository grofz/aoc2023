module day2321_mod
  use parse_mod, only : read_pattern, string_t, read_strings
  use iso_fortran_env, only : I1=>int32, I8=>int64
  implicit none

  character(len=1), parameter :: CH_ROCK='#', CH_PATH='.', CH_START='S'

contains

  subroutine day2321(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable :: garden(:,:)
    integer :: pos(2), i, ans1, nx, ny, fid
    integer, parameter :: NSTEPS=450, NCOPIES=7, NDERS=2, NPART1=64
    logical, allocatable :: able(:,:,:)
    integer :: sums(-NDERS:NSTEPS,0:NDERS)

    garden = read_pattern(file)
    nx = size(garden,1)
    ny = size(garden,2)
    if (nx/=ny) error stop 'Part 2 requires square map'
    if (3*nx+1 > NSTEPS) error stop 'Part 2 - increase NSTEPS'
    pos = findloc(garden,CH_START)
    garden(pos(1),pos(2)) = CH_PATH
    call make_copies(garden, NCOPIES)
    allocate(able(0:1, size(garden,1), size(garden,2)), source=.false.)

    able(0, (NCOPIES/2+1-1)*nx+pos(1), &
            (NCOPIES/2+1-1)*ny+pos(2)) = .true.
    sums = 0
    open(newunit=fid, status='replace', file='./day2321.tmp')
    do i=1, NSTEPS
      call fill_able(able(0:1,:,:), garden)
      if (at_edge(able(1,:,:))) error stop 'edge reached'
      sums(i,0) = count(able(1,:,:))
      sums(i,1) = sums(i,0) - sums(i-1,0)
      sums(i,2) = sums(i,1) - sums(i-1,1)
      write(fid,*) i, sums(i,:)
      able(0,:,:) = able(1,:,:)
    end do
    close(fid)
    ans1 = sums(NPART1,0)
    print '("Anwer 21/1 ",i0,l2)', ans1, ans1==3820

    call part2('./day2321.tmp', int(nx,kind=I8))
    open(newunit=fid, file='./day2321.tmp', status='old')
    close(fid, status='delete')
  end subroutine day2321


  subroutine part2(file, per)
    character(len=*), intent(in) :: file
    integer(I8), intent(in) :: per

    type(string_t), allocatable :: lines(:)
    integer(I8), allocatable :: y(:,:)
    integer(I8) :: x, ans2, yprev(0:2)
    integer :: i
    integer(I8), parameter :: NMAX=26501365

    lines = read_strings(file)
    allocate(y(0:2,size(lines)))
    do i=1, size(lines)
      ! x, y, dydx, d2y/dx2
      read(lines(i)%str,*) x, y(0,i), y(1,i), y(2,i)
      if (x/=i) error stop 'wrong data'
    enddo

    ! Back-fill from 2rd derivation
    yprev = y(:,4)
    do i=5,NMAX
      associate(y2=>y(2,2+2*per:1+3*per), y1=>y(2,2+per:1+2*per))
        yprev(2) = integrate(int(i,I8),PER, y2-y1, 2+per, y1)
      end associate
      yprev(1) = yprev(1) + yprev(2)
      yprev(0) = yprev(0) + yprev(1)
    end do
    ans2 = yprev(0)
    print '("Answer 21/2 ",i0,l2)', ans2, ans2==632421652138917_I8
  end subroutine part2


  integer(I8) function integrate(x, per, d, x0, y0) result(y)
    integer(I8), intent(in) :: x, per, d(:), x0, y0(:)

    integer(I8) :: np, xd, np0

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

end module day2321_mod