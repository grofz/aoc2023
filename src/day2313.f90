module day2313_mod
  use parse_mod, only : string_t, read_strings
  implicit none

contains

  subroutine day2313(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    character(len=1), allocatable :: pattern(:,:), smudge(:,:)
    integer :: i, jpart1, ans1, k, jpart2, ans2

    lines = read_strings(file)
    i = 1
    ans1 = 0
    ans2 = 0
    do 
      call get_next_frame(lines, i, pattern)
      jpart1=find_symmetry(pattern,-1)
      ans1 = ans1 + jpart1

      jpart2 = 0
      do k=1,size(pattern)
        smudge = make_smudge(pattern, k)
        jpart2 = find_symmetry(smudge,jpart1)
        if (jpart2/=0) exit 
      end do
      if (jpart2==0) error stop 'smudge for Part 2 not found'
      ans2 = ans2 + jpart2

      if (i>size(lines)) exit
    end do
    print '("Answer 13/1 ",i0,l2)', ans1, ans1==37975
    print '("Answer 13/1 ",i0,l2)', ans2, ans2==32497

  end subroutine day2313


  function make_smudge(pattern,n) result(smudge)
    character(len=1), intent(in) :: pattern(:,:)
    integer, intent(in) :: n
    character(len=1) :: smudge(size(pattern,1),size(pattern,2))

    integer :: row, col

    row = (n-1)/size(pattern,2) + 1
    col = n - size(pattern,2)*(row-1) 

    smudge = pattern
    if (smudge(row,col)=='#') then
      smudge(row,col)='.'
    else 
      smudge(row,col)='#'
    end if
  end function make_smudge


  function find_symmetry(pattern, ignore) result(res)
    character(len=1), intent(in) :: pattern(:,:)
    integer, intent(in) :: ignore
    integer :: res

    integer :: i

    res = 0

    do i=1, size(pattern,2)-1
      if (i==ignore) cycle
      if (test_vertical(pattern,i)) then
        if (res/=0) then
          res = 0
          return
        else
          res = i
        end if
      end if
    end do

    do i=1, size(pattern,1)-1
      if (i*100==ignore) cycle
      if (test_horizontal(pattern,i)) then
        if (res/=0) then
          res = 0
          return
        else 
          res = i*100
        end if
      end if
    end do
  end function find_symmetry


  function test_vertical(pattern, col) result(is_symmetry)
    character(len=1), intent(in) :: pattern(:,:)
    integer, intent(in) :: col
    logical :: is_symmetry

    integer :: i

    is_symmetry = .true.
    do i=1, min(col, size(pattern,2)-col)
      if (all(pattern(:,col-i+1)==pattern(:,col+i))) cycle
      is_symmetry = .false.
    end do
  end function test_vertical


  function test_horizontal(pattern, row) result(is_symmetry)
    character(len=1), intent(in) :: pattern(:,:)
    integer, intent(in) :: row
    logical :: is_symmetry

    integer :: i

    is_symmetry = .true.
    do i=1, min(row, size(pattern,1)-row)
      if (all(pattern(row-i+1,:)==pattern(row+i,:))) cycle
      is_symmetry = .false.
    end do
  end function test_horizontal


  subroutine get_next_frame(lines, rstart, pattern)
    type(string_t), intent(in) :: lines(:)
    integer, intent(inout) :: rstart
    character(len=1), allocatable, intent(out) :: pattern(:,:)

    integer :: row, col, rend

    do row=rstart, size(lines)
      if (lines(row)%str=='') exit
    end do
    rend = row-1

    allocate(pattern(rend-rstart+1, len(lines(rstart)%str)))
    do row=rstart, rend
      if (len(lines(row)%str)/=size(pattern,2)) error stop 'not all lines of the same length'
      do col=1, size(pattern,2)
        pattern(row-rstart+1, col) = lines(row)%str(col:col)
      end do
    end do

    rstart = min(rend+2, size(lines)+1)
  end subroutine get_next_frame

end module day2313_mod