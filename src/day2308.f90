module day2308_mod
  use parse_mod, only : read_strings, string_t, split
  use iso_fortran_env, only : int64
  implicit none

  integer, parameter :: NLEN=3
  character(len=1), parameter :: DIRS(2)=['L','R']

  type vertex_t
    character(len=NLEN) :: label
    character(len=NLEN) :: next(2)
  end type
  interface vertex_t
    module procedure vertex_new
  end interface vertex_t

contains

  subroutine day2308(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(string_t) :: inst
    type(vertex_t), allocatable :: vs(:)
    integer :: i
    integer(int64) :: ans1, ans2

    lines = read_strings(file)
    allocate(vs(size(lines)-2)) ! 1line=instruction,2line=empty
    if (len_trim(lines(2)%str)/=0) &
        error stop 'Day8 - expecting empty second line'
    inst = string_t(lines(1)%str)
    do i=1,size(vs)
      vs(i) = vertex_t(lines(i+2)%str)
    end do
    call v_sort(vs)

    call walk_part1(vs, inst%str, ans1)
    print '("Answer 08/1 ",i0,l2)', ans1, ans1==19099
    call walk_part2(vs, inst%str, ans2)
    print '("Answer 08/2 ",i0,l2)', ans2, ans2==17099847107071_int64
  end subroutine day2308


  function next(i, vs, instruction) result(inext)
    integer, intent(in) :: i
    type(vertex_t), intent(in) :: vs(:)
    character(len=1), intent(in) :: instruction
    integer :: inext

    associate(nextdir=>findloc(DIRS, instruction, dim=1))
      if (nextdir==0) error stop 'invalid dir in instruction string'
      inext = find_label_bs(vs, vs(i)%next(nextdir))
      if (inext==0) error stop 'label '//vs(i)%next(nextdir)//' not found'
    end associate
  end function


  subroutine walk_part1(vs, str, ans1)
    type(vertex_t), intent(in) :: vs(:)
    character(len=*), intent(in) :: str
    integer(int64), intent(out) :: ans1

    integer :: ipos, j

    ans1 = 0
    j = 1
    ipos = find_label_bs(vs, 'AAA')
    if (ipos==0) error stop 'label AAA not found'
    do
      if (j>len(str)) j=1 ! reset finger
      ipos = next(ipos, vs, str(j:j))
      ans1 = ans1 + 1
      if (vs(ipos)%label=='ZZZ') exit
      j = j + 1
    end do
  end subroutine walk_part1


  subroutine walk_part2(vs, str, ans)
    type(vertex_t), intent(in) :: vs(:)
    character(len=*), intent(in) :: str
    integer(int64), intent(out) :: ans

    integer, allocatable :: ipos(:)
    integer(int64) :: cnts
    integer :: j, i

    allocate(ipos(0))
    do i=1,size(vs)
      if (vs(i)%label(NLEN:NLEN)=='A') ipos = [ipos, i]
    end do

    do i=1,size(ipos)
      j = 1
      cnts = 0
      do
        if (j>len(str)) j=1 ! reset finger
        ipos(i) = next(ipos(i), vs, str(j:j))
        cnts = cnts + 1
        if (vs(ipos(i))%label(NLEN:NLEN)=='Z') exit
        j = j + 1
      end do
!     print '(i0,"/",i0,1x,i0)', i, size(ipos), cnts
      if (i==1) then
        ans = cnts
      else
        ans = lcm(ans, cnts)
      end if
      print '(i18)', ans
    end do
  end subroutine walk_part2


  type(vertex_t) function vertex_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: peq, plp, pcm, prp

    peq = scan(str,'=')
    plp = scan(str,'(')
    pcm = scan(str,',')
    prp = scan(str,')')
    if (peq==0 .or. plp==0 .or. pcm==0 .or. prp==0) &
      error stop 'invalid line format'

    new%label = str(1:peq-2)
    new%next(1) = str(plp+1:pcm-1)
    new%next(2) = str(pcm+2:prp-1)
  end function vertex_new


  integer function find_label_bs(arr,label) result(pos)
    type(vertex_t), intent(in) :: arr(:)
    character(len=NLEN), intent(in) :: label

    integer :: l, r, m

    pos = 0
    l = 0
    r = size(arr)-1
    do while (l<=r)
      m = l + (r-l)/2
      if (arr(m+1)%label==label) then
        pos = m+1
        exit
      else if (arr(m+1)%label < label) then
        l = m+1
      else
        r = m-1
      end if
    end do
  end function find_label_bs


  subroutine v_sort(arr)
    type(vertex_t), intent(inout) :: arr(:)

    integer :: i, j
    type(vertex_t) :: tmp

    do i = 2, size(arr)
      tmp = arr(i)
      do j = i-1, 1, -1
        if (arr(j)%label > tmp%label) then
          arr(j+1) = arr(j)
        else
          exit
        end if
      end do
      arr(j+1) = tmp
    end do
  end subroutine v_sort


  integer(int64) function lcm(a,b)
    integer(int64), intent(in) :: a, b

    integer(int64) :: greater, smallest, i

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

end module day2308_mod
