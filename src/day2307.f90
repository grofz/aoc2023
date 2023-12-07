module day2307_mod
  use parse_mod, only : string_t, read_strings
  implicit none
  private
  public day2307

  integer, parameter :: NCARDS=5
  character(len=*), parameter :: &
    CTYPES = 'AKQJT98765432', ORDER2 = 'AKQT98765432J'
  
  character(len=:), pointer :: ORDER

  type card_t
    character(len=NCARDS) :: hand
    integer :: bet
    integer :: rank
  end type
  interface card_t
    module procedure card_new
  end interface

  interface operator(>)
    module procedure card_gt
  end interface

contains
  subroutine day2307(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    type(card_t), allocatable :: cards(:)
    integer :: i, ans1, ans2

    lines = read_strings(file)
    allocate(cards(size(lines)))
    do i=1, size(lines)
      cards(i) = card_t(lines(i)%str)
    end do
    allocate(character(len=len(CTYPES)) :: ORDER)
    ORDER = CTYPES
    call card_sort(cards)

    ans1 = 0
    do i=1, size(cards)
      ans1 = ans1 + cards(i)%bet*i
    end do
    print '("Answer 07/1 ",i0,l2)', ans1, ans1==248836197

    do i=1, size(cards)
      cards(i)%rank = identify_rank(cards(i),.true.)
    end do
    ORDER = ORDER2
    call card_sort(cards)
    ans2 = 0
    do i=1, size(cards)
      ans2 = ans2 + cards(i)%bet*i
    end do
    print '("Answer 07/2 ",i0,l2)', ans2, ans2==251195607

  end subroutine day2307


  type(card_t) function card_new(str) result(new)
    character(len=*), intent(in) :: str

    read(str,*) new%hand, new%bet
    new%rank = identify_rank(new,.false.)
  end function card_new


  function counts(this)
    type(card_t), intent(in) :: this
    integer :: counts(len(CTYPES))
    integer :: i, j

    counts = 0
    do i=1, len(CTYPES)
      do j=1, NCARDS
        if (this%hand(j:j)==CTYPES(i:i)) counts(i)=counts(i)+1
      end do
    end do
  end function counts


  function identify_rank(this, is_part_two) result(rank)
    type(card_t), intent(in) :: this
    logical, intent(in) :: is_part_two
    integer :: rank

    type(card_t) :: new
    integer :: posk, posj, j, k
    integer :: cnt(len(CTYPES))

    new = this
    posj = scan(CTYPES,'J')
    cnt = counts(new)
    if (is_part_two) then
      do
        ! As long as hand contains a JOKER, change it to the card of the
        ! most frequent type of card in hand
        if (cnt(scan(CTYPES,'J'))==0) exit
        if (cnt(scan(CTYPES,'J'))==NCARDS) exit
        cnt(posj) = -1 ! jokers should be ignored in maxloc search
        posk = maxloc(cnt, dim=1)
        j = scan(new%hand,CTYPES(posj:posj))
        k = scan(new%hand,CTYPES(posk:posk))
        new%hand(j:j) = new%hand(k:k)
        cnt = counts(new)
      end do
    end if

    if (maxval(cnt)==5) then
      rank = 7
    else if (maxval(cnt)==4) then
      rank = 6
    else if (count(cnt==3)==1 .and. count(cnt==2)==1) then
      rank = 5
    else if (count(cnt==3)==1) then
      rank = 4
    else if (count(cnt==2)==2) then
      rank = 3
    else if (count(cnt==2)==1) then
      rank = 2
    else if (count(cnt==1)==5) then
      rank = 1
    else
      print *, new%hand, cnt
      error stop 'identify rank error'
    end if
  end function identify_rank


  logical function card_gt(a, b) result(gt)
    type(card_t), intent(in) :: a, b

    integer :: i, fa, fb

    gt = .false.
    if (a%rank > b%rank) then
      gt = .true.
    else if (a%rank < b%rank) then
      gt = .false.
    else
      do i=1, NCARDS
        fa = scan(ORDER, a%hand(i:i))
        fb = scan(ORDER, b%hand(i:i))
        if (fa==0 .or. fb==0) error stop 'invalid char in hand'
        if (fa<fb) then
          gt = .true.
        else if (fa>fb) then
          gt = .false.
        else
          cycle
        end if
        exit
      end do
    end if
  end function card_gt


  subroutine card_sort(arr)
    type(card_t), intent(inout) :: arr(:)

    integer :: i, j
    type(card_t) :: tmp

    do i = 2, size(arr)
      tmp = arr(i)
      do j = i-1, 1, -1
        if (arr(j) > tmp) then
          arr(j+1) = arr(j)
        else
          exit
        end if
      end do
      arr(j+1) = tmp
    end do
  end subroutine card_sort

end module day2307_mod