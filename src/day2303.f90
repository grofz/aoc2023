module day2303_mod
  use parse_mod, only : read_pattern
  implicit none
  private
  public day2303

  type :: label_t
    integer :: value = -1
    integer :: x0, x1, y             ! co-ordinates of the box
    integer :: xg = 0, yg = 0        ! co-ordinates of GEAR or zeros
    logical :: is_part_no = .false.  ! adjacent not-null character
  end type

  interface operator(>)
    module procedure label_gt ! label_t > label_t
  end interface

  character(len=1), parameter :: CH_GEAR = '*', CH_NULL = '.'

contains

  subroutine day2303(file)
    character(len=*), intent(in) :: file

    character(len=1), allocatable, dimension(:,:) :: plan
    type(label_t), allocatable :: labels(:)
    integer :: i, ans1, ans2
    
    plan = read_pattern(file)
    labels = extract_labels(plan)
    call sort_labels(labels)

    ans1 = 0
    ans2 = 0
    do i=1, size(labels)
      ! Part 1
      if (labels(i)%is_part_no) ans1 = ans1 + labels(i)%value

      ! Part 2
      if (i < size(labels) .and. labels(i)%xg/=0) then
        if (labels(i)%xg==labels(i+1)%xg .and. labels(i)%yg==labels(i+1)%yg) &
          ans2 = ans2 + labels(i)%value*labels(i+1)%value
      end if
    end do

    print '("Answer 03/1 ",i0,l2)', ans1, ans1==498559
    print '("Answer 03/2 ",i0,l2)', ans2, ans2==72246648
  end subroutine day2303


  subroutine add_partnumber(label, plan)
    type(label_t), intent(inout) :: label
    character(len=1), dimension(0:,0:), intent(in) :: plan
    ! Scan around the label and add information about other character

    integer :: x

    label%is_part_no = .false.
    label%xg = 0
    label%yg = 0

    if (plan(label%y,label%x0-1)/=CH_NULL) then
      ! Left from label
      label%is_part_no = .true.
      if (plan(label%y,label%x0-1)==CH_GEAR) then
        label%yg = label%y
        label%xg = label%x0-1
      end if
    else if (plan(label%y,label%x1+1)/=CH_NULL) then
      ! Right from label
      label%is_part_no = .true.
      if (plan(label%y,label%x1+1)==CH_GEAR) then
        label%yg = label%y
        label%xg = label%x1+1
      end if
    else
      do x=label%x0-1, label%x1+1
        if (plan(label%y-1,x)/=CH_NULL) then
          ! Above the label
          label%is_part_no = .true.
          if (plan(label%y-1,x)==CH_GEAR) then
            label%yg = label%y-1
            label%xg = x
          end if
          exit
        else if (plan(label%y+1,x)/=CH_NULL) then
          ! Below the label
          label%is_part_no = .true.
          if (plan(label%y+1,x)==CH_GEAR) then
            label%yg = label%y+1
            label%xg = x
          end if
          exit
        end if
      end do
    end if
  end subroutine add_partnumber


  function extract_labels(plan) result(labels)
    character(len=*), intent(in) :: plan(:,:)
    type(label_t), allocatable :: labels(:)

    integer :: x, y, i
    type(label_t) :: newlabel
    character(len=:), allocatable :: text
    character(len=1), dimension(0:size(plan,1)+1, 0:size(plan,2)+1) :: plan_extended

    allocate(labels(0))
    plan_extended = CH_NULL
    plan_extended(1:size(plan,1),1:size(plan,2)) = plan

    ROWS: do y = 1, size(plan,1)
      x = 0
      COLS: do
        x = x+1
        if (x > size(plan,2)) exit COLS

        if (todigit(plan(y,x))==-1) cycle COLS

        ! new label, identify box
        newlabel%y = y
        newlabel%x0 = x
        SCAN_LABEL: do 
          x = x+1
          if (x > size(plan,2)) exit SCAN_LABEL
          if (todigit(plan(y,x))/=-1) cycle SCAN_LABEL
          exit SCAN_LABEL
        end do SCAN_LABEL
        newlabel%x1 = x-1

        ! convert characters to an integer
        allocate(character(len=newlabel%x1-newlabel%x0+1) :: text)
        do i=newlabel%x0, newlabel%x1
          text(i-newlabel%x0+1:i-newlabel%x0+1) = plan(newlabel%y, i)
        end do
        read(text,*) newlabel%value
        deallocate(text)

        call add_partnumber(newlabel, plan_extended)

        ! append to an array of labels
        labels = [labels, newlabel]
      end do COLS
    end do ROWS
  end function extract_labels


  function todigit(ch) result(d)
    character(len=1), intent(in) :: ch
    integer :: d
    if (iachar(ch(1:1))<iachar('0') .or. iachar(ch(1:1))>iachar('9')) then
      d = -1
    else
      d = ichar(ch(1:1))-iachar('0')
    end if
  end function


  subroutine sort_labels(arr)
    type(label_t), intent(inout) :: arr(:)
    ! Insertion sort
    integer :: i, j
    type(label_t) :: tmp

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
  end subroutine sort_labels


  logical function label_gt(a, b)
    type(label_t), intent(in) :: a, b

    label_gt = .false.
    if (a%yg > b%yg) then
      label_gt = .true.
    else if (a%yg == b%yg) then
      if (a%xg > b%xg) label_gt = .true.
    end if
  end function label_gt

end module day2303_mod