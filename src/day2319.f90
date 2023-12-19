module day2319_mod
  use parse_mod, only : string_t, read_strings, split
  use iso_fortran_env, only : I8 => int64
  implicit none

  character(len=4), parameter :: CATEGORIES = "xmas"
  integer, parameter :: INVALID_VALUE = -1
  integer, parameter :: MIN_PARTRANGE=1, MAX_PARTRANGE=4000
  integer, parameter :: OP_ALL=0, OP_GT=1, OP_LT=2

  type part_t
    integer :: vals(len(CATEGORIES)) = INVALID_VALUE
  end type
  interface part_t
    module procedure part_new
  end interface

  type partrange_t
    integer :: lb(len(CATEGORIES)) = MIN_PARTRANGE
    integer :: ub(len(CATEGORIES)) = MAX_PARTRANGE
    type(string_t) :: label
  contains
    procedure :: n => partrange_count
  end type 

  type rule_t
    integer :: cat, op, val
    type(string_t) :: dest
  end type
  interface rule_t
    module procedure rule_new
  end interface

  type workflow_t
    type(string_t) :: label
    type(rule_t), allocatable :: rules(:)
  end type
  interface workflow_t
    module procedure workflow_new
  end interface

contains
  subroutine day2319(file)
    character(len=*), intent(in) :: file

    type(string_t), allocatable :: lines(:)
    integer :: iempty, i, ans1
    integer(I8) :: cnt
    type(part_t), allocatable :: parts(:)
    type(workflow_t), allocatable :: wflows(:)
    type(string_t) :: dest
    type(partrange_t) :: pr
    type(partrange_t), allocatable :: prs(:), oldprs(:), newprs(:)

    ! Parse inputs. Get parts and workflows
    lines = read_strings(file)
    do i=1, size(lines)
      if (lines(i)%str=='') exit
    end do
    iempty = i

    allocate(wflows(iempty-1))
    do i=1, iempty-1
      wflows(i) = workflow_t(lines(i)%str)
    end do

    ans1 = 0
    allocate(parts(size(lines)-iempty))
    do i=1, size(parts)
      parts(i) = part_t(lines(iempty+i)%str)
      dest = route_part(parts(i), wflows)
      if (dest%str=='R') cycle
      ! part accepted
      ans1 = ans1 + sum(parts(i)%vals)
    end do
    print '("Answer 19/1 ",i0,l2)', ans1, ans1==319295

    ! Part 2
    allocate(oldprs(1))
    oldprs(1) = partrange_t()
    oldprs(1)%label = string_t('in')

    do
      cnt = 0
      allocate(newprs(0))

      do i=1, size(oldprs)
        if (oldprs(i)%label%str=='A') then
          newprs = [newprs, oldprs(i)]
        else if (oldprs(i)%label%str=='R') then
          newprs = [newprs, oldprs(i)]
        else
          cnt = cnt + oldprs(i)%n()
          call process_wflow2(oldprs(i), wflows, prs)
          newprs = [newprs, prs]
        end if
      end do

     !do i=1, size(newprs)
       !print *, newprs(i)%n(), newprs(i)%label%str
     !end do
     !print *
      call move_alloc(newprs, oldprs)
      if (cnt==0) exit
    end do

    cnt = 0
    do i=1, size(oldprs)
      if (oldprs(i)%label%str=='A') cnt = cnt + oldprs(i)%n()
    end do
    print '("Answer 19/2 ",i0,l2)', cnt, cnt==110807725108076_I8

  end subroutine day2319


  type(string_t) function route_part(part, wflows) result(dest)
    type(part_t), intent(in) :: part
    type(workflow_t), intent(in) :: wflows(:)

    integer :: iwflow, i

    do i=1, size(wflows)
      if (wflows(i)%label%str=='in') exit
    end do
    if (i==size(wflows)+1) error stop 'in workflow not found'
    iwflow = i

    do
      dest = next_wflow(part, wflows(iwflow))
      if (dest%str=='R' .or. dest%str=='A') exit

      do i=1, size(wflows)
        if (wflows(i)%label%str==dest%str) exit
      end do
      if (i==size(wflows)+1) error stop 'destination label not found'
      iwflow = i
    end do
  end function route_part


  subroutine process_wflow2(prin, wflows, prnews)
    type(partrange_t), intent(in) :: prin
    type(workflow_t), intent(in) :: wflows(:)
    type(partrange_t), allocatable, intent(out) :: prnews(:)

    integer :: i, k, iw
    type(partrange_t) :: pr, prnext
    type(partrange_t), allocatable :: wprs(:)

    ! Find a correct workflow
    do i=1, size(wflows)
      if (wflows(i)%label%str==prin%label%str) exit
    end do
    if (i==size(wflows)+1) error stop 'destination label not found'
    iw = i
    allocate(wprs(size(wflows(iw)%rules)))

    pr = prin
    k = 1
    do i=1, size(wflows(iw)%rules)
      call match_rule2(pr, wflows(iw)%rules(i), wprs(k), prnext)
      pr = prnext
      if (wprs(k)%n()>0) then
        wprs(k)%label = string_t(wflows(iw)%rules(i)%dest%str)
        k = k+1
      end if
    end do
    if (pr%n()>0) error stop 'something left'

    allocate(prnews(k-1))
    do i=1,k-1
      prnews(i) = wprs(i)
    end do
  end subroutine process_wflow2


  type(string_t) function next_wflow(part, wflow) result(dest)
    type(part_t), intent(in) :: part
    type(workflow_t), intent(in) :: wflow

    integer :: i

    do i=1, size(wflow%rules)
      if (match_rule(part, wflow%rules(i))) exit
    end do
    if (i==size(wflow%rules)+1) error stop 'no rule match'
    dest = string_t(wflow%rules(i)%dest%str)
  end function next_wflow


  subroutine match_rule2(pr, rule, prtrue, prfalse)
    type(partrange_t), intent(in) :: pr
    type(rule_t), intent(in) :: rule
    type(partrange_t), intent(out) :: prtrue, prfalse

    associate(i=>rule%cat)
      prtrue = pr
      prfalse = pr
      if (rule%op==OP_ALL) then
        prfalse%lb = 1
        prfalse%ub = 0
      else if (rule%op==OP_GT) then
        prtrue%lb(i)  = max(pr%lb(i), rule%val+1)
        prfalse%ub(i) = min(pr%ub(i), rule%val)
      else if (rule%op==OP_LT) then
        prtrue%ub(i)  = min(pr%ub(i), rule%val-1)
        prfalse%lb(i) = max(pr%lb(i), rule%val)
      else
        error stop 'match_rule - undefined operation'
      end if
    end associate

    if (pr%n()/=prtrue%n()+prfalse%n()) then
      error stop 'match_rule2 - addition check failed'
    end if
  end subroutine match_rule2


  logical function match_rule(part, rule) result(is_match)
    type(part_t), intent(in) :: part
    type(rule_t), intent(in) :: rule

    if (rule%op==OP_ALL) then
      is_match = .true.
    else if (rule%op==OP_GT) then
      is_match = part%vals(rule%cat) > rule%val
    else if (rule%op==OP_LT) then
      is_match = part%vals(rule%cat) < rule%val
    else
      error stop 'match_rule - undefined operation'
    end if
  end function match_rule


  type(workflow_t) function workflow_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i1, i2, i
    type(string_t), allocatable :: rules_list(:)

    i1 = scan(str, '{')
    i2 = scan(str, '}')
    if (i1==0 .or. i2==0) error stop 'workflow_new - parse error'
    call split(str(i1+1:i2-1),',',rules_list)

    new%label = string_t(str(:i1-1))
    allocate(new%rules(size(rules_list)))
    do i=1, size(rules_list)
      new%rules(i) = rule_t(rules_list(i)%str)
    end do
!print *, new%label%str," no of rules ",size(new%rules)
  end function workflow_new


  type(rule_t) function rule_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i1, i2

    i1 = scan(str,'<>')
    i2 = scan(str,':')
    if (i1==0 .and. i2==0) then
      ! the last rule
      new%cat = 0
      new%op = OP_ALL
      new%val = INVALID_VALUE
      new%dest = string_t(str)
    else if (i1==0 .or. i2==0) then
      error stop 'rule_new - parse error'
    else
      new%cat = scan(CATEGORIES, str(:i1-1))
      if (new%cat==0) error stop 'rule_new - category not recognized'
      select case(str(i1:i1))
      case('<')
        new%op = OP_LT
      case('>')
        new%op = OP_GT
      case default
        error stop 'rule_new - operation not recognized'
      end select
      read(str(i1+1:i2-1),*) new%val
      new%dest = string_t(str(i2+1:))
    end if

!print '("Rule: cat=",i0," op=",i0," val=",i0," ->",a)', new%cat, new%op, new%val, new%dest%str
  end function rule_new


  type(part_t) function part_new(str) result(new)
    character(len=*), intent(in) :: str

    integer :: i1, i2, i, j1, ind
    type(string_t), allocatable :: components(:)

    i1 = scan(str,'{')
    i2 = scan(str,'}')
    if (i1==0 .or. i2==0) error stop 'part_new - missing { }'
    call split(str(i1+1:i2-1),',',components)

    do i=1, size(components)
      j1 = scan(components(i)%str, '=')
      if (j1==0) error stop 'part_new - wrong component'
      ind = scan(CATEGORIES, components(i)%str(:j1-1))
      if (ind==0) error stop 'part_new - unknonwn component'
      read(components(i)%str(j1+1:),*) new%vals(ind)
    end do

!print *, 'part_new: ',new%vals
  end function part_new


  pure function partrange_count(this) result(n)
    class(partrange_t), intent(in) :: this
    integer(I8) :: n

    integer :: i

    n = 1_I8
    do i=1, size(this%lb)
      associate(lb=>this%lb(i), ub=>this%ub(i))
        n = n * int(max(0,ub-lb+1), kind=I8)
      end associate
    end do
  end function partrange_count

end module day2319_mod