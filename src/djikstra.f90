! TO-DO backtracking, hash_table must be cpmpulsory
! This 2023 version is using an ad-hoc priority queue with a
! constant hash map 

module djikstra_mod
    implicit none
    private
    public djikstra_search

    integer :: dummy_int

    type, abstract, public :: djikstra_node_at
        logical :: visited = .false.
        integer :: d = huge(dummy_int)
    contains
        procedure(nextngb_ai), deferred :: nextngb
        procedure(isequal_ai), deferred :: isequal
        procedure(istarget_ai), deferred :: istarget
        procedure(calchash_ai), deferred :: calchash
    end type

    abstract interface
        subroutine nextngb_ai(node, flag, node_ngb, distance)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: node
            integer, intent(inout) :: flag !"0" on entry: first ngb, "0" on return: no more ngb
            class(djikstra_node_at), intent(out), allocatable :: node_ngb
            integer, intent(out) :: distance
        end subroutine

        logical function isequal_ai(anode, bnode)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: anode, bnode
        end function

        logical function istarget_ai(node)
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: node
        end function

        integer function calchash_ai(node) 
            import djikstra_node_at
            implicit none
            class(djikstra_node_at), intent(in) :: node
        end function
    end interface

    type, public :: djikstra_node_ptr
        class(djikstra_node_at), allocatable :: ptr
    end type

contains

    subroutine djikstra_search(nodes, startnode, shortest_d, hash_size)
        type(djikstra_node_ptr), allocatable, intent(inout) :: nodes(:)
        class(djikstra_node_at), intent(in) :: startnode
        integer, intent(out) :: shortest_d
        integer, intent(in), optional :: hash_size

        integer :: n,  i, ngb_i, d, flag
        class(djikstra_node_at), allocatable :: curr, ngb
        type(djikstra_node_ptr), allocatable :: nodes1(:)
        integer, allocatable :: hash_table(:)

        ! Set all nodes unvisited and of infinite distance
        shortest_d = huge(shortest_d)
        if (.not. allocated(nodes)) allocate(nodes(0))
        n = size(nodes)
        do i=1, n
            if (.not. allocated(nodes(i)%ptr)) error stop 'djikstra_search - unallocated node in the list'
            nodes(i)%ptr%visited = .false.
            nodes(i)%ptr%d = huge(nodes(i)%ptr%d)
        end do

        ! Initialize the hash table (optional)
        if (present(hash_size)) then
            allocate(hash_table(hash_size), source=0)
            do i=1, n
                hash_table(nodes(i)%ptr%calchash()) = i
            end do
        end if

        ! Heapify
        do i=n/2,  1, -1
            call push_down(nodes, n, hash_table, i)
        end do

        ! Add (if needed) the starting node to the list
        ! Set "d" of starting node to "0"
        call addnode_unique(startnode, nodes, n, hash_table)
        i = findnode(startnode, nodes, n, hash_table)
        if (i==0) error stop 'djikstra_search - error 1'
        if (i==-1) error stop 'djikstra_search - error 1a'
        call update_priority(nodes, n, nodes(i)%ptr, 0, hash_table)
        nodes(i)%ptr%visited = .false.


        ! Main loop
        MAIN: do 
            if (n==0) then
                print *, 'djikstra - all nodes searched, target not found'
                exit MAIN
            end if
            call remove_top(curr, nodes, n, hash_table)

            ! Find and process all neighbors of the current node
            flag = 0
            do
                call curr%nextngb(flag, ngb, d)
                if (flag==0) exit
                call addnode_unique(ngb, nodes, n, hash_table)
                ngb_i = findnode(ngb, nodes, n, hash_table)
                if (ngb_i==0) error stop 'ngbi = 0'
                if (ngb_i==-1) cycle ! node has been alredy visited
                d = d + curr%d
                if (nodes(ngb_i)%ptr%d > d) then
                    call update_priority(nodes, n, nodes(ngb_i)%ptr, d, hash_table)
                end if
            end do

            ! Mark current node as visited (already removed)
           !nodes(curr_i)%ptr%visited = .true.

            ! Exit from loop if target has been reached
            if (curr%istarget()) then
                shortest_d = curr%d
                exit MAIN
            end if
            
        end do MAIN

        ! return "nodes" at a correct size
        allocate(nodes1(n))
        do i=1,n
            allocate(nodes1(i)%ptr, source=nodes(i)%ptr)
        end do
        call move_alloc(nodes1, nodes)

       !if (allocated(hash_table)) then
       !    print *, 'Visited nodes ',count(hash_table==-1)
       !    print *, 'Nodes in queue',count(hash_table/=0 .and. hash_table/=-1)
       !end if
    end subroutine djikstra_search


    integer function findnode(node, nodes, n, hash_table) result(i_found)
        class(djikstra_node_at), intent(in) :: node
        type(djikstra_node_ptr), intent(in) :: nodes(:)
        integer, intent(in) :: n
        integer, intent(in), allocatable :: hash_table(:)

        integer :: i

        if (allocated(hash_table)) then
            i_found = hash_table(node%calchash())
        else
            i_found = 0
            do i = n, 1, -1
                if (nodes(i)%ptr%isequal(node)) then
                    i_found = i
                    exit
                end if
            end do
        end if
    end function findnode


!   integer function findsmallest(nodes, n) result(i_smallest)
!       type(djikstra_node_ptr), intent(in) :: nodes(:)
!       integer, intent(in) :: n

!       integer :: min_d, i
!       min_d = huge(min_d)
!       i_smallest = 0
!       do i = 1, n
!           ! only unvisited nodes are considered
!           if (nodes(i)%ptr%visited) cycle
!           if (nodes(i)%ptr%d < min_d) then
!               i_smallest = i
!               min_d = nodes(i)%ptr%d
!           end if
!       end do
!   end function findsmallest


    subroutine remove_top(top, arr, n, hash_table)
        class(djikstra_node_at), allocatable, intent(out) :: top
        type(djikstra_node_ptr), allocatable, intent(inout) :: arr(:)
        integer, intent(inout) :: n
        integer, allocatable, intent(inout) :: hash_table(:)

        if (n<1) error stop 'remove_top - empty queue'

        allocate(top, source=arr(1)%ptr)
        if (allocated(hash_table)) then
            hash_table(top%calchash()) = -1
        end if
        
        ! replace removed element by tail element
        if (n /= 1) then
            deallocate(arr(1)%ptr)
            allocate(arr(1)%ptr, source = arr(n)%ptr)
            if (allocated(hash_table)) then
                hash_table(arr(1)%ptr%calchash()) = 1
            end if
        end if
        n = n - 1

        ! repair heap
        if (n>1) call push_down(arr, n, hash_table, 1)
    end subroutine remove_top


    subroutine addnode_unique(newnode, nodes, n, hash_table)
        class(djikstra_node_at), intent(in) :: newnode
        type(djikstra_node_ptr), allocatable, intent(inout) :: nodes(:)
        integer, intent(inout) :: n
        integer, intent(inout), allocatable :: hash_table(:)

        type(djikstra_node_ptr), allocatable :: wrk(:)
        integer :: nmax, i

        ! expand if needed
        nmax = size(nodes)
        if (nmax<=n) then
            allocate(wrk(max(2*nmax,100)))
            do i=1, n
                allocate(wrk(i)%ptr, source=nodes(i)%ptr)
            end do
            call move_alloc(wrk, nodes)
        end if

        i = findnode(newnode, nodes, n, hash_table)
        if (i==0) then
            ! node is not in list, add it at the end
            n = n + 1
            if (allocated(nodes(n)%ptr)) deallocate(nodes(n)%ptr)
            allocate(nodes(n)%ptr, source=newnode)
            ! initialize workings of a new node
            nodes(n)%ptr%d = huge(nodes(n)%ptr%d)
            nodes(n)%ptr%visited = .false.

            if (allocated(hash_table)) then
                hash_table(newnode%calchash()) = n
            end if
            call bubble_up(nodes, hash_table, n)
        end if
    end subroutine addnode_unique


    subroutine bubble_up(arr, hash_map, index)
        type(djikstra_node_ptr), intent(inout), allocatable :: arr(:)
        integer, intent(inout), allocatable :: hash_map(:)
        integer, intent(in) :: index

        integer :: icur, ipar
        class(djikstra_node_at), allocatable :: node

        icur = index
        allocate(node, source=arr(icur)%ptr)

        do
            if (icur==1) exit
            ipar = icur/2
            if (arr(ipar)%ptr%d <= node%d) exit
            deallocate(arr(icur)%ptr)
            allocate(arr(icur)%ptr, source=arr(ipar)%ptr)
            if (allocated(hash_map)) then
                hash_map(arr(icur)%ptr%calchash()) = icur
            end if
            icur = ipar
        end do
        if (allocated(arr(icur)%ptr)) deallocate(arr(icur)%ptr)
        allocate(arr(icur)%ptr, source=node)
        if (allocated(hash_map)) then
            hash_map(arr(icur)%ptr%calchash()) = icur
        end if
    end subroutine bubble_up


    subroutine push_down(arr, n, hash_map, index)
        type(djikstra_node_ptr), intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer, intent(inout), allocatable :: hash_map(:)
        integer, intent(in) :: index

        integer :: icur, ilch, irch, ich
        class(djikstra_node_at), allocatable :: node

        icur = index
        allocate(node, source=arr(icur)%ptr)

        do
            if (icur*2 > n) exit

            ! select child with higher priority
            if (icur*2+1 > n) then
                ich = icur*2
            else
                ilch = icur*2
                irch = icur*2+1
                if (arr(ilch)%ptr%d < arr(irch)%ptr%d) then
                    ich = ilch
                else
                    ich = irch
                end if
            end if

            if (arr(ich)%ptr%d >= node%d) exit
            deallocate(arr(icur)%ptr)
            allocate(arr(icur)%ptr, source=arr(ich)%ptr)
            if (allocated(hash_map)) then
                hash_map(arr(icur)%ptr%calchash()) = icur
            end if
            icur = ich
        end do

        deallocate(arr(icur)%ptr)
        allocate(arr(icur)%ptr, source=node)
        if (allocated(hash_map)) then
            hash_map(arr(icur)%ptr%calchash()) = icur
        end if
    end subroutine push_down


    subroutine update_priority(arr, n, node, new_priority, hash_table)
        type(djikstra_node_ptr), intent(inout), allocatable :: arr(:)
        integer, intent(in) :: n
        class(djikstra_node_at), intent(inout) :: node
        integer, intent(in) :: new_priority
        integer, allocatable, intent(inout) :: hash_table(:)

        integer :: i, old_priority

        i = findnode(node, arr, n, hash_table)
        if (i < 1) error stop 'update_priority - not in heap'
        if (i > n) error stop 'update element - index out bounds'

        old_priority = node%d
        arr(i)%ptr%d = new_priority
        if (new_priority < old_priority) then
            call bubble_up(arr, hash_table, i)
        else if (new_priority > old_priority) then
            call push_down(arr, n, hash_table, i)
        end if
    end subroutine update_priority

end module djikstra_mod