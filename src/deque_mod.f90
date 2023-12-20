module deque_mod
   implicit none
   private
   public::deque
   abstract interface
      subroutine  equal(a,b)
         class(*),intent(inout)::a
         class(*),intent(in)::b
      end subroutine equal
   end interface
   integer,parameter::dmax=20
   type node
      class(*),allocatable::a(:)
      type(node),pointer::prev=>null()
      type(node),pointer::next=>null()
   end type node
   type deque
      integer::start,end
      integer::size
      type(node),pointer::head=>null()
      type(node),pointer::tail=>null()
      procedure(equal),pointer,nopass::eq
    contains
        procedure,pass::clean=>deque_clean
        procedure,pass::init=>deque_init
        procedure,pass::front=>deque_front
        procedure,pass::append=>deque_append
        procedure,pass::pop=>deque_pop
        procedure,pass::popfront=>deque_popfront
   end type deque
contains
   subroutine deque_init(this,val,eq)
      class(deque),intent(inout)::this
      class(*),intent(in)::val
      procedure(equal)::eq
      this%eq=>eq
      ! head
      !   |
      ! tail
      allocate(this%head)
      this%tail=>this%head
      allocate(this%head%a(dmax),source=val)
      this%start=1
      this%end=0
      this%size=0
   end subroutine deque_init

   subroutine deque_append(this,val)
      class(deque),intent(inout)::this
      class(*),intent(in)::val
      type(node),pointer::tmp
      if(this%end==dmax)then
        allocate(tmp)
        allocate(tmp%a(dmax),source=val)
        this%end=1
        call this%eq(tmp%a(this%end),val)
        this%tail%next=>tmp
        tmp%prev=>this%tail
        this%tail=>tmp
        nullify(tmp)
      else
         this%end=this%end+1
         call this%eq(this%tail%a(this%end),val)
      end if
      this%size=this%size+1
   end subroutine deque_append

   subroutine deque_front(this,val)
      class(deque),intent(inout)::this
      class(*),intent(in)::val
      type(node),pointer::tmp
      if(this%start==1)then
        allocate(tmp)
        allocate(tmp%a(dmax),source=val)
        this%start=dmax
        call this%eq(tmp%a(this%start),val)
        this%head%prev=>tmp
        tmp%next=>this%head
        this%head=>tmp
        nullify(tmp)
      else
         this%start=this%start-1
         call this%eq(this%head%a(this%start),val)
      end if
      this%size=this%size+1
   end subroutine deque_front

    subroutine deque_pop(this,val)
        class(deque),intent(inout)::this
        class(*),intent(inout)::val
        type(node),pointer::tmp
        if(this%end==0)then
            tmp=>this%tail
            this%tail=>this%tail%prev
            nullify(this%tail%next)
            deallocate(tmp%a)
            nullify(tmp%prev)
            nullify(tmp%next)
            nullify(tmp)
            this%end=dmax
        end if
        call this%eq(val,this%tail%a(this%end))
        this%end=this%end-1
       this%size=this%size-1

    end subroutine deque_pop
    

    subroutine deque_popfront(this,val)
        class(deque),intent(inout)::this
        class(*),intent(inout)::val
        type(node),pointer::tmp
        if(this%start==dmax+1)then
            tmp=>this%head
            this%head=>this%head%next
            nullify(this%head%prev)
            deallocate(tmp%a)
            nullify(tmp%prev)
            nullify(tmp%next)
            nullify(tmp)
            this%start=1
        end if
        call this%eq(val,this%head%a(this%start))
        this%start=this%start+1
        this%size=this%size-1
    end subroutine deque_popfront

    subroutine deque_clean(this)
        class(deque),intent(inout)::this
        if(associated(this%head))then
            call node_clean(this%head)
            nullify(this%head)
        end if
        this%size=0
        this%start=1
        this%end=0
   end subroutine deque_clean

   recursive subroutine node_clean(this)
        type(node),intent(inout)::this
        deallocate(this%a)
        nullify(this%prev)
        if(associated(this%next))then
            call node_clean(this%next)
        end if
   end subroutine node_clean
end module deque_mod
