module set_mod
   implicit none
   private
   public::set,setiter

   abstract interface
      subroutine  equal(a,b)
         class(*),intent(inout)::a
         class(*),intent(in)::b
      end subroutine equal
      subroutine  bitarray(a,b)
         class(*),intent(in)::a
         integer(1),intent(inout),allocatable::b(:)
      end subroutine bitarray
   end interface

   type pair
      class(*),allocatable::key
      type(pair),pointer::next=>null()
   contains
      final::final_pair
   end type pair

   type pair_list
      type(pair),pointer::head=>null()
   end type pair_list

   integer,parameter::mdim=1000
   type set
      integer::num
      type(pair_list)::a(mdim)
      procedure(equal),nopass,pointer::eq
      procedure(bitarray),nopass,pointer::bit
   contains
      generic::operator(.in.) => in
      procedure,pass::append => set_append
      procedure,pass::init => set_init
      procedure,pass::remove => set_remove
      procedure,pass(this)::in => set_in
      procedure,pass::clean => set_clean
   end type set

   type setiter
      integer::size
      type(pair),pointer::pos
      type(set),pointer::s
    contains
          procedure,pass::init => setiter_init
          procedure,pass::next => setiter_next
   end type setiter
contains
   subroutine setiter_init(this,s)
      class(setiter),intent(inout)::this
      type(set),intent(in),target::s
      this%size=1
      this%s=>s
      this%pos=>this%s%a(this%size)%head
   end subroutine setiter_init

   logical function setiter_next(this,key)result(res)
      class(setiter),intent(inout)::this
      class(*),intent(inout)::key
      res=.true.
      do
        
         if(.not.associated(this%pos))then
            this%size=this%size+1
            if(this%size>mdim)then
               res=.false.
               return
            end if
            this%pos=>this%s%a(this%size)%head
         else
            exit
         end if
      end do
      call this%s%eq(key,this%pos%key)
      this%pos=>this%pos%next
   end function setiter_next

   subroutine set_init(this,eq,bit)
      class(set),intent(inout)::this
      procedure(equal)::eq
      procedure(bitarray)::bit
      this%eq=>eq
      this%bit=>bit
      this%num=0
   end subroutine set_init
   

   recursive subroutine final_pair(this)
      type(pair),intent(inout)::this
      if(allocated(this%key))deallocate(this%key)
      if(associated(this%next))then
         call final_pair(this%next)
         deallocate(this%next)
      end if
   end subroutine final_pair

   subroutine set_clean(this)
      class(set),intent(inout),target::this
      integer::i
      do i=1,size(this%a)
         if(associated(this%a(i)%head))deallocate(this%a(i)%head)
      end do
      this%num=0
   end subroutine set_clean

   subroutine set_append(this,key)
      class(set),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=mod(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
        if(.not.associated(t%head))then
            allocate(t%head)
            allocate(t%head%key,source=key)
            this%num=this%num+1
        else
        tmp=>t%head
        do
            call this%bit(tmp%key,pb)
            if(eqv(pb,pk))then
                exit
            else
                if(.not.associated(tmp%next))then
                    allocate(tmp%next)
                    allocate(tmp%next%key,source=key)
                    this%num=this%num+1
                    exit
                else
                    tmp=>tmp%next
                end if
            end if
        end do
        end if
      end associate
   end subroutine set_append

   logical function set_in(key,this) result(val)
      class(set),intent(in),target::this
      class(*),intent(in)::key
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=mod(hash_code(pk),mdim)+1
      val=.false.
      associate(t=>this%a(idx))
         if(.not.associated(t%head))then
            val=.false.
         else
            tmp=>t%head
            do
               call this%bit(tmp%key,pb)
               if(eqv(pb,pk))then
                  val=.true.
                  exit
               else
                  if(.not.associated(tmp%next))then
                     val=.false.
                     exit
                  else
                     tmp=>tmp%next
                  end if
               end if
            end do
         end if
      end associate
   end function set_in

   subroutine set_remove(this,key)
      class(set),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(pair),pointer::tmp,prev
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=mod(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
         if(.not.associated(t%head))then
            return
         else
            tmp=>t%head
            call this%bit(tmp%key,pb)
            if(eqv(pb,pk))then
               deallocate(tmp%key)
               this%num=this%num-1
               t%head=>t%head%next
               return
            end if
            do
               if(.not.associated(tmp%next))then
                  exit
               else
                  prev=>tmp
                  tmp=>tmp%next
                  call this%bit(tmp%key,pb)
                  if(eqv(pb,pk))then
                     deallocate(tmp%key)
                     this%num=this%num-1
                     prev%next=>tmp%next
                     exit
                  end if
               end if
            end do
         end if
      end associate     
   end subroutine set_remove

   logical function eqv(pa,pb)result(res)
      integer(1),intent(in)::pa(:)
      integer(1),intent(in)::pb(:)
      if(size(pa)/=size(pb))then
         res=.false.
      else
         res=all(pa==pb)
      end if
   end function eqv

   integer(8) function hash_code(p)result(h)
      integer(1)::p(:)
      integer::i
      integer(8)::g
      integer(8),parameter::PP=int(z"F0000000",8)
      h=1
      do i=1,size(p)
         h=shiftl(h,4)+p(i)
         g=iand(h,pp)
         if(g/=0)h=ieor(h,shiftl(g,24))
         h=iand(h,not(g))
      end do
   end function hash_code
end module set_mod
