module hash_mod
   implicit none
   private
   public::hashmap,hashiter
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
      class(*),allocatable::val
      type(pair),pointer::next=>null()
   contains
      final::final_pair
   end type pair

   type pair_list
      type(pair),pointer::head=>null()
   end type pair_list

   integer,parameter::mdim=997
   type hashmap
      integer::num
      type(pair_list)::a(mdim)
      procedure(equal),nopass,pointer::eq
      procedure(bitarray),nopass,pointer::bit
   contains
      generic::operator(.in.) => in
      procedure,pass::append => hash_append
      procedure,pass::init => hash_init
      procedure,pass::get    => hash_get
      procedure,pass::view    => hash_view
      procedure,pass::remove => hash_remove
      procedure,pass(this)::in => hash_in
      procedure,pass::clean => hash_clean
   end type hashmap

   type hashiter
      integer::size
      type(pair),pointer::pos
      type(hashmap),pointer::h
    contains
          procedure,pass::init => hashiter_init
          procedure,pass::next => hashiter_next

   end type hashiter
contains
   subroutine hashiter_init(this,h)
      class(hashiter),intent(inout)::this
      type(hashmap),intent(in),target::h
      this%size=1
      this%h=>h
      this%pos=>this%h%a(this%size)%head
   end subroutine hashiter_init

   logical function hashiter_next(this,key,val)result(res)
      class(hashiter),intent(inout)::this
      class(*),intent(inout)::key,val
      res=.true.
      do
        
         if(.not.associated(this%pos))then
            this%size=this%size+1
            if(this%size>mdim)then
               res=.false.
               return
            end if
            this%pos=>this%h%a(this%size)%head
         else
            exit
         end if
      end do
      call this%h%eq(key,this%pos%key)
      call this%h%eq(val,this%pos%val)
      this%pos=>this%pos%next
   end function hashiter_next

   subroutine hash_init(this,eq,bit)
      class(hashmap),intent(inout)::this
      procedure(equal)::eq
      procedure(bitarray)::bit
      this%eq=>eq
      this%bit=>bit
   end subroutine hash_init
   

   recursive subroutine final_pair(this)
      type(pair),intent(inout)::this
      if(allocated(this%key))deallocate(this%key)
      if(allocated(this%val))deallocate(this%val)
      if(associated(this%next))then
         call final_pair(this%next)
         deallocate(this%next)
      end if
   end subroutine final_pair

   subroutine hash_clean(this)
      class(hashmap),intent(inout),target::this
      integer::i
      type(pair),pointer::tmp
      do i=1,size(this%a)
         if(associated(this%a(i)%head))deallocate(this%a(i)%head)
      end do
      this%num=0
   end subroutine hash_clean

   subroutine hash_append(this,key,val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),intent(in)::val
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
        if(.not.associated(t%head))then
            allocate(t%head)
            allocate(t%head%key,source=key)
            allocate(t%head%val,source=val)
            this%num=this%num+1
        else
        tmp=>t%head
        do
            call this%bit(tmp%key,pb)
            if(eqv(pb,pk))then
                call this%eq(tmp%val,val)
                exit
            else
                if(.not.associated(tmp%next))then
                    allocate(tmp%next)
                    allocate(tmp%next%key,source=key)
                    allocate(tmp%next%val,source=val)
                    this%num=this%num+1
                    exit
                else
                    tmp=>tmp%next
                end if
            end if
        end do
        end if
      end associate
   end subroutine hash_append

   subroutine  hash_get(this,key,val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),intent(inout)::val
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
         if(.not.associated(t%head))then
         else
            tmp=>t%head
            do
               call this%bit(tmp%key,pb)
               if(eqv(pb,pk))then
                  call this%eq(val,tmp%val)
                  exit
               else
                  if(.not.associated(tmp%next))then
                     exit
                  else
                     tmp=>tmp%next
                  end if
               end if
            end do
         end if
      end associate
   end subroutine hash_get

   function hash_view(this,key)result(val)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      class(*),pointer::val
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
         if(.not.associated(t%head))then
         else
            tmp=>t%head
            do
               call this%bit(tmp%key,pb)
               if(eqv(pb,pk))then
                  !call this%eq(val,tmp%val)
                  val=>tmp%val
                  exit
               else
                  if(.not.associated(tmp%next))then
                     exit
                  else
                     tmp=>tmp%next
                  end if
               end if
            end do
         end if
      end associate
   end function hash_view

   logical function hash_in(key,this) result(val)
      class(hashmap),intent(in),target::this
      class(*),intent(in)::key
      integer::idx
      type(pair),pointer::tmp
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
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
   end function hash_in

   subroutine hash_remove(this,key)
      class(hashmap),intent(inout),target::this
      class(*),intent(in)::key
      integer::idx
      type(pair),pointer::tmp,prev
      integer(1),allocatable::pk(:)
      integer(1),allocatable::pb(:)
      call this%bit(key, pk)
      idx=modulo(hash_code(pk),mdim)+1
      associate(t=>this%a(idx))
         if(.not.associated(t%head))then
            return
         else
            tmp=>t%head
            call this%bit(tmp%key,pb)
            if(eqv(pb,pk))then
               deallocate(tmp%val)
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
                     deallocate(tmp%val)
                     deallocate(tmp%key)
                     prev%next=>tmp%next
                     this%num=this%num-1
                     exit
                  end if
               end if
            end do
         end if
      end associate     
   end subroutine hash_remove

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
end module hash_mod

