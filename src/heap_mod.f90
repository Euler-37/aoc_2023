module heap_mod
   implicit none
   private
   public::heap
   abstract interface
      logical function compare(a,b)
         class(*),intent(in)::a,b
      end function compare

      subroutine  swap(a,b)
         class(*),intent(inout)::a,b
      end subroutine swap

      subroutine  equal(a,b)
         class(*),intent(inout)::a
         class(*),intent(in)::b
      end subroutine equal
   end interface

   type heap
      class(*),allocatable::val(:)
      integer::size
      procedure(compare),nopass,pointer::cmp
      procedure(swap)     ,nopass,pointer::sw
      procedure(equal)  ,nopass,pointer::eq
   contains
      procedure,pass::adjustup
      procedure,pass::adjustdown
      procedure,pass::insert=>heap_insert
      procedure,pass::pop=>heap_pop
      procedure,pass::init=>heap_init
      procedure,pass::clean=>heap_clean
      final::final_heap
   end type heap

contains
   subroutine heap_init(this,n,source,eq,cmp,sw)
      class(heap),intent(inout)::this
      integer,intent(in)::n
      class(*),intent(in)::source
      procedure(equal)::eq
      procedure(compare)::cmp
      procedure(swap)::sw
      allocate(this%val(0:n),source=source)
      this%size=0
      this%eq=>eq
      this%cmp=>cmp
      this%sw=>sw
   end subroutine heap_init

   subroutine adjustup(this,size,idx)
      class(heap),intent(inout)::this
      integer,intent(in)::size,idx
      integer::child,parent
      if(idx>size)then
         return
      end if
      child=idx
      parent=(child-1)/2
      do while(child>0)
         if(this%cmp(this%val(child),this%val(parent)))then
            call this%sw(this%val(child),this%val(parent))
         end if
         child=parent
         parent=(child-1)/2
      end do
   end subroutine adjustup

   subroutine heap_insert(this,val)
      class(heap),intent(inout)::this
      class(*),intent(in)::val
      call this%eq(this%val(this%size),val)
      this%size=this%size+1
      call this%adjustup(this%size, this%size-1)
   end subroutine heap_insert

   subroutine root(this,val)
      class(heap),intent(inout)::this
      class(*),intent(inout)::val
      call this%eq(val,this%val(0))
   end subroutine root

   subroutine adjustdown(this,size,idx)
      class(heap),intent(inout)::this
      integer,intent(in)::size,idx
      integer::child,parent
      parent=idx
      child=parent*2+1
      do while(child<size)
         if(child+1<size)then
            if(this%cmp(this%val(child+1),this%val(child)))then
               child=child+1
            end if
         end if
         if(this%cmp(this%val(child),this%val(parent)))then
            call this%sw(this%val(child),this%val(parent))
            parent=child
            child=parent*2+1
         else
            exit
         endif
      end do
   end subroutine adjustdown

   subroutine heap_pop(this,val)
      class(heap),intent(inout)::this
      class(*),intent(inout)::val
      if(this%size==0)return
      call this%eq(val,this%val(0))
      call this%sw(this%val(0), this%val(this%size-1))
      this%size=this%size-1
      call this%adjustdown(this%size-1,0)
   end subroutine heap_pop

   subroutine final_heap(this)
      type(heap),intent(inout)::this
      if(allocated(this%val))deallocate(this%val)
      this%size=0
   end subroutine final_heap

   subroutine heap_clean(this)
      class(heap),intent(inout)::this
      if(allocated(this%val))deallocate(this%val)
      this%size=0
   end subroutine heap_clean

end module heap_mod
