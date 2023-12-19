module heap_mod
   implicit none
   type heap
      class(*),allocatable::val(:)
      integer::size
   contains
      procedure,pass::adjustup
      procedure,pass::adjustdown
      procedure,pass::insert
      procedure,pass::pop
      procedure,pass::init
      procedure,pass::clean
      final::final_heap
   end type heap
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
contains
   subroutine init(this,n,source)
      class(heap),intent(inout)::this
      integer,intent(in)::n
      class(*),intent(in)::source
      allocate(this%val(0:n),source=source)
      this%size=0
   end subroutine init

   subroutine adjustup(this,cmp,sw,size,idx)
      class(heap),intent(inout)::this
      integer,intent(in)::size,idx
      procedure(compare)::cmp
      procedure(swap)::sw
      integer::child,parent
      if(idx>size)then
         return
      end if
      child=idx
      parent=(child-1)/2
      do while(child>0)
         if(cmp(this%val(child),this%val(parent)))then
            call sw(this%val(child),this%val(parent))
         end if
         child=parent
         parent=(child-1)/2
      end do
   end subroutine adjustup

   subroutine insert(this,val,eq,cmp,sw)
      class(heap),intent(inout)::this
      class(*),intent(in)::val
      procedure(equal)::eq
      procedure(compare)::cmp
      procedure(swap)::sw
      call eq(this%val(this%size),val)
      this%size=this%size+1
      call this%adjustup(cmp, sw, this%size, this%size-1)
   end subroutine insert

   subroutine root(this,val,eq)
      class(heap),intent(inout)::this
      class(*),intent(inout)::val
      procedure(equal)::eq
      call eq(val,this%val(0))
   end subroutine root

   subroutine adjustdown(this,cmp,sw,size,idx)
      class(heap),intent(inout)::this
      integer,intent(in)::size,idx
      procedure(compare)::cmp
      procedure(swap)::sw
      integer::child,parent
      parent=idx
      child=parent*2+1
      do while(child<size)
         if(child+1<size)then
            if(cmp(this%val(child+1),this%val(child)))then
               child=child+1
            end if
         end if
         if(cmp(this%val(child),this%val(parent)))then
            call sw(this%val(child),this%val(parent))
            parent=child
            child=parent*2+1
         else
            exit
         endif
      end do
   end subroutine adjustdown

   subroutine pop(this,cmp,sw,eq,val)
      class(heap),intent(inout)::this
      class(*),intent(inout)::val
      procedure(compare)::cmp
      procedure(swap)::sw
      procedure(equal)::eq
      if(this%size==0)return
      call eq(val,this%val(0))
      call sw(this%val(0), this%val(this%size-1))
      this%size=this%size-1
      call this%adjustdown(cmp,sw,this%size-1,0)
   end subroutine pop

   subroutine final_heap(this)
      type(heap),intent(inout)::this
      if(allocated(this%val))deallocate(this%val)
      this%size=0
   end subroutine final_heap

   subroutine clean(this)
      class(heap),intent(inout)::this
      if(allocated(this%val))deallocate(this%val)
      this%size=0
   end subroutine clean

end module heap_mod
