module hashbit_mod
   use iso_fortran_env,only:i8=>int8
   implicit none
   private
   public::hashbit,hash_bit
   type :: hashbit
      integer(8)::num
      integer(8)::dimen
      integer(i8),allocatable::i(:)
   contains
      procedure,pass::init=>bit_init
      procedure,pass::set =>bit_set
      procedure,pass::get =>bit_get
      procedure,pass::clear=>bit_clear
      procedure,pass::clean=>bit_clean
   end type hashbit
   integer,parameter::kd=storage_size(1_i8)
contains
   subroutine bit_clean(this)
      class(hashbit),intent(inout)::this
      this%num=0
      this%dimen=0
      deallocate(this%i)
   end subroutine bit_clean

   subroutine bit_init(this,n)
      class(hashbit),intent(inout)::this
      integer(8),intent(in)::n
      this%num=n
      this%dimen=n/kd+merge(0,1,mod(n,kd)==0)
      allocate(this%i(0:this%dimen-1),source=0_i8)
   end subroutine bit_init

   subroutine bit_set(this,i)
      class(hashbit),intent(inout)::this
      integer(8),intent(in)::i
      integer(8)::pos,idx
      pos=i/kd    ! 8 , 0
      idx=modulo(i,kd)! 8 , 8
      this%i(pos)=ibset(this%i(pos),idx)
   end subroutine bit_set

   subroutine bit_clear(this,i)
      class(hashbit),intent(inout)::this
      integer(8),intent(in)::i
      integer(8)::pos,idx
      pos=i/kd    ! 8 , 0
      idx=modulo(i,kd)! 8 , 8
      this%i(pos)=ibclr(this%i(pos),idx)
   end subroutine bit_clear

   logical function bit_get(this,i)result(res)
      class(hashbit),intent(inout)::this
      integer(8),intent(in)::i
      integer(8)::pos,idx
      pos=i/kd        ! 8 , 0
      idx=modulo(i,kd)! 8 , 8
      res=btest(this%i(pos),idx)
   end function bit_get

   integer(8) function hash_bit(a,b)result(res)
      integer(8),intent(in)::a(:)
      integer(8),intent(in)::b(:)
      integer(8)::n,i,idx
      n=size(a)
      idx=1
      res=a(n)
      do i=n,2,-1
         idx=idx*b(i)
         res=res+a(i-1)*idx
      end do
   end function hash_bit
end module hashbit_mod
