module string_mod
   implicit none
   type string
      character(len=:),allocatable::str
   contains
      generic::assignment(=)=>string_construct
      procedure,pass::print=>print_string
      procedure,pass::string_construct
      final::final_string
   end type string

   interface split
      module procedure split_str
      module procedure split_string
   end interface
contains

   subroutine string_construct(this,str)
      class(string),intent(inout)::this
      character(len=*),intent(in)::str
      this%str=str
   end subroutine string_construct

   function split_string(str,sep)result(res)
      type(string),intent(in)::str
      character(len=*),intent(in)::sep
      type(string),allocatable::res(:)
      if (sep==" ")then
         res=split(adjustl(str%str),sep)
      else
         res=split(str%str,sep)
      end if
   end function split_string

   function split_str(str,sep)result(res)
      character(len=*),intent(in)::str
      character(len=*),intent(in)::sep
      type(string),allocatable::res(:)
      type(string)::tmp
      integer::start,end,l,ls
      start=1
      l=len(sep)
      ls=len_trim(str)
      allocate(res(0))
      do
         end=index(str(start:),sep)
         if(end==0)exit
         end=end+start-1
         tmp=str(start:end-1)
         res=[res,tmp]
         start=end+l
      end do
      tmp=str(start:ls)
      res=[res,tmp]
   end function split_str

   impure elemental subroutine print_string(this)
      class(string),intent(in)::this
      write(*,*,delim="quote")this%str
   end subroutine print_string

    subroutine final_string(this)
        type(string),intent(inout)::this
        if(allocated(this%str))deallocate(this%str)
    end subroutine

    function string_view(s,n)result(res)
       character(len=1),intent(in),target::s(*)
       integer,intent(in)::n
       character(len=1),pointer::res(:)
       res=>s(1:n)
    end function string_view
end module string_mod
