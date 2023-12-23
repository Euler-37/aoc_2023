program main
   use aoc_2023
   implicit none
   character(len=100)::s
   !integer,parameter::n=1233
   integer,parameter::n=7
   integer::squ(6,n),i,j
   integer::tmp(6),t
   integer,allocatable::tower(:,:,:)
   integer::mx,my,mz
   type vec
      integer,allocatable::a(:)
   end type vec
   type(vec)::v(n)
   integer,allocatable::a(:)
   !open(10,file="data/22.txt")
   open(10,file="data/test22.txt")
   do i=1,n
      s=""
      read(10,"(A)")s
      call replace(s,"~",",")
      read(s,*)squ(:,i)
   end do
   close(10)
   tmp=maxval(squ,dim=2)
   mx=max(tmp(1),tmp(4))
   my=max(tmp(2),tmp(5))
   mz=max(tmp(3),tmp(6))
   allocate(tower(0:mx,0:my,0:mz),source=0)
   do i=1,n-1
      do j=1,n-i
         if(squ(3,j)>squ(3,j+1))then
            tmp=squ(:,j+1)
            squ(:,j+1)=squ(:,j)
            squ(:,j)=tmp
         end if
      end do
   end do
   ! continue
end program main
