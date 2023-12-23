program main
   use aoc_2023
   implicit none
   integer,parameter::n=23
   !integer,parameter::n=141
   character(len=1)::s(n,n)
   integer::flag(n,n)
   integer::i
   integer::p,l
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer,parameter::up=1,down=2,left=3,right=4
   integer::start(2),end(2)
   !open(10,file="data/23.txt")
   open(10,file="data/test23.txt")
   do i=1,n
      read(10,"("//tostring(n)//"A1)")s(i,:)
   end do
   ! 1
   flag=merge(2,0,s=="#")
   p=0
   start=[1,2]
   end=[n,n-1]
   l=0
   flag(start(1),start(2))=1
   call dfs(start,l,.true.)
   write(*,*)p
   ! 2
   flag=merge(2,0,s=="#")
   p=0
   start=[1,2]
   end=[n,n-1]
   l=0
   flag(start(1),start(2))=1
   call dfs(start,l,.false.)
   write(*,*)p
contains
   recursive subroutine dfs(pos,l,is)
      integer,intent(inout)::pos(2),l
      integer::i
      logical::is
      !do i=1,n
         !write(*,"(23i1)")flag(i,:)
      !end do
      !write(*,*)"--------------------"
      !read(*,*)
      if(all(pos==end))then
         p=max(p,l)
         return
      end if
      if(is)then
         select case(s(pos(1),pos(2)))
         case(".");i=0
         case("^");i=up
         case("v");i=down
         case("<");i=left
         case(">");i=right
         end select
      else
         i=0
      end if
      if(i==0)then
         do i=1,4
            pos=pos+mv(:,i)
            if(check(pos))then
               if(flag(pos(1),pos(2))==0)then
                  l=l+1
                  flag(pos(1),pos(2))=1
                  call dfs(pos,l,is)
                  flag(pos(1),pos(2))=0
                  l=l-1
               end if
            end if
            pos=pos-mv(:,i)
         end do
      else
         pos=pos+mv(:,i)
         if(check(pos))then
            if(flag(pos(1),pos(2))==0)then
               l=l+1
               flag(pos(1),pos(2))=1
               call dfs(pos,l,is)
               flag(pos(1),pos(2))=0
               l=l-1
            end if
         end if
         pos=pos-mv(:,i)
      end if
   end subroutine dfs

   logical function check(pos)result(res)
      integer,intent(in)::pos(2)
      res=all(pos>0).and.all(pos<n+1)
   end function check
end program main
