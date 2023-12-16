program main
   use aoc_2023
   implicit none
   integer,parameter::n=110
   !integer,parameter::n=10
   character(len=1)::s(n,n)
   integer::i,j
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer::flag(n,n),direc(4,n,n)
   type path
      integer::pos(2)
      integer::id
   end type path
   type(path)::vec(1000)
   integer::ps
   integer::num
   open(10,file="data/16.txt")
   do i=1,n
      read(10,"("//tostring(n)//"A1)")s(i,:)
   end do
   write(*,*)check([1,1],4)
   num=0
   do i=1,n
      num=max(num,check([n,i],1))
      num=max(num,check([1,i],2))
      num=max(num,check([i,n],3))
      num=max(num,check([i,1],4))
   end do
   write(*,*)num
contains
   integer function check(p,i)result(res)
      integer,intent(in)::i,p(2)
      integer::id,pos(2)
      flag=0
      vec(1)=path(p,i)
      ps=1
      direc=0
      do
         pos =vec(ps)%pos
         id=vec(ps)%id
         ps=ps-1
         call dfs(s, id, pos)
         if(ps==0)exit
      end do
      res=sum(flag)
   end function check

   recursive subroutine dfs(s,id,pos)
      character(len=1),intent(inout)::s(:,:)
      integer,intent(inout)::id,pos(2)
      integer::pos0(2)
      if(any(pos<=0).or.any(pos>=n+1))return
      if(flag(pos(1),pos(2))==1.and.direc(id,pos(1),pos(2))==1)return
      flag(pos(1),pos(2))=1
      direc(id,pos(1),pos(2))=1
      select case(s(pos(1),pos(2)))
      case(".")
         do
            pos=pos+mv(:,id)
            if(all(pos>0).and.all(pos<n+1))then
               if(s(pos(1),pos(2))==".")then
                  flag(pos(1),pos(2))=1
               else
                  call dfs(s,id,pos)
                  exit
               end if
            else
               exit
            end if
         end do
      case("|")
         if(id<3)then
            pos=pos+mv(:,id)
            call dfs(s,id,pos)
         else
            ps=ps+1; vec(ps)=path(pos,2)
            pos0=pos
            id=1; pos=pos0+mv(:,id); call dfs(s,id,pos)
            id=2; pos=pos0+mv(:,id); call dfs(s,id,pos)
         end if
      case("-")
         if(id>2)then
            pos=pos+mv(:,id)
            call dfs(s,id,pos)
         else
            ps=ps+1; vec(ps)=path(pos,4)
            pos0=pos
            id=3; pos=pos0+mv(:,id); call dfs(s,id,pos)
            id=4; pos=pos0+mv(:,id); call dfs(s,id,pos)
         end if
      case("\")
         !mv=mv([2,1])
         ! 1 3
         ! 2 4
         ! 4 2
         ! 3 1
         select case(id)
         case(1);id=3
         case(2);id=4
         case(3);id=1
         case(4);id=2
         end select
         pos=pos+mv(:,id)
         call dfs(s,id,pos)
      case("/")
         !mv=mv([2,1])*(-1)
         ! 1 4
         ! 2 3
         ! 3 2
         ! 4 1
         select case(id)
         case(1);id=4
         case(2);id=3
         case(3);id=2
         case(4);id=1
         end select
         pos=pos+mv(:,id)
         call dfs(s,id,pos)
      end select 
   end subroutine dfs

end program main
