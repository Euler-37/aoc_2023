program main
   use aoc_2023
   implicit none
   !integer,parameter::n=5
   integer,parameter::n=140
   character(len=1)::path(n,n)
   integer::flag(n,n)
   integer::i,num,j,l,loop
   integer::idx(2),ids(2),its(4)
   character(len=1),parameter::map(3,4)=reshape(&
      &["|","J","7",& !-
      & "|","F","L",& !- 
      & "-","J","L",& !|
      & "-","F","7" & !|
      &],shape=[3,4])
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   open(10,file="data/10.txt")
   do i=1,n
      read(10,"("//tostring(n)//"A1)")path(:,i)
   end do
   idx=findloc(path,"S")
   ids=idx
   flag=0
   its=0
   if(idx(1)-1/=0)   its(1)=1
   if(idx(1)+1/=n+1) its(2)=2
   if(idx(2)-1/=0)   its(3)=3
   if(idx(2)+1/=n+1) its(4)=4
   do i=1,4
      if(its(i)/=0)then
         flag=0
         flag(ids(1),ids(2))=1
         idx=ids+mv(:,its(i))
         if(flag(idx(1),idx(2))==0.and.&
            &all(path(idx(1),idx(2))/=map(:,its(i))) )then
            call dfs(idx)
            write(*,*)sum(flag)/2
            exit
         end if
      end if
   end do
   num=0
   path(ids(1),ids(2))="|" ! just change 
   do i=1,n
      do j=1,n
         if(flag(i,j)==0)then
            loop=0
            do l=1,min(i,j)-1
               ! find corner
               if(flag(i-l,j-l)/=0.and.any(path(i-l,j-l)==["|","-","F","J"]))then
                  loop=loop+1
               end if
            end do
            if(mod(loop,2)==1)num=num+1
         end if
      end do
   end do
   write(*,*)num
contains
   recursive subroutine dfs(idx)
      integer,intent(inout)::idx(2)
      integer::num,i
      integer::it(2),tp
      it=0
      associate(ix=>(idx(1)),iy=>(idx(2)))
         flag(ix,iy)=1
         select case(path(ix,iy))
         case("|")
            if(iy-1/=0)   it(1)=3
            if(iy+1/=n+1) it(2)=4
         case("-")
            if(ix-1/=0)   it(1)=1
            if(ix+1/=n+1) it(2)=2
         case("7")
            if(ix-1/=0)   it(1)=1
            if(iy+1/=n+1) it(2)=4
         case("L")
            if(ix+1/=n+1) it(1)=2
            if(iy-1/=0)   it(2)=3
         case("F")
            if(ix+1/=n+1) it(1)=2
            if(iy+1/=n+1) it(2)=4
         case("J")
            if(ix-1/=0)   it(1)=1
            if(iy-1/=0)   it(2)=3
         case("S")
         end select
         do i=1,2
            if(it(i)/=0)then
               idx=[ix,iy]+mv(:,it(i))
               if(flag(idx(1),idx(2))==0.and.all(path(idx(1),idx(2))/=map(:,it(i))))then
                  call dfs(idx)
               end if
            idx=[ix,iy]
            end if
         end do
      end associate
   end subroutine dfs
end program main
