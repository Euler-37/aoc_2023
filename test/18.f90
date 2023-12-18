program main
   implicit none
   integer::ios
   character(len=1)::s
   integer::i,j,l,loop
   integer(8)::num
   integer(8)::k
   integer,parameter::n=400
   character(len=1),target::map(-n:n,-n:n)
   integer(8)::pos(2)
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer,parameter::up=1,down=2,left=3,right=4
   character(len=1),pointer::p(:,:)
   character(len=9)::hex
   integer::m,lb(2),ub(2)
   integer::before,flag=999
   type pair
      integer(8)::r(2)
   end type pair
   type(pair)::path(700)
   integer::ps
   open(10,file="data/18.txt")
   pos=[0,0]
   map="."
   map(pos(1),pos(2))="#"
   do
      read(10,*,iostat=ios)s,k
      if(is_iostat_end(ios))exit
      select case(s)
      case("U");m=up
      case("D");m=down
      case("L");m=left
      case("R");m=right
      end select 
      if(flag==999)then
         flag=m
      end if
      if(before==left.and.m==up) map(pos(1),pos(2))="L"
      if(before==left.and.m==down) map(pos(1),pos(2))="F"
      if(before==right.and.m==up) map(pos(1),pos(2))="J"
      if(before==right.and.m==down) map(pos(1),pos(2))="7"
      if(before==up.and.m==left) map(pos(1),pos(2))="7"
      if(before==up.and.m==right) map(pos(1),pos(2))="F"
      if(before==down.and.m==left) map(pos(1),pos(2))="J"
      if(before==down.and.m==right) map(pos(1),pos(2))="L"
      do i=1,k
         pos=pos+mv(:,m)
         map(pos(1),pos(2))="#"
      end do
      before=m
   end do
   m=flag
   if(before==left.and.m==up) map(0,0)="L"
   if(before==left.and.m==down) map(0,0)="F"
   if(before==right.and.m==up) map(0,0)="J"
   if(before==right.and.m==down) map(0,0)="7"
   if(before==up.and.m==left) map(0,0)="7"
   if(before==up.and.m==right) map(0,0)="F"
   if(before==down.and.m==left) map(0,0)="J"
   if(before==down.and.m==right) map(0,0)="L"
   close(10)
   do i=-n,n
      if(any(map(i,:)/="."))then
         lb(1)=i
         exit
      end if
   end do
   do i=n,-n,-1
      if(any(map(i,:)/="."))then
         lb(2)=i
         exit
      end if
   end do
   do i=-n,n
      if(any(map(:,i)/="."))then
         ub(1)=i
         exit
      end if
   end do
   do i=n,-n,-1
      if(any(map(:,i)/="."))then
         ub(2)=i
         exit
      end if
   end do
   p=>map(lb(1):lb(2),ub(1):ub(2))
   num=0
   do i=1,size(p,1)
      do j=1,size(p,2)
         if(p(i,j)==".")then
            loop=0
            do l=1,min(size(p,1)-i,size(p,2)-j)
               ! find corner
               if(any(p(i+l,j+l)==["#","J","F"]))then
                  loop=loop+1
               end if
            end do
            if(mod(loop,2)==1)num=num+1
         end if
      end do
   end do
   write(*,*)num+count(p/=".")

   !https://handwiki.org/wiki/Shoelace_formula
   open(10,file="data/18.txt")
   ps=0
   pos=[0,0]
   ps=ps+1
   path(ps)=pair(pos)
   num=0
   do
      read(10,*,iostat=ios)s,k,hex
      if(is_iostat_end(ios))exit
      read(hex(8:8),"(i1)")m
      select case(m)
      case(0);m=right
      case(1);m=down
      case(2);m=left
      case(3);m=up
      end select
      read(hex(3:7),"(z5)")k
      !bond
      num=num+abs(k*mv(1,m))
      num=num+abs(k*mv(2,m))
      pos=pos+mv(:,m)*k
      !write(*,*)pos
      ps=ps+1
      path(ps)=pair(pos)
   end do
   do i=1,ps-1
      associate(r1=>path(i)%r,r2=>path(i+1)%r)
         num=num+(r1(2)*r2(1)-r2(2)*r1(1))
      end associate
   end do
   write(*,*)num/2+1
end program main
