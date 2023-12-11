program main
   implicit none
   integer,parameter::n=140
   !integer,parameter::n=10
   character(len=1)::s(n,n)
   integer::i,j
   type pair
      integer::x,y
   end type pair
   type(pair),allocatable::plant(:)
   integer,allocatable::col(:),row(:)
   integer(8)::dis,num
   open(10,file="data/11.txt")
   do i=1,n
      read(10,"(140A1)")s(:,i)
      !read(10,"(10A1)")s(:,i)
   end do
   allocate(plant(0))
   allocate(col(0))
   allocate(row(0))
   do i=1,n
      if(all(s(:,i)=="."))col=[col,i]
      if(all(s(i,:)=="."))row=[row,i]
   end do
   do i=1,n
      do j=1,n
         if(s(j,i)=="#")then
            plant=[plant,pair(j,i)]
         end if
      end do
   end do
   num=0
   do i=1,size(plant)
      do j=i+1,size(plant)
         associate(ix=>plant(i)%x,iy=>plant(i)%y,&
               &jx=>plant(j)%x,jy=>plant(j)%y)
            dis=abs(jx-ix)+abs(jy-iy)&
               &+count(min(ix,jx)<row.and.row<max(ix,jx))&
               &+count(min(iy,jy)<col.and.col<max(iy,jy))
            num=num+dis
         end associate
      end do
   end do
   write(*,*)num
   num=0
   do i=1,size(plant)
      do j=i+1,size(plant)
         associate(ix=>plant(i)%x,iy=>plant(i)%y,&
               &jx=>plant(j)%x,jy=>plant(j)%y)
            dis=abs(jx-ix)+abs(jy-iy)&
               &+count(min(ix,jx)<row.and.row<max(ix,jx))*(1000000-1)&
               &+count(min(iy,jy)<col.and.col<max(iy,jy))*(1000000-1)
            num=num+dis
         end associate
      end do
   end do
   write(*,*)num
end program main
