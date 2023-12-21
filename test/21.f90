program main
   use aoc_2023
   implicit none
   integer,parameter::n=131
   !integer,parameter::n=11
   character(len=1)::a(n,n)
   integer::idx(2),pos(2),pos2(2),i,j,k
   integer::next(2,n*n),current(2,n*n),prev(2,n*n),cu,nex,pre,tcu
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer,parameter::up=1,down=2,left=3,right=4
   integer::length(n,n),s(-1:n*3)
   integer,parameter::step=26501365
   integer(8)::si(3),sa,sb
   open(10,file="data/21.txt")
   do i=1,n
      read(10,"("//tostring(n)//"A1)")a(i,:)
   end do
   close(10)
   idx=findloc(a,"S")
   cu=1
   pre=1
   nex=0
   current(:,cu)=idx
   s(-1:0)=[0,1]
   do i=1,64
      tcu=cu
      ! point 1 must different than 2
      ! may same as 3
      do
         loopj:do j=1,4
            pos=current(:,cu)+mv(:,j)
            if(check(pos))then
               if(a(pos(1),pos(2))/="#")then
                  ! if you pass somewhere,you can go again
                  do k=1,pre
                     if(all(prev(:,k)==pos))cycle loopj
                  end do
                  ! remove duplicate
                  do k=1,nex
                     if(all(next(:,k)==pos))cycle loopj
                  end do
                  nex=nex+1
                  next(:,nex)=pos
               end if
            end if
         end do loopj
         cu=cu-1
         if(cu==0)exit
      end do
      pre=tcu
      cu=nex
      prev(:,1:pre)=current(:,1:pre)
      current(:,1:cu)=next(:,1:cu)
      s(i)=s(i-2)+cu ! remember the number
      nex=0
   end do
   write(*,*)s(64)
   cu=1
   pre=1
   nex=0
   current(:,cu)=idx
   s(-1:0)=[0,1]
   ! find loop 3
   do i=1,n*3
      tcu=cu
      do
         loopj2:do j=1,4
            pos=current(:,cu)+mv(:,j)
            pos2=modulo(pos-1,n)+1
            if(a(pos2(1),pos2(2))/="#")then
               do k=1,pre
                  if(all(prev(:,k)==pos))cycle loopj2
               end do
               do k=1,nex
                  if(all(next(:,k)==pos))cycle loopj2
               end do
               nex=nex+1
               next(:,nex)=pos
            end if
         end do loopj2
         cu=cu-1
         if(cu==0)exit
      end do
      pre=tcu
      cu=nex
      prev(:,1:pre)  =current(:,1:pre)
      current(:,1:cu)=next(:,1:cu)
      s(i)=s(i-2)+cu
      nex=0
   end do
   si=s(mod(step,n)::n)
   sb=si(2)-si(1)
   sa=(si(3)-si(2))-(si(2)-si(1))
   associate(m=>step/n)
      ! s1
      !
      ! sa 2sa 3sa 4sa ....
      !  +  +   +   +
      ! sb sb  sb  sb
      write(*,*)sa*m*(m-1)/2+sb*m+si(1)
   end associate
contains
   logical function check(pos)result(res)
      integer,intent(in)::pos(2)
      res=all(pos>0).and.all(pos<n+1)
   end function check
end program main
