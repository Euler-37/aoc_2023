program main
   use aoc_2023
   implicit none
   !integer,parameter::n=10
   integer,parameter::n=100
   integer,parameter::m=400
   character(len=1)::s(n,n)
   integer,allocatable::idx(:)
   integer::i,j,k,rock,num
   integer::loop(m),period,before
   open(10,file="data/14.txt")
   do i=1,n
      read(10,"("//tostring(n)//"A1)")s(i,:)
   end do
   close(10)
   do i=1,n
      call line(s(n:1:-1,i))
   end do
   num=0
   do i=1,n
      num=num+count(s(i,:)=="O")*(n+1-i)
   end do
   write(*,*)num
   ! guess
   do k=1,m
      do i=1,n
         call line(s(n:1:-1,i))
      end do
      do i=1,n
         call line(s(i,n:1:-1))
      end do
      do i=1,n
         call line(s(:,i))
      end do
      do i=1,n
         call line(s(i,:))
      end do
      num=0
      do i=1,n
         num=num+count(s(i,:)=="O")*(n+1-i)
      end do
      loop(k)=num
   end do
   do period=1,m
      if(all(loop(m-period+1:m)==loop(m-2*period+1:m-period)))exit
   end do
   !write(*,*)period
   do before=1,m
      if(all(loop(before:before+period-1)==loop(before+period:before+2*period-1)))exit
   end do
   before=before-1
   !write(*,*)before
   !write(*,*)mod(1000000000_8-before,period)
   write(*,*)loop(mod(1000000000_8-before,period)+before)
contains
   subroutine line(a)
      character(len=1),intent(inout)::a(:)
      integer::start,end,i,rock,k
      idx=[1,pack([(j,j=1,n)],a(:)=="#"),n+1]
      do j=1,size(idx)-1
         rock=count(a(idx(j):idx(j+1)-1)=="O")
         do k=idx(j),idx(j+1)-1
            if(a(k)/="#")a(k)="."
         end do
         do k=idx(j+1)-rock,idx(j+1)-1
            a(k)="O"
         end do
      end do
   end subroutine line
end program main
