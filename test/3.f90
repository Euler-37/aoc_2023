program main
   use aoc_2023
   implicit none
   integer,parameter::n=140
   character(len=n)::s(n)
   character(len=1),allocatable::near(:)
   integer::i,p,p0,num,len,start,end,j
   integer(8)::res
   type num_pos
      integer::num,i0,j0
   end type num_pos
   type line_num
      type(num_pos),allocatable::a(:)
   end type line_num
   type(line_num)::line(n)
   integer,allocatable::pos(:)

   open(10,file="data/3.txt")
   do i=1,n
      read(10,"(A)")s(i)
   end do
   close(10)
   res=0
   do i=1,n
      ! find a num
      p=1
      p0=1
      allocate(line(i)%a(0))
      do
         if(isdigit(s(i)(p:p)))then
            num=strtol(s(i), p)
            line(i)%a=[line(i)%a,num_pos(num,p0,p-1)]
            !check
            allocate(near(0))
            if(p0-1==0)then
               start=1
            else
               start=p0-1
               near=[character(1)::near,s(i)(start:start)]
            end if
            if(p==n+1)then
               end=n
            else
               end=p
               near=[character(1)::near,s(i)(end:end)]
            end if
            if(i>1)then
               do j=start,end
                  near=[near,s(i-1)(j:j)]
               end do
            end if
            if(i<n)then
               do j=start,end
                  near=[near,s(i+1)(j:j)]
               end do
            end if
            if(any(.not.isdigit(near).and.near/="."))then
               res=res+num
            end if
            p0=p
            deallocate(near)
         else
            p=p+1
            p0=p0+1
         end if
         if(p>n)exit
      end do
   end do
   print*,res
   res=0
   do i=1,n
      p=1
      do
         j=index(s(i)(p:),"*")
         if(j/=0)then
            j=j+p-1
            allocate(pos(0))
            if(i>1) call check(i-1)
            call check(i)
            if(i<n) call check(i+1)
            if(size(pos)==2) res=res+pos(1)*pos(2)
            deallocate(pos)
            p=j+1
            if(p>n)exit
         else
            exit
         end if
      end do
   end do
   print*,res

contains
   logical function isnear(i,i0,j,p0,p1)result(res)
      !i    *
      !j  p0 p1
      integer,intent(in)::i,i0,j,p0,p1
      res=(i-j)**2+(i0-p1)**2 <=2 .or.  (i-j)**2+(i0-p0)**2 <=2
   end function isnear

   subroutine check(i1)
      integer,intent(in)::i1
      integer::k
      do k=1,size(line(i1)%a)
         associate(pn=>line(i1)%a(k))
            if(isnear(i,j,i1,pn%i0,pn%j0))then
               pos=[pos,pn%num]
            end if
         end associate
      end do
   end subroutine check
end program main
