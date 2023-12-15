program main
   use string_mod
   use aoc_2023
   implicit none
   integer::lens
   character(len=:),allocatable::str
   type(string),allocatable::s(:)
   integer::i,idx,j,n
   integer(8)::num
   type box
      character(len=10),allocatable::key(:)
      integer,allocatable::len(:)
   end type box
   type(box)::a(0:255)
   open(10,file="data/15.txt",form="unformatted",access="stream")
   inquire(10,size=lens)
   allocate(character(len=lens-1)::str)
   read(10)str
   close(10)
   s=split(str, ",", .false.)
   num=0
   do i=1,size(s)
      num=num+hash(s(i)%str)
   end do
   write(*,*)num
   do i=0,255
      allocate(a(i)%key(0))
      allocate(a(i)%len(0))
   end do
   do i=1,size(s)
      lens=len(s(i)%str)
      if(s(i)%str(lens:lens)=="-")then
         j=hash(s(i)%str(1:lens-1))
         idx=findloc(a(j)%key,s(i)%str(1:lens-1),1)
         if(idx==0)cycle
         a(j)%key=[a(j)%key(1:idx-1),a(j)%key(idx+1:)]
         a(j)%len=[a(j)%len(1:idx-1),a(j)%len(idx+1:)]
      else
         n=tonum(s(i)%str(lens:lens))
         j=hash(s(i)%str(1:lens-2))
         idx=findloc(a(j)%key,s(i)%str(1:lens-2),1)
         if(idx/=0)then
            a(j)%len(idx)=n
         else
            a(j)%key=[character(len=10)::a(j)%key,s(i)%str(1:lens-2)]
            a(j)%len=[a(j)%len,n]
         end if
      end if
   end do
   num=0
   do i=0,255
      do j=1,size(a(i)%key)
         num=num+(i+1)*j*a(i)%len(j)
      end do
   end do
   write(*,*)num
contains
   integer function hash(s)result(res)
      character(len=*),intent(in)::s
      integer(1)::a(len(s))
      integer::i
      a=transfer(s,1_1,len(s))
      res=0
      do i=1,size(a)
         res=res+a(i)
         res=mod(res*17,256)
      end do
   end function hash
end program main
