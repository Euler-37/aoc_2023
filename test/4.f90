program main
   use aoc_2023
   implicit none
   !integer,parameter::n=5,m=8,l=6
   integer,parameter::n=10,m=25,l=209
   integer::hash(100)
   integer::a(n),b(m),ios,i,j
   character(len=120)::str
   character(1)::tmp
   integer(8)::nc(l),num,nn(l)
   open(10,file="data/4.txt")
   num=0
   do j=1,l
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      call replace(str, ":", " ")
      call replace(str, "|", " ")
      read(str,*)tmp,i,a,b
      hash=0
      hash(b)=1
      nc(j)=sum(hash(a))
      num=num+2**(nc(j)-1)
   end do
   write(*,*)num
   nn=1
   do j=1,l
      do i=1,nc(j)
         if(j+i<=l)then
            nn(j+i)=nn(j+i)+nn(j)
         end if
      end do
   end do
   write(*,*)sum(nn)
end program main
