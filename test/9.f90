program main
   use aoc_2023
   implicit none
   integer,allocatable::a(:)
   integer,allocatable::b(:)
   integer::n,ios,i
   integer(8)::num1,m,num2
   character(len=200)::str
   open(10,file="data/9.txt")
   num1=0
   num2=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      n=getcolnum(str)
      allocate(a(n))
      allocate(b(0))
      read(str,*)a
      m=0
      do
         n=size(a)
         m=m+a(n)
         b=[a(1),b]
         if(all(a==0))exit
         a=a(2:)-a(:n-1)
      end do
      num1=num1+m
      m=0
      do i=1,size(b)-1
         m=b(i+1)-m
      end do
      num2=num2+m
      deallocate(a)
      deallocate(b)
   end do
   write(*,*)num1
   write(*,*)num2

end program main
