program main
   use string_mod
   use aoc_2023
   implicit none
   type(string),allocatable::a(:),b(:),c(:)
   integer::i,num,idx,ios,j,num2
   integer::rgb(3)
   character(len=1)::tmp
   character(len=1024)::str
   open(10,file="data/2.txt")
   num=0
   num2=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      a=split(str,":",.false.)
      idx=tonum(a(1)%str(6:))
      a=split(a(2),";",.false.)
      rgb=0
      do i=1,size(a)
         b=split(a(i),",",.false.)
         do j=1,size(b)
            c=split(b(j)," ",.false.)
            select case(c(2)%str)
            case("red")   ; rgb(1)=max(rgb(1),tonum(c(1)%str))
            case("green") ; rgb(2)=max(rgb(2),tonum(c(1)%str))
            case("blue")  ; rgb(3)=max(rgb(3),tonum(c(1)%str))
            end select 
         end do
      end do
      num2=num2+product(rgb)
      if(all(rgb <=[12,13,14]))then
         num=num+idx
      end if
   end do
   close(10)
   write(*,*)num
   write(*,*)num2
end program main
