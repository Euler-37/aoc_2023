program main
   use aoc_2023
   use string_mod
   implicit none
   !integer,parameter::n=2
   !integer,parameter::m=8
   !integer,parameter::n=3
   !integer,parameter::m=3
   integer,parameter::n=277
   integer,parameter::m=750
   character(len=1),pointer::ptr(:)
   character(len=1)::lr
   character(len=n)::move
   character(len=3)::a(m),b(m),c(m)
   character(len=20)::tmp
   integer::i,j,start,num
   integer,allocatable::starta(:)
   integer(8),allocatable::loop(:)
   integer(8)::l
   open(10,file="data/8.txt")
   read(10,"(A)")move
   read(10,*)
   ptr=>string_view(move, n)
   do i=1,m
      read(10,"(A)")tmp
      call replace(tmp, "=", " ")
      call replace(tmp, "(", " ")
      call replace(tmp, ",", " ")
      call replace(tmp, ")", " ")
      read(tmp,*)a(i),b(i),c(i)
   end do
   start=findloc(a,"AAA",1)
   i=1
   num=1
   do
      lr=ptr(i)
      if(lr=="L")then
         if(b(start)=="ZZZ") exit
         start=findloc(a,b(start),1)
      else
         if(c(start)=="ZZZ") exit
         start=findloc(a,c(start),1)
      end if
      i=i+1
      num=num+1
      if(i>n)i=1
   end do
   write(*,*)num
   allocate(starta(0))
   do i=1,m
      if(a(i)(3:3)=="A") starta=[starta,i]
   end do
   allocate(loop(size(starta)))
   do j=1,size(starta)
      i=1
      num=1
      do
         lr=ptr(i)
         if(lr=="L")then
            if(b(starta(j))(3:3)=="Z")then
               write(*,*)"L",i,num
               loop(j)=num
               exit
            end if
            starta(j)=findloc(a,b(starta(j)),1)
         else
            if(c(starta(j))(3:3)=="Z")then
               write(*,*)"R",i,num
               loop(j)=num
               exit
            end if
            starta(j)=findloc(a,c(starta(j)),1)
         end if
         i=i+1
         num=num+1
         if(i>n)i=1
      end do
   end do
   l=loop(1)
   do i=2,size(starta)
      l=lcm(l,loop(i))
   end do
   write(*,*)l
end program main
