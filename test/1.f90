
program main
   use aoc_2023
   implicit none
   character(len=1000)::str
   integer::ios,i,j,num,idx(0:9),jdx(0:9),k
   character(len=10),parameter::a(*)=[character(len=10)::"one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
   real(8)::tic
   tic=clock()
   open(10,file="data/1.txt")
   num=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      i=scan(str,"0123456789")
      j=scan(str,"0123456789",back=.true.)
      num=num+tonum(str(i:i)//str(j:j))
   end do
   close(10)
   write(*,*)num
   open(10,file="data/1.txt")
   num=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      idx=0
      idx(0)=scan(str,"0123456789")
      do k=1,9
         idx(k)=index(str,trim(a(k)))
      end do
      i=minloc(idx,mask=idx/=0,dim=1)-1

      jdx=0
      jdx(0)=scan(str,"0123456789",back=.true.)
      do k=1,9
         jdx(k)=index(str,trim(a(k)),back=.true.)
      end do
      j=maxloc(jdx,mask=jdx/=0,dim=1)-1
      associate(ki=>idx(0),kj=>jdx(0))
         if(i==0.and.j==0) num=num+tonum(str(ki:ki)//str(kj:kj))
         if(i==0.and.j/=0) num=num+tonum(str(ki:ki))*10+j
         if(i/=0.and.j==0) num=num+i*10+tonum(str(kj:kj))
         if(i/=0.and.j/=0) num=num+i*10+j
      end associate
   end do
   close(10)
   write(*,*)num
   write(*,*)clock()-tic
end program main
