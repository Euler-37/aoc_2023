program main
   implicit none
   character(len=100)::str
   integer::lens,i,ios,num,j,n,m,is,num1
   character(len=:),allocatable::a
   character(len=1),allocatable::pa(:)
   character(len=1),allocatable::pb(:,:)
   open(10,file="data/13.txt")
   num=0
   num1=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      lens=len_trim(str)
      n=lens
      if(n==0)exit
      backspace(10)
      a=""
      do
         read(10,"(A)",iostat=ios)str
         if(len_trim(str)==0.or.is_iostat_end(ios))exit
         a=a//str(1:lens)
      end do
      m=len(a)/n
      allocate(pb(n,m))
      pb(:,:)=reshape(string2array(a),shape=[n,m])
      do i=1,m-1
         if(2*i>m)exit
         is=count(pb(:,1:i)/=pb(:,2*i:i+1:-1))
         if(is==0)then
            num=num+i*100
         elseif(is==1)then
            num1=num1+(i)*100
         end if
      end do
      do i=m,2,-1
         if(2*i-m-1<=0)exit
         is=count(pb(:,i:m)/=pb(:,i-1:(2*i-m-1):-1))
         if(is==0)then
            num=num+(i-1)*100
         elseif(is==1)then
            num1=num1+(i-1)*100
         end if
      end do
      do i=1,n-1
         if(2*i>n)exit
         is=count((pb(1:i,:)/=pb(2*i:i+1:-1,:)))
         if(is==0)then
            num=num+i
         elseif(is==1)then
            num1=num1+i
         end if
      end do
      do i=n,2,-1
         if(2*i-n-1<=0)exit
         is=count((pb(i:n,:)/=pb(i-1:(2*i-n-1):-1,:)))
         if(is==0)then
            num=num+(i-1)
         elseif(is==1)then
            num1=num1+(i-1)
         end if
      end do
      deallocate(pb)
   end do
   write(*,*)num
   write(*,*)num1
contains
   function string2array(a)result(b)
      character(len=*),intent(in)::a
      character(1)::b(len_trim(a))
      integer::i
      do i=1,len_trim(a)
         b(i)=a(i:i)
      end do
   end function string2array
end program main
