program main
   use aoc_2023
   use string_mod
   implicit none
   character(len=100)::str
   character(len=:),allocatable::str2
   integer::ios,idx,ndp
   integer(8)::nall2,nall
   integer,allocatable::a(:),a2(:)
   character(1),pointer::ps(:)
   character(1),pointer::ps2(:)
   open(10,file="data/12.txt")
   nall=0
   nall2=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      idx=index(str," ")
      ps=>string_view(str,idx-1)
      if(allocated(a))deallocate(a)
      allocate(a(getcolnum(trim(str(idx+1:)))))
      read(str(idx+1:),*)a
      str2=repeat(str(1:idx-1)//"?",5)
      ! 2
      ps2=>string_view(str2,len(str2)-1)
      a2=[spread(a,2,5)]
      ! dp
      nall=nall+nums(ps,a)
      nall2=nall2+nums(ps2,a2)
      !block
      !integer::nums,numq
      !character(len=1),allocatable::s(:)
      !character(len=1),allocatable::ts(:)
      !integer,allocatable::pos(:)
      !type(string),allocatable::sa(:)
      !integer,allocatable::sas(:)
      !integer::i,k,num
      !nums=sum(a)-count(ps=="#")
      !numq=count(ps=="?")
      !pos=pack([(i,i=1,idx-1)],ps=="?")
      !if(nums<0.or.nums>numq)then
      !cycle
      !end if
      !s=[("#",i=1,nums),(".",i=1,numq-nums)]
      !num=0
      !ts=ps
      !do
      !call p_replace(ps,s,pos)
      !sa=split(str(1:idx-1),".",.true.)
      !!call sa%print()
      !sas=sa%size()
      !!write(*,*)sas
      !!read(*,*)
      !sas=pack(sas,.true.,vector=[(0,i=1,size(a))])
      !num=num+merge(1,0,all(sas==a))
      !if(.not.next_permutation(s))exit
      !end do
      !!nall=nall+num
      !if(ndp/=num)then
      !print*,ts,a
      !print*,num,ndp
      !read(*,*)
      !end if
      !end block
   end do
   close(10)
   write(*,*)nall
   write(*,*)nall2
contains
   subroutine p_replace(a,b,s)
      character(len=1)::a(:),b(:)
      integer::s(:)
      integer::k,i
      k=1
      do i=1,size(a)
         if(i==s(k))then
            a(i)=b(k)
            if(k==size(b))exit
            k=k+1
         end if
      end do
   end subroutine p_replace


   integer(8) function nums(str,arr)result(res)
      character(len=1)::str(:)
      integer::arr(:),k,ka
      integer(8)::dp(size(arr),0:size(str))
      integer::m,n,i,l
      m=size(arr)
      n=size(str)
      dp=0
      do l=1,n
         if(l-arr(1)>=0)then
            ! 只加1个空位，前面l-1不变
            ! 放在最后1个(保证可以放入)，前面不放
            if(str(l)==".")then
               dp(1,l)=dp(1,l-1)
            else if(str(l)=="#")then
               if(all(str(l-arr(1)+1:l)/="."))then
                  if(l-arr(1)==0)then
                     dp(1,l)=1
                  else
                     !前面没有#
                     if(all(str(1:l-arr(1))/="#"))then
                        dp(1,l)=1
                     else
                        dp(1,l)=0
                     end if
                  end if
               else
                  dp(1,l)=0
               end if
            else
               if(all(str(l-arr(1)+1:l)/="."))then
                  !放在后面，那么前面必须没有#
                  if(l-arr(1)==0)then
                     dp(1,l)=1
                  else
                     if(all(str(:l-arr(1))/='#'))then
                        dp(1,l)=1+dp(1,l-1)
                     else
                        dp(1,l)=dp(1,l-1)
                     end if
                  end if
               else
                  dp(1,l)=dp(1,l-1)
               end if
            end if
         end if
      end do
      !write(*,*)dp(1,1:)
      do i=2,m
         do l=1,n
            if(l-arr(i)-1>0)then
               ! 字符长度为l,填i个数组
               ! (l-a(i)-1) 填 i-1个数组 最后放一个a(i)
               ! 最后1个空位，前面l-1填i个
               if(str(l)==".")then
                  dp(i,l)=dp(i,l-1)
               else if(str(l)=="#")then
                  if(all(str(l-arr(i)+1:l)/="."))then
                     if(str(l-arr(i))/="#")then
                        dp(i,l)=dp(i-1,l-arr(i)-1)
                     else
                        dp(i,l)=0
                     end if
                  else
                     dp(i,l)=0
                  end if
               else
                  if(all(str(l-arr(i)+1:l)/="."))then
                     if(str(l-arr(i))/="#")then
                        dp(i,l)=dp(i-1,l-arr(i)-1)+dp(i,l-1)
                     else
                        dp(i,l)=dp(i,l-1)
                     end if
                  else
                     dp(i,l)=dp(i,l-1)
                  end if
               end if
            end if
         end do
         !print*,dp(i,1:)
      end do
      res=dp(m,n)
   end function nums
end program main
