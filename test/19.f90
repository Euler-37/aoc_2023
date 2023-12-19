program main
   use aoc_2023
   use string_mod
   implicit none
   integer::ios
   character(len=100)::str
   integer,parameter::rej=-1,acc=0,big=1,small=2,pass=3
   type rule
      integer::tp
      integer::id
      integer::num
      integer::to
   end type rule
   type flow
      integer::size
      type(rule)::r(4)
   end type flow
   type(flow)::work(27*27*27)
   integer(8)::num
   integer::t(4)
   integer(8)::lb(4),ub(4)
   integer(8)::arr(1000),ps
   open(10,file="data/19.txt")
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      if(str==" ")exit
      call parser(str)
   end do
   num=0
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      call replace(str, "=", " ")
      call replace(str, "x", " ")
      call replace(str, "m", " ")
      call replace(str, "a", " ")
      call replace(str, "s", " ")
      read(str(2:len_trim(str)-1),*)t
      num=num+is_accept(t)
   end do
   write(*,*)num
   close(10)
   ps=0
   arr=0
   lb=1
   ub=4000
   call is_accept_range(lb,ub,hash("in "))
   write(*,*)sum(arr(1:ps))
contains
   recursive subroutine is_accept_range(lb,ub,idx)
      integer::idx
      integer::i
      integer(8)::lb(4),ub(4)
      integer(8)::tlb(4),tub(4)
      do i=1,work(idx)%size
         associate(w=>work(idx)%r(i))
            select case(w%tp)
            case(acc);ps=ps+1;arr(ps)=product(ub-lb+1);return
            case(rej);return
            case(pass);call is_accept_range(lb,ub,w%to)
            case(small)
               !  lb      ub     num
               if(ub(w%id)<w%num)then
                  select case(w%to)
                  case(acc);ps=ps+1;arr(ps)=product(ub-lb+1);return
                  case(rej);return
                  case default;call is_accept_range(lb,ub,w%to)
                  end select
               ! num      lb      ub
               else if(lb(w%id)>=w%num)then
                  return
               else
                  ! lb      num      ub
                  tlb=lb
                  tub=ub
                  ub(w%id)=w%num-1
                  select case(w%to)
                  case(acc);ps=ps+1;arr(ps)=product(ub-lb+1)
                  case(rej)
                  case default;call is_accept_range(lb,ub,w%to)
                  end select
                  lb=tlb
                  ub=tub
                  lb(w%id)=w%num
               end if
            case(big);
               ! num      lb      ub
               if(lb(w%id)>w%num)then
                  select case(w%to)
                  case(acc);ps=ps+1;arr(ps)=product(ub-lb+1);return
                  case(rej);return
                  case default;call is_accept_range(lb,ub,w%to)
                  end select
               elseif(ub(w%id)<=w%num)then
                  return
               else
                  !lb      num  ub 
                  tlb=lb
                  tub=ub
                  lb(w%id)=w%num+1
                  select case(w%to)
                  case(acc);ps=ps+1;arr(ps)=product(ub-lb+1)
                  case(rej)
                  case default;call is_accept_range(lb,ub,w%to)
                  end select
                  lb=tlb
                  ub=tub
                  ub(w%id)=w%num
               end if
            end select
         end associate
      end do
   end subroutine is_accept_range

   integer function is_accept(t)result(res)
      integer::t(4),idx
      integer::i
      idx=hash("in ")
      res=0
      do
         do i=1,work(idx)%size
            associate(w=>work(idx)%r(i))
               select case(w%tp)
               case(acc);res=sum(t);return
               case(rej);res=0     ;return
               case(pass);
                  idx=w%to         ;exit
               case(small)
                  if(t(w%id)<w%num)then
                     select case(w%to)
                     case(acc);res=sum(t);return
                     case(rej);res=0     ;return
                     case default; idx=w%to     ;exit
                     end select
                  end if
               case(big);
                  if(t(w%id)>w%num)then
                     select case(w%to)
                     case(acc);res=sum(t);return
                     case(rej);res=0     ;return
                     case default; idx=w%to     ;exit
                     end select
                  end if
               end select
            end associate
         end do
      end do
   end function is_accept

   subroutine parser(str)
      character(len=*),intent(inout)::str
      integer::idx
      type(string),allocatable,target::s(:)
      character(len=1),pointer::ps(:)
      integer::jdx,i
      idx=index(str,"{")
      s=split(str(idx+1:len_trim(str)-1),',',.false.)
      idx=hash(str(1:idx-1)//"   ")
      work(idx)%size=size(s)
      do i=1,work(idx)%size
         ps=>string_view(s(i)%str,len(s(i)%str))
         jdx=index(s(i)%str,":")
         ! has :
         associate(w=>work(idx)%r(i))
            if(jdx/=0)then
               work(idx)%r(i)%tp=merge(big,small,ps(2)==">")
               select case(ps(1))
               case("x");w%id=1
               case("m");w%id=2
               case("a");w%id=3
               case("s");w%id=4
               end select
               w%num=tonum(s(i)%str(3:jdx-1))
               select case(s(i)%str(jdx+1:))
               case("A"); w%to=acc
               case("R"); w%to=rej
               case default; w%to=hash(s(i)%str(jdx+1:)//" ")
               end select
            else
               select case(s(i)%str)
               case("A"); w%tp=acc
               case("R"); w%tp=rej
               case default
                  w%tp=pass
                  w%to=hash(s(i)%str//"   ")
               end select
            end if
         end associate
      end do
   end subroutine parser


   integer function hash(a)result(res)
      character(len=1),intent(in)::a(3)
      integer::b(3)
      b=ichar(a)-96
      where(b==32-96)b=0
      res= b(1)*27*27+b(2)*27+b(3)
   end function hash
end program main
