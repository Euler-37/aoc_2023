program main
   implicit none
   !integer,parameter::n=5
   integer,parameter::n=1000
   character(len=5)::card(n),s
   integer::score(n),i,j,tmp
   integer(8)::num
   open(10,file="data/7.txt")
   do i=1,n
      read(10,*)card(i),score(i)
   end do
   close(10)
   do i=2,n
      do j=1,n-i+1
         if(compare(card(j),card(j+1),map,check))then
            s=card(j)
            card(j)=card(j+1)
            card(j+1)=s
            tmp=score(j)
            score(j)=score(j+1)
            score(j+1)=tmp
         end if
      end do
   end do
   num=0
   do i=1,n
      num=num+int(i,8)*score(i)
   end do
   write(*,*)num
   do i=2,n
      do j=1,n-i+1
         if(compare(card(j),card(j+1),map2,check2))then
            s=card(j)
            card(j)=card(j+1)
            card(j+1)=s
            tmp=score(j)
            score(j)=score(j+1)
            score(j+1)=tmp
         end if
      end do
   end do
   num=0
   do i=1,n
      num=num+int(i,8)*score(i)
   end do
   write(*,*)num
contains

   logical function compare(a,b,map_t,check_t)result(res)
      character(len=1),intent(in)::a(5),b(5)
      integer::ca,cb,i
      procedure(map)::map_t
      procedure(check)::check_t
      ca=check_t(a)
      cb=check_t(b)
      if(ca>cb)then
         res=.true.
      elseif(ca<cb)then
         res=.false.
      else
         do i=1,5
            ca=map_t(a(i))
            cb=map_t(b(i))
            if(ca>cb)then
               res=.true.
               return
            elseif(ca<cb)then
               res=.false.
               return
            end if
         end do
         error stop "Equal"
      end if
   end function compare

   integer function map(a)result(res)
      character(len=1),intent(in)::a
      select case(a)
      case("A");res=13
      case("K");res=12
      case("Q");res=11
      case("J");res=10
      case("T");res=9
      case("9");res=8
      case("8");res=7
      case("7");res=6
      case("6");res=5
      case("5");res=4
      case("4");res=3
      case("3");res=2
      case("2");res=1
      end select
   end function map

   integer function check(a)result(res)
      character(len=1),intent(in)::a(5)
      integer::hash(13),i,j
      hash=0
      do i=1,5
         j=map(a(i))
         hash(j)=hash(j)+1
      end do
      res=name(hash)
   end function check

   integer function map2(a)result(res)
      character(len=1),intent(in)::a
      select case(a)
      case("A");res=13
      case("K");res=12
      case("Q");res=11
      case("T");res=10
      case("9");res=9
      case("8");res=8
      case("7");res=7
      case("6");res=6
      case("5");res=5
      case("4");res=4
      case("3");res=3
      case("2");res=2
      case("J");res=1
      end select
   end function map2

   integer function check2(a)result(res)
      character(len=1),intent(in)::a(5)
      integer::hash(13),i,j,idx
      hash=0
      do i=1,5
         j=map2(a(i))
         hash(j)=hash(j)+1
      end do
      res=name(hash)
      if(any(a=="J"))then
         select case(res)
         case(6);res=7    ! 4 + J  or  4J + 1
         case(5);res=7    ! 3 + 2J or  3J + 2
         case(4);res=6    ! 3 + 1J or  3J + 1
         case(3);res=merge(6,5,hash(1)==2) !2J + 2 or J + 2  
         case(2);res=4    ! 2J+ 1 or J + 2
         case(1);res=2    ! J +1
         end select
      end if
   end function check2

   integer function name(hash)result(res)
      integer,intent(in)::hash(13)
      if(any(hash==5))then                  ; res=7 ; return ; end if !five of kind
      if(any(hash==4))then                  ; res=6 ; return ; end if !foure of kind
      if(any(hash==3).and.any(hash==2))then ; res=5 ; return ; end if !full house
      if(any(hash==3))then                  ; res=4 ; return ; end if !three of kind
      if(count(hash==2)==2)then             ; res=3 ; return ; end if !two pair
      if(any(hash==2))then                  ; res=2 ; return ; end if !one pair
      res=1
   end function name

end program main
