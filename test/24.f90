program main
   use aoc_2023
   implicit none
   !integer,parameter::n=5
   integer,parameter::n=300
   integer(8)::x(3,n),v(3,n)
   character(len=100)::str
   integer::i,j,num
   open(10,file="data/24.txt")
   do i=1,n
      str=""
      read(10,"(A)")str
      call replace(str, "@", ",")
      read(str,*)x(:,i),v(:,i)
   end do
   close(10)
   num=0
   do i=1,n
      do j=i+1,n
         block
            real(8)::a(2,2),b(2),work(2)
            integer::ipiv(2)
            !x1 + v1 * t1 = x2 + v2 * t2
            !v1 * t1 - v2 * t2 = x2 - x1
            a(1,:)=[real(8)::v(1,i),-v(1,j)]! [t1]
            a(2,:)=[real(8)::v(2,i),-v(2,j)]! [t2]
            b(1)=x(1,j)-x(1,i)
            b(2)=x(2,j)-x(2,i)
            if(inv(a, 2, work, ipiv))then
               b=matmul(a,b)
               if(any(b<0))cycle
               b=x(1:2,i)+b(1)*v(1:2,i)
               if(all(b>=200000000000000_8).and.all(b<=400000000000000_8))then
                  num=num+1
               end if
            end if
         end block
      end do
   end do
   write(*,*)num
   block
      !(x1 + v1 * t1)=(x' + v' * t1)
      ! x1 X v1  = x' X v1 + t1 v' X v1
      ! t1 = -(x'-x1) X v1 / v' X v1 
      ! x1 X v' + t1 v1 X v' =x' X v'
      ! t1 =  (x'-x1)X v'/ v1 X v'
      ! (x'-x1) X v' = (x'-x1) X v1
      !
      ! x' X v' = (x'-x1) X v1 + x1 X v'
      ! x' X v' = (x'-x2) X v2 + x2 X v'
      ! x' X v' = (x'-x3) X v3 + x2 X v'
      !
      ! -v1 X x' + x1 X v' - x1 X v1 = -v2 X x' + x2 X v' - x2 X v2
      ! -v2 X x' + x2 X v' - x2 X v2 = -v3 X x' + x3 X v' - x3 X v3
      !
      !(v2-v1) X x' + (x1-x2)v' = x1 X v1 - x2 X v2 
      !(v3-v2) X x' + (x2-x3)v' = x2 X v2 - x3 X v3 
      !
      ! i j k
      ! a b c
      ! x y z
      ! bz-cy, cx-az , ay-bx
      ! (0 ,-c,b )(0,y,z)
      ! (c , 0,-a)(x,0,z)
      ! (-b, a,0 )(x,y,0)
      real(8)::a(6,6),b(6),work(6)
      integer::ipiv(6)
      associate(v1=>v(:,1:3),x1=>(x(:,1:3)))
         associate(v21=>v(:,2)-v(:,1),&
               v32=>v(:,3)-v(:,2),&
               x12=>x(:,1)-x(:,2),&
               x23=>x(:,2)-x(:,3))
            a(1,:)=[real(8)::      0,-v21(3), v21(2),      0,-x12(3), x12(2)]
            a(2,:)=[real(8):: v21(3),      0,-v21(1), x12(3),      0,-x12(1)]
            a(3,:)=[real(8)::-v21(2), v21(1),      0,-x12(2), x12(1),      0]
            a(4,:)=[real(8)::      0,-v32(3), v32(2),      0,-x23(3), x23(2)]
            a(5,:)=[real(8):: v32(3),      0,-v32(1), x23(3),      0,-x23(1)]
            a(6,:)=[real(8)::-v32(2), v32(1),      0,-x23(2), x23(1),      0]

            b(1:3)= [real(8)::cross(x(:,1),v(:,1))-cross(x(:,2),v(:,2))]
            b(4:6)= [real(8)::cross(x(:,2),v(:,2))-cross(x(:,3),v(:,3))]
            if(inv(a,6,work,ipiv))then
               b=matmul(a,b)
               write(*,*)sum(b(1:3))
            end if
         end  associate
      end associate
   end block
contains
   function cross(a,b)result(c)
      integer(8)::a(3),b(3),c(3)
      c(1)=a(2)*b(3)-a(3)*b(2)
      c(2)=a(3)*b(1)-a(1)*b(3)
      c(3)=a(1)*b(2)-a(2)*b(1)
   end function cross

   logical function inv (b,n,work,ipiv) result(res)
      implicit none
      integer,intent(in)   :: n
      real(8),intent(inout):: b(n,n)
      real(8),intent(inout):: work(n)
      integer,intent(inout):: ipiv(n)
      real(8) ::  c, d
      integer :: i, j, k, m
      res=.true.
      do i=1,n
         ipiv(i)=i
      end do
      do k = 1,n
         m = maxloc(abs(b(k:n,k)),dim=1)+k-1
         if(abs(b(m,k))<1.d-14) then
            res=.false.
            !write(*,*) 'Singular Matrix'
            !write(*,*) m,k
            return
         end if
         if (m /= k) then
            ipiv(k)=m
            call swap(b(m,:),b(k,:))
         end if
         d = 1/b(k,k)
         work = b(:,k)
         do j = 1, n
            c = b(k,j)*d
            b(:,j) = b(:,j)-work*c
            b(k,j) = c
         end do
         b(:,k) = work*(-d)
         b(k,k) = d
      end do
      do i=n,1,-1
         if(ipiv(i)==i)cycle
         call swap(b(:,ipiv(i)),b(:,i))
      end do
   end function inv

   pure elemental subroutine swap(a,b)
      real(8),intent(inout)::a
      real(8),intent(inout)::b
      real(8)::tmp
      tmp=a;a=b;b=tmp
   end subroutine swap
end program main
