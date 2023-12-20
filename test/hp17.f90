program main
   use aoc_2023
   use heap_mod
   implicit none
   integer,parameter::n=141
   !integer,parameter::n=13
   integer::a(n,n),i,j
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer,parameter::up=2,down=1,left=3,right=4
   type node
      integer::lens
      integer::ix,iy
      integer::mv,step
   end type node
   integer::flag(0:4,0:11,n,n)
   type(node)::ff(n*n*(12*5))
   integer::ps
   open(10,file="data/17.txt")
   do i=1,n
      read(10,"("//tostring(n)//"i1)")(a(i,j),j=1,n)
   end do
   close(10)
   call dijkstra_heap(.false.)
   call dijkstra_heap(.true.)
contains
   logical function flag_eq(a,b)result(res)
      type(node)::a,b
      res=all([a%ix==b%ix,a%iy==b%iy,a%step==b%step,a%mv==b%mv])
   end function flag_eq

   subroutine  dijkstra_heap(ultra)
      type(heap)::m
      type(node)::x
      integer::idx,res
      integer::ix,iy,jx,jy
      logical::ultra
      call m%init(n*n*(12*5),node(0,0,0,0,0),eq,cmp,sw)
      call m%insert(node(0,1,1,0,0))
      ps=0
      flag=0
      do while(m%size>0)
         call m%pop(x)
         idx=flag(x%mv,x%step,x%ix,x%iy)
         if(idx/=0)cycle
         ps=ps+1
         ff(ps)=x
         flag(x%mv,x%step,x%ix,x%iy)=1
         do i=1,4
            jx=x%ix+mv(1,i)
            jy=x%iy+mv(2,i)
            if(check(jx,jy).and.move(x%mv,i))then
               if(ultra)then
                  if(i==x%mv)then
                     if(x%step+1<11)then
                        call m%insert(node(x%lens+a(jx,jy),jx,jy,i,x%step+1))
                     end if
                  else
                     if(x%step>3.or.x%mv==0)then
                        call m%insert(node(x%lens+a(jx,jy),jx,jy,i,1))
                     end if
                  end if
               else
                  if(i==x%mv)then
                     if(x%step+1<4)then
                        call m%insert(node(x%lens+a(jx,jy),jx,jy,i,x%step+1))
                     end if
                  else
                     call m%insert(node(x%lens+a(jx,jy),jx,jy,i,1))
                  end if
               end if
            end if
         end do
      end do
      res=99999
      do i=1,ps
         if(ultra)then
            if(ff(i)%step>4.and.ff(i)%step<10.and.ff(i)%ix==n.and.ff(i)%iy==n)then
               res=min(res,ff(i)%lens)
            end if
         else
            if(ff(i)%step<4.and.ff(i)%ix==n.and.ff(i)%iy==n)then
               res=min(res,ff(i)%lens)
            end if
         end if
      end do
      write(*,*)res
      call m%clean()
   end subroutine

   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a)
      class is (node)
         select type(b)
         class is (node)
            a%lens=b%lens
            a%ix=b%ix
            a%iy=b%iy
            a%step=b%step
            a%mv=b%mv
         end select
      end select 
   end subroutine eq

   subroutine sw(a,b)
      class(*),intent(inout)::a,b
      type(node)::tmp
      call eq(tmp,b)
      call eq(b,a)
      call eq(a,tmp)
   end subroutine sw

   logical function cmp(a,b)result(res)
      class(*),intent(in)::a,b
      select type(a)
      class is (node)
         select type(b)
         class is (node)
            res=a%lens<b%lens
         end select
      end select 
   end function cmp


   logical function check(ix,iy)result(res)
      integer,intent(in)::ix,iy
      res=all([ix,iy]>0).and.all([ix,iy]<n+1)
   end function check

    logical function move(i,flag)result(res)
       integer::i,flag
       res=.true.
       if(i==2.and.flag==1)res=.false.
       if(i==1.and.flag==2)res=.false.
       if(i==3.and.flag==4)res=.false.
       if(i==4.and.flag==3)res=.false.
    end function move
end program main
