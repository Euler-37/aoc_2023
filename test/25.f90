program main
   use aoc_2023
   use string_mod
   use set_mod
   use hash_mod
   implicit none
   character(len=80)::str
   type(string),allocatable::s(:)
   integer::i,j,ios
   type(set)::graph2,graph1
   type(setiter)::set_iter
   type(hashiter)::hash_iter
   character(len=3),allocatable::nt(:)
   character(len=3)::key
   type(hashmap)::grpah
   class(*),pointer::pg
   type sarray
      integer::size=0
      character(len=3)::a(10)
   end type sarray
   type(sarray)::val
   call grpah%init(eq,bit)
   !---------------- read graph
   open(10,file="data/25.txt")
   do
      str=""
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      call replace(str, ":", " ")
      s=split(trim(str)," ",.true.)
      if(.not.(s(1)%str.in.grpah)) call grpah%append(s(1)%str,val)
      pg=>grpah%view(s(1)%str)
      select type(pg)
      type is (sarray) 
         do i=2,size(s)
            pg%size=pg%size+1
            pg%a(pg%size)=s(i)%str
         end do
      end select
      do i=2,size(s)
         if(.not.(s(i)%str.in.grpah)) call grpah%append(s(i)%str,val)
         pg=>grpah%view(s(i)%str)
         select type(pg)
         type is (sarray) 
            pg%size=pg%size+1
            pg%a(pg%size)=s(1)%str
         end select
      end do
   end do
   close(10)
   !------------- read graph
   !-------------- get keys
   allocate(nt(grpah%num))
   call hash_iter%init(grpah)
   i=0
   do while(hash_iter%next(key,val))
      i=i+1
      nt(i)=key
   end do
   !-------------- get keys
   do i=1,grpah%num
      !------------- split as graph1 graph2
      call graph1%init(eq,bit)
      call graph1%append(nt(i))
      call graph2%init(eq,bit)
      do j=1,grpah%num
         if(j/=i) call graph2%append(nt(j))
      end do
      !------------- split as graph1 graph2
      do while(graph2%num>0)
         !
         block
            integer::maxnum,cnt,nums
            character(len=3)::maxkey
            cnt=0
            maxnum=0
            call set_iter%init(graph2)
            ! count connect betweem `graph1` and `graph2`
            do while(set_iter%next(key))
               nums=0
               pg=>grpah%view(key)
               select type(pg)
               type is (sarray) 
                  do j=1,pg%size
                     if(pg%a(j).in.graph1)then
                        nums=nums+1
                     end if
                  end do
               end select
               if(maxnum<nums)then
                  maxnum=nums
                  maxkey=key
               end if
               cnt=cnt+nums
            end do
            ! cnt < 3 return
            if(cnt <=3)then
               write(*,*)i,graph1%num*graph2%num
               stop
            end if
            ! else
            ! move the max one to graph1
            call graph1%append(maxkey)
            call graph2%remove(maxkey)
         end block
      end do
   end do
contains  
   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); type is (character(len=*))
      select type(b); type is (character(len=*))
      a=b
      end select; end select 
      select type(a); type is (sarray)
      select type(b); type is (sarray)
      a%size =b%size
      a%a    =b%a
      end select; end select 
   end subroutine eq

   subroutine bit(a,b)
      class(*),intent(in)::a
      integer(1),intent(inout),allocatable::b(:)
      select type(a)
      type is (character(len=*))
         b=transfer(a,1_1,len(a))
      end select
   end subroutine bit
end program main
