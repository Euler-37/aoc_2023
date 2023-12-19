program main
   use aoc_2023
   implicit none
   integer,parameter::n=141
   !integer,parameter::n=13
   integer::a(n,n),i,j
   integer,parameter::mv(2,4)=reshape([-1,0,1,0,0,-1,0,1],shape=[2,4])
   integer,parameter::up=1,down=2,left=3,right=4
   integer::path(2,n,n),idx(2) ,p(n,n)
   open(10,file="data/17.txt")
   do i=1,n
      read(10,"("//tostring(n)//"i1)")a(i,:)
   end do
   close(10)
   path=0
   call dijkstra()
   call dijkstra_ultra()
contains
    subroutine dijkstra_ultra()
        implicit none
        integer::is_add(0:4,10,n,n)
        integer::dis(0:4,10,n,n)
        integer::jx,jy,jrx,jry
        integer::pos(2),next(2),val
        integer::idx(4)
        logical::ultra
        path=0
        is_add=0
        dis=9999999
        dis(0,1,1,1)=0
        do while(any(is_add==0))
            idx=minloc(dis, mask=is_add/=1)-[1,0,0,0]
            pos=idx(3:4)
            associate(ix=>pos(1),iy=>pos(2),is=>idx(1:2))
               is_add(is(1),is(2),ix,iy)=1
               do i=1,4
                  next=pos+mv(:,i)
                  if(check(next).and.move(i,is(1)))then
                     associate(jx=>next(1),jy=>next(2))
                        if(is(1)==i)then
                           if(is(2)+1<11)then
                              if(is_add(i,is(2)+1,jx,jy)==0)then
                                 if(dis(i,is(2)+1,jx,jy)>dis(i,is(2),ix,iy)+a(jx,jy))then
                                    dis(i,is(2)+1,jx,jy)=dis(i,is(2),ix,iy)+a(jx,jy)
                                 end if
                              end if
                           end if
                        else
                           if(is(2)>3.or.is(1)==0)then
                              if(is_add(i,1,jx,jy)==0)then
                                 if(dis(i,1,jx,jy)>dis(is(1),is(2),ix,iy)+a(jx,jy))then
                                    dis(i,1,jx,jy)=dis(is(1),is(2),ix,iy)+a(jx,jy)
                                 end if
                              end if
                           end if
                        end if
                     end associate
                  end if
               end do
            end associate
        end do
        write(*,*)minval(dis(:,:,n,n))
    end subroutine dijkstra_ultra

    subroutine dijkstra()
        implicit none
        integer::is_add(4,3,n,n)
        integer::dis(4,3,n,n)
        integer::jx,jy,jrx,jry
        integer::pos(2),next(2),val
        integer::idx(4)
        logical::ultra
        path=0
        is_add=0
        dis=9999999
        dis(down,1,1,1)=0
        dis(right,1,1,1)=0
        do while(any(is_add==0))
            idx=minloc(dis, mask=is_add/=1)
            pos=idx(3:4)
            associate(ix=>pos(1),iy=>pos(2),is=>idx(1:2))
               is_add(is(1),is(2),ix,iy)=1
               do i=1,4
                  next=pos+mv(:,i)
                  if(check(next).and.move(i,is(1)))then
                     associate(jx=>next(1),jy=>next(2))
                        if(is(1)==i)then
                           if(is(2)+1<4)then
                              if(is_add(i,is(2)+1,jx,jy)==0)then
                                 if(dis(i,is(2)+1,jx,jy)>dis(i,is(2),ix,iy)+a(jx,jy))then
                                    dis(i,is(2)+1,jx,jy)=dis(i,is(2),ix,iy)+a(jx,jy)
                                    path(:,jx,jy)=[ix,iy]
                                 end if
                              end if
                           end if
                        else
                           if(is_add(i,1,jx,jy)==0)then
                              if(dis(i,1,jx,jy)>dis(is(1),is(2),ix,iy)+a(jx,jy))then
                                 dis(i,1,jx,jy)=dis(is(1),is(2),ix,iy)+a(jx,jy)
                                 path(:,jx,jy)=[ix,iy]
                              end if
                           end if
                        end if
                     end associate
                  end if
               end do
            end associate
        end do
        write(*,*)minval(dis(:,:,n,n))
    end subroutine dijkstra

    logical function move(i,flag)result(res)
       integer::i,flag
       res=.true.
       if(i==2.and.flag==1)res=.false.
       if(i==1.and.flag==2)res=.false.
       if(i==3.and.flag==4)res=.false.
       if(i==4.and.flag==3)res=.false.
    end function move

   logical function check(pos)result(res)
      integer,intent(in)::pos(2)
      res=all(pos>0).and.all(pos<n+1)
   end function check

   logical function find(p,pos)result(res)
      integer,intent(in)::p(2,n*n)
      integer,intent(in)::pos(2)
      integer::i
      res=.false.
      do i=1,n*n
         if(all(p(:,i)==pos))then
            res=.true.
            return
         end if
      end do
   end function find
end program main
