program main
   implicit none
   !integer,parameter::n=3
   integer,parameter::n=4
   integer::time(n),Distance(n)
   character::tmp
   character(20)::line
   integer(8)::num
   integer::i,j,k
   integer(8)::tn,dn
   open(10,file="data/6.txt")
   read(10,*)tmp,time
   read(10,*)tmp,Distance
   close(10)
   num=1
   do i=1,n
      k=0
      associate(s1=>Distance(i)/time(i),&
            s2=>nint(0.5d0*time(i)+0.5d0*sqrt(time(i)**2-4.d0*Distance(i)),8))
         do j=s1,s2
            if(j*(time(i)-j)>Distance(i))then
               k=k+1
            end if
         end do
      end associate
      num=num*k
   end do
   write(*,*)num
   write(line,"(*(g0))")time
   read(line,*)tn
   write(line,"(*(g0))")Distance
   read(line,*)dn
   num=0
   associate(s2=>nint(0.5d0*tn+0.5d0*sqrt(tn**2-4.d0*dn),8))
      do j=dn/tn,s2
         if(j*(tn-j)>dn)then
            num=num+1
         end if
      end do
   end associate
   write(*,*)num
end program main
