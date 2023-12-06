program main
   use aoc_2023
   implicit none
   character(len=1000)::line
   integer::n,i,j,k,ios
   integer(8),allocatable::seed(:)
   type tri
      integer(8)::a,b,c
   end type tri
   type map_t
      type(tri),allocatable::m(:)
   end type map_t
   type(map_t)::map(7)
   type(tri)::tmp
   integer(8)::si,ii
   integer(8)::num
   open(10,file="data/5.txt")
   read(10,"(A)")line
   n=getcolnum(line(7:))
   allocate(seed(n))
   read(line(7:),*)seed
   do i=1,7
      allocate(map(i)%m(0))
   end do
   i=0
   do
      read(10,"(A)",iostat=ios)line
      if(is_iostat_end(ios))exit
      if(isdigit(line(1:1)))then
         read(line,*)tmp
         map(i)%m=[map(i)%m,tmp]
      else
         if(isspace(line))cycle
         i=i+1
      end if
   end do
   ! 
   num=huge(num)
   do i=1,n
      si=seed(i)
      do j=1,7
         do k=1,size(map(j)%m)
            associate(mt=>map(j)%m(k))
               if(si>=mt%b.and.si<mt%b+mt%c)then
                  si=mt%a+(si-mt%b)
                  exit
               end if
            end associate
         end do
      end do
      num=min(num,si)
   end do
   !if(this_image()==1)write(*,*)num
   write(*,*)num
   !
   num=huge(num)
   do i=1,n,2
      !$omp parallel do reduction(min:num)default(private)shared(map,seed)num_threads(20)
      !do ii=seed(i),seed(i)+seed(i+1)-1,num_images()
      do ii=seed(i),seed(i)+seed(i+1)-1
         si=ii
         do j=1,7
            do k=1,size(map(j)%m)
               associate(mt=>map(j)%m(k))
                  if(si>=mt%b.and.si<mt%b+mt%c)then
                     si=mt%a+(si-mt%b)
                     exit
                  end if
               end associate
            end do
         end do
         num=min(num,si)
      end do
      !call co_min(num,result_image=1)
      !call co_broadcast(num,source_image=1)
      !$omp end parallel do
   end do
   !if(this_image()==1)write(*,*)num
   write(*,*)num
end program main
