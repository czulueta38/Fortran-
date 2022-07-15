program bubble

   !Cariza Zulueta
   !CDS 251
   !Assignment 5
   !September 29,2021

   !This program asks the user to type a file name and reads in the Data
   !This program implements best bubble sort with indexing
   !This program writes to the file output.txt
   !this program writes all numbers in sorted order one per line
   !this program prints done to the screen

   implicit none

   !Declaring variables
   character*50 :: filein
   integer::i,j,n,Temp
   integer, allocatable::inA(:)
   real, allocatable::A(:)
   logical:: Done

   !Reading in files and allocating for array
   print*,'input file:'
   read(*,*) filein
   open(42,file=filein)
   read(42,*) n
   allocate(a(n))
   allocate(inA(n))

   !Reading in array for index
   do i=1,n
     read(42,*) a(i)
     inA(i)=i
   end do

   !swapping values and creating best bubble sort
   do i= 1, n - 1
     Done=.false.
     do j = 1, n - i
         if (A(inA(j)) .gt. A(inA(j+1))) then
           Temp = inA(j)
           inA(j) = inA(j+1)
           inA(j+1) = Temp
         endif
     enddo
     if (Done) exit
   enddo

   open(43,file='Output.txt') !creates output file

   do i=1,n
         write(43,*) A(inA(i)) !accessing data through inA
   end do

   !Closing files and deallocating arrays
   close(42)
   close(43)
   deallocate(a)
   deallocate(inA)

   print*, 'Done!'

end program bubble
