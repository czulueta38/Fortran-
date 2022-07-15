program encrypt

   !Cariza Zulueta
   !CDS 251
   !Assignment 4 part 1
   !September 22,2021

   !This program reads in a encryption matrix and inverts it
   !Reads in two numbers at a time decrypts them and then inserts the characers
   !into a character string which then prints out a message

   implicit none

   !Declaring variables
   integer, dimension(2) :: b,a,x
   integer, dimension(2,2) ::Matrix1,Matrix2,Matrix3
   integer::n, determinant, row, col, i
   character(len=32):: message, string
   character(len=1) ::c1,c2

   !Open file
   open(42, file='Data3.txt')

   !Do loop to read in file and matrix
   do row = 1,2
      read(42,*) (Matrix1(row,col), col = 1, 2)
   end do

   !Taking the inverse of matrix and reassigning
   Matrix2(1,1) = Matrix1(2,2)
   Matrix2(2,2) = Matrix1(1,1)
   Matrix2(1,2) = -Matrix1(1,2)
   Matrix2(2,1) = -Matrix1(2,1)
   Matrix3=Matrix2/Determinant(Matrix1,2)

   !do loop that takes matrix3 and multiplies it by the vector to get 2 numbers
   !The two numbers then go through the char function to become characters
   do i=1,16
      read(42,*) b
      a=matmul(Matrix3,b)
      c1=char(a(1))
      c2=char(a(2))
      message(i*2-1:i*2-1)=c1 !inserting characters into message string
      message(i*2:i*2)=c2     !inserting characters into message string
   end do

   print*,message !prints out decoded message

   close(42) !closing file

end program encrypt

   !Function used to divide matrix2 by determinant of matrix 1
   function Determinant(M, n) result(Det)

   implicit none

   integer:: m(n,n),det
   integer:: n

   if (n==2) then
      det=m(1,1)*m(2,2)-m(1,2)*m(2,1)
      return
   else if (n==3) then
      det=m(1,1)*m(2,2)*m(3,3)&
      +m(1,2)*m(2,3)*m(3,1)&
      +m(1,3)*m(2,1)*m(3,2)&
      -m(1,3)*m(2,2)*m(3,1)&
      -m(1,2)*m(2,1)*m(3,3)&
      -m(1,1)*m(2,3)*m(3,2)
      return
   end if
   end function Determinant
