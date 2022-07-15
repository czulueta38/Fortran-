program numbers
  !Cariza Zulueta
  !CDS 251
  !Assignment 3
  !September 15,2021

  !This program prints the standard deviation and average from text file
  !This program reads from a text file
  !This program allocates memory for an array using integer for the size

  implicit none

  !Declartion of all variables used
  real,allocatable ::a(:) !Array of numbers
  integer :: i,n  !Used to iterate do loop and n is used to allocate memory size
  real :: average, oldaverage,std,variance   !variables used for formulas

  !opens numbers.txt file and reads in first integer
  !allocates memory for program using n
  open(42,file='numbers.txt')
    read(42,*) n
    allocate(a(n))

  !Do loop that goes from 1 to n and reads into array a
  !In do loop average and standard deviation are calculated using update formulas
  do i=1,n
    read(42,*) a(i)  !indexing on a which reads one number at a time
    OldAverage=Average
    Average = average + (a(i) - OldAverage)/float(i)
    variance= variance + (a(i)-OldAverage)*(a(i)-Average)
  end do

  !standard deviation calculation using n not i because it is not in do loop
  std=sqrt(variance/float(n))

  !closing file and deallocating array
  close(42)
  deallocate(a)

  !Print statements for average and standard deviation
  print*,'average=',average
  print*,'standard deviation=',std
  print*, 'Done!'

end program numbers
