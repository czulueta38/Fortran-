program lab
    !Cariza Zulueta
    !CDS 251
    !Assignment 7
    !October 21,2021

    !This program is uses Newton's method to find roots
    !This program creates two functions to find the root for the equation
    !This program finds all five roots of y=sin(x)-15x+1.5
    !NEED TO FIX DOUBLE PRECISION NUMBERS

    implicit none
    !declaring variables
    double precision::x,MyFunc,NewFunc,Tolerance
    integer::Count

    !Reading in variable x
    print*, 'Enter a number:'
    read(*,*)x

    !Initializations for count function and tolerance
    Count = 0
    Tolerance=1.0d-14

    !do loop for count function and Newtons method
    do
      if (abs(MyFunc(x)) .lt. Tolerance) exit
        Count = Count + 1.0d0  !count function to count iterations
      if (Count .gt. 1000000) exit
        x=x-(MyFunc(x))/(NewFunc(x)) !Newtons method
    enddo

      !prints the root, iterations, and value of function
      print*,x
      print*,Count
      print*,MyFunc(x)

end program lab


function MyFunc(x) result(y)
    !Function evaluation 
    implicit none

    double precision::x,y

    y=sin(x)+1.5-0.15*x

end function MyFunc


function NewFunc(x) result(y)
    !Derivative of function for Newtons method
    implicit none

    double precision::x,y

    y=cos(x)-.15

end function NewFunc
