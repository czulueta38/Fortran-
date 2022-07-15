program one
!Cariza Zulueta
!CDS 251
!Assignment 2
!9/6/2021
!This program creates a do loop that
!iterates two variables ten million times

implicit none !declaring all variables

!variables
integer :: I !do loop

real*4 A,B !four byte floating numbers

A=1.0 !real number set to 1.0
B=2.0 !real number set to 2.0

DO I=1,10000000 !iterates ten million times
	A=A+1.E-7 !adds 1.e-7 to each variable
	B=B+1.E-7 !adds 1.e-7 to each variable
END DO

!print statements 
print*,'A=',A
print*,'B=',B

END PROGRAM  one
