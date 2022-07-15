program eulers
    !Cariza Zulueta
    !CDS 251
    !Assignment 11
    !11/16/2021

    !This program solves an ordinary differential equation
    !This program asks for user input for y values, final time, and step size
    ! This program implements the runge kutta method to solve the equations


    implicit none

    !Declaringn variables
    real::t,y,h,tfinal,stepsize,myfunc,k1,k2,k3,k4
    integer::i,n

    !User input statements
    print*,'insert initial y-value:'
    read(*,*)y
    print*,'insert Final time'
    read(*,*)tfinal
    print*,'insert stepsize'
    read(*,*)h

    !initializing variables
    t=0.0
    n=tfinal/h

    !opens file,writes to file and  implements runge kutta method in do loop
    open(42,file='kutta4.txt')
    do i=1.0,n
       k1=myfunc(t,y)
       k2=myfunc(t+h/2.0*k1,y+h/2.0)
       k3=myfunc(t+h/2.0*k2,y+h/2.0)
       k4=myfunc(t+h*k3,y+h)
       t=t+h
       y=y+(h/6.0)*(k1+k2*2+2*k3+k4)
       write(42,*)t,y
    end do

    close(42)

end program eulers

    !Derivative function we are solving
function myfunc(t,y)result(h)

    implicit none
    real::y,t,h

    h=y**2.0-(y**3.0/5.0)-t

end function myfunc
