program exponent
    !Cariza Zulueta
    !CDS 251
    !Assignment 7
    !October 14,2021

    !This program reads in file Edata.txt
    !Edata.txt has 200 data points that look like an Exponential function
    !This program uses update formulas that compute averages and sums in the do loop

    implicit none

    !Declaring Variables
    integer :: i
    real ::Sxx,Sxy,xAverage,yAverage,m,x,y,b

    !opening data file
    open(42,file='Edata.txt')

    !Initializing Variables for equations
    Sxx=0
    Sxy=0
    xAverage=0
    yAverage=0

      !Do loop that iterates the 200 points
     do i=1,200
        read(42,*) x,y !Reading in x and y
        y=log(y) !log of y putting it back into y
        !Equations for Exponential equation turned to linear equation
        Sxx=Sxx+(float(i-1))/float(i)*(x-xAverage)**2
        Sxy=Sxy+(float(i-1))/float(i)*(x-xAverage)*(y-yAverage)
        xAverage=xAverage +(x-xAverage)/float(i)
        yAverage=yAverage +(y-yAverage)/float(i)
      end do

    !Calculations for m and b also a is just m
    m=Sxy/Sxx
    b=yAverage-m*xAverage

    !Print statements
    print*,'Exponent a =',m
    print*,'Coefficient b =',exp(b)

    !Closing File
    close(42)

end program exponent
