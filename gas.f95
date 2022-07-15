program numbers
    !Cariza Zulueta
    !CDS 251
    !Assignment 10
    !November 8,2021

    !This program generates 13000 numbers
    !This program has a gaussian distribution of 22.0 and 2.5
    !This program generates 7000 numbers this program has an average of 15.5 and a standard deviation  of 1.0

    !Declaring variables
    real::p,x,y,StDev,Mean,w,x1,x2,BoxMuller,num
    integer::i,n,seed

    !Calls Seed
    print*,'enter seed:'
    read(*,*) seed
    call srand(seed)

    !open file Bumps.txt
    open(43,file='Bumps.txt')

    !Initializing Mean and Stdev
    Mean=22.0
    StDev=2.5

    !Do loop that generates numbers and uses BoxMuller function
    do i=1,13000
      num=BoxMuller(Stdev,Mean)
      write(43,*) num
    end do

    !Do loop for 7000 numbers
    Mean=15.5
    StDev=1.0
    do i=1,7000
      num=BoxMuller(Stdev,Mean)
      write(43,*) num
    end do

    !Closing file
    close(43)
end program numbers

!Box Mueller Function
function BoxMuller(StDev,Mean) result(rNum1)
    implicit none
    real::StDev,Mean,w,x1,x2,rNum2,rNum1

    do
      x1 = 2.d0 * rand() - 1.d0
      x2 = 2.d0 * rand() - 1.d0
      w = x1**2 + x2**2
      if (w .lt. 1.d0) exit ! Valid numbers
    enddo
    w = sqrt((-2.0 * log(w))/w)
    rNum1 = x1 * w
    rNum2 = x2 * w
    rNum1 = rNum1 * StDev + Mean
end function BoxMuller
