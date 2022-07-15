program his
    !Cariza Zulueta
    !CDS 251
    !Assignment 10
    !November 8,2021

    !This program creates a histogram to a file
    !This program has a histogram range of 10 to 30 with 100 boxes

    implicit none

    !Declaring Variables
    real::Histart,histend,HistRange,boxwidth,x,num
    integer::i,nBoxes,BadCount,iHist
    integer,allocatable::Hist(:)

    !Opening file and asks for user input
    open(43,file='Bumps.txt')
    print*,'Enter Histart:'
    read(*,*)Histart
    print*,'Enter Histend'
    read(*,*)Histend
    HistRange=HistEnd - HiStart
    print*,'Enter nboxes'
    read(*,*)nBoxes
    BoxWidth = HistRange / nBoxes

    !Allocating array
    allocate(Hist(nBoxes))

    !Initializing vairables
    BadCount=0.0
    Hist=0.0

    !Do loop reads in file and collects histogram data
    do i=1,20000
    read(43,*) num
    iHist = Int((num-Histart) * float(NBoxes)/HistRange)
    if (iHist .ge. 1 .and. iHist .le. nBoxes) then
      Hist(iHist) = Hist(iHist) + 1
    else
      BadCount = BadCount + 1
    endif
    enddo

    !Do loop for plotting histogram x and y coordinates
    open(44,file='BumpsHist.txt')
    do i = 1, nBoxes
      x = (float(i) - 0.5) * BoxWidth + Histart
      write(44,*)x,Hist(i)
    end do

    !Closing files and deallocating array
    close(43)
    close(44)
    deallocate(Hist)

end program his
