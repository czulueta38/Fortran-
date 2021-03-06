program qsort
    !Cariza Zulueta
    !CDS 251
    !Assignment 6
    !October 7,2021

    !This program asks the user to type in the file name and reads in Data
    !This program calls quicksort to the array
    !This program writes sorted array to Output file
    !This program implements a recursive subroutine
    !This program implements a non recursive partition function

    implicit none

    !declaring variables
    character*50 :: filein
    real, allocatable::A(:)
    integer:: n,lo,hi,p,i
    real::Pivot

    !Reading in files and allocating for array
    print*,'input file:'
    read(*,*) filein
    open(42,file=filein)
    read(42,*) n
    allocate(a(n))

    !Reading array
    do i=1,n
      read(42,*) a(i)
    end do

    !sorting the whole array shows what is hi and what is lo
    lo=1
    hi=n

    !Calling subroutine
    call quicksort(a,n,lo,hi)

    !creates output file
    open(43,file='HW6Out.txt') 

    !accessing data through a(i)
    do i=1,n
          write(43,*) a(i)
    end do

    !deallocating array and closing files
    deallocate(a)
    close(42)
    close(43)

end program qsort

recursive subroutine quicksort(a,n,lo,hi)

    implicit none

    !declaring variables
    integer::p
    real, dimension(n)::A
    integer:: hi, lo
    integer::n, Partition

    !implements partition function
    if (lo < hi) then
       p=Partition(a,n,lo,hi)
       call quicksort(a,n,lo,p-1)
       call quicksort(a,n,p+1,hi)
    end if

end subroutine quicksort

function Partition(A, n, lo, hi) result(P)

    implicit none

    !declaring variables
    real, dimension(n)::A
    integer::hi, lo , i, n, P
    real::Pivot,Temp

    !setting values
    Pivot = A(hi)
    P = lo

    !swapping values and creating quicksort
    do i = lo, hi - 1
       if (A(i) .le. Pivot) then
       Temp=A(I)
       A(I)=A(P)
       A(P)=Temp
       P=P+1
       endif
    enddo
      Temp=A(hi)
      A(hi)=A(P)
      A(P)=Temp

end function Partition
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
