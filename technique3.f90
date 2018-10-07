program technique3
  implicit none
  ! In this program, I use the techniques that eliminating the branches in loop 
  ! I want to calculate the create the matrix Aij = i^j if i<3000  and Aij = j^i if i>=3000
  integer, parameter::arraySize = 6000 ! The arraySize is big enough to see the time difference
  call original_loop()
  call eliminating_branches()
  

  contains
    subroutine original_loop
      integer, parameter::dp = selected_real_kind(15, 307)
      real(kind = dp), dimension(arraySize,arraySize)::A 
      integer :: counts, count_rate, count_max, counte
      integer :: i,j
      call system_clock(counts, count_rate, count_max) 
      do i = 1, arraySize
         do j = 1, arraySize
            if (i < 3000) then
               A(i,j) = i**j
            else
               A(i,j) = j**i
            endif
         enddo
      enddo
      call system_clock(counte)
      write(*,*)"original loop:", counte - counts
   end subroutine original_loop

   subroutine eliminating_branches
     integer, parameter::dp = selected_real_kind(15, 307)
     real(kind = dp), dimension(arraySize,arraySize)::A
     integer :: counts, count_rate, count_max, counte
     integer :: i, j
     call system_clock(counts, count_rate, count_max)
     do i = 1, 2999
        do j = 1, arraySize
           A(i,j) = i**j
        enddo
     enddo
     do i = 3000, arraySize
        do j = 1, arraySize
           A(i,j) = j**i
        enddo
     enddo
     call system_clock(counte)
     write(*,*)"eliminating branches:", counte - counts
   end subroutine eliminating_branches
   
end program technique3

