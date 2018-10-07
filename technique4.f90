program technique4
  implicit none
  ! In this program, I use the techniques that stop testing when you know the answer 
  ! I want to find the target in a large matrix 
  integer, parameter::arraySize = 6000 ! The arraySize is big enough to see the time difference
    
  call original_loop()
  call stop_testing()
  

  contains
    subroutine original_loop
      integer, parameter::dp = selected_real_kind(15, 307)
      real(kind = dp), dimension(arraySize,arraySize)::A 
      integer :: counts, count_rate, count_max, counte
      integer :: i,j
      logical :: found = .false.
      real :: Target_value = 4000
      !given the matrix A
      do i = 1, arraySize
         do j = 1, arraySize
            A(i,j) = i ** j
         enddo
      enddo
      
      call system_clock(counts, count_rate, count_max) 
      do i = 1, arraySize
         do j = 1, arraySize
            if (A(i,j) == Target_value) then
               found = .true.
            endif
         enddo
      enddo

      call system_clock(counte)
      write(*,*)"original loop:", counte - counts
   end subroutine original_loop

   subroutine stop_testing
     integer :: counts, count_rate, count_max, counte
     integer :: i, j
     integer, parameter::dp = selected_real_kind(15, 307)
     real(kind = dp), dimension(arraySize,arraySize)::A
     logical :: found = .false.
     real :: Target_value = 4000
     !given the matrix A
     do i = 1, arraySize
        do j = 1, arraySize
           A(i,j) = i ** j
        enddo
     enddo
          
     call system_clock(counts, count_rate, count_max)
     do i = 1, arraySize
        j = 1
        do while(j <= arraySize .and. .not. found)
           if (A(i,j) == Target_value) then
              found = .true.
           endif
           j = j + 1
        enddo
     enddo
     
     call system_clock(counte)
     write(*,*)"stop testing:", counte - counts
   end subroutine stop_testing
   
end program technique4

