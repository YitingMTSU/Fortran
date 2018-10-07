program technique2
  implicit none
  ! In this program, I use the techniques that unrolling loops 
  ! I want to calculate the sum of 1^i , 2^i and 3^i
  integer, parameter::arraySize = 6000 ! The arraySize is big enough to see the time difference
  call original_loop()
  call unrolling_loop()
  

  contains
    subroutine original_loop
      integer, parameter::dp = selected_real_kind(15, 307)
      real(kind = dp), dimension(3)::A 
      integer :: counts, count_rate, count_max, counte
      integer :: i,j,k !k is how many times it do to see the difference
      call system_clock(counts, count_rate, count_max)
      do k = 1, 5000 
      do i = 1, 3
         do j = 1, arraySize
            A(i) = A(i) + i**j
         enddo
      enddo
      enddo
      call system_clock(counte)
      write(*,*)"original_loop:", counte - counts
   end subroutine original_loop

   subroutine unrolling_loop
     integer, parameter::dp = selected_real_kind(15, 307)
     real(kind = dp)::A1, A2, A3
     integer :: counts, count_rate, count_max, counte
     integer :: j, k
     call system_clock(counts, count_rate, count_max)
     do k = 1, 5000
     do j = 1, arraySize
        A1 = A1 + 1**j
        A2 = A2 + 2**j
        A3 = A3 + 3**j
     enddo
     enddo
     call system_clock(counte)
     write(*,*)"better algorithms:", counte - counts
   end subroutine unrolling_loop
   
end program technique2

