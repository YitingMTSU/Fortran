program technique1
  implicit none
  ! In this program, I use the better algorithms
  ! For (x-1)*(x^4+x^3+x^2+x+1), we can use the easier way to solve it by x^5-1
  integer, parameter::arraySize = 6000000 ! The arraySize is big enough to see the time difference
  call worse_algorithms()
  call better_algorithms()
  

  contains
    subroutine worse_algorithms
      integer, parameter::dp = selected_real_kind(15, 307)
      real(kind = dp), dimension(arraySize)::x,y
      integer :: counts, count_rate, count_max, counte
      integer :: i
      call system_clock(counts, count_rate, count_max)
      do i = 1, arraySize
         x(i) = i * 0.2
         y(i) = (x(i) - 1) * (x(i)**4 + x(i)**3 + x(i)**2 + x(i) + 1)
      enddo
      call system_clock(counte)
      write(*,*)"worse algorithms:", counte - counts
   end subroutine worse_algorithms

   subroutine better_algorithms
     integer, parameter::dp = selected_real_kind(15, 307)
     real(kind = dp), dimension(arraySize)::x,y
     integer :: counts, count_rate, count_max, counte
     integer :: i
     call system_clock(counts, count_rate, count_max)
     do i = 1, arraySize
        x(i) = i * 0.2
        y(i) = x(i)**5 - 1
     enddo
     call system_clock(counte)
     write(*,*)"better algorithms:", counte - counts
   end subroutine better_algorithms
   
end program technique1

