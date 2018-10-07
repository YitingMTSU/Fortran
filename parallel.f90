program parallel
  ! COMS 6100 Final Project
  ! Steady State Temperature Distribution Problem
  ! Due Thursday, December 8, 2016 @ 10:00 AM
  ! Code for Parallel Method
  ! Yiting Wang
  ! M01360917

implicit none

integer, parameter :: num=500
integer :: i, j, k=0
integer :: counts=0, flag=1
integer :: CountStart, CountRate, CountMax,CountEnd, HeatingNum
real :: time, MeanTemp
real,dimension(num, num) :: temp, NextTemp
character(len = 4) :: filename

! call the starting time from system clock
call system_clock(CountStart, CountRate, CountMax)
!room temperature
temp = 20.0
!heating elements temperature
temp(1,:) = 200.0
temp(:, [1, num]) = 200.0
temp(171, [171, 343]) = 200.0
temp(343, [171, 343]) = 200.0
!next time heating elements temperature for boundaries except bottom
NextTemp(num, :) = 20.0
NextTemp(1, :) = 200.0
NextTemp(:, [1, num]) = 200.0
!use formula to calculate the temperature for inside points
do while(flag == 1)
   !$OMP PARALLEL DO
   do i = 2, num-1
      do j = 2, num-1
         NextTemp(i, j)=(temp(i + 1, j) + temp(i - 1, j) + temp(i, j + 1)+temp(i, j - 1) + 4 * temp(i, j)) / 8.0
      enddo
   enddo
   !OMP END PARALLEL DO
   !set the heating points to 200 degrees
   NextTemp(171, [171, 343]) = 200.0
   NextTemp(343, [171, 343]) = 200.0
   !if the difference is small enough, stop the loop
   if(maxval(abs(NextTemp-temp)) < 0.01) then
      flag = 0
   endif

   temp = NextTemp
   counts = counts + 1
enddo

!calcuate the average temperature for the non-heating cells
HeatingNum = 500 * 2 + 498 + 4
MeanTemp = (sum(temp) - 200.0 * HeatingNum) / (500 * 500 - HeatingNum)

! get the end time from system clock
call system_clock(CountEnd)

! calculate the time difference
time = real(CountEnd - CountStart) / real(CountRate)

write (*,*), "Time spent on the parallel code is", time, "counts:", counts
write(*,*),"The average temperature for the non-heating cells is: ", MeanTemp

end program parallel
