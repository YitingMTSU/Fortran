program serial
  ! COMS 6100 Final Project
  ! Steady State Temperature Distribution Problem
  ! Due Thursday, December 8, 2016 @ 10:00 AM
  ! Code for Serial Method
  ! Yiting Wang
  ! M01360917

implicit none

integer, parameter :: num = 500 ! plate size
integer :: i, j, k = 0 !control number for loops
integer :: counts = 0, flag = 1 !counts is the number to calculate the steps, and flag is a signal to control the while loop 
integer :: CountStart, CountRate, CountMax, CountEnd, HeatingNum
!time calculation
real :: time, MeanTemp 
real, dimension(num, num) :: temp, NextTemp !temprature for this time and next time 
character(len = 4) :: filename ! output the filename

! get starting time from system clock
call system_clock(CountStart, CountRate, CountMax)

temp = 20.0 ! Room temperature
! heating elements temperature for special points
temp(1, :) = 200.0 
temp(:, [1, num]) = 200.0 
temp(171, [171, 343]) = 200.0
temp(343, [171, 343]) = 200.0
! next time the boundaries points are 200 degree except the bottom
NextTemp(num, :) = 20.0
NextTemp(1, :) = 200.0
NextTemp(:, [1, num]) = 200.0

! use the formula to calculate the temperature for the inside points 
do while(flag == 1)
   do i = 2, num - 1
      do j = 2, num - 1
         NextTemp(i, j) = (temp(i + 1, j) + temp(i - 1, j) + temp(i, j + 1)+temp(i, j - 1) + 4 * temp(i, j)) / 8.0
      enddo
   enddo
! make the heating points still be 200 degrees
   NextTemp(171, [171, 343]) = 200.0
   NextTemp(343, [171, 343]) = 200.0
! judge if the difference between this temperature and next temperature is small enough stop the loop
   if (maxval(abs(NextTemp - temp)) < 0.01) then
      flag = 0
   endif

   ! output data every 100 steps
   ! uncomment this when you need to output data
   if(mod(counts,100) == 0) then
      ! output the file
      write(filename, '(i4.4)') k
      open(unit = 1, file = 'hotPlate'//trim(adjustl(filename)//'.csv'))
      do i = 1, num
         do j=1 , num
            write(1, *) [real(j), real(i), temp(num - i + 1, num - j + 1)]
         enddo
         !need blank line
         write(1, *)
      enddo
      close(unit = 1)
      k = k + 1
   endif

   temp = NextTemp
   counts = counts + 1
enddo


! calcuate the average temperature for the non-heating cells
HeatingNum = 500 * 2 + 498 + 4
MeanTemp = (sum(temp) - 200.0 * HeatingNum)/(500 * 500 - HeatingNum)
! call the end time from system clock
call system_clock(CountEnd)
! find the time difference
time = real(CountEnd - CountStart)/real(CountRate)

write (*,*), "Time spent on the serial code is:", time, "counts:", counts
write(*,*),"The average temperature for the non-heating cells is: ", MeanTemp

end program serial
