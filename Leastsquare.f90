program LeastSquare
  implicit none
  integer, parameter :: Num = 3
  real, dimension(:), allocatable :: t, y
  real :: t_whatever, y_whatever, S1 = 0,S2 = 0,S3 = 0,S4 = 0, x1, x2
  integer ::  i , j, m = 0, io   
  character(len = 30) :: filename

  write(*,"(A)")'What is your file data? '
  read(*,*) filename
  write(*,'(A18,a)') 'Your data file is ', filename
  !read data from the file
  
  open(unit = Num, file = filename)
  do
     read(Num, *, iostat = io) t_whatever, y_whatever
     if (io /= 0) exit
         m = m + 1
  end do
  close (Num)
  print *, 'm = ', m
  allocate(t(m))
  allocate(y(m))
  open(unit = Num, file = filename, status = 'old', action = 'read')
  do j = 1, m
     read(Num, *) t(j), y(j)
    ! print *, 't=', t(j), 'y=', y(j) 
  end do
  close(Num)
  do i = 1, m
     S1=S1+t(i)
     S2=S2+t(i)**2
     S3=S3+y(i)
     S4=S4+t(i)*y(i)
  end do
  x2 = (S3*S1/m -S4)/(S1**2/m - S2)
  x1 = 1/m *(S3-S1*x2)
  print*, 'S1',S1,'S2=',S2,'S3=',S3,'S4=',S4
  print*,'x1=',x1
  print*,'x2=',x2
   
end program LeastSquare
