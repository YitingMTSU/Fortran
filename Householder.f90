program Householder
  implicit none
  integer, parameter :: Num = 3
  real, dimension(:), allocatable :: t, y
  real, dimension(:,:), allocatable :: x
  real, dimension(:,:),allocatable :: A, R
  real :: t_whatever, y_whatever
  integer ::  i , j, m = 0, io = 0, degree   
  character(len = 30) :: filename
  real, dimension(:, :),allocatable :: bnew
  

  write(*, "(A)")'What is your file data? '
  read(*, *) filename
  write(*, '(A18,a)') 'Your data file is ', filename
  !determine which is the file


  !read(*, *) degree
  !define the degree of the function
  
  open(unit = Num, file = filename, status = "old", iostat = io)
  do while(io == 0)
     read(Num, *, iostat = io) t_whatever, y_whatever
     if (io /= 0) exit
         m = m + 1
  end do
  close (Num)
  !calculate how much rows in the data file
  print *, 'The rows in data file m = ', m
  
  allocate(t(m))
  allocate(y(m))
  allocate(bnew(m, 1))
  open(unit = Num, file = filename, status = 'old', action = 'read')
  do j = 1, m
     read(Num, *) t(j), y(j)
    ! print *, 't=', t(j), 'y=', y(j) 
  end do
  close(Num)
  !Read the data from the file
  
  write(*,*) 'what is your degree'
  read(*,*) degree
     allocate(x(degree + 1, 1))
     allocate(A(m, degree + 1))
 !  allocate(Q(m, degree + 1))
     allocate(R(m, degree + 1))
  
     do i = 1, degree + 1
        do j = 1, m
           A(j, i) = t(j)**(i - 1)
        end do
     end do
     print*, 'A = ', A

     call QRfraction(A, y, m, degree, bnew, R)
     write(*,*)"R = ", R
     write(*,*) "bnew = ", bnew
     call solution(R, degree, m, bnew, x)
 
  
end program Householder
