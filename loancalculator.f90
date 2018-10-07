program loancalculator
  implicit none
  integer, parameter:: rkind = selected_real_kind(15,307)
  real(kind=rkind) :: Monthly_payment, Initial_Balance, Interest_rate, Interest, Balance, Principal, Total_Payments, Pre_Interest
  real(kind=rkind) :: Total_Interest 
  integer :: Years, period, io = 0, counter = 1, i
  integer, parameter :: Num = 12
  character(len=45) :: filename

  !remind the user to put the right file
  print *, '##########################################################'
  print *, '# Your loan terms should in the following format         #'
  print *, '# Initial_Loan1  Number_of_years1  Annual_interest_rate1 #'
  print *, '# Initial_Loan2  Number_of_years2  Annual_interest_rate2 #'
  print *, '# Initial_Loan3  Number_of_years3  Annual_interest_rate3 #'
  print *, '# ...                                                    #'
  print *, '##########################################################'
  
  write(*,'(A44)')'Please enter the name of a loan terms file: '
  read (*,*) filename
  write(*,'(A12,a)')'You entered ', filename
  !read data from loan file
  open(unit = Num, file = filename, status = 'OLD', iostat = io)
  do while (io == 0)
     read(Num, *, end =100) Initial_Balance, Years, Interest_rate
     write(*,'(A4, 1X, A1, I1)') 'Loan', '#', counter 
     write(*,' (A16, 1X, A1, f11.2)') 'Initial Balance:', '$', Initial_Balance
     write(*,'(A16, 2X, I11 )')'Years to pay:', Years
     write(*,'(A16, 2X, f11.3, A1)')'Interest Rate:', Interest_rate, '%'
     write(*,*)
     period = Years * 12
     Interest_rate = Interest_rate / 100 / 12
     Monthly_payment = Initial_Balance * Interest_rate *(1 + Interest_rate)**period / ((1 + Interest_rate)**period - 1)
     write(*,'(A16, 1X, A1, f11.2)') 'Monthly Payment:', '$', Monthly_payment
     write(*,*)
     Balance = Initial_Balance
     Total_Interest = 0
     Total_Payments = 0
     Pre_Interest = 0
     write(*,'(A41)') 'Period   Principal   Interest     Balance'
     do i = 1, period
        Interest = Balance * Interest_rate
        Principal = Monthly_payment - Interest
        Balance = Balance - Principal
        call cal_total_interest(Pre_Interest, Interest, Total_Interest)
        if ((i <= 12) .or. (i>= period - 11)) then
           write(*,'(I6,3X,f9.2,3X,f8.2, 3X,f9.2 )') i, Principal, Interest, Balance
        end if
        if (i == 12) then
           write(*,*)
        endif
        Pre_Interest = Total_Interest
     end do
     write(*,*)
     Total_Payments = total_payment(Monthly_payment,period)
     write(*, '(A16, 1X, A1, f12.2)') 'Total Interest:', '$', Total_Interest
     write(*, '(A16, 1X, A1, F12.2)') 'Total Payments:', '$', Total_Payments
     print *, '  '
     counter = counter + 1
  end do
100 close (Num)

contains
  ! calculate the total interest
  subroutine cal_total_interest(pre_interest, interest, total_interest)
    integer, parameter :: rkind = selected_real_kind(15,307)
    real(kind = rkind) :: pre_interest, interest, total_interest

    total_interest = pre_interest + interest
  end subroutine cal_total_interest

  
  ! calculate the total payments
  real(kind = rkind) function total_payment(Monthly_payment, period)
    integer, parameter :: rkind = selected_real_kind(15,307)
    real(kind = rkind) :: Monthly_payment
    integer :: period
    total_payment = Monthly_payment * period
   end function total_payment

end program loancalculator
  
  
