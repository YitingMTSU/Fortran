subroutine solution(R, degree, m, bnew, x)
  integer :: degree, m, i, j
  real, dimension(degree + 1, 1) :: x
  real, dimension(m, degree + 1) :: R
  real, dimension(m, 1) :: bnew
  real :: ak
  if (degree == 0) then
     x(degree + 1, 1) = bnew(degree + 1, 1) / R(degree + 1, degree + 1)
  else
     x(degree + 1, 1) = bnew(degree + 1, 1) / R(degree + 1, degree + 1)
     do i = degree + 1 , 1, -1
    !    print *, 'i = ', i
        ak = 0
        do j = i + 1, degree + 1
           ak = ak + R(i, j) * x(j, 1)
        end do
        x(i, 1) =(bnew(i, 1) - ak) / R(i, i)
     end do
  end if
  print*, 'x = ', x
end subroutine solution
  
