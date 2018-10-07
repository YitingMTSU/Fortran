subroutine QRfraction(A ,y, m, degree, bnew, R)
  integer:: m, degree
  real, dimension(m, degree + 1) :: A, Q, R
  real, dimension(m) :: y,v1
  real, dimension(m, 1) :: v, bnew
  real, dimension(m, m) :: II, H, vvt
  !real, dimension(m, 1) :: bnew
  integer :: i, j, k
  real :: ak, bk, ri, vtv
  II = 0 
  do j = 1, m
     II(j, j) = 1
 end do
 bnew(:,1) = y    
 do k = 1, degree + 1    
    do j = 1, m
       v(j,1) = A(j, k)
    end do
    do j = 1, k - 1
        v(j,1) = 0
     end do
     ak = sign(1.0,v(k,1)) * norm2(v)
     v(k,1) = v(k,1) +  ak * 1
     vvt = matmul(v,transpose(v))
     v1 = v(:,1)
     vtv = dot_product(v1,v1)
     H = II - 2 * vvt / vtv
     !       do i = k, degree + 1
     !         ri = 0
     !        do j = 1, m
     !          ri = ri + v(j) * A(j, i))
     !      end do
     A = matmul(H, A)
     bnew = matmul(H , bnew)
  end do
  
  R = A(1 : degree+1, 1 : degree+1)
end subroutine QRfraction
