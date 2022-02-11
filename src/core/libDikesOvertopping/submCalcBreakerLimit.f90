submodule (formulaModuleOvertopping) submCalcBreakerLimit
   use parametersOvertopping
contains

!> calculateBreakerLimit:
!! calculate the breaker limit
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateBreakerLimit
!***********************************************************************************************************
   implicit none
!
   real(kind=wp) :: a           !< coefficients cubic function
   real(kind=wp) :: b           !< coefficients cubic function
   real(kind=wp) :: c           !< coefficients cubic function
   real(kind=wp) :: d           !< coefficients cubic function
   integer  :: N                !< number of real roots cubic function
   real(kind=wp) :: x(3)        !< real roots cubic function
   integer  :: i                !< counter real roots cubic function
   integer  :: M                !< number of possible limit values breaker parameter
   real(kind=wp) :: ksi0(2)     !< possible limit values breaker parameter

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0
   if (gammaB == 1.0_wp) then
       ksi0limit = 1.73383901115961_wp
       return
   end if

   ! fRunup1 * gammaB * ksi0 = fRunup2 - fRunup3/sqrt(ksi0)
   !
   ! substitution of x = sqrt(ksi0) gives:
   !
   ! fRunup1 * gammaB * x^3 - fRunup2 * x + fRunup3 = 0
   
   ! set the coefficients of the cubic function
   a =  fRunup1 * gammaB
   b =  0.0d0
   c = -fRunup2
   d =  fRunup3

   ! calculate the real roots of the cubic function
   call realRootsCubicFunction (a, b, c, d, N, x, error)

   if (error%errorCode == 0) then

      ! calculate ksi0 from positive solutions
      M = 0
      do i=1, N
         if (x(i) > 0.0d0) then
            M       = M+1
            ksi0(M) = x(i)**2
         endif
      enddo

      ! check the number of solutions
      if (M == 0) then
         ksi0Limit = 0.0d0
      else
         ksi0Limit = maxval(ksi0)
      endif

   endif

end procedure calculateBreakerLimit
!
end submodule submCalcBreakerLimit
