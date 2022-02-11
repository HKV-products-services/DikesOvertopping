! Copyright (C) Stichting Deltares 2022. All rights reserved.
!
! This file is part of the Dikes Overtopping Kernel.
!
! The Dikes Overtopping Kernel is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with this program. If not, see <http://www.gnu.org/licenses/>.
!
! All names, logos, and references to "Deltares" are registered trademarks of
! Stichting Deltares and remain full property of Stichting Deltares at all times.
! All rights reserved.
!

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
