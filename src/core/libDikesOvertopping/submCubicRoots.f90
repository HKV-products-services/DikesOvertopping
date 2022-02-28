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

submodule (formulaModuleOvertopping) submCubicRoots
   use utils
   use OvertoppingMessages
contains

!> realRootsCubicFunction:
!! calculate the roots of a cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure realRootsCubicFunction
!***********************************************************************************************************
   implicit none
!
   double complex :: z(3)  !< complex roots cubic function
   integer        :: i     !< counter complex roots

! ==========================================================================================================

   ! calculate complex roots cubic function
   call rootsGeneralCubic (coeff, z, error)

   ! initialize (number of) real roots
   N = 0
   x = 0.0d0

   if (error%errorCode == 0) then

      ! loop over complex roots cubic function
      do i=1, 3

         ! check if imaginary part equals zero
         if (isEqualZero (imag(z(i)))) then

            ! add to real roots cubic function
            if (error%errorCode == 0) then
               N    = N+1
               x(N) = dble(z(i))
            endif

         endif

      enddo

   endif

end procedure realRootsCubicFunction

!> rootsGeneralCubic:
!! calculate the roots of a generic cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine rootsGeneralCubic (coeff, z, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in   ) :: coeff(:)       !< four coefficients cubic function
   double complex,   intent(  out) :: z(3)           !< roots cubic function
   type(tMessage),   intent(inout) :: error          !< error struct
!
!  Local parameters
!
   real(kind=wp)  :: a, b, c, d
   real(kind=wp)  :: p, q  !< coefficients depressed cubic function

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   a = coeff(1)
   b = coeff(2)
   c = coeff(3)
   d = coeff(4)

   ! check first coefficient cubic function
   if (isEqualZero (a)) error%errorCode = 1

   if (error%errorCode == 0) then

      ! substitution of z = t-b/(3a) gives: t^3 + pt + q = 0
      p = (3*a*c - b**2)/(3*(a**2))
      q = (2*(b**3) -9*a*b*c +27*(a**2)*d)/(27*(a**3))

      call rootsDepressedCubic (p, q, z)

      ! calculate roots general cubic
      z = z - b/(3*a)

   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      call GetMSGcalc_roots_cubic_function(error%Message)
   endif

   end subroutine rootsGeneralCubic

!> rootsDepressedCubic:
!! calculate the roots of a depressed cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine rootsDepressedCubic (p, q, z)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),   intent(in)  :: p     !< coefficient p depressed cubic
   real(kind=wp),   intent(in)  :: q     !< coefficient q depressed cubic
   double complex,  intent(out) :: z(3)  !< roots depressed cubic
!
!  Local parameters
!
   double complex  :: D     !< discriminant quadratic equation in w^3
   double complex  :: w3    !< one solution quadratic equation in w^3
   double complex  :: w(3)  !< cubic roots  quadratic equation in w^3

! ==========================================================================================================

   ! substitution of z = w-p/(3w) gives:  w^6 + qw^3 - (p^3)/27 = 0

   ! calculate one solution for w^3:
   D  = q**2+4*(p**3)/27
   w3 = (-q + sqrt(D))/2

   ! calculate cubic roots
   call cubicRoots (w3, w)

   if (minval(abs(w)) > 0.0d0) then
      z = w - p/(3*w)
   else
      z = 0.0d0 ! only if p=0 and q=0
   endif

   end subroutine rootsDepressedCubic

!> cubicRoots:
!! calculate the roots of a cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine cubicRoots (z, roots)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   double complex, intent(in)  :: z        !< complex number
   double complex, intent(out) :: roots(3) !< cubic roots
!
!  Local parameters
!
   double complex, parameter   :: i = cmplx(0.0d0,1.0d0)   !< complex number i
   real(kind=wp)               :: arg                      !< argument complex number

! ==========================================================================================================

   ! check radius of z
   if (isEqualZero (abs(z))) then

      ! z equals zero
      roots = 0.0d0

   else

      ! calculate argument z
      arg = atan2(imag(z),dble(z))

      ! calculate cubic roots z
      roots(1) = (abs(z)**(1/3.0d0)) * exp(i* arg/3)
      roots(2) = (abs(z)**(1/3.0d0)) * exp(i*(arg+2*pi)/3)
      roots(3) = (abs(z)**(1/3.0d0)) * exp(i*(arg-2*pi)/3)

   endif

   end subroutine cubicRoots

end submodule submCubicRoots
