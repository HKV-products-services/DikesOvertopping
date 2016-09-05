! Copyright (C) Stichting Deltares 2016. All rights reserved.
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

!> @file
!! This file contains a module with the core computations for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants
!              Edwin Spee, Deltares
!
!  $Id$
!
!***********************************************************************************************************
!
!> the core computations for Dikes Overtopping
!
   module formulaModuleOvertopping
!***********************************************************************************************************
!
   use typeDefinitionsOvertopping
   use OvertoppingMessages

   implicit none

   private
   
   public ::  calculateWaveRunup, calculateWaveLength, calculateWaveSteepness
   public ::  calculateBreakerParameter, calculateAngleWaveAttack, calculateBreakerLimit, adjustInfluenceFactors
   public ::  calculateWaveOvertoppingDischarge
   public ::  isEqualReal, isEqualZero

   contains
!
!> calculateWaveRunup:
!! calculate wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateWaveRunup (Hm0, ksi0, ksi0Limit, gammaB, gammaF, gammaBeta, modelFactors, z2, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),             intent(in)     :: Hm0            !< significant wave height (m)
   real(kind=wp),             intent(in)     :: ksi0           !< breaker parameter
   real(kind=wp),             intent(in)     :: ksi0Limit      !< limit value breaker parameter
   real(kind=wp),             intent(inout)  :: gammaB         !< influence factor berms
   real(kind=wp),             intent(inout)  :: gammaF         !< influence factor roughness
   real(kind=wp),             intent(inout)  :: gammaBeta      !< influence factor angle of wave attack
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   real(kind=wp),             intent(out)    :: z2             !< 2% wave run-up (m)
   logical,                   intent(out)    :: succes         !< flag for succes
   character(len=*),          intent(out)    :: errorMessage   !< error message

! ==========================================================================================================

   ! if applicable adjust influence factors
   call adjustInfluenceFactors (gammaB, gammaF, gammaBeta, 1, ksi0, ksi0Limit, succes, errorMessage)

   if (succes) then

      ! calculate 2% wave run-up for small breaker parameters
      if (ksi0 < ksi0Limit) then
         z2 = Hm0 * fRunup1 * gammaB * gammaF * gammaBeta * ksi0

      ! calculate 2% wave run-up for large breaker parameters
      else if (ksi0 > 0.0d0) then
         z2 = Hm0 * gammaF * gammaBeta * (fRunup2 - fRunup3/sqrt(ksi0))
         z2 = max(z2, 0.0d0)
      else
         errorMessage = GetOvertoppingMessage(breaker_param_is_zero)
         succes = .false.
         z2 = 0d0
      endif
      z2 = z2 * modelFactors%m_z2

   endif

   end subroutine calculateWaveRunup

!> calculateWaveOvertoppingDischarge:
!! calculate the wave overtopping discharge
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateWaveOvertoppingDischarge (h, Hm0, tanAlpha, gammaB, gammaF, gammaBeta, ksi0, &
                                                 hCrest, modelFactors, Qo, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),            intent(in)  :: h              !< local water level (m+NAP)
   real(kind=wp),            intent(in)  :: Hm0            !< significant wave height (m)
   real(kind=wp),            intent(in)  :: tanAlpha       !< representative slope angle
   real(kind=wp),            intent(in)  :: gammaB         !< influence factor berms
   real(kind=wp),            intent(in)  :: gammaF         !< influence factor roughness
   real(kind=wp),            intent(in)  :: gammaBeta      !< influence factor angle of wave attack
   real(kind=wp),            intent(in)  :: ksi0           !< breaker parameter
   real(kind=wp),            intent(in)  :: hCrest         !< crest level (m+NAP)
   type(tpOvertoppingInput), intent(in)  :: modelFactors   !< structure with model factors
   real(kind=wp),            intent(out) :: Qo             !< wave overtopping discharge (l/m per s)
   logical,                  intent(out) :: succes         !< flag for succes
   character(len=*),         intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   real(kind=wp)  :: Qb !< dimensionless  overtopping discharge for breaking waves
   real(kind=wp)  :: Qn !< dimensionless  overtopping discharge for non-breaking waves
   real(kind=wp)  :: Qs !< dimensionless  overtopping discharge for shallow waves

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! check input parameters calculation wave overtopping discharge
   if (succes) succes = (Hm0            > 0.0d0)
   if (succes) succes = (tanAlpha       > 0.0d0)
   if (succes) succes = (gammaB         > 0.0d0)
   if (succes) succes = (gammaF         > 0.0d0)
   if (succes) succes = (gammaBeta      > 0.0d0)
   if (succes) succes = (ksi0           > 0.0d0)

   ! calculate dimensionless overtopping discharges
   if (succes) then
   
      ! breaking waves
      Qb = (0.067d0/sqrt(tanAlpha)) * gammaB * ksi0 * &
               exp(-modelFactors%factorDeterminationQ_b_f_b * ((hCrest-h)/Hm0) * (1/(ksi0 * gammaBeta * gammaB * gammaF)))
   
      ! non-breaking waves
      Qn = 0.2d0 * exp(-modelFactors%factorDeterminationQ_b_f_n * ((hCrest-h)/Hm0) * (1/(gammaBeta * gammaF)))

      ! shallow waves
      Qs = (10.0d0 ** -modelFactors%fshallow) * exp(-(hCrest-h) / (gammaBeta * gammaF * Hm0 * (0.33d0+0.022d0*max(ksi0,7.0d0))))

   endif

   ! calculate wave overtopping discharge
   if (succes) then
   
      ! check value breaker parameter
      if (ksi0 > 5.0d0) then

         if (ksi0 < 7.0d0) then

            ! 5 < breaker parameter < 7
            if (Qn > 0d0 .and. Qs > 0d0) then
               Qo = max(Qn, exp(log(Qn) + (log(Qs)-log(Qn))*(ksi0-5.0d0)/2)) * sqrt(gravityConstant * Hm0**3)
            else
               Qo = 0d0
            endif

         else

            ! breaker parameter >= 7
            Qo = Qs * sqrt(gravityConstant * Hm0**3)

         endif

      else

         ! 0 < breaker parameter <= 5
         Qo = min(Qb,Qn) * sqrt(gravityConstant * Hm0**3)

      endif

   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = GetOvertoppingMessage(calc_wave_overtopping_discharge)
   endif

   end subroutine calculateWaveOvertoppingDischarge

!> calculateWaveLength:
!! calculate the wave length
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateWaveLength (Tm_10, L0)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: Tm_10 !< spectral wave period (s)
   real(kind=wp), intent(out) :: L0    !< wave length (m)

! ==========================================================================================================

   ! calculate the wave length
   L0 = gravityConstant * (Tm_10**2) / (2*pi)

   end subroutine calculateWaveLength

!> calculateWaveSteepness:
!! calculate the wave steepness
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateWaveSteepness (Hm0, Tm_10, s0, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)  :: Hm0            !< significant wave height (m)
   real(kind=wp),    intent(in)  :: Tm_10          !< spectral wave period (s)
   real(kind=wp),    intent(out) :: s0             !< wave steepness
   logical,          intent(out) :: succes         !< flag for succes
   character(len=*), intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   real(kind=wp)  :: L0 !< wave length (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! calculate the wave length
   call calculateWaveLength (Tm_10, L0)

   ! calculate the wave steepness
   if (succes) then
      if (L0 > 0.0d0) then
         s0 = Hm0/L0
      else
         succes = .false.
         errorMessage = GetOvertoppingMessage(calc_wave_steepness_period_is_zero)
      endif
   endif

   end subroutine calculateWaveSteepness

!> calculateBreakerParameter:
!! calculate the breaker parameter
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateBreakerParameter (tanAlpha, s0, ksi0, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)  :: tanAlpha       !< representative slope angle
   real(kind=wp),    intent(in)  :: s0             !< wave steepness
   real(kind=wp),    intent(out) :: ksi0           !< breaker parameter
   logical,          intent(out) :: succes         !< flag for succes
   character(len=*), intent(out) :: errorMessage   !< error message

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! calculate the breaker parameter
   if (s0 > 0.0d0) then
      if (sqrt(s0) > 0.0d0) then
         ksi0 = tanAlpha/sqrt(s0)
      else
         succes = .false.
      endif
   else
      succes = .false.
   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = GetOvertoppingMessage(calc_breaker_param_steepness_is_zero)
   endif

   end subroutine calculateBreakerParameter

!> calculateAngleWaveAttack:
!! calculate the angle of wave attack
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateAngleWaveAttack (phi, psi, beta)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: phi   !< wave direction (degree)
   real(kind=wp), intent(in)  :: psi   !< dike normal (degree)
   real(kind=wp), intent(out) :: beta  !< angle of wave attack (degree)

! ==========================================================================================================

   ! calculate angle of wave attack
   beta = abs (phi - psi)
   if (beta > 180.0d0) beta = 360.0d0 - beta

   end subroutine calculateAngleWaveAttack

!> calculateBreakerLimit:
!! calculate the breaker limit
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateBreakerLimit (gammaB, ksi0Limit, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),             intent(in)  :: gammaB         ! influence factor for berms
   real(kind=wp),             intent(out) :: ksi0Limit      ! limit value breaker parameter
   logical,                   intent(out) :: succes         ! flag for succes
   character(len=*),          intent(out) :: errorMessage   ! error message
!
!  Local parameters
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
   succes = .true.
   errorMessage = ' '

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
   call realRootsCubicFunction (a, b, c, d, N, x, succes, errorMessage)

   if (succes) then

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

   end subroutine calculateBreakerLimit

!> adjustInfluenceFactors:
!! adjust the influence factors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine adjustInfluenceFactors (gammaB, gammaF, gammaBeta, gammaBetaType, ksi0, ksi0Limit, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(inout)  :: gammaB         !< influence factor berms
   real(kind=wp),    intent(inout)  :: gammaF         !< influence factor roughness
   real(kind=wp),    intent(inout)  :: gammaBeta      !< influence factor angle of wave attack
   integer,          intent(in)     :: gammaBetaType  !< type influence factor angle of wave attack: 1 = wave run-up, 2 = overtopping
   real(kind=wp),    intent(in)     :: ksi0           !< breaker parameter
   real(kind=wp),    intent(in)     :: ksi0Limit      !< limit value breaker parameter
   logical,          intent(out)    :: succes         !< flag for succes
   character(len=*), intent(out)    :: errorMessage   !< error message
!
!  Local parameters
!
   real(kind=wp)  :: gammaB_min     !< minimal value influence factor berms
   real(kind=wp)  :: gammaF_min     !< minimal value influence factor roughness
   real(kind=wp)  :: gammaBeta_min  !< minimal value influence factor angle of wave attack
   real(kind=wp)  :: gammaT         !< total influence factor
   real(kind=wp)  :: ratioB         !< ratio influence factor berms
   real(kind=wp)  :: ratioF         !< ratio influence factor roughness
   real(kind=wp)  :: ratioBeta      !< ratio influence factor angle of wave attack
   real(kind=wp)  :: ratioT         !< sum ratios influence factors

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! initialize the minimal values for the influence factors
   gammaB_min = 0.6d0
   gammaF_min = rFactor_min

   select case (gammaBetaType)
      case (1)
         gammaBeta_min = 0.824d0 ! wave run-up
      case (2)
         gammaBeta_min = 0.736d0 ! overtopping
      case default
         succes = .false.
   end select

   ! calculate the total influence factor
   if (succes) then
      if (ksi0 < ksi0Limit) then
         gammaT = gammaB * gammaF * gammaBeta
      else
         gammaT = gammaF * gammaBeta
      endif

      ! check if the total influence factor is greather than zero
      succes = (gammaT > 0.0d0)
   endif

   ! adjust the influence factors if the total influence is less than 0.4
   if (succes) then
      if (gammaT < 0.4d0) then

         ! calculate the ratios of the influence factors in relation to their minimum
         ratioB    = (1-gammaB)    / (1-gammaB_min)
         ratioF    = (1-gammaF)    / (1-gammaF_min)
         ratioBeta = (1-gammaBeta) / (1-gammaBeta_min)

         ! check the ratio values
         if ((ratioB    < 0.0d0) .or. (ratioB    > 1.0d0)) succes = .false.
         if ((ratioF    < 0.0d0) .or. (ratioF    > 1.0d0)) succes = .false.
         if ((ratioBeta < 0.0d0) .or. (ratioBeta > 1.0d0)) succes = .false.

         if (succes) then
         
            ! calculate the sum of the ratios
            ratioT = ratioB + ratioF + ratioBeta

            ! adjust the influence factors
            gammaB    = gammaB    * exp((ratioB   /ratioT) * log(0.4d0/gammaT))
            gammaF    = gammaF    * exp((ratioF   /ratioT) * log(0.4d0/gammaT))
            gammaBeta = gammaBeta * exp((ratioBeta/ratioT) * log(0.4d0/gammaT))
            
         endif         

      endif
   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = GetOvertoppingMessage(calc_influence_factors)
   endif

   end subroutine adjustInfluenceFactors

!> realRootsCubicFunction:
!! calculate the roots of a cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine realRootsCubicFunction (a, b, c, d, N, x, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)  :: a              !< coefficient a cubic function
   real(kind=wp),    intent(in)  :: b              !< coefficient b cubic function
   real(kind=wp),    intent(in)  :: c              !< coefficient c cubic function
   real(kind=wp),    intent(in)  :: d              !< coefficient d cubic function
   integer,          intent(out) :: N              !< number of real roots cubic function
   real(kind=wp),    intent(out) :: x(3)           !< real roots cubic function
   logical,          intent(out) :: succes         !< flag for succes
   character(len=*), intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   double complex :: z(3)  !< complex roots cubic function
   integer        :: i     !< counter complex roots

! ==========================================================================================================

   ! calculate complex roots cubic function
   call rootsGeneralCubic (a, b, c, d, z, succes, errorMessage)

   ! initialize (number of) real roots
   N = 0
   x = 0.0d0

   if (succes) then
      
      ! loop over complex roots cubic function
      do i=1, 3

         ! check if imaginary part equals zero
         if (isEqualZero (imag(z(i)))) then

            ! add to real roots cubic function
            if (succes) then
               N    = N+1
               x(N) = dble(z(i))
            endif

         endif

      enddo

   endif

   end subroutine realRootsCubicFunction

!> rootsGeneralCubic:
!! calculate the roots of a generic cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine rootsGeneralCubic (a, b, c, d, z, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)  :: a              !< coefficients a cubic function
   real(kind=wp),    intent(in)  :: b              !< coefficients b cubic function
   real(kind=wp),    intent(in)  :: c              !< coefficients c cubic function
   real(kind=wp),    intent(in)  :: d              !< coefficients d cubic function
   double complex,   intent(out) :: z(3)           !< roots cubic function
   logical,          intent(out) :: succes         !< flag for succes
   character(len=*), intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   real(kind=wp)  :: p, q  !< coefficients depressed cubic function

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! check first coefficient cubic function
   succes = (.not. isEqualZero (a))

   if (succes) then

      ! substitution of z = t-b/(3a) gives: t^3 + pt + q = 0
      p = (3*a*c - b**2)/(3*(a**2))
      q = (2*(b**3) -9*a*b*c +27*(a**2)*d)/(27*(a**3))

      ! calculate roots depressed cubic
      call rootsDepressedCubic (p, q, z)

      ! calculate roots general cubic
      z = z - b/(3*a)

   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = GetOvertoppingMessage(calc_roots_cubic_function)
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

   ! calculate roots depressed cubic:
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

!> isEqualReal:
!! are two reals (almost) equal
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   logical function isEqualReal (x1, x2)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: x1 !< first real
   real(kind=wp), intent(in)  :: x2 !< second real
!
!  Local parameters
!
   real(kind=wp), parameter   :: margin = 1.0d-6 !< relative value for the margin
      
! ==========================================================================================================

   isEqualReal = (abs(x1-x2) <= margin * (abs(x1)+abs(x2))/2)

   end function isEqualReal

!> isEqualZero:
!! is a real (almost) zero
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   logical function isEqualZero (x)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: x  !< real number
!
!  Local parameters
!
   real(kind=wp), parameter   :: margin = 1.0d-14 !< absolute value for the margin
      
! ==========================================================================================================

   isEqualZero = (abs(x) <= margin)

   end function isEqualZero

!***********************************************************************************************************
   end module formulaModuleOvertopping
!***********************************************************************************************************
