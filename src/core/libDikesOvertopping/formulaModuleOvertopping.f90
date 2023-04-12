! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
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
   use precision
   use typeDefinitionsOvertopping
   use errorMessages

   implicit none

   private

   public ::  calculateWaveRunup, calculateWaveLength, calculateWaveSteepness
   public ::  calculateBreakerParameter, calculateAngleWaveAttack, calculateBreakerLimit, adjustInfluenceFactors
   public ::  calculateWaveOvertoppingDischarge

   interface
      module subroutine calculateWaveRunup (Hm0, ksi0, ksi0Limit, gamma, modelFactors, z2, error)
         real(kind=wp),             intent(in)     :: Hm0            !< significant wave height (m)
         real(kind=wp),             intent(in)     :: ksi0           !< breaker parameter
         real(kind=wp),             intent(in)     :: ksi0Limit      !< limit value breaker parameter
         type(tpInfluencefactors),  intent(inout)  :: gamma          !< influence factors
         type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
         real(kind=wp),             intent(out)    :: z2             !< 2% wave run-up (m)
         type(tMessage),            intent(inout)  :: error          !< error struct
      end subroutine calculateWaveRunup
   end interface

   interface
      module subroutine calculateWaveOvertoppingDischarge (load, tanAlpha, gamma, ksi0, hCrest, modelFactors, Qo, error)
         type(tpLoadX),            intent(in   ) :: load           !< load struct
         real(kind=wp),            intent(in   ) :: tanAlpha       !< representative slope angle
         type(tpInfluencefactors), intent(in   ) :: gamma          !< influence factors
         real(kind=wp),            intent(in   ) :: ksi0           !< breaker parameter
         real(kind=wp),            intent(in   ) :: hCrest         !< crest level (m+NAP)
         type(tpOvertoppingInput), intent(in   ) :: modelFactors   !< structure with model factors
         real(kind=wp),            intent(  out) :: Qo             !< wave overtopping discharge (l/m per s)
         type(tMessage),           intent(inout) :: error          !< error struct
      end subroutine calculateWaveOvertoppingDischarge
   end interface

   interface
      module subroutine calculateWaveLength (Tm_10, L0)
         real(kind=wp), intent(in)  :: Tm_10 !< spectral wave period (s)
         real(kind=wp), intent(out) :: L0    !< wave length (m)
      end subroutine calculateWaveLength
   end interface

   interface
      module subroutine calculateWaveSteepness (load, error)
         type(tpLoadX),    intent(inout) :: load           !< load struct
         type(tMessage),   intent(inout) :: error          !< error struct
      end subroutine calculateWaveSteepness
   end interface

   interface
      module subroutine adjustInfluenceFactors (gamma, gammaBetaType, ksi0, ksi0Limit, error)
         type(tPInfluenceFactors), intent(inout) :: gamma          !< influence factors
         integer,                  intent(in)    :: gammaBetaType  !< type influence factor angle of wave attack: 1 = wave run-up, 2 = overtopping
         real(kind=wp),            intent(in)    :: ksi0           !< breaker parameter
         real(kind=wp),            intent(in)    :: ksi0Limit      !< limit value breaker parameter
         type(tMessage),           intent(inout) :: error          !< error struct
      end subroutine adjustInfluenceFactors
   end interface

   interface
      module subroutine realRootsCubicFunction (coeff, N, x, error)
         real(kind=wp),    intent(in  )  :: coeff(:) !< four coefficients cubic function
         integer,          intent(  out) :: N        !< number of real roots cubic function
         real(kind=wp),    intent(  out) :: x(3)     !< real roots cubic function
         type(tMessage),   intent(inout) :: error    !< error struct
      end subroutine realRootsCubicFunction
   end interface

   interface
      module subroutine calculateBreakerLimit (gammaB, ksi0Limit, error)
         real(kind=wp),    intent(in   ) :: gammaB         ! influence factor for berms
         real(kind=wp),    intent(  out) :: ksi0Limit      ! limit value breaker parameter
         type(tMessage),   intent(inout) :: error          ! error struct
      end subroutine calculateBreakerLimit
   end interface

   interface
      module subroutine calculateBreakerParameter (tanAlpha, s0, ksi0, error)
         real(kind=wp),    intent(in   ) :: tanAlpha       !< representative slope angle
         real(kind=wp),    intent(in   ) :: s0             !< wave steepness
         real(kind=wp),    intent(  out) :: ksi0           !< breaker parameter
         type(tMessage),   intent(inout) :: error          !< error struct
      end subroutine calculateBreakerParameter
   end interface

   interface
      module subroutine calculateAngleWaveAttack (phi, psi, beta)
         real(kind=wp), intent(in)  :: phi   !< wave direction (degree)
         real(kind=wp), intent(in)  :: psi   !< dike normal (degree)
         real(kind=wp), intent(out) :: beta  !< angle of wave attack (degree)
      end subroutine calculateAngleWaveAttack
   end interface

!***********************************************************************************************************
   end module formulaModuleOvertopping
!***********************************************************************************************************
