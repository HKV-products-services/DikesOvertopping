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

submodule (formulaModuleOvertopping) submCalcWaveRunup
   use parametersOvertopping
contains

!> calculateWaveRunup:
!! calculate wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveRunup
!***********************************************************************************************************
   implicit none

   ! if applicable adjust influence factors
   call adjustInfluenceFactors (gamma, 1, ksi0, ksi0Limit, error)

   if (error%errorCode == 0) then

      ! calculate 2% wave run-up for small breaker parameters
      if (ksi0 < ksi0Limit) then
         z2 = Hm0 * fRunup1 * gamma%gammaB * gamma%gammaF * gamma%gammaBeta * ksi0

      ! calculate 2% wave run-up for large breaker parameters
      else
         z2 = Hm0 * gamma%gammaF * gamma%gammaBeta * (fRunup2 - fRunup3/sqrt(ksi0))
         z2 = max(z2, 0.0d0)
      endif
      z2 = z2 * modelFactors%m_z2

   endif

end procedure calculateWaveRunup

end submodule submCalcWaveRunup
