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

submodule (formulaModuleOvertopping) submCalcWaveProps
   use OvertoppingMessages
contains

!> calculateWaveLength:
!! calculate the wave length
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveLength
!***********************************************************************************************************
   implicit none

   ! calculate the wave length
   L0 = gravityConstant * (Tm_10**2) / (2.0_wp*pi)

end procedure calculateWaveLength

!> calculateWaveSteepness:
!! calculate the wave steepness
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveSteepness
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! calculate the wave steepness
   if (load%L0 > 0.0d0) then
      load%s0 = load%Hm0 / load%L0
   else
      error%errorCode = 1
      call GetMSGcalc_wave_steepness_period_is_zero(error%Message)
   endif

end procedure calculateWaveSteepness

!> calculateBreakerParameter:
!! calculate the breaker parameter
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateBreakerParameter
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! calculate the breaker parameter
   if (s0 > 0.0d0) then
      ksi0 = tanAlpha/sqrt(s0)
   else
      error%errorCode = 1
      call GetMSGcalc_breaker_param_steepness_is_zero(error%Message)
   endif

end procedure calculateBreakerParameter

!> calculateAngleWaveAttack:
!! calculate the angle of wave attack
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateAngleWaveAttack
!***********************************************************************************************************
   implicit none

   ! calculate angle of wave attack
   beta = abs (phi - psi)
   if (beta > 180.0d0) beta = 360.0d0 - beta

end procedure calculateAngleWaveAttack

end submodule submCalcWaveProps
