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

submodule (waveRunup) submInnerCalculation
   use formulaModuleOvertopping
   use geometryModuleOvertopping
   use factorModuleOvertopping
contains

!> innerCalculation:
!! inner calculation for the wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure innerCalculation
!***********************************************************************************************************
   implicit none
!
   real(kind=wp)     :: tanAlpha   !< representative slope angle
   real(kind=wp)     :: ksi0       !< breaker parameter
   real(kind=wp)     :: ksi0Limit  !< limit value breaker parameter
   real(kind=wp)     :: z2_smooth  !< 2% wave run-up (no roughness)
   real(kind=wp)     :: z2_rough   !< 2% wave run-up (no berms)
   
! ==========================================================================================================

   ! initialize influence factor berms and roughness and z2_end
   gamma_z%gammaB  = 1.0d0
   gamma_z%gammaF  = 1.0d0
   z2_end          = 0.0d0

   ! calculate representative slope angle
   if (z2 > 0.0d0) then
      call calculateTanAlpha (load, z2, geometryFlatBerms, tanAlpha, error)
   else
      error%errorCode = 0  ! avoid multiple tests on z2 > 0 which should always be true
      return
   endif

   ! calculate breaker parameter
   if (error%errorCode == 0) then
      call calculateBreakerParameter (tanAlpha, load%s0, ksi0, error)
   endif

   ! calculate limit value breaker parameter
   if (error%errorCode == 0) then
      call calculateBreakerLimit (gamma_z%gammaB, ksi0Limit, error)
   endif

   ! calculate z2% smooth (gammaB=1, gammaF=1)
   if (error%errorCode == 0) then
      call calculateWaveRunup (load%Hm0, ksi0, ksi0Limit, gamma_z, modelFactors, z2_smooth, error)
   endif

   ! calculate influence factor roughness (gammaF)
   if (error%errorCode == 0) then
      if (z2_smooth > 0.0d0) then
         call calculateGammaF (load%h, ksi0, ksi0Limit, gamma_z, z2_smooth, geometry, error)

         ! calculate z2% rough (gammaB=1)
         if (error%errorCode == 0) then
            call calculateWaveRunup (load%Hm0, ksi0, ksi0Limit, gamma_z, modelFactors, z2_rough, error)
         endif
      else
         return
      endif
   endif

   ! if cross section contains one or more berms
   if (geometry%NbermSegments > 0 .and. z2_rough > 0.0d0) then

      ! calculate influence factor berms (gammaB)
      if (error%errorCode == 0) then
         call calculateGammaB (load, z2_rough, geometryFlatBerms, gamma_z%gammaB, error)
      endif

      ! calculate limit value breaker parameter
      if (error%errorCode == 0) then
         call calculateBreakerLimit (gamma_z%gammaB, ksi0Limit, error)
      endif

      ! calculate influence factor roughness (gammaF)
      if (error%errorCode == 0) then
         call calculateGammaF (load%h, ksi0, ksi0Limit, gamma_z, z2_rough, geometry, error)
      endif
   
      ! calculate z2%
      if (error%errorCode == 0) then
         call calculateWaveRunup (load%Hm0, ksi0, ksi0Limit, gamma_z, modelFactors, z2_end, error)
      endif

   else

      ! no berms present
      z2_end = z2_rough

   endif

end procedure innerCalculation

end submodule submInnerCalculation
