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

submodule (formulaModuleOvertopping) submAdjInfluenceFactors
    use parametersOvertopping
    use OvertoppingMessages
contains

!> adjustInfluenceFactors:
!! adjust the influence factors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure adjustInfluenceFactors
!***********************************************************************************************************
   implicit none
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
   error%errorCode = 0

   ! initialize the minimal values for the influence factors
   gammaB_min = 0.6d0
   gammaF_min = rFactor_min

   select case (gammaBetaType)
      case (1)
         gammaBeta_min = 0.824d0 ! wave run-up
      case (2)
         gammaBeta_min = 0.736d0 ! overtopping
      case default
         error%errorCode = 1
   end select

   ! calculate the total influence factor
   if (error%errorCode == 0) then
      if (ksi0 < ksi0Limit) then
         gammaT = gamma%gammaB * gamma%gammaF * gamma%gammaBeta
      else
         gammaT = gamma%gammaF * gamma%gammaBeta
      endif

      ! check if the total influence factor is greater than zero
      if (gammaT <= 0.0d0) error%errorCode = 1
   endif

   ! adjust the influence factors if the total influence is less than 0.4
   if (error%errorCode == 0) then
      if (gammaT < 0.4d0) then

         ! calculate the ratios of the influence factors in relation to their minimum
         ratioB    = (1.0d0-gamma%gammaB)    / (1.0d0-gammaB_min)
         ratioF    = (1.0d0-gamma%gammaF)    / (1.0d0-gammaF_min)
         ratioBeta = (1.0d0-gamma%gammaBeta) / (1.0d0-gammaBeta_min)

         ! check the ratio values
         if ((ratioB    < 0.0d0) .or. (ratioB    > 1.0d0)) error%errorCode = 1
         if ((ratioF    < 0.0d0) .or. (ratioF    > 1.0d0)) error%errorCode = 1
         if ((ratioBeta < 0.0d0) .or. (ratioBeta > 1.0d0)) error%errorCode = 1

         if (error%errorCode == 0) then

            ! calculate the sum of the ratios
            ratioT = ratioB + ratioF + ratioBeta

            ! adjust the influence factors
            gamma%gammaB    = gamma%gammaB    * exp((ratioB   /ratioT) * log(0.4d0/gammaT))
            gamma%gammaF    = gamma%gammaF    * exp((ratioF   /ratioT) * log(0.4d0/gammaT))
            gamma%gammaBeta = gamma%gammaBeta * exp((ratioBeta/ratioT) * log(0.4d0/gammaT))

         endif

      endif
   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      call GetMSGcalc_influence_factors(error%Message)
   endif

end procedure adjustInfluenceFactors

end submodule submAdjInfluenceFactors
