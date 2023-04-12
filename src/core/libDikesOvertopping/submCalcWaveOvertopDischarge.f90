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

submodule (formulaModuleOvertopping) submCalcWaveOvertopDischarge
   use OvertoppingMessages
contains

!> calculateWaveOvertoppingDischarge:
!! calculate the wave overtopping discharge
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveOvertoppingDischarge
!***********************************************************************************************************
   implicit none
!
   real(kind=wp)  :: Qb !< dimensionless  overtopping discharge for breaking waves
   real(kind=wp)  :: Qn !< dimensionless  overtopping discharge for non-breaking waves
   real(kind=wp)  :: Qs !< dimensionless  overtopping discharge for shallow waves

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check input parameters calculation wave overtopping discharge
   if (load%Hm0        <= 0.0d0) error%errorCode = 1
   if (tanAlpha        <= 0.0d0) error%errorCode = 1
   if (gamma%gammaB    <= 0.0d0) error%errorCode = 1
   if (gamma%gammaF    <= 0.0d0) error%errorCode = 1
   if (gamma%gammaBeta <= 0.0d0) error%errorCode = 1
   if (ksi0            <= 0.0d0) error%errorCode = 1

   ! calculate dimensionless overtopping discharges
   if (error%errorCode == 0) then

      ! breaking waves
      Qb = (0.067d0/sqrt(tanAlpha)) * gamma%gammaB * ksi0 * &
               exp(-modelFactors%factorDeterminationQ_b_f_b * ((hCrest-load%h)/load%Hm0) * &
               (1.0d0 /(ksi0 * gamma%gammaBeta * gamma%gammaB * gamma%gammaF)))

      ! non-breaking waves
      Qn = 0.2d0 * exp(-modelFactors%factorDeterminationQ_b_f_n * ((hCrest-load%h)/load%Hm0) * &
               (1.0d0 /(gamma%gammaBeta * gamma%gammaF)))

      ! shallow waves
      Qs = (10.0d0 ** -modelFactors%fshallow) * exp(-(hCrest-load%h) / &
               (gamma%gammaBeta * gamma%gammaF * load%Hm0 * (0.33d0+0.022d0*max(ksi0,7.0d0))))

      ! calculate wave overtopping discharge

      ! check value breaker parameter
      if (ksi0 > 5.0d0) then

         if (ksi0 < 7.0d0) then

            ! 5 < breaker parameter < 7
            if (Qn > 0d0 .and. Qs > 0d0) then
               Qo = max(Qn, exp(log(Qn) + (log(Qs)-log(Qn))*(ksi0-5.0d0)/2.0d0)) * sqrt(gravityConstant * load%Hm0**3)
            else
               Qo = 0d0
            endif

         else

            ! breaker parameter >= 7
            Qo = Qs * sqrt(gravityConstant * load%Hm0**3)

         endif

      else

         ! 0 < breaker parameter <= 5
         Qo = min(Qb,Qn) * sqrt(gravityConstant * load%Hm0**3)

      endif

   else
      ! determine possible error message
      call GetMSGcalc_wave_overtopping_discharge(error%Message)
   endif

end procedure calculateWaveOvertoppingDischarge

end submodule submCalcWaveOvertopDischarge
