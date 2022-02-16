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

submodule (mainModuleOvertopping) submCheckModelFactors
   use parametersOvertopping
   use OvertoppingMessages
contains

!> checkModelFactors:
!! check the input data
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure checkModelFactors
!***********************************************************************************************************
   implicit none
!
   integer           :: i        !< counter model factors
   character(len=32) :: par_txt  !< description model factor
   real(kind=wp)     :: par      !< value model factor
   real(kind=wp)     :: par_min  !< minimal value model factor
   real(kind=wp)     :: par_max  !< maximal value model factor

   ierr = 0
   ! loop over model factors
   do i=4, 8
      ! determine description model factors and
      ! determine value and minimum/maximum model factor
      select case (i)
         case (4)
             par_txt = GetOvertoppingParameter(par_fB)
             par = modelFactors%factorDeterminationQ_b_f_b
             par_min = fB_min
             par_max = fB_max
         case (5)
             par_txt = GetOvertoppingParameter(par_fN)
             par = modelFactors%factorDeterminationQ_b_f_n
             par_min = fN_min
             par_max = fN_max
         case (6)
             par_txt = GetOvertoppingParameter(par_fS)
             par = modelFactors%fshallow
             par_min = fS_min
             par_max = fS_max
         case (7)
             par_txt = GetOvertoppingParameter(par_2percent_wave_runup)
             par = modelFactors%m_z2
             par_min = mz2_min
             par_max = mz2_max
         case (8)
             par_txt = GetOvertoppingParameter(reductionFactorForeshore)
             par = modelFactors%reductionFactorForeshore
             par_min = foreshore_min
             par_max = foreshore_max
      end select
      ! check value model factor
      if ((par < par_min) .or. (par > par_max)) then
         ierr = ierr + 1
         if (par_max == huge(par_max)) then
            write (errorMessages(ierr), GetFMTmodel_factor_smaller_than()) trim(par_txt), par_min
         else
            write (errorMessages(ierr), GetFMTmodel_factor_not_between()) trim(par_txt), par_min, par_max
         endif
      endif
   enddo

end procedure checkModelFactors

end submodule submCheckModelFactors
