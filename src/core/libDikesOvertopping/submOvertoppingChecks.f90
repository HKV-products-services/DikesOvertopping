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

submodule (mainModuleOvertopping) submOvertoppingChecks
   use parametersOvertopping
   use OvertoppingMessages
contains

!> checkInputdata:
!! check the input data
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure checkInputdata
!***********************************************************************************************************
   implicit none
    character(len=StrLenMessages) :: errorTexts(1)  !< local error or validation messages

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check local water level
   if (error%errorCode == 0) then
      if (isnan(load%h)) then
         error%errorCode = 1
         error%Message = 'load%h is NaN'
      else if (load%h > geometry%Coordinates%y(geometry%Coordinates%N)) then 
         error%errorCode = 1
         call GetMSGwl_above_crest(error%Message)
      endif
   endif

   ! check wave height and period
   if (error%errorCode == 0) then
      if (load%Hm0   < 0.0d0 .or. isnan(load%Hm0)) error%errorCode = 1
      if (load%Tm_10 < 0.0d0 .or. isnan(load%Tm_10)) error%errorCode = 1
      if (error%errorCode /= 0) then
         call GetMSGwave_height_or_periode_less_zero(error%Message)
      endif
   endif

   ! check wave direction
   if (error%errorCode == 0) then
      if ((load%phi < 0.0d0) .or. (load%phi > 360.0d0) .or. isnan(load%phi)) then
         error%errorCode = 1
         call GetMSGwave_direction_not_in_range(error%Message)
      endif
   endif

   if (error%errorCode == 0) then
      ! liever niet tijdens z-func
      call checkModelFactors (modelFactors, 1, errorTexts , error%errorcode)
      if (error%errorcode /= 0) then
         error%message = errorTexts(1)
      end if
   endif

end procedure checkInputdata

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
      ! determine description model factor
      select case (i)
         case (4)
             par_txt = GetOvertoppingParameter(par_fB)
         case (5)
             par_txt = GetOvertoppingParameter(par_fN)
         case (6)
             par_txt = GetOvertoppingParameter(par_fS)
         case (7)
             par_txt = GetOvertoppingParameter(par_2percent_wave_runup)
         case (8)
             par_txt = GetOvertoppingParameter(reductionFactorForeshore)
      end select
      ! determine value and minimum/maximum model factor
      select case (i)
         case (4)
             par = modelFactors%factorDeterminationQ_b_f_b
             par_min = fB_min
             par_max = fB_max
         case (5)
             par = modelFactors%factorDeterminationQ_b_f_n
             par_min = fN_min
             par_max = fN_max
         case (6)
             par = modelFactors%fshallow
             par_min = fS_min
             par_max = fS_max
         case (7)
             par = modelFactors%m_z2
             par_min = mz2_min
             par_max = mz2_max
         case (8)
             par = modelFactors%reductionFactorForeshore
             par_min = foreshore_min
             par_max = foreshore_max
      end select
      ! check value model factor
      if (ierr < dimErrMessage) then
         if ((par < par_min) .or. (par > par_max)) then
            ierr = ierr + 1
            if (par_max == huge(par_max)) then
               write (errorMessages(ierr), GetFMTmodel_factor_smaller_than()) trim(par_txt), par_min
            else
               write (errorMessages(ierr), GetFMTmodel_factor_not_between()) trim(par_txt), par_min, par_max
            endif
         endif
      endif
   enddo

end procedure checkModelFactors

end submodule submOvertoppingChecks
