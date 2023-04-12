! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
!
! This file is part of the Hydra Ring Application.
!
! The Hydra Ring Application is free software: you can redistribute it and/or modify
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
!!     Module waveParametersUtilities for wave parameter computations
!
!
! $Id$
!
!
!> Module to calculate the wave steepness or the wave period
!!
!!   @ingroup general
module waveParametersUtilities

    use precision, only : wp, pi, gravityConstant, set_nan
    use errorMessages

    implicit none

    private :: wp, pi, gravityConstant, set_nan

contains

!> Module to calculate the the wave steepness from the wave height and the wave period
!!
!!   @ingroup general
function computeWaveSteepness( waveHeight, wavePeriod, error )
!
!   input/output parameters
!
    real(kind=wp),    intent(in)    :: waveHeight            !< wave height Hs
    real(kind=wp),    intent(in)    :: wavePeriod            !< wave period Tm-1,0
    type(tMessage),   intent(out)   :: error                 !< error code and message
    real(kind=wp)                   :: computeWaveSteepness  !< wave steepness
!
!   source
!
    error = errorNone
    ! Check that the wave height is higher than or equal to zero:
    if( waveHeight < 0.0d0 ) then
        error%errorCode = 1
        error%message   = "Wave height less than zero"
        call set_nan(computeWaveSteepness)
    else
        !
        ! Check that the wave period is higher than zero:
        if (waveHeight > 0) then
            if( wavePeriod <= 0.0d0) then
                error%errorCode = 1
                error%message   = "Wave period less than or equal to zero"
                call set_nan(computeWaveSteepness)
            else
                !
                ! compute wave steepness
                computeWaveSteepness = ( waveHeight * 2.0d0 * pi ) / ( gravityConstant * (wavePeriod ** 2) )
            endif
        else
            !
            ! Wave height is equal to zero; also the wave steepness
            computeWaveSteepness = 0.0d0
        endif
    endif

end function computeWaveSteepness

!> Module to calculate the the wave period from the wave height and the wave steepness
!!
!!   @ingroup general
function computeWavePeriod( waveHeight, waveSteepness, error )
!
!   input/output parameters
!
    real(kind=wp),    intent(in)    :: waveHeight               !< wave height Hs
    real(kind=wp),    intent(in)    :: waveSteepness            !< wave steepness
    type(tMessage),   intent(out)   :: error                    !< error code and message
    real(kind=wp)                   :: computeWavePeriod        !< wave period Tm-1,0
!
!   source
!
    error = errorNone
    ! Check that the wave height is higher than or equal to zero:
    if( waveHeight < 0.0d0 ) then
        error%errorCode = 1
        error%message   = "Wave height less than zero"
        call set_nan(computeWavePeriod)
    !
    ! Check that the wave steepness is higher than zero:
    else if( waveSteepness <= 0.0d0) then
        error%errorCode = 1
        error%message   = "Wave steepness less than or equal to zero"
        call set_nan(computeWavePeriod)
    else
        !
        ! compute wave period Tm-1,0
        computeWavePeriod = sqrt(waveHeight/(waveSteepness*gravityConstant/(2.0d0 * pi)))
    endif

end function computeWavePeriod

end module waveParametersUtilities
