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
!! Contains the module testHelper
!
! $Id$
!
!> 
!! Module, holding helper routine(s) for the DikesOvertopping tests
!!
!! @ingroup DikeOvertoppingTests
module testHelper
use precision, only : wp
use typeDefinitionsOvertopping
use waveParametersUtilities
use ftnunit

implicit none

private

public :: init_modelfactors_and_load

contains

!> initialize modelFactors and load
!! @ingroup DikeOvertoppingTests
subroutine init_modelfactors_and_load(modelFactors, load, waveheight)
    type(tpOvertoppingInput), intent(inout) :: modelFactors      !< structure with modelFactors
    type (tpLoad), intent(out), optional    :: load              !< structure with load data
    real(kind=wp), intent(in), optional     :: waveheight

    real(kind=wp)                           :: waveSteepness
    character(len=128)                      :: errorMessage      !< error message
    integer                                 :: ierr              !< error code

    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%fshallow                   = 0.92
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0

    if (present(load)) then
        if (present(waveheight)) then
            load%h        =  waveheight
        else
            load%h        =  5.50_wp
        endif
        load%phi      = 50.00_wp
        load%Hm0      =  1.00_wp
        waveSteepness =  0.04_wp
        load%Tm_10    = computeWavePeriod(load%Hm0, waveSteepness, ierr, errorMessage)
        call assert_equal(ierr, 0, errorMessage)
    endif
end subroutine init_modelfactors_and_load

end module testHelper
