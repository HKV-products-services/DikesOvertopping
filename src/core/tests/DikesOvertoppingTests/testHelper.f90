!> @file
!! Contains the module testHelper
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
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