!> @file
!! This file contains the limit state functions for wave overtopping within WTI
!<
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module for the Limit State Functions (Z-functions) for wave overtopping
!! @ingroup dllOvertopping
!<
module zFunctionsWTIOvertopping
    use precision,                      only : wp
    use overtoppingInterface,           only : tpProfileCoordinate, varModelFactorCriticalOvertopping
    use typeDefinitionsRTOovertopping,  only : xDiff_min, slope_min, tpGeometry, tpLoad, tpOvertoppingInput, tpOvertopping
    use mainModuleRTOovertopping,       only : calculateOvertopping, convertOvertoppingInput
    use geometryModuleRTOovertopping,   only : initializeGeometry, deallocateGeometry
    use vectorUtilities,                only : interpolateLine

    implicit none

    private

    public :: calculateQoRTO, zFuncLogRatios, profileInStructure

contains

!>
!! Subroutine to calculate the overtopping discharge with the RTO-overtopping dll
!! @ingroup LSF
subroutine calculateQoRTO( dikeHeight, modelFactors, overtopping, load, geometry, succes, errorMessage )
    real(kind=wp),                 intent(in)    :: dikeHeight     !< dike height
    type(tpOvertoppingInput),      intent(inout) :: modelFactors   !< struct with model factors
    type (tpOvertopping),          intent(out)   :: overtopping    !< structure with overtopping results
    type (tpGeometry),             intent(in)    :: geometry       !< structure with geometry data
    type (tpLoad),                 intent(in)    :: load           !< structure with load parameters
    logical,                       intent(out)   :: succes         !< flag for succes
    character(len=*),              intent(out)   :: errorMessage   !< error message

    integer                       :: nrCoordsAdjusted       !< number of coordinates of the adjusted profile
    real(kind=wp), pointer        :: xCoordsAdjusted(:)     !< vector with x-coordinates of the adjusted profile
    real(kind=wp), pointer        :: zCoordsAdjusted(:)     !< vector with y-coordinates of the adjusted profile
    type (tpGeometry)             :: geometryAdjusted       !< structure for the adjusted profile

    ! ==========================================================================================================
    !
    !   Initialise
    !
    nullify(xCoordsAdjusted)
    nullify(zCoordsAdjusted)

    call profileInStructure(geometry%nCoordinates, geometry%xcoordinates, geometry%ycoordinates, dikeHeight, &
                            nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage)

    if (succes) then
        call initializeGeometry (geometry%psi, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, &
                                 geometry%roughnessFactors, geometryAdjusted, succes, errorMessage)
    endif

    if (succes) then
        call convertOvertoppingInput(modelFactors, succes, errorMessage)
    endif

    if (succes) then
        call calculateOvertopping (geometryAdjusted, load, modelFactors, overtopping, succes, errorMessage)
    endif

    if (.not. succes) then
        overtopping%Qo = tiny(1.0d0)
        overtopping%z2 = 0d0
    endif
    call deallocateGeometry( geometryAdjusted )

    if (associated(xCoordsAdjusted)) deallocate(xCoordsAdjusted)
    if (associated(zCoordsAdjusted)) deallocate(zCoordsAdjusted)

end subroutine calculateQoRTO

!>
!! Subroutine to fill the profile in a structure and call the adjustment function of the profile due to a desired dike height
!! @ingroup LSF
subroutine profileInStructure( nrCoordinates, xcoordinates, ycoordinates, dikeHeight, nrCoordsAdjusted, &
                               xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage )
    integer,            intent(in)          :: nrCoordinates                !< number of coordinates of the profile
    real(kind=wp),      intent(in)          :: xcoordinates(nrCoordinates)  !< vector with x-coordinates of the profile
    real(kind=wp),      intent(in)          :: ycoordinates(nrCoordinates)  !< vector with y-coordinates of the profile
    real(kind=wp),      intent(in)          :: dikeHeight                   !< dike height
    integer,            intent(out)         :: nrCoordsAdjusted             !< number of coordinates in the adjusted profile
    real(kind=wp),      pointer             :: xCoordsAdjusted(:)           !< vector with x-coordinates of the adjusted profile
    real(kind=wp),      pointer             :: zCoordsAdjusted(:)           !< vector with y-coordinates of the adjusted profile
    logical,            intent(out)         :: succes                       !< flag for succes
    character(len=*),   intent(out)         :: errorMessage                 !< error message

    type(tpProfileCoordinate)     :: coordinates(nrCoordinates)             !< structure for the profile
    integer                       :: i                                      !< do-loop counter

    do i = 1, nrCoordinates
        coordinates(i)%xCoordinate = xcoordinates(i)
        coordinates(i)%zCoordinate = ycoordinates(i)
    enddo
    call adjustProfile(nrCoordinates, coordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage)

end subroutine profileInStructure

!>
!! Subroutine adjust the profile due to a desired dike height
!! @ingroup LSF
subroutine adjustProfile(nrCoordinates, coordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage)

    integer,                    intent(in)  :: nrCoordinates                !< number of coordinates of the profile
    type(tpProfileCoordinate),  intent(in)  :: coordinates(nrCoordinates)   !< structure for the profile
    real(kind=wp),              intent(in)  :: dikeHeight                   !< dike height
    integer,                    intent(out) :: nrCoordsAdjusted             !< number of coordinates in the adjusted profile
    real(kind=wp),              pointer     :: xCoordsAdjusted(:)           !< vector with x-coordinates of the adjusted profile
    real(kind=wp),              pointer     :: zCoordsAdjusted(:)           !< vector with y-coordinates of the adjusted profile
    logical,                    intent(out) :: succes                       !< flag for succes
    character(len=*),           intent(out) :: errorMessage                 !< error message

    ! locals
    !
    real(kind=wp)     :: auxiliaryHeightToe     !< auxiliary height toe of the profile
    real(kind=wp)     :: auxiliaryHeightBerm    !< auxiliary height berm of the profile
    real(kind=wp)     :: auxiliaryHeight        !< auxiliary height for the profile
    real(kind=wp)     :: slope                  !< slope of the profile
    integer           :: i                      !< do-loop counter

    succes = .true.
    if (nrCoordinates < 2) then
        succes = .false.
        errorMessage = 'number of coordinates cross section less than 2'
        return
    endif

    ! the minimal distance between two x-coordinates of the cross section is xDiff_min 
    ! compute the height of the cross section on the point xDiff_min meters after the toe

    auxiliaryHeightToe = interpolateLine(coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, coordinates(1)%xCoordinate + xDiff_min)

    ! First the situation where the new dike height is close to the dike toe
    if (dikeHeight <= auxiliaryHeightToe) then
        nrCoordsAdjusted = 2
        allocate( xCoordsAdjusted(nrCoordsAdjusted) )
        allocate( zCoordsAdjusted(nrCoordsAdjusted) )
        zCoordsAdjusted(2) = dikeHeight
        xCoordsAdjusted(2) = interpolateLine(coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, dikeHeight)
        xCoordsAdjusted(1) = xCoordsAdjusted(2) - xDiff_min
        zCoordsAdjusted(1) = interpolateLine(coordinates(1)%xCoordinate, coordinates(2)%xCoordinate, coordinates(1)%zCoordinate, coordinates(2)%zCoordinate, xCoordsAdjusted(1))
    else
        nrCoordsAdjusted = nrCoordinates
        do i = 2, nrCoordinates - 1
            slope = ( coordinates(i+1)%zCoordinate - coordinates(i)%zCoordinate ) / ( coordinates(i+1)%xCoordinate - coordinates(i)%xCoordinate )
            if (slope < slope_min .and. i < nrCoordinates - 1) then
                !
                ! the next segment of the cross sectin is a berm segment
                auxiliaryHeightBerm = interpolateLine(coordinates(i+1)%xCoordinate, coordinates(i+2)%xCoordinate, coordinates(i+1)%zCoordinate, coordinates(i+2)%zCoordinate, coordinates(i+1)%xCoordinate + xDiff_min)
                if (dikeHeight < auxiliaryHeightBerm) then 
                    nrCoordsAdjusted = i
                    exit
                endif
            else
                auxiliaryHeight = interpolateLine(coordinates(i-1)%xCoordinate, coordinates(i)%xCoordinate, coordinates(i-1)%zCoordinate, coordinates(i)%zCoordinate, coordinates(i-1)%xCoordinate + xDiff_min)
                if (dikeHeight < auxiliaryHeight) then
                    nrCoordsAdjusted = i - 1
                    exit
                elseif (dikeHeight < coordinates(i)%zCoordinate) then
                    nrCoordsAdjusted = i
                    exit
                endif
            endif
        enddo
        allocate( xCoordsAdjusted(nrCoordsAdjusted) )
        allocate( zCoordsAdjusted(nrCoordsAdjusted) )

        ! all segments of the profile except the last
        do i = 1, nrCoordsAdjusted - 1
            xCoordsAdjusted(i) = coordinates(i)%xCoordinate
            zCoordsAdjusted(i) = coordinates(i)%zCoordinate
        enddo

        ! last segment of the profile
        zCoordsAdjusted(nrCoordsAdjusted) = dikeHeight
        xCoordsAdjusted(nrCoordsAdjusted) = interpolateLine(coordinates(nrCoordsAdjusted-1)%zCoordinate, coordinates(nrCoordsAdjusted)%zCoordinate, &

        coordinates(nrCoordsAdjusted-1)%xCoordinate, coordinates(nrCoordsAdjusted)%xCoordinate, dikeHeight)
        
        if (xCoordsAdjusted(nrCoordsAdjusted) < xCoordsAdjusted(nrCoordsAdjusted-1)) then
            succes = .false.
            errorMessage = 'error in calculation of adjusted x-coordinates'
        endif
    endif

end subroutine adjustProfile

!>
!! Routine to compute the limit state value by using the logs of the overtopping discharges (computed and desired)
!! @ingroup LSF
function zFuncLogRatios(qo, qc, mqo, mqc, success, errorMessage) result (z)
    real (kind=wp),   intent(in)  :: qo            !< computed overtopping discharge
    real (kind=wp),   intent(in)  :: qc            !< Critical overtopping discharge
    real (kind=wp),   intent(in)  :: mqo           !< Model factor computed overtopping discharge
    real (kind=wp),   intent(in)  :: mqc           !< Model factor Critical overtopping discharge
    logical,          intent(out) :: success       !< Flag for succes
    character(len=*), intent(out) :: errorMessage  !< error message, only set if not successful
    real (kind=wp)                :: z             !< Value z-function
    !
    ! ==========================================================================================================
    !
    ! Notes:
    ! - The overtopping rates are an exponential function of several of its arguments
    !   Use the logarithm instead of the function itself to avoid convergence problems
    ! - Tiny overtopping rates should not lead to log(0)
    !
    if ( mqc > 0.0d0 .and. qc > 0.0d0 ) then
        z = log( mqc * qc ) - log( max( mqo * qo, tiny(1.0d0) ) )
        success = .true.
    else
        success = .false.
        if ( mqc <= 0.0d0 ) then
            write(errorMessage, '(a,g)') &
                "Negative or zero variance of critical overtopping uncertainty model; variable number: ", varModelFactorCriticalOvertopping
        else ! qc <=0.0d0
            write(errorMessage, '(a,g)') "Negative or zero critical overtopping: ", qc
        endif
    endif
end function ZFuncLogRatios

end module zFunctionsWTIOvertopping