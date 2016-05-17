!> @file
!! This file contains the omkeerVariant
!<
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module for the 'omkeerVariant'
!! @ingroup LibOvertopping
!<
module omkeerVariantModule
use OvertoppingMessages
use equalReals
use vectorUtilities
implicit none
private
public :: iterateToGivenDischarge
contains
!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
subroutine iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use overtoppingInterface
    use ModuleLogging
    use zFunctionsWTIOvertopping
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
    real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
    type(tLogging), intent(in)                 :: logging        !< logging struct
!
    type (tpGeometry)                          :: geometry       ! structure with geometry data
!
    currentLogging = logging

    !
    ! basic test : water level must be > toe, otherwise return water level
    !
    if (geometryF%ycoords(1) > load%h ) then
        success        = .true.
        dikeHeight     = load%h
        overtopping%z2 = 0d0
        overtopping%Qo = 0d0
    else
        call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                                 geometryF%roughness, geometry, success, errorText)

        if (success) then
            call iterateToGivenDischargeValidProfile(load, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText )
        else
            call set_nan(dikeHeight)
        endif

        call deallocateGeometry(geometry )
    endif
end subroutine iterateToGivenDischarge

!>
!! Subroutine with iterateToGivenDischarge, with already checked profile
!!
!! @ingroup dllDikesOvertopping
subroutine iterateToGivenDischargeValidProfile(load, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText )
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use overtoppingInterface
    use ModuleLogging
    use zFunctionsWTIOvertopping
    type (tpGeometry), intent(in)              :: geometry       !< internal structure with geometry data
    type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
    real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
    real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
    type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
    type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
    logical, intent(out)                       :: success        !< flag for success
    character(len=*), intent(out)              :: errorText      !< error message (only set if not successful)
!
    real(kind=wp)                              :: dis1                  ! discharge at minDikeHeight
    real(kind=wp)                              :: dis2                  ! discharge at maxDikeHeight
    real(kind=wp)                              :: disNext               ! discharge at nextDikeHeight
    real(kind=wp)                              :: minDikeHeight         ! lower bound dike heigth
    real(kind=wp)                              :: maxDikeHeight         ! upper bound dike heigth
    real(kind=wp)                              :: nextDikeHeight        ! dike heigth for next calculation
    integer                                    :: i                     ! loop counter
    integer, parameter                         :: maxIt = 5             ! maximum number of iterations
    real(kind=wp), parameter                   :: tolDischarge = 1d-3   ! relative tolerance
    real(kind=wp), parameter                   :: tolDikeHeight = 1d-3  ! absolute tolerance in m
    
    logical, allocatable                       :: isBerm(:)             ! is part i a berm or not
    logical, allocatable                       :: isValidZ(:)           ! is z in profile valid (above water level)
    real(kind=wp), allocatable                 :: dischargeProfile(:)   ! discharge at profile points
    real(kind=wp), allocatable                 :: ZProfile(:)           ! dikeheight at profile points
    integer                                    :: nPoints               ! number of profile points
    integer                                    :: ierr                  ! error code
    integer                                    :: iUp                   ! upper bound on profile
    integer                                    :: iLow                  ! lower bound on profile
    real(kind=wp)                              :: X(2), Y(2)            ! arguments for logInterpolate
    real(kind=wp), parameter                   :: minZberm = 0.1_wp     ! minimal difference in Z before interpolation
    logical                                    :: foundValue            ! flag for early succesfull return
    
    foundValue = .false.
    nPoints = geometry%nCoordinates
    allocate(isBerm(nPoints-1), dischargeProfile(nPoints), isValidZ(nPoints), ZProfile(nPoints), stat=ierr)
    if (ierr /= 0) then
        success = .false.
        write(errorText, GetOvertoppingFormat(allocateError)) 4*nPoints-1
        return
    endif
    
    do i = 1, nPoints-1
        isBerm(i) = geometry%segmentTypes(i) == 2
    enddo
    do i = 1, nPoints
        isValidZ(i) = .false.
        call set_nan(ZProfile(i))
        call set_nan(dischargeProfile(i))
    enddo

    !
    ! get discharge for end of slope segments:
    !
    do i = 1, nPoints
        nextDikeHeight = geometry%yCoordinates(i)
        isValidZ(i) = nextDikeHeight >= load%H
        if (i > 1) then
            isValidZ(i) = isValidZ(i) .and. .not. isBerm(i-1)
        endif
        if (isValidZ(i)) then
            call calculateQoRTO(nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
            ZProfile(i) = nextDikeHeight
            dischargeProfile(i) = overtopping%Qo
            if (overtopping%Qo < givenDischarge ) then
                exit
            endif
        endif
    enddo

    iLow = nPoints + 1
    iUp = 0
    do i = 1, nPoints
        if (isValidZ(i)) then
            if (equalRealsRelative(givenDischarge , dischargeProfile(i) , tolDischarge )) then
                success = .true.
                dikeHeight = geometry%yCoordinates(i)
                overtopping%Qo = dischargeProfile(i)
                foundValue = .true.
            else if (givenDischarge < dischargeProfile(i)) then
                iLow = i
            else if (iUp == 0) then
                iUp = i
            endif
        endif
    enddo
    
    !
    ! check: possibly on berm
    !
    if (foundValue ) then
        continue
    elseif (iUp > iLow + 1 ) then
        minDikeHeight = ZProfile(iLow)
        dis1 = dischargeProfile(iLow)
        maxDikeHeight = ZProfile(iUp)
        dis2 = dischargeProfile(iUp)
        do i = iLow + 1, iUp - 1
            if (.not. isBerm(i) ) then
                nextDikeHeight = geometry%yCoordinates(i) + xDiff_min * geometry%segmentSlopes(i)
                call calculateQoRTO(nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
                ZProfile(i) = nextDikeHeight
                dischargeProfile(i) = overtopping%Qo
                isValidZ(i) = .true.
                if (dischargeProfile(i) < givenDischarge) then
                    if (ZProfile(i) - ZProfile(iLow) > minZberm ) then
                        X = (/ dischargeProfile(iLow), dischargeProfile(i) /)
                        Y = (/ ZProfile(iLow), ZProfile(i) /)
                    else
                        nextDikeHeight = ZProfile(i) - minZberm
                        call calculateQoRTO(nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
                        X = (/ overtopping%Qo, dischargeProfile(i) /)
                        Y = (/ nextDikeHeight, ZProfile(i) /)
                    endif
                    dikeHeight = LogLinearInterpolate(X , Y , givenDischarge, ierr, errorText)
                    overtopping%Qo = givenDischarge
                    success = ierr == 0
                    foundValue = .true.
                else
                    minDikeHeight = ZProfile(i)
                    dis1 = dischargeProfile(i)
                endif
            endif
        enddo
    else if (iUp == iLow + 1 ) then
        minDikeHeight = ZProfile(iLow)
        dis1 = dischargeProfile(iLow)
        maxDikeHeight = ZProfile(iUp)
        dis2 = dischargeProfile(iUp)
    else if (iLow == nPoints + 1 ) then
        minDikeHeight = load%h
        call calculateQoRTO(minDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        dis1 = overtopping%Qo
        if (dis1 < givenDischarge) then
            dikeHeight = minDikeHeight
            return
        endif
        maxDikeHeight = ZProfile(iUp)
        dis2 = dischargeProfile(iUp)
    else
        minDikeHeight = ZProfile(iLow)
        dis1 = dischargeProfile(iLow)
        maxDikeHeight = max(minDikeHeight, load%h + load%Hm0, geometry%yCoordinates(nPoints)) + 1.0_wp
        do
            call calculateQoRTO(maxDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
            dis2 = overtopping%Qo
            
            if (.not. success) return ! only in very exceptional cases
            
            if (abs(dis2 - givenDischarge ) < givenDischarge * tolDischarge) then
                dikeHeight = maxDikeHeight
                foundValue = .true.
                exit
            else if (dis2 > givenDischarge) then
                X = (/ dis1 , dis2 /)
                Y = (/ minDikeHeight , maxDikeHeight /)
                minDikeHeight = maxDikeHeight
                dis1 = dis2
                maxDikeHeight = LogLinearInterpolate(X , Y , givenDischarge, ierr, errorText)
            else
                exit
            endif
        enddo
    endif
    !
    ! use logarithmic relation between discharge and dike height
    !
    if (.not. foundValue) then
        do i = 1, maxIt
            X = (/ dis1 , dis2 /)
            Y = (/ minDikeHeight , maxDikeHeight /)
            nextDikeHeight = LogLinearInterpolate(X , Y , givenDischarge, ierr, errorText)
            if (ierr /= 0) then
                !
                ! if loglinear fails, fall back on mean
                !
                nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight)
            endif
            call calculateQoRTO(nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        
            if (.not. success) return  ! only in very exceptional cases
        
            disNext = overtopping%Qo
        
            !
            ! check convergence
            !
            if (abs(disNext - givenDischarge ) < givenDischarge * tolDischarge) exit
            if (abs(maxDikeHeight - minDikeHeight ) < tolDikeHeight) exit
            
            !
            ! change upper or lower bound to be used in next iteration
            !
            if (disNext > givenDischarge) then
                minDikeHeight = nextDikeHeight
                dis1 = disNext
            else
                maxDikeHeight = nextDikeHeight
                dis2 = disNext
            endif
        enddo
        if (success) then
            dikeHeight = nextDikeHeight
        else
            call set_nan(dikeHeight)
        endif
    endif

    deallocate(isBerm, dischargeProfile, isValidZ, ZProfile)

end subroutine iterateToGivenDischargeValidProfile

end module omkeerVariantModule
