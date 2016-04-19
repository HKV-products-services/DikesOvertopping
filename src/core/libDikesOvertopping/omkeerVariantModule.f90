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
implicit none
private
public :: iterateToGivenDischarge
contains
!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
subroutine iterateToGivenDischarge ( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
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

    call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)


    if (success) then
        call iterateToGivenDischargeValidProfile( load, geometryF, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText )
    else
        call set_nan(dikeHeight)
    endif

    call deallocateGeometry( geometry )
end subroutine iterateToGivenDischarge

!>
!! Subroutine with iterateToGivenDischarge, with already checked profile
!!
!! @ingroup dllDikesOvertopping
subroutine iterateToGivenDischargeValidProfile ( load, geometryF, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText )
    use geometryModuleRTOovertopping
    use typeDefinitionsRTOovertopping
    use overtoppingInterface
    use ModuleLogging
    use zFunctionsWTIOvertopping
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
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
    integer                                    :: j                     ! loop counter
    integer, parameter                         :: maxIt = 50            ! maximum number of iterations
    integer, parameter                         :: maxItBerms = 10       ! maximum number of iterations for skipping berms
    real(kind=wp), parameter                   :: bermJump = 2.5d-2     ! jumps for skipping berms
    real(kind=wp), parameter                   :: tolDischarge = 1d-3   ! relative tolerance
    real(kind=wp), parameter                   :: tolDikeHeight = 1d-3  ! absolute tolerance in m
!

    minDikeHeight = load%h
    maxDikeHeight = max(load%h + load%Hm0, geometryF%ycoords(geometryF%npoints)) + 2.5_wp

    !
    ! calculate discharge for lower and upperbound of dikeheight:
    !
    do i = 0, maxItBerms
        minDikeHeight = minDikeHeight + bermJump * real(i, wp)
        call calculateQoRTO ( minDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        dis1 = overtopping%Qo
        if (success) exit
        continue
    enddo

    if (.not. success) return ! too many errors

    if (dis1 < givenDischarge) then
        dikeHeight = minDikeHeight
        return
    endif

    do i = 0, maxItBerms
        maxDikeHeight = maxDikeHeight + bermJump * real(i, wp)
        call calculateQoRTO ( maxDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        dis2 = overtopping%Qo
        if (success) exit
        continue
    enddo

    if (.not. success) return ! too many errors

    if (dis2 > givenDischarge) then
        errorText = 'foute start waardes'
        success = .false.
        return
    endif

    !
    ! iterate using bisection
    !
    do i = 1, maxIt
        nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight)
        call calculateQoRTO ( nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        if (.not. success) then
            do j = 1, maxItBerms
               nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight) + bermJump * real(j, wp)
               call calculateQoRTO ( nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
               if (success) exit
               nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight) - bermJump * real(j, wp)
               call calculateQoRTO ( nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
               if (success) exit
            enddo
        endif
        
        if (.not. success) return ! too many errors

        disNext = overtopping%Qo

        !
        ! check convergence
        !
        if ( abs ( disNext - givenDischarge ) < givenDischarge * tolDischarge) exit
        if ( abs ( maxDikeHeight - minDikeHeight ) < tolDikeHeight) exit
        
        !
        ! change upper or lower bound to be used in next iteration
        !
        if (disNext > givenDischarge) then
            minDikeHeight = nextDikeHeight
        else
            maxDikeHeight = nextDikeHeight
        endif
    enddo

    if (success) then
        dikeHeight = nextDikeHeight
    else
        call set_nan(dikeHeight)
    endif

end subroutine iterateToGivenDischargeValidProfile

end module omkeerVariantModule