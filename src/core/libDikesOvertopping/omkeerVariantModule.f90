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
public :: iterateToGiveDischarge
contains
!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
subroutine iterateToGiveDischarge ( load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, success, errorText, logging)
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
    type (tpGeometry)                          :: geometry              ! structure with geometry data
    real(kind=wp)                              :: dis1                  ! discharge at minDikeHeight
    real(kind=wp)                              :: dis2                  ! discharge at maxDikeHeight
    real(kind=wp)                              :: disNext               ! discharge at nextDikeHeight
    real(kind=wp)                              :: minDikeHeight         ! lower bound dike heigth
    real(kind=wp)                              :: maxDikeHeight         ! upper bound dike heigth
    real(kind=wp)                              :: nextDikeHeight        ! dike heigth for next calculation
    integer                                    :: i                     ! loop counter
    integer, parameter                         :: maxIt = 50            ! maximum number of iterations
    real(kind=wp), parameter                   :: tolDischarge = 1d-3   ! relative tolerance
    real(kind=wp), parameter                   :: tolDikeHeight = 1d-3  ! absolute tolerance in m
!
    currentLogging = logging
    
    call initializeGeometry (geometryF%normal, geometryF%npoints, geometryF%xcoords, geometryF%ycoords, &
                             geometryF%roughness, geometry, success, errorText)
    
    minDikeHeight = geometryF%ycoords(geometryF%npoints - 1) + 0.05_wp
    minDikeHeight = max(load%h, minDikeHeight)
    maxDikeHeight = max(load%h + load%Hm0, geometryF%ycoords(geometryF%npoints)) + 2.5_wp

    !
    ! calculate discharge for lower and upperbound of dikeheight:
    !
    if (success) then
        call calculateQoRTO ( minDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        dis1 = overtopping%Qo
    endif

    if (success) then
        call calculateQoRTO ( maxDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
        dis2 = overtopping%Qo
        if (success .and. (dis2 > givenDischarge .or. dis1 < givenDischarge)) then
            errorText = 'foute start waardes'
            success = .false.
        endif
    endif

    !
    ! iterate using bisection
    !
    do i = 1, maxIt
        if (success) then
            nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight)
            call calculateQoRTO ( nextDikeHeight, modelFactors, overtopping, load, geometry, success, errorText )
            if (.not. success) exit

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
        endif
    enddo

    if (success) then
        dikeHeight = nextDikeHeight
    else
        call set_nan(dikeHeight)
    endif

    call deallocateGeometry( geometry )
end subroutine iterateToGiveDischarge

end module omkeerVariantModule