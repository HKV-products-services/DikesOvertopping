submodule (omkeerVariantModule) submOmkeerMain
   use geometryModuleOvertopping
contains

!>
!! Subroutine with omkeerVariant
!!
!! @ingroup dllDikesOvertopping
module procedure iterateToGivenDischarge
    type (tpCoordinatePair)                    :: coordinates    !< vector with x/y-coordinates
    type (tpGeometry)                          :: geometry       ! structure with geometry data
!
    !
    ! basic test : water level must be > toe, otherwise return water level
    !
    if (geometryF%ycoords(1) > load%h ) then
        error%errorCode = 0
        dikeHeight     = load%h
        overtopping%z2 = 0d0
        overtopping%Qo = 0d0
    else
        coordinates%N = geometryF%npoints
        coordinates%x = geometryF%xcoords
        coordinates%y = geometryF%ycoords
        call initializeGeometry (geometryF%normal, coordinates, geometryF%roughness, geometry, error)

        if (error%errorCode == 0) then
            call OmkeerValidProfile(load, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, error )
        else
            call set_nan(dikeHeight)
        endif

        call deallocateGeometry(geometry )
        if (associated(geometry%parent)) then
            deallocate(geometry%parent)
        end if
    endif
end procedure iterateToGivenDischarge

end submodule submOmkeerMain
