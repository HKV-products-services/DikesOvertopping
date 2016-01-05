module overtoppingInterface
!> @file
!! This file contains the parameters and types (structs)
!! as part of the interface to and from dllOvertopping
!<
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module for the interface of dllOvertopping
!! @ingroup dllOvertopping
!<
    use precision, only : wp
    use, intrinsic :: iso_c_binding
    
    integer, parameter :: varModelFactorCriticalOvertopping =   8   !< Model factor critical overtopping

    type :: tpProfileCoordinate
        real(kind=wp)               :: xCoordinate                  !< X-coordinate foreland profile
        real(kind=wp)               :: zCoordinate                  !< Z-coordinate foreland profile
        real(kind=wp)               :: roughness                    !< Roughness of the area between two points
    end type
    
    type, bind(C) :: OvertoppingGeometryType
        real(kind=wp) :: normal
        integer       :: nPoints
        type(c_ptr)   :: xCoords
        type(c_ptr)   :: yCoords
        type(c_ptr)   :: roughness
    end type OvertoppingGeometryType

    type :: OvertoppingGeometryTypeF
        real(kind=wp)          :: normal
        integer                :: nPoints
        real(kind=wp), pointer :: xCoords(:)
        real(kind=wp), pointer :: yCoords(:)
        real(kind=wp), pointer :: roughness(:)
    end type OvertoppingGeometryTypeF

    private
    
    public :: tpProfileCoordinate, OvertoppingGeometryType, OvertoppingGeometryTypeF, varModelFactorCriticalOvertopping

end module overtoppingInterface
