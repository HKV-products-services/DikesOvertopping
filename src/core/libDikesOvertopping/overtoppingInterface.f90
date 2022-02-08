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

!> @file
!! This file contains the parameters and types (structs)
!! as part of the interface to and from dllOvertopping
!<
!
! $Id$
!
!>
!! Module for the interface of dllOvertopping
!! @ingroup LibOvertopping
!<
module overtoppingInterface
    use precision, only : wp
    use, intrinsic :: iso_c_binding
    
    integer, parameter :: varModelFactorCriticalOvertopping =   8   !< Model factor critical overtopping

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
