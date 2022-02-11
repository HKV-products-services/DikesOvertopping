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
!! This file contains the omkeerVariant
!<
!
! $Id$
!
!>
!! Module for the 'omkeerVariant'
!! @ingroup LibOvertopping
!<
module omkeerVariantModule
use typeDefinitionsOvertopping
use overtoppingInterface
use OvertoppingMessages

use equalReals
use errorMessages
use precision, only : wp, set_nan
implicit none

real(kind=wp), parameter                   :: tolDischarge = 1d-3   ! relative tolerance
real(kind=wp), parameter                   :: tolDikeHeight = 1d-3  ! absolute tolerance in m
real(kind=wp), parameter                   :: minZberm = 0.1_wp     ! minimal difference in Z before interpolation

type :: tOmkeer
    logical, allocatable                   :: isBerm(:)             ! is part i a berm or not
    logical, allocatable                   :: isValidZ(:)           ! is z in profile valid (above water level)
    real(kind=wp), allocatable             :: dischargeProfile(:)   ! discharge at profile points
    real(kind=wp), allocatable             :: ZProfile(:)           ! dikeheight at profile points
end type

private
public :: iterateToGivenDischarge

interface
    module subroutine iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping, error)
        type(OvertoppingGeometryTypeF), intent(in) :: geometryF      !< struct with geometry and roughness
        type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
        real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
        real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
        type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
        type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
        type(tMessage), intent(inout)              :: error          !< error struct
    end subroutine iterateToGivenDischarge
end interface

interface
    module subroutine OmkeerValidProfile(load, geometry, givenDischarge, dikeHeight, modelFactors, overtopping, error)
        type(tpGeometry), intent(inout)            :: geometry       !< internal structure with geometry data
        type(tpLoad), intent(in)                   :: load           !< struct with waterlevel and wave parameters
        real(kind=wp), intent(in)                  :: givenDischarge !< discharge to iterate to
        real(kind=wp), intent(out)                 :: dikeHeight     !< dike height
        type(tpOvertoppingInput), intent(inout)    :: modelFactors   !< struct with modelFactors
        type (tpOvertopping), intent(inout)        :: overtopping    !< structure with overtopping results
        type(tMessage), intent(inout)              :: error          !< error struct
    end subroutine OmkeerValidProfile
end interface

end module omkeerVariantModule
