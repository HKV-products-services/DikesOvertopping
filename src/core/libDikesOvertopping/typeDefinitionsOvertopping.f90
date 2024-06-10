! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
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
!! This file contains a module with the type definitions for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants / Edwin Spee, Deltares
!
!  $Id$
!
!***********************************************************************************************************
!
!> type definitions for Dikes Overtopping
!
   module typeDefinitionsOvertopping
!***********************************************************************************************************
   !
   use precision, only : wp
   use typeForFactors
   use typeForLoad
   private :: wp

   !> tpCoordinatePair: structure with a vector of x and y coordinates
   type, public :: tpCoordinatePair
      integer                     :: N = 0            !< number of coordinates
      real(kind=wp), allocatable  :: x(:)             !< vector with x-coordinates (m)
      real(kind=wp), allocatable  :: y(:)             !< vector with y-coordinates (m)
   end type tpCoordinatePair

   !> tpGeometry: structure with geometry data
   type, public :: tpGeometry
      real(kind=wp)               :: psi                         !< dike normal (degrees)
      type(tpCoordinatePair)      :: Coordinates                 !< x and y coordinates cross section
      real(kind=wp), allocatable  :: roughnessFactors(:)         !< vector with roughness factors cross section
      type(tpCoordinatePair)      :: CoordDiff                   !< differences in x and y coordinates
      real(kind=wp), allocatable  :: segmentSlopes(:)            !< vector with slopes dike segments
      integer, allocatable        :: segmentTypes(:)             !< vector with segment types (1=slope,2=berm,3=other)
      integer                     :: NbermSegments               !< number of berm segments
      character                   :: splitId                     !< id for B and F splitted type geometries
      type(tpGeometries), pointer :: parent => null()            !< (temp) pointer to all geometries
   end type tpGeometry

   !> tpGeometries: tree structure with several geometries
   type, public :: tpGeometries
      type(tpGeometry), pointer     :: base
      type(tpGeometry), allocatable :: adjWithDikeHeight
      type(tpGeometry), allocatable :: geometryMergedBerms
      type(tpGeometry), allocatable :: geometrySectionB
      type(tpGeometry), allocatable :: geometrySectionBNoBerms
      type(tpGeometry), allocatable :: geometrySectionF
      type(tpGeometry), allocatable :: geometrySectionFNoBerms
      type(tpGeometry), allocatable :: geometryFlatBerms
      type(tpGeometry), allocatable :: geometryRemoveDikeSegments
      type(tpCoordinatePair)        :: CoordsAdjusted              !< vector with x/y-coordinates of the adjusted profile
   end type tpGeometries

!***********************************************************************************************************
    end module typeDefinitionsOvertopping
!***********************************************************************************************************

