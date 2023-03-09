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

submodule (mainModuleOvertopping) submAllocOvertopping
   use geometryModuleOvertopping
contains
module procedure initGeometries
      type (tpCoordinatePair)         :: coordinates         !< vector with x/y-coordinates

      coordinates%N = geometryF%npoints
      coordinates%x = geometryF%xcoords
      coordinates%y = geometryF%ycoords
      call initializeGeometry(geometryF%normal, coordinates, geometryF%roughness, geometry, error)
end procedure initGeometries

module procedure setupGeometries
      allocate(geometries%adjWithDikeHeight)
      allocate(geometries%geometryMergedBerms)
      allocate(geometries%geometrySectionB)
      allocate(geometries%geometrySectionBNoBerms)
      allocate(geometries%geometrySectionF)
      allocate(geometries%geometrySectionFNoBerms)
      allocate(geometries%geometryFlatBerms)
      allocate(geometries%geometryRemoveDikeSegments)
      geometries%adjWithDikeHeight%parent => geometries
      geometries%geometrySectionB%parent => geometries
      geometries%geometrySectionF%parent => geometries
      geometries%geometryFlatBerms%parent => geometries
      geometries%geometryRemoveDikeSegments%parent => geometries
end procedure setupGeometries

module procedure cleanupGeometry
      call deallocateGeometry(geometries%adjWithDikeHeight)
      call deallocateGeometry(geometries%geometryMergedBerms)
      call deallocateGeometry(geometries%geometrySectionB)
      call deallocateGeometry(geometries%geometrySectionBNoBerms)
      call deallocateGeometry(geometries%geometrySectionF)
      call deallocateGeometry(geometries%geometrySectionFNoBerms)
      call deallocateGeometry(geometries%geometryFlatBerms)
      call deallocateGeometry(geometries%geometryRemoveDikeSegments)

      deallocate(geometries%adjWithDikeHeight)
      deallocate(geometries%geometryMergedBerms)
      deallocate(geometries%geometrySectionB)
      deallocate(geometries%geometrySectionBNoBerms)
      deallocate(geometries%geometrySectionF)
      deallocate(geometries%geometrySectionFNoBerms)
      deallocate(geometries%geometryFlatBerms)
      deallocate(geometries%geometryRemoveDikeSegments)

      call cleanupCoordinatePair(geometries%CoordsAdjusted)

end procedure cleanupGeometry

end submodule submAllocOvertopping
