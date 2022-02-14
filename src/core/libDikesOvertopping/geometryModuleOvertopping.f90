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
!! This file contains a module with the core computations for Dikes Overtopping related to the geometry
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants; Edwin Spee, Deltares
!
!  $Id$
!
!***********************************************************************************************************
!
!> core computations related to the geometry
!
   module geometryModuleOvertopping
!***********************************************************************************************************

   use precision, only : wp
   use typeDefinitionsOvertopping
   use overtoppingInterface, only : OvertoppingGeometryTypeF
   use errorMessages, only : TErrorMessages, TMessage

   implicit none

   private

   public :: initializeGeometry, checkCrossSection, calculateSegmentSlopes
   public :: copyGeometry, mergeSequentialBerms, adjustNonHorizontalBerms
   public :: removeBerms, removeDikeSegments, splitCrossSection, calculateHorzLengths
   public :: calculateHorzDistance, deallocateGeometry, basicGeometryValidation
   public :: checkSegmentTypes, cleanupCoordinatePair, allocCoordinatePair, copyCoordinates

   interface
       module subroutine checkCrossSection (psi, coordinates, roughnessFactors, error)
          real(kind=wp),          intent(in)    :: psi                 !< dike normal (degrees)
          type(tpCoordinatePair), intent(in)    :: coordinates         !< x/y coordinates
          real(kind=wp),          intent(in)    :: roughnessFactors(:) !< roughness factors
          type(tMessage),         intent(inout) :: error               !< error struct
      end subroutine checkCrossSection
   end interface

   interface
      module subroutine checkSegmentTypes(geometry, error)
         type (tpGeometry), intent(in)   :: geometry     !< structure with geometry data
         type (tMessage),   intent(inout):: error        !< error struct
      end subroutine checkSegmentTypes
   end interface

   interface
      module subroutine initializeGeometry (psi, coordinates, roughnessFactors, geometry, error)
         real(kind=wp),             intent(in   ) :: psi                 !< dike normal (degree)
         type (tpCoordinatePair),   intent(in   ) :: coordinates         !< x/y-coordinates
         real(kind=wp),             intent(in   ) :: roughnessFactors(:) !< roughness factors
         type (tpGeometry), target, intent(inout) :: geometry            !< structure with geometry data
         type (tMessage),           intent(inout) :: error               !< error struct
      end subroutine initializeGeometry
   end interface

   interface
      module subroutine allocateVectorsGeometry (nCoordinates, geometry, error)
         integer,             intent(in)     :: nCoordinates   !< number of coordinates
         type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
         type (tMessage),     intent(inout)  :: error          !< error struct
         end subroutine allocateVectorsGeometry
      end interface

      interface
         module subroutine cleanupCoordinatePair(xy)
           type(tpCoordinatePair), intent(inout) :: xy
         end subroutine cleanupCoordinatePair
      end interface

      interface
         module subroutine allocCoordinatePair(xy, n, ierr)
           type(tpCoordinatePair), intent(inout) :: xy
           integer               , intent(in   ) :: n
           integer               , intent(  out) :: ierr
         end subroutine allocCoordinatePair
      end interface

      interface
         module subroutine copyCoordinates(coordIn, coordOut)
           type(tpCoordinatePair), intent(in   ) :: coordIn
           type(tpCoordinatePair), intent(inout) :: coordOut
         end subroutine copyCoordinates
      end interface

    interface
       module subroutine deallocateGeometry(geometry)
          type (tpGeometry), intent(inout) :: geometry     !< structure with geometry data
       end subroutine deallocateGeometry
    end interface

   interface
      module subroutine calculateSegmentSlopes (geometry, error)
         type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
         type (tMessage),     intent(inout)  :: error          !< error struct
       end subroutine calculateSegmentSlopes
    end interface

   interface
      module subroutine determineSegmentTypes (geometry, error)
         type (tpGeometry),   intent(inout)  :: geometry     !< structure with geometry data
         type (tMessage),     intent(inout)  :: error        !< error struct
       end subroutine determineSegmentTypes
    end interface
    interface
       module subroutine copyGeometry (geometry, geometryCopy, error, splitId)
          type (tpGeometry),   intent(in)     :: geometry       !< structure with geometry data
          type (tpGeometry),   intent(inout)  :: geometryCopy   !< structure with geometry data copy
          type (tMessage),     intent(inout)  :: error          !< error struct
          character, optional, intent(in)     :: splitId        !< B or F section
       end subroutine copyGeometry
    end interface

    interface
       module subroutine mergeSequentialBerms (geometry, geometryMergedBerms, error)
          type (tpGeometry),   intent(in   )   :: geometry             !< structure with geometry data
          type (tpGeometry),   intent(inout)   :: geometryMergedBerms  !< geometry data with merged sequential berms
          type (tMessage),     intent(inout)   :: error                !< error struct
       end subroutine mergeSequentialBerms
    end interface
    interface
       module subroutine adjustNonHorizontalBerms (geometry, geometryFlatBerms, error)
          type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
          type (tpGeometry),   intent(inout) :: geometryFlatBerms !< geometry data with horizontal berms
          type (tMessage),     intent(inout) :: error             !< error
       end subroutine adjustNonHorizontalBerms
    end interface

    interface
       module subroutine removeBerms (geometry, geometryNoBerms, error)
          type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
          type (tpGeometry),   intent(inout) :: geometryNoBerms   !< geometry data withouth berms
          type (tMessage),     intent(inout) :: error             !< error struct
       end subroutine removeBerms
    end interface

    interface
       module subroutine removeDikeSegments (geometry, index, geometryAdjusted, error)
          type (tpGeometry),   intent(in)    :: geometry          !< structure with geometry data
          integer,             intent(in)    :: index             !< index starting point new cross section
          type (tpGeometry),   intent(inout) :: geometryAdjusted  !< geometry data with removed dike segments
          type (tMessage),     intent(inout) :: error             !< error struct
       end subroutine removeDikeSegments
    end interface

    interface
       module subroutine splitCrossSection (geometry, L0, NwideBerms, geometrysectionB, geometrysectionF, error)
          type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
          real(kind=wp),       intent(in   ) :: L0                !< wave length (m)
          integer,             intent(  out) :: NwideBerms        !< number of wide berms
          type (tpGeometry),   intent(inout) :: geometrySectionB  !< geometry data with wide berms to ordinary berms
          type (tpGeometry),   intent(inout) :: geometrySectionF  !< geometry data with wide berms to foreshores
          type (tMessage),     intent(inout) :: error             !< error struct
       end subroutine splitCrossSection
    end interface

    interface
       module subroutine calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)
          type (tpGeometry), intent(in   ) :: geometry       !< structure with geometry data
          real(kind=wp),     intent(in   ) :: yLower         !< y-coord. lower bound (m+NAP)
          real(kind=wp),     intent(in   ) :: yUpper         !< y-coord. upper bound (m+NAP)
          real(kind=wp),     intent(  out) :: horzLengths(:) !< horizontal lengths segments (m)
          type(tMessage),    intent(inout) :: error          !< error struct
       end subroutine calculateHorzLengths
    end interface

    interface
       module subroutine calculateHorzDistance (geometry, yLower, yUpper, dx, error)
          type (tpGeometry),   intent(in)     :: geometry       !< structure with geometry data
          real(kind=wp),       intent(in)     :: yLower         !< y-coordinate lower bound (m+NAP)
          real(kind=wp),       intent(in)     :: yUpper         !< y-coordinate upper bound (m+NAP)
          real(kind=wp),       intent(inout)  :: dx             !< horizontal distance between bounds (m)
          type(tMessage),      intent(inout)  :: error          !< error struct
       end subroutine calculateHorzDistance
    end interface

    interface
       module subroutine basicGeometryValidation(geometryF, success, errorStruct)
          type(OvertoppingGeometryTypeF), intent(in) :: geometryF           !< struct with geometry and roughness
          type(TErrorMessages), intent(inout)        :: errorStruct         !< error message (only set if not successful)
          logical, intent(out)                       :: success             !< success flag
       end subroutine basicGeometryValidation
    end interface

end module geometryModuleOvertopping

