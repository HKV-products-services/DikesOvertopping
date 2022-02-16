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
!! This file contains a module with the core computations for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants; Edwin Spee, Deltares
!
!  $Id$
!
!***********************************************************************************************************
!
!> core computations for Dikes Overtopping
!
   module mainModuleOvertopping
!***********************************************************************************************************

   use typeDefinitionsOvertopping
   use overtoppingInterface
   use errorMessages
   use precision, only : wp

   implicit none

   private

   public :: calculateOvertopping, calculateOvertoppingSection, calculateWaveOvertopping
   public :: interpolateResultsSections, checkInputdata, checkModelFactors
   public :: setupGeometries, cleanupGeometry, initGeometries

   interface
      module subroutine calculateOvertopping (geometry, load, modelFactors, overtopping, error)
         type (tpGeometry),         intent(inout) :: geometry       !< structure with geometry data
         type (tpLoad),             intent(in)    :: load           !< structure with load parameters
         type (tpOvertoppingInput), intent(in)    :: modelFactors   !< structure with model factors
         type (tpOvertopping),      intent(  out) :: overtopping    !< structure with overtopping results
         type(tMessage),            intent(inout) :: error          !< error struct
      end subroutine calculateOvertopping
   end interface

   interface
      module subroutine calculateOvertoppingSection (geometry, load, gamma_z, gamma_o, modelFactors, overtopping, error)
         type (tpGeometry),         intent(in)     :: geometry       !< structure with geometry data
         type (tpLoadX),            intent(inout)  :: load           !< load struct
         type(tpInfluencefactors),  intent(inout)  :: gamma_z        !< influence angle wave attack wave run-up
         type(tpInfluencefactors),  intent(inout)  :: gamma_o        !< influence angle wave attack overtopping
         type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
         type (tpOvertopping),      intent(out)    :: overtopping    !< structure with overtopping results
         type(tMessage),            intent(inout)  :: error          !< error struct
      end subroutine calculateOvertoppingSection
   end interface

   interface
      module subroutine calculateWaveOvertopping (geometry, load, z2, gamma, modelFactors, Qo, error)
         type (tpGeometry),         intent(in)     :: geometry       !< structure with geometry data
         type (tpLoadX),            intent(inout)  :: load           !< load struct
         real(kind=wp),             intent(in)     :: z2             !< 2% wave run-up (m)
         type(tpInfluencefactors),  intent(inout)  :: gamma          !< influence factors
         type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
         real(kind=wp),             intent(out)    :: Qo             !< wave overtopping discharge (m3/m per s)
         type(tMessage),            intent(inout)  :: error          !< error struct
      end subroutine calculateWaveOvertopping
   end interface

   interface
      module subroutine interpolateResultsSections (geometry, L0, NwideBerms, overtoppingB, overtoppingF, overtopping, error)
         type (tpGeometry),      intent(in   ) :: geometry       !< structure with geometry data
         real(kind=wp),          intent(in   ) :: L0             !< wave length (m)
         integer,                intent(in   ) :: NwideBerms     !< number of wide berms
         type (tpOvertopping),   intent(in   ) :: overtoppingB   !< structure with overtopping results ordinary berms
         type (tpOvertopping),   intent(in   ) :: overtoppingF   !< structure with overtopping results foreshores
         type (tpOvertopping),   intent(  out) :: overtopping    !< structure with combined overtopping results
         type (tMessage),        intent(inout) :: error          !< error struct
      end subroutine interpolateResultsSections
   end interface

   interface
      module subroutine checkInputdata (geometry, load, modelFactors, error)
         type (tpGeometry)    ,     intent(in   ) :: geometry       !< structure with geometry data
         type (tpLoad)        ,     intent(in   ) :: load           !< structure with load parameters
         type (tpOvertoppingInput), intent(in   ) :: modelFactors   !< structure with model factors
         type (tMessage),           intent(inout) :: error          !< error struct
      end subroutine checkInputdata
   end interface

   interface
      module subroutine checkModelFactors (modelFactors, errorMessages, ierr)
         type (tpOvertoppingInput), intent(in)  :: modelFactors       !< structure with model factors
         integer,                   intent(out) :: ierr               !< number of errors found
         character(len=*),          intent(out) :: errorMessages(:)   !< error message
      end subroutine checkModelFactors
   end interface

   interface
      module subroutine initGeometries(geometryF, geometry, error)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"initGeometries" :: initGeometries
         type(OvertoppingGeometryTypeF), intent(inout) :: geometryF      !< struct with geometry and roughness
         type(tpGeometry),               intent(inout) :: geometry
         type(tMessage)  ,               intent(inout) :: error
      end subroutine initGeometries
   end interface

   interface
      module subroutine setupGeometries(geometries)
         type(tpGeometries), target, intent(inout) :: geometries
      end subroutine setupGeometries
   end interface

   interface
      module subroutine cleanupGeometry(geometries, evenBase)
!DEC$ ATTRIBUTES DLLEXPORT,ALIAS:"cleanupGeometry" :: cleanupGeometry
         type(tpGeometries), intent(inout) :: geometries
         logical           , intent(in   ) :: evenBase
      end subroutine cleanupGeometry
   end interface

end module mainModuleOvertopping

