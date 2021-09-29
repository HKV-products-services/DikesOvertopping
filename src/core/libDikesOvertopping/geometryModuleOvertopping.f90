! Copyright (C) Stichting Deltares 2019. All rights reserved.
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
!  Programmer: Bastiaan Kuijper, HKV consultants
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
   use formulaModuleOvertopping
   use OvertoppingMessages

   implicit none

   private

   public :: initializeGeometry, checkCrossSection, allocateVectorsGeometry, calculateSegmentSlopes
   public :: determineSegmentTypes, copyGeometry, mergeSequentialBerms, adjustNonHorizontalBerms
   public :: removeBerms, removeDikeSegments, splitCrossSection, calculateHorzLengths
   public :: calculateHorzDistance, deallocateGeometry, basicGeometryTest
   public :: checkSegmentTypes

   contains

!> checkCrossSection:
!! check cross section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine checkCrossSection (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)  :: psi                              !< dike normal (degrees)
   integer,          intent(in)  :: nCoordinates                     !< number of coordinates
   real(kind=wp),    intent(in)  :: xCoordinates    (nCoordinates)   !< x-coordinates (m)
   real(kind=wp),    intent(in)  :: yCoordinates    (nCoordinates)   !< y-coordinates (m+NAP)
   real(kind=wp),    intent(in)  :: roughnessFactors(nCoordinates-1) !< roughness factors
   logical,          intent(out) :: succes                           !< flag for succes
   character(len=*), intent(inout) :: errorMessage                   !< error message
!
!  Local parameters
!
   type (tpGeometry) :: geometry !< structure with geometry data
   real(kind=wp)     :: rFactor  !< offending roughness factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! check dike normal
   if (succes) then
      if ((psi < 0.0d0) .or. (psi > 360.0d0)) then
         succes = .false.
         errorMessage = GetOvertoppingMessage(psi_not_in_range)
      endif
   endif

   ! check number of coordinates
   if (succes) then
      if (nCoordinates < 2) then
         succes = .false.
         errorMessage = GetOvertoppingMessage(dimension_cross_section_less_than_2)
      endif
   endif

   ! initialize structure with geometry data
   if (succes) then
      call initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, &
                               geometry, succes, errorMessage)
   endif

   ! check minimal distance x-coordinates
   if (succes) then
      if (minval(geometry%xCoordDiff) < xDiff_min - marginDiff) then
         succes = .false.
         write (errorMessage, GetOvertoppingFormat(xcoordinates_must_increase)) xDiff_min
      endif
   endif

   ! check minimal distance y-coordinates
   if (succes) then
      if (minval(geometry%yCoordDiff) < 0.0d0) then
         succes = .false.
         errorMessage = GetOvertoppingMessage(ycoordinates_must_be_nondecreasing)
      endif
   endif

   if (succes) then
       call checkSegmentTypes(geometry, succes, errorMessage)
    endif

   ! check roughness factors
   if (succes) then
      if (any(geometry%roughnessFactors < rFactor_min)) then
         succes = .false.
         rFactor = minval(geometry%roughnessFactors)
         write (errorMessage,GetOvertoppingFormat(roughnessfactors_out_of_range)) rFactor_min, rFactor_max, rFactor
      else if (any(geometry%roughnessFactors > rFactor_max)) then
         succes = .false.
         rFactor = maxval(geometry%roughnessFactors)
         write (errorMessage,GetOvertoppingFormat(roughnessfactors_out_of_range)) rFactor_min, rFactor_max, rFactor
      endif
   endif

   call deallocateGeometry(geometry)
   
   end subroutine checkCrossSection

   ! validation routine for segment types
   subroutine checkSegmentTypes(geometry, success, errorMessage)
   type (tpGeometry), intent(in)   :: geometry     !< structure with geometry data
   logical, intent(out)            :: success      !< validation failed or not
   character(len=*), intent(inout) :: errorMessage !< error message, only set in case of an error

   ! check segment types
   if (count(geometry%segmentTypes == 3) > 0d0) then
      success = .false.
      errorMessage = GetOvertoppingMessage(dike_segment_mismatches)
   endif

   ! check number of berm segments
   if (success) then
      if (geometry%NbermSegments > 2) then
         success = .false.
         errorMessage = GetOvertoppingMessage(max2berm_segments)
      endif
   endif

   ! check if first and last dike segment is a slope segment
   if (success) then
      if ((geometry%segmentTypes(1) == 2) .or. &
          (geometry%segmentTypes(geometry%nCoordinates-1) == 2)) then
         success = .false.
         errorMessage = GetOvertoppingMessage(first_and_last_must_be_slope)
      endif
   endif
   end subroutine checkSegmentTypes

!> initializeGeometry:
!! initialize the geometry
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, &
                                  geometry, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),       intent(in   ) :: psi                              !< dike normal (degree)
   integer,             intent(in   ) :: nCoordinates                     !< number of coordinates
   real(kind=wp),       intent(in   ) :: xCoordinates(nCoordinates)       !< x-coordinates (m)
   real(kind=wp),       intent(in   ) :: yCoordinates(nCoordinates)       !< y-coordinates (m+NAP)
   real(kind=wp),       intent(in   ) :: roughnessFactors(nCoordinates-1) !< roughness factors
   type (tpGeometry),   intent(  out) :: geometry                         !< structure with geometry data
   logical,             intent(  out) :: succes                           !< flag for succes
   character(len=*),    intent(inout) :: errorMessage                     !< error message, only set in case of an error
!
!  local parameters
!
   integer                          :: i                                !< loop counter
   integer                          :: ierr                             !< error code

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! copy dike normal to structure with geometry data
   geometry%psi = psi

   ! copy number of coordinates to structure with geometry data
   geometry%nCoordinates = nCoordinates

   ! allocate vectors in structure with geometry data
   call allocateVectorsGeometry (nCoordinates, geometry, succes, errorMessage)

   if (succes) then
   ! copy (x,y)-coordinates and roughness factors to structure with geometry data
      geometry%xCoordinates     = xCoordinates
      geometry%yCoordinates     = yCoordinates
      geometry%roughnessFactors = roughnessFactors

   ! calculate the differences and segment slopes
      call calculateSegmentSlopes (geometry, succes, errorMessage)
   endif

   ! determine the type of the segments (slope or berm)
   if (succes) then
      call determineSegmentTypes (geometry, succes, errorMessage)
   endif
   
   if (succes) then
       do i = 1, geometry%nCoordinates - 1
           if (geometry%Roughnessfactors(i) < rFactor_min .or. geometry%Roughnessfactors(i) > rFactor_max) then
               succes = .false.
               write(errorMessage, GetOvertoppingFormat(roughnessfactors_out_of_range), iostat=ierr) &
                   rFactor_min, rFactor_max, geometry%Roughnessfactors(i)
               if (ierr /= 0) then
                   errorMessage = GetOvertoppingFormat(roughnessfactors_out_of_range)
               endif
               exit
           endif
       enddo
   endif 

   ! determine the number of berm segments
   if (succes) then
      geometry%NbermSegments = count(geometry%segmentTypes == 2)
   endif

   end subroutine initializeGeometry

!> allocateVectorsGeometry:
!! allocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine allocateVectorsGeometry (nCoordinates, geometry, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   integer,             intent(in)     :: nCoordinates   !< number of coordinates
   type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
   logical,             intent(out)    :: succes         !< succes flag
   character(len=*),    intent(inout)  :: errorMessage   !< error message (only set in case of error)
!
!  local parameters
!
   integer                             :: ierr           !< error code allocate
   integer                             :: size           !< total size of arrays to be allocated

! ==========================================================================================================

                  allocate (geometry%xCoordinates     (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%yCoordinates     (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%roughnessFactors (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%xCoordDiff       (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%yCoordDiff       (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentSlopes    (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentTypes     (nCoordinates-1), stat=ierr)

   succes = (ierr == 0)
   if (ierr /= 0) then
       size  = 2 * nCoordinates + 5 * (nCoordinates-1)
       write(errorMessage, GetOvertoppingFormat(allocateError)) size
   endif 

   end subroutine allocateVectorsGeometry

!> deallocateGeometry:
!! deallocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
subroutine deallocateGeometry(geometry)
!***********************************************************************************************************
!
!   input/output parameters
!
    type (tpGeometry), intent(inout) :: geometry     !< structure with geometry data
!
!   source
!
    if (associated(geometry%xCoordinates))     deallocate(geometry%xCoordinates)
    if (associated(geometry%yCoordinates))     deallocate(geometry%yCoordinates)
    if (associated(geometry%roughnessFactors)) deallocate(geometry%roughnessFactors)
    if (associated(geometry%xCoordDiff))       deallocate(geometry%xCoordDiff)
    if (associated(geometry%yCoordDiff))       deallocate(geometry%yCoordDiff)
    if (associated(geometry%segmentSlopes))    deallocate(geometry%segmentSlopes)
    if (associated(geometry%segmentTypes))     deallocate(geometry%segmentTypes)

end subroutine deallocateGeometry

!> calculateSegmentSlopes:
!! calculate the segment slopes
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateSegmentSlopes (geometry, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
   logical,             intent(  out)  :: succes         !< flag for succes
   character(len=*),    intent(inout)  :: errorMessage   !< error message, only set in case of an error

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! calculate horizontal distances
   geometry%xCoordDiff = geometry%xCoordinates(2:geometry%nCoordinates  ) - &
                         geometry%xCoordinates(1:geometry%nCoordinates-1) 

   ! calculate vertical distances
   geometry%yCoordDiff = geometry%yCoordinates(2:geometry%nCoordinates  ) - &
                         geometry%yCoordinates(1:geometry%nCoordinates-1) 

   ! calculate the segment slopes
   if (minval(geometry%xCoordDiff) > 0.0d0) then
      geometry%segmentSlopes = geometry%yCoordDiff / geometry%xCoordDiff
   else
      succes = .false.
      errorMessage = GetOvertoppingMessage(slope_negative)
   endif

   end subroutine calculateSegmentSlopes

!> determineSegmentTypes:
!! determine the segment types
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine determineSegmentTypes (geometry, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(inout)  :: geometry     !< structure with geometry data
   logical,             intent(out  )  :: succes       !< success flag
   character(len=*),    intent(inout)  :: errorMessage !< error message; only set in case of error
!
!  Local parameters
!
   integer  :: i              !< counter dike segments
   logical  :: slopeSegment   !< flag for slope segment
   logical  :: bermSegment    !< flag for berm segment
   integer  :: nBerms         !< total number of berms found; must be <= 2
   character(len=100) :: cfmt !< format string for error message

! ==========================================================================================================

   succes = .true.
   nBerms = 0

   ! loop over dike segments
   do i=1, geometry%nCoordinates - 1

      ! determine type of dike segment
      slopeSegment = ((geometry%segmentSlopes(i) >= slope_min - marginGrad) .and. &
                      (geometry%segmentSlopes(i) <= slope_max + marginGrad))
      bermSegment  = ((geometry%segmentSlopes(i) >= berm_min  - marginGrad) .and. &
                      (geometry%segmentSlopes(i) <= berm_max  + marginGrad))

      ! add type to structure with geometry data
      if (slopeSegment) then
         geometry%segmentTypes(i) = 1 ! slope
      elseif (bermSegment) then
         geometry%segmentTypes(i) = 2 ! berm
         nBerms = nBerms + 1
         if (i == 1) then
             succes = .false.
             call GetMSG_first_segment_berm(errorMessage)
         else if (i == geometry%nCoordinates - 1) then
             succes = .false.
             call GetMSG_last_segment_berm(errorMessage)
         endif
      else
         geometry%segmentTypes(i) = 3 ! other
      endif

   enddo

   if (nBerms > 2) then
       succes = .false.
       call GetFormatTooManyBerms(cfmt)
       write(errorMessage, cfmt) nBerms
   endif

   end subroutine determineSegmentTypes

!> copyGeometry:
!! copy a geometry structure
!!   @ingroup LibOvertopping
!**********************************************************************************************************
   subroutine copyGeometry (geometry, geometryCopy, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in)     :: geometry       !< structure with geometry data
   type (tpGeometry),   intent(inout)  :: geometryCopy   !< structure with geometry data copy
   logical,             intent(out)    :: succes         !< succes flag
   character(len=*),    intent(inout)  :: errorMessage   !< error message, only set in case of error
!
!  Local parameters
!
   integer  :: i  !< counter dike segments

! ==========================================================================================================

   ! allocate vectors in structure with geometry data copy
   call allocateVectorsGeometry (geometry%nCoordinates, geometryCopy, succes, errorMessage)

   if (succes) then
   ! copy dike normal and number of coordinates to structure with geometry data copy
      geometryCopy%copyNumber   = geometry%copyNumber + 1
      geometryCopy%psi          = geometry%psi
      geometryCopy%nCoordinates = geometry%nCoordinates

      ! copy (x,y)-coordinates to structure with geometry data copy
      do i=1, geometry%nCoordinates
         geometryCopy%xCoordinates(i) = geometry%xCoordinates(i)
         geometryCopy%yCoordinates(i) = geometry%yCoordinates(i)
      enddo

      ! copy roughness factors and segment data to structure with geometry data copy
      do i=1, geometry%nCoordinates-1
         geometryCopy%roughnessFactors(i) = geometry%roughnessFactors(i)
         geometryCopy%xCoordDiff(i)       = geometry%xCoordDiff(i)
         geometryCopy%yCoordDiff(i)       = geometry%yCoordDiff(i)
         geometryCopy%segmentSlopes(i)    = geometry%segmentSlopes(i)
         geometryCopy%segmentTypes(i)     = geometry%segmentTypes(i)
      enddo

      ! copy the number of berm segments to structure with geometry data copy
      geometryCopy%NbermSegments = geometry%NbermSegments
   endif

   end subroutine copyGeometry

!> mergeSequentialBerms:
!! merge sequential berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine mergeSequentialBerms (geometry, geometryMergedBerms, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   )   :: geometry             !< structure with geometry data
   type (tpGeometry),   intent(inout)   :: geometryMergedBerms  !< geometry data with merged sequential berms
   logical,             intent(  out)   :: succes               !< flag for succes
   character(len=*),    intent(inout)   :: errorMessage         !< error message, only set in case of an error
!
!  Local parameters
!
   integer  :: i                 !< counter dike segments
   logical  :: sequentialBerms   !< flag for sequential berms
   integer  :: index             !< index of first berm
   real(kind=wp) :: B1           !< width of first berm
   real(kind=wp) :: B2           !< width of second berm
   real(kind=wp) :: rFactor1     !< roughness factor of first berm
   real(kind=wp) :: rFactor2     !< roughness factor of second berm

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! ---------------------------------------
   ! determine if there are sequential berms
   ! ---------------------------------------

   ! initialize flag for sequential berms
   sequentialBerms = .false.

   ! loop over possible berm segments
   do i=2, geometry%nCoordinates - 2

      ! determine if the current and the previous dike segment are sequential berms
      sequentialBerms = ((geometry%segmentTypes(i) == 2) .and. ((geometry%segmentTypes(i-1) == 2)))

      ! if these dike segments are indeed sequential berms then index berms and exit loop
      if (sequentialBerms) then
         index    = i-1
         B1       = geometry%xCoordDiff(index)
         B2       = geometry%xCoordDiff(index+1)
         rFactor1 = geometry%roughnessFactors(index)
         rFactor2 = geometry%roughnessFactors(index+1)
         exit
      endif

   enddo

   ! ------------------------------------------
   ! determine structure with new geometry data
   ! ------------------------------------------

   if (.not. sequentialBerms) then

      ! no sequential berms present
      call copyGeometry (geometry, geometryMergedBerms, succes, errorMessage)

   else

      ! sequential berms present
      geometryMergedBerms%psi           = geometry%psi
      geometryMergedBerms%nCoordinates  = geometry%nCoordinates  - 1
      geometryMergedBerms%NbermSegments = geometry%NbermSegments - 1

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryMergedBerms%nCoordinates, geometryMergedBerms, succes, errorMessage)

      if (succes) then
         ! remove x-coordinate between sequential berms
         geometryMergedBerms%xCoordinates(1:index)  = geometry%xCoordinates(1:index)
         geometryMergedBerms%xCoordinates(index+1:) = geometry%xCoordinates(index+2:)

         ! remove y-coordinate between sequential berms
         geometryMergedBerms%yCoordinates(1:index)  = geometry%yCoordinates(1:index)
         geometryMergedBerms%yCoordinates(index+1:) = geometry%yCoordinates(index+2:)

         ! calculate the influence factor for the compound berm
         geometryMergedBerms%roughnessFactors(1:index-1) = geometry%roughnessFactors(1:index-1)
         if ((B1+B2) > 0.0d0) then
            geometryMergedBerms%roughnessFactors(index)  = (B1*rFactor1 + B2*rFactor2) / (B1+B2)
         else
            succes = .false.
            errorMessage = GetOvertoppingMessage(merging_seq_berm)
         endif
         geometryMergedBerms%roughnessFactors(index+1:)  = geometry%roughnessFactors(index+2:)
      endif
      
      ! recalculate differences and slopes
      if (succes) then
         call calculateSegmentSlopes (geometryMergedBerms, succes, errorMessage)
      endif

      ! remove sequential berm from dike segment types
      geometryMergedBerms%segmentTypes(1:index)  = geometry%segmentTypes(1:index)
      geometryMergedBerms%segmentTypes(index+1:) = geometry%segmentTypes(index+2:)

   endif

   end subroutine mergeSequentialBerms

!> adjustNonHorizontalBerms:
!! adjust non-horizontal berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine adjustNonHorizontalBerms (geometry, geometryFlatBerms, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   type (tpGeometry),   intent(  out) :: geometryFlatBerms !< geometry data with horizontal berms
   logical,             intent(  out) :: succes            !< flag for succes
   character(len=*),    intent(inout) :: errorMessage      !< error message, only set in case of an error
!
!  Local parameters
!
   integer        :: i     !< counter dike segments
   real(kind=wp)  :: hBerm !< average berm height (m)
   real(kind=wp)  :: dx1   !< horizontal distance from previous point to starting point horizontal berm (m)
   real(kind=wp)  :: dx2   !< horizontal distance from end point horizontal berm to next point (m)
   
! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! initialize the structure with geometry data with horizontal berms
   call copyGeometry (geometry, geometryFlatBerms, succes, errorMessage)

   if (succes) then
      ! loop over possible berm segments
      do i=2, geometry%nCoordinates - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! calculate average berm height
            hBerm = (geometry%yCoordinates(i)+geometry%yCoordinates(i+1))/2

            ! calculate horizontal distance from previous point to starting point horizontal berm
            if (geometryFlatBerms%segmentSlopes(i-1) > 0.0d0) then
               dx1 = (hBerm - geometryFlatBerms%yCoordinates(i-1)) / geometryFlatBerms%segmentSlopes(i-1)
            else
               succes = .false.
            endif

            ! calculate horizontal distance from end point horizontal berm to next point
            if (geometryFlatBerms%segmentSlopes(i+1) > 0.0d0) then
               dx2 = (geometryFlatBerms%yCoordinates(i+1) - hBerm) / geometryFlatBerms%segmentSlopes(i+1)
            else
               succes = .false.
            endif

            ! ---------------------------------------------------------------------------------------------
            ! NOTE : for calculation of dx1 and dx2 the slopes of the previous and next dike segment should
            !        have a gradient greather than zero. Therefore, sequential berms should be merged first
            ! ---------------------------------------------------------------------------------------------

            ! determine possible error message
            if (.not. succes) then
               errorMessage = GetOvertoppingMessage(adjust_non_horizontal_seq_berm)
               exit ! exit loop
            endif

            ! determine new starting point horizontal berm
            geometryFlatBerms%yCoordinates(i)   = hBerm
            geometryFlatBerms%xCoordinates(i)   = geometryFlatBerms%xCoordinates(i-1) + dx1

            ! determine new end point horizontal berm
            geometryFlatBerms%yCoordinates(i+1) = hBerm
            geometryFlatBerms%xCoordinates(i+1) = geometryFlatBerms%xCoordinates(i+1) - dx2

            ! recalculate segment slopes
            call calculateSegmentSlopes (geometryFlatBerms, succes, errorMessage)
            if (.not. succes) then
               exit ! exit loop
            endif

         endif

      enddo
   
   endif

   end subroutine adjustNonHorizontalBerms

!> removeBerms:
!! remove berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine removeBerms (geometry, geometryNoBerms, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   type (tpGeometry),   intent(  out) :: geometryNoBerms   !< geometry data withouth berms
   logical,             intent(  out) :: succes            !< flag for succes
   character(len=*),    intent(inout) :: errorMessage      !< error message, only set in case of an error
!
!  Local parameters
!
   integer        :: i     !< counter dike segments
   integer        :: N     !< counter points cross section without berms
   real(kind=wp)  :: Bsum  !< total berm width (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! determine (x,y)-coordinates cross section without berms
   if (geometry%NbermSegments > 0) then

      ! copy dike normal to structure with new geometry data
      geometryNoBerms%psi = geometry%psi

      ! determine number of coordinates cross section without berms
      geometryNoBerms%nCoordinates = geometry%nCoordinates - geometry%NbermSegments

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryNoBerms%nCoordinates, geometryNoBerms, succes, errorMessage)

      if (succes) then
         ! initialize total berm width
         Bsum = 0.0d0

         ! add first point to cross section without berms
         N = 1
         geometryNoBerms%xCoordinates(N) = geometry%xCoordinates(1)
         geometryNoBerms%yCoordinates(N) = geometry%yCoordinates(1)

         ! loop over dike segments
         do i=1, geometry%nCoordinates - 1

            ! determine if the current dike segment is a berm segment
            if (geometry%segmentTypes(i) == 2) then

               ! check if the berm segment is a horizontal berm
               if (geometry%segmentSlopes(i) > 0.0d0) then
                  succes = .false.
                  call GetMSGRemoveNonHorizontalBerm(errorMessage)
               endif

               ! add the width of the berm segment to the total sum
               Bsum = Bsum + geometry%xCoordDiff(i)

            else

               ! if dike segment is not a berm, then add end point and roughness
               N = N+1
               geometryNoBerms%xCoordinates(N)       = geometry%xCoordinates(i+1) - Bsum
               geometryNoBerms%yCoordinates(N)       = geometry%yCoordinates(i+1)
               geometryNoBerms%roughnessFactors(N-1) = geometry%roughnessFactors(i)
         
            endif
         enddo
      endif

      ! calculate the differences and segment slopes
      if (succes) then
         call calculateSegmentSlopes (geometryNoBerms, succes, errorMessage)
      endif

      ! determine the type of the segments (slope or berm)
      if (succes) then
         call determineSegmentTypes (geometryNoBerms, succes, errorMessage)
      endif

      ! determine the number of berm segments
      if (succes) then
         geometryNoBerms%NbermSegments = count(geometryNoBerms%segmentTypes == 2)
      endif

   else

      ! no berms present
      call copyGeometry (geometry, geometryNoBerms, succes, errorMessage)

   endif

   end subroutine removeBerms

!> removeDikeSegments:
!! remove dike segments
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine removeDikeSegments (geometry, index, geometryAdjusted, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in)  :: geometry          !< structure with geometry data
   integer,             intent(in)  :: index             !< index starting point new cross section
   type (tpGeometry),   intent(out) :: geometryAdjusted  !< geometry data with removed dike segments
   logical,             intent(out) :: succes            !< flag for succes
   character(len=*),    intent(inout) :: errorMessage    !< error message
   
! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! check index starting point new cross section
   if ((index < 1) .or. (index > geometry%nCoordinates-1)) then
      succes = .false.
      errorMessage = GetOvertoppingMessage(remove_dike_segments_index)
   endif

   if (succes) then

      ! copy the dike normal
      geometryAdjusted%psi = geometry%psi

      ! determine the number of coordinates for the new cross section
      geometryAdjusted%nCoordinates = geometry%nCoordinates - index + 1

      ! allocate vectors in structure with geometry data
      call allocateVectorsGeometry (geometryAdjusted%nCoordinates, geometryAdjusted, succes, errorMessage)

      if (succes) then
         ! select the remaining dike segments for the new cross section
         geometryAdjusted%xCoordinates     = geometry%xCoordinates    (index:)
         geometryAdjusted%yCoordinates     = geometry%yCoordinates    (index:)
         geometryAdjusted%roughnessFactors = geometry%roughnessFactors(index:)

         ! calculate the differences and segment slopes
         call calculateSegmentSlopes (geometryAdjusted, succes, errorMessage)
      endif

      ! determine the type of the segments (slope or berm)
      if (succes) then
         call determineSegmentTypes (geometryAdjusted, succes, errorMessage)
      endif

      ! determine the number of berm segments for the new cross section
      if (succes) then
         geometryAdjusted%NbermSegments = count(geometryAdjusted%segmentTypes == 2)
      endif

   endif

   end subroutine removeDikeSegments

!> splitCrossSection:
!! split a cross section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine splitCrossSection (geometry, L0, NwideBerms, geometrysectionB, geometrysectionF, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   real(kind=wp),       intent(in   ) :: L0                !< wave length (m)
   integer,             intent(  out) :: NwideBerms        !< number of wide berms
   type (tpGeometry),   intent(  out) :: geometrySectionB  !< geometry data with wide berms to ordinary berms
   type (tpGeometry),   intent(  out) :: geometrySectionF  !< geometry data with wide berms to foreshores
   logical,             intent(  out) :: succes            !< flag for succes
   character(len=*),    intent(inout) :: errorMessage      !< error message, only set in case of an error
!
!  Local parameters
!
   integer        :: i           !< counter dike segments
   real(kind=wp)  :: B           !< width of berm segment (m)
   real(kind=wp)  :: horzShift   !< horizontal shift to adapt wide berms (m)
   real(kind=wp)  :: vertShift   !< vertical shift to adapt wide berms (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! initialize the structures with geometry data for split cross sections
   call copyGeometry (geometry, geometrySectionB, succes, errorMessage)
   if (succes) call copyGeometry (geometry, geometrySectionF, succes, errorMessage)

   ! initialize number of wide berms
   NwideBerms = 0

   ! ------------------------------------------------------------
   ! determine the (x,y)-coordinates for the split cross sections
   ! ------------------------------------------------------------

   if (succes) then
      ! loop over possible berm segments
      do i=2, geometry%nCoordinates - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! determine the width of the berm segment
            B = geometry%xCoordDiff(i)

            ! determine if the berm segment is a wide berm
            if ((B < L0) .and. (B > 0.25d0*L0)) then

               ! count the number of wide berms
               NwideBerms = NwideBerms+1

               ! adapt berm width to B=L0/4 for cross section with ordinary berms

               ! shift start point berm along the segment to reduce width to B=L0/4
               horzShift = (B-0.25d0*L0)
               vertShift = (B-0.25d0*L0) * geometry%segmentSlopes(i)
               geometrySectionB%xCoordinates(i)     = geometrySectionB%xCoordinates(i)     + horzShift
               geometrySectionB%yCoordinates(i)     = geometrySectionB%yCoordinates(i)     + vertShift

               ! shift preceding points horizontally to keep original segment slopes
               if (geometry%segmentSlopes(i-1) > 0.0d0) then
                  horzShift = (B-0.25d0*L0) * (1 - geometry%segmentSlopes(i)/geometry%segmentSlopes(i-1))
               else
                  horzShift = 0.0d0 
                  succes = .false. ! previous dike segment has a gradient <= zero
                  errorMessage = GetOvertoppingMessage(split_cross_section_seq_berm)
               endif
               if (horzShift < 0.0d0) then
                  succes = .false. ! previous dike segment has a more gentle slope
                  errorMessage = GetOvertoppingMessage(split_cross_section_seq_berm)
               endif
               geometrySectionB%xCoordinates(1:i-1) = geometrySectionB%xCoordinates(1:i-1) + horzShift

               ! adapt berm width to B=L0 for cross section with foreshores
               horzShift = B-L0
               geometrySectionF%xCoordinates(1:i)   = geometrySectionF%xCoordinates(1:i)   + horzShift

            endif

         endif

      enddo
   
   endif

   ! ---------------------------------------------------------------
   ! recalculate differences and slopes for the split cross sections
   ! ---------------------------------------------------------------

   ! calculate the differences and segment slopes
   if (NwideBerms > 0) then
      if (succes) call calculateSegmentSlopes (geometrySectionB, succes, errorMessage)
      if (succes) call calculateSegmentSlopes (geometrySectionF, succes, errorMessage)
   end if

   ! type of segments and number of berm segments is NOT recalculated

   end subroutine splitCrossSection

!> calculateHorzLengths:
!! calculate horizontal lengths
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateHorzLengths (geometry, yLower, yUpper, horzLengths, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry), intent(in   ) :: geometry                             !< structure with geometry data
   real(kind=wp),     intent(in   ) :: yLower                               !< y-coord. lower bound (m+NAP)
   real(kind=wp),     intent(in   ) :: yUpper                               !< y-coord. upper bound (m+NAP)
   real(kind=wp),     intent(  out) :: horzLengths(geometry%nCoordinates-1) !< horizontal lengths segments (m)
   logical,           intent(  out) :: succes                               !< flag for succes
   character(len=*),  intent(inout) :: errorMessage                         !< error message, only set in case of an error
!
!  Local parameters
!
   integer        :: iLower   !< index dike segment lower bound
   integer        :: iUpper   !< index dike segment upper bound
   real(kind=wp)  :: dy       !< vertical distance (m)
   real(kind=wp)  :: dx       !< horizontal distance (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.

   ! y-coordinate upper bound not lower than y-coordinate lower bound
   if (yUpper < yLower) then
      succes = .false.
   endif

   ! y-coordinate lower bound not lower than dike toe (first y-coordinate)
   if (yLower < geometry%yCoordinates(1)) then
      succes = .false.
   endif

   ! y-coordinate upper bound not higher than crest level (last y-coordinate)
   if (yUpper > geometry%yCoordinates(geometry%nCoordinates)) then
      succes = .false.
   endif

   if (succes) then

      ! initialize horizontal lengths
      horzLengths = geometry%xCoordDiff

      ! determine index first dike segment containing the lower bound
      iLower = count(geometry%yCoordinates < yLower)
      if (iLower == 0) iLower = 1

      ! determine index last dike segment containing the upper bound
      iUpper = geometry%nCoordinates - count(geometry%yCoordinates > yUpper)
      if (iUpper == geometry%nCoordinates) iUpper = geometry%nCoordinates - 1

      ! ---------------------------------------------------------------------------------------------
      ! NOTE: dike segments corresponding to indices above cannot be horizontal berms (by definition)
      ! ---------------------------------------------------------------------------------------------

      ! set the horizontal lengths of all segments before the segment with the lower bound to zero
      if (iLower > 1) then
         horzLengths(1:iLower-1) = 0.0d0
      endif

      ! set the horizontal lengths of all segments after the segment with the upper bound to zero
      if (iUpper < geometry%nCoordinates-1) then
         horzLengths(iUpper+1:geometry%nCoordinates-1) = 0.0d0
      endif

      ! adjust the horizontal length of the segment containing the lower bound
      dy = yLower - geometry%yCoordinates(iLower)
      dx = dy / geometry%segmentSlopes(iLower)
      horzLengths(iLower) = horzLengths(iLower) - dx
      
      ! adjust the horizontal length of the segment containing the upper bound
      dy = geometry%yCoordinates(iUpper+1) - yUpper
      dx = dy / geometry%segmentSlopes(iUpper)
      horzLengths(iUpper) = horzLengths(iUpper) - dx
      
      ! ---------------------------------------------------------------------------------------------
      ! NOTE: calculations above use the fact that the segments are no horizontal berms (tanAlpha<>0)
      ! ---------------------------------------------------------------------------------------------

   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = GetOvertoppingMessage(calc_horizontal_lengths)
   endif   

   end subroutine calculateHorzLengths

!> calculateHorzDistance:
!! calculate horizontal distance
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateHorzDistance (geometry, yLower, yUpper, dx, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in)     :: geometry       !< structure with geometry data
   real(kind=wp),       intent(in)     :: yLower         !< y-coordinate lower bound (m+NAP)
   real(kind=wp),       intent(in)     :: yUpper         !< y-coordinate upper bound (m+NAP)
   real(kind=wp),       intent(inout)  :: dx             !< horizontal distance between bounds (m)
   logical,             intent(out)    :: succes         !< flag for succes
   character(len=*),    intent(inout)  :: errorMessage   !< error message, only set in case of an error
!
!  Local parameters
!
   real(kind=wp) :: horzLengths(geometry%nCoordinates-1) !< horizontal lengths segments (m)

! ==========================================================================================================

   ! calculate horizontal lengths
   call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, succes, errorMessage)

   ! calculate horizontal distance
   if (succes) dx = sum(horzLengths)

   end subroutine calculateHorzDistance

!> basicGeometryTest:
!! test the input geometry (the adjusted geometry is checked elsewhere)
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine basicGeometryTest(geometryF, success, errorStruct)
!***********************************************************************************************************
    use overtoppingInterface
    use errorMessages
    type(OvertoppingGeometryTypeF), intent(in) :: geometryF           !< struct with geometry and roughness
    type(TErrorMessages), intent(inout)        :: errorStruct         !< error message (only set if not successful)
    logical, intent(out)                       :: success             !< success flag 
!
!  Local parameters
!
   integer                                     :: i                   !< loop counter
   real(kind=wp)                               :: diffx               !< difference in two consecutive x coordinates
   real(kind=wp)                               :: diffy               !< difference in two consecutive y coordinates
   real(kind=wp), parameter                    :: tol = 1d-6          !< tolerance for comparing reals
   type(tMessage)                              :: message             !< local error message
   real(kind=wp)                               :: xi                  !< current x coordinate
   real(kind=wp)                               :: xnext               !< next x coordinate
   real(kind=wp)                               :: yi                  !< current y coordinate
   real(kind=wp)                               :: ynext               !< next y coordinate
   real(kind=wp)                               :: roughnessFactor     !< current roughness factor
   character(len=128)                          :: formatstr           !< format string
! ==========================================================================================================
   success = .true.
   do i = 1, geometryF%nPoints - 1
       xi              = geometryF%xCoords(i)
       yi              = geometryF%yCoords(i)
       xnext           = geometryF%xCoords(i+1)
       ynext           = geometryF%yCoords(i+1)
       roughnessFactor = geometryF%roughness(i)
       diffx           = xnext - xi
       diffy           = ynext - yi
       if (diffx < 0) then
           success = .false.
           message%errorcode = diffx_negative
           message%severity = severityError
           formatstr = GetOvertoppingFormat(diffx_negative)
           write(message%message, formatstr) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       else if (diffx < xDiff_min - tol) then
           success = .false.
           message%errorcode = diffx_too_small
           message%severity = severityError
           formatstr = GetOvertoppingFormat(diffx_too_small)
           write(message%message, formatstr) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       endif
       if (diffy < 0d0) then
           success = .false.
           message%errorcode = diffy_too_small
           message%severity = severityError
           formatstr = GetOvertoppingFormat(diffy_too_small)
           write(message%message, formatstr) yi, ynext
           call addMessage(errorStruct, message)
       endif
       if (roughnessFactor < rFactor_min .or. roughnessFactor > rFactor_max) then
           success = .false.
           message%errorcode = roughnessfactors_out_of_range
           message%severity = severityError
           formatstr = GetOvertoppingFormat(roughnessfactors_out_of_range)
           write (message%message, formatstr) rFactor_min, rFactor_max, roughnessFactor
           call addMessage(errorStruct, message)
       endif
   enddo
   end subroutine basicGeometryTest
!***********************************************************************************************************
   end module geometryModuleOvertopping
!***********************************************************************************************************
