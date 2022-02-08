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
   use parametersOvertopping
   use formulaModuleOvertopping
   use OvertoppingMessages
   use errorMessages

   implicit none

   private

   public :: initializeGeometry, checkCrossSection, allocateVectorsGeometry, calculateSegmentSlopes
   public :: determineSegmentTypes, copyGeometry, mergeSequentialBerms, adjustNonHorizontalBerms
   public :: removeBerms, removeDikeSegments, splitCrossSection, calculateHorzLengths
   public :: calculateHorzDistance, deallocateGeometry, basicGeometryTest
   public :: checkSegmentTypes, cleanupCoordinatePair

   contains

!> checkCrossSection:
!! check cross section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine checkCrossSection (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in)    :: psi                              !< dike normal (degrees)
   integer,          intent(in)    :: nCoordinates                     !< number of coordinates
   real(kind=wp),    intent(in)    :: xCoordinates    (nCoordinates)   !< x-coordinates (m)
   real(kind=wp),    intent(in)    :: yCoordinates    (nCoordinates)   !< y-coordinates (m+NAP)
   real(kind=wp),    intent(in)    :: roughnessFactors(nCoordinates-1) !< roughness factors
   type(tMessage),   intent(inout) :: error                            !< error struct
!
!  Local parameters
!
   type (tpGeometry) :: geometry !< structure with geometry data
   real(kind=wp)     :: rFactor  !< offending roughness factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check dike normal
   if ((psi < 0.0d0) .or. (psi > 360.0d0)) then
      error%errorCode = 1
      call GetMSGpsi_not_in_range(error%Message)
   endif

   ! check number of coordinates
   if (error%errorCode == 0) then
      if (nCoordinates < 2) then
         error%errorCode = 1
         call GetMSGdimension_cross_section_less_than_2(error%Message)
      endif
   endif

   ! initialize structure with geometry data
   if (error%errorCode == 0) then
      call initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, geometry, error)
   endif

   ! check minimal distance x-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%x) < xDiff_min - marginDiff) then
         error%errorCode = 1
         write (error%Message, GetFMTxcoordinates_must_increase()) xDiff_min
      endif
   endif

   ! check minimal distance y-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%y) < 0.0d0) then
         error%errorCode = 1
         call GetMSGycoordinates_must_be_nondecreasing(error%Message)
      endif
   endif

   if (error%errorCode == 0) then
       call checkSegmentTypes(geometry, error)
    endif

   ! check roughness factors
   if (error%errorCode == 0) then
      if (any(geometry%roughnessFactors < rFactor_min)) then
         error%errorCode = 1
         rFactor = minval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      else if (any(geometry%roughnessFactors > rFactor_max)) then
         error%errorCode = 1
         rFactor = maxval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      endif
   endif

   call deallocateGeometry(geometry)
   
   end subroutine checkCrossSection

   ! validation routine for segment types
   subroutine checkSegmentTypes(geometry, error)
   type (tpGeometry), intent(in)   :: geometry     !< structure with geometry data
   type (tMessage),   intent(inout):: error        !< error struct

   ! check segment types
   if (count(geometry%segmentTypes == 3) > 0) then
      error%errorCode = 1
      call GetMSGdike_segment_mismatches(error%Message)
   endif

   ! check number of berm segments
   if (error%errorCode == 0) then
      if (geometry%NbermSegments > 2) then
         error%errorCode = 1
         call GetMSGmax2berm_segments(error%Message)
      endif
   endif

   ! check if first and last dike segment is a slope segment
   if (error%errorCode == 0) then
      if ((geometry%segmentTypes(1) == 2) .or. &
          (geometry%segmentTypes(geometry%Coordinates%N-1) == 2)) then
         error%errorCode = 1
         call GetMSGfirst_and_last_must_be_slope(error%Message)
      endif
   endif
   end subroutine checkSegmentTypes

!> initializeGeometry:
!! initialize the geometry
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, roughnessFactors, geometry, error)
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
   type (tpGeometry), target, intent(inout) :: geometry                   !< structure with geometry data
   type (tMessage),     intent(inout) :: error                            !< error struct
!
!  local parameters
!
   integer                          :: i                                !< loop counter
   integer                          :: ierr                             !< error code

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! copy dike normal to structure with geometry data
   geometry%psi = psi

   ! copy number of coordinates to structure with geometry data
   geometry%Coordinates%N = nCoordinates

   ! allocate vectors in structure with geometry data
   call allocateVectorsGeometry (nCoordinates, geometry, error)

   if (error%errorCode == 0) then
   ! copy (x,y)-coordinates and roughness factors to structure with geometry data
      geometry%Coordinates%x     = xCoordinates
      geometry%Coordinates%y     = yCoordinates
      geometry%roughnessFactors = roughnessFactors

   ! calculate the differences and segment slopes
      call calculateSegmentSlopes (geometry, error)
   endif

   ! determine the type of the segments (slope or berm)
   if (error%errorCode == 0) then
      call determineSegmentTypes (geometry, error)
   endif

   if (error%errorCode == 0) then
       do i = 1, geometry%Coordinates%N - 1
           if (geometry%Roughnessfactors(i) < rFactor_min .or. geometry%Roughnessfactors(i) > rFactor_max) then
               error%errorCode = 1
               write(error%Message, GetFMTroughnessfactors_out_of_range(), iostat=ierr) &
                   rFactor_min, rFactor_max, geometry%Roughnessfactors(i)
               if (ierr /= 0) then
                   error%Message = GetFMTroughnessfactors_out_of_range()
               endif
               exit
           endif
       enddo
   endif 

   ! determine the number of berm segments
   if (error%errorCode == 0) then
      geometry%NbermSegments = count(geometry%segmentTypes == 2)
   endif

   if ( .not. associated(geometry%parent)) then
       allocate(geometry%parent)
       geometry%parent%base => geometry
   end if
   end subroutine initializeGeometry

!> allocateVectorsGeometry:
!! allocate the geometry vectors
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine allocateVectorsGeometry (nCoordinates, geometry, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   integer,             intent(in)     :: nCoordinates   !< number of coordinates
   type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
   type (tMessage),     intent(inout)  :: error          !< error struct
!
!  local parameters
!
   integer                             :: ierr           !< error code allocate
   integer                             :: sizeArrays     !< total size of arrays to be allocated

! ==========================================================================================================

   if (allocated(geometry%Coordinates%x)) then
       if (size(geometry%Coordinates%x) == nCoordinates) then
           return
       else
           call deallocateGeometry(geometry)
       end if
   end if

                  allocate (geometry%Coordinates%x    (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%Coordinates%y    (nCoordinates  ), stat=ierr)
   if (ierr == 0) allocate (geometry%roughnessFactors (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%CoordDiff%x      (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%CoordDiff%y      (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentSlopes    (nCoordinates-1), stat=ierr)
   if (ierr == 0) allocate (geometry%segmentTypes     (nCoordinates-1), stat=ierr)

   error%errorCode = ierr
   if (ierr /= 0) then
       sizeArrays  = 2 * nCoordinates + 5 * (nCoordinates-1)
       write(error%Message, GetFMTallocateError()) sizeArrays
   endif 

   end subroutine allocateVectorsGeometry

   subroutine cleanupCoordinatePair(xy)
      type(tpCoordinatePair), intent(inout) :: xy

      if (allocated(xy%x)) deallocate(xy%x)
      if (allocated(xy%y)) deallocate(xy%y)
   end subroutine cleanupCoordinatePair

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
    call cleanupCoordinatePair(geometry%Coordinates)
    call cleanupCoordinatePair(geometry%CoordDiff)
    if (allocated(geometry%roughnessFactors)) deallocate(geometry%roughnessFactors)
    if (allocated(geometry%segmentSlopes))    deallocate(geometry%segmentSlopes)
    if (allocated(geometry%segmentTypes))     deallocate(geometry%segmentTypes)

end subroutine deallocateGeometry

!> calculateSegmentSlopes:
!! calculate the segment slopes
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateSegmentSlopes (geometry, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(inout)  :: geometry       !< structure with geometry data
   type (tMessage),     intent(inout)  :: error          !< error struct

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0
   if (geometry%Coordinates%N < 2) then
       error%errorCode = 1
       call GetMSGdimension_cross_section_less_than_2(error%Message)
   else

      ! calculate horizontal distances
      geometry%CoordDiff%x = geometry%Coordinates%x(2:geometry%Coordinates%N  ) - &
                             geometry%Coordinates%x(1:geometry%Coordinates%N-1) 

      ! calculate vertical distances
      geometry%CoordDiff%y = geometry%Coordinates%y(2:geometry%Coordinates%N  ) - &
                             geometry%Coordinates%y(1:geometry%Coordinates%N-1) 

      ! calculate the segment slopes
      if (minval(geometry%CoordDiff%x) > 0.0d0) then
         geometry%segmentSlopes = geometry%CoordDiff%y / geometry%CoordDiff%x
      else
         error%errorCode = 1
         call GetMSGslope_negative(error%Message)
      endif
   endif

   end subroutine calculateSegmentSlopes

!> determineSegmentTypes:
!! determine the segment types
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine determineSegmentTypes (geometry, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(inout)  :: geometry     !< structure with geometry data
   type (tMessage),     intent(inout)  :: error        !< error struct
!
!  Local parameters
!
   integer  :: i              !< counter dike segments
   logical  :: slopeSegment   !< flag for slope segment
   logical  :: bermSegment    !< flag for berm segment
   integer  :: nBerms         !< total number of berms found; must be <= 2
   character(len=100) :: cfmt !< format string for error message

! ==========================================================================================================

   error%errorCode = 0
   nBerms = 0

   ! loop over dike segments
   do i=1, geometry%Coordinates%N - 1

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
             error%errorCode = 1
             call GetMSG_first_segment_berm(error%Message)
         else if (i == geometry%Coordinates%N - 1) then
             error%errorCode = 1
             call GetMSG_last_segment_berm(error%Message)
         endif
      else
         geometry%segmentTypes(i) = 3 ! other
      endif

   enddo

   if (nBerms > 2) then
       error%errorCode = 1
       call GetFormatTooManyBerms(cfmt)
       write(error%Message, cfmt) nBerms
   endif

   end subroutine determineSegmentTypes

!> copyGeometry:
!! copy a geometry structure
!!   @ingroup LibOvertopping
!**********************************************************************************************************
   subroutine copyGeometry (geometry, geometryCopy, error, splitId)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in)     :: geometry       !< structure with geometry data
   type (tpGeometry),   intent(inout)  :: geometryCopy   !< structure with geometry data copy
   type (tMessage),     intent(inout)  :: error          !< error struct
   character, optional, intent(in)     :: splitId        !< B or F section
!
!  Local parameters
!
   integer  :: i  !< counter dike segments

! ==========================================================================================================

   ! allocate vectors in structure with geometry data copy
   call allocateVectorsGeometry (geometry%Coordinates%N, geometryCopy, error)

   if (error%errorCode == 0) then
   ! copy dike normal and number of coordinates to structure with geometry data copy
      geometryCopy%psi           = geometry%psi
      geometryCopy%Coordinates%N = geometry%Coordinates%N

      ! copy (x,y)-coordinates to structure with geometry data copy
      do i=1, geometry%Coordinates%N
         geometryCopy%Coordinates%x(i) = geometry%Coordinates%x(i)
         geometryCopy%Coordinates%y(i) = geometry%Coordinates%y(i)
      enddo

      ! copy roughness factors and segment data to structure with geometry data copy
      do i=1, geometry%Coordinates%N-1
         geometryCopy%roughnessFactors(i) = geometry%roughnessFactors(i)
         geometryCopy%CoordDiff%x(i)      = geometry%CoordDiff%x(i)
         geometryCopy%CoordDiff%y(i)      = geometry%CoordDiff%y(i)
         geometryCopy%segmentSlopes(i)    = geometry%segmentSlopes(i)
         geometryCopy%segmentTypes(i)     = geometry%segmentTypes(i)
      enddo

      if (present(splitId)) then
         geometryCopy%splitId = splitId
      else
         geometryCopy%splitId = ' '
      endif

      ! copy the number of berm segments to structure with geometry data copy
      geometryCopy%NbermSegments = geometry%NbermSegments
   endif

   end subroutine copyGeometry

!> mergeSequentialBerms:
!! merge sequential berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine mergeSequentialBerms (geometry, geometryMergedBerms, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   )   :: geometry             !< structure with geometry data
   type (tpGeometry),   intent(inout)   :: geometryMergedBerms  !< geometry data with merged sequential berms
   type (tMessage),     intent(inout)   :: error                !< error struct
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
   error%errorCode = 0

   ! ---------------------------------------
   ! determine if there are sequential berms
   ! ---------------------------------------

   ! initialize flag for sequential berms
   sequentialBerms = .false.

   ! loop over possible berm segments
   do i=2, geometry%Coordinates%N - 2

      ! determine if the current and the previous dike segment are sequential berms
      sequentialBerms = ((geometry%segmentTypes(i) == 2) .and. ((geometry%segmentTypes(i-1) == 2)))

      ! if these dike segments are indeed sequential berms then index berms and exit loop
      if (sequentialBerms) then
         index    = i-1
         B1       = geometry%CoordDiff%x(index)
         B2       = geometry%CoordDiff%x(index+1)
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
      call copyGeometry (geometry, geometryMergedBerms, error)

   else

      ! sequential berms present
      geometryMergedBerms%psi            = geometry%psi
      geometryMergedBerms%Coordinates%N  = geometry%Coordinates%N  - 1
      geometryMergedBerms%NbermSegments  = geometry%NbermSegments - 1

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryMergedBerms%Coordinates%N, geometryMergedBerms, error)

      if (error%errorCode == 0) then
         ! remove x-coordinate between sequential berms
         geometryMergedBerms%Coordinates%x(1:index)  = geometry%Coordinates%x(1:index)
         geometryMergedBerms%Coordinates%x(index+1:) = geometry%Coordinates%x(index+2:)

         ! remove y-coordinate between sequential berms
         geometryMergedBerms%Coordinates%y(1:index)  = geometry%Coordinates%y(1:index)
         geometryMergedBerms%Coordinates%y(index+1:) = geometry%Coordinates%y(index+2:)

         ! calculate the influence factor for the compound berm
         geometryMergedBerms%roughnessFactors(1:index-1) = geometry%roughnessFactors(1:index-1)
         if ((B1+B2) > 0.0d0) then
            geometryMergedBerms%roughnessFactors(index)  = (B1*rFactor1 + B2*rFactor2) / (B1+B2)
         else
            error%errorCode = 1
            call GetMSGmerging_seq_berm(error%Message)
         endif
         geometryMergedBerms%roughnessFactors(index+1:)  = geometry%roughnessFactors(index+2:)
      endif
      
      ! recalculate differences and slopes
      if (error%errorCode == 0) then
         call calculateSegmentSlopes (geometryMergedBerms, error)
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
   subroutine adjustNonHorizontalBerms (geometry, geometryFlatBerms, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   type (tpGeometry),   intent(inout) :: geometryFlatBerms !< geometry data with horizontal berms
   type (tMessage),     intent(inout) :: error             !< error
!
!  Local parameters
!
   integer        :: i     !< counter dike segments
   real(kind=wp)  :: hBerm !< average berm height (m)
   real(kind=wp)  :: dx1   !< horizontal distance from previous point to starting point horizontal berm (m)
   real(kind=wp)  :: dx2   !< horizontal distance from end point horizontal berm to next point (m)
   
! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! initialize the structure with geometry data with horizontal berms
   call copyGeometry (geometry, geometryFlatBerms, error, geometry%splitId)

   if (error%errorCode == 0) then
      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! calculate average berm height
            hBerm = (geometry%Coordinates%y(i)+geometry%Coordinates%y(i+1))/2

            ! calculate horizontal distance from previous point to starting point horizontal berm
            if (geometryFlatBerms%segmentSlopes(i-1) > 0.0d0) then
               dx1 = (hBerm - geometryFlatBerms%Coordinates%y(i-1)) / geometryFlatBerms%segmentSlopes(i-1)
            else
               error%errorCode = 1
            endif

            ! calculate horizontal distance from end point horizontal berm to next point
            if (geometryFlatBerms%segmentSlopes(i+1) > 0.0d0) then
               dx2 = (geometryFlatBerms%Coordinates%y(i+1) - hBerm) / geometryFlatBerms%segmentSlopes(i+1)
            else
               error%errorCode = 1
            endif

            ! ---------------------------------------------------------------------------------------------
            ! NOTE : for calculation of dx1 and dx2 the slopes of the previous and next dike segment should
            !        have a gradient greather than zero. Therefore, sequential berms should be merged first
            ! ---------------------------------------------------------------------------------------------

            ! determine possible error message
            if (error%errorCode /= 0) then
               call GetMSGadjust_non_horizontal_seq_berm(error%Message)
               exit ! exit loop
            endif

            ! determine new starting point horizontal berm
            geometryFlatBerms%Coordinates%y(i)   = hBerm
            geometryFlatBerms%Coordinates%x(i)   = geometryFlatBerms%Coordinates%x(i-1) + dx1

            ! determine new end point horizontal berm
            geometryFlatBerms%Coordinates%y(i+1) = hBerm
            geometryFlatBerms%Coordinates%x(i+1) = geometryFlatBerms%Coordinates%x(i+1) - dx2

            ! recalculate segment slopes
            call calculateSegmentSlopes (geometryFlatBerms, error)
            if (error%errorCode /= 0) then
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
   subroutine removeBerms (geometry, geometryNoBerms, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   type (tpGeometry),   intent(inout) :: geometryNoBerms   !< geometry data withouth berms
   type (tMessage),     intent(inout) :: error             !< error struct
!
!  Local parameters
!
   integer        :: i     !< counter dike segments
   integer        :: N     !< counter points cross section without berms
   real(kind=wp)  :: Bsum  !< total berm width (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! determine (x,y)-coordinates cross section without berms
   if (geometry%NbermSegments > 0) then

      ! copy dike normal to structure with new geometry data
      geometryNoBerms%psi = geometry%psi

      ! determine number of coordinates cross section without berms
      geometryNoBerms%Coordinates%N = geometry%Coordinates%N - geometry%NbermSegments

      ! allocate vectors in structure with new geometry data
      call allocateVectorsGeometry (geometryNoBerms%Coordinates%N, geometryNoBerms, error)

      if (error%errorCode == 0) then
         ! initialize total berm width
         Bsum = 0.0d0

         ! add first point to cross section without berms
         N = 1
         geometryNoBerms%Coordinates%x(N) = geometry%Coordinates%x(1)
         geometryNoBerms%Coordinates%y(N) = geometry%Coordinates%y(1)

         ! loop over dike segments
         do i=1, geometry%Coordinates%N - 1

            ! determine if the current dike segment is a berm segment
            if (geometry%segmentTypes(i) == 2) then

               ! check if the berm segment is a horizontal berm
               if (geometry%segmentSlopes(i) > 0.0d0) then
                  error%errorCode = 1
                  call GetMSGRemoveNonHorizontalBerm(error%Message)
               endif

               ! add the width of the berm segment to the total sum
               Bsum = Bsum + geometry%CoordDiff%x(i)

            else

               ! if dike segment is not a berm, then add end point and roughness
               N = N+1
               geometryNoBerms%Coordinates%x(N)       = geometry%Coordinates%x(i+1) - Bsum
               geometryNoBerms%Coordinates%y(N)       = geometry%Coordinates%y(i+1)
               geometryNoBerms%roughnessFactors(N-1)  = geometry%roughnessFactors(i)
         
            endif
         enddo
      endif

      ! calculate the differences and segment slopes
      if (error%errorCode == 0) then
         call calculateSegmentSlopes (geometryNoBerms, error)
      endif

      ! determine the type of the segments (slope or berm)
      if (error%errorCode == 0) then
         call determineSegmentTypes (geometryNoBerms, error)
      endif

      ! determine the number of berm segments
      if (error%errorCode == 0) then
         geometryNoBerms%NbermSegments = count(geometryNoBerms%segmentTypes == 2)
      endif

   else

      ! no berms present
      call copyGeometry (geometry, geometryNoBerms, error)

   endif

   end subroutine removeBerms

!> removeDikeSegments:
!! remove dike segments
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine removeDikeSegments (geometry, index, geometryAdjusted, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in)    :: geometry          !< structure with geometry data
   integer,             intent(in)    :: index             !< index starting point new cross section
   type (tpGeometry),   intent(inout) :: geometryAdjusted  !< geometry data with removed dike segments
   type (tMessage),     intent(inout) :: error             !< error struct
   
! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check index starting point new cross section
   if ((index < 1) .or. (index > geometry%Coordinates%N-1)) then
      error%errorCode = 1
      call GetMSGremove_dike_segments_index(error%Message)
   endif

   if (error%errorCode == 0) then

      ! copy the dike normal
      geometryAdjusted%psi = geometry%psi

      ! determine the number of coordinates for the new cross section
      geometryAdjusted%Coordinates%N = geometry%Coordinates%N - index + 1

      ! allocate vectors in structure with geometry data
      call allocateVectorsGeometry (geometryAdjusted%Coordinates%N, geometryAdjusted, error)

      if (error%errorCode == 0) then
         ! select the remaining dike segments for the new cross section
         geometryAdjusted%Coordinates%x    = geometry%Coordinates%x   (index:)
         geometryAdjusted%Coordinates%y    = geometry%Coordinates%y   (index:)
         geometryAdjusted%roughnessFactors = geometry%roughnessFactors(index:)

         ! calculate the differences and segment slopes
         call calculateSegmentSlopes (geometryAdjusted, error)
      endif

      ! determine the type of the segments (slope or berm)
      if (error%errorCode == 0) then
         call determineSegmentTypes (geometryAdjusted, error)
      endif

      ! determine the number of berm segments for the new cross section
      if (error%errorCode == 0) then
         geometryAdjusted%NbermSegments = count(geometryAdjusted%segmentTypes == 2)
      endif

   endif

   end subroutine removeDikeSegments

!> splitCrossSection:
!! split a cross section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine splitCrossSection (geometry, L0, NwideBerms, geometrysectionB, geometrysectionF, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),   intent(in   ) :: geometry          !< structure with geometry data
   real(kind=wp),       intent(in   ) :: L0                !< wave length (m)
   integer,             intent(  out) :: NwideBerms        !< number of wide berms
   type (tpGeometry),   intent(inout) :: geometrySectionB  !< geometry data with wide berms to ordinary berms
   type (tpGeometry),   intent(inout) :: geometrySectionF  !< geometry data with wide berms to foreshores
   type (tMessage),     intent(inout) :: error             !< error struct
!
!  Local parameters
!
   integer        :: i           !< counter dike segments
   real(kind=wp)  :: B           !< width of berm segment (m)
   real(kind=wp)  :: horzShift   !< horizontal shift to adapt wide berms (m)
   real(kind=wp)  :: vertShift   !< vertical shift to adapt wide berms (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! initialize the structures with geometry data for split cross sections
   call copyGeometry (geometry, geometrySectionB, error, 'B')
   if (error%errorCode == 0) call copyGeometry (geometry, geometrySectionF, error, 'F')

   ! initialize number of wide berms
   NwideBerms = 0

   ! ------------------------------------------------------------
   ! determine the (x,y)-coordinates for the split cross sections
   ! ------------------------------------------------------------

   if (error%errorCode == 0) then
      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! determine the width of the berm segment
            B = geometry%CoordDiff%x(i)

            ! determine if the berm segment is a wide berm
            if ((B < L0) .and. (B > 0.25d0*L0)) then

               ! count the number of wide berms
               NwideBerms = NwideBerms+1

               ! adapt berm width to B=L0/4 for cross section with ordinary berms

               ! shift start point berm along the segment to reduce width to B=L0/4
               horzShift = (B-0.25d0*L0)
               vertShift = (B-0.25d0*L0) * geometry%segmentSlopes(i)
               geometrySectionB%Coordinates%x(i)     = geometrySectionB%Coordinates%x(i)     + horzShift
               geometrySectionB%Coordinates%y(i)     = geometrySectionB%Coordinates%y(i)     + vertShift

               ! shift preceding points horizontally to keep original segment slopes
               if (geometry%segmentSlopes(i-1) > 0.0d0) then
                  horzShift = (B-0.25d0*L0) * (1 - geometry%segmentSlopes(i)/geometry%segmentSlopes(i-1))
               else
                  horzShift = 0.0d0 
                  error%errorCode = 1 ! previous dike segment has a gradient <= zero
                  call GetMSGsplit_cross_section_seq_berm(error%Message)
               endif
               if (horzShift < 0.0d0) then
                  error%errorCode = 1 ! previous dike segment has a more gentle slope
                  call GetMSGsplit_cross_section_seq_berm(error%Message)
               endif
               geometrySectionB%Coordinates%x(1:i-1) = geometrySectionB%Coordinates%x(1:i-1) + horzShift

               ! adapt berm width to B=L0 for cross section with foreshores
               horzShift = B-L0
               geometrySectionF%Coordinates%x(1:i)   = geometrySectionF%Coordinates%x(1:i)   + horzShift

            endif

         endif

      enddo
   
   endif

   ! ---------------------------------------------------------------
   ! recalculate differences and slopes for the split cross sections
   ! ---------------------------------------------------------------

   ! calculate the differences and segment slopes
   if (NwideBerms > 0) then
      if (error%errorCode == 0) call calculateSegmentSlopes (geometrySectionB, error)
      if (error%errorCode == 0) call calculateSegmentSlopes (geometrySectionF, error)
   end if

   ! type of segments and number of berm segments is NOT recalculated

   end subroutine splitCrossSection

!> calculateHorzLengths:
!! calculate horizontal lengths
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry), intent(in   ) :: geometry                              !< structure with geometry data
   real(kind=wp),     intent(in   ) :: yLower                                !< y-coord. lower bound (m+NAP)
   real(kind=wp),     intent(in   ) :: yUpper                                !< y-coord. upper bound (m+NAP)
   real(kind=wp),     intent(  out) :: horzLengths(geometry%Coordinates%N-1) !< horizontal lengths segments (m)
   type(tMessage),    intent(inout) :: error                                 !< error struct
!
!  Local parameters
!
   integer        :: iLower   !< index dike segment lower bound
   integer        :: iUpper   !< index dike segment upper bound
   real(kind=wp)  :: dy       !< vertical distance (m)
   real(kind=wp)  :: dx       !< horizontal distance (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! y-coordinate upper bound not lower than y-coordinate lower bound
   if (yUpper < yLower) then
      error%errorCode = 1
   endif

   ! y-coordinate lower bound not lower than dike toe (first y-coordinate)
   if (yLower < geometry%Coordinates%y(1)) then
      error%errorCode = 1
   endif

   ! y-coordinate upper bound not higher than crest level (last y-coordinate)
   if (yUpper > geometry%Coordinates%y(geometry%Coordinates%N)) then
      error%errorCode = 1
   endif

   if (error%errorCode == 0) then

      ! initialize horizontal lengths
      horzLengths = geometry%CoordDiff%x

      ! determine index first dike segment containing the lower bound
      iLower = count(geometry%Coordinates%y < yLower)
      if (iLower == 0) iLower = 1

      ! determine index last dike segment containing the upper bound
      iUpper = geometry%Coordinates%N - count(geometry%Coordinates%y > yUpper)
      if (iUpper == geometry%Coordinates%N) iUpper = geometry%Coordinates%N - 1

      ! ---------------------------------------------------------------------------------------------
      ! NOTE: dike segments corresponding to indices above cannot be horizontal berms (by definition)
      ! ---------------------------------------------------------------------------------------------

      ! set the horizontal lengths of all segments before the segment with the lower bound to zero
      if (iLower > 1) then
         horzLengths(1:iLower-1) = 0.0d0
      endif

      ! set the horizontal lengths of all segments after the segment with the upper bound to zero
      if (iUpper < geometry%Coordinates%N-1) then
         horzLengths(iUpper+1:geometry%Coordinates%N-1) = 0.0d0
      endif

      ! adjust the horizontal length of the segment containing the lower bound
      dy = yLower - geometry%Coordinates%y(iLower)
      dx = dy / geometry%segmentSlopes(iLower)
      horzLengths(iLower) = horzLengths(iLower) - dx
      
      ! adjust the horizontal length of the segment containing the upper bound
      dy = geometry%Coordinates%y(iUpper+1) - yUpper
      dx = dy / geometry%segmentSlopes(iUpper)
      horzLengths(iUpper) = horzLengths(iUpper) - dx
      
      ! ---------------------------------------------------------------------------------------------
      ! NOTE: calculations above use the fact that the segments are no horizontal berms (tanAlpha<>0)
      ! ---------------------------------------------------------------------------------------------

   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      call GetMSGcalc_horizontal_lengths(error%Message)
   endif

   end subroutine calculateHorzLengths

!> calculateHorzDistance:
!! calculate horizontal distance
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateHorzDistance (geometry, yLower, yUpper, dx, error)
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
   type(tMessage),      intent(inout)  :: error          !< error struct
!
!  Local parameters
!
   real(kind=wp) :: horzLengths(geometry%Coordinates%N-1) !< horizontal lengths segments (m)

! ==========================================================================================================

   ! calculate horizontal lengths
   call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)

   ! calculate horizontal distance
   if (error%errorCode == 0) dx = sum(horzLengths)

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
           message%errorcode = -1
           message%severity = severityError
           formatstr = GetFMTdiffx_negative()
           write(message%message, formatstr) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       else if (diffx < xDiff_min - tol) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           formatstr = GetFMTdiffx_too_small()
           write(message%message, formatstr) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       endif
       if (diffy < 0d0) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           formatstr = GetFMTdiffy_too_small()
           write(message%message, formatstr) yi, ynext
           call addMessage(errorStruct, message)
       endif
       if (roughnessFactor < rFactor_min .or. roughnessFactor > rFactor_max) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           formatstr = GetFMTroughnessfactors_out_of_range()
           write (message%message, formatstr) rFactor_min, rFactor_max, roughnessFactor
           call addMessage(errorStruct, message)
       endif
   enddo
   end subroutine basicGeometryTest
!***********************************************************************************************************
   end module geometryModuleOvertopping
!***********************************************************************************************************
