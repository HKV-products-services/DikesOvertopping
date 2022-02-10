submodule (geometryModuleOvertopping) submDetermineSegmentTypes
   use OvertoppingMessages
   use parametersOvertopping
contains

!> determineSegmentTypes:
!! determine the segment types
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure determineSegmentTypes
!***********************************************************************************************************
   implicit none

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

end procedure determineSegmentTypes

end submodule submDetermineSegmentTypes
