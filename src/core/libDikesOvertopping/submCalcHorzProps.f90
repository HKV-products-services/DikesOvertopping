submodule (geometryModuleOvertopping) submCalcHorzProps
   use OvertoppingMessages
contains

!> calculateHorzLengths:
!! calculate horizontal lengths
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateHorzLengths
!***********************************************************************************************************
   implicit none
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

end procedure calculateHorzLengths

!> calculateHorzDistance:
!! calculate horizontal distance
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateHorzDistance
!***********************************************************************************************************
   implicit none
!
   real(kind=wp) :: horzLengths(geometry%Coordinates%N-1) !< horizontal lengths segments (m)

! ==========================================================================================================

   ! calculate horizontal lengths
   call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)

   ! calculate horizontal distance
   if (error%errorCode == 0) dx = sum(horzLengths)

end procedure calculateHorzDistance

end submodule submCalcHorzProps
