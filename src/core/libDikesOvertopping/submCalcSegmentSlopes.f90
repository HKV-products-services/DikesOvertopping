submodule (geometryModuleOvertopping) submCalcSegmentSlopes
contains

!> calculateSegmentSlopes:
!! calculate the segment slopes
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateSegmentSlopes
!***********************************************************************************************************
   implicit none

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

end procedure calculateSegmentSlopes

end submodule submCalcSegmentSlopes

