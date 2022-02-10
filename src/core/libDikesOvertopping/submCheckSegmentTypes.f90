submodule (geometryModuleOvertopping) submCheckSegmentTypes
   use OvertoppingMessages
contains

! validation routine for segment types
!!   @ingroup LibOvertopping
module procedure checkSegmentTypes

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
end procedure checkSegmentTypes

end submodule submCheckSegmentTypes

