submodule (geometryModuleOvertopping) submRemoveSegments
   use OvertoppingMessages
contains

!> removeBerms:
!! remove berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure removeBerms
!***********************************************************************************************************
   implicit none
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

end procedure removeBerms

!> removeDikeSegments:
!! remove dike segments
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure removeDikeSegments
!***********************************************************************************************************
   implicit none

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

end procedure removeDikeSegments

end submodule submRemoveSegments
