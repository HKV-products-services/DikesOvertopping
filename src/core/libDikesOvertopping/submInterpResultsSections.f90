submodule (mainModuleOvertopping) submInterpResultsSections
contains

!> interpolateResultsSections:
!! interpolate results for split cross sections
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure interpolateResultsSections
!***********************************************************************************************************
   implicit none
!
   integer        :: i              !< counter dike segments
   real(kind=wp)  :: B              !< width of berm segment (m)
   real(kind=wp)  :: Bsum           !< total width of wide berms (m)
   real(kind=wp)  :: interpFactor   !< interpolation factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! -------------------------------------------
   ! calculate the total width of the wide berms
   ! -------------------------------------------

   ! initialize total width of wide berms
   Bsum = 0.0d0

   ! loop over possible berm segments
   do i=2, geometry%Coordinates%N - 2

      ! determine if the current dike segment is a berm segment
      if (geometry%segmentTypes(i) == 2) then

         ! determine the width of the berm segment
         B = geometry%CoordDiff%x(i)

         ! add to total width if the berm segment is a wide berm
         if ((B < L0) .and. (B > 0.25d0*L0)) then
            Bsum = Bsum + B
         endif

      endif

   enddo

   ! -----------------------------------
   ! interpolate the overtopping results
   ! -----------------------------------

   ! calculate interpolation factor
   if ((NwideBerms > 0) .and. (L0 > 0.0d0)) then
      interpFactor = (Bsum - NwideBerms * 0.25d0*L0) / (NwideBerms * 0.75d0*L0)
      if (interpFactor < 0.0d0) error%errorCode = 1
      if (interpFactor > 1.0d0) error%errorCode = 1
   else
      error%errorCode = 1
   endif

   ! interpolate results
   if (error%errorCode == 0) then
      overtopping%z2 = overtoppingB%z2 + interpFactor * (overtoppingF%z2 - overtoppingB%z2)
      overtopping%Qo = overtoppingB%Qo + interpFactor * (overtoppingF%Qo - overtoppingB%Qo)
   else
      call GetMSGinterpolation_error_split_cross_sections(error%Message)
   endif

end procedure interpolateResultsSections

end submodule submInterpResultsSections
