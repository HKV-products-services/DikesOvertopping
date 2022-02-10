submodule (mainModuleOvertopping) submCalcOvertopSection
   use waveRunup
   use geometryModuleOvertopping
contains

!> calculateOvertoppingSection:
!! calculate the overtopping for a section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateOvertoppingSection
!***********************************************************************************************************
!   implicit none
!
   integer                    :: foreshoreCase     !< foreshore case
   real(kind=wp)              :: dH                !< water depth at the end of the foreshore (m)
   integer                    :: index             !< index coordinates at the end of the foreshore
   real(kind=wp)              :: z2max             !< maximum 2% wave run-up due to foreshore (m)
   type (tpLoadX)             :: load_red          !< load with reduced Hm0
   type (tpGeometry), pointer :: geometryAdjusted  !< geometry with removed dike segments

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0
   overtopping%z2 = 0.0_wp
   overtopping%Qo = 0.0_wp

   geometryAdjusted => geometry%parent%geometryRemoveDikeSegments

   foreshoreCase = getForeshoreCase ()

   ! calculate 2% wave run-up and wave overtopping discharge for given foreshore case
   select case (foreshoreCase)

      ! ----------------------
      case (0) ! no foreshores
      ! ----------------------

         ! calculate 2% wave run-up
         call iterationWaveRunup (geometry, load, gamma_z, modelFactors, overtopping%z2, error)

         ! calculate wave overtopping discharge
         if (error%errorCode == 0) then
            if (overtopping%z2 > 0.0d0) then
               call calculateWaveOvertopping (geometry, load, overtopping%z2, gamma_o, &
                                              modelFactors, overtopping%Qo, error)
            else
               overtopping%Qo = 0.0d0
            endif
         endif

      ! ------------------------------------------------
      case (1) ! local water level below first foreshore
      ! ------------------------------------------------

         ! calculate 2% wave run-up
         call iterationWaveRunup (geometry, load, gamma_z, modelFactors, overtopping%z2, error)

         if (error%errorCode == 0) then

            ! 2% wave run-up is limited due to foreshore
            overtopping%z2 = min(overtopping%z2, z2max)
         
            ! wave overtopping discharge equals zero
            overtopping%Qo = 0.0d0

         endif

      ! -----------------------------------------------
      case (2) ! local water level above last foreshore
      ! -----------------------------------------------

         ! remove dike segments below local water level
         call removeDikeSegments (geometry, index, geometryAdjusted, error)

         ! significant wave height is reduced due to foreshore
         load_red = load
         load_red%Hm0 = min(load%Hm0, dH * modelFactors%reductionFactorForeshore)

         ! calculate 2% wave run-up
         if (error%errorCode == 0) then
            call iterationWaveRunup (geometryAdjusted, load_red, gamma_z, modelFactors, overtopping%z2, error)
         endif

         ! calculate wave overtopping discharge
         if (error%errorCode == 0) then
            if (overtopping%z2 > 0.0d0) then
               call calculateWaveOvertopping (geometryAdjusted, load_red, overtopping%z2, &
                                              gamma_o, modelFactors, overtopping%Qo, error)
            else
               overtopping%Qo = 0.0d0
            endif
         endif

      ! ---------------------------------------------
      case (3) ! local water level between foreshores
      ! ---------------------------------------------

         ! remove dike segments below local water level
         call removeDikeSegments (geometry, index, geometryAdjusted, error)

         ! significant wave height is reduced due to foreshore
         load_red = load
         load_red%Hm0 = min(load%Hm0, dH * modelFactors%reductionFactorForeshore)

         ! calculate 2% wave run-up
         if (error%errorCode == 0) then
            call iterationWaveRunup (geometryAdjusted, load_red, gamma_z, modelFactors, overtopping%z2, error)
         endif

         ! 2% wave run-up is limited due to foreshore
         overtopping%z2 = min(overtopping%z2, z2max)

         ! wave overtopping discharge equals zero
         overtopping%Qo = 0.0d0

      ! ---------------------------------------
      case (4) ! local water level on foreshore
      ! ---------------------------------------

         ! 2% wave run-up equals zero
         overtopping%z2 = 0.0d0

         ! wave overtopping discharge equals zero
         overtopping%Qo = 0.0d0

      end select

contains

!> helper function
function getForeshoreCase () result(foreshoreCase)
   integer           :: foreshoreCase  !< function result

   integer           :: i              !< loop counter
   real(kind=wp)     :: hBerm          !< average berm height (m)
   real(kind=wp)     :: B              !< width of berm segment (m)

   ! determine foreshore case:
   !  0 = no foreshores
   !  1 = local water level below first foreshore
   !  2 = local water level above last foreshore
   !  3 = local water level between foreshores
   !  4 = local water level on foreshore
   foreshoreCase = 0
   if (geometry%NbermSegments > 0) then

      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! determine the width of the berm segment
            B = geometry%CoordDiff%x(i)

            ! determine if the berm segment is a foreshore
            if (B >= load%L0) then

               ! compare height of the foreshore to local water level
               if (load%h < geometry%Coordinates%y(i)) then

                  ! ---------------------------------
                  ! local water level below foreshore
                  ! ---------------------------------

                  ! foreshore case depends on current status
                  if (foreshoreCase == 0) then
                     foreshoreCase = 1 ! below first foreshore (no foreshores so far)
                  else 
                     foreshoreCase = 3 ! between foreshores (above first foreshore)
                  endif

                  ! calculate average berm height
                  hBerm = (geometry%Coordinates%y(i)+geometry%Coordinates%y(i+1))/2

                  ! maximum z2% equals average berm height minus local water level
                  z2max = hBerm - load%h

                  ! exit loop over dike segments
                  exit

               elseif (load%h > geometry%Coordinates%y(i+1)) then

                  ! ---------------------------------
                  ! local water level above foreshore
                  ! ---------------------------------
                  foreshoreCase = 2

                  ! water depth at the end of the foreshore
                  dH = load%h - geometry%Coordinates%y(i+1)

                  ! index coordinates at the end of the foreshore
                  index = i+1

                  ! (no exit: local water level may be on or below another foreshore)

               else

                  ! ------------------------------
                  ! local water level on foreshore
                  ! ------------------------------
                  foreshoreCase = 4

                  ! exit loop over dike segments
                  exit

               endif
            endif
         endif
      enddo
   endif

end function getForeshoreCase

end procedure calculateOvertoppingSection

end submodule submCalcOvertopSection
