! Copyright (C) Stichting Deltares 2017. All rights reserved.
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
!  Programmer: Bastiaan Kuijper, HKV consultants
!
!  $Id$
!
!***********************************************************************************************************
!
!> core computations for Dikes Overtopping
!
   module mainModuleOvertopping
!***********************************************************************************************************

   use factorModuleOvertopping
   use typeDefinitionsOvertopping
   use formulaModuleOvertopping
   use geometryModuleOvertopping
   use waveRunup
   use OvertoppingMessages

   implicit none

   private
   
   public :: calculateOvertopping, calculateOvertoppingSection, calculateWaveOvertopping
   public :: interpolateResultsSections, checkInputdata, checkModelFactors

   contains
!
!> calculateOvertopping:
!! calculate the overtopping
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateOvertopping (geometry, load, modelFactors, overtopping, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),         intent(in)  :: geometry       !< structure with geometry data
   type (tpLoad),             intent(in)  :: load           !< structure with load parameters
   type (tpOvertoppingInput), intent(in)  :: modelFactors   !< structure with model factors
   type (tpOvertopping),      intent(out) :: overtopping    !< structure with overtopping results
   logical,                   intent(out) :: succes         !< flag for succes
   character(len=*),          intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   type (tpGeometry)        :: geometryMergedBerms  !< structure with merged sequential berms
   real(kind=wp)            :: toe                  !< height of the dike toe (m+NAP)
   real(kind=wp)            :: crest                !< crest height (m+NAP)
   real(kind=wp)            :: h                    !< water level (m+NAP)
   real(kind=wp)            :: Hm0                  !< significant wave height (m)
   real(kind=wp)            :: Tm_10                !< spectral wave period (s)
   real(kind=wp)            :: beta                 !< angle of wave attack (degree)
   real(kind=wp)            :: gammaBeta_z          !< influence factor angle of wave attack 2% wave run-up
   real(kind=wp)            :: gammaBeta_o          !< influence factor angle of wave attack overtopping
   real(kind=wp)            :: L0                   !< wave length (m)
   integer                  :: NwideBerms           !< number of wide berms
   type (tpGeometry)        :: geometrySectionB     !< geometry data with wide berms to ordinary berms
   type (tpGeometry)        :: geometrySectionF     !< geometry data with wide berms to foreshores
   type (tpOvertopping)     :: overtoppingB         !< structure with overtopping results for ordinary berms
   type (tpOvertopping)     :: overtoppingF         !< structure with overtopping results for foreshores
   real(kind=wp), parameter :: tinyWaves = 1d-7     !< waves smaller than tinyWaves can be neglected

! ==========================================================================================================

   ! check load parameters and model factors
   call checkInputdata (geometry, load, modelFactors, succes, errorMessage)
      
   if (succes) then

      toe   = geometry%yCoordinates(1)
      crest = geometry%yCoordinates(geometry%nCoordinates)

      ! initialize local values wave height and wave period
      Hm0   = min(load%Hm0, max(load%h - toe, 0.0d0))
      Tm_10 = load%Tm_10
      
      h = load%h

      ! calculate angle of wave attack
      call calculateAngleWaveAttack (load%phi, geometry%psi, beta)
         
      ! calculate influence factors angle of wave attack
      call calculateGammaBeta (Hm0, Tm_10, beta, gammaBeta_z, gammaBeta_o)
   
      ! check wave height and wave period
      if (abs (Hm0) < tinyWaves .or. isEqualZero (Tm_10)) then

         ! wave height and/or wave period equal to zero
         overtopping%z2 = 0.0d0
         overtopping%Qo = 0.0d0
    
      ! water level below toe of the cross section
      elseif (h < toe) then

         ! water level lower than toe of the cross section
         overtopping%z2 = 0.0d0
         overtopping%Qo = 0.0d0
      else
      
         ! if applicable merge two sequential berms
         call mergeSequentialBerms (geometry, geometryMergedBerms, succes, errorMessage)
         
         ! calculate the wave length
         if (succes) then
            call calculateWaveLength (Tm_10, L0)
         endif
         
         ! if applicable split cross section
         if (succes) then
            call splitCrossSection (geometryMergedBerms, L0, NwideBerms, &
                                    geometrysectionB, geometrysectionF, succes, errorMessage)
         endif

         ! start calculation for each cross section
         if (succes) then

            if (NwideBerms == 0) then

               ! no wide berms (geometrySectionB = geometrySectionF)
               call calculateOvertoppingSection (geometrySectionB, h, Hm0, Tm_10, L0,       &
                                                 gammaBeta_z, gammaBeta_o, modelFactors,    &
                                                 overtopping, succes, errorMessage)
            else

               ! geometrySectionB equals geometry with all wide berms reduced to B=L0/4
               call calculateOvertoppingSection (geometrySectionB, h, Hm0, Tm_10, L0,       &
                                                 gammaBeta_z, gammaBeta_o, modelFactors,    &
                                                 overtoppingB, succes, errorMessage)

               ! geometrySectionF equals geometry with all wide berms extended B=L0
               if (succes) then
                  call calculateOvertoppingSection (geometrySectionF, h, Hm0, Tm_10, L0,    &
                                                    gammaBeta_z, gammaBeta_o, modelFactors, &
                                                    overtoppingF, succes, errorMessage)
               endif
               
               ! interpolation of results for both cross sections
               if (succes) then
                  call interpolateResultsSections (geometryMergedBerms, L0, NwideBerms,     &
                                                   overtoppingB, overtoppingF, overtopping, &
                                                   succes, errorMessage)
               endif
            endif
         endif
         call deallocateGeometry(geometryMergedBerms)
         call deallocateGeometry(geometrysectionB)
         call deallocateGeometry(geometrysectionF)
      endif

   endif

   end subroutine calculateOvertopping

!> calculateOvertoppingSection:
!! calculate the overtopping for a section
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateOvertoppingSection (geometry, h, Hm0, Tm_10, L0, gammaBeta_z, gammaBeta_o, &
                                           modelFactors, overtopping, succes, errorMessage)
!***********************************************************************************************************
!   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),         intent(in)     :: geometry       !< structure with geometry data
   real(kind=wp),             intent(in)     :: h              !< local water level (m+NAP)
   real(kind=wp),             intent(in)     :: Hm0            !< significant wave height (m)
   real(kind=wp),             intent(in)     :: Tm_10          !< spectral wave period (s)
   real(kind=wp),             intent(in)     :: L0             !< wave length (m)
   real(kind=wp),             intent(inout)  :: gammaBeta_z    !< influence angle wave attack wave run-up
   real(kind=wp),             intent(inout)  :: gammaBeta_o    !< influence angle wave attack overtopping
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   type (tpOvertopping),      intent(out)    :: overtopping    !< structure with overtopping results
   logical,                   intent(out)    :: succes         !< flag for succes
   character(len=*),          intent(out)    :: errorMessage   !< error message
!
!  Local parameters
!
   integer           :: foreshoreCase     !< foreshore case
   integer           :: i                 !< counter dike segments
   real(kind=wp)     :: B                 !< width of berm segment (m)
   integer           :: index             !< index coordinates at the end of the foreshore
   real(kind=wp)     :: hBerm             !< average berm height (m)
   real(kind=wp)     :: z2max             !< maximum 2% wave run-up due to foreshore (m)
   real(kind=wp)     :: dH                !< water depth at the end of the foreshore (m)
   real(kind=wp)     :: Hm0_red           !< reduced significant wave height (m)
   type (tpGeometry) :: geometryAdjusted  !< geometry with removed dike segments

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '
   overtopping%z2 = 0.0_wp
   overtopping%Qo = 0.0_wp

   ! initialize foreshore case
   foreshoreCase = 0

   ! determine foreshore case:
   !  0 = no foreshores
   !  1 = local water level below first foreshore
   !  2 = local water level above last foreshore
   !  3 = local water level between foreshores
   !  4 = local water level on foreshore
   if (geometry%NbermSegments > 0) then

      ! loop over possible berm segments
      do i=2, geometry%nCoordinates - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! determine the width of the berm segment
            B = geometry%xCoordDiff(i)

            ! determine if the berm segment is a foreshore
            if (B >= L0) then

               ! compare height of the foreshore to local water level
               if (h < geometry%yCoordinates(i)) then
               
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
                  hBerm = (geometry%yCoordinates(i)+geometry%yCoordinates(i+1))/2
                  
                  ! maximum z2% equals average berm height minus local water level 
                  z2max = hBerm - h

                  ! exit loop over dike segments
                  exit

               elseif (h > geometry%yCoordinates(i+1)) then
               
                  ! ---------------------------------
                  ! local water level above foreshore
                  ! ---------------------------------
                  foreshoreCase = 2

                  ! water depth at the end of the foreshore
                  dH = h - geometry%yCoordinates(i+1)

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

   ! calculate 2% wave run-up and wave overtopping discharge for given foreshore case
   select case (foreshoreCase)

      ! ----------------------
      case (0) ! no foreshores
      ! ----------------------

         ! calculate 2% wave run-up
         call iterationWaveRunup (geometry, h, Hm0, Tm_10, gammaBeta_z, &
                                  modelFactors, overtopping%z2, succes, errorMessage)
      
         ! calculate wave overtopping discharge
         if (succes) then
            if (overtopping%z2 > 0.0d0) then
               call calculateWaveOvertopping (geometry, h, Hm0, Tm_10, overtopping%z2, gammaBeta_o, &
                                              modelFactors, overtopping%Qo, succes, errorMessage)
            else
               overtopping%Qo = 0.0d0
            endif
         endif

      ! ------------------------------------------------
      case (1) ! local water level below first foreshore
      ! ------------------------------------------------

         ! calculate 2% wave run-up
         call iterationWaveRunup (geometry, h, Hm0, Tm_10, gammaBeta_z, &
                                  modelFactors, overtopping%z2, succes, errorMessage)
            
         if (succes) then
         
            ! 2% wave run-up is limited due to foreshore
            overtopping%z2 = min(overtopping%z2, z2max)
         
            ! wave overtopping discharge equals zero
            overtopping%Qo = 0.0d0

         endif

      ! -----------------------------------------------
      case (2) ! local water level above last foreshore
      ! -----------------------------------------------

         ! remove dike segments below local water level
         call removeDikeSegments (geometry, index, geometryAdjusted, succes, errorMessage)

         ! significant wave height is reduced due to foreshore
         Hm0_red = min(Hm0, dH * modelFactors%reductionFactorForeshore)

         ! calculate 2% wave run-up
         if (succes) then
            call iterationWaveRunup (geometryAdjusted, h, Hm0_red, Tm_10, gammaBeta_z, &
                                     modelFactors, overtopping%z2, succes, errorMessage)
         endif

         ! calculate wave overtopping discharge
         if (succes) then
            if (overtopping%z2 > 0.0d0) then
               call calculateWaveOvertopping (geometryAdjusted, h, Hm0_red, Tm_10, overtopping%z2, &
                                              gammaBeta_o, modelFactors, overtopping%Qo, &
                                              succes, errorMessage)
            else
               overtopping%Qo = 0.0d0
            endif
         endif

         call deallocateGeometry(geometryAdjusted)

      ! ---------------------------------------------
      case (3) ! local water level between foreshores
      ! ---------------------------------------------

         ! remove dike segments below local water level
         call removeDikeSegments (geometry, index, geometryAdjusted, succes, errorMessage)

         ! significant wave height is reduced due to foreshore
         Hm0_red = min(Hm0, dH * modelFactors%reductionFactorForeshore)

         ! calculate 2% wave run-up
         if (succes) then
            call iterationWaveRunup (geometryAdjusted, h, Hm0_red, Tm_10, gammaBeta_z, &
                                     modelFactors, overtopping%z2, succes, errorMessage)
         endif
                     
         ! 2% wave run-up is limited due to foreshore
         overtopping%z2 = min(overtopping%z2, z2max)

         ! wave overtopping discharge equals zero
         overtopping%Qo = 0.0d0

         call deallocateGeometry(geometryAdjusted)

      ! ---------------------------------------
      case (4) ! local water level on foreshore
      ! ---------------------------------------

         ! 2% wave run-up equals zero
         overtopping%z2 = 0.0d0

         ! wave overtopping discharge equals zero
         overtopping%Qo = 0.0d0

      end select

   end subroutine calculateOvertoppingSection


!> calculateWaveOvertopping:
!! calculate wave overtopping
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateWaveOvertopping (geometry, h, Hm0, Tm_10, z2, gammaBeta_o, modelFactors, Qo, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),         intent(in)     :: geometry       !< structure with geometry data
   real(kind=wp),             intent(in)     :: h              !< local water level (m+NAP)
   real(kind=wp),             intent(in)     :: Hm0            !< significant wave height (m)
   real(kind=wp),             intent(in)     :: Tm_10          !< spectral wave period (s)
   real(kind=wp),             intent(in)     :: z2             !< 2% wave run-up (m)
   real(kind=wp),             intent(inout)  :: gammaBeta_o    !< influence angle wave attack overtopping
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   real(kind=wp),             intent(out)    :: Qo             !< wave overtopping discharge (m3/m per s)
   logical,                   intent(out)    :: succes         !< flag for succes
   character(len=*),          intent(out)    :: errorMessage   !< error message
!
!  Local parameters
!
   type (tpGeometry) :: geometryFlatBerms !< structure with geometry data with horizontal berms
   real(kind=wp)     :: s0                !< wave steepness
   real(kind=wp)     :: tanAlpha          !< representative slope angle
   real(kind=wp)     :: ksi0              !< breaker parameter
   real(kind=wp)     :: ksi0Limit         !< limit value breaker parameter
   real(kind=wp)     :: gammaB            !< influence factor berms
   real(kind=wp)     :: gammaF            !< influence factor roughness
   
! ==========================================================================================================

   ! calculate wave steepness
   call calculateWaveSteepness (Hm0, Tm_10, s0, succes, errorMessage)
   
   ! if applicable adjust non-horizontal berms
   if (succes) then
      call adjustNonHorizontalBerms (geometry, geometryFlatBerms, succes, errorMessage)
   endif

   ! calculate representative slope angle
   if (succes) then
      call calculateTanAlpha (h, Hm0, z2, geometryFlatBerms, tanAlpha, succes, errorMessage)
   endif

   ! calculate breaker parameter
   if (succes) then
      call calculateBreakerParameter (tanAlpha, s0, ksi0, succes, errorMessage)
   endif

   ! calculate influence factor berms (gammaB)
   if (succes) then
      if (geometry%NbermSegments > 0) then
         call calculateGammaB (h, Hm0, z2, geometryFlatBerms, gammaB, succes, errorMessage)
      else
         gammaB = 1.0d0
      endif
   endif

   call deallocateGeometry(geometryFlatBerms)

   ! calculate limit value breaker parameter
   if (succes) then
      call calculateBreakerLimit (gammaB, ksi0Limit, succes, errorMessage)
   endif

   ! calculate influence factor roughness (gammaF)
   if (succes) then
      call calculateGammaF (h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
   endif
   
   ! if applicable adjust influence factors
   if (succes) then
      call adjustInfluenceFactors (gammaB, gammaF, gammaBeta_o, 2, ksi0, ksi0Limit, succes, errorMessage)
   endif

   ! calculate wave overtopping discharge
   if (succes) then
      call calculateWaveOvertoppingDischarge (h, Hm0, tanAlpha, gammaB, gammaF, gammaBeta_o, ksi0, &
                                              geometry%yCoordinates(geometry%nCoordinates),        &
                                              modelFactors, Qo, succes, errorMessage)
   endif

   end subroutine calculateWaveOvertopping

!> calculateOvertoppingNegativeFreeboard:
!! calculate overtopping in case of negative freeboard
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateOvertoppingNegativeFreeboard (load, geometry, overtopping, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),      intent(in)    :: geometry       !< structure with geometry data
   type (tpLoad),          intent(in)    :: load           !< structure with load parameters
   type (tpOvertopping),   intent(inout) :: overtopping    !< structure with overtopping results
   logical,                intent(out)   :: succes         !< flag for succes
   character(len=*),       intent(out)   :: errorMessage   !< error message
!
!  Local parameters
!
   real(kind=wp)  :: freeBoard               !< overflow depth
   real(kind=wp)  :: overtoppingOverflow     !< overtopping discharge for overflow

! ==========================================================================================================

   freeBoard = load%h - geometry%yCoordinates(geometry%nCoordinates)
   if (freeBoard <= 0.0d0) then 
      succes = .false.
      errorMessage = GetOvertoppingMessage(wl_above_crest_not_allowed)
   else
      overtoppingOverflow = 0.6d0 * sqrt(gravityConstant * (freeBoard ** 3))
      overtopping%Qo = overtopping%Qo + overtoppingOverflow
   endif

   end subroutine calculateOvertoppingNegativeFreeboard 

!> interpolateResultsSections:
!! interpolate results for split cross sections
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine interpolateResultsSections (geometry, L0, NwideBerms, overtoppingB, overtoppingF, &
                                          overtopping, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),      intent(in)  :: geometry       !< structure with geometry data
   real(kind=wp),          intent(in)  :: L0             !< wave length (m)
   integer,                intent(in)  :: NwideBerms     !< number of wide berms
   type (tpOvertopping),   intent(in)  :: overtoppingB   !< structure with overtopping results ordinary berms
   type (tpOvertopping),   intent(in)  :: overtoppingF   !< structure with overtopping results foreshores
   type (tpOvertopping),   intent(out) :: overtopping    !< structure with combined overtopping results
   logical,                intent(out) :: succes         !< flag for succes
   character(len=*),       intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   integer        :: i              !< counter dike segments
   real(kind=wp)  :: B              !< width of berm segment (m)
   real(kind=wp)  :: Bsum           !< total width of wide berms (m)
   real(kind=wp)  :: interpFactor   !< interpolation factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '
   
   ! -------------------------------------------
   ! calculate the total width of the wide berms
   ! -------------------------------------------

   ! initialize total width of wide berms
   Bsum = 0.0d0

   ! loop over possible berm segments
   do i=2, geometry%nCoordinates - 2

      ! determine if the current dike segment is a berm segment
      if (geometry%segmentTypes(i) == 2) then

         ! determine the width of the berm segment
         B = geometry%xCoordDiff(i)

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
      if (interpFactor < 0.0d0) succes = .false.
      if (interpFactor > 1.0d0) succes = .false.
   else
      succes = .false.
   endif

   ! interpolate results
   if (succes) then
      overtopping%z2 = overtoppingB%z2 + interpFactor * (overtoppingF%z2 - overtoppingB%z2)
      overtopping%Qo = overtoppingB%Qo + interpFactor * (overtoppingF%Qo - overtoppingB%Qo)
   else
      errorMessage = GetOvertoppingMessage(interpolation_error_split_cross_sections)
   endif

   end subroutine interpolateResultsSections

!> checkInputdata:
!! check the input data
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine checkInputdata (geometry, load, modelFactors, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry)    ,     intent(in)  :: geometry       !< structure with geometry data
   type (tpLoad)        ,     intent(in)  :: load           !< structure with load parameters
   type (tpOvertoppingInput), intent(in)  :: modelFactors   !< structure with model factors
   logical,                   intent(out) :: succes         !< flag for succes
   character(len=*),          intent(out) :: errorMessage   !< error message
!
!  local parameters
!
   integer                                :: ierr           !< number of validation messages

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! check local water level
   if (succes) then
      if (isnan(load%h)) then
         succes = .false.
         errorMessage = 'load%h is NaN'
      else if (load%h > geometry%yCoordinates(geometry%nCoordinates)) then 
         succes = .false.
         errorMessage = GetOvertoppingMessage(wl_above_crest)
      endif
   endif

   ! check wave height and period
   if (succes) then
      if (load%Hm0   < 0.0d0 .or. isnan(load%Hm0)) succes = .false.
      if (load%Tm_10 < 0.0d0 .or. isnan(load%Tm_10)) succes = .false.
      if (.not. succes) then
         errorMessage = GetOvertoppingMessage(wave_height_or_periode_less_zero)
      endif
   endif

   ! check wave direction
   if (succes) then
      if ((load%phi < 0.0d0) .or. (load%phi > 360.0d0) .or. isnan(load%phi)) then
         succes = .false.
         errorMessage = GetOvertoppingMessage(wave_direction_not_in_range)
      endif
   endif

   if (succes) then
      call checkModelFactors (modelFactors, 1, errorMessage, ierr)
      succes = (ierr == 0)
   endif

   end subroutine checkInputdata

!> checkModelFactors:
!! check the input data
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine checkModelFactors (modelFactors, dimErrMessage, errorMessages, ierr)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type (tpOvertoppingInput), intent(in)  :: modelFactors                   !< structure with model factors
   integer,                   intent(in)  :: dimErrMessage                  !< max. number of error messages
   integer,                   intent(out) :: ierr                           !< number of errors found
   character(len=*),          intent(out) :: errorMessages(dimErrMessage)   !< error message
!
!  Local parameters
!
   integer           :: i        !< counter model factors
   character(len=32) :: par_txt  !< description model factor
   real(kind=wp)     :: par      !< value model factor
   real(kind=wp)     :: par_min  !< minimal value model factor
   real(kind=wp)     :: par_max  !< maximal value model factor

   ierr = 0
   ! loop over model factors
   do i=4, 8
      ! determine description model factor
      select case (i)
         case (4)
             par_txt = GetOvertoppingParameter(par_fB)
         case (5)
             par_txt = GetOvertoppingParameter(par_fN)
         case (6)
             par_txt = GetOvertoppingParameter(par_fS)
         case (7)
             par_txt = GetOvertoppingParameter(par_2percent_wave_runup)
         case (8)
             par_txt = GetOvertoppingParameter(reductionFactorForeshore)
      end select
      ! determine value and minimum/maximum model factor
      select case (i)
         case (4)
             par = modelFactors%factorDeterminationQ_b_f_b
             par_min = fB_min
             par_max = fB_max
         case (5)
             par = modelFactors%factorDeterminationQ_b_f_n
             par_min = fN_min
             par_max = fN_max
         case (6)
             par = modelFactors%fshallow
             par_min = fS_min
             par_max = fS_max
         case (7)
             par = modelFactors%m_z2
             par_min = mz2_min
             par_max = mz2_max
         case (8)
             par = modelFactors%reductionFactorForeshore
             par_min = foreshore_min
             par_max = foreshore_max
      end select
      ! check value model factor
      if (ierr < dimErrMessage) then
         if ((par < par_min) .or. (par > par_max)) then
            ierr = ierr + 1
            if (par_max == huge(par_max)) then
               write (errorMessages(ierr), GetOvertoppingFormat(model_factor_smaller_than)) trim(par_txt), par_min
            else
               write (errorMessages(ierr), GetOvertoppingFormat(model_factor_not_between)) trim(par_txt), par_min, par_max
            endif
         endif
      endif
   enddo

   end subroutine checkModelFactors

!***********************************************************************************************************
   end module mainModuleOvertopping
!***********************************************************************************************************
