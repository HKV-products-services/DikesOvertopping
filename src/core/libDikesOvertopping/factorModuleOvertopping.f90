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
!! This file contains a module with functions for the slope angle and influence factors
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants
!
!  $Id$
!
!***********************************************************************************************************
!
!> functions for the slope angle and influence factors
!
   module factorModuleOvertopping
!***********************************************************************************************************

   use typeDefinitionsOvertopping
   use geometryModuleOvertopping
   use precision, only : wp, pi
   use OvertoppingMessages
   use errorMessages

   implicit none

   private

   public :: calculateTanAlpha  ! representative slope angle
   public :: calculateGammaBeta ! influence factor angle of wave attack
   public :: calculateGammaF    ! influence factor roughness
   public :: calculateGammaB    ! influence factor berms
!
    contains
!
!> calculateTanAlpha
!! representative slope angle
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateTanAlpha (load, z2, geometry, tanAlpha, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type(tpLoadX),            intent(in   ) :: load           !< load struct
   real(kind=wp),            intent(in   ) :: z2             !< 2% wave run-up (m)
   type(tpGeometry), target, intent(in   ) :: geometry       !< structure with geometry data
   real(kind=wp),            intent(  out) :: tanAlpha       !< representative slope angle
   type(tMessage),           intent(inout) :: error          !< error struct
!
!  Local parameters
!
   type(tpGeometry), pointer :: geometryNoBerms        !< geometry without berms
   type(tpGeometry), pointer :: LocalGeometryNoBerms   !< geometry without berms, as in the computations
   real(kind=wp)             :: yLower                 !< y-coordinate lower bound representative slope (m+NAP)
   real(kind=wp)             :: yUpper                 !< y-coordinate upper bound representative slope (m+NAP)
   real(kind=wp)             :: dx                     !< horizontal distance between lower and upper bound (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   if (geometry%splitId == 'B') then
      geometryNoBerms => geometry%parent%geometryNoBerms(2)
   else
      geometryNoBerms => geometry%parent%geometryNoBerms(1)
   end if

   ! check local water level not lower than dike toe (first y-coordinate)
   !   and local water level not higher than crest level (last y-coordinate)
   if (load%h < geometry%Coordinates%y(1) .or. &
       load%h > geometry%Coordinates%y(geometry%Coordinates%N)) then
      error%errorCode = 1
   endif

   ! determine cross section without berms
   if (error%errorCode == 0) then
      if (geometry%NbermSegments > 0) then
         if (geometryNoBerms%Coordinates%N == 0) then
            call removeBerms (geometry, geometryNoBerms, error)
         endif
         LocalGeometryNoBerms => geometryNoBerms
      else
         LocalGeometryNoBerms => geometry
      endif
   endif

   ! calculate representative slope angle
   if (error%errorCode == 0) then

      ! determine y-coordinates lower and upper bound representative slope
      yLower = max(load%h-1.5d0*load%Hm0, LocalGeometryNoBerms%Coordinates%y(1))
      yUpper = min(load%h+z2,      LocalGeometryNoBerms%Coordinates%y(LocalGeometryNoBerms%Coordinates%N))

      ! calculate horizontal distance between lower and upper bound
      call calculateHorzDistance (LocalGeometryNoBerms, yLower, yUpper, dx, error)
      ! calculate representative slope angle
      if (error%errorCode == 0) then
         if (dx > 0.0) then
            tanAlpha = (yUpper - yLower) / dx
         else
            error%errorCode = 1
            call GetMSGcalc_representative_slope_angle(error%Message)
         endif
      endif

   endif

   end subroutine calculateTanAlpha

!> calculateGammaBeta
!! influence factor angle of wave attack
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaBeta (load, beta, gamma_z, gamma_o)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type(tpLoadX),            intent(inout) :: load     !< load struct
   real(kind=wp),            intent(in)    :: beta     !< angle of wave attack (degree)
   type(tpInfluencefactors), intent(out)   :: gamma_z  !< influence factor angle of wave attack 2% wave run-up
   type(tpInfluencefactors), intent(out)   :: gamma_o  !< influence factor angle of wave attack overtopping

! ==========================================================================================================

   ! calculate influence factors angle of wave attack for 2% wave run-up and overtopping
   gamma_z%gammabeta = 1d0 - 0.0022d0 * min(beta,80.0d0)
   gamma_o%gammabeta = 1d0 - 0.0033d0 * min(beta,80.0d0)

   ! adjustment of the wave parameters if beta > 80
   if (beta > 80.0d0) then

      if (beta > 110.0d0) then
         ! beta > 110
         load%Hm0   = 0.0d0
         load%Tm_10 = 0.0d0
       else
         ! 80 < beta <= 110
         load%Hm0   = load%Hm0   *      (110.0d0 - beta)/30.0d0
         load%Tm_10 = load%Tm_10 * sqrt((110.0d0 - beta)/30.0d0)
      endif

   endif

   end subroutine calculateGammaBeta

!> calculateGammaF
!! influence factor roughness
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaF (h, ksi0, ksi0Limit, gamma, z2, geometry, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),            intent(in   ) :: h              !< local water level (m+NAP)
   real(kind=wp),            intent(in   ) :: ksi0           !< breaker parameter
   real(kind=wp),            intent(in   ) :: ksi0Limit      !< limit value breaker parameter
   type(tpInfluencefactors), intent(inout) :: gamma          !< influence factors
   real(kind=wp),            intent(in   ) :: z2             !< 2% wave run-up (m)
   type(tpGeometry),         intent(in   ) :: geometry       !< structure with geometry data
   type(tMessage),           intent(inout) :: error          !< error struct
!
!  Local parameters
!
   real(kind=wp)              :: rFactors(geometry%Coordinates%N-1)     !< roughness factors  of segments with influence
   real(kind=wp)              :: horzLengths(geometry%Coordinates%N-1)  !< horizontal lengths of segments with influence (m)
   real(kind=wp)              :: yLower          !< y-coordinate lower bound segments with influence (m+NAP)
   real(kind=wp)              :: yUpper          !< y-coordinate upper bound segments with influence (m+NAP)
   integer                    :: iLower          !< index dike segment lower bound
   integer                    :: iUpper          !< index dike segment upper bound
   real(kind=wp)              :: dy1             !< delta y in calculation yLower
   real(kind=wp)              :: dy2             !< delta y in calculation yUpper

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! local water level not lower than dike toe (first y-coordinate)
   if (h < geometry%Coordinates%y(1)) then
      error%errorCode = 1
      call GetMSG_calculateGammaFtoLow(error%Message)
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (h > geometry%Coordinates%y(geometry%Coordinates%N)) then
      error%errorCode = 1
      call GetMSG_calculateGammaFtoHigh(error%Message)
   endif

   if (error%errorCode == 0) then
      ! determine y-coordinates lower and upper bound segments with influence
      dy1 = max(1d-5, 0.25d0*z2)
      dy2 = max(1d-5, 0.50d0*z2)
      yLower = max(h-dy1, geometry%Coordinates%y(1))
      yUpper = min(h+dy2, geometry%Coordinates%y(geometry%Coordinates%N))

      ! --------------------------------------------------
      ! determine roughness factors and horizontal lengths
      ! --------------------------------------------------

      call determineRoughnessFactors()

      ! --------------------------------------------------
      ! calculate (and adjust) influence factor roughness
      ! --------------------------------------------------

      call calculateInfluenceFactorRoughness()

   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
       if (error%Message == ' ') then
           call GetMSG_calc_influence_roughness(error%Message)
       endif
   endif

   contains

   subroutine determineRoughnessFactors()

      ! initialize roughness factors
      rFactors = geometry%roughnessFactors

      ! determine index first dike segment containing the lower bound
      iLower = count(geometry%Coordinates%y < yLower)
      if (iLower == 0) iLower = 1

      ! determine index last dike segment containing the upper bound
      iUpper = geometry%Coordinates%N - count(geometry%Coordinates%y > yUpper)
      if (iUpper == geometry%Coordinates%N) iUpper = geometry%Coordinates%N - 1

      ! set the roughness of all segments before the segment with the lower bound to zero
      if (iLower > 1) then
         rFactors(1:iLower-1) = 0.0d0
      endif

      ! set the roughness of all segments after the segment with the upper bound to zero
      if (iUpper < geometry%Coordinates%N-1) then
         rFactors(iUpper+1:geometry%Coordinates%N-1) = 0.0d0
      endif
   end subroutine determineRoughnessFactors

   subroutine calculateInfluenceFactorRoughness()
      real(kind=wp)              :: sum_horzLengths !< sum of all horzLengths
      real(kind=wp), parameter   :: one = 1.0_wp    !< constant in comparision with breaker parameters
      real(kind=wp), parameter   :: ten = 10.0_wp   !< constant in comparision with breaker parameters

      if (geometry%Coordinates%N == 2) then
          gamma%gammaF = rFactors(1)
      else
          call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)
          if (error%errorCode == 0) then
              sum_horzLengths = sum(horzLengths)
              if (sum_horzLengths > 0.0d0) then
                  gamma%gammaF = dot_product (horzLengths, rFactors) / sum_horzLengths
              else
                  gamma%gammaF = rFactors(iLower)
              endif
          endif
      endif

      ! adjust influence factor if breaker parameter is greater than limit value
      if (error%errorCode == 0 .and. (ksi0 > ksi0Limit)) then
         if (gamma%gammaB*ksi0Limit < ten) then
            gamma%gammaF = gamma%gammaF + (one-gamma%gammaF)*gamma%gammaB*(ksi0-ksi0Limit)/(ten-gamma%gammaB*ksi0Limit)
         endif
         gamma%gammaF = min(one, gamma%gammaF)
      endif
   end subroutine calculateInfluenceFactorRoughness

   end subroutine calculateGammaF

!> calculateGammaB
!! influence factor berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaB (load, z2, geometry, gammaB, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type(tpLoadX),          intent(in   ) :: load           !< load struct
   real(kind=wp),          intent(in   ) :: z2             !< 2% wave run-up (m)
   type(tpGeometry),       intent(in   ) :: geometry       !< structure with geometry data
   real(kind=wp),          intent(  out) :: gammaB         !< influence factor berms
   type(tMessage),         intent(inout) :: error          !< error struct
!
!  Local parameters
!
   integer        :: N                          !< counter berm segments
   real(kind=wp)  :: B (geometry%NbermSegments) !< berm widths (m)
   real(kind=wp)  :: LB(geometry%NbermSegments) !< berm influence lengths (m)
   real(kind=wp)  :: rB(geometry%NbermSegments) !< relative berm widths
   real(kind=wp)  :: rD(geometry%NbermSegments) !< relative depths

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! local water level not lower than dike toe (first y-coordinate)
   if (load%h < geometry%Coordinates%y(1)) then
      error%errorCode = 1
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (load%h > geometry%Coordinates%y(geometry%Coordinates%N)) then
      error%errorCode = 1
   endif


   if (error%errorCode == 0) then

      call calculateBermProperties()

   endif

   ! check number of berm segments
   if (N /= geometry%NbermSegments) error%errorCode = 1

   if (error%errorCode == 0) then

      call calculateInfluenceFactorBerms()
   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      if (error%Message == ' ') then
         call GetMSGcalc_influence_berms(error%Message)
      endif
   endif

   contains

   ! ---------------------------------------------------------------------
   ! calculate berm widths, heights, influence lengths and relative depths
   ! ---------------------------------------------------------------------
   subroutine calculateBermProperties()
      integer        :: i         !< counter dike segments
      real(kind=wp)  :: yLower    !< y-coordinate lower bound influence length (m+NAP)
      real(kind=wp)  :: yUpper    !< y-coordinate upper bound influence length (m+NAP)
      real(kind=wp)  :: dH        !< berm depths (m)
      real(kind=wp)  :: hB        !< berm heights (m+NAP)

      ! initialize berm counter
      N = 0

      ! loop over possible berm segments
      do i=2, geometry%Coordinates%N - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! edit counter berm segments
            N = N+1

            ! check if the berm segment is a horizontal berm
            if (geometry%segmentSlopes(i) > 0.0d0) then
               error%errorCode = 1
               exit
            end if

            ! determine the width of the berm segment
            B(N)  = geometry%CoordDiff%x(i)

            ! determine the height of the (horizontal) berm segment
            hB = geometry%Coordinates%y(i)

            ! calculate the influence length
            yLower = max(hB-load%Hm0, geometry%Coordinates%y(1))
            yUpper = min(hB+load%Hm0, geometry%Coordinates%y(geometry%Coordinates%N))
            call calculateHorzDistance (geometry, yLower, yUpper, LB(N), error)

            ! calculate relative berm width
            if (error%errorCode == 0 .and. LB(N) > 0.0d0) then
               rB(N) = B(N) / LB(N)
            else
               error%errorCode = 1
               exit
            endif

            ! calculate the berm depth
            dH = load%h - hB

            ! calculate the influence of the berm depth
            if (dH < 0.0d0) then ! local water level below the berm (dH<0)

               if (z2 > -dH) then
                  ! local water level + z2% above the berm (influence)
                  rD(N) = 0.5d0 - 0.5d0 * cos(pi*dH/z2)
               else
                  ! local water level + z2% on or below the berm (no influence)
                  rD(N) = 1.0d0
               endif

            else ! local water level on or above the berm (dH>=0)

               if (dH < 2.0d0*load%Hm0) then
                  ! local water level less than 2*Hm0 above the berm (influence)
                  rD(N) = 0.5d0 - 0.5d0 * cos(pi*dH/(2.0d0*load%Hm0))
               else
                  ! local water level 2*Hm0 or more above the berm (no influence)
                  rD(N) = 1.0d0
               endif

            endif

         endif
      enddo
   end subroutine calculateBermProperties

   ! ------------------------------------
   ! calculate influence factor for berms
   ! ------------------------------------
   subroutine calculateInfluenceFactorBerms()
      real(kind=wp)  :: f1(geometry%NbermSegments) !< vector with help factors for combining influence factors
      real(kind=wp)  :: f2(geometry%NbermSegments) !< vector with help factors for combining influence factors
      real(kind=wp)  :: f3                         !< help factor for combining influence factors
      real(kind=wp)  :: f4                         !< help factor for combining influence factors
      real(kind=wp)  :: f5                         !< help factor for combining influence factors

      select case (N)
      case (0)

         ! no berms present
         gammaB = 1.0d0

      case (1)

         ! one berm present
         gammaB = 1.0d0 - rB(1) * (1.0d0-rD(1))

      case default

         ! more berms present
         f1 =     B  * (1.0d0-rD)
         f2 = (LB-B) * (1.0d0-rD)
         f3 = sum(f1)
         if (f3 > 0.0d0) then
            f4 = sum(f1 * f2)
            f5 = sum(f1 * (1.0d0-rD))
            gammaB = 1.0d0 - f5 / (f4/f3 + f3)
         else
            gammaB = 1.0d0
         endif
      end select

      ! ---------------------------------
      ! adjust influence factor for berms
      ! ---------------------------------
      gammaB = max(0.6d0, gammaB)
   end subroutine calculateInfluenceFactorBerms

   end subroutine calculateGammaB

!***********************************************************************************************************
   end module factorModuleOvertopping
!***********************************************************************************************************
