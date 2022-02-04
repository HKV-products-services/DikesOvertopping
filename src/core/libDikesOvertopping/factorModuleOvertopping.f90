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

   ! local water level not lower than dike toe (first y-coordinate)
   if (load%h < geometry%yCoordinates(1)) then
      error%errorCode = 1
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (load%h > geometry%yCoordinates(geometry%nCoordinates)) then
      error%errorCode = 1
   endif

   ! determine cross section without berms
   if (error%errorCode == 0) then
      if (geometry%NbermSegments > 0) then
         if (geometryNoBerms%nCoordinates == 0) then
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
      yLower = max(load%h-1.5d0*load%Hm0, LocalGeometryNoBerms%yCoordinates(1))
      yUpper = min(load%h+z2,      LocalGeometryNoBerms%yCoordinates(LocalGeometryNoBerms%nCoordinates))

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
   subroutine calculateGammaBeta (load, beta, gammaBeta_z, gammaBeta_o)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   type(tpLoadX), intent(inout)  :: load        !< load struct
   real(kind=wp), intent(in)     :: beta        !< angle of wave attack (degree)
   real(kind=wp), intent(out)    :: gammaBeta_z !< influence factor angle of wave attack 2% wave run-up
   real(kind=wp), intent(out)    :: gammaBeta_o !< influence factor angle of wave attack overtopping

! ==========================================================================================================

   ! calculate influence factors angle of wave attack for 2% wave run-up and overtopping
   gammaBeta_z = 1d0 - 0.0022d0 * min(beta,80.0d0)
   gammaBeta_o = 1d0 - 0.0033d0 * min(beta,80.0d0)

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
   subroutine calculateGammaF (h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),          intent(in   ) :: h              !< local water level (m+NAP)
   real(kind=wp),          intent(in   ) :: ksi0           !< breaker parameter
   real(kind=wp),          intent(in   ) :: ksi0Limit      !< limit value breaker parameter
   real(kind=wp),          intent(in   ) :: gammaB         !< influence factor berms
   real(kind=wp),          intent(in   ) :: z2             !< 2% wave run-up (m)
   type(tpGeometry),       intent(in   ) :: geometry       !< structure with geometry data
   real(kind=wp),          intent(  out) :: gammaF         !< influence factor roughness
   type(tMessage),         intent(inout) :: error          !< error struct
!
!  Local parameters
!
   real(kind=wp)              :: rFactors(geometry%nCoordinates-1)     !< roughness factors  of segments with influence
   real(kind=wp)              :: horzLengths(geometry%nCoordinates-1)  !< horizontal lengths of segments with influence (m)
   real(kind=wp)              :: yLower          !< y-coordinate lower bound segments with influence (m+NAP)
   real(kind=wp)              :: yUpper          !< y-coordinate upper bound segments with influence (m+NAP)
   integer                    :: iLower          !< index dike segment lower bound
   integer                    :: iUpper          !< index dike segment upper bound
   real(kind=wp)              :: sum_horzLengths !< sum of all horzLengths
   real(kind=wp), parameter   :: one = 1.0_wp    !< constant in comparision with breaker parameters
   real(kind=wp), parameter   :: ten = 10.0_wp   !< constant in comparision with breaker parameters
   real(kind=wp)              :: dy1             !< delta y in calculation yLower
   real(kind=wp)              :: dy2             !< delta y in calculation yUpper

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! local water level not lower than dike toe (first y-coordinate)
   if (h < geometry%yCoordinates(1)) then
      error%errorCode = 1
      call GetMSG_calculateGammaFtoLow(error%Message)
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (h > geometry%yCoordinates(geometry%nCoordinates)) then
      error%errorCode = 1
      call GetMSG_calculateGammaFtoHigh(error%Message)
   endif

   if (error%errorCode == 0) then
      ! determine y-coordinates lower and upper bound segments with influence
      dy1 = max(1d-5, 0.25d0*z2)
      dy2 = max(1d-5, 0.50d0*z2)
      yLower = max(h-dy1, geometry%yCoordinates(1))
      yUpper = min(h+dy2, geometry%yCoordinates(geometry%nCoordinates))

      ! --------------------------------------------------
      ! determine roughness factors and horizontal lengths
      ! --------------------------------------------------

      ! initialize roughness factors
      rFactors = geometry%roughnessFactors

      ! determine index first dike segment containing the lower bound
      iLower = count(geometry%yCoordinates < yLower)
      if (iLower == 0) iLower = 1

      ! determine index last dike segment containing the upper bound
      iUpper = geometry%nCoordinates - count(geometry%yCoordinates > yUpper)
      if (iUpper == geometry%nCoordinates) iUpper = geometry%nCoordinates - 1

      ! set the roughness of all segments before the segment with the lower bound to zero
      if (iLower > 1) then
         rFactors(1:iLower-1) = 0.0d0
      endif

      ! set the roughness of all segments after the segment with the upper bound to zero
      if (iUpper < geometry%nCoordinates-1) then
         rFactors(iUpper+1:geometry%nCoordinates-1) = 0.0d0
      endif


      ! --------------------------------------------------
      ! calculate (and adjust) influence factor roughness
      ! --------------------------------------------------

      if (geometry%nCoordinates == 2) then
          gammaF = rFactors(1)
      else
          call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, error)
          if (error%errorCode == 0) then
              sum_horzLengths = sum(horzLengths)
              if (sum_horzLengths > 0.0d0) then
                  gammaF = dot_product (horzLengths, rFactors) / sum_horzLengths
              else
                  gammaF = rFactors(iLower)
              endif
          endif
      endif

      ! adjust influence factor if breaker parameter is greater than limit value
      if (error%errorCode == 0 .and. (ksi0 > ksi0Limit)) then
         if (gammaB*ksi0Limit < ten) then
            gammaF = gammaF + (one-gammaF)*gammaB*(ksi0-ksi0Limit)/(ten-gammaB*ksi0Limit)
         endif
         gammaF = min(one, gammaF)
      endif

   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
       if (error%Message == ' ') then
           call GetMSG_calc_influence_roughness(error%Message)
       endif
   endif

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
   integer        :: i                          !< counter dike segments
   integer        :: N                          !< counter berm segmetns
   real(kind=wp)  :: B (geometry%NbermSegments) !< berm widths (m)
   real(kind=wp)  :: hB(geometry%NbermSegments) !< berm heights (m+NAP)
   real(kind=wp)  :: LB(geometry%NbermSegments) !< berm influence lengths (m)
   real(kind=wp)  :: rB(geometry%NbermSegments) !< relative berm widths
   real(kind=wp)  :: dH(geometry%NbermSegments) !< berm depths (m)
   real(kind=wp)  :: rD(geometry%NbermSegments) !< relative depths
   real(kind=wp)  :: yLower                     !< y-coordinate lower bound influence length (m+NAP)
   real(kind=wp)  :: yUpper                     !< y-coordinate upper bound influence length (m+NAP)
   real(kind=wp)  :: f1(geometry%NbermSegments) !< vector with help factors for combining influence factors
   real(kind=wp)  :: f2(geometry%NbermSegments) !< vector with help factors for combining influence factors
   real(kind=wp)  :: f3                         !< help factor for combining influence factors
   real(kind=wp)  :: f4                         !< help factor for combining influence factors
   real(kind=wp)  :: f5                         !< help factor for combining influence factors

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! local water level not lower than dike toe (first y-coordinate)
   if (load%h < geometry%yCoordinates(1)) then
      error%errorCode = 1
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (load%h > geometry%yCoordinates(geometry%nCoordinates)) then
      error%errorCode = 1
   endif

   ! ---------------------------------------------------------------------
   ! calculate berm widhts, heights, influence lengths and relative depths
   ! ---------------------------------------------------------------------

   if (error%errorCode == 0) then

      ! initialize berm counter
      N = 0

      ! loop over possible berm segments
      do i=2, geometry%nCoordinates - 2

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
            B(N)  = geometry%xCoordDiff(i)

            ! determine the height of the (horizontal) berm segment
            hB(N) = geometry%yCoordinates(i)

            ! calculate the influence length
            yLower = max(hB(N)-load%Hm0, geometry%yCoordinates(1))
            yUpper = min(hB(N)+load%Hm0, geometry%yCoordinates(geometry%nCoordinates))
            call calculateHorzDistance (geometry, yLower, yUpper, LB(N), error)

            ! calculate relative berm width
            if (error%errorCode == 0 .and. LB(N) > 0.0d0) then
               rB(N) = B(N) / LB(N)
            else
               error%errorCode = 1
               exit
            endif

            ! calculate the berm depth
            dH(N) = load%h - hB(N)

            ! calculate the influence of the berm depth
            if (dH(N) < 0.0d0) then ! local water level below the berm (dH<0)

               if (z2 > -dH(N)) then
                  ! local water level + z2% above the berm (influence)
                  rD(N) = 0.5d0 - 0.5d0 * cos(pi*dH(N)/z2)
               else
                  ! local water level + z2% on or below the berm (no influence)
                  rD(N) = 1.0d0
               endif

            else ! local water level on or above the berm (dH>=0)

               if (dH(N) < 2.0d0*load%Hm0) then
                  ! local water level less than 2*Hm0 above the berm (influence)
                  rD(N) = 0.5d0 - 0.5d0 * cos(pi*dH(N)/(2.0d0*load%Hm0))
               else
                  ! local water level 2*Hm0 or more above the berm (no influence)
                  rD(N) = 1.0d0
               endif

            endif

         endif
      enddo

   endif

   ! check number of berm segments
   if (N /= geometry%NbermSegments) error%errorCode = 1

   ! ------------------------------------
   ! calculate influence factor for berms
   ! ------------------------------------

   if (error%errorCode == 0) then

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
   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      if (error%Message == ' ') then
         call GetMSGcalc_influence_berms(error%Message)
      endif
   endif

   end subroutine calculateGammaB

!***********************************************************************************************************
   end module factorModuleOvertopping
!***********************************************************************************************************
