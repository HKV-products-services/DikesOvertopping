!> @file
!! This file contains a module with functions for the slope angle and influence factors
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants
!
!  Copyright (c) 2015, Deltares, HKV lijn in water, TNO
!  $Id$
!
!***********************************************************************************************************
   module factorModuleRTOovertopping
!***********************************************************************************************************

   use typeDefinitionsRTOovertopping
   use geometryModuleRTOovertopping
   use precision, only : wp

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
   subroutine calculateTanAlpha (h, Hm0, z2, geometry, tanAlpha, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(wp),            intent(in)  :: h              !< local water level (m+NAP)
   real(wp),            intent(in)  :: Hm0            !< significant wave height (m)
   real(wp),            intent(in)  :: z2             !< 2% wave run-up (m)
   type(tpGeometry),    intent(in)  :: geometry       !< structure with geometry data
   real(wp),            intent(out) :: tanAlpha       !< representative slope angle
   logical,             intent(out) :: succes         !< flag for succes
   character(len=*),    intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   type(tpGeometry)  :: geometryNoBerms   !< geometry without berms
   real(wp)          :: yLower            !< y-coordinate lower bound representative slope (m+NAP)
   real(wp)          :: yUpper            !< y-coordinate upper bound representative slope (m+NAP)
   real(wp)          :: dx                !< horizontal distance between lower and upper bound (m)

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! local water level not lower than dike toe (first y-coordinate)
   if (h < geometry%yCoordinates(1)) then
      succes = .false.
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (h > geometry%yCoordinates(geometry%nCoordinates)) then
      succes = .false.
   endif

   ! determine cross section without berms
   if (succes) then

      if (geometry%NbermSegments > 0) then
         call removeBerms (geometry, geometryNoBerms, succes, errorMessage)
      else
         call copyGeometry (geometry, geometryNoBerms)
      endif

   endif

   ! calculate representative slope angle
   if (succes) then

      ! determine y-coordinates lower and upper bound representative slope
      yLower = max(h-1.5d0*Hm0, geometryNoBerms%yCoordinates(1))
      yUpper = min(h+z2,      geometryNoBerms%yCoordinates(geometryNoBerms%nCoordinates))
   
      ! calculate horizontal distance between lower and upper bound
      call calculateHorzDistance (geometryNoBerms, yLower, yUpper, dx, succes, errorMessage)
      
      ! calculate representative slope angle
      if (succes) then
         if (dx > 0.0) then
            tanAlpha = (yUpper - yLower) / dx
         else
            succes = .false.
            errorMessage = 'Error in calculation representative slope angle'
         endif
      endif

   endif

   call deallocateGeometry( geometryNoBerms )

   end subroutine calculateTanAlpha

!> calculateGammaBeta
!! influence factor angle of wave attack
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaBeta (Hm0, Tm_10, beta, gammaBeta_z, gammaBeta_o)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(wp), intent(inout)  :: Hm0         !< significant wave height (m)
   real(wp), intent(inout)  :: Tm_10       !< spectral wave period (s)
   real(wp), intent(in)     :: beta        !< angle of wave attack (degree)
   real(wp), intent(out)    :: gammaBeta_z !< influence factor angle of wave attack 2% wave run-up
   real(wp), intent(out)    :: gammaBeta_o !< influence factor angle of wave attack overtopping

! ==========================================================================================================

   ! calculate influence factors angle of wave attack for 2% wave run-up and overtopping
   gammaBeta_z = 1 - 0.0022d0 * min(beta,80.0d0)
   gammaBeta_o = 1 - 0.0033d0 * min(beta,80.0d0)

   ! adjustment of the wave parameters if beta > 80
   if (beta > 80.0d0) then
      
      if (beta > 110.0d0) then
         ! beta > 110
         Hm0   = 0.0d0
         Tm_10 = 0.0d0
       else
         ! 80 < beta <= 110
         Hm0   = Hm0   *      (110.0d0 - beta)/30.0d0
         Tm_10 = Tm_10 * sqrt((110.0d0 - beta)/30.0d0)
      endif

   endif

   end subroutine calculateGammaBeta

!> calculateGammaF
!! influence factor roughness
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaF (h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(wp),               intent(in)  :: h              !< local water level (m+NAP)
   real(wp),               intent(in)  :: ksi0           !< breaker parameter
   real(wp),               intent(in)  :: ksi0Limit      !< limit value breaker parameter
   real(wp),               intent(in)  :: gammaB         !< influence factor berms
   real(wp),               intent(in)  :: z2             !< 2% wave run-up (m)
   type(tpGeometry),       intent(in)  :: geometry       !< structure with geometry data
   real(wp),               intent(out) :: gammaF         !< influence factor roughness
   logical,                intent(out) :: succes         !< flag for succes
   character(len=*),       intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   real(wp), allocatable :: rFactors(:)     !< roughness factors  of segments with influence
   real(wp), allocatable :: horzLengths(:)  !< horizontal lengths of segments with influence (m)
   real(wp)              :: yLower          !< y-coordinate lower bound segments with influence (m+NAP)
   real(wp)              :: yUpper          !< y-coordinate upper bound segments with influence (m+NAP)
   integer               :: iLower          !< index dike segment lower bound
   integer               :: iUpper          !< index dike segment upper bound
   real(wp)              :: sum_horzLengths !< sum of all horzLengths
   real(wp), parameter   :: one = 1.0_wp    !< constant in comparision with breaker parameters
   real(wp), parameter   :: ten = 10.0_wp   !< constant in comparision with breaker parameters

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! local water level not lower than dike toe (first y-coordinate)
   if (h < geometry%yCoordinates(1)) then
      succes = .false.
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (h > geometry%yCoordinates(geometry%nCoordinates)) then
      succes = .false.
   endif

   if (succes) then

      ! allocate roughness factos and horizontal lengths
      allocate (rFactors   (geometry%nCoordinates-1))
      allocate (horzLengths(geometry%nCoordinates-1))

      ! determine y-coordinates lower and upper bound segments with influence
      yLower = max(h-0.25d0*z2, geometry%yCoordinates(1))
      yUpper = min(h+0.50d0*z2, geometry%yCoordinates(geometry%nCoordinates))

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
          call calculateHorzLengths (geometry, yLower, yUpper, horzLengths, succes, errorMessage)
          if (succes) then
              sum_horzLengths = sum(horzLengths)
              succes = ( sum_horzLengths > 0.0d0 )
          endif
          if (succes) then
              gammaF = dot_product (horzLengths, rFactors) / sum_horzLengths
          endif
      endif

      ! adjust influence factor if breaker parameter is greater than limit value
      if (succes .and. (ksi0 > ksi0Limit)) then
         if (gammaB*ksi0Limit < ten) then
            gammaF = gammaF + (one-gammaF)*gammaB*(ksi0-ksi0Limit)/(ten-gammaB*ksi0Limit)
         endif
         gammaF = min(one, gammaF)
      endif

      ! deallocate roughness factors and horizontal lengths
      deallocate (rFactors)
      deallocate (horzLengths)

   endif

   ! determine possible error message
   if (.not. succes) then
       if ( errorMessage /= ' ') then
           errorMessage = 'Error in calculation influence roughness'
       endif
   endif

   end subroutine calculateGammaF

!> calculateGammaB
!! influence factor berms
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine calculateGammaB (h, Hm0, z2, geometry, gammaB, succes, errorMessage)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(wp),               intent(in)  :: h              !< local water level (m+NAP)
   real(wp),               intent(in)  :: Hm0            !< significant wave height (m)
   real(wp),               intent(in)  :: z2             !< 2% wave run-up (m)
   type(tpGeometry),       intent(in)  :: geometry       !< structure with geometry data
   real(wp),               intent(out) :: gammaB         !< influence factor berms
   logical,                intent(out) :: succes         !< flag for succes
   character(len=*),       intent(out) :: errorMessage   !< error message
!
!  Local parameters
!
   integer   :: i                          !< counter dike segments
   integer   :: N                          !< counter berm segmetns
   real(wp)  :: B (geometry%NbermSegments) !< berm widths (m)
   real(wp)  :: hB(geometry%NbermSegments) !< berm heights (m+NAP)
   real(wp)  :: LB(geometry%NbermSegments) !< berm influence lengths (m)
   real(wp)  :: rB(geometry%NbermSegments) !< relative berm widths
   real(wp)  :: dH(geometry%NbermSegments) !< berm depths (m)
   real(wp)  :: rD(geometry%NbermSegments) !< relative depths
   real(wp)  :: yLower                     !< y-coordinate lower bound influence length (m+NAP)
   real(wp)  :: yUpper                     !< y-coordinate upper bound influence length (m+NAP)
   real(wp)  :: f1(geometry%NbermSegments) !< vector with help factors for combining influence factors
   real(wp)  :: f2(geometry%NbermSegments) !< vector with help factors for combining influence factors
   real(wp)  :: f3                         !< help factor for combining influence factors
   real(wp)  :: f4                         !< help factor for combining influence factors
   real(wp)  :: f5                         !< help factor for combining influence factors

! ==========================================================================================================

   ! initialize flag for succes and error message
   succes = .true.
   errorMessage = ' '

   ! local water level not lower than dike toe (first y-coordinate)
   if (h < geometry%yCoordinates(1)) then
      succes = .false.
   endif

   ! local water level not higher than crest level (last y-coordinate)
   if (h > geometry%yCoordinates(geometry%nCoordinates)) then
      succes = .false.
   endif

   ! ---------------------------------------------------------------------
   ! calculate berm widhts, heights, influence lengths and relative depths
   ! ---------------------------------------------------------------------

   if (succes) then

      ! initialize berm counter
      N = 0

      ! loop over possible berm segments
      do i=2, geometry%nCoordinates - 2

         ! determine if the current dike segment is a berm segment
         if (geometry%segmentTypes(i) == 2) then

            ! edit counter berm segments
            N = N+1

            ! check if the berm segment is a horizontal berm
            if (geometry%segmentSlopes(i) > 0.0d0) succes = .false.

            ! determine the width of the berm segment
            if (succes) then
               B(N)  = geometry%xCoordDiff(i)
            endif

            ! determine the height of the (horizontal) berm segment
            if (succes) then
               hB(N) = geometry%yCoordinates(i)
            endif

            ! calculate the influence length
            if (succes) then
               yLower = max(hB(N)-Hm0, geometry%yCoordinates(1))
               yUpper = min(hB(N)+Hm0, geometry%yCoordinates(geometry%nCoordinates))
               call calculateHorzDistance (geometry, yLower, yUpper, LB(N), succes, errorMessage)
            endif

            ! calculate relative berm width
            if (succes) then
               if (LB(N) > 0.0d0) then
                  rB(N) = B(N) / LB(N)
               else
                  succes = .false.
               endif
            endif

            ! calculate the berm depth
            if (succes) then
               dH(N) = h - hB(N)
            endif

            ! calculate the influence of the berm depth
            if (succes) then

               if (dH(N) < 0.0d0) then ! local water level below the berm (dH<0)
            
                  if (z2 > -dH(N)) then
                     ! local water level + z2% above the berm (influence)
                     rD(N) = 0.5d0 - 0.5d0 * cos( pi*dH(N)/z2 )
                  else
                     ! local water level + z2% on or below the berm (no influence)
                     rD(N) = 1.0d0
                  endif

               else ! local water level on or above the berm (dH>=0)

                  if (dH(N) < 2*Hm0) then
                     ! local water level less than 2*Hm0 above the berm (influence)
                     rD(N) = 0.5d0 - 0.5d0 * cos( pi*dH(N)/(2*Hm0) )
                  else
                     ! local water level 2*Hm0 or more above the berm (no influence)
                     rD(N) = 1.0d0
                  endif

               endif

            endif
         endif
      enddo

   endif

   ! check number of berm segments
   succes = (succes .and. (N == geometry%NbermSegments))

   ! ------------------------------------
   ! calculate influence factor for berms
   ! ------------------------------------

   if (succes) then

      if (N == 0) then

         ! no berms present
         gammaB = 1.0d0

      elseif (N == 1) then

         ! one berm present
         gammaB = 1 - rB(1) * (1-rD(1))

      else

         ! more berms present
         f1 =     B  * (1-rD)
         f2 = (LB-B) * (1-rD)
         f3 = sum(f1)
         f4 = sum(f1 * f2)
         f5 = sum(f1 * (1-rD))
         if (f3 > 0.0d0) then
            gammaB = 1 - f5 / (f4/f3 + f3)
         else
            gammaB = 1.0d0
         endif
      endif

   endif

   ! ---------------------------------
   ! adjust influence factor for berms
   ! ---------------------------------
   if (succes) then
      if (gammaB < 0.6d0) then
         gammaB = 0.6d0
      endif
   endif

   ! determine possible error message
   if (.not. succes) then
      errorMessage = 'Error in calculation influence berms'
   endif

   end subroutine calculateGammaB

!***********************************************************************************************************
   end module factorModuleRTOovertopping
!***********************************************************************************************************
