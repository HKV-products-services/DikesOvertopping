! Copyright (C) Stichting Deltares 2019. All rights reserved.
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
!! This file contains a module with the iteration procedure for 2% wave runup
!<
!***********************************************************************************************************
!
!  Programmers: Bastiaan Kuijper, HKV consultants / Edwin Spee, Deltares
!
!  $Id$
!
!> Iteration procedure for 2% wave runup
!
!***********************************************************************************************************
module waveRunup
!***********************************************************************************************************

   use precision, only : wp
   use factorModuleOvertopping
   use typeDefinitionsOvertopping
   use formulaModuleOvertopping
   use geometryModuleOvertopping
   use OvertoppingMessages
   use ModuleLogging

   implicit none

   private
   
   public :: iterationWaveRunup

   contains

!> iterationWaveRunup:
!! iteration for the wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine iterationWaveRunup (geometry, h, Hm0, Tm_10, gammaBeta_z, modelFactors, z2, logging, succes, errorMessage)
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
   real(kind=wp),             intent(inout)  :: gammaBeta_z    !< influence factor angle wave attack 2% run-up
   type (tpOvertoppingInput), intent(in)     :: modelFactors   !< structure with model factors
   real(kind=wp),             intent(out)    :: z2             !< 2% wave run-up (m)
   type(tLogging),            intent(in)     :: logging        !< logging struct
   logical,                   intent(out)    :: succes         !< flag for succes
   character(len=*),          intent(inout)  :: errorMessage   !< error message, only set in case of an error
!
!  Local parameters
!
   type (tpGeometry), pointer :: geometryFlatBerms        !< structure with geometry data with horizontal berms
   real(kind=wp)     :: s0                       !< wave steepness
   integer           :: i                        !< counter iteration steps
   real(kind=wp)     :: z2_start  (z2_iter_max2) !< starting value 2% wave run-up for each iteration step
   real(kind=wp)     :: z2_end    (z2_iter_max2) !< ending value 2% wave run-up   for each iteration step
   integer           :: Niterations              !< number of iterations before convergence
   logical           :: convergence              !< flag for convergence iteration procedure
   integer           :: k                        !< index with lowest difference between z2_start and z2_end
   real(kind=wp)     :: z2k                      !< z2 corresponding to index k
   real(kind=wp)     :: offset                   !< index near z2k
   
! ==========================================================================================================

   ! calculate wave steepness
   call calculateWaveSteepness (Hm0, Tm_10, s0, succes, errorMessage)

   ! if applicable adjust non-horizontal berms
   geometryFlatBerms => geometry%parent%geometryFlatBerms
   if (succes) then
      call adjustNonHorizontalBerms (geometry, geometryFlatBerms, succes, errorMessage)
   endif

   ! initialize number of iterations and flag for convergence
   Niterations = 0
   convergence = .false.

   ! -------------------------
   ! start iteration procedure
   ! -------------------------

   ! iteration procedure for calculating 2% wave run-up
   if (succes) then
      do i=1, z2_iter_max1

         ! edit number of iterations
         Niterations = Niterations + 1

         z2_start(i) = determineStartingValue(i, modelFactors%relaxationFactor, z2_start, z2_end, Hm0)

         z2_end(i) = innerCalculation(geometry, h, Hm0, gammaBeta_z, modelFactors, z2_start(i), s0, geometryFlatBerms, succes, errorMessage)

         ! calculate convergence criterium
         convergence = (abs(z2_start(i) - z2_end(i)) < z2_margin)

         ! exit loop when an error occurs or convergence is reached
         if ((.not. succes) .or. (convergence)) then
             exit
         endif

      enddo
   endif

   if (succes .and. .not. convergence) then
      !
      ! iteration process, even with relaxation does not converge
      ! fall back on:
      ! - find case k with lowest difference between z2_start and z2_end
      ! - search with small steps around this z2 value to the smallest difference
      !
      k = findSmallestResidu(z2_start, z2_end, z2_iter_max1)
      z2k = z2_start(k)
      offset = dble((z2_iter_max1 + 1 + z2_iter_max2) / 2) + 0.25_wp
      do i = z2_iter_max1 + 1, z2_iter_max2

         ! edit number of iterations
         Niterations = Niterations + 1
      
         z2_start(i) = z2k + 2.0_wp * z2_margin * (real(i,wp)-offset)
      
         z2_end(i) = innerCalculation(geometry, h, Hm0, gammaBeta_z, modelFactors, z2_start(i), s0, geometryFlatBerms, succes, errorMessage)

         ! calculate convergence criterium
         convergence = (abs(z2_start(i) - z2_end(i)) < z2_margin)

         ! exit loop when an error occurs or convergence is reached
         if ((.not. succes) .or. (convergence)) then
            exit
         endif

      enddo
      if ((succes) .and. (.not. convergence)) then
         call convergedWithResidu(z2_start, z2_end, logging)
         convergence = .true.
      endif
   endif 

   ! -----------------------
   ! end iteration procedure
   ! -----------------------

   ! if no error occured, return z2_end of last iteration
   if (succes) then
      z2 = z2_end(Niterations)
   endif

   end subroutine iterationWaveRunup

!> innerCalculation:
!! inner calculation for the wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   function innerCalculation(geometry, h, Hm0, gammaBeta_z, modelFactors, z2, s0, &
                             geometryFlatBerms, succes, errorMessage) result(z2_end)
!***********************************************************************************************************
   implicit none
!
!  Input/output parameters
!
   type (tpGeometry),         intent(in)     :: geometry           !< structure with geometry data
   real(kind=wp),             intent(in)     :: h                  !< local water level (m+NAP)
   real(kind=wp),             intent(in)     :: Hm0                !< significant wave height (m)
   real(kind=wp),             intent(inout)  :: gammaBeta_z        !< influence factor angle wave attack 2% run-up
   type (tpOvertoppingInput), intent(in)     :: modelFactors       !< structure with model factors
   real(kind=wp),             intent(in)     :: z2                 !< 2% wave run-up (m)
   real(kind=wp),             intent(in)     :: s0                 !< wave steepness
   type (tpGeometry),         intent(in)     :: geometryFlatBerms  !< structure with geometry data with horizontal berms
   logical,                   intent(out)    :: succes             !< flag for succes
   character(len=*),          intent(inout)  :: errorMessage       !< error message, only set in case of an error
   real(kind=wp)                             :: z2_end             !< 2% wave run-up at end of inner calculation
!
!  Local parameters
!
   real(kind=wp)     :: tanAlpha   !< representative slope angle
   real(kind=wp)     :: ksi0       !< breaker parameter
   real(kind=wp)     :: ksi0Limit  !< limit value breaker parameter
   real(kind=wp)     :: z2_smooth  !< 2% wave run-up (no roughness)
   real(kind=wp)     :: gammaF     !< influence factor roughness
   real(kind=wp)     :: z2_rough   !< 2% wave run-up (no berms)
   real(kind=wp)     :: gammaB     !< influence factor berms
   
! ==========================================================================================================

   ! initialize influence factor berms and roughness and z2_end
   gammaB     = 1.0d0
   gammaF     = 1.0d0
   z2_end     = 0.0d0

   ! calculate representative slope angle
   if (z2 > 0.0d0) then
      call calculateTanAlpha (h, Hm0, z2, geometryFlatBerms, tanAlpha, succes, errorMessage)
   else
      succes = .true.  ! avoid multiple tests on z2 > 0 which should always be true
      return
   endif

   ! calculate breaker parameter
   if (succes) then
      call calculateBreakerParameter (tanAlpha, s0, ksi0, succes, errorMessage)
   endif

   ! calculate limit value breaker parameter
   if (succes) then
      call calculateBreakerLimit (gammaB, ksi0Limit, succes, errorMessage)
   endif

   ! calculate z2% smooth (gammaB=1, gammaF=1)
   if (succes) then
      call calculateWaveRunup (Hm0, ksi0, ksi0Limit, gammaB, gammaF, &
                                  gammaBeta_z, modelFactors, z2_smooth, succes, errorMessage)
   endif

   ! calculate influence factor roughness (gammaF)
   if (succes) then
      if (z2_smooth > 0.0d0) then
         call calculateGammaF (h, ksi0, ksi0Limit, gammaB, z2_smooth, geometry, gammaF, succes, errorMessage)

         ! calculate z2% rough (gammaB=1)
         if (succes) then
            call calculateWaveRunup (Hm0, ksi0, ksi0Limit, gammaB, gammaF, &
                                  gammaBeta_z, modelFactors, z2_rough, succes, errorMessage)
         endif
      else
         return
      endif
   endif

   ! if cross section contains one or more berms
   if (geometry%NbermSegments > 0 .and. z2_rough > 0.0d0) then
      
      ! calculate influence factor berms (gammaB)
      if (succes) then
         call calculateGammaB (h, Hm0, z2_rough, geometryFlatBerms, gammaB, succes, errorMessage)
      endif

      ! calculate limit value breaker parameter
      if (succes) then
         call calculateBreakerLimit (gammaB, ksi0Limit, succes, errorMessage)
      endif

      ! calculate influence factor roughness (gammaF)
      if (succes) then
         call calculateGammaF (h, ksi0, ksi0Limit, gammaB, z2_rough, geometry, gammaF, succes, errorMessage)
      endif
   
      ! calculate z2%
      if (succes) then
         call calculateWaveRunup (Hm0, ksi0, ksi0Limit, gammaB, gammaF, &
                                  gammaBeta_z, modelFactors, z2_end, succes, errorMessage)
      endif

   else

      ! no berms present
      z2_end = z2_rough

   endif

   end function innerCalculation

!> determineStartingValue:
!! helper function to find a start value for z2
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   function determineStartingValue(i, relaxationFactor, z2_start, z2_end, Hm0) result (startValue)
!***********************************************************************************************************
   implicit none
!
!  Input/output parameters
!
   integer, intent(in)        :: i                  !< current iteration number
   real(kind=wp), intent(in)  :: relaxationFactor   !< relaxation factor as given by user
   real(kind=wp), intent(in)  :: z2_start(:)        !< array with z2 values at the start of the iteration i
   real(kind=wp), intent(in)  :: z2_end(:)          !< array with z2 values at the end of iteration i
   real(kind=wp), intent(in)  :: Hm0                !< significant wave height
   real(kind=wp)              :: startValue         !< return value: start value for z2 in current iteration
!
!  Local parameters
!
   real(kind=wp) :: relaxation    ! locally adjusted relaxation factor
   
! ==========================================================================================================

   ! determine starting value 2% wave run-up for this iteration step
   if (i == 1) then
      startValue = 1.5d0 * Hm0
   else if (i <= 5) then
      startValue = z2_end(i-1)
   else
      if (i <= 25) then 
          relaxation = relaxationFactor
      else
          relaxation = min(0.5_wp, relaxationFactor)
      endif
      startValue = z2_end(i-1) * relaxation + (1.0_wp - relaxation) * z2_start(i-1)
   endif
   end function determineStartingValue

!> findSmallestResidu:
!! helper function to find the smallest residu
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   integer function findSmallestResidu(z2_start, z2_end, n)
!***********************************************************************************************************
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)     :: z2_start(:) !< array with z2 values at the start of the iteration i
   real(kind=wp), intent(in)     :: z2_end(:)   !< array with z2 values at the end of iteration i
   integer, intent(in), optional :: n           !< number of iterations already done
!
!  Local parameters
!
   real(kind=wp) :: residu  ! difference between z2_end and z2_start in iteration i
   integer       :: j       ! loop counter
   integer       :: k       ! iteration with lowest residu during search
   integer       :: maxi    ! upper bound of arrays z2_start and z2_end
   real(kind=wp) :: min_res ! minimal difference found
   
! ==========================================================================================================

   if (present(n)) then
         maxi = n
   else
         maxi = size(z2_start)
   endif

   min_res = huge(min_res)
   do j = 1, maxi
       residu = z2_end(j) - z2_start(j)
       if (abs(residu) < min_res) then
           k = j
           min_res = abs(residu)
       endif
   enddo
   findSmallestResidu = k

   end function findSmallestResidu

!> convergedWithResidu:
!! helper function to handle convergence with a higher residu than expected
!! if logging is enabled, it writes a small message to the logfile
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine convergedWithResidu(z2_start, z2_end, logging)
!***********************************************************************************************************
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)    :: z2_start(:)  !< array with z2 values at the start of the iteration i
   real(kind=wp), intent(inout) :: z2_end(:)    !< array with z2 values at the end of iteration i
   type(tLogging), intent(in)   :: logging      !< logging struct

!
!  Local parameters
!
   integer :: iunit  ! logical unit for log file
   integer :: k      ! index with lowest residu
   
! ==========================================================================================================

   k = findSmallestResidu(z2_start, z2_end)
   z2_end(z2_iter_max2) = (z2_end(k) + z2_start(k)) * 0.5_wp
   if (logging%fileName /= ' ') then
       open(newunit=iunit, file = logging%fileName, position = 'append')
       write(iunit,'(a,f8.4)') 'residu in 2% wave run-up:', abs(z2_start(k) - z2_end(k))
       close(iunit)
   endif

   end subroutine convergedWithResidu

!***********************************************************************************************************
end module waveRunup
!***********************************************************************************************************
