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

submodule (mainModuleOvertopping) submCalcOvertopping
   use formulaModuleOvertopping
   use factorModuleOvertopping
   use geometryModuleOvertopping
   use utils
contains

!> calculateOvertopping:
!! calculate the overtopping
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateOvertopping
!***********************************************************************************************************
   implicit none
!
   real(kind=wp)            :: toe                  !< height of the dike toe (m+NAP)
   real(kind=wp)            :: crest                !< crest height (m+NAP)
   type (tpLoadX)           :: loadAdj              !< structure with load parameters
   real(kind=wp)            :: beta                 !< angle of wave attack (degree)
   type(tpInfluencefactors) :: gamma_z              !< influence factor angle of wave attack 2% wave run-up
   type(tpInfluencefactors) :: gamma_o              !< influence factor angle of wave attack overtopping
   integer                  :: NwideBerms           !< number of wide berms
   type (tpOvertopping)     :: overtoppingB         !< structure with overtopping results for ordinary berms
   type (tpOvertopping)     :: overtoppingF         !< structure with overtopping results for foreshores
   real(kind=wp), parameter :: tinyWaves = 1d-7     !< waves smaller than tinyWaves can be neglected

   type (tpGeometry), pointer :: geometrySectionB     !< geometry data with wide berms to ordinary berms
   type (tpGeometry), pointer :: geometrySectionF     !< geometry data with wide berms to foreshores
   type (tpGeometry), pointer :: geometryMergedBerms  !< structure with merged sequential berms
   logical                    :: needCleanUp

! ==========================================================================================================

   ! check load parameters and model factors
   call checkInputdata (geometry, load, modelFactors, error)

   if ( .not. allocated (geometry%parent%geometryMergedBerms)) then
       call setupGeometries(geometry%parent)
       needCleanUp = .true.
   else
       needCleanUp = .false.
   end if
   geometryMergedBerms => geometry%parent%geometryMergedBerms
   geometrySectionB    => geometry%parent%geometrySectionB
   geometrySectionF    => geometry%parent%geometrySectionF

   if (error%errorCode == 0) then

      toe   = geometry%Coordinates%y(1)
      crest = geometry%Coordinates%y(geometry%Coordinates%N)

      ! initialize local values wave height and wave period
      loadAdj%Hm0   = min(load%Hm0, max(load%h - toe, 0.0d0))
      loadAdj%h     = load%h
      loadAdj%Tm_10 = load%Tm_10
      loadAdj%phi   = load%phi

      ! calculate angle of wave attack
      call calculateAngleWaveAttack (load%phi, geometry%psi, beta)

      ! calculate influence factors angle of wave attack
      call calculateGammaBeta (loadAdj, beta, gamma_z, gamma_o)

      ! check wave height and wave period
      if (abs (loadAdj%Hm0) < tinyWaves .or. isEqualZero (loadAdj%Tm_10)) then

         ! wave height and/or wave period equal to zero
         overtopping%z2 = 0.0d0
         overtopping%Qo = 0.0d0

      ! water level below toe of the cross section
      elseif (loadAdj%h < toe) then

         ! water level lower than toe of the cross section
         overtopping%z2 = 0.0d0
         overtopping%Qo = 0.0d0
      else

         ! if applicable merge two sequential berms  TODO: liever eerder
         call mergeSequentialBerms (geometry, geometryMergedBerms, error)

         ! calculate the wave length
         if (error%errorCode == 0) then
            call calculateWaveLength (loadAdj%Tm_10, loadAdj%L0)
         endif
         
         ! if applicable split cross section
         if (error%errorCode == 0) then
            call splitCrossSection (geometryMergedBerms, loadAdj%L0, NwideBerms, &
                                    geometrysectionB, geometrysectionF, error)
         endif

         ! start calculation for each cross section
         if (error%errorCode == 0) then

            call startCalcCrossSections()
         endif
      endif

   endif

   if (needCleanUp) then
       call cleanupGeometry(geometry%parent, .false.)
   end if

contains

   subroutine startCalcCrossSections()

      if (NwideBerms == 0) then

         ! no wide berms (geometrySectionB = geometrySectionF)
         call calculateOvertoppingSection (geometrySectionB, loadAdj,  &
                                           gamma_z, gamma_o, modelFactors, overtopping, error)
      else

         ! geometrySectionB equals geometry with all wide berms reduced to B=L0/4
         call calculateOvertoppingSection (geometrySectionB, loadAdj,   &
                                           gamma_z, gamma_o, modelFactors, overtoppingB, error)

         ! geometrySectionF equals geometry with all wide berms extended B=L0
         if (error%errorCode == 0) then
            call calculateOvertoppingSection (geometrySectionF, loadAdj,  &
                                              gamma_z, gamma_o, modelFactors, overtoppingF, error)
         endif
         
         ! interpolation of results for both cross sections
         if (error%errorCode == 0) then
            call interpolateResultsSections (geometryMergedBerms, loadAdj%L0, NwideBerms,     &
                                             overtoppingB, overtoppingF, overtopping, error)
         endif
      endif
   end subroutine startCalcCrossSections

end procedure calculateOvertopping

end submodule submCalcOvertopping
