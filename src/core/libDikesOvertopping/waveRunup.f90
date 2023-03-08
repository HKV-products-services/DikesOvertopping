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
   use typeDefinitionsOvertopping
   use errorMessages

   implicit none

   private

   public :: iterationWaveRunup

   interface
      module subroutine iterationWaveRunup (geometry, geometryFlatBerms, geometryNoBerms, load, gamma_z, modelFactors, z2, error)
         type (tpGeometry),         intent(in)     :: geometry          !< structure with geometry data
         type (tpGeometry),         intent(inout)  :: geometryFlatBerms !< structure with geometry data with horizontal berms
         type (tpGeometry),         intent(inout)  :: geometryNoBerms   !< structure with geometry data with no berms
         type (tpLoadX),            intent(inout)  :: load              !< load struct
         type(tpInfluencefactors),  intent(inout)  :: gamma_z           !< influence factor angle wave attack 2% run-up
         type (tpOvertoppingInput), intent(in)     :: modelFactors      !< structure with model factors
         real(kind=wp),             intent(out)    :: z2                !< 2% wave run-up (m)
         type(tMessage),            intent(inout)  :: error             !< error struct
      end subroutine iterationWaveRunup
   end interface

   interface
      module function innerCalculation(geometry, load, gamma_z, modelFactors, z2, &
                             geometryFlatBerms, geometryNoBerms, error) result(z2_end)
         type (tpGeometry),         intent(in)     :: geometry           !< structure with geometry data
         type (tpLoadX),            intent(in)     :: load               !< load struct
         type(tpInfluencefactors),  intent(inout)  :: gamma_z            !< influence factor angle wave attack 2% run-up
         type (tpOvertoppingInput), intent(in)     :: modelFactors       !< structure with model factors
         real(kind=wp),             intent(in)     :: z2                 !< 2% wave run-up (m)
         type (tpGeometry),         intent(in)     :: geometryFlatBerms  !< structure with geometry data with horizontal berms
         type (tpGeometry),         intent(inout)  :: geometryNoBerms  !< structure with geometry data with no berms
         type (tMessage),           intent(inout)  :: error              !< error struct
         real(kind=wp)                             :: z2_end             !< 2% wave run-up at end of inner calculation (m)
      end function innerCalculation
   end interface

!***********************************************************************************************************
end module waveRunup
!***********************************************************************************************************
