! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
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

module typeForFactors
    use precision, only : wp
    type, public             :: tpInfluenceFactors
       real(kind=wp)         :: gammaBeta         !< influence angle wave attack overtopping
       real(kind=wp)         :: gammaB            !< influence factor berms
       real(kind=wp)         :: gammaF            !< influence factor roughness
    end type tpInfluenceFactors

   !> OvertoppingModelFactors: C-structure with model factors
   type, public, bind(C) :: tpOvertoppingInput
      real(kind=wp) :: factorDeterminationQ_b_f_n         !< model factor for non-breaking waves
      real(kind=wp) :: factorDeterminationQ_b_f_b         !< model factor for breaking waves
      real(kind=wp) :: m_z2                               !< model factor describing the uncertainty of 2% runup height
      real(kind=wp) :: fshallow                           !< model factor for shallow waves
      real(kind=wp) :: ComputedOvertopping                !< model factor computed overtopping
      real(kind=wp) :: CriticalOvertopping                !< model factor critical overtopping
      real(kind=wp) :: relaxationFactor                   !< relaxation factor iteration procedure wave runup
      real(kind=wp) :: reductionFactorForeshore = 0.5_wp  !< reduction factor foreshore
   end type tpOvertoppingInput

end module typeForFactors
