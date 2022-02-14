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
!! This file contains a module with the type definitions for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants / Edwin Spee, Deltares
!
!  $Id$
!
!***********************************************************************************************************

module parametersOvertopping
   use precision, only : wp
   private :: wp

   real(kind=wp), parameter :: fRunup1 = 1.65_wp
   real(kind=wp), parameter :: fRunup2 = 4.00_wp
   real(kind=wp), parameter :: fRunup3 = 1.50_wp

   ! -------------------------------------
   !  Public parameters
   ! -------------------------------------
   real(kind=wp),  parameter :: xDiff_min     = 2.0d-2            !< minimal value distance between x-coordinates (m)
   real(kind=wp),  parameter :: marginDiff    = 1.0d-14           !< margin for minimal distance (m)
   real(kind=wp),  parameter :: berm_min      = 0.0d0             !< minimal value gradient berm segment
   real(kind=wp),  parameter :: berm_max      = 1.0d0/15          !< maximal value gradient berm segment
   real(kind=wp),  parameter :: slope_min     = 1.0d0/8           !< minimal value gradient slope segment
   real(kind=wp),  parameter :: slope_max     = 1.0d0             !< maximal value gradient slope segment
   real(kind=wp),  parameter :: marginGrad    = 0.0025d0          !< margin for minimal and maximal gradients
   real(kind=wp),  parameter :: rFactor_min   = 0.5d0             !< minimal value roughness factor dike segments
   real(kind=wp),  parameter :: rFactor_max   = 1.0d0             !< maximal value roughness factor dike segments
   real(kind=wp),  parameter :: mz2_min       = 0.0d0             !< minimal value model factor of 2% runup height
   real(kind=wp),  parameter :: mz2_max       = huge(mz2_max)     !< maximal value model factor of 2% runup height
   real(kind=wp),  parameter :: fB_min        = 0.0d0             !< minimal value model factor for breaking waves
   real(kind=wp),  parameter :: fB_max        = huge(fB_max)      !< maximal value model factor for breaking waves
   real(kind=wp),  parameter :: fN_min        = 0.0d0             !< minimal value model factor for non-breaking waves
   real(kind=wp),  parameter :: fN_max        = huge(fN_max)      !< maximal value model factor for non-breaking waves
   real(kind=wp),  parameter :: fS_min        = 0.0d0             !< minimal value model factor for shallow waves
   real(kind=wp),  parameter :: fS_max        = huge(fS_max)      !< maximal value model factor for shallow waves
   real(kind=wp),  parameter :: foreshore_min = 0.3d0             !< minimal value reduction factor foreshore
   real(kind=wp),  parameter :: foreshore_max = 1.0d0             !< maximal value reduction factor foreshore
   integer,   parameter      :: z2_iter_max1  =  49               !< maximal number of iterations for calculation z2 part 1
   integer,   parameter      :: z2_iter_max2  =  70               !< maximal number of iterations for calculation z2 part 1 & 2
   real(kind=wp),  parameter :: z2_margin     = 0.001d0           !< margin for convergence criterium calculation z2
end module parametersOvertopping
