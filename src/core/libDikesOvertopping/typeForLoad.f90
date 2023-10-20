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
!
!> type definitions for Dikes Overtopping
!
   module typeForLoad
!***********************************************************************************************************
   !
   use precision, only : wp
   private :: wp

   !> tpLoad: structure with load parameters
   type, public, bind(C)     :: tpLoad
      real(kind=wp)          :: h                       !< local water level (m+NAP)
      real(kind=wp)          :: Hm0                     !< significant wave height (m)
      real(kind=wp)          :: Tm_10                   !< spectral wave period (s)
      real(kind=wp)          :: phi                     !< wave direction (degrees)
   end type tpLoad

   !> tpLoad: structure with load parameters, extend with with 2 derived properties
   type, public :: tpLoadX
      real(kind=wp)          :: h                       !< local water level (m+NAP)
      real(kind=wp)          :: Hm0                     !< significant wave height (m)
      real(kind=wp)          :: Tm_10                   !< spectral wave period (s)
      real(kind=wp)          :: phi                     !< wave direction (degrees)
      real(kind=wp)          :: L0                      !< wave length (m)
      real(kind=wp)          :: s0                      !< wave steepness (-)
   end type tpLoadX

   !> tpOvertopping: structure with overtopping results
   type, public, bind(C)    :: tpOvertopping
      real(kind=wp)         :: z2                      !< 2% wave run-up (m)
      real(kind=wp)         :: Qo                      !< wave overtopping discharge (m3/m per s)
   end type

!***********************************************************************************************************
    end module typeForLoad
!***********************************************************************************************************

