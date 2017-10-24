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
!! Contains the module overtoppingUnitTests of the DikesOvertopping dll
!
! $Id$
!
!> 
!! Module, holding tests of some low levels functions of the kernel.
!!
!! @ingroup DikeOvertoppingTests
module overtoppingUnitTests
use precision
use ftnunit
use typeDefinitionsOvertopping
use factorModuleOvertopping
use overtoppingMessages
implicit none
private
public :: allOvertoppingUnitTests

contains

subroutine allOvertoppingUnitTests
    call SetLanguage('NL')
    call testWithLevel(testCalculateGammaF,  "Unit tests; Test CalculateGammaF (tiny waves)", 1)
    call testWithLevel(testCalculateGammaF2, "Unit tests; Test CalculateGammaF (normal waves)", 1)
end subroutine allOvertoppingUnitTests

!> test function CalculateGammaF tiny waves
!! @ingroup DikeOvertoppingTests
subroutine testCalculateGammaF
   real(kind=wp)      :: h              !< local water level (m+NAP)
   real(kind=wp)      :: ksi0           !< breaker parameter
   real(kind=wp)      :: ksi0Limit      !< limit value breaker parameter
   real(kind=wp)      :: gammaB         !< influence factor berms
   real(kind=wp)      :: z2             !< 2% wave run-up (m)
   type(tpGeometry)   :: geometry       !< structure with geometry data
   real(kind=wp)      :: gammaF         !< influence factor roughness
   logical            :: succes         !< flag for succes
   character(len=256) :: errorMessage   !< error message
   character(len=256) :: MsgExpected    !< expected error message
   integer            :: i              !< loop counter
   integer, parameter :: npoints = 5    !< number of profile points
   integer            :: ierr           !< error code of allocate

   ksi0      = 7.61e-9_wp
   ksi0Limit = 1.73_wp
   gammaB    = 1.0_wp
   z2        = 2.23e-15_wp

   allocate(geometry%xCoordinates(npoints), geometry%yCoordinates(npoints), geometry%roughnessFactors(npoints-1), &
            geometry%xCoordDiff(npoints-1), geometry%yCoordDiff(npoints-1), geometry%segmentSlopes(npoints-1), stat=ierr)
   call assert_equal(ierr, 0, 'allocate error')

   geometry%nCoordinates = npoints
   geometry%roughnessFactors = [1.0_wp, 0.9_wp, 0.8_wp, 0.7_wp]
   geometry%xCoordinates = [ -5.0_wp, -1.73_wp, 33.82_wp, 38.16_wp, 47.34_wp ]
   geometry%yCoordinates = [ -4.0_wp, -2.89_wp, 6.03_wp, 6.31_wp, 8.64_wp ]
   do i = 1, npoints - 1
       geometry%xCoordDiff(i)    = geometry%xCoordinates(i+1) - geometry%xCoordinates(i)
       geometry%yCoordDiff(i)    = geometry%yCoordinates(i+1) - geometry%yCoordinates(i)
       geometry%segmentSlopes(i) = geometry%yCoordDiff(i) / geometry%xCoordDiff(i)
   enddo

   do i = 1, npoints - 1
       h = 0.5_wp * (geometry%yCoordinates(i+1) + geometry%yCoordinates(i))
       call calculateGammaF(h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
       call assert_true(succes, errorMessage)
       call assert_comparable(gammaF, geometry%roughnessFactors(i), 1d-6, 'diff in gammaF')
   enddo
   
   ! error handling
   h = -4.01_wp
   call calculateGammaF(h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
   call GetMSG_calculateGammaFtoLow(MsgExpected)
   call assert_equal(errorMessage, MsgExpected, 'test error handling')

   h = 8.65_wp
   call calculateGammaF(h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
   call GetMSG_calculateGammaFtoHigh(MsgExpected)
   call assert_equal(errorMessage, MsgExpected, 'test error handling')

   deallocate(geometry%xCoordinates, geometry%yCoordinates, geometry%roughnessFactors, &
              geometry%xCoordDiff, geometry%yCoordDiff, geometry%segmentSlopes)

end subroutine testCalculateGammaF

!> test function CalculateGammaF normal waves
!! @ingroup DikeOvertoppingTests
subroutine testCalculateGammaF2
   real(kind=wp)      :: h                       !< local water level (m+NAP)
   real(kind=wp)      :: ksi0                    !< breaker parameter
   real(kind=wp)      :: ksi0Limit               !< limit value breaker parameter
   real(kind=wp)      :: gammaB                  !< influence factor berms
   real(kind=wp)      :: z2                      !< 2% wave run-up (m)
   type(tpGeometry)   :: geometry                !< structure with geometry data
   real(kind=wp)      :: gammaF                  !< influence factor roughness
   logical            :: succes                  !< flag for succes
   character(len=256) :: errorMessage            !< error message
   integer            :: i                       !< loop counter
   integer, parameter :: npoints = 5             !< number of profile points
   integer            :: ierr                    !< error code of allocate
   real(kind=wp)      :: gammaFexpected(npoints) !< expected results

   ksi0      = 7.61e-9_wp
   ksi0Limit = 1.73_wp
   gammaB    = 1.0_wp
   z2        = 1.0_wp

   allocate(geometry%xCoordinates(npoints), geometry%yCoordinates(npoints), geometry%roughnessFactors(npoints-1), &
            geometry%xCoordDiff(npoints-1), geometry%yCoordDiff(npoints-1), geometry%segmentSlopes(npoints-1), stat=ierr)
   call assert_equal(ierr, 0, 'allocate error')

   geometry%nCoordinates = npoints
   geometry%roughnessFactors = [1.0_wp, 0.9_wp, 0.8_wp, 0.7_wp]
   geometry%xCoordinates = [ -5.0_wp, -1.73_wp, 33.82_wp, 38.16_wp, 47.34_wp ]
   geometry%yCoordinates = [ -4.0_wp, -2.89_wp, 6.03_wp, 6.31_wp, 8.64_wp ]
   do i = 1, npoints - 1
       geometry%xCoordDiff(i)    = geometry%xCoordinates(i+1) - geometry%xCoordinates(i)
       geometry%yCoordDiff(i)    = geometry%yCoordinates(i+1) - geometry%yCoordinates(i)
       geometry%segmentSlopes(i) = geometry%yCoordDiff(i) / geometry%xCoordDiff(i)
   enddo

   gammaFexpected = [1.0_wp, 0.926985439830363_wp, 0.802088868478168_wp, 0.766296466268931_wp, 0.7_wp]
   do i = 1, npoints
       h = geometry%yCoordinates(i)
       call calculateGammaF(h, ksi0, ksi0Limit, gammaB, z2, geometry, gammaF, succes, errorMessage)
       call assert_true(succes, errorMessage)
       call assert_comparable(gammaF, gammaFexpected(i), 1d-6, 'diff in gammaF')
   enddo

   deallocate(geometry%xCoordinates, geometry%yCoordinates, geometry%roughnessFactors, &
              geometry%xCoordDiff, geometry%yCoordDiff, geometry%segmentSlopes)

end subroutine testCalculateGammaF2

end module overtoppingUnitTests