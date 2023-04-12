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
!! Contains the module overtoppingTests
!<
!
! $Id$
!
!>
!!Module overtoppingTests, holding the calls to test series of overtopping using the Overtopping dll\n
!! @ingroup DikeOvertoppingTests
module overtoppingTests

    use loadTests
    use crossSectionsAdaptionTests
    use crossSectionRoughnessTests
#if defined _WIN32 || _WIN64
    use dllTests
    use dllFewsTests
#endif
    use omkeerVariantTests
    use overtoppingUnitTests

    implicit none
    private

    public :: allovertoppingTests

contains

!> Call all the overtopping test series for the Overtopping dll
!! 
subroutine allovertoppingTests
    implicit none
    integer, parameter      :: nCrossSections   = 8     !< number of cross sections
    integer, parameter      :: nBasicTestSeries = 7     !< number of basic test series

    !
    ! Test using external dll
#if defined _WIN32 || _WIN64
    call allOvertoppingDllTests
#endif

    !
    ! test for 'omkeerVariant'
    call allOmkeerVariantTests

    !
    ! Test using external dll, Java/FEWS interface
#if defined _WIN32 || _WIN64
    call allOvertoppingDllFewsTests
#endif

    !    
    ! All test series with varying the load for different cross sections
    call allLoadTests(nCrossSections, nBasicTestSeries)

    !
    ! All test series with adapting the cross section
    call allCrossSectionsTests(nCrossSections, nBasicTestSeries)

    !
    ! All test series with adapting the roughness of one or more segments of the cross sections
    call allCrossSectionRoughnessTests(nCrossSections, nBasicTestSeries)

    !
    ! All tests for some specific low level kernel functions
    call allOvertoppingUnitTests

end subroutine allovertoppingTests

end module overtoppingTests
