!> @file
!! Contains the module overtoppingTests
!<
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!!Module overtoppingTests, holding the calls to test series of overtopping using the Overtopping dll\n
!! @ingroup DikeOvertoppingTests
module overtoppingTests

    use loadTests
    use crossSectionsAdaptionTests
    use crossSectionRoughnessTests
    use dllTests
    use omkeerVariantTests

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
    call allOvertoppingDllTests

    !    
    ! test for 'omkeerVariant'
    call allOmkeerVariantTests

    !
    ! All test series with varying the load for different cross sections
    call allLoadTests(nCrossSections, nBasicTestSeries)

    !
    ! All test series with adapting the cross section
    call allCrossSectionsTests(nCrossSections, nBasicTestSeries)

    !
    ! All test series with adapting the roughness of one or more segments of the cross sections
    call allCrossSectionRoughnessTests(nCrossSections, nBasicTestSeries)

end subroutine allovertoppingTests

end module overtoppingTests
