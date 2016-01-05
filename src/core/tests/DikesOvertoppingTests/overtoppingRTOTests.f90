!> @file
!! Contains the module overtoppingRTOTests
!! RTO stands for Risk, Assessment and Design instruments (in dutch: Risico, Toets en Ontwerpinstrumentarium)
!<
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!!Module overtoppingRTOTests, holding the calls to test series of overtopping using the RTO overtopping dll\n
!! @ingroup DikeOvertoppingTests
module overtoppingRTOTests

    use loadRTOTests
    use crossSectionsAdaptionTests
    use crossSectionRoughnessTests
    use dllTests
    use ftnunit

    implicit none
    private

    public :: allovertoppingRTOTests

contains

!> Call all the overtopping test series for the RTO overtopping dll
!! 
subroutine allovertoppingRTOTests
    implicit none
    integer, parameter      :: nCrossSections   = 8     !< number of cross sections
    integer, parameter      :: nBasicTestSeries = 7     !< number of basic test series

    ! Test using external dll
    call testWithLevel(overtoppingDllTest, 'Test the external overtopping dll', 1)
    call testWithLevel(overtoppingZ2Test, 'Test h+z2 > dikeheigth', 1)
    call testWithLevel(overtoppingValidationTest, 'Test validation of incorrect profile and negative model factor', 1)
    call testWithLevel(influenceRoughnessTest, 'Test influence roughness', 1)

    !
    ! All test series with varying the load for different cross sections
    call allLoadRTOTests( nCrossSections, nBasicTestSeries )

    !
    ! All test series with adapting the cross section
    call allCrossSectionsRTOTests( nCrossSections, nBasicTestSeries )

    !
    ! All test series with adapting the roughness of one or more segments of the cross sections
    call allCrossSectionRoughnessTests( nCrossSections, nBasicTestSeries )

    ! test for the adjustment of the cross section
    call testWithLevel(TestProfileAdjustment, "Test whether the profile is adapted correctly", 1)

end subroutine allovertoppingRTOTests

end module overtoppingRTOTests