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
!! Contains the module crossSectionsAdaptionTests of the Overtopping dll
!
!
! $Id$
!
!>
!! Module holding the test series of adapting the cross section characteristics
!!
!! @ingroup DikeOvertoppingTests
module crossSectionsAdaptionTests

    use precision
    use ftnunit
    use waveParametersUtilities
    use readCrossSectionForTests
    use mainModuleOvertopping
    use typeDefinitionsOvertopping
    use geometryModuleOvertopping
    use moduleLogging
    use omkeerVariantModule
    use overtoppingInterface
    
    implicit none

    type (tpOvertoppingInput) :: modelFactors         ! structure with model factors
    type (tpLoad)             :: load                 ! structure with load data
    character(len=90)         :: outputFile           ! file for the output of the testserie
    integer                   :: crossSectionId       ! id-number of the cross section 
    integer                   :: numberTestSerie      ! number of test serie
    logical                   :: extraSlopeTest       ! perform extra slope test
    character(len=128)        :: ModuleErrorMessage   ! error message
    character(len=120)        :: frozenFile           ! frozen copy of the output file of the testserie

    private

    public :: allCrossSectionsTests

contains

!> Call all the cross section varying overtopping test series for the Overtopping dll. In these test series 
!! the characteristics of the cross sections are varied.
!!
!! @ingroup DikesOvertoppingTests
subroutine allCrossSectionsTests(nCrossSections, nBasicTestSeries)
!
!   input/output parameters
!
    integer, intent(in)     :: nCrossSections       !< number of cross sections
    integer, intent(in)     :: nBasicTestSeries     !< number of basic test series
!
!   local parameters
!
    character(len=128)         :: errorMessage           ! error message
    character(len=1)           :: crossSectionNumber     ! number of the cross section
    character(len=2)           :: testSerieNumber        ! number of the test serie

    integer                    :: i, j, k                ! do-loop counters
    integer                    :: nWaveDirections        ! number of wave directions in the test series
    integer                    :: nTestSeriesSlopes      ! amount of test series for slopes

    real(kind=wp)              :: waveSteepness          ! wave steepness
    real(kind=wp), allocatable :: phi(:)                 ! wave directions
    type(tMessage)             :: error                  ! error struct
!
!   source
!
    !
    ! fill the model factors for the Overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fShallow = 0.92
    modelFactors%relaxationFactor = 1.0d0
    
    !
    ! General settings for cross section specific test series
    load%h        = 3.0d0
    load%Hm0      = 2.0d0
    waveSteepness = 0.04d0
    !
    ! compute the wave period
    load%Tm_10 = computeWavePeriod(load%Hm0, waveSteepness, error)
    call assert_equal(error%errorCode, 0, errorMessage)

    nWaveDirections = 2
    allocate (phi(nWaveDirections))
    phi(1) = 0.0d0
    phi(2) = 85.0d0

    extraSlopeTest = .false.
    !
    ! execute test series for slopes for all 8 cross sections
    do i = 1, nCrossSections
        crossSectionId = i
        numberTestSerie = nBasicTestSeries
        call numberTestSeriesSlopes (i, nTestSeriesSlopes)

        do j = 1, nTestSeriesSlopes
            do k = 1, nWaveDirections
                load%phi = phi(k)
                numberTestSerie = numberTestSerie + 1

                write (outputFile,'(a,i1,a,i2.2,a)') './output_section', crossSectionId, '_test', numberTestSerie, '.txt'

                write (crossSectionNumber,'(I1)') crossSectionId
                write (testSerieNumber,   '(I2)') numberTestSerie
                write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputOvertopping/output_section', i, '_test', numberTestSerie, '.txt'
                ModuleErrorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'

                call testWithLevel(TestSeriesCrossSections, "Trends; Series (A) of varying geometry with the dll in test series " // &
                                           trim(testSerieNumber) // " for cross section " // crossSectionNumber, 3)

            enddo
        enddo
    enddo

    !
    ! some extra tests to vary the slope of the berm as it stays a berm
    extraSlopeTest = .true.
    do i = 1, nCrossSections
        crossSectionId = i
        numberTestSerie = nBasicTestSeries
        call numberExtraTestsSlopes (i, nTestSeriesSlopes)

        do j = 1, nTestSeriesSlopes
            do k = 1, nWaveDirections
                load%phi = phi(k)
                numberTestSerie = numberTestSerie + 1

                write (outputFile,'(a,i1,a,i2.2,a)') './output_section', crossSectionId, '_test', numberTestSerie, 'berm.txt'

                write (crossSectionNumber,'(I1)') crossSectionId
                write (testSerieNumber,   '(I2)') numberTestSerie

                write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputOvertopping/output_section', i, '_test', numberTestSerie, 'berm.txt'
                ModuleErrorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
                call testWithLevel(TestSeriesCrossSections, "Trends; Series (B) of varying geometry with the dll in test series " // &
                                           trim(testSerieNumber) // " for cross section " // crossSectionNumber, 3)

            enddo
        enddo
    enddo

    deallocate (phi)

end subroutine allCrossSectionsTests

!> One test serie with varying one characteristic of the cross sections.
!!
!! @ingroup DikesOvertoppingTests
subroutine TestSeriesCrossSections
!
!   Local parameters
!
    character(len=90)          :: crossSectionFile     ! file with cross section coordinates
    integer                    :: ounit                ! unit-number for the output file

    real(kind=wp)              :: psi                  ! dike normal (degree)
    type (tpCoordinatePair)    :: coordinates          !< vector with x/y-coordinates
    real(kind=wp), allocatable :: roughnessFactors(:)  ! roughness factors

    integer                    :: ios                  ! input/output-status
    integer                    :: nstep                ! number of computations in the test serie
    integer                    :: istep                ! do-loop counter in the test serie
    
    real(kind=wp)              :: var                  ! value of the variable in the test serie
    real(kind=wp)              :: varmin               ! minimum value of the variable in the test serie
    real(kind=wp)              :: varmax               ! maximum value of the variable in the test serie
    real(kind=wp)              :: varstep              ! step size of the variable in the test serie
    integer                    :: varSlope             ! number of the slope to vary

    type (tpGeometry)          :: geometry             ! structure with geometry data
    type (tpOvertopping)       :: overtopping          ! structure with overtopping results
    character(len=5)           :: ratio                ! ratio to be printed
    type(tMessage)             :: error                ! error struct

    type(OvertoppingGeometryTypeF) :: geometryF
    integer                    :: i                    ! do-loop counter
    integer                    :: npoints              ! number of profile points
    real(kind=wp)              :: z2                   ! z2 from overtopping calculation
    real(kind=wp)              :: q0                   ! q0 from overtopping calculation
    real(kind=wp)              :: HBN_2                ! HBN from omkeervariant at qc=1E-2
    real(kind=wp)              :: HBN_3                ! HBN from omkeervariant at qc=1E-3
    real(kind=wp)              :: HBN_4                ! HBN from omkeervariant at qc=1E-4  
    real(kind=wp), parameter   :: HBNdummy = -9.999    ! dummy value for HBN (in case of failure)
!
!   source
!
    !
    ! read the cross section
    write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputOvertopping/Cross_section', crossSectionId, '.txt'
    call readCrossSection(crossSectionFile, geometry, error)

    ! fill the model factors for the Overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fShallow = 0.92
    modelFactors%relaxationFactor = 1.0d0

    coordinates%N = geometry%Coordinates%N
    allocate (coordinates%x(coordinates%N))
    allocate (coordinates%y(coordinates%N))
    allocate (roughnessFactors(coordinates%N-1))

    coordinates%x     = geometry%Coordinates%x
    coordinates%y     = geometry%Coordinates%y
    roughnessFactors = geometry%roughnessFactors

    npoints = geometry%Coordinates%N
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))

    !
    ! boundaries for variation of the slope
    call variableBoundaries (crossSectionId, numberTestSerie, varmin, varmax, varstep, varSlope)
    nstep = nint((varmax - varmin) / varstep)

    !
    ! Open the output file
    open (newunit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    call assert_equal(ios, 0 ,  'Unable to open the file: ' // trim(outputFile))

    write (ounit,'(a)')     '# Input and results test serie Overtopping dll'
    write (ounit,'(a)')     '#       1       2              3       4       5       6'
    if (varSlope > 0) then
        write (ounit,'(a)') '#   Slope     Z2%             Qo   HBN_4   HBN_3   HBN_2'
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        write (ounit,'(a)') '#   Buckl     Z2%             Qo   HBN_4   HBN_3   HBN_2'
    elseif (crossSectionId == 4) then
        write (ounit,'(a)') '#   BermL     Z2%             Qo   HBN_4   HBN_3   HBN_2'
    endif

    psi = geometry%psi
    do istep = 0, nstep
        !
        ! Adapt the cross section
        var = varmin + dble(istep) * varstep

        call adaptCrossSection (crossSectionId, numberTestSerie, var, coordinates)

        call initializeGeometry (psi, coordinates, roughnessFactors(1:coordinates%N-1), geometry, error)

        call checkCrossSection (geometry%psi, geometry%Coordinates, geometry%roughnessFactors, error)

        do i = 1, npoints
            geometryF%xcoords(i)   = geometry%Coordinates%x(i)
            geometryF%ycoords(i)   = geometry%Coordinates%y(i)
            if (i < npoints) geometryF%roughness(i) = geometry%roughnessFactors(i)
        enddo
        geometryF%normal  = geometry%psi
        geometryF%npoints = npoints

        call assert_equal (error%errorCode, 0, error%Message)
        !
        ! Compute the wave runup and the wave overtopping discharge with the Overtopping module
        call calculateOvertopping (geometry, load, modelFactors, overtopping, error)

        if (error%errorCode /= 0) then
            write(ounit, '(2a)') 'Failure: ', trim(error%Message)
        else
            z2   = overtopping%z2
            q0   = overtopping%Qo
            call iterateToGivenDischarge(load, geometryF, 1.0E-4_wp, HBN_4, modelFactors, overtopping, error)
            if (error%errorCode /= 0) then
               HBN_4 = HBNdummy
            end if
            call iterateToGivenDischarge(load, geometryF, 1.0E-3_wp, HBN_3, modelFactors, overtopping, error)
            if (error%errorCode /= 0) then
               HBN_3 = HBNdummy
            end if
            call iterateToGivenDischarge(load, geometryF, 1.0E-2_wp, HBN_2, modelFactors, overtopping, error)
            if (error%errorCode /= 0) then
               HBN_2 = HBNdummy
            end if
!            if (.not. succes) then
!                write(ounit, '(2a)') 'Failure: ', trim(errorMessage)
!            else
                ! Write the results to the output file
                if (varSlope > 0) then
                    write(ratio, '(f5.1)') 1.0d0 / geometry%segmentSlopes(varSlope)
                    if (.not. extraSlopeTest) then
                        write (ounit,'(2a,f8.3,f15.10,f8.3,f8.3,f8.3)') '    1:', trim(adjustl(ratio)), &
                            load%h+z2, q0, HBN_4, HBN_3, HBN_2
                    else
                        write (ounit,'(2a,f8.3,f15.10,f8.3,f8.3,f8.3)') '    1:', ratio, &
                            load%h+z2, q0, HBN_4, HBN_3, HBN_2
                    endif
                elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
                    write (ounit,'(f9.3,f8.3,f15.10,f8.3,f8.3,f8.3)') geometry%Coordinates%y(2), &
                            load%h+z2, q0, HBN_4, HBN_3, HBN_2
                elseif (crossSectionId == 4) then
                    write (ounit,'(f9.1,f8.3,f15.10,f8.3,f8.3,f8.3)') geometry%Coordinates%x(3) - geometry%Coordinates%x(2), &
                            load%h+z2, q0, HBN_4, HBN_3, HBN_2
                endif
 !           endif
        endif
    enddo
    close(ounit)

    if (associated(geometry%parent)) then
        deallocate(geometry%parent)
    end if
    call deallocateGeometry(geometry)
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

    call cleanupCoordinatePair(coordinates)
    deallocate(roughnessFactors)

    call assert_files_comparable(outputFile, frozenFile, trim(ModuleErrorMessage))

end subroutine TestSeriesCrossSections

!> Define the boundaries of the specific test serie for varying one characteristic of the cross section
!!
!! @ingroup DikesOvertoppingTests
subroutine variableBoundaries (crossSectionId, numberTestSerie, varmin, varmax, varstep, varSlope)
!
!   input/output parameters
!
    integer,       intent(in)  :: crossSectionId       !< id-number of the cross section 
    integer,       intent(in)  :: numberTestSerie      !< number of test serie
    real(kind=wp), intent(out) :: varmin               !< minimum value of the variable in the test serie
    real(kind=wp), intent(out) :: varmax               !< maximum value of the variable in the test serie
    real(kind=wp), intent(out) :: varstep              !< step size of the variable in the test serie
    integer,       intent(out) :: varSlope             !< number of the slope to vary
!
!   source
!
    if (crossSectionId == 1) then
        ! slope
        varmin   =  8.0d0
        varmax   = 64.0d0
        varstep  =  0.8d0
        varSlope = 1
    elseif (crossSectionId == 2) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then
            ! lower slope
            varmin   =  6.0d0
            varmax   = 48.0d0
            varstep  =  0.6d0
            varSlope = 1
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            varmin   = 50.0d0
            varmax   = 64.0d0
            varstep  =  0.2d0
            varSlope = 2
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            varmin   =  2.0d0
            varmax   = 48.0d0
            varstep  =  1.0d0
            varSlope = 0
        endif
    elseif (crossSectionId == 3) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then
             ! lower slope
             varmin   =  6.0d0
             varmax   = 48.0d0
             varstep  =  0.6d0
             varSlope = 1
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            varmin   =  8.0d0
            varmax   = 22.0d0
            varstep  =  0.2d0
            varSlope = 2
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            varmin   =  2.0d0
            varmax   =  6.0d0
            varstep  =  0.1d0
            varSlope = 0
        endif
    elseif (crossSectionId == 4) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then
            ! berm slope
            if (.not. extraSlopeTest) then 
                ! parameter values to vary the slope of the berm. With this choice it does not stay a berm
                varmin   =  1.0d0
                varmax   =  8.0d0
                varstep  =  0.1d0
            else
                ! parameter values to vary the slope of the berm, while it stays a berm 
                varmin   =  15.0d0
                varmax   =  400.0d0
                varstep  =  0.5d0
            endif
            varSlope = 2
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            varmin   =  6.0d0
            varmax   = 48.0d0
            varstep  =  0.6d0
            varSlope = 1
       elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

           ! bucking point
           varmin   =  2.0d0
           varmax   = 20.0d0
           varstep  =  1.0d0
           varSlope = 0
       endif
    elseif (crossSectionId == 5) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then
            ! first berm slope
            if (.not. extraSlopeTest) then 
                ! parameter values to vary the slope of the berm. With this choice it does not stay a berm
                varmin   =  1.0d0
                varmax   =  8.0d0
                varstep  =  0.1d0
            else
                ! parameter values to vary the slope of the first berm, while it stays a berm 
                varmin   =  15.0d0
                varmax   =  400.0d0
                varstep  =  0.5d0
            endif
            varSlope = 2
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            varmin   =  1.0d0
            varmax   =  8.0d0
            varstep  =  0.1d0
            varSlope = 1
        endif
    elseif (crossSectionId == 6) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then
            ! second berm slope
            if (.not. extraSlopeTest) then 
                ! parameter values to vary the slope of the berm. With this choice it does not stay a berm
                varmin   =  1.2d0
                varmax   =  8.0d0
                varstep  =  0.1d0
            else
                ! parameter values to vary the slope of the second berm, while it stays a berm 
                varmin   =  15.0d0
                varmax   =  400.0d0
                varstep  =  0.5d0
            endif
            varSlope = 3
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            varmin   =  1.0d0
            varmax   =  8.0d0
            varstep  =  0.1d0
            varSlope = 1
        endif
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then

        ! lower, middle and upper slope
        varmin   =  1.0d0
        varmax   =  8.0d0
        varstep  =  0.1d0
        varSlope = 1
    endif

end subroutine variableBoundaries

!> Routine to adapt the cross section
!!
!! @ingroup DikesOvertoppingTests
subroutine adaptCrossSection (crossSectionId, numberTestSerie, var, coordinates)
!
!   Input/output parameters
!
    integer,                 intent(in)    :: crossSectionId       !< id-number of the cross section 
    integer,                 intent(in)    :: numberTestSerie      !< number of test serie
    real(kind=wp),           intent(in)    :: var                  !< value of the variable in the test serie
    type (tpCoordinatePair), intent(inout) :: coordinates          !< vector with x/y-coordinates
!
!   source
!
    if (crossSectionId == 1) then
        ! slope
        coordinates%x(2) = var
    elseif (crossSectionId == 2) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! lower slope
            coordinates%x(2) = var
            coordinates%x(3) = coordinates%x(2) + 2.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            coordinates%x(3)  = var
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            coordinates%x(2) = var
            coordinates%y(2) = coordinates%x(2) / 8 - 2.0d0
            coordinates%x(3) = coordinates%x(2) + (6.0d0 - coordinates%y(2))
        endif
    elseif (crossSectionId == 3) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! lower slope
            coordinates%x(2) = var
            coordinates%x(3) = coordinates%x(2) + 16.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            coordinates%x(3)  = var
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            coordinates%x(2) = var
            coordinates%y(2) = coordinates%x(2) - 2.0d0
            coordinates%x(3) = coordinates%x(2) + (6.0d0 - coordinates%y(2)) * 8.0d0
        endif
    elseif (crossSectionId == 4) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! berm slope
            coordinates%y(2) = 4.0d0 - 4.0d0 / var
            coordinates%x(2) = coordinates%x(1) + (coordinates%y(2) - coordinates%y(1)) * 4.0d0
            coordinates%x(3) = coordinates%x(2) + 4.0d0
            coordinates%x(4) = coordinates%x(3) + (coordinates%y(4) - coordinates%y(3)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            coordinates%x(2) = coordinates%x(1) + var
            coordinates%x(3) = coordinates%x(2) + 4.0d0
            coordinates%x(4) = coordinates%x(3) + var / 3.0d0
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! length Berm
            coordinates%x(3) = coordinates%x(2) + var
            coordinates%x(4) = coordinates%x(3) + 6.0d0
        endif
    elseif (crossSectionId == 5) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! first berm slope
            coordinates%y(2) = 4.0d0 - 4.0d0 / var
            coordinates%x(2) = coordinates%x(1) + (coordinates%y(2) - coordinates%y(1)) * 4.0d0
            coordinates%x(3) = coordinates%x(2) + 4.0d0
            coordinates%x(4) = coordinates%x(3) + 4.0d0
            coordinates%x(5) = coordinates%x(4) + (coordinates%y(5) - coordinates%y(4)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            coordinates%x(2) = coordinates%x(1) + (coordinates%y(2) - coordinates%y(1)) * var
            coordinates%x(3) = coordinates%x(2) + 4.0d0
            coordinates%x(4) = coordinates%x(3) + 4.0d0
            coordinates%x(5) = coordinates%x(4) + (coordinates%y(5) - coordinates%y(4)) * var
        endif
    elseif (crossSectionId == 6) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! second berm slope
            coordinates%y(4) = coordinates%y(3) + (coordinates%x(4) - coordinates%x(3)) / var
            coordinates%x(5) = coordinates%x(4) + (coordinates%y(5) - coordinates%y(4)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            coordinates%x(2) = coordinates%x(1) + (coordinates%y(2) - coordinates%y(1)) * var
            coordinates%x(3) = coordinates%x(2) + 4.0d0
            coordinates%x(4) = coordinates%x(3) + 4.0d0
            coordinates%x(5) = coordinates%x(4) + (coordinates%y(5) - coordinates%y(4)) * var
        endif
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then

        ! lower, middle and upper slope
        coordinates%x(2) = coordinates%x(1) + (coordinates%y(2) - coordinates%y(1)) * var
        coordinates%x(3) = coordinates%x(2) + 4.0d0
        coordinates%x(4) = coordinates%x(3) + (coordinates%y(4) - coordinates%y(3)) * var
        coordinates%x(5) = coordinates%x(4) + 4.0d0
        coordinates%x(6) = coordinates%x(5) + (coordinates%y(6) - coordinates%y(5)) * var
    endif

end subroutine adaptCrossSection

!>  Define the boundaries of the specific test serie for varying one characteristic of the cross section
!!
!! @ingroup DikesOvertoppingTests
subroutine numberExtraTestsSlopes(crossSectionId, nTestSeriesSlopes)
!
!   input/output parameters
!
    integer,           intent(in)  :: crossSectionId    !< id-number of the cross section
    integer,           intent(out) :: nTestSeriesSlopes !< amount of test series for slopes

    if ((crossSectionId == 1) .or. (crossSectionId == 2) .or. (crossSectionId == 3))then
        nTestSeriesSlopes = 0
    elseif ((crossSectionId == 4) .or. (crossSectionId == 5) .or. (crossSectionId == 6)) then
        nTestSeriesSlopes = 1
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then
        nTestSeriesSlopes = 0
    else
        call assert_true(.false. , 'Wrong cross section id-number')
    endif

end subroutine numberExtraTestsSlopes

end module crossSectionsAdaptionTests
