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
!! Contains the module crossSectionRoughnessTests of the Overtopping dll
!
!
! $Id$
!
!>
!! Module holding test series of changing the roughness of a the cross sections
!!
!! @ingroup DikeOvertoppingTests
module crossSectionRoughnessTests
!***********************************************************************************************************
!
    use ftnunit
    use waveParametersUtilities
    use mainModuleOvertopping
    use readCrossSectionForTests
    use typeDefinitionsOvertopping
    use geometryModuleOvertopping
    use moduleLogging
    use omkeerVariantModule
    use overtoppingInterface
    use precision, only : wp
!
    implicit none

    type (tpOvertoppingInput)  :: modelFactors         ! structure with model factors
    type (tpLoad)              :: load                 ! structure with load data
    character(len=90)          :: outputFile           ! file for the output of the testserie
    integer                    :: crossSectionId       ! id-number of the cross section 
    integer                    :: numberTestSerie      ! number of test serie
    real(kind=wp), pointer     :: roughness(:)         ! roughness variation for segment numbers
    integer                    :: ii                   ! loop counter

    private

    public :: allCrossSectionRoughnessTests

contains

!> Call all the roughness varying overtopping test series for the Overtopping dll. In these test series 
!! the roughness of the cross sections is varied.
!!
!! @ingroup DikesOvertoppingTests
subroutine allCrossSectionRoughnessTests(nCrossSections, nBasicTestSeries)
!
!   input/output parameters
!
    integer, intent(in)     :: nCrossSections       !< number of cross sections
    integer, intent(in)     :: nBasicTestSeries     !< number of basic test series
!
!   local parameters
!
    character(len=128)         :: errorMessage         ! error message
    character(len=1)           :: crossSectionNumber   ! number of the cross section
    character(len=2)           :: testSerieNumber      ! number of the test serie

    integer                    :: j, k                 ! do-loop counters
    integer                    :: nTestSeriesSlopes    ! amount of test series for slopes
    integer                    :: nWaveDirections      ! number of wave directions in the test series
    integer                    :: nRoughnesses         ! number of test series for the roughness for the specific cross section

    real(kind=wp), allocatable :: phi(:)               ! wave directions
    real(kind=wp)              :: waveSteepness        ! wave steepness
    type(tMessage)             :: error                ! error struct
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

    !
    ! execute roughness test series for all cross sections
    do ii = 1, nCrossSections
        crossSectionId = ii
        call numberTestSeriesSlopes (ii, nTestSeriesSlopes)
        call numberTestSeriesRoughness (ii, nRoughnesses)
        numberTestSerie = nBasicTestSeries + nWaveDirections * nTestSeriesSlopes
        do j = 1, nRoughnesses
            call roughnesses (ii, j, roughness)

            do k = 1, nWaveDirections
                load%phi = phi(k)
                numberTestSerie = numberTestSerie + 1
                

                write (crossSectionNumber,'(I1)') crossSectionId
                write (testSerieNumber,   '(I2)') numberTestSerie

                call testWithLevel(TestSeriesRoughness, "Trends; Series of varying roughness with the dll in test series " // &
                                           trim(testSerieNumber) // " for cross section " // crossSectionNumber, 3)

            enddo
            deallocate (roughness)
        enddo
    enddo
    deallocate (phi)

end subroutine allCrossSectionRoughnessTests

!> One test serie with varying the roughness of one or more segments of the cross sections.
!!
!! @ingroup DikesOvertoppingTests
subroutine TestSeriesRoughness
!
!   local parameters
!
    character(len=90)    :: crossSectionFile     ! file with cross section coordinates
    integer              :: ounit                ! unit-number for the output file

    integer              :: i                    ! do-loop counter
    integer              :: segment              ! segment number

    integer              :: ios                  ! input/output-status
    integer              :: nstep                ! number of computations in the test serie
    integer              :: istep                ! do-loop counter in the test serie

    real(kind=wp)        :: var                  ! value of the variable in the test serie
    real(kind=wp)        :: varmin               ! minimum value of the variable in the test serie
    real(kind=wp)        :: varmax               ! maximum value of the variable in the test serie
    real(kind=wp)        :: varstep              ! step size of the variable in the test serie

    type (tpGeometry)    :: geometry             ! structure with geometry data
    type (tpOvertopping) :: overtopping          ! structure with overtopping results
    character(len=120)   :: frozenFile           ! frozen copy of the output file of the testserie
    type(tMessage)       :: error                ! error struct

    type(OvertoppingGeometryTypeF) :: geometryF
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
    ! read the cross section
    !
    write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputOvertopping/Cross_section', crossSectionId, '.txt'
    call readCrossSection(crossSectionFile, geometry, error)

    npoints = geometry%Coordinates%N
    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = geometry%Coordinates%x(i)
        geometryF%ycoords(i)   = geometry%Coordinates%y(i)
        if (i < npoints) geometryF%roughness(i) = geometry%roughnessFactors(i)
    enddo
    geometryF%normal  = geometry%psi
    geometryF%npoints = npoints

    !
    ! open the output file
    write (outputFile,'(a,i1,a,i2.2,a)') './output_section', crossSectionId, '_test', numberTestSerie, '.txt'
    open (newunit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    call assert_equal(ios, 0, 'Unable to open the file: ' // trim(outputFile))

    write (ounit,'(a)') '# Input and results test serie Overtopping dll'
    write (ounit,'(a)') '#      1          2              3       4       5       6'
    write (ounit,'(a)') '# Roughn        Z2%             Qo   HBN_4   HBN_3   HBN_2'
    !
    ! boundaries for variation of the roughness
    varmin  =  0.5d0
    varmax  =  1.0d0
    varstep =  0.01d0
    nstep = nint((varmax - varmin) / varstep)

    do istep = 0, nstep
        var = varmin + dble(istep) * varstep

        !
        ! Adapt the roughness of the cross section
        do i = 1, size(roughness)
            segment = roughness(i)
            geometry%roughnessFactors(segment) = var
            geometryF%roughness(segment) = var
        enddo

        !
        ! compute the wave runup and the wave overtopping discharge with the overtopping module
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
                !
                ! write the results to the output file
                write (ounit,'(f8.2,f11.3,f15.10,f8.3,f8.3,f8.3)') var, &
                    load%h + z2, q0, HBN_4, HBN_3, HBN_2
!            end if
        end if
    enddo
    close(ounit)

    if (associated(geometry%parent)) then
        deallocate(geometry%parent)
    end if
    call deallocateGeometry(geometry)
    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

    !
    ! do comparison:
    !

    write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputOvertopping/output_section', ii, '_test', numberTestSerie, '.txt'
    error%Message = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
    call assert_files_comparable(outputFile, frozenFile, trim(error%Message))

end subroutine TestSeriesRoughness

!> Routine to determine the nummer of slope varying test series per cross section.
!!
!! @ingroup DikesOvertoppingTests
subroutine numberTestSeriesRoughness(crossSectionId, nRoughnesses)
!
!   input/output parameters
!
    integer,      intent(in)       :: crossSectionId    !< id-number of the cross section
    integer,      intent(out)      :: nRoughnesses      !< number of test series for the roughness for the specific cross section
!
!   source
!
    ! determine the number of different test series of the roughness
    if (crossSectionId == 1) then
        nRoughnesses = 1
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        nRoughnesses = 3
    elseif (crossSectionId == 4) then
        nRoughnesses = 2
    elseif ((crossSectionId == 5) .or. (crossSectionId == 6)) then
        nRoughnesses = 4
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then
        nRoughnesses = 2
    else
        call assert_true (.false. , 'Wrong cross section id-number')
    endif

end subroutine numberTestSeriesRoughness

!> Routine to determine the slopes to vary the roughness in the test series per cross section.
!!
!! @ingroup DikesOvertoppingTests
subroutine roughnesses(crossSectionId, roughnessSlopesId, roughness)
!
!   input/output parameters
!
    integer,    intent(in)      :: crossSectionId       !< id-number of the cross section
    integer,    intent(in)      :: roughnessSlopesId    !< id-number of the roughness variation
    real(kind=wp), pointer      :: roughness(:)         !< roughness variation structure with segment numbers
!
!   local parameter
!
    integer                     :: nSegments            ! number of segments to vary the roughness in the computation
!
!   source
!
    ! determine the slopes to vary the roughness
    if (crossSectionId == 1) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 1
        else
            ! only one roughness test serie
            call assert('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 2
            allocate (roughness(nSegments))
            roughness(1) = 1
            roughness(2) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 1
        elseif (roughnessSlopesId == 3) then
            ! third roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 2
        else
            call assert('Wrong roughnesses id-number')
        endif
    elseif (crossSectionId == 4) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 2
            allocate (roughness(nSegments))
            roughness(1) = 1
            roughness(2) = 3
        else
            call assert('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 5) .or. (crossSectionId == 6)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 1
            allocate (roughness(nSegments))
            roughness(1) = 3
        elseif (roughnessSlopesId == 3) then
            ! third roughness test serie
            nSegments = 2
            allocate (roughness(nSegments))
            roughness(1) = 1
            roughness(2) = 3
        elseif (roughnessSlopesId == 4) then
            ! fourth roughness test serie
            nSegments = 2
            allocate (roughness(nSegments))
            roughness(1) = 2
            roughness(2) = 4
        else
            call assert('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 2
            allocate (roughness(nSegments))
            roughness(1) = 2
            roughness(2) = 4
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 3
            allocate (roughness(nSegments))
            roughness(1) = 1
            roughness(2) = 3
            roughness(3) = 5
        else
            call assert('Wrong roughnesses id-number')
        endif
    else
        call assert('Wrong cross section id-number')
    endif

end subroutine roughnesses

end module crossSectionRoughnessTests
