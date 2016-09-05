! Copyright (C) Stichting Deltares 2016. All rights reserved.
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
!! Contains the module loadTests of the Overtopping dll
!
! $Id$
!
!> 
!! Module, holding test series in which the load parameters are varied. These are the basic test series.
!!
!! @ingroup DikeOvertoppingTests
module loadTests

    use equalReals
    use ftnunit
    use utilities
    use angleUtilities
    use waveParametersUtilities, only : computeWavePeriod
    use typeDefinitionsOvertopping
    use mainModuleOvertopping
    use geometryModuleOvertopping
    use readCrossSectionForTests
    use overtoppingMessages

    implicit none
    private

    type (tpOvertoppingInput) :: modelFactors            ! structure with model factors
    character(len=90)         :: crossSectionFile        ! file with cross section coordinates
    character(len=90)         :: testSerieFile           ! file with values of the test serie
    character(len=90)         :: outputFile              ! file for the output of the testserie

    public :: allLoadTests

contains

!> Call all the basic overtopping test series for the Overtopping dll. In these test series the load parameters are varied.
!!
!! @ingroup DikesOvertoppingTests
subroutine allLoadTests(nCrossSections, nBasicTestSeries)
!
!   input/output parameters
!
    integer, intent(in)     :: nCrossSections       !< number of cross sections
    integer, intent(in)     :: nBasicTestSeries     !< number of basic test series
!
!   local parameters
!
    character(len=120)      :: frozenFile           ! frozen copy of the output file of the testserie
    character(len=90)       :: errorMessage         ! error message
    character(len=1)        :: crossSectionNumber   ! number of the cross section
    character(len=2)        :: testSerieNumber      ! number of the test serie
    integer                 :: i, j                 ! do-loop counters
!
!   Source
!
    ! fill the structure with model factors for the Overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fShallow = 0.92
    modelFactors%relaxationFactor = 1.0d0
    call setLanguage('UK')
    !
    ! execute all test series for all cross sections
    do i = 1, nCrossSections
        write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputOvertopping/Cross_section', i, '.txt'
        do j = 1, nBasicTestSeries
            write (testSerieFile,'(a,i1,a)')   '../DikesOvertoppingTests/InputOvertopping/Basic_test', j, '.txt'
            write (outputFile,'(a,i1,a,i2.2,a)') 'output_section', i, '_test', j, '.txt'
            
            write (crossSectionNumber,'(I1)') i
            write (testSerieNumber,   '(I2)') j
            call testWithLevel(testSeriesLoad, "Calculations with the Overtopping dll in the test serie " // &
                                           trim(testSerieNumber) // " and cross section " // crossSectionNumber, 1)
            
            write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputOvertopping/output_section', i, '_test', j, '.txt'
            errorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
            call assert_files_comparable(outputFile, frozenFile, trim(errorMessage))
        enddo
    enddo

end subroutine allLoadTests

!> One test serie with varying one load parameter.
!!
!! @ingroup DikesOvertoppingTests
subroutine testSeriesLoad
!
!   Local parameters
!
    integer                  :: tunit                ! unit-number for the test serie file
    integer                  :: ounit                ! unit-number for the output file
    integer                  :: ios                  ! input/output-status

    integer                  :: nstep                ! number of computations in the test serie
    integer                  :: istep                ! do-loop counter in the test serie

    real(kind=wp)            :: waveSteepness        ! wave steepness
    real(kind=wp)            :: beta                 ! wave direction w.r.t. the dike normal
    real(kind=wp)            :: var                  ! value of the variable in the test serie
    real(kind=wp)            :: varmin               ! minimum value of the variable in the test serie
    real(kind=wp)            :: varmax               ! maximum value of the variable in the test serie
    real(kind=wp)            :: varstep              ! step size of the variable in the test serie
    real(kind=wp)            :: trig                 ! trigger for the variable in the test serie

    character(len=1)         :: comment              ! comment character
    character(len=2)         :: free                 ! symbol for the parameter to vary in the test serie 

    type (tpGeometry)        :: geometry             ! structure with geometry data
    type (tpLoad)            :: load                 ! structure with load data
    type (tpOvertopping)     :: overtopping          ! structure with overtopping results
    logical                  :: succes               ! flag for succes
    character(len=250)       :: errorMessage         ! error message

    real(kind=wp), parameter :: margin = 1.0d-6      ! relative value for the margin
    integer                  :: ierr                 ! error code
!
!   source
!
    !
    !   read the cross section
    call readCrossSection(crossSectionFile, geometry, succes, errorMessage)
    !
    ! Read the test serie
    !
    ! Open the file with the test serie
    call getFreeLuNumber(tunit)
    open (unit=tunit, file=trim(testseriefile), status='old', iostat=ios)
    call assert_equal(ios, 0, 'Unable to open the file: ' // trim(testseriefile))
    !
    ! Skip comment lines in test serie file 
    comment='#'
    do while (comment == '#')
        read (tunit,'(a)', iostat=ios) comment
        call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))
    enddo
    backspace(unit=tunit)

    ! read values for test serie in test serie file
    read(tunit,*,iostat=ios) varmin,varmax,varstep
    call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))
    ! read water level (h)
    read(tunit,*,iostat=ios) load%h
    call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))
    ! read wave height (Hm0)
    read(tunit,*,iostat=ios) load%Hm0
    call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))
    ! read wave steepness
    read(tunit,*,iostat=ios) waveSteepness
    call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))
    ! read wave direction w.r.t. North
    read(tunit,*,iostat=ios) load%phi
    call assert_equal(ios, 0, 'Read error from the file: ' // trim(testseriefile))

    close(tunit)

    ! The parameter, which is varied in the test serie, has the value -999.99
    trig = -999.99d0
    if     (equalRealsRelative(load%h,        trig, margin)) then
        free='wl'
    elseif (equalRealsRelative(load%Hm0,      trig, margin)) then
        free='hs'
    elseif (equalRealsRelative(waveSteepness, trig, margin)) then
        free='ws' 
    elseif (equalRealsRelative(load%phi,      trig, margin)) then
        free='th'
    endif
    !
    ! open the output file
    call getFreeLuNumber(ounit)
    open (unit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    call assert_equal(ios, 0, 'Unable to open the file: ' // trim(outputFile))
    !
    ! write headers to output file
    write (ounit,'(a)') '# Input and results test serie Overtopping dll'
    write (ounit,'(a)') '#  1       2       3       4       5       6       7'
    write (ounit,'(a)') '#  WL      HS      WS      BETA    TSPEC   Z2%     Qo'

    nstep = nint( (varmax - varmin) / varstep)
    do istep = 0, nstep
        var = varmin + dble(istep) * varstep
 
        if     (free == 'wl') then
            load%h        = var
        elseif (free == 'hs') then
            load%Hm0      = var
        elseif (free == 'ws') then
            waveSteepness = var
        elseif (free == 'th') then
            load%phi      = var
        endif

        !
        ! compute the wave period
        load%Tm_10 = computeWavePeriod(load%Hm0, waveSteepness, ierr, errorMessage)
        call assert_equal(ierr, 0, errorMessage)
        !
        ! compute the angle of wave attack
        beta = angleBetween2Directions(load%phi, geometry%psi, ierr, errorMessage)
        call assert_equal(ierr, 0, errorMessage)
        !
        ! Compute the wave runup and the wave overtopping discharge with the Overtopping module
        call calculateOvertopping (geometry, load, modelFactors, overtopping, succes, errorMessage)
        if (.not. succes) then
            write(ounit, '(2a)') 'Failure: ', trim(errorMessage)
        else
        ! Write the results to the output file
            write (ounit,'(f8.2,2f8.3,f8.1,f8.2,f8.3,f15.10)') load%h, load%Hm0, waveSteepness, beta, load%Tm_10, &
                                                           load%h + overtopping%z2, overtopping%Qo
        end if
    enddo
    close(ounit)

    call deallocateGeometry(geometry)

end subroutine testSeriesLoad

end module loadTests
