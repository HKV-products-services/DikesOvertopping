!> @file
!! Contains the module crossSectionsAdaptionTests of the RTO overtopping dll
!! RTO stands for Risk, Assessment and Design instruments (in dutch: Risico, Toets en Ontwerpinstrumentarium)
!
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module holding the test series of adapting the cross section characteristics
!!
!! @ingroup DikeOvertoppingTests
module crossSectionsAdaptionTests

    use precision
    use utilities
    use feedback
    use ftnunit
    use waveParametersUtilities
    use readCrossSectionForTests
    use mainModuleRTOovertopping
    use zFunctionsWTIOvertopping
    use typeDefinitionsRTOovertopping
    use geometryModuleRTOovertopping
    
    implicit none

    type (tpOvertoppingInput) :: modelFactors         ! structure with model factors
    type (tpLoad)             :: load                 ! structure with load data
    character(len=90)         :: outputFile           ! file for the output of the testserie
    integer                   :: crossSectionId       ! id-number of the cross section 
    integer                   :: numberTestSerie      ! number of test serie
    logical                   :: extraSlopeTest       ! number of test serie

    private
    
    public :: allCrossSectionsRTOTests, TestProfileAdjustment

contains

!> Call all the cross section varying overtopping test series for the RTO overtopping dll. In these test series 
!! the characteristics of the cross sections are varied.
!!
!! @ingroup FailureMechanismsTests
subroutine allCrossSectionsRTOTests( nCrossSections, nBasicTestSeries )
!
!   input/output parameters
!
    integer, intent(in)     :: nCrossSections       !< number of cross sections
    integer, intent(in)     :: nBasicTestSeries     !< number of basic test series
!
!   local parameters
!
    character(len=120)    :: frozenFile             ! frozen copy of the output file of the testserie
    character(len=90)     :: errorMessage           ! error message
    character(len=1)      :: crossSectionNumber     ! number of the cross section
    character(len=2)      :: testSerieNumber        ! number of the test serie

    integer               :: i, j, k                ! do-loop counters
    integer               :: nWaveDirections        ! number of wave directions in the test series
    integer               :: nTestSeriesSlopes      ! amount of test series for slopes

    real(wp)              :: waveSteepness          ! wave steepness
    real(wp), allocatable :: phi(:)                 ! wave directions
!
!   source
!
    !
    ! fill the model factors for the RTO overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fRunup1 = 1.65d0
    modelFactors%fRunup2 = 4.3d0
    modelFactors%fRunup3 = 1.5d0
    modelFactors%fShallow = 0.92
    modelFactors%typeRunup = 1
    modelFactors%relaxationFactor = 1.0d0
    
    !
    ! General settings for cross section specific test series
    load%h        = 3.0d0
    load%Hm0      = 2.0d0
    waveSteepness = 0.04d0
    !
    ! compute the wave period
    load%Tm_10 = computeWavePeriod( load%Hm0, waveSteepness )

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

                call testWithLevel(TestSeriesCrossSections, "Calculations (cross sections test) with the RTO overtopping dll in the test serie " // &
                                           trim(testSerieNumber) // " and cross section " // crossSectionNumber, 1)

                write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputRTOovertopping/output_section', i, '_test', numberTestSerie, '.txt'
                errorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
                call assert_files_comparable( outputFile, frozenFile, trim(errorMessage) )
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

                call testWithLevel(TestSeriesCrossSections, "Calculations (cross sections test/extra slope) with the RTO overtopping dll in the test serie " // &
                                           trim(testSerieNumber) // " and cross section " // crossSectionNumber, 1)

                write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputRTOovertopping/output_section', i, '_test', numberTestSerie, 'berm.txt'
                errorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
                call assert_files_comparable( outputFile, frozenFile, trim(errorMessage) )
            enddo
        enddo
    enddo

    deallocate (phi)

end subroutine allCrossSectionsRTOTests

!> One test serie with varying one characteristic of the cross sections.
!!
!! @ingroup FailureMechanismsTests
subroutine TestSeriesCrossSections
!
!   Local parameters
!
    character(len=90)     :: crossSectionFile     ! file with cross section coordinates
    integer               :: ounit                ! unit-number for the output file

    real(wp)              :: psi                  ! dike normal (degree)
    integer               :: nCoordinates         ! number of coordinates
    real(wp), allocatable :: xCoordinates    (:)  ! x-coordinates (m)
    real(wp), allocatable :: yCoordinates    (:)  ! y-coordinates (m+NAP)
    real(wp), allocatable :: roughnessFactors(:)  ! roughness factors

    integer               :: ios                  ! input/output-status
    integer               :: nstep                ! number of computations in the test serie
    integer               :: istep                ! do-loop counter in the test serie

    real(wp)              :: var                  ! value of the variable in the test serie
    real(wp)              :: varmin               ! minimum value of the variable in the test serie
    real(wp)              :: varmax               ! maximum value of the variable in the test serie
    real(wp)              :: varstep              ! step size of the variable in the test serie
    integer               :: varSlope             ! number of the slope to vary

    type (tpGeometry)     :: geometry             ! structure with geometry data
    type (tpOvertopping)  :: overtopping          ! structure with overtopping results
    logical               :: succes               ! flag for succes
    character(len=250)    :: errorMessage         ! error message
    character(len=5)      :: ratio                ! ratio to be printed
!
!   source
!
    !
    ! read the cross section
    write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputRTOovertopping/Cross_section', crossSectionId, '.txt'
    call readCrossSection(crossSectionFile, geometry, succes, errorMessage)

    ! fill the model factors for the RTO overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fRunup1 = 1.75d0
    modelFactors%fRunup2 = 4.3d0
    modelFactors%fRunup3 = 1.6d0
    modelFactors%fShallow = 0.92
    modelFactors%typeRunup = 0
    modelFactors%relaxationFactor = 1.0d0

    nCoordinates = geometry%nCoordinates
    allocate (xCoordinates    (nCoordinates))
    allocate (yCoordinates    (nCoordinates))
    allocate (roughnessFactors(nCoordinates-1))

    xCoordinates     = geometry%xCoordinates
    yCoordinates     = geometry%yCoordinates
    roughnessFactors = geometry%roughnessFactors

    !
    ! boundaries for variation of the slope
    call variableBoundaries (crossSectionId, numberTestSerie, varmin, varmax, varstep, varSlope)
    nstep = nint( (varmax - varmin) / varstep)

    !
    ! Open the output file
    call getFreeLuNumber( ounit )
    open (unit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    if (ios /= 0) then
        call fatalError ('Unable to open the file: ' // trim(outputFile) )
    endif

    write (ounit,'(a)') '# Input and results test serie RTO overtopping dll'
    write (ounit,'(a)') '#  1       2       3'
    if (varSlope > 0) then
        write (ounit,'(a)') '#  Slope   Z2%     Qo'
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        write (ounit,'(a)') '#  Buckling Z2%     Qo'
    elseif (crossSectionId == 4) then
        write (ounit,'(a)') '#  BermLeng Z2%     Qo'
    endif

    psi = geometry%psi
    do istep = 0, nstep
        !
        ! Adapt the cross section
        var = varmin + dble(istep) * varstep

        call adaptCrossSection (crossSectionId, numberTestSerie, var, nCoordinates, xCoordinates, &
                                yCoordinates)

        call initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, &
                                 roughnessFactors(1:nCoordinates-1), geometry,  &
                                 succes, errorMessage)

        call checkCrossSection (geometry%psi, geometry%nCoordinates,            &
                                geometry%xCoordinates, geometry%yCoordinates,   &
                                geometry%roughnessFactors, succes, errorMessage)

        if (.not. succes) then
            call fatalError (errorMessage)
        endif
        !
        ! Compute the wave runup and the wave overtopping discharge with the RTO-overtopping module
        call calculateOvertopping (geometry, load, modelFactors, overtopping, succes, errorMessage)
        if ( .not. succes ) then
            write ( ounit, '(2a)') 'Failure: ', trim(errorMessage)
        !
        ! Write the results to the output file
        else if (varSlope > 0) then
            write(ratio, '(f5.1)') 1.0d0 / geometry%segmentSlopes(varSlope)
            if (.not. extraSlopeTest) then
                write (ounit,'(2a,f8.3,f15.10)') '    1:', trim(adjustl(ratio)), load%h + overtopping%z2, overtopping%Qo
            else
                write (ounit,'(2a,f8.3,f15.10)') '    1:', ratio, load%h + overtopping%z2, overtopping%Qo
            endif
        elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
            write (ounit,'(f9.3,f8.3,f15.10)') geometry%yCoordinates(2), load%h + overtopping%z2, overtopping%Qo
        elseif (crossSectionId == 4) then
            write (ounit,'(f9.1,f8.3,f15.10)') geometry%xCoordinates(3) - geometry%xCoordinates(2), &
                                               load%h + overtopping%z2, overtopping%Qo
        endif

    enddo
    close( ounit )

    call deallocateGeometry( geometry )

    deallocate( xCoordinates )
    deallocate( yCoordinates )
    deallocate( roughnessFactors )

end subroutine TestSeriesCrossSections

!> Define the boundaries of the specific test serie for varying one characteristic of the cross section
!!
!! @ingroup FailureMechanismsTests
subroutine variableBoundaries (crossSectionId, numberTestSerie, varmin, varmax, varstep, varSlope)
!
!   input/output parameters
!
    integer,       intent(in)  :: crossSectionId       !< id-number of the cross section 
    integer,       intent(in)  :: numberTestSerie      !< number of test serie
    real(wp),      intent(out) :: varmin               !< minimum value of the variable in the test serie
    real(wp),      intent(out) :: varmax               !< maximum value of the variable in the test serie
    real(wp),      intent(out) :: varstep              !< step size of the variable in the test serie
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
!! @ingroup FailureMechanismsTests
subroutine adaptCrossSection (crossSectionId, numberTestSerie, var, nCoordinates, xCoordinates, yCoordinates)
!
!   Input/output parameters
!
    integer,   intent(in)    :: crossSectionId             !< id-number of the cross section 
    integer,   intent(in)    :: numberTestSerie            !< number of test serie
    real(wp),  intent(in)    :: var                        !< value of the variable in the test serie
    integer,   intent(in)    :: nCoordinates               !< number of coordinates
    real(wp),  intent(inout) :: xCoordinates(nCoordinates) !< x-coordinates (m)
    real(wp),  intent(inout) :: yCoordinates(nCoordinates) !< y-coordinates (m+NAP)
!
!   source
!
    if (crossSectionId == 1) then
        ! slope
        xCoordinates(2) = var
    elseif (crossSectionId == 2) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! lower slope
            xCoordinates(2) = var
            xCoordinates(3) = xCoordinates(2) + 2.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            xCoordinates(3)  = var
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            xCoordinates(2) = var
            yCoordinates(2) = xCoordinates(2) / 8 - 2.0d0
            xCoordinates(3) = xCoordinates(2) + (6.0d0 - yCoordinates(2))
        endif
    elseif (crossSectionId == 3) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! lower slope
            xCoordinates(2) = var
            xCoordinates(3) = xCoordinates(2) + 16.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! upper slope
            xCoordinates(3)  = var
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! bucking point
            xCoordinates(2) = var
            yCoordinates(2) = xCoordinates(2) - 2.0d0
            xCoordinates(3) = xCoordinates(2) + (6.0d0 - yCoordinates(2)) * 8.0d0 
        endif
    elseif (crossSectionId == 4) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! berm slope
            yCoordinates(2) = 4.0d0 - 4.0d0 / var
            xCoordinates(2) = xCoordinates(1) + (yCoordinates(2) - yCoordinates(1)) * 4.0d0
            xCoordinates(3) = xCoordinates(2) + 4.0d0
            xCoordinates(4) = xCoordinates(3) + (yCoordinates(4) - yCoordinates(3)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            xCoordinates(2) = xCoordinates(1) + var
            xCoordinates(3) = xCoordinates(2) + 4.0d0
            xCoordinates(4) = xCoordinates(3) + var / 3.0d0
        elseif ((numberTestSerie == 12) .or. (numberTestSerie == 13)) then

            ! length Berm
            xCoordinates(3) = xCoordinates(2) + var
            xCoordinates(4) = xCoordinates(3) + 6.0d0
        endif
    elseif (crossSectionId == 5) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! first berm slope
            yCoordinates(2) = 4.0d0 - 4.0d0 / var
            xCoordinates(2) = xCoordinates(1) + (yCoordinates(2) - yCoordinates(1)) * 4.0d0
            xCoordinates(3) = xCoordinates(2) + 4.0d0
            xCoordinates(4) = xCoordinates(3) + 4.0d0
            xCoordinates(5) = xCoordinates(4) + (yCoordinates(5) - yCoordinates(4)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            xCoordinates(2) = xCoordinates(1) + (yCoordinates(2) - yCoordinates(1)) * var
            xCoordinates(3) = xCoordinates(2) + 4.0d0
            xCoordinates(4) = xCoordinates(3) + 4.0d0
            xCoordinates(5) = xCoordinates(4) + (yCoordinates(5) - yCoordinates(4)) * var
        endif
    elseif (crossSectionId == 6) then
        if ((numberTestSerie == 8) .or. (numberTestSerie == 9)) then

            ! second berm slope
            yCoordinates(4) = yCoordinates(3) + (xCoordinates(4) - xCoordinates(3)) / var
            xCoordinates(5) = xCoordinates(4) + (yCoordinates(5) - yCoordinates(4)) * 3.0d0
        elseif ((numberTestSerie == 10) .or. (numberTestSerie == 11)) then

            ! lower and upper slope
            xCoordinates(2) = xCoordinates(1) + (yCoordinates(2) - yCoordinates(1)) * var
            xCoordinates(3) = xCoordinates(2) + 4.0d0
            xCoordinates(4) = xCoordinates(3) + 4.0d0
            xCoordinates(5) = xCoordinates(4) + (yCoordinates(5) - yCoordinates(4)) * var
        endif
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then

        ! lower, middle and upper slope
        xCoordinates(2) = xCoordinates(1) + (yCoordinates(2) - yCoordinates(1)) * var
        xCoordinates(3) = xCoordinates(2) + 4.0d0
        xCoordinates(4) = xCoordinates(3) + (yCoordinates(4) - yCoordinates(3)) * var
        xCoordinates(5) = xCoordinates(4) + 4.0d0
        xCoordinates(6) = xCoordinates(5) + (yCoordinates(6) - yCoordinates(5)) * var
    endif

end subroutine adaptCrossSection

!>  Define the boundaries of the specific test serie for varying one characteristic of the cross section
!!
!! @ingroup FailureMechanismsTests
subroutine numberExtraTestsSlopes( crossSectionId, nTestSeriesSlopes )
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
        call fatalError ('Wrong cross section id-number')
    endif

end subroutine numberExtraTestsSlopes

!> test for the adjustment of the cross section
!!
!! @ingroup FailureMechanismsTests
subroutine TestProfileAdjustment
!
!   Local parameters
!
    integer                 :: nCoordinates         ! number of coordinates
    integer                 :: nrCoordsAdjusted     ! number of coordinates
    real(wp), allocatable   :: xCoordinates(:)      ! x-coordinates (m)
    real(wp), allocatable   :: yCoordinates    (:)  ! y-coordinates (m+NAP)
    real(kind=wp), pointer  :: xCoordsAdjusted(:)   ! vector with x-coordinates of the adjusted profile
    real(kind=wp), pointer  :: zCoordsAdjusted(:)   ! vector with y-coordinates of the adjusted profile
    logical                 :: succes               ! flag for succes
    character(len=255)      :: errorMessage         ! error message
    real(kind=wp)           :: dikeHeight           ! vector with x-coordinates of the adjusted profile
    integer                 :: i                    ! do-loop counter
    
    nCoordinates = 4
    allocate (xCoordinates (nCoordinates))
    allocate (yCoordinates (nCoordinates))
    
    xCoordinates(1) = 28.5d0
    xCoordinates(2) = 30.55d0
    xCoordinates(3) = 34.17d0
    xCoordinates(4) = 37.0d0
    
    yCoordinates(1) = 0.02d0
    yCoordinates(2) = 0.36d0
    yCoordinates(3) = 1.12d0
    yCoordinates(4) = 1.74d0
    
    dikeHeight = 0.74d0
    
    call profileInStructure( nCoordinates, xcoordinates, ycoordinates, dikeHeight, nrCoordsAdjusted, xCoordsAdjusted, zCoordsAdjusted, succes, errorMessage )
    
    do i = 2, nrCoordsAdjusted
        call assert_true (xCoordsAdjusted(i) > xCoordsAdjusted(i-1), "X coordinates increasing" )
    enddo

    deallocate( xCoordinates )
    deallocate( yCoordinates )

    deallocate( xCoordsAdjusted )
    deallocate( zCoordsAdjusted )

end subroutine TestProfileAdjustment

end module crossSectionsAdaptionTests