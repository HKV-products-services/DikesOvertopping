!> @file
!! Contains the module crossSectionRoughnessTests of the RTO overtopping dll
!! RTO stands for Risk, Assessment and Design instruments (in dutch: Risico, Toets en Ontwerpinstrumentarium)
!
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
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
    use feedback
    use utilities
    use waveParametersUtilities
    use mainModuleRTOovertopping
    use readCrossSectionForTests
    use typeDefinitionsRTOovertopping
    use geometryModuleRTOovertopping
!
    implicit none

    type (tpOvertoppingInput),  private     :: modelFactors         ! structure with model factors
    type (tpLoad),              private     :: load                 ! structure with load data
    character(len=90),          private     :: outputFile           ! file for the output of the testserie
    integer,                    private     :: crossSectionId       ! id-number of the cross section 
    integer,                    private     :: numberTestSerie      ! number of test serie
    real(wp), pointer,          private     :: roughness(:)         ! roughness variation for segment numbers

    private
    
    public :: allCrossSectionRoughnessTests

contains

!> Call all the roughness varying overtopping test series for the RTO overtopping dll. In these test series 
!! the roughness of the cross sections is varied.
!!
!! @ingroup FailureMechanismsTests
subroutine allCrossSectionRoughnessTests( nCrossSections, nBasicTestSeries )
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

    integer                 :: i, j, k              ! do-loop counters
    integer                 :: nTestSeriesSlopes    ! amount of test series for slopes
    integer                 :: nWaveDirections      ! number of wave directions in the test series
    integer                 :: nRoughnesses         ! number of test series for the roughness for the specific cross section

    real(wp), allocatable   :: phi(:)               ! wave directions
    real(wp)                :: waveSteepness        ! wave steepness
!
!   source
!
    !
    ! fill the model factors for the RTO overtopping dll
    modelFactors%factorDeterminationQ_b_f_n      = 2.3d0
    modelFactors%factorDeterminationQ_b_f_b      = 4.3d0
    modelFactors%m_z2    = 1.0d0
    modelFactors%fRunup1 = 1.65d0
    modelFactors%fRunup2 = 4.0d0
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

    !
    ! execute roughness test series for all cross sections
    do i = 1, nCrossSections
        crossSectionId = i
        call numberTestSeriesSlopes (i, nTestSeriesSlopes)
        call numberTestSeriesRoughness (i, nRoughnesses)
        numberTestSerie = nBasicTestSeries + nWaveDirections * nTestSeriesSlopes
        do j = 1, nRoughnesses
            call roughnesses (i, j, roughness)

            do k = 1, nWaveDirections
                load%phi = phi(k)
                numberTestSerie = numberTestSerie + 1
                
                write (outputFile,'(a,i1,a,i2.2,a)') './output_section', crossSectionId, '_test', &
                                        numberTestSerie, '.txt'

                write (crossSectionNumber,'(I1)') crossSectionId
                write (testSerieNumber,   '(I2)') numberTestSerie

                call testWithLevel(TestSeriesRoughness, "Calculations (Cross Section Roughness) with the RTO overtopping dll in the test serie " // &
                                           trim(testSerieNumber) // " and cross section " // crossSectionNumber, 1)

                write (frozenFile,'(a,i1,a,i2.2,a)') '../DikesOvertoppingTests/OutputRTOovertopping/output_section', i, '_test', numberTestSerie, '.txt'
                errorMessage = 'The file "' // trim(outputFile) // '" differs with the same file computed before.'
                call assert_files_comparable( outputFile, frozenFile, trim(errorMessage) )

            enddo
            deallocate (roughness)
        enddo
    enddo
    deallocate (phi)

end subroutine allCrossSectionRoughnessTests

!> One test serie with varying the roughness of one or more segments of the cross sections.
!!
!! @ingroup FailureMechanismsTests
subroutine TestSeriesRoughness
!
!   local parameters
!
    character(len=90)    :: crossSectionFile     ! file with cross section coordinates
    integer              :: ounit                ! unit-number for the output file

    integer              :: i                    ! do-loop counter
    integer              :: segment              ! segment numner

    integer              :: ios                  ! input/output-status
    integer              :: nstep                ! number of computations in the test serie
    integer              :: istep                ! do-loop counter in the test serie

    real(wp)             :: var                  ! value of the variable in the test serie
    real(wp)             :: varmin               ! minimum value of the variable in the test serie
    real(wp)             :: varmax               ! maximum value of the variable in the test serie
    real(wp)             :: varstep              ! step size of the variable in the test serie

    type (tpGeometry)    :: geometry             ! structure with geometry data
    type (tpOvertopping) :: overtopping          ! structure with overtopping results
    logical              :: succes               ! flag for succes
    character(len=250)   :: errorMessage         ! error message
!
!   source
!
    ! read the cross section
    !
    write (crossSectionFile,'(a,i1,a)') '../DikesOvertoppingTests/InputRTOovertopping/Cross_section', crossSectionId, '.txt'
    call readCrossSection( crossSectionFile, geometry, succes, errorMessage )
    !
    ! open the output file
    call getFreeLuNumber( ounit )
    open (unit=ounit, file=trim(outputFile), status='unknown', iostat=ios)
    if (ios /= 0) then
        call fatalError ('Unable to open the file: ' // trim(outputFile) )
    endif

    write (ounit,'(a)') '# Input and results test serie RTO overtopping dll'
    write (ounit,'(a)') '#  1          2       3'
    write (ounit,'(a)') '#  Roughness  Z2%     Qo'
    !
    ! boundaries for variation of the roughness
    varmin  =  0.5d0
    varmax  =  1.0d0
    varstep =  0.01d0
    nstep = nint( (varmax - varmin) / varstep)

    do istep = 0, nstep
        var = varmin + dble(istep) * varstep

        !
        ! Adapt the roughness of the cross section
        do i = 1, size(roughness)
            segment = roughness(i)
            geometry%roughnessFactors(segment) = var
        enddo

        !
        ! compute the wave runup and the wave overtopping discharge with the RTO-overtopping module
        call calculateOvertopping (geometry, load, modelFactors, overtopping, succes, errorMessage)
        if ( .not. succes ) then
            write ( ounit, '(2a)') 'Failure: ', trim(errorMessage)
        else
            !
            ! write the results to the output file
            write (ounit,'(f8.2,f11.3,f15.10)') var, load%h + overtopping%z2, overtopping%Qo
        end if
    enddo
    close( ounit )

    call deallocateGeometry( geometry )

end subroutine TestSeriesRoughness

!> Routine to determine the nummer of slope varying test series per cross section.
!!
!! @ingroup FailureMechanismsTests
subroutine numberTestSeriesRoughness( crossSectionId, nRoughnesses )
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
        call fatalError ('Wrong cross section id-number')
    endif

end subroutine numberTestSeriesRoughness

!> Routine to determine the slopes to vary the roughness in the test series per cross section.
!!
!! @ingroup FailureMechanismsTests
subroutine roughnesses( crossSectionId, roughnessSlopesId, roughness )
!
!   input/output parameters
!
    integer,    intent(in)      :: crossSectionId       !< id-number of the cross section
    integer,    intent(in)      :: roughnessSlopesId    !< id-number of the roughness variation
    real(wp),   pointer         :: roughness(:)         !< roughness variation structure with segment numbers
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
            call fatalError ('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 2
            allocate (roughness(nSegments) )
            roughness(1) = 1
            roughness(2) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 1
            allocate (roughness(nSegments) )
            roughness(1) = 1
        elseif (roughnessSlopesId == 3) then
            ! third roughness test serie
            nSegments = 1
            allocate (roughness(nSegments) )
            roughness(1) = 2
        else
            call fatalError ('Wrong roughnesses id-number')
        endif
    elseif (crossSectionId == 4) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 1
            allocate (roughness(nSegments) )
            roughness(1) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 2
            allocate (roughness(nSegments) )
            roughness(1) = 1
            roughness(2) = 3
        else
            call fatalError ('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 5) .or. (crossSectionId == 6)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 1
            allocate (roughness(nSegments) )
            roughness(1) = 2
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 1
            allocate (roughness(nSegments) )
            roughness(1) = 3
        elseif (roughnessSlopesId == 3) then
            ! third roughness test serie
            nSegments = 2
            allocate (roughness(nSegments) )
            roughness(1) = 1
            roughness(2) = 3
        elseif (roughnessSlopesId == 4) then
            ! fourth roughness test serie
            nSegments = 2
            allocate (roughness(nSegments) )
            roughness(1) = 2
            roughness(2) = 4
        else
            call fatalError ('Wrong roughnesses id-number')
        endif
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then
        if (roughnessSlopesId == 1) then
            ! first roughness test serie
            nSegments = 2
            allocate (roughness(nSegments) )
            roughness(1) = 2
            roughness(2) = 4
        elseif (roughnessSlopesId == 2) then
            ! second roughness test serie
            nSegments = 3
            allocate (roughness(nSegments) )
            roughness(1) = 1
            roughness(2) = 3
            roughness(3) = 5
        else
            call fatalError ('Wrong roughnesses id-number')
        endif
    else
        call fatalError ('Wrong cross section id-number')
    endif

end subroutine roughnesses

end module crossSectionRoughnessTests