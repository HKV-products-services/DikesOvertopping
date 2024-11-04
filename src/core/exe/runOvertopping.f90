program runOvertopping
    use mainModuleOvertopping
    use typeForLoad
    use overtoppingInterface
    use typeDefinitionsOvertopping
    use errorMessages
    use geometryModuleOvertopping
    use zFunctionsOvertopping
    use omkeerVariantModule
    implicit none
    character(len=256) :: infile, infile2, errorText
    integer :: iunit, iunit2, nPoints, ierr
    double precision:: dikeHeight, psi, givenDischarge
    double precision, allocatable :: roughnessFactors(:)
    type(tpCoordinatePair) :: coordinates, coordinatesAdj
    type(tpLoad) :: load           !< struct with waterlevel and wave parameters
    type(tpGeometry), target :: geometry       !< fortran struct with geometry and roughness
    type(tpOvertoppingInput) :: modelFactors   !< struct with modelfactors
    type(tpOvertopping) :: overtopping, overtopping2
    logical             :: success
    type(tMessage)      :: error
    type(OvertoppingGeometryTypeF) :: geometryF

    if (command_argument_count() < 1) then
        write(*,*) "need name of input file as first argument"
        stop -1
    end if

    call get_command_argument( 1, infile, status = ierr )

    write(*,*) 'input file : ', trim(infile)
    open(newunit = iunit, file = infile, status='old', action='read', iostat=ierr)
    if (ierr == 0) read(iunit,*, iostat=ierr) nPoints
    if (ierr == 0) allocate(coordinates%x(nPoints), coordinates%y(nPoints), roughnessFactors(nPoints-1), stat=ierr)
    if (ierr == 0) read(iunit,*,iostat=ierr) coordinates%x
    if (ierr == 0) read(iunit,*,iostat=ierr) coordinates%y
    if (ierr == 0) read(iunit,*,iostat=ierr) roughnessFactors
    if (ierr == 0) read(iunit,*,iostat=ierr) psi
    if (ierr == 0) read(iunit,*,iostat=ierr) dikeHeight
    if (ierr == 0) read(iunit,*,iostat=ierr) load
    if (ierr /= 0) then
        write(*,*) "error at opening or reading file: ", infile
        stop -1
    end if
    close(iunit)

    coordinates%n = nPoints
    call profileInStructure(coordinates, dikeHeight, coordinatesAdj, error)
    if (error%errorCode == 0) then
        call initializeGeometry (psi, coordinatesAdj, roughnessFactors(1:coordinates%N-1), geometry, error)
        allocate(geometry%parent)
        geometry%parent%base => geometry
        call setupGeometries(geometry%parent)
    end if

    modelFactors%factorDeterminationQ_b_f_n = 1.0d0
    modelFactors%factorDeterminationQ_b_f_b = 1.0d0
    modelFactors%m_z2                       = 1.0d0
    modelFactors%fshallow                   = 1.0d0
    modelFactors%ComputedOvertopping        = 1.0d0
    modelFactors%CriticalOvertopping        = 1.0d0
    modelFactors%relaxationFactor           = 1.0d0
    modelFactors%reductionFactorForeshore   = 0.5d0

    if (command_argument_count() > 1) then
        call get_command_argument( 2, infile2, status = ierr )

        write(*,*) 'model factors from : ', trim(infile2)
        if (ierr == 0) open(newunit = iunit2, file = infile2, status='old', action='read', iostat=ierr)
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%factorDeterminationQ_b_f_n
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%factorDeterminationQ_b_f_b
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%m_z2
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%fshallow
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%ComputedOvertopping
        if (ierr == 0) read(iunit2,*, iostat=ierr) modelFactors%CriticalOvertopping
        if (ierr /= 0) then
            write(*,*) "error at opening or reading file: ", infile2
            stop -1
        end if
        close(iunit2)
    end if

    if (error%errorCode == 0) then
        call calculateOvertopping (geometry, load, modelFactors, overtopping, error)
    end if

    givenDischarge = modelFactors%CriticalOvertopping
    if (error%errorCode == 0) then
        geometryF%normal  = psi
        geometryF%nPoints = nPoints
        allocate(geometryF%xCoords(nPoints), geometryF%yCoords(nPoints), geometryF%roughness(nPoints))
        geometryF%xCoords = coordinates%x
        geometryF%yCoords = coordinates%y
        geometryF%roughness = roughnessfactors
        call iterateToGivenDischarge(load, geometryF, givenDischarge, dikeHeight, modelFactors, overtopping2, error)
    end if

    if (error%errorCode == 0) then
        write(*,*) 'z-2%           = ', overtopping%z2
        write(*,*) 'q              = ', overtopping%qo
        write(*,*) 'omkeer variant = ', dikeHeight
    else
        write(*,*) 'error: ', error%message
    endif

end program runOvertopping

! format input file
! nPoints  # number of coordinates
! x1 .. xN # all xcoordinates
! y1 .. yN # all ycoordinates
! r1 .. rN-1 # all roughness
! psi        # dikenormal
! dikeheight
! load
