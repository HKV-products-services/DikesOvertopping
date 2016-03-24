!> @file
!! Contains the module dllTests of the DikesOvertopping dll
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!> 
!! Module, holding tests of the functions of the dll.
!!
!! @ingroup DikeOvertoppingTests
module dllTests
use precision, only : wp
use overtoppingInterface, only : OvertoppingGeometryTypeF
use typeDefinitionsRTOovertopping
use ModuleLogging
use waveParametersUtilities, only : computeWavePeriod
use ftnunit
use errorMessages

implicit none

private

public :: overtoppingDllTest, overtoppingValidationTest, overtoppingZ2Test, influenceRoughnessTest, &
          overtoppingValidationRoughnessTest, overtoppingMultipleValidationTest

contains
!> Test the functions in DikesOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow ( waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup FailureMechanismsTests
subroutine overtoppingDllTest
    use user32
    use kernel32

    real(kind=wp)      :: z                 !< z value

    real(kind=wp), parameter :: zExpected1a =   11.725
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer                        :: p
    character(len=12)              :: version
    external                       :: calcZValue
    external                       :: calculateQoF
    external                       :: versionNumber
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging

    pointer            (qz, calcZValue)
    pointer            (qv, versionNumber)
    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qv = getprocaddress (p, "versionNumber"C)
    qc = getprocaddress (p, "calculateQoF"C)
    qz = getprocaddress (p, "calcZValue"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2     = 1.00_wp
    modelFactors%frunup1  = 1.65_wp
    modelFactors%frunup2  = 4.00_wp
    modelFactors%frunup3  = 1.50_wp
    modelFactors%typeRunup = 1
    modelFactors%fshallow = 0.92
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    modelFactors%relaxationFactor    = 1.0d0
    criticalOvertoppingRate        = 1.0d-3

    call versionNumber(version)

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  5.50_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( overtopping%Qo, 0.808902537811215d-8, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable ( overtopping%z2, 1.51985829057_wp, margin, 'z2 from dllOvertopping.dll')
    call calcZValue ( criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_true ( succes, errorMessage )
    call assert_comparable( z, zExpected1a, margin, "Z value from dllOvertopping.dll; overflow" )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel > dikeheigth (overflow)
    !
    load%h        =  9.50_wp
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_false ( succes, errorMessage )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth, without waves
    !
    load%h        =  5.50_wp
    load%Hm0      =  0.00_wp
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call calcZValue ( criticalOvertoppingRate, modelFactors, overtopping%Qo, z, succes, errorMessage)
    call assert_comparable( z, zExpected1b, margin, "Z value from dllOvertopping.dll; no waves test" )

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingDllTest

!> Test the functions in dllOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow ( waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup FailureMechanismsTests
subroutine overtoppingZ2Test
    use user32
    use kernel32

    real(kind=wp), parameter :: zExpected1a =   29.88985_wp
    real(kind=wp), parameter :: zExpected1b =  701.48866_wp
    real(kind=wp), parameter :: zExpected2 = -6.43985_wp
    real(kind=wp), parameter :: margin     =  0.0000100_wp

    integer                        :: p
    external                       :: calculateQoF
    integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 3
    real(kind=wp)                  :: waveSteepness
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2     = 1.00_wp
    modelFactors%frunup1  = 1.65_wp
    modelFactors%frunup2  = 4.00_wp
    modelFactors%frunup3  = 1.50_wp
    modelFactors%typeRunup = 1
    modelFactors%fshallow = 0.92
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    modelFactors%relaxationFactor    = 1.0d0
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    do i = 1, npoints
        geometryF%xcoords(i)   = 5 * i
        geometryF%ycoords(i)   = 3 + 2 * i
        if (i < npoints) geometryF%roughness(i) = 1.0_wp
    enddo
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    !
    load%h        =  8.99_wp
    load%phi      = 50.00_wp
    load%Hm0      =  1.00_wp
    waveSteepness =  0.04_wp
    load%Tm_10    = computeWavePeriod( load%Hm0, waveSteepness )
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_comparable ( overtopping%Qo, 0.48213d0, margin, 'Qo from dllOvertopping.dll')
    call assert_comparable ( overtopping%z2,  2.874674352d0, margin, 'z2 from dllOvertopping.dll')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingZ2Test

!> Test influence roughness in dllOvertopping.dll.
!!
!!
!! @ingroup FailureMechanismsTests
subroutine influenceRoughnessTest
    use user32
    use kernel32

    integer                        :: p
    external                       :: calculateQoF
   !integer                        :: i
    logical                        :: succes
    integer, parameter             :: npoints = 2
    type (tpOvertopping)           :: overtopping
    character(len=128)             :: errorMessage
    type (tpLoad)                  :: load              !< structure with load data
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    type(tLogging)                 :: logging

    pointer            (qc, calculateQoF)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "calculateQoF"C)
    !
    ! initializations
    !
    dikeHeight = 3.7_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2                       = 1.00_wp
    modelFactors%frunup1                    = 1.65_wp
    modelFactors%frunup2                    = 4.00_wp
    modelFactors%frunup3                    = 1.50_wp
    modelFactors%typeRunup                  = 1
    modelFactors%fshallow                   = 0.67778_wp
    modelFactors%ComputedOvertopping        = 1.0_wp
    modelFactors%CriticalOvertopping        = 1.0_wp
    modelFactors%relaxationFactor           = 1.0d0
    modelFactors%reductionfactorforeshore   = 0.5_wp

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords(1) =  0.0_wp
    geometryF%xcoords(2) = 24.0_wp
    geometryF%ycoords(1) = -3.0_wp
    geometryF%ycoords(2) =  3.0_wp
    geometryF%roughness  =  1.0_wp
    geometryF%normal     =  0.0_wp
    geometryF%npoints    =  npoints
    !
    load%h     = -0.361314622129615_wp
    load%phi   = 45.0_wp
    load%hm0   = 1d-6
    load%tm_10 = 1.912229230397281D-012
    !
    ! test actual computations in calculateQo and zFuncOvertopping for waterlevel < dikeheigth
    !
    logging%verbosity = -1
    logging%filename = ' '
    call calculateQoF( load, geometryF, dikeHeight, modelFactors, overtopping, succes, errorMessage, logging )
    call assert_true ( succes, errorMessage )
    call assert_inbetween ( overtopping%z2, 1d-16, 2d-15, 'difference in z2')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine influenceRoughnessTest

!> Test the functions in dllOvertopping.dll.
!!     these functions are:
!!     - calcZValue
!!     - calculateQoF
!!     - versionNumber
!!
!!     - test overflow ( waterlevel > dike height)
!!     - test with and without waves
!!
!! @ingroup FailureMechanismsTests
subroutine overtoppingValidationTest
    use user32
    use kernel32

    integer                        :: p
    external                       :: ValidateInputFold, SetLanguage
    logical                        :: succes
    integer, parameter             :: npoints = 5
    character(len=128)             :: errorMessage
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputFold)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputFold"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2     = 1.00_wp
    modelFactors%frunup1  = 1.75_wp
    modelFactors%frunup2  = 4.30_wp
    modelFactors%frunup3  = 1.60_wp
    modelFactors%typeRunup = 1
    modelFactors%fshallow  = 0.92
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    modelFactors%relaxationFactor    = 1.0d0
    criticalOvertoppingRate          = 1.0d-3
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, -1, 4, 0]
    geometryF%roughness = [ 0.5, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input ( geometry not correct ) :
    !
    call ValidateInputFold( geometryF, dikeHeight, modelFactors, succes, errorMessage )
    
    call assert_equal(trim(errorMessage), 'Fout in bepaling van gecorrigeerde x-coordinaten', 'error handling')

    !
    ! do validation of input ( modelfactor m_z2 < 0 ) :
    !
    geometryF%ycoords = [-5, 0, 5, 6, 7]
    modelFactors%m_z2     = -1.00_wp
    call SetLanguage('UK')
    call ValidateInputFold( geometryF, dikeHeight, modelFactors, succes, errorMessage )
    
    call assert_equal(trim(errorMessage), 'Model factor 2% wave runup smaller than  0.000', 'error handling')
    
    !
    ! do validation of input ( modelfactor foreshore < 0.3 ) :
    !
    modelFactors%m_z2                      = 1.00_wp
    modelFactors%reductionFactorForeshore  = 0.25_wp
    call ValidateInputFold( geometryF, dikeHeight, modelFactors, succes, errorMessage )
    call assert_equal(trim(errorMessage), 'Model factor reduction factor foreshore not between  0.300 and  1.000', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingValidationTest

subroutine overtoppingValidationRoughnessTest
    use user32
    use kernel32

    integer                        :: p
    external                       :: ValidateInputFold, SetLanguage
    logical                        :: succes
    integer, parameter             :: npoints = 5
    character(len=128)             :: errorMessage
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputFold)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputFold"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2     = 1.00_wp
    modelFactors%frunup1  = 1.75_wp
    modelFactors%frunup2  = 4.30_wp
    modelFactors%frunup3  = 1.60_wp
    modelFactors%typeRunup = 1
    modelFactors%fshallow  = 0.92
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    modelFactors%relaxationFactor    = 1.0d0
    criticalOvertoppingRate          = 1.0d-3
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, 5, 6, 7]
    geometryF%roughness = [ 0.0, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    !
    ! do validation of input ( geometry not correct ) :
    !
    call SetLanguage('NL')
    call ValidateInputFold( geometryF, dikeHeight, modelFactors, succes, errorMessage )
    
    call assert_equal(trim(errorMessage), 'Ruwheidsfactoren moeten liggen tussen 0.5 ... 1.0; gevonden:  0.00', 'error handling')

    geometryF%roughness = [ 0.49, 0.5, 0.5, 0.5 ]
    call ValidateInputFold( geometryF, dikeHeight, modelFactors, succes, errorMessage )
    
    call assert_equal(trim(errorMessage), 'Ruwheidsfactoren moeten liggen tussen 0.5 ... 1.0; gevonden:  0.49', 'error handling')
    !
    ! do validation of input ( modelfactor m_z2 < 0 ) :
    !
end subroutine overtoppingValidationRoughnessTest

!! @ingroup FailureMechanismsTests
subroutine overtoppingMultipleValidationTest
    use user32
    use kernel32

    integer                        :: p
    external                       :: ValidateInputF, SetLanguage
    integer, parameter             :: npoints = 5
    type(TErrorMessages)           :: errorStruct
    type(OvertoppingGeometryTypeF) :: geometryF
    real(kind=wp)                  :: dikeHeight
    type(tpOvertoppingInput)       :: modelFactors
    real(kind=wp)                  :: criticalOvertoppingRate

    pointer            (qc, ValidateInputF)
    pointer            (qsl, SetLanguage)

    p = loadlibrary    ("dllDikesOvertopping.dll"C) ! the C at the end says add a null byte as in C
    qc = getprocaddress (p, "ValidateInputF"C)
    qsl = getprocaddress (p, "SetLanguage"C)
    !
    ! initializations
    !
    dikeHeight  = 9.1_wp
    modelFactors%factorDeterminationQ_b_f_n = 2.3_wp
    modelFactors%factorDeterminationQ_b_f_b = 4.3_wp
    modelFactors%m_z2     = 1.00_wp
    modelFactors%frunup1  = 1.75_wp
    modelFactors%frunup2  = 4.30_wp
    modelFactors%frunup3  = 1.60_wp
    modelFactors%typeRunup = 1
    modelFactors%fshallow  = 0.92
    modelFactors%ComputedOvertopping = 1.0_wp
    modelFactors%CriticalOvertopping = 1.0_wp
    modelFactors%relaxationFactor    = 1.0d0
    criticalOvertoppingRate          = 1.0d-3
    criticalOvertoppingRate        = 1.0d-3

    allocate(geometryF%xcoords(npoints), geometryF%ycoords(npoints), geometryF%roughness(npoints-1))
    geometryF%xcoords = [ 0, 10, 20, 30, 40 ]
    geometryF%ycoords = [-5, 0, 5, 4, 0]
    geometryF%roughness = [ 0.5, 0.5, 0.5, 0.5 ]
    
    geometryF%normal = 60.0_wp ! degrees
    geometryF%npoints = npoints
    modelFactors%m_z2     = -1.00_wp
    modelFactors%reductionFactorForeshore  = 0.25_wp
    !
    ! do validation of input :
    !
    call initErrorMessages(errorStruct)
    call SetLanguage('UK')
    call ValidateInputF( geometryF, dikeHeight, modelFactors, errorStruct )

    call assert_equal(3, errorStruct%nErrors, 'expected 3 validation errors')
    call assert_equal(trim(errorStruct%messages(1)%message), 'Error in calculation of adjusted x-coordinates', 'error handling')
    
    call assert_equal(trim(errorStruct%messages(2)%message), 'Model factor 2% wave runup smaller than  0.000', 'error handling')
    
    call assert_equal(trim(errorStruct%messages(3)%message), 'Model factor reduction factor foreshore not between  0.300 and  1.000', 'error handling')

    deallocate(geometryF%xcoords, geometryF%ycoords, geometryF%roughness)

end subroutine overtoppingMultipleValidationTest

end module dllTests
