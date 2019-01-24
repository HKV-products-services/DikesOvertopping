! Copyright (C) Stichting Deltares 2019. All rights reserved.
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
!! Contains the module readCrossSectionForTests. This module is used in the tests of the Overtopping dll
!
!
! $Id$
!
!> 
!!Module for reading the cross section file for testing the Overtopping kernel
!!
!! @ingroup DikeOvertoppingTests
module readCrossSectionForTests

    use utilities
    use precision
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use ftnunit

    implicit none
    
    private
    
    public :: readCrossSection, numberTestSeriesSlopes, readNumberOfRelevantLines

contains

!> Routine to read the cross section file
!!
!! @ingroup DikesOvertoppingTests
subroutine readCrossSection(crossSectionFile, geometry, succes, errorMessage)
!
!   input/output parameters
!
    character(len=*),  intent(in)  :: crossSectionFile  !< file with cross section coordinates
    type (tpGeometry), intent(out) :: geometry          !< structure with geometry data
    logical,           intent(out) :: succes            !< flag for succes
    character(len=*),  intent(out) :: errorMessage      !< error message
!
!   local parameters
!
    integer                     :: punit                ! unit-number for the cross section file
    integer                     :: ios                  ! input/output-status
    character(len=1)            :: comment              ! comment character

    integer                     :: i                    ! counter
    real(kind=wp)               :: psi                  ! dike normal (degree)
    integer                     :: nCoordinates         ! number of coordinates
    real(kind=wp), allocatable  :: xCoordinates    (:)  ! x-coordinates (m)
    real(kind=wp), allocatable  :: yCoordinates    (:)  ! y-coordinates (m+NAP)
    real(kind=wp), allocatable  :: roughnessFactors(:)  ! roughness factors
!
!   source
!
    !
    ! Read the number of coordinates of the cross section
    call readNumberOfRelevantLines(crossSectionFile, nCoordinates)
    allocate (xCoordinates    (nCoordinates))
    allocate (yCoordinates    (nCoordinates))
    allocate (roughnessFactors(nCoordinates))

    ios=0
    call getFreeLuNumber(punit)
    open (unit=punit, file=trim(crossSectionFile), status='old', iostat=ios)
    call assert_equal(ios, 0, 'Unable to open the file: ' // trim(crossSectionFile))
    !
    ! Skip comment lines in cross section file
    comment='#'
    do while (comment == '#')
       read (punit,'(a)', iostat=ios) comment
       call assert_equal(ios, 0, 'Read error from the file: ' // trim(crossSectionFile))
    enddo
    backspace(unit=punit)
    !
    ! Read the cross section 
    do i = 1, nCoordinates
        read(punit,*,iostat=ios) xcoordinates(i), ycoordinates(i), roughnessfactors(i)
        call assert_equal(ios, 0, 'Read error from the file: ' // trim(crossSectionFile))
    enddo
    close(punit)
    !
    ! Set dike normal (this is used for all cross sections in all test series) 
    psi = 0.0d0
    !
    ! Initialize geometry structure
    call initializeGeometry (psi, nCoordinates, xCoordinates, yCoordinates, &
                             roughnessFactors(1:nCoordinates-1), geometry,  &
                             succes, errorMessage)

    deallocate (xCoordinates)
    deallocate (yCoordinates)
    deallocate (roughnessFactors)

end subroutine readCrossSection

!> Read the number of relevant lines in a cross section file for testing the Overtopping dll
!!
!! @ingroup DikesOvertoppingTests
subroutine readNumberOfRelevantLines (fileName, NumberOfRelevantLines)
!
!   input/output parameters
!
    character(len=*), intent(in)  :: filename                ! file name
    integer,          intent(out) :: numberOfRelevantLines   ! Number of relevant lines
!
!   local parameters
!
    integer            :: ios           !  IO status
    integer            :: ifile         !  Unit number
    integer            :: i             !  do-loop counter
    character(len=100) :: line          !  line of the input file
!
!   source
!
    call getFreeLuNumber(ifile)
    open(ifile, file=trim(filename), status='old', iostat=ios)
    call assert_equal(ios, 0, 'File ' // trim(filename) // ' can not opened')
    !
    ! Read the file 
    i = 0
    do while (ios == 0)
        read (ifile,'(a)', iostat=ios) line
        !
        ! count the line if the first symbol is not equal to # 
        if (line(1:1) /= '#') then
            i = i + 1
        endif
    enddo

    numberOfRelevantLines = i - 1
    !
    ! close file
    close(ifile)

end subroutine readNumberOfRelevantLines

!> Assign the number of test series for the slopes of a cross section
!!
!! @ingroup DikesOvertoppingTests
subroutine numberTestSeriesSlopes (crossSectionId, nTestSeriesSlopes)
!
!   input/output parameters
!
    integer,    intent(in)      :: crossSectionId       !< id-number of the cross section
    integer,    intent(out)     :: nTestSeriesSlopes    !< amount of test series for slopes

    if (crossSectionId == 1) then
        nTestSeriesSlopes = 1
    elseif ((crossSectionId == 2) .or. (crossSectionId == 3)) then
        nTestSeriesSlopes = 3
    elseif (crossSectionId == 4) then
        nTestSeriesSlopes = 3
    elseif ((crossSectionId == 5) .or. (crossSectionId == 6)) then
        nTestSeriesSlopes = 2
    elseif ((crossSectionId == 7) .or. (crossSectionId == 8)) then
        nTestSeriesSlopes = 1
    else
        call assert_true(.false. , 'Wrong cross section id-number')
    endif

end subroutine numberTestSeriesSlopes

end module readCrossSectionForTests
