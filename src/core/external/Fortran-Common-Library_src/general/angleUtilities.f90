! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
!
! This file is part of the Hydra Ring Application.
!
! The Hydra Ring Application is free software: you can redistribute it and/or modify
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

!> @file angleUtilities.f90
!!  Implement various utility functions dealing with directions.
!
! $Id$
!
!>
!! Module with direction utility functions
!!   @ingroup general
module angleUtilities

    use precision
    use errorMessages

    implicit none

    private

    public :: angleBetween2Directions, signedAngleBetween2Directions

    contains
!>
!! Function angleBetween2Directions() for the computation of the (absolute value of the) angle between two directions. \n
!! The two directions must be given in degrees and within [0, 360] \n
!! The computed angle between these two directions is also in degrees, and within [0, 180] \n
!! @ingroup general
function angleBetween2Directions( direction1, direction2, error)

    real( kind= wp) , intent(in)    :: direction1               !< first  direction (an angle in degrees, in range [0, 360])
    real( kind= wp) , intent(in)    :: direction2               !< second direction (idem)
    type(tMessage),   intent(out)   :: error                    !< error code/message
    real( kind= wp)                 :: angleBetween2Directions  !< angle between the two directions

    error = errorNone
!   Check that the input directions are within [0, 360] degrees:
    if( direction1 < 0.0d0  .or. direction1 > 360.0d0) then
        error%errorCode = 1
        call setErrorMessage(1, 'angleBetween2Directions', error%message)
    else if( direction2 < 0.0d0  .or. direction2 > 360.0d0) then
        error%errorCode = 1
        call setErrorMessage(2, 'angleBetween2Directions', error%message)
    endif

!   Compute the angle between the two directions:
    if (error%errorCode == 0) then
        angleBetween2Directions= modulo( direction1 - direction2, 360.0d0)
        if ( angleBetween2Directions >  180.0d0 ) then
            angleBetween2Directions= 360.0d0 - angleBetween2Directions
        endif
    else
        call set_nan(angleBetween2Directions)
    endif

end function angleBetween2Directions
!
!> alternative for angleBetween2Directions
!! result is in range [-180, +180]
function signedAngleBetween2Directions(direction1, direction2, error)
    real(kind=wp),    intent(in)    :: direction1                     !< first  direction (an angle in degrees, in range [0, 360])
    real(kind=wp),    intent(in)    :: direction2                     !< second direction (idem)
    type(tMessage),   intent(out)   :: error                          !< error code/message
    real( kind= wp)                 :: signedAngleBetween2Directions  !< angle between the two directions

    error = errorNone
!   Check that the input directions are within [0, 360] degrees:
    if( direction1 < 0.0d0 .or. direction1 > 360.0d0) then
        error%errorCode = 1
        call setErrorMessage(1, 'signedAngleBetween2Directions', error%message)
    else if( direction2 < 0.0d0 .or. direction2 > 360.0d0) then
        error%errorCode = 1
        call setErrorMessage(2, 'signedAngleBetween2Directions', error%message)
    endif

    if (error%errorCode == 0) then
        signedAngleBetween2Directions = direction1 - direction2
        if (signedAngleBetween2Directions > 180.0_wp) then
            signedAngleBetween2Directions = signedAngleBetween2Directions - 360.0_wp
        else if (signedAngleBetween2Directions < -180.0_wp) then
            signedAngleBetween2Directions = signedAngleBetween2Directions + 360.0_wp
        endif
    else
        call set_nan(signedAngleBetween2Directions)
    endif
end function signedAngleBetween2Directions

!> build error message
subroutine setErrorMessage(cnt, subname, errorMessage)
    integer, intent(in) :: cnt
    character(len=*), intent(in) :: subname
    character(len=*), intent(out) :: errorMessage

    character(len=*), parameter :: numbersNL(2) = ['eerste', 'tweede']
    character(len=*), parameter :: numbersUK(2) = ['first ', 'second']
    character(len=2) :: lang

    call GetLanguage(lang)
    if (lang == 'NL') then
        errorMessage = 'Hoek in het ' // numbersNL(cnt) // ' argument van de functie ' // subname //'() is niet in de range [0, 360].'
    else
        errorMessage = 'Direction in the ' // trim(numbersUK(cnt)) // ' argument of function ' // subname //'() is not within [0, 360].'
    endif
end subroutine setErrorMessage

end module angleUtilities
