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

!> @file
!! General write utilities
!<
!
! $Id$
module writeUtilities

    use precision

    implicit none

    interface writeToDevice
        module procedure writeToDevice_line
        module procedure writeToDevice_text_real
        module procedure writeToDevice_text_int
    end interface

    private

    public :: writeToDevice, writeToDeviceChars, intToStr

contains

!>
!! Write to device
subroutine writeToDevice_line(fileId, line)
    integer, intent(in)                 :: fileId   !< device number to write to, 0 = screen
    character(len=*), intent(in)        :: line     !< text to write

    if ( fileId == 0 ) then
        write( *, "(a)" ) trim(line)
    else
        write( fileId, "(a)" ) trim(line)
    end if

end subroutine writeToDevice_line

!>
!! Write to device
subroutine writeToDevice_text_real(fileId, formatText, textValue, realValue)
    integer, intent(in)                 :: fileId       !< device number to write to, 0 = screen
    character(len=*), intent(in)        :: formatText   !< format specifier
    character(len=*), intent(in)        :: textValue    !< text to write
    real (kind=wp), intent(in)          :: realValue    !< real value to print

    character(len=120)                  :: line         !< helper text

    write (line, formatText) textValue, realValue
    call writeToDevice_line(fileId, line)

end subroutine writeToDevice_text_real

!>
!! Write to text and integer to device
subroutine writeToDevice_text_int(fileId, textValue, intValue)
    integer,          intent(in)        :: fileId       !< device number to write to, 0 = screen
    character(len=*), intent(in)        :: textValue    !< text to write
    integer,          intent(in)        :: intValue     !< real value to print

    character(len=20)                   :: cnum         !< integer converted to string

    write (cnum, '(I20)') intValue
    cnum = adjustl(cnum)
    call writeToDevice_line(fileId, textValue // cnum)

end subroutine writeToDevice_text_int

!>
!! Write count times the same character to output device
subroutine writeToDeviceChars(fileId, char, count)
    integer, intent(in)                 :: fileId   !< device number to write to, 0 = screen
    character(len=1), intent(in)        :: char     !< character to be printed
    integer, intent(in)                 :: count    !< number of times to output the character

    character(len=count)                :: line     !< text to write

    line = repeat( char, count )
    call writeToDevice(fileId, line)

end subroutine writeToDeviceChars

!>
!! Convert integer to string
function intToStr(intValue)
    integer, intent(in)                 :: intValue     !< integer number to convert
    character(len=5)                    :: intToStr     !< return text
    
    write(intToStr, '(I5)') intValue
    
end function IntToStr

end module writeUtilities
