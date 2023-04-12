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
!!  Facilities for checking whether a given value or parameter is within a prescibed range 
!!  given by a lower bound and an upper bound.
!!
!
! $Id$
!
!>
!! Module for range check to verify whether an input variable x is within a prescribed range [xMinValue, xMaxValue]
!! @ingroup general
module rangeCheck
!
    use  precision, only : wp
    use  genericRealToString, only : genericWriteRealToString
    use  errorMessages, only : GetLanguage

    implicit none
!
    character( len= 1 ):: errSeparationChar= ';'  ! Separation character between multiple error reports stored in one string
!
    interface checkParameterRange
        module procedure checkParameterRange1, checkParameterRange2
    end interface checkParameterRange
    
    private :: checkParameterRange1, checkParameterRange2, wp, genericWriteRealToString, GetLanguage

    contains
!
!> Subroutine checkParameterRange() for checking whether a parameter is within a prescribed range. \n
!! @ingroup tools
   subroutine checkParameterRange1( x, xName, xMinValue, xMaxValue, iErrTotal, totalErrorMessage)
   integer           , intent( inout)     :: iErrTotal                    !< Error status on exit of checkParameterRange()
   real( kind= wp)   , intent( in   )     :: x                            !< Variable for which range must be tested
   real( kind= wp)   , intent( in   )     :: xMinValue                    !< Lower bound of the allowed range that x must satisfy
   real( kind= wp)   , intent( in   )     :: xMaxValue                    !< Upper bound of the allowed range that x must satisfy
   character( len= *), intent( in   )     :: xName                        !< Name or description of the present parameter
   character( len= *), intent( inout)     :: totalErrorMessage            !< Total error report on exit of checkParameterRange()
   integer           , parameter          :: nSignifFigs= 6               !< Number of significant figures to print values in character strings
   character( len= 132)                   :: errorMessage                 !< Locally produced error message
   character( len= 30)                    :: strLowerBound                !< String for storing lower bound of range
   character( len= 30)                    :: strUpperBound                !< String for storing upper bound of range
   character( len= 30)                    :: strXValue                    !< String for storing the input parameter to which range check is applied
   integer                                :: iErr                         !< Error status for the test with the present input value and admissable range
   character(len=2)                       :: language                     !< current language
!
!  Initialisation of the error status and error report:
   iErr        = 0           ! Indicator for occurrence local error
   errorMessage= ' '         ! Report/diagnostic of local error
!  Initialisation of strings with the values of the test value and its range, to be used in error reports
   call genericWriteRealToString( xMinValue, strLowerBound, nSignifFigs)
   call genericWriteRealToString( xMaxValue, strUpperBound, nSignifFigs)
   call genericWriteRealToString( x        , strXValue    , nSignifFigs)
!
!  The actual range check:
   call GetLanguage(language)
   if( xMinValue> xMaxValue) then
       iErr        = 99
       if (language == 'NL') then
           errorMessage= 'Ongeldige range voor parameter ' // trim( adjustl( xName) ) // ': Ondergrens ' // trim( strLowerBound) // ' is groter dan boven grens ' // trim( strUpperBound)
       else
           errorMessage= 'Invalid range for parameter ' // trim( adjustl( xName) ) // ': Lower bound ' // trim( strLowerBound) // ' is greater than upper bound ' // trim( strUpperBound)
       endif
   else
       if( x< xMinValue) then
           iErr =-1
       elseif( x> xMaxValue) then
           iErr = 1
       endif
       if( iErr/= 0 ) then
           if (language == 'NL') then
               errorMessage="Parameter " // trim( adjustl( xName) ) // "= " // trim( StrXValue    ) // " is buiten zijn bereik [" //  &
                                            trim( strLowerBound   ) // ", " // trim( strUpperBound) // "]"
           else
               errorMessage="Parameter " // trim( adjustl( xName) ) // "= " // trim( StrXValue    ) // " is out of it's range [" //  &
                                            trim( strLowerBound   ) // ", " // trim( strUpperBound) // "]"
           endif
       endif
   endif
!
   if( iErr/=0) then
       iErrTotal        = iErrTotal+ 1
       totalErrorMessage= trim( totalErrorMessage) // errSeparationChar // trim( errorMessage)
   endif
!
   end subroutine checkParameterRange1

!> Subroutine checkParameterRange() for checking whether a parameter is within a prescribed range. \n
!! @ingroup tools
   subroutine checkParameterRange2( x, xName, xMinValue, xMaxValue, totalErrorMessages)
   use errorMessages
   real( kind= wp)   , intent( in   )     :: x                            !< Variable for which range must be tested
   real( kind= wp)   , intent( in   )     :: xMinValue                    !< Lower bound of the allowed range that x must satisfy
   real( kind= wp)   , intent( in   )     :: xMaxValue                    !< Upper bound of the allowed range that x must satisfy
   character( len= *), intent( in   )     :: xName                        !< Name or description of the present parameter
   type(TErrorMessages), intent( inout)   :: totalErrorMessages           !< Total error report on exit of checkParameterRange()

   integer                       :: iErr
   character(len=StrLenMessages) :: ErrorMessage
   type (tMessage)               :: msgStruct
!
   ierr = 0
   ErrorMessage = ' '
   call checkParameterRange1( x, xName, xMinValue, xMaxValue, iErr, ErrorMessage)
   if (ierr /= 0) then
       msgStruct%severity = severityError
       msgStruct%errorCode = 1
       msgStruct%message   = ErrorMessage
       call addMessage(totalErrorMessages, msgStruct)
   endif

   end subroutine checkParameterRange2


end module rangeCheck
