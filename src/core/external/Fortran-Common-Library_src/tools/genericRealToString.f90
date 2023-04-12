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
!!  Facilities for converting a real to a string
!!
!
! $Id$
!
!>
!! Module for converting a real to a string
!! @ingroup tools
module genericRealToString
    use precision
    implicit none

    private

    public :: genericWriteRealToString

contains
!
!>
!> Subroutine genericWriteRealToString() for writing a real to a character string, and get a fully compiler independent result \n
   subroutine genericWriteRealToString( x, xString, nSignifFigs)
!   
   integer            , intent( in   )     :: nSignifFigs                  !< Number of significant figures to write x to a string
   real( kind= wp)    , intent( in   )     :: x                            !< Real valued variable that must be stored in a string
   character( len=  *), intent( out  )     :: xString                      !< Character string with the value of x
   character( len= 12), parameter          :: wrtFormat='(1Pg26.16E3)'     !< write format of reals to strings
   integer                                 :: indexDot                     !< Location of the decimal point in a with a E-format printed value
   integer                                 :: indexExp                     !< Location of the exponent in a with a E-format printed value
   integer                                 :: mSignifFigs                  !< Copy of nSignifFigs, and possibly adapted
   character( len= 30)                     :: TempStr                      !< Locally used character string for storage and manipulations

   mSignifFigs= min( max( 1, nSignifFigs), 16)    ! Number of significant figures must be between 1 and 16
   write( TempStr, wrtFormat)  x
   indexDot   = index( TempStr, '.' )     + mSignifFigs- 1
   indexExp   = max( index( TempStr, 'e' ), index( TempStr, 'E' ) ) 
   if( indexExp<=0) then
       xString= TempStr( 1: indexDot)
   else
       xString= TempStr( 1: indexDot) // TempStr( indexExp: )
   endif
   xString= adjustl( xString)
!   
   end subroutine genericWriteRealToString
!
end module genericRealToString
