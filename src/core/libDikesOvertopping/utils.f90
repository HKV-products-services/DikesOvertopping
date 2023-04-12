! Copyright (C) Stichting Deltares and State of the Netherlands 2023. All rights reserved.
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

module utils
    use precision
    private
    public :: isEqualZero

contains

!> isEqualZero:
!! is a real (almost) zero
!***********************************************************************************************************
   logical function isEqualZero (x)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: x  !< real number
!
!  Local parameters
!
   real(kind=wp), parameter   :: margin = 1.0d-14 !< absolute value for the margin
      
! ==========================================================================================================

   isEqualZero = (abs(x) <= margin)

   end function isEqualZero

end module utils
