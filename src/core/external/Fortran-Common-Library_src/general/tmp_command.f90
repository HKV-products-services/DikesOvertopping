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
    
! tmp_command.f90 --
!     Temporary solution to the problem of EXECUTE_COMMAND_LINE not being
!     supported by Intel Fortran 2013 (it was introduced in Fortran 2003 and is
!     supported by Intel Fortran 2015 and later):
!
!     As it is an _intrinsic_ routine, we can use the feature that such routines
!     exclude user-defined routines, unless these are explicitly defined as
!     external. But it should NOT be in a module for this to work.
!
subroutine execute_command_line( string, exitstatus )
    use ifport

    character(len=*), intent(in) :: string
    integer, intent(out)         :: exitstatus

    exitstatus = system( string )
end subroutine execute_command_line
