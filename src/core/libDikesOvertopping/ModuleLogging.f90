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

!> @file
!! Module for steering the extra logging
!<
!
! $Id$
!
!> steering the extra logging
!
module ModuleLogging
use feedback_parameters, only : verboseNone, verboseBasic, verboseDetailed, verboseDebugging
implicit none

integer, parameter :: maxFilenameLength = 256      !< maximum length of filename

!> TLogging: structure for steering the logging
type :: TLogging
   integer                          :: verbosity =  verboseBasic  !< level of verbosity: one of verboseNone, verboseBasic, verboseDetailed, verboseDebugging
   character(len=maxFilenameLength) :: fileName  =  ' '          !< filename of logging
end type TLogging

end module ModuleLogging
