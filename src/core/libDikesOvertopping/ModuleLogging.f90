!> @file
!! Module for steering the extra logging
!<
!
! Copyright (c) 2015, Deltares, HKV lijn in water, TNO
!
! $Id$
!
module ModuleLogging
use feedback_parameters, only : verboseNone, verboseBasic, verboseDetailed, verboseDebugging
implicit none

integer, parameter :: maxFilenameLength = 256      !< maximum length of filename

!> TLogging: structure for steering the logging
type, bind(C) :: TLogging
   integer                          :: verbosity =  verboseNone  !< level of verbosity: one of verboseNone, verboseBasic, verboseDetailed, verboseDebugging
   character(len=maxFilenameLength) :: fileName  =  ' '          !< filename of logging
end type TLogging

type(TLogging) :: currentLogging                                 !< copy of argument logging

end module ModuleLogging