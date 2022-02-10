submodule (geometryModuleOvertopping) submBasicGeometryValidation
   use errorMessages, only : severityError, addMessage
   use OvertoppingMessages
   use parametersOvertopping
contains

!> basicGeometryValidation:
!! validate the input geometry (the adjusted geometry is checked elsewhere)
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure basicGeometryValidation
!***********************************************************************************************************
   implicit none
!
!  Local parameters
!
   integer                   :: i                   !< loop counter
   real(kind=wp)             :: diffx               !< difference in two consecutive x coordinates
   real(kind=wp)             :: diffy               !< difference in two consecutive y coordinates
   real(kind=wp), parameter  :: tol = 1d-6          !< tolerance for comparing reals
   type(tMessage)            :: message             !< local error message
   real(kind=wp)             :: xi                  !< current x coordinate
   real(kind=wp)             :: xnext               !< next x coordinate
   real(kind=wp)             :: yi                  !< current y coordinate
   real(kind=wp)             :: ynext               !< next y coordinate
   real(kind=wp)             :: roughnessFactor     !< current roughness factor
! ==========================================================================================================
   success = .true.
   do i = 1, geometryF%nPoints - 1
       xi              = geometryF%xCoords(i)
       yi              = geometryF%yCoords(i)
       xnext           = geometryF%xCoords(i+1)
       ynext           = geometryF%yCoords(i+1)
       roughnessFactor = geometryF%roughness(i)
       diffx           = xnext - xi
       diffy           = ynext - yi
       if (diffx < 0) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           write(message%message, GetFMTdiffx_negative()) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       else if (diffx < xDiff_min - tol) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           write(message%message, GetFMTdiffx_too_small()) xDiff_min, xi, xnext
           call addMessage(errorStruct, message)
       endif
       if (diffy < 0d0) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           write(message%message, GetFMTdiffy_too_small()) yi, ynext
           call addMessage(errorStruct, message)
       endif
       if (roughnessFactor < rFactor_min .or. roughnessFactor > rFactor_max) then
           success = .false.
           message%errorcode = -1
           message%severity = severityError
           write (message%message, GetFMTroughnessfactors_out_of_range()) &
               rFactor_min, rFactor_max, roughnessFactor
           call addMessage(errorStruct, message)
       endif
   enddo
end procedure basicGeometryValidation

end submodule submBasicGeometryValidation
