! Copyright (C) Stichting Deltares 2019. All rights reserved.
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
!! Module for retrieving file version info for a .dll or .exe 
!<
!
! $Id: versionInfo.f90 9219 2019-02-05 08:01:26Z spee $
!
!!   @ingroup General
module versionInfo

#if defined _WIN64
    use kernel32
#endif
    use iso_c_binding
    use precision
    implicit none

    ! Version info structure
    type :: tpVersionStruct
        integer                         :: versionMajor               !< Major version number
        integer                         :: versionMinor               !< Minor version number
        integer                         :: versionRelease             !< Release number 
        integer                         :: versionBuild               !< Build number
    end type
    
    private
    
public :: tpVersionStruct, getFileVersion, getFileNameModule

contains

!> Get pointer to dll
!!
!! @ingroup General
function p()
integer(kind=pntlen)       :: p
integer(kind=pntlen), save :: p2dll = 0

    if (p2dll == 0) then
#if defined _WIN64
        p2dll = loadlibrary    ("winapi.dll" // c_null_char)
#endif
    endif
    p = p2dll
end function

!> Get file version info
!!
!! @ingroup General
function getFileVersion(filename, versionStruct)
    character(len=*), intent(in)        :: filename         !< Name of the file to get info from
    character(len=20)                   :: getFileVersion   !< The resulting file version
    type(tpVersionStruct), intent(out)  :: versionStruct    !< Will hold the version info
    integer                             :: returnCode       !< Return code of the call
    
    integer, external                   :: dllGetFileVersion
    pointer (q, dllGetFileVersion)

#if defined _WIN64
    q = getprocaddress (p(), "GetFileVersion" // c_null_char)
#endif
    returnCode = -1
    if (q /= 0) then
        returnCode = dllGetFileVersion(filename, versionStruct)
    endif
    if (returnCode /= 0) then
        versionStruct%versionMajor = 0
        versionStruct%versionMinor = 0 
        versionStruct%versionRelease = 0
        versionStruct%versionBuild = 0
    endif
    
    write(getFileVersion, '(i0,".",i0,".",i0,".",i0)') versionStruct%versionMajor, versionStruct%versionMinor, versionStruct%versionRelease, versionStruct%versionBuild
    
end function getFileVersion

!> Get file filename of module
!  if moduleHandle is <> 0 then the filename of the calling executable is returned
!!
!! @ingroup General
function getFileNameModule(moduleHandle, filename)
    integer, intent(in)                 :: moduleHandle         !< Handle of the module
    character(len=*), intent(out)       :: filename             !< Name of the file to get info from
    integer                             :: getFileNameModule    !< The length of the filename
    
    integer, external                   :: dllGetFileNameModule
    pointer (q, dllGetFileNameModule)

#if defined _WIN64
    q = getprocaddress (p(), "GetFileNameModule" // c_null_char)
#endif
    if (q /= 0) then
        getFileNameModule = dllGetFileNameModule(moduleHandle, filename)
    else
        getFileNameModule = 0
    endif

    
end function getFileNameModule

end module versionInfo

