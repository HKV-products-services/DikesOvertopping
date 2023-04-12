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
!! (Re-)allocation of arrays
!<
!
! $Id$
!
! based on: https://svn.oss.deltares.nl/repos/delft3d/trunk/src/utils_lgpl/deltares_common/packages/deltares_common/src/malloc.f90 
!
!>
!! Module with subroutine realloc
!!     @ingroup General

module m_realloc
implicit none

private

public :: realloc

interface realloc
   module procedure reallocInt
   module procedure reallocDouble
end interface realloc

contains

subroutine reallocInt(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   integer, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   integer, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   integer, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocInt

!
!===============================================================================
subroutine reallocDouble(arr, uindex, lindex, stat, fill, shift, keepExisting)
   implicit none
   double precision, allocatable, intent(inout)             :: arr(:)
   integer, intent(in)                          :: uindex
   integer, intent(in), optional                :: lindex
   integer, intent(out), optional               :: stat
   double precision, intent(in), optional                   :: fill
   integer, intent(in), optional                :: shift
   logical, intent(in), optional                :: keepExisting

   double precision, allocatable                            :: b(:)
   integer        :: uind, lind, muind, mlind, lindex_, shift_

   integer        :: localErr
   logical        :: docopy
   logical        :: equalSize

   if (present(lindex)) then
      lindex_ = lindex
   else
      lindex_ = 1
   endif

   if (present(shift)) then
      shift_ = shift
   else
      shift_ = 0
   endif

   if (present(keepExisting)) then
      docopy = keepExisting
   else
      docopy = .true.
   end if

   if (present(stat)) stat = 0
   localErr = 0
   if (allocated(arr)) then
      uind = ubound(arr,1)
      lind = lbound(arr,1)
      equalSize = (uindex == uind) .and. (lindex_ == lind)
      if (equalSize .and. (docopy .or. .not. present(fill)) .and. shift_==0) then
         goto 999 ! output=input
      end if
!
      if (docopy) then
         mlind = max(lind + shift_, lindex_)
         muind = min(uind + shift_, uindex)
         allocate (b(mlind:muind))
        b(mlind:muind) = arr(mlind-shift_:muind-shift_)
      endif
      if (.not.equalSize) deallocate(arr, stat = localErr)
   endif
   if (.not.allocated(arr) .and. localErr==0) then
       allocate(arr(lindex_:uindex), stat = localErr)
   endif
   if (present(fill) .and. localErr==0) arr = fill
   if (allocated(b) .and. localErr==0 .and. size(b)>0) then
      arr(mlind:muind) = b(mlind:muind)
      deallocate(b, stat = localErr)
   endif
999 continue
   if (present(stat)) stat = localErr
end subroutine reallocDouble
!

end module m_realloc
