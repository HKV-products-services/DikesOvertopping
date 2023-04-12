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

!> @file MatrixFunctions.f90
!!  Implement various utility functions for matrices
!
! $Id$
!
!>
!! Module with matrix utility functions
!!   @ingroup general
module matrixFunctions
use precision
use errorMessages
implicit none
private
public :: CholeskyDecomposition
public :: tMessage
contains

!> Cholesky decomposition
subroutine CholeskyDecomposition(matrix, cholesky, error)
    real(kind=wp),     intent(in)     :: matrix(:,:)          !< the input matrix
    real(kind=wp),     intent(out)    :: cholesky(:,:)        !< output: the cholesky matrix
    type(tMessage),    intent(out)    :: error                !< error code/message

    integer       :: nStochasts                !< Number of stochastic variables
    real(kind=wp) :: sumrow, sumcol, arg_sqrt
    integer       :: i, j, k

    error     = errorNone
    cholesky  = 0.0_wp

    nStochasts = size(matrix, 1)
    if (size(matrix,2)/=nStochasts) then
        error%errorCode = -1
        error%message   = 'Cholesky decomposition fails: input matrix not square'
        return
    endif
    if (any(shape(cholesky)/=(/nStochasts,nStochasts/))) then
        error%errorCode = -1
        error%message   = 'Cholesky decomposition fails: output matrix has the wrong shape'
        return
    endif

    do j = 1,nStochasts
        sumrow = 0.0_wp
        do k = 1,j-1
            sumrow = sumrow + cholesky(j,k) ** 2
        enddo
        arg_sqrt = matrix(j,j) - sumrow
        if (arg_sqrt >= 0.0_wp) then
            cholesky(j,j) = sqrt( arg_sqrt )
        else
            error%errorCode = -1
            error%message   = 'Cholesky decomposition fails: negative argument sqrt'
            return
        endif

        do i = j+1,nStochasts
            sumcol = 0.0_wp
            if (cholesky(j,j) == 0.0_wp) then
                error%errorCode = -1
                error%message   = 'Cholesky decomposition fails: division by zero'
                return
            endif
            do k = 1,j-1
                sumcol = sumcol + cholesky(i,k) * cholesky(j,k)
            enddo
            cholesky(i,j) = (matrix(i,j) - sumcol) / cholesky(j,j)
        enddo
    enddo
end subroutine CholeskyDecomposition

end module matrixFunctions
