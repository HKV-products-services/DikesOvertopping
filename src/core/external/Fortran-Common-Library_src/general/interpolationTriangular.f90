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
!!  Module containing the function for triangular interpolation
!
! $Id$
!
!>
!! Module with function for triangular interpolation
!!   @ingroup general
module interpolationTriangular

    use precision
    use equalReals
    use errorMessages

    implicit none

    private
    public :: tMessage
    public :: triangularInterpolation

contains

!> Function for triangular interpolation
!! Using three result values of three different stations on the Dutch RD grid a plane is spaned.
!! The result values are in general water levels. For a fourth point on the Dutch RD grid
!! the result value is calculated lying on this plane. In general a water level is calculated.
!!   @ingroup general
function triangularInterpolation (x1, y1, h1, x2, y2, h2, x3, y3, h3, x, y, error)

    real(kind=wp),    intent(in)    :: x1                       !< x-coordinate station 1
    real(kind=wp),    intent(in)    :: y1                       !< y-coordinate station 1
    real(kind=wp),    intent(in)    :: h1                       !< Result value station 1 (water level)
    real(kind=wp),    intent(in)    :: x2                       !< x-coordinate station 2
    real(kind=wp),    intent(in)    :: y2                       !< y-coordinate station 2
    real(kind=wp),    intent(in)    :: h2                       !< Result value station 2 (water level)
    real(kind=wp),    intent(in)    :: x3                       !< x-coordinate station 3
    real(kind=wp),    intent(in)    :: y3                       !< y-coordinate station 3
    real(kind=wp),    intent(in)    :: h3                       !< Result value station 3 (water level)
    real(kind=wp),    intent(in)    :: x                        !< x-coordinate point of examination
    real(kind=wp),    intent(in)    :: y                        !< y-coordinate point of examination
    type(tMessage),   intent(out)   :: error                    !< error code/message
    real(kind=wp)                   :: triangularInterpolation  !< Result value (water level) on point of examination
                                                                !! after triangular interpolation

    real(kind=wp)            :: a_1              ! auxiliary parameter
    real(kind=wp)            :: a_2              ! auxiliary parameter
    real(kind=wp)            :: b_1              ! auxiliary parameter
    real(kind=wp)            :: b_2              ! auxiliary parameter

    real(kind=wp), parameter :: margin = 0.01d0  ! margin to compare x- and y-coordinates
    real(kind=wp)            :: a                ! auxiliary parameter
    real(kind=wp)            :: b1               ! auxiliary parameter
    real(kind=wp)            :: b2               ! auxiliary parameter
    real(kind=wp)            :: b3               ! auxiliary parameter

    error = errorNone
    if ( equalRealsAbsolute( x1, x2, margin ) .and. equalRealsAbsolute( y1, y2, margin )) then
        error%errorCode = 1
        error%message   = "triangularInterpolation: two input locations are exactly the same. That's not allowed"
    endif
    if ( equalRealsAbsolute( x1, x3, margin ) .and. equalRealsAbsolute( y1, y3, margin )) then
        error%errorCode = 1
        error%message   =  "triangularInterpolation: two input locations are exactly the same. That's not allowed"
    endif
    if ( equalRealsAbsolute( x2, x3, margin ) .and. equalRealsAbsolute( y2, y3, margin )) then
        error%errorCode = 1
        error%message   = "triangularInterpolation: two input locations are exactly the same. That's not allowed"
    endif
    if ( equalRealsAbsolute( x1, x2, margin ) .and. equalRealsAbsolute( x1, x3, margin )) then
        error%errorCode = 1
        error%message   = "triangularInterpolation: the three input locations are on a line. That's not allowed"
    endif
    if ( equalRealsAbsolute( y1, y2, margin ) .and. equalRealsAbsolute( y1, y3, margin )) then
        error%errorCode = 1
        error%message   = "triangularInterpolation: the three input locations are on a line. That's not allowed"
    endif

!   examine if the three input locations are on a line
    if (error%errorCode == 0) then
        a_1 = (y1 - y2)/(x1 - x2)
        b_1 = y1 - a_1 * x1
        a_2 = (y3 - y2)/(x3 - x2)
        b_2 = y3 - a_1 * x3
        if ( equalRealsAbsolute( a_1, a_2, margin ) .and. equalRealsAbsolute( b_1, b_2, margin )) then
            error%errorCode = 1
            error%message   = "triangularInterpolation: the three input locations are on a line. That's not allowed"
        endif
    endif

!   The general mathematical expression for the plane is:
!   h = c_1 * x + c_2 * y + c_3
!   In which:
!   h          = water level
!   x, y       = coördinates
!   c1, c2, c3 = coëfficiënts
!
!   h_i = c_1 * x_i + c_2 * y_i + c_3,  i = 1, 2, 3
!
!   c_j = b_j / a,  j = 1, 2, 3
!
!   a   = x_1 * (y_2 - y_3) - x_2 * (y_1 - y_3) + x_3 * (y_1 - y_2)
!   b_1 = h_1 * (y_2 - y_3) - h_2 * (y_1 - y_3) + h_3 * (y_1 - y_2)
!   b_2 = x_1 * (h_2 - h_3) - x_2 * (h_1 - h_3) + x_3 * (h_1 - h_2)
!   b_3 = (y_2 * h_3 - y_3 * h_2) * x_1 - (y_1 * h_3 - y_3 * h_ 1) * x_2 + (y_1 * h_2 - y_2 * h_1) * x3

!   compute auxiliary parameters
    if ( error%errorCode == 0) then
        a  = x1 * (y2-y3) - x2 * (y1-y3) + x3 * (y1-y2)

!       because of the above checks a can't be equal to zero. But we keep the check
        if ( equalRealsRelative( a, 0.0d0, margin ) ) then
            error%errorCode = 1
            error%message   = "triangularInterpolation: auxiliary parameter is equal to zero. That's not allowed"
        endif
    endif

    if ( error%errorCode == 0) then
        b1 = h1 * (y2-y3) - h2 * (y1-y3) + h3 * (y1-y2)
        b2 = x1 * (h2-h3) - x2 * (h1-h3) + x3 * (h1-h2)
        b3 = x1 * (y2*h3-y3*h2) - x2 * (y1*h3-y3*h1) + x3 * (y1*h2-y2*h1)

!       compute the result value on the interpolation plane
        triangularInterpolation = (b1/a)*x + (b2/a)*y + b3/a
    else
        call set_nan(triangularInterpolation)
    endif

end function triangularInterpolation

end module interpolationTriangular
