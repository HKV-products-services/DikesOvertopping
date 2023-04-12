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
!!  Define KINDs for controlling the precision of the REAL variables
!!  and define parameters for mathematical constants.
!!
!!  <b>Note:</b> Use the parameter "wp" to set the working precision for using
!!  routines.
!<
!
! $Id$
!
!>
!! Module with parameters for KINDs and mathematical constants
!!   @ingroup general
module precision
    use iso_c_binding

    implicit none

    integer, parameter :: sp = selected_real_kind(6, 37)     !< Single precision
    integer, parameter :: dp = selected_real_kind(15, 307)   !< Double precision
    integer, parameter :: qd = selected_real_kind(33, 4931)  !< Quadruple precision

    ! Never change the working precision to single precision!!!
    ! The programmed construction is just for cases of higher precision than doubles
    !
    integer, parameter :: wp = dp                !< Working precision
    !
    ! Numerical constants
    !
    real(kind=wp), parameter :: almostzero = 1.0d-30           !< Definition of zero for the machine
    real(kind=wp), parameter :: almostInf  = huge(1.0d0)       !< Very large value as limit
    real(kind=wp), parameter :: rhoLimit   = 0.99999d0         !< Limit value for the correlation coefficient DO NOT CHANGE THIS VALUE WITHOUT SUFFICIENT RESEARCH
    real(kind=wp), parameter :: betaLimit  = 0.99999999d0      !< Limit value for the beta ratio in combining DO NOT CHANGE ONLY USED FOR EXTRA WIND STOCHAST

    !
    ! Mathematical constants
    !
    real(kind=wp), parameter :: pi = 3.141592653589793238462643383279502884197_wp                     !< Circle constant
    real(kind=wp), parameter :: EMconstant = 0.57721566490153286060651209008240243104215933593992_wp  !< Euler Mascheroni constant
    real(kind=wp), parameter :: gravityConstant = 9.80665_wp                                          !< Gravity constant for the Netherlands

    !
    ! Parameters for defining character string lengths
    !
    integer, parameter :: shortMax =  32
    integer, parameter :: longMax  = 255

    !
    ! Parameter for switching between 32 and 64 bits pointers
    !
    integer, parameter :: pntlen = c_intptr_t

    !> set_nan
    !! set number to NaN
    interface set_nan
        module procedure :: set_nan4, set_nan8, set_nan16
    end interface set_nan

    private :: set_nan4, set_nan8, set_nan16

contains

    !> set_nan
    !! set number to NaN, single precision version
    subroutine set_nan4(value)
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    real(kind=sp), intent(out) :: value   !< real to be set to NaN

    value = ieee_value(value, ieee_quiet_nan)

    end subroutine set_nan4

    !> set_nan
    !! set number to NaN, double precision version
    subroutine set_nan8(value)
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    real(kind=dp), intent(out) :: value   !< double to be set to NaN

    value = ieee_value(value, ieee_quiet_nan)

    end subroutine set_nan8

    !> set_nan
    !! set number to NaN, quad precision version
    subroutine set_nan16(value)
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    real(kind=qd), intent(out) :: value   !< quad to be set to NaN

    value = ieee_value(value, ieee_quiet_nan)

    end subroutine set_nan16

end module precision
