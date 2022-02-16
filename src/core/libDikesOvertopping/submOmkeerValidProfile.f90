! Copyright (C) Stichting Deltares 2022. All rights reserved.
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

submodule (omkeerVariantModule) submOmkeerValidProfile
    use geometryModuleOvertopping
    use typeDefinitionsOvertopping
    use overtoppingInterface
    use ModuleLogging
    use zFunctionsOvertopping
    use parametersOvertopping
    use vectorUtilities, only : logLinearInterpolate
    use mainModuleOvertopping, only : setupGeometries, cleanupGeometry

contains

!>
!! Subroutine with iterateToGivenDischarge, with already checked profile
!!
!! @ingroup dllDikesOvertopping
module procedure OmkeerValidProfile
implicit none
!
    real(kind=wp)                              :: dis1                  ! discharge at minDikeHeight
    real(kind=wp)                              :: dis2                  ! discharge at maxDikeHeight
    real(kind=wp)                              :: minDikeHeight         ! lower bound dike height
    real(kind=wp)                              :: maxDikeHeight         ! upper bound dike height
    integer                                    :: i                     ! loop counter

    integer                                    :: nPoints               ! number of profile points
    integer                                    :: ierr                  ! error code
    integer                                    :: iUp                   ! upper bound on profile
    integer                                    :: iLow                  ! lower bound on profile
    logical                                    :: foundValue            ! flag for early succesfull return
    type(tOmkeer)                              :: omkeerProps           ! struct with derived properties

    foundValue = .false.
    nPoints = geometry%Coordinates%N
    allocate(omkeerProps%isBerm(nPoints-1), omkeerProps%dischargeProfile(nPoints), omkeerProps%isValidZ(nPoints), &
             omkeerProps%ZProfile(nPoints), stat=ierr)
    if (ierr /= 0) then
        error%errorCode = 1
        write(error%message, GetFMTallocateError()) 4*nPoints-1
        return
    endif

    do i = 1, nPoints-1
        omkeerProps%isBerm(i) = geometry%segmentTypes(i) == 2
    enddo
    do i = 1, nPoints
        omkeerProps%isValidZ(i) = .false.
        call set_nan(omkeerProps%ZProfile(i))
        call set_nan(omkeerProps%dischargeProfile(i))
    enddo

    call getDischargeEndSlope()

    iLow = nPoints + 1
    iUp = 0
    do i = 1, nPoints
        if (omkeerProps%isValidZ(i)) then
            if (equalRealsRelative(givenDischarge , omkeerProps%dischargeProfile(i) , tolDischarge )) then
                error%errorCode = 0
                dikeHeight = omkeerProps%ZProfile(i)
                overtopping%Qo = omkeerProps%dischargeProfile(i)
                foundValue = .true.
            else if (givenDischarge < omkeerProps%dischargeProfile(i)) then
                iLow = i
            else if (iUp == 0) then
                iUp = i
            endif
        endif
    enddo

    call checkIfOnBerm()
    if ( error%errorCode /= 0) return

    call DischargeBasedOnLogRelation()

    call cleanupGeometry(geometry%parent, .false.)
    call deallocateGeometry(geometry)

    deallocate(omkeerProps%isBerm, omkeerProps%dischargeProfile, omkeerProps%isValidZ, omkeerProps%ZProfile)

contains

!
! get discharge for end of slope segments:
!
subroutine getDischargeEndSlope()
    integer             :: i, j
    integer, parameter  :: maxItA = 20           ! maximum number of iterations in first loop
    real(kind=wp)       :: nextDikeHeight        ! dike height for next calculation

    call setupGeometries(geometry%parent)
    do i = 1, nPoints
        nextDikeHeight = geometry%Coordinates%y(i)
        omkeerProps%isValidZ(i) = nextDikeHeight >= load%H
        if (i > 1) then
            omkeerProps%isValidZ(i) = omkeerProps%isValidZ(i) .and. .not. omkeerProps%isBerm(i-1)
        endif
        if (omkeerProps%isValidZ(i)) then
            do j = 1, MaxItA
                call calculateQoHPC(nextDikeHeight, modelFactors, overtopping, load, geometry%parent, error)
                if (error%errorCode /= 0) return ! only in very exceptional cases
                omkeerProps%ZProfile(i) = nextDikeHeight
                omkeerProps%dischargeProfile(i) = overtopping%Qo
                if (i == 1 .or. overtopping%Qo > 0.0_wp) exit
                nextDikeHeight = 0.5_wp * ( max(geometry%Coordinates%y(i-1) + xDiff_min * geometry%segmentSlopes(i-1), load%H) + nextDikeHeight)
            enddo
            if (overtopping%Qo < givenDischarge ) then
                exit
            endif
        endif
    enddo
end subroutine getDischargeEndSlope

subroutine DischargeBasedOnLogRelation()
    real(kind=wp)       :: disNext               ! discharge at nextDikeHeight
    real(kind=wp)       :: X(2)                  ! argument for logInterpolate
    real(kind=wp)       :: Y(2)                  ! argument for logInterpolate
    integer             :: i
    real(kind=wp)       :: nextDikeHeight        ! dike height for next calculation
    integer, parameter  :: maxItB = 5            ! maximum number of iterations in second loop
    !
    ! use logarithmic relation between discharge and dike height
    !
    if (.not. foundValue) then
        do i = 1, maxItB
            X = (/ dis1 , dis2 /)
            Y = (/ minDikeHeight , maxDikeHeight /)
            nextDikeHeight = LogLinearInterpolate(X , Y , givenDischarge, error)
            if (error%errorCode /= 0) then
                !
                ! if loglinear fails, fall back on mean
                !
                nextDikeHeight = 0.5_wp * (minDikeHeight + maxDikeHeight)
            endif
            call calculateQoHPC(nextDikeHeight, modelFactors, overtopping, load, geometry%parent, error)

            if (error%errorCode /= 0) return  ! only in very exceptional cases

            disNext = overtopping%Qo

            !
            ! check convergence
            !
            if (abs(disNext - givenDischarge ) < givenDischarge * tolDischarge) exit
            if (abs(maxDikeHeight - minDikeHeight ) < tolDikeHeight) exit

            !
            ! change upper or lower bound to be used in next iteration
            !
            if (disNext > givenDischarge) then
                minDikeHeight = nextDikeHeight
                dis1 = disNext
            else
                maxDikeHeight = nextDikeHeight
                dis2 = disNext
            endif
        enddo
        if (error%errorCode == 0) then
            dikeHeight = nextDikeHeight
        else
            call set_nan(dikeHeight)
        endif
    endif
end subroutine DischargeBasedOnLogRelation

subroutine checkIfOnBerm()
    !
    ! locals
    !
    integer                  :: i               ! loop counter
    integer                  :: nPoints         ! number of profile points
    real(kind=wp)            :: nextDikeheight  ! dike height for next calculation
    real(kind=wp)            :: X(2)            ! argument for logInterpolate
    real(kind=wp)            :: Y(2)            ! argument for logInterpolate
    !
    ! check: possibly on berm
    !
    nPoints = geometry%Coordinates%N
    if (foundValue ) then
        continue
    elseif (iUp > iLow + 1 ) then
        minDikeHeight = omkeerProps%ZProfile(iLow)
        dis1 = omkeerProps%dischargeProfile(iLow)
        maxDikeHeight = omkeerProps%ZProfile(iUp)
        dis2 = omkeerProps%dischargeProfile(iUp)
        call BermCheckLoopLowUp()
    else if (iUp == iLow + 1 ) then
        minDikeHeight = omkeerProps%ZProfile(iLow)
        dis1 = omkeerProps%dischargeProfile(iLow)
        maxDikeHeight = omkeerProps%ZProfile(iUp)
        dis2 = omkeerProps%dischargeProfile(iUp)
    else if (iLow == nPoints + 1 ) then
        minDikeHeight = load%h
        call calculateQoHPC(minDikeHeight, modelFactors, overtopping, load, geometry%parent, error)
        if (error%errorCode /= 0) return ! only in very exceptional cases
        dis1 = overtopping%Qo
        if (dis1 < givenDischarge) then
            dikeHeight = minDikeHeight
            foundValue = .true.
            return
        endif
        if (iUp /= 0) then
            if (omkeerProps%dischargeProfile(iUp) > 0.0_wp) then
                maxDikeHeight = omkeerProps%ZProfile(iUp)
                dis2 = omkeerProps%dischargeProfile(iUp)
            else
                dikeHeight = load%h
                overtopping%z2 = 0d0
                overtopping%Qo = 0d0
                foundValue = .true.
                return
            endif
        else
            maxDikeHeight = max(minDikeHeight, load%h + load%Hm0, geometry%Coordinates%y(nPoints)) + 1.0_wp
            call calculateQoHPC(maxDikeHeight, modelFactors, overtopping, load, geometry%parent, error)
            if (error%errorCode /= 0) return ! only in very exceptional cases
            dis2 = overtopping%Qo
        endif
    else
        minDikeHeight = omkeerProps%ZProfile(iLow)
        dis1 = omkeerProps%dischargeProfile(iLow)
        maxDikeHeight = max(minDikeHeight, load%h + load%Hm0, geometry%Coordinates%y(nPoints)) + 1.0_wp
        do
            call calculateQoHPC(maxDikeHeight, modelFactors, overtopping, load, geometry%parent, error)
            dis2 = overtopping%Qo

            if (error%errorCode /= 0) return ! only in very exceptional cases

            if (abs(dis2 - givenDischarge ) < givenDischarge * tolDischarge) then
                dikeHeight = maxDikeHeight
                foundValue = .true.
                exit
            else if (dis2 > givenDischarge) then
                X = (/ dis1 , dis2 /)
                Y = (/ minDikeHeight , maxDikeHeight /)
                minDikeHeight = maxDikeHeight
                dis1 = dis2
                maxDikeHeight = LogLinearInterpolate(X , Y , givenDischarge, error)
            else
                exit
            endif
        enddo
    endif

end subroutine checkIfOnBerm

subroutine BermCheckLoopLowUp()
    integer :: i
    real(kind=wp)            :: nextDikeheight  ! dike height for next calculation
    real(kind=wp)            :: X(2)            ! argument for logInterpolate
    real(kind=wp)            :: Y(2)            ! argument for logInterpolate
    real(kind=wp), parameter :: eps = 1e-4_wp   ! epsilon for waterlevel near berm

    do i = iLow + 1, iUp - 1
        if (.not. omkeerProps%isBerm(i) ) then
            nextDikeHeight = geometry%Coordinates%y(i) + xDiff_min * geometry%segmentSlopes(i)
            call calculateQoHPC(nextDikeHeight, modelFactors, overtopping, load, geometry%parent, error )
            if (error%errorCode /= 0) return ! only in very exceptional cases
            omkeerProps%ZProfile(i) = nextDikeHeight
            omkeerProps%dischargeProfile(i) = overtopping%Qo
            omkeerProps%isValidZ(i) = .true.
            if (omkeerProps%dischargeProfile(i) < givenDischarge) then
                if (omkeerProps%ZProfile(i) - omkeerProps%ZProfile(iLow) > minZberm ) then
                    X = (/ omkeerProps%dischargeProfile(iLow), omkeerProps%dischargeProfile(i) /)
                    Y = (/ omkeerProps%ZProfile(iLow), omkeerProps%ZProfile(i) /)
                else
                    nextDikeHeight = max(load%H + eps, omkeerProps%ZProfile(i) - minZberm)
                    call calculateQoHPC(nextDikeHeight, modelFactors, overtopping, load, geometry%parent, error)
                    if (error%errorCode /= 0) return ! only in very exceptional cases
                    X = (/ overtopping%Qo, omkeerProps%dischargeProfile(i) /)
                    Y = (/ nextDikeHeight, omkeerProps%ZProfile(i) /)
                endif
                X = max(X, tiny(X))
                dikeHeight = LogLinearInterpolate(X, Y, givenDischarge, error)
                overtopping%Qo = givenDischarge
                foundValue = .true.
            else
                minDikeHeight = omkeerProps%ZProfile(i)
                dis1 = omkeerProps%dischargeProfile(i)
            endif
        endif
    enddo
end subroutine BermCheckLoopLowUp

end procedure OmkeerValidProfile

end submodule submOmkeerValidProfile
