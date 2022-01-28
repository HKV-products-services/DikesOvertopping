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

!> @file
!! This file contains the messages in the overtopping dll, in Dutch or English
!<
!
! $Id$
!
!>
!! Module for the messages in the overtopping dll, in Dutch or English
!! @ingroup LibOvertopping
!<
module OvertoppingMessages
implicit none

integer, parameter :: maxmsg = 128, maxpar=32
integer, parameter :: LanguageNL = 2
integer, parameter :: LanguageUK = 1
integer :: language = LanguageNL   !< default : Dutch

private :: maxmsg, maxpar, language

!> IDs for the strings in this module:
enum, bind(c)
! messages:
    enumerator :: errorIndicator
    enumerator :: warningIndicator
    enumerator :: validation_only_for_type_runup1
    enumerator :: adjusted_xcoordinates
    enumerator :: slope_negative
    enumerator :: split_cross_section_seq_berm
    enumerator :: adjust_non_horizontal_seq_berm
    enumerator :: merging_seq_berm
    enumerator :: calc_horizontal_lengths
    enumerator :: calc_horizontal_distance
    enumerator :: remove_dike_segments_index
    enumerator :: calc_representative_slope_angle
    enumerator :: calc_influence_berms
    enumerator :: calc_influence_factors
    enumerator :: calc_wave_overtopping_discharge
    enumerator :: breaker_param_is_zero
    enumerator :: calc_wave_steepness_period_is_zero
    enumerator :: calc_breaker_param_steepness_is_zero
    enumerator :: calc_roots_cubic_function
    enumerator :: psi_not_in_range
    enumerator :: dimension_cross_section_less_than_2
    enumerator :: ycoordinates_must_be_nondecreasing
    enumerator :: dike_segment_mismatches
    enumerator :: max2berm_segments
    enumerator :: first_and_last_must_be_slope
    enumerator :: wl_above_crest_not_allowed
    enumerator :: interpolation_error_split_cross_sections
    enumerator :: wl_above_crest
    enumerator :: wave_height_or_periode_less_zero
    enumerator :: wave_direction_not_in_range
end enum
! parameters :
    integer, parameter :: par_fB = 1
    integer, parameter :: par_fN = 2
    integer, parameter :: par_fS = 3
    integer, parameter :: par_2percent_wave_runup  = 4
    integer, parameter :: reductionFactorForeshore = 5

contains

!>
!! Subroutine that sets the language for error and validation messages
!!    only strings 'NL' and 'UK' are recoqnized (lower and upper case)
!!
!! @ingroup LibOvertopping
subroutine SetLanguage(lang)
use feedback
use utilities, only : to_upper
character(len=*), intent(in) :: lang   !< new language ID to be used

character(len=len(lang)) :: langUpper

langUpper = to_upper(lang)

select case (langUpper)
    case ('NL')
        language = languageNL
    case ('UK')
        language = languageUK
    case default
        call fatalError("Language " // lang // " not supported")
end select
end subroutine SetLanguage

!>
!! Subroutine that gets the language for error and validation messages
!!
!! @ingroup LibOvertopping
subroutine GetLanguage(lang)
character(len=*), intent(out) :: lang   !< filled with current language ID

lang = merge( "NL", "UK", language == languageNL)
end subroutine GetLanguage

!>
!! Subroutine that returns a message with the corresponding ID in the current language
!!
!! @ingroup LibOvertopping
character(len=maxmsg) function GetOvertoppingMessage(ID)
integer, intent(in) :: ID  !< identification number of string

select case(language)
    case(languageUK)
        select case (ID)
            case (errorIndicator)
                GetOvertoppingMessage = 'ERROR'
            case (warningIndicator)
                GetOvertoppingMessage = 'WARNING'
            case (validation_only_for_type_runup1)
                GetOvertoppingMessage = 'Validation only implemented for typeRunup=1'
            case (adjusted_xcoordinates)
                GetOvertoppingMessage = 'Error in calculation of adjusted x-coordinates'
            case (slope_negative)
                GetOvertoppingMessage = 'Error in calculating slope (dx <= 0)'
            case (split_cross_section_seq_berm)
                GetOvertoppingMessage = 'Error in splitting cross section: sequential berms'
            case (adjust_non_horizontal_seq_berm)
                GetOvertoppingMessage = 'Error adjusting non-horizontal berms: sequential berms'
            case (merging_seq_berm)
                GetOvertoppingMessage = 'Error in merging sequential berms (B=0)'
            case (calc_horizontal_lengths)
                GetOvertoppingMessage = 'Error in calculation horizontal lengths'
            case (calc_horizontal_distance)
                GetOvertoppingMessage = 'Error in calculation horizontal distance'
            case (remove_dike_segments_index)
                GetOvertoppingMessage = 'Error removing dike segments: incorrect index'
            case (calc_representative_slope_angle)
                GetOvertoppingMessage = 'Error in calculation representative slope angle'
            case (calc_influence_berms)
                GetOvertoppingMessage = 'Error in calculation influence berms'
            case (calc_influence_factors)
                GetOvertoppingMessage = 'Error in adjustment of influence factors'
            case (calc_wave_overtopping_discharge)
                GetOvertoppingMessage = 'Error calculating wave overtopping discharge'
            case (breaker_param_is_zero)
                GetOvertoppingMessage = 'Error calculating 2% wave run-up: breaker parameter equals zero'
            case (calc_wave_steepness_period_is_zero)
                GetOvertoppingMessage = 'Error calculating wave steepness: wave period equals zero'
            case (calc_breaker_param_steepness_is_zero)
                GetOvertoppingMessage = 'Error calculating breaker parameter: wave steepness equals zero'
            case (calc_roots_cubic_function)
                GetOvertoppingMessage = 'Error calculating roots general cubic function'
            case (psi_not_in_range)
                GetOvertoppingMessage = 'Dike normal (psi) not between 0 and 360 degree'
            case (dimension_cross_section_less_than_2)
                GetOvertoppingMessage = 'Number of coordinates cross section less than 2'
            case (ycoordinates_must_be_nondecreasing)
                GetOvertoppingMessage = 'Coordinates in the vertical direction must be non-decreasing'
            case (dike_segment_mismatches)
                GetOvertoppingMessage = 'Dike segment mismatches berm segment or slope segment'
            case (max2berm_segments)
                GetOvertoppingMessage = 'A maximum of two berm segments is allowed'
            case (first_and_last_must_be_slope)
                GetOvertoppingMessage = 'First and last dike segment must be a slope segment'
            case (wl_above_crest_not_allowed)
                GetOvertoppingMessage = 'In the overtopping freeboard routine is a local water level below crest not allowed.'
            case (interpolation_error_split_cross_sections)
                GetOvertoppingMessage = 'Error in interpolation between results for split cross sections'
            case (wl_above_crest)
                GetOvertoppingMessage = 'local water level above the crest level'
            case (wave_height_or_periode_less_zero)
                GetOvertoppingMessage = 'Wave height and/or wave period less than zero'
            case (wave_direction_not_in_range)
                GetOvertoppingMessage = 'Wave direction not between 0 and 360 degree'
            case default
                write(GetOvertoppingMessage,*) 'Internal error, ID = ', ID
        end select
    case default
        select case (ID)
            case (errorIndicator)
                GetOvertoppingMessage = 'FOUT'
            case (warningIndicator)
                GetOvertoppingMessage = 'WAARSCHUWING'
            case (validation_only_for_type_runup1)
                GetOvertoppingMessage = 'Validatie alleen geimplementeerd voor typeRunup=1'
            case (adjusted_xcoordinates)
                GetOvertoppingMessage = 'Fout in bepaling van gecorrigeerde x-coordinaten'
            case (slope_negative)
                GetOvertoppingMessage = 'Fout in berekening helling (dx <= 0)'
            case (split_cross_section_seq_berm)
                GetOvertoppingMessage = 'Fout in opsplitsen doorsnede: sequentiele bermen'  ! check
            case (adjust_non_horizontal_seq_berm)
                GetOvertoppingMessage = 'Fout bij aanpassing niet-horizontale bermen: sequentiele bermen' ! check
            case (merging_seq_berm)
                GetOvertoppingMessage = 'Fout in samenvoegen sequentiele bermen (B=0)' ! check
            case (calc_horizontal_lengths)
                GetOvertoppingMessage = 'Fout in berekening horizontale lengtes'
            case (calc_horizontal_distance)
                GetOvertoppingMessage = 'Error in berekening horizontale afstanden'
            case (remove_dike_segments_index)
                GetOvertoppingMessage = 'Fout bij verwijderen dijk secties: foute index'
            case (calc_representative_slope_angle)
                GetOvertoppingMessage = 'Fout in berekening representatieve hellingshoek'
            case (calc_influence_berms)
                GetOvertoppingMessage = 'Fout in berekening invloed bermen'
            case (calc_influence_factors)
                GetOvertoppingMessage = 'Error in berekening van invloedsfactoren'
            case (calc_wave_overtopping_discharge)
                GetOvertoppingMessage = 'Fout in berekening golf overslag debiet'
            case (breaker_param_is_zero)
                GetOvertoppingMessage = 'Fout in berekening 2% golf oploop: brekerparameter is nul'
            case (calc_wave_steepness_period_is_zero)
                GetOvertoppingMessage = 'Fout in berekening golf steilte: golf periode is nul'
            case (calc_breaker_param_steepness_is_zero)
                GetOvertoppingMessage = 'Fout in berekening brekerparameter: golf steilte is nul'
            case (calc_roots_cubic_function)
                GetOvertoppingMessage = 'Fout in berekening van wortel 3e graads functie'
            case (psi_not_in_range)
                GetOvertoppingMessage = 'Dijk normaal (psi) ligt niet tussen 0 en 360 graden'
            case (dimension_cross_section_less_than_2)
                GetOvertoppingMessage = 'Aantal coordinaten dijk doorsnede is kleiner dan 2'
            case (ycoordinates_must_be_nondecreasing)
                GetOvertoppingMessage = 'Verticale coordinaten mogen niet afnemen'
            case (dike_segment_mismatches)
                GetOvertoppingMessage = 'Dijk segment is van ander type dan berm segment of helling segment'
            case (max2berm_segments)
                GetOvertoppingMessage = 'Maximum van twee berm segmenten overschreden'
            case (first_and_last_must_be_slope)
                GetOvertoppingMessage = 'Eerste en laatste dijk segment moeten een helling segment zijn'
            case (wl_above_crest_not_allowed)
                GetOvertoppingMessage = 'In the overtopping module is een lokale waterstand boven de kruin niet toegestaan.'
            case (interpolation_error_split_cross_sections)
                GetOvertoppingMessage = 'Fout in interpolatie tussen resultaten for split cross sections'
            case (wl_above_crest)
                GetOvertoppingMessage = 'Lokale waterstand boven de kruin'
            case (wave_height_or_periode_less_zero)
                GetOvertoppingMessage = 'Golf hoogte en/of golf periode kleiner dan nul'
            case (wave_direction_not_in_range)
                GetOvertoppingMessage = 'Golf hoek niet tussen 0 and 360 graden'
            case default
                write(GetOvertoppingMessage,*) 'Interne fout, ID = ', ID
        end select
    end select

end function GetOvertoppingMessage

!>
!! Subroutine that returns the name of an input parameter with the corresponding ID in the current language
!!
!! @ingroup LibOvertopping
character(len=maxpar) function GetOvertoppingParameter(ID)
integer, intent(in) :: ID  !< identification number of string
character(len=*), dimension(10), parameter :: string_msg = [&
        'fB (breaking waves)       ', &
        'fN (non-breaking waves)   ', &
        'fS (shallow waves)        ', &
        '2% wave runup             ', &
        'reduction factor foreshore', &
        'fB (brekende golven)      ', &
         'fN (niet brekende golven)', &
        'fS (ondiepe golven)       ', &
        '2% golf oploop            ', &
        'reductie factor vooroever ']

        GetOvertoppingParameter = string_msg(ID + 5*(language - 1))
end function GetOvertoppingParameter

! new implementation: one subroutine for each message (avoids internal error)

subroutine GetMSG_calculateGammaFtoLow(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'calculateGammaF: local water level lower than dike toe', &
        'calculateGammaF: waterstand lager dan teen            ']

    message = trim(string_msg(language))
end subroutine GetMSG_calculateGammaFtoLow

subroutine GetMSG_calculateGammaFtoHigh(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'calculateGammaF: local water level higher than crest level', &
        'calculateGammaF: waterstand hoger dan kruin               ']

    message = trim(string_msg(language))
end subroutine GetMSG_calculateGammaFtoHigh

subroutine GetMSG_calc_influence_roughness(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculation influence roughness', &
        'Fout in berekening invloed ruwheid      ']

    message = trim(string_msg(language))
end subroutine GetMSG_calc_influence_roughness

subroutine GetMSG_first_segment_berm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'First or last segment is a berm. This is not allowed.', &
        'Eerste segment is een berm. Dat is niet toegestaan.  ']

    message = trim(string_msg(language))
end subroutine GetMSG_first_segment_berm

subroutine GetMSG_last_segment_berm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Last segment is a berm. This is not allowed.        ', &
        'Laatste segment is een berm. Dat is niet toegestaan.']

    message = trim(string_msg(language))
end subroutine GetMSG_last_segment_berm

subroutine GetFormatTooManyBerms(cfmt)
character(len=*), intent(out) :: cfmt
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Found ",i0, " berm segments. Maximum is number of berm segments is 2.")            ', &
        '("Er zijn ",i0, " berm segmenten gevonden. Het maximaal aantal berm segmenten is 2.")']

    cfmt = trim(string_msg(language))
end subroutine GetFormatTooManyBerms

subroutine GetMSGRemoveNonHorizontalBerm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Try to remove a non horizontal berm             ', &
        'Probeer een niet-horizontale berm te verwijderen']

    message = trim(string_msg(language))
end subroutine GetMSGRemoveNonHorizontalBerm

function GetFMTmodel_factor_smaller_than() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Model factor ",a," smaller than ",F6.3)', &
        '("Model factor ",a," kleiner dan ",F6.3) ']

    message = trim(string_msg(language))
end function GetFMTmodel_factor_smaller_than

function GetFMTmodel_factor_not_between() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Model factor ",a," not between ",F6.3," and ",F6.3)', &
        '("Model factor ",a," niet tussen ",F6.3," en ",F6.3) ']

    message = trim(string_msg(language))
end function GetFMTmodel_factor_not_between

function GetFMTroughnessfactors_out_of_range() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Roughnessfactors must be in range ",F3.1," ... ",F3.1,"; found: ",F5.2)       ', &
        '("Ruwheidsfactoren moeten liggen tussen ",F3.1," ... ",F3.1,"; gevonden: ",F5.2)']

    message = trim(string_msg(language))
end function GetFMTroughnessfactors_out_of_range

function GetFMTallocateError() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Memory allocation error for array(s) with total size: ",I0)    ', &
        '("Geheugen allocatie fout voor array(s) met totale grootte: ",I0)']

    message = trim(string_msg(language))
end function GetFMTallocateError

function GetFMTxcoordinates_must_increase() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("x-coordinates must increase with dx >= ",F4.1," m") ', &
        '("x-coordinaten moeten toenemen met dx >= ",F4.1," m")']

    message = trim(string_msg(language))
end function GetFMTxcoordinates_must_increase

function GetFMTzero_or_negative_varModelFactorCriticalOvertopping() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Negative or zero variance of critical overtopping uncertainty model; variable number: ",I0)  ', &
        '("Negatieve of nul variantie van kritieke overtopping model onzekerheid; variabel nummer: ",I0)']

    message = trim(string_msg(language))
end function GetFMTzero_or_negative_varModelFactorCriticalOvertopping

function GetFMTzero_or_negative_critical_overtopping() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Negative or zero critical overtopping: ",G)      ', &
        '("Negatieve of nul kritiek overtopping debiet: ",G)']

    message = trim(string_msg(language))
end function GetFMTzero_or_negative_critical_overtopping

function GetFMTdiffx_too_small() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("X-coordinates must differ at least ",F4.2,".",F8.3," and ",F8.3," are too close to each other.")                ', &
        '("X-coordinaten moeten ten minste ",F4.2," van elkaar verschillen.",F8.3," en ",F8.3," ligt te dicht bij elkaar.")']

    message = trim(string_msg(language))
end function GetFMTdiffx_too_small

function GetFMTdiffy_too_small() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("Coordinates in vertical direction must be non-decreasing.",F7.2," and ",F7.2," are not.")', &
        '("Verticale coordinaten mogen niet afnemen.",F7.2," en ",F7.2," doen dat wel.")            ']

    message = trim(string_msg(language))
end function GetFMTdiffy_too_small

function GetFMTdiffx_negative() result (message)
character(len=maxmsg) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        '("X-coordinates must increase at least with ",F4.2,".",F8.3," and ",F8.3," do not meet this condition.")', &
         '("X-coordinaten moeten ten minste met ",F4.2," toenemen.",F8.3," en ",F8.3," voldoen hier niet aan.")  ']

    message = trim(string_msg(language))
end function GetFMTdiffx_negative

end module OvertoppingMessages
