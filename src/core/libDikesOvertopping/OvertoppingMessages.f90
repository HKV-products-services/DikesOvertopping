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

!> IDs for the strings/parameters in this module:
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
        'fN (niet brekende golven) ', &
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

subroutine GetMSGerrorIndicator(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [ 'ERROR', 'FOUT ']

    message = trim(string_msg(language))
end subroutine GetMSGerrorIndicator

subroutine GetMSGwarningIndicator(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [ 'WARNING     ', 'WAARSCHUWING']

    message = trim(string_msg(language))
end subroutine GetMSGwarningIndicator

subroutine GetMSGadjusted_xcoordinates(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculation of adjusted x-coordinates  ', &
        'Fout in bepaling van gecorrigeerde x-coordinaten']

    message = trim(string_msg(language))
end subroutine GetMSGadjusted_xcoordinates

subroutine GetMSGslope_negative(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculating slope (dx <= 0)', &
        'Fout in berekening helling (dx <= 0)']

    message = trim(string_msg(language))
end subroutine GetMSGslope_negative

subroutine GetMSGsplit_cross_section_seq_berm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in splitting cross section: sequential berms', &
        'Fout in opsplitsen doorsnede: sequentiele bermen  ']

    message = trim(string_msg(language))
end subroutine GetMSGsplit_cross_section_seq_berm

subroutine GetMSGadjust_non_horizontal_seq_berm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error adjusting non-horizontal berms: sequential berms         ', &
        'Fout bij aanpassing niet-horizontale bermen: sequentiele bermen']

    message = trim(string_msg(language))
end subroutine GetMSGadjust_non_horizontal_seq_berm

subroutine GetMSGmerging_seq_berm(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in merging sequential berms (B=0)     ', &
        'Fout in samenvoegen sequentiele bermen (B=0)']

    message = trim(string_msg(language))
end subroutine GetMSGmerging_seq_berm

subroutine GetMSGcalc_horizontal_lengths(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculation horizontal lengths', &
        'Fout in berekening horizontale lengtes ']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_horizontal_lengths

subroutine GetMSGremove_dike_segments_index(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error removing dike segments: incorrect index ', &
        'Fout bij verwijderen dijk secties: foute index']

    message = trim(string_msg(language))
end subroutine GetMSGremove_dike_segments_index

subroutine GetMSGcalc_representative_slope_angle(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculation representative slope angle', &
        'Fout in berekening representatieve hellingshoek']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_representative_slope_angle

subroutine GetMSGcalc_influence_berms(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in calculation influence berms', &
        'Fout in berekening invloed bermen   ']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_influence_berms

subroutine GetMSGcalc_influence_factors(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in adjustment of influence factors', &
        'Error in berekening van invloedsfactoren']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_influence_factors

subroutine GetMSGcalc_wave_overtopping_discharge(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error calculating wave overtopping discharge', &
        'Fout in berekening golf overslag debiet     ']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_wave_overtopping_discharge

subroutine GetMSGbreaker_param_is_zero(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error calculating 2% wave run-up: breaker parameter equals zero', &
        'Fout in berekening 2% golf oploop: brekerparameter is nul      ']

    message = trim(string_msg(language))
end subroutine GetMSGbreaker_param_is_zero

subroutine GetMSGcalc_wave_steepness_period_is_zero(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error calculating wave steepness: wave period equals zero', &
        'Fout in berekening golf steilte: golf periode is nul     ']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_wave_steepness_period_is_zero

subroutine GetMSGcalc_breaker_param_steepness_is_zero(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error calculating breaker parameter: wave steepness equals zero', &
        'Fout in berekening brekerparameter: golf steilte is nul        ']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_breaker_param_steepness_is_zero

subroutine GetMSGcalc_roots_cubic_function(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error calculating roots general cubic function ', &
        'Fout in berekening van wortel 3e graads functie']

    message = trim(string_msg(language))
end subroutine GetMSGcalc_roots_cubic_function

subroutine GetMSGpsi_not_in_range(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Dike normal (psi) not between 0 and 360 degrees    ', &
        'Dijk normaal (psi) ligt niet tussen 0 en 360 graden']

    message = trim(string_msg(language))
end subroutine GetMSGpsi_not_in_range

subroutine GetMSGdimension_cross_section_less_than_2(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Number of coordinates cross section less than 2   ', &
        'Aantal coordinaten dijk doorsnede is kleiner dan 2']

    message = trim(string_msg(language))
end subroutine GetMSGdimension_cross_section_less_than_2

subroutine GetMSGycoordinates_must_be_nondecreasing(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Coordinates in the vertical direction must be non-decreasing', &
        'Verticale coordinaten mogen niet afnemen                    ']

    message = trim(string_msg(language))
end subroutine GetMSGycoordinates_must_be_nondecreasing

subroutine GetMSGdike_segment_mismatches(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Dike segment mismatches berm segment or slope segment             ', &
        'Dijk segment is van ander type dan berm segment of helling segment']

    message = trim(string_msg(language))
end subroutine GetMSGdike_segment_mismatches

subroutine GetMSGmax2berm_segments(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'A maximum of two berm segments is allowed   ', &
        'Maximum van twee berm segmenten overschreden']

    message = trim(string_msg(language))
end subroutine GetMSGmax2berm_segments

subroutine GetMSGfirst_and_last_must_be_slope(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'First and last dike segment must be a slope segment           ', &
        'Eerste en laatste dijk segment moeten een helling segment zijn']

    message = trim(string_msg(language))
end subroutine GetMSGfirst_and_last_must_be_slope

subroutine GetMSGwl_above_crest_not_allowed(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'In the overtopping freeboard routine is a local water level below crest not allowed.', &
        'In the overtopping module is een lokale waterstand boven de kruin niet toegestaan.  ']

    message = trim(string_msg(language))
end subroutine GetMSGwl_above_crest_not_allowed

subroutine GetMSGinterpolation_error_split_cross_sections(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Error in interpolation between results for split cross sections', &
        'Fout in interpolatie tussen resultaten for split cross sections']

    message = trim(string_msg(language))
end subroutine GetMSGinterpolation_error_split_cross_sections

subroutine GetMSGwl_above_crest(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'local water level above the crest level', &
        'Lokale waterstand boven de kruin       ']

    message = trim(string_msg(language))
end subroutine GetMSGwl_above_crest

subroutine GetMSGwave_height_or_periode_less_zero(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Wave height and/or wave period less than zero ', &
        'Golf hoogte en/of golf periode kleiner dan nul']

    message = trim(string_msg(language))
end subroutine GetMSGwave_height_or_periode_less_zero

subroutine GetMSGwave_direction_not_in_range(message)
character(len=*), intent(out) :: message
character(len=*), dimension(2), parameter :: string_msg = [&
        'Wave direction not between 0 and 360 degree', &
        'Golf hoek niet tussen 0 and 360 graden     ']

    message = trim(string_msg(language))
end subroutine GetMSGwave_direction_not_in_range

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
