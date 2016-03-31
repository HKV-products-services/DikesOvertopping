module OvertoppingMessages
!> @file
!! This file contains the messages in the overtopping dll, in Dutch or English
!<
!
! Copyright (c) 2016, Deltares, HKV lijn in water, TNO
! $Id$
!
!>
!! Module for the messages in the overtopping dll, in Dutch or English
!! @ingroup LibOvertopping
!<
implicit none

integer, parameter :: maxmsg = 128, maxpar=32

character(len=2) :: language = 'NL'   !< default : Dutch

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
    enumerator :: calc_influence_roughness
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
    enumerator :: no_convergence_2percent_wave_runup
! formats :
    enumerator :: model_factor_smaller_than
    enumerator :: model_factor_not_between
    enumerator :: roughnessfactors_out_of_range
    enumerator :: allocateError
    enumerator :: xcoordinates_must_increase
    enumerator :: typeRunup_not_in_range
    enumerator :: zero_or_negative_varModelFactorCriticalOvertopping
    enumerator :: zero_or_negative_critical_overtopping
    enumerator :: diffx_too_small
    enumerator :: diffy_too_small
! parameters :
    enumerator :: par_fB
    enumerator :: par_fN
    enumerator :: par_fS
    enumerator :: par_2percent_wave_runup
    enumerator :: reductionFactorForeshore
end enum

contains

!>
!! Subroutine that sets the language for error and validation messages
!!    only strings 'NL' and 'UK' are recoqnized (lower and upper case)
!!
!! @ingroup LibOvertopping
subroutine SetLanguage(lang)
use utilities, only : to_upper
character(len=*), intent(in) :: lang   !< new language ID to be used

character(len=len(lang)) :: langUpper

langUpper = to_upper(lang)

select case (langUpper)
    case ('NL', 'UK')
        language = langUpper
end select
end subroutine SetLanguage

!>
!! Subroutine that gets the language for error and validation messages
!!
!! @ingroup LibOvertopping
subroutine GetLanguage(lang)
character(len=*), intent(out) :: lang   !< filled with current language ID

lang = language
end subroutine GetLanguage

!>
!! Subroutine that returns a message with the corresponding ID in the current language
!!
!! @ingroup LibOvertopping
character(len=maxmsg) function GetOvertoppingMessage(ID)
integer, intent(in) :: ID  !< identification number of string

select case(language)
    case('UK')
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
            case (calc_influence_roughness)
                GetOvertoppingMessage = 'Error in calculation influence roughness'
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
            case (no_convergence_2percent_wave_runup)
                GetOvertoppingMessage = 'No convergence in iteration procedure 2% wave run-up'
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
            case (calc_influence_roughness)
                GetOvertoppingMessage = 'Fout in berekening invloed ruwheid'
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
            case (no_convergence_2percent_wave_runup)
                GetOvertoppingMessage = 'Geen convergentie in iteratief proces voor bepaling 2% golf oploop'
            case default
                write(GetOvertoppingMessage,*) 'Interne fout, ID = ', ID
        end select
    end select

end function GetOvertoppingMessage

!>
!! Subroutine that returns a Fortran format string with the corresponding ID in the current language
!!
!! @ingroup LibOvertopping
character(len=maxmsg) function GetOvertoppingFormat(ID)
integer, intent(in) :: ID  !< identification number of string

select case(language)
    case('UK')
        select case (ID)
            case (model_factor_smaller_than)
                GetOvertoppingFormat = '("Model factor ",a," smaller than ",F6.3)'
            case (model_factor_not_between)
                GetOvertoppingFormat = '("Model factor ",a," not between ",F6.3," and ",F6.3)'
            case (roughnessfactors_out_of_range)
                GetOvertoppingFormat = '("Roughnessfactors must be in range ",F3.1," ... ",F3.1,"; found: ",F5.2)'
            case (allocateError)
                GetOvertoppingFormat = '("Memory allocation error for array(s) with total size: ",I0)'
            case (xcoordinates_must_increase)
                GetOvertoppingFormat = '("x-coordinates must increase with dx >= ",F4.1," m")'
            case (typeRunup_not_in_range)
                GetOvertoppingFormat = '("TypeRunup must be 0 or 1, found: ",I0)'
            case (zero_or_negative_varModelFactorCriticalOvertopping)
                GetOvertoppingFormat = '("Negative or zero variance of critical overtopping uncertainty model; variable number: ",I0)'
            case (zero_or_negative_critical_overtopping)
                GetOvertoppingFormat = '("Negative or zero critical overtopping: ",G)'
            case (diffx_too_small)
                GetOvertoppingFormat = '("X-coordinates must differ at least ",F4.2,".",F8.3," and ",F8.3," are too close to each other.")'
            case (diffy_too_small)
                GetOvertoppingFormat = '("Coordinates in vertical direction must be non-decreasing.",F7.2," and ",F7.2," are not.")'
            case default
                write(GetOvertoppingFormat,*) '(Internal error, ID = ', ID, ')'
        end select
    case default
        select case (ID)
            case (model_factor_smaller_than)
                GetOvertoppingFormat = '("Model factor ",a," kleiner dan ",F6.3)'
            case (model_factor_not_between)
                GetOvertoppingFormat = '("Model factor ",a," niet tussen ",F6.3," en ",F6.3)'
            case (roughnessfactors_out_of_range)
                GetOvertoppingFormat = '("Ruwheidsfactoren moeten liggen tussen ",F3.1," ... ",F3.1,"; gevonden: ",F5.2)'
            case (allocateError)
                GetOvertoppingFormat = '("Geheugen allocatie fout voor array(s) met totale grootte: ",I0)'
            case (xcoordinates_must_increase)
                GetOvertoppingFormat = '("x-coordinaten moeten toenemen met dx >= ",F4.1," m")'
            case (typeRunup_not_in_range)
                GetOvertoppingFormat = '("TypeRunup moet 0 of 1 zijn; gevonden: ",I0)'
            case (zero_or_negative_varModelFactorCriticalOvertopping)
                GetOvertoppingFormat = '("Negatieve of nul variantie van kritieke overtopping model onzekerheid; variabel nummer: ",I0)'
            case (zero_or_negative_critical_overtopping)
                GetOvertoppingFormat = '("Negatieve of nul kritiek overtopping debiet: ",G)'
            case (diffx_too_small)
                GetOvertoppingFormat = '("X-coordinaten moeten ten minste ",F4.2," van elkaar verschillen.",F8.3," en ",F8.3," ligt te dicht bij elkaar.")'
            case (diffy_too_small)
                GetOvertoppingFormat = '("Verticale coordinaten mogen niet afnemen.",F7.2," en ",F7.2," doen dat wel.")'
            case default
                write(GetOvertoppingFormat,*) '(Interne fout, ID = ', ID, ')'
        end select
    end select

end function GetOvertoppingFormat

!>
!! Subroutine that returns the name of an input parameter with the corresponding ID in the current language
!!
!! @ingroup LibOvertopping
character(len=maxpar) function GetOvertoppingParameter(ID)
integer, intent(in) :: ID  !< identification number of string

select case(language)
    case('UK')
        select case (ID)
            case (par_fB)
                GetOvertoppingParameter = 'fB (breaking waves)'
            case (par_fN)
                GetOvertoppingParameter = 'fN (non-breaking waves)'
            case (par_fS)
                GetOvertoppingParameter = 'fS (shallow waves)'
            case (par_2percent_wave_runup)
                GetOvertoppingParameter = '2% wave runup'
            case (reductionFactorForeshore)
                GetOvertoppingParameter = 'reduction factor foreshore'
            case default
                write(GetOvertoppingParameter,*) '(Internal error, ID = ', ID, ')'
        end select
    case default
        select case (ID)
            case (par_fB)
                GetOvertoppingParameter = 'fB (brekende golven)'
            case (par_fN)
                GetOvertoppingParameter = 'fN (niet brekende golven)'
            case (par_fS)
                GetOvertoppingParameter = 'fS (ondiepe golven)'
            case (par_2percent_wave_runup)
                GetOvertoppingParameter = '2% golf oploop'
            case (reductionFactorForeshore)
                GetOvertoppingParameter = 'reductie factor vooroever'
            case default
                write(GetOvertoppingParameter,*) '(Interne fout, ID = ', ID, ')'
        end select
 end select
end function GetOvertoppingParameter

end module OvertoppingMessages