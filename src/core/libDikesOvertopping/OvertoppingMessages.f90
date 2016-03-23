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

integer, parameter :: maxmsg = 128

character(len=2) :: language = 'NL'   !< default : Dutch

private :: maxmsg, language
! IDs:
enum, bind(c)
! messages:
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
! formats :
    enumerator :: model_factor_smaller_than
    enumerator :: model_factor_not_between
    enumerator :: roughnessfactors_out_of_range
    enumerator :: allocateError
end enum

contains

!>
!! Subroutine that sets the language for error and validation messages
!!
!! @ingroup LibOvertopping
subroutine SetLanguage(lang)
use utilities
character(len=*), intent(in) :: lang

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
character(len=*), intent(out) :: lang

lang = language
end subroutine GetLanguage

character(len=maxmsg) function GetOvertoppingMessage(ID)
integer, intent(in) :: ID

select case(language)
    case('UK')
        select case (ID)
            case (validation_only_for_type_runup1)
                GetOvertoppingMessage = 'Validation only implemented for typeRunup=1'
            case (adjusted_xcoordinates)
                GetOvertoppingMessage = 'Error in calculation of adjusted x-coordinates'
            case (slope_negative)
                GetOvertoppingMessage = 'Error in calculating slopes (dx <= 0)'
            case (split_cross_section_seq_berm)
                GetOvertoppingMessage = 'Error in split cross section: sequential berms'
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
            case (breaker_param_is_zero)
                GetOvertoppingMessage = 'Error calculating 2% wave run-up: breaker parameter equals zero'
            case (calc_wave_overtopping_discharge)
                GetOvertoppingMessage = 'Error calculating wave overtopping discharge'
            case (calc_wave_steepness_period_is_zero)
                GetOvertoppingMessage = 'Error calculating wave steepness: wave period equals zero'
            case (calc_breaker_param_steepness_is_zero)
                GetOvertoppingMessage = 'Error calculating breaker parameter: wave steepness equals zero'
            case (calc_roots_cubic_function)
                GetOvertoppingMessage = 'Error calculating roots general cubic function'
            case default
                write(GetOvertoppingMessage,*) 'Internal error, ID = ', ID
        end select
    case default
        select case (ID)
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
            case (calc_influence_factors)
                GetOvertoppingMessage = 'Error in berekening van invloedsfactoren'
            case (remove_dike_segments_index)
                GetOvertoppingMessage = 'Fout bij verwijderen dijk secties: foute index'
            case (calc_representative_slope_angle)
                GetOvertoppingMessage = 'Fout in berekening representatieve hellingshoek'
            case (calc_influence_roughness)
                GetOvertoppingMessage = 'Fout in berekening invloed ruwheid'
            case (calc_influence_berms)
                GetOvertoppingMessage = 'Fout in berekening invloed bermen'
            case (breaker_param_is_zero)
                GetOvertoppingMessage = 'Fout in berekening 2% golf oploop: brekerparameter is nul'
            case (calc_wave_overtopping_discharge)
                GetOvertoppingMessage = 'Fout in berekening golf overslag debiet'
            case (calc_wave_steepness_period_is_zero)
                GetOvertoppingMessage = 'Fout in berekening golf steilte: golf periode is nul'
            case (calc_breaker_param_steepness_is_zero)
                GetOvertoppingMessage = 'Fout in berekening brekerparameter: golf steilte is nul'
            case (calc_roots_cubic_function)
                GetOvertoppingMessage = 'Fout in berekening van wortel 3e graads functie'
            case default
                write(GetOvertoppingMessage,*) 'Interne fout, ID = ', ID
        end select
    end select

end function GetOvertoppingMessage

character(len=maxmsg) function GetOvertoppingFormat(ID)
integer, intent(in) :: ID

select case(language)
    case('UK')
        select case (ID)
            case (model_factor_smaller_than)
                GetOvertoppingFormat = '("Model factor ",a," smaller than ",F6.3)'
            case (model_factor_not_between)
                GetOvertoppingFormat = '("Model factor ",a," not between ",F6.3," and ",F6.3)'
            case (roughnessfactors_out_of_range)
                GetOvertoppingFormat = '("Roughnessfactors must be in range 0.5 ... 1.0 ; found: ",F5.2)'
            case (allocateError)
                GetOvertoppingFormat = '("Memory allocation error for array(s) with total size: ",I0)'
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
                GetOvertoppingFormat = '("Ruwheidsfactoren moeten liggen tussen 0.5 ... 1.0 ; gevonden: ",F5.2)'
            case (allocateError)
                GetOvertoppingFormat = '("Geheugen allocatie fout voor array(s) met totale grootte: ",I0)'
            case default
                write(GetOvertoppingFormat,*) '(Interne fout, ID = ', ID, ')'
        end select
    end select

end function GetOvertoppingFormat

end module OvertoppingMessages