submodule (formulaModuleOvertopping) submCalcWaveProps
contains

!> calculateWaveLength:
!! calculate the wave length
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveLength
!***********************************************************************************************************
   implicit none

   ! calculate the wave length
   L0 = gravityConstant * (Tm_10**2) / (2.0_wp*pi)

end procedure calculateWaveLength

!> calculateWaveSteepness:
!! calculate the wave steepness
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveSteepness
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! calculate the wave steepness
   if (load%L0 > 0.0d0) then
      load%s0 = load%Hm0 / load%L0
   else
      error%errorCode = 1
      call GetMSGcalc_wave_steepness_period_is_zero(error%Message)
   endif

end procedure calculateWaveSteepness

!> calculateBreakerParameter:
!! calculate the breaker parameter
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateBreakerParameter
!***********************************************************************************************************
   implicit none

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! calculate the breaker parameter
   if (s0 > 0.0d0) then
      ksi0 = tanAlpha/sqrt(s0)
   else
      error%errorCode = 1
      call GetMSGcalc_breaker_param_steepness_is_zero(error%Message)
   endif

end procedure calculateBreakerParameter

!> calculateAngleWaveAttack:
!! calculate the angle of wave attack
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateAngleWaveAttack
!***********************************************************************************************************
   implicit none

   ! calculate angle of wave attack
   beta = abs (phi - psi)
   if (beta > 180.0d0) beta = 360.0d0 - beta

end procedure calculateAngleWaveAttack

end submodule submCalcWaveProps
