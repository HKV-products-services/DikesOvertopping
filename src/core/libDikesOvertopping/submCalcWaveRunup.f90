submodule (formulaModuleOvertopping) submCalcWaveRunup
   use parametersOvertopping
contains

!> calculateWaveRunup:
!! calculate wave runup
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveRunup
!***********************************************************************************************************
   implicit none

   ! if applicable adjust influence factors
   call adjustInfluenceFactors (gamma, 1, ksi0, ksi0Limit, error)

   if (error%errorCode == 0) then

      ! calculate 2% wave run-up for small breaker parameters
      if (ksi0 < ksi0Limit) then
         z2 = Hm0 * fRunup1 * gamma%gammaB * gamma%gammaF * gamma%gammaBeta * ksi0

      ! calculate 2% wave run-up for large breaker parameters
      else
         z2 = Hm0 * gamma%gammaF * gamma%gammaBeta * (fRunup2 - fRunup3/sqrt(ksi0))
         z2 = max(z2, 0.0d0)
      endif
      z2 = z2 * modelFactors%m_z2

   endif

end procedure calculateWaveRunup

end submodule submCalcWaveRunup
