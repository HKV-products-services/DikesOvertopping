submodule (mainModuleOvertopping) submCalcWaveOvertopping
   use formulaModuleOvertopping
   use geometryModuleOvertopping
   use factorModuleOvertopping
contains

!> calculateWaveOvertopping:
!! calculate wave overtopping
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure calculateWaveOvertopping
!***********************************************************************************************************
   implicit none
!
   type (tpGeometry), pointer :: geometryFlatBerms !< structure with geometry data with horizontal berms
   real(kind=wp)     :: tanAlpha          !< representative slope angle
   real(kind=wp)     :: ksi0              !< breaker parameter
   real(kind=wp)     :: ksi0Limit         !< limit value breaker parameter

! ==========================================================================================================

   ! calculate wave steepness
   call calculateWaveSteepness (load, error)
   
   ! if applicable adjust non-horizontal berms
   geometryFlatBerms => geometry%parent%geometryFlatBerms
   if (error%errorCode == 0) then
      if (geometryFlatBerms%Coordinates%N == 0) then
          call adjustNonHorizontalBerms (geometry, geometryFlatBerms, error)
      end if
   endif

   ! calculate representative slope angle
   if (error%errorCode == 0) then
      call calculateTanAlpha (load, z2, geometryFlatBerms, tanAlpha, error)
   endif

   ! calculate breaker parameter
   if (error%errorCode == 0) then
      call calculateBreakerParameter (tanAlpha, load%s0, ksi0, error)
   endif

   ! calculate influence factor berms (gammaB)
   if (error%errorCode == 0) then
      if (geometry%NbermSegments > 0) then
         call calculateGammaB (load, z2, geometryFlatBerms, gamma%gammaB, error)
      else
         gamma%gammaB = 1.0d0
      endif
   endif

   ! calculate limit value breaker parameter
   if (error%errorCode == 0) then
      call calculateBreakerLimit (gamma%gammaB, ksi0Limit, error)
   endif

   ! calculate influence factor roughness (gammaF)
   if (error%errorCode == 0) then
      call calculateGammaF (load%h, ksi0, ksi0Limit, gamma, z2, geometry, error)
   endif

   ! if applicable adjust influence factors
   if (error%errorCode == 0) then
      call adjustInfluenceFactors (gamma, 2, ksi0, ksi0Limit, error)
   endif

   ! calculate wave overtopping discharge
   if (error%errorCode == 0) then
      call calculateWaveOvertoppingDischarge (load, tanAlpha, gamma, ksi0, &
                                              geometry%Coordinates%y(geometry%Coordinates%N),        &
                                              modelFactors, Qo, error)
   endif

end procedure calculateWaveOvertopping

end submodule submCalcWaveOvertopping
