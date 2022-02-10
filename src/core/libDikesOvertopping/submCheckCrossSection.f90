submodule (geometryModuleOvertopping) submCheckCrossSection
contains
!> checkCrossSection:
!! check cross section
!!   @ingroup LibOvertopping
module procedure checkCrossSection
!
   implicit none
!
!  Local parameters
!
   type (tpGeometry) :: geometry !< structure with geometry data
   real(kind=wp)     :: rFactor  !< offending roughness factor

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check dike normal
   if ((psi < 0.0d0) .or. (psi > 360.0d0)) then
      error%errorCode = 1
      call GetMSGpsi_not_in_range(error%Message)
   endif

   ! check number of coordinates
   if (error%errorCode == 0) then
      if (coordinates%N < 2) then
         error%errorCode = 1
         call GetMSGdimension_cross_section_less_than_2(error%Message)
      endif
   endif

   ! initialize structure with geometry data
   if (error%errorCode == 0) then
      call initializeGeometry (psi, coordinates, roughnessFactors, geometry, error)
   endif

   ! check minimal distance x-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%x) < xDiff_min - marginDiff) then
         error%errorCode = 1
         write (error%Message, GetFMTxcoordinates_must_increase()) xDiff_min
      endif
   endif

   ! check minimal distance y-coordinates
   if (error%errorCode == 0) then
      if (minval(geometry%CoordDiff%y) < 0.0d0) then
         error%errorCode = 1
         call GetMSGycoordinates_must_be_nondecreasing(error%Message)
      endif
   endif

   if (error%errorCode == 0) then
       call checkSegmentTypes(geometry, error)
    endif

   ! check roughness factors
   if (error%errorCode == 0) then
      if (any(geometry%roughnessFactors < rFactor_min)) then
         error%errorCode = 1
         rFactor = minval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      else if (any(geometry%roughnessFactors > rFactor_max)) then
         error%errorCode = 1
         rFactor = maxval(geometry%roughnessFactors)
         write (error%Message,GetFMTroughnessfactors_out_of_range()) rFactor_min, rFactor_max, rFactor
      endif
   endif

   call deallocateGeometry(geometry)

   end procedure checkCrossSection

end submodule subMCheckCrossSection

