submodule (geometryModuleOvertopping) submInitializeGeometry
   use parametersOvertopping
   use OvertoppingMessages
contains

!> initializeGeometry:
!! initialize the geometry
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure initializeGeometry
!***********************************************************************************************************
   implicit none
!
!  local parameters
!
   integer                          :: i                                !< loop counter
   integer                          :: ierr                             !< error code

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! copy dike normal to structure with geometry data
   geometry%psi = psi

   ! copy number of coordinates to structure with geometry data
   geometry%Coordinates%N = coordinates%N

   ! allocate vectors in structure with geometry data
   call allocateVectorsGeometry (coordinates%N, geometry, error)

   if (error%errorCode == 0) then
   ! copy (x,y)-coordinates and roughness factors to structure with geometry data
      geometry%Coordinates%x     = Coordinates%x
      geometry%Coordinates%y     = Coordinates%y
      geometry%roughnessFactors = roughnessFactors

   ! calculate the differences and segment slopes
      call calculateSegmentSlopes (geometry, error)
   endif

   ! determine the type of the segments (slope or berm)
   if (error%errorCode == 0) then
      call determineSegmentTypes (geometry, error)
   endif

   if (error%errorCode == 0) then
       do i = 1, geometry%Coordinates%N - 1
           if (geometry%Roughnessfactors(i) < rFactor_min .or. geometry%Roughnessfactors(i) > rFactor_max) then
               error%errorCode = 1
               write(error%Message, GetFMTroughnessfactors_out_of_range(), iostat=ierr) &
                   rFactor_min, rFactor_max, geometry%Roughnessfactors(i)
               if (ierr /= 0) then
                   error%Message = GetFMTroughnessfactors_out_of_range()
               endif
               exit
           endif
       enddo
   endif 

   ! determine the number of berm segments
   if (error%errorCode == 0) then
      geometry%NbermSegments = count(geometry%segmentTypes == 2)
   endif

   if ( .not. associated(geometry%parent)) then
       allocate(geometry%parent)
       geometry%parent%base => geometry
   end if
end procedure initializeGeometry

end submodule submInitializeGeometry
