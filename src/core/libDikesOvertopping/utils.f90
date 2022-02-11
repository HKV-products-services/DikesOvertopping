module utils
    use precision
    private
    public :: isEqualZero

contains

!> isEqualZero:
!! is a real (almost) zero
!***********************************************************************************************************
   logical function isEqualZero (x)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp), intent(in)  :: x  !< real number
!
!  Local parameters
!
   real(kind=wp), parameter   :: margin = 1.0d-14 !< absolute value for the margin
      
! ==========================================================================================================

   isEqualZero = (abs(x) <= margin)

   end function isEqualZero

end module utils
