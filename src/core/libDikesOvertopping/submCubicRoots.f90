submodule (formulaModuleOvertopping) submCubicRoots
   use utils
   use OvertoppingMessages
contains

!> realRootsCubicFunction:
!! calculate the roots of a cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
module procedure realRootsCubicFunction
!***********************************************************************************************************
   implicit none
!
   double complex :: z(3)  !< complex roots cubic function
   integer        :: i     !< counter complex roots

! ==========================================================================================================

   ! calculate complex roots cubic function
   call rootsGeneralCubic (a, b, c, d, z, error)

   ! initialize (number of) real roots
   N = 0
   x = 0.0d0

   if (error%errorCode == 0) then

      ! loop over complex roots cubic function
      do i=1, 3

         ! check if imaginary part equals zero
         if (isEqualZero (imag(z(i)))) then

            ! add to real roots cubic function
            if (error%errorCode == 0) then
               N    = N+1
               x(N) = dble(z(i))
            endif

         endif

      enddo

   endif

end procedure realRootsCubicFunction

!> rootsGeneralCubic:
!! calculate the roots of a generic cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine rootsGeneralCubic (a, b, c, d, z, error)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),    intent(in   ) :: a              !< coefficients a cubic function
   real(kind=wp),    intent(in   ) :: b              !< coefficients b cubic function
   real(kind=wp),    intent(in   ) :: c              !< coefficients c cubic function
   real(kind=wp),    intent(in   ) :: d              !< coefficients d cubic function
   double complex,   intent(  out) :: z(3)           !< roots cubic function
   type(tMessage),   intent(inout) :: error          !< error struct
!
!  Local parameters
!
   real(kind=wp)  :: p, q  !< coefficients depressed cubic function

! ==========================================================================================================

   ! initialize flag for succes and error message
   error%errorCode = 0

   ! check first coefficient cubic function
   if (isEqualZero (a)) error%errorCode = 1

   if (error%errorCode == 0) then

      ! substitution of z = t-b/(3a) gives: t^3 + pt + q = 0
      p = (3*a*c - b**2)/(3*(a**2))
      q = (2*(b**3) -9*a*b*c +27*(a**2)*d)/(27*(a**3))

      ! calculate roots depressed cubic
      call rootsDepressedCubic (p, q, z)

      ! calculate roots general cubic
      z = z - b/(3*a)

   endif

   ! determine possible error message
   if (error%errorCode /= 0) then
      call GetMSGcalc_roots_cubic_function(error%Message)
   endif

   end subroutine rootsGeneralCubic

!> rootsDepressedCubic:
!! calculate the roots of a depressed cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine rootsDepressedCubic (p, q, z)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   real(kind=wp),   intent(in)  :: p     !< coefficient p depressed cubic
   real(kind=wp),   intent(in)  :: q     !< coefficient q depressed cubic
   double complex,  intent(out) :: z(3)  !< roots depressed cubic
!
!  Local parameters
!
   double complex  :: D     !< discriminant quadratic equation in w^3
   double complex  :: w3    !< one solution quadratic equation in w^3
   double complex  :: w(3)  !< cubic roots  quadratic equation in w^3

! ==========================================================================================================

   ! substitution of z = w-p/(3w) gives:  w^6 + qw^3 - (p^3)/27 = 0

   ! calculate one solution for w^3:
   D  = q**2+4*(p**3)/27
   w3 = (-q + sqrt(D))/2

   ! calculate cubic roots
   call cubicRoots (w3, w)

   ! calculate roots depressed cubic:
   if (minval(abs(w)) > 0.0d0) then
      z = w - p/(3*w)
   else
      z = 0.0d0 ! only if p=0 and q=0
   endif

   end subroutine rootsDepressedCubic

!> cubicRoots:
!! calculate the roots of a cubic function
!!   @ingroup LibOvertopping
!***********************************************************************************************************
   subroutine cubicRoots (z, roots)
!***********************************************************************************************************
!
   implicit none
!
!  Input/output parameters
!
   double complex, intent(in)  :: z        !< complex number
   double complex, intent(out) :: roots(3) !< cubic roots
!
!  Local parameters
!
   double complex, parameter   :: i = cmplx(0.0d0,1.0d0)   !< complex number i
   real(kind=wp)               :: arg                      !< argument complex number

! ==========================================================================================================

   ! check radius of z
   if (isEqualZero (abs(z))) then

      ! z equals zero
      roots = 0.0d0

   else

      ! calculate argument z
      arg = atan2(imag(z),dble(z))

      ! calculate cubic roots z
      roots(1) = (abs(z)**(1/3.0d0)) * exp(i* arg/3)
      roots(2) = (abs(z)**(1/3.0d0)) * exp(i*(arg+2*pi)/3)
      roots(3) = (abs(z)**(1/3.0d0)) * exp(i*(arg-2*pi)/3)

   endif

   end subroutine cubicRoots

end submodule submCubicRoots
