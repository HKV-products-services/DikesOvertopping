!> @file
!! This file contains a module with the type definitions for Dikes Overtopping
!<
!***********************************************************************************************************
!
!  Programmer: Bastiaan Kuijper, HKV consultants / Edwin Spee, Deltares
!
!  Copyright (c) 2016, Deltares, HKV lijn in water, TNO
!  $Id$
!
!***********************************************************************************************************
!
!> type definitions for Dikes Overtopping
!
   module typeDefinitionsOvertopping
!***********************************************************************************************************
   !
   use precision 
   
   !> tpGeometry: structure with geometry data
   type, public              :: tpGeometry
      real(kind=wp)          :: psi                              !< dike normal (degrees)
      integer                :: nCoordinates                     !< number of coordinates cross section 
      real(kind=wp), pointer :: xCoordinates(:)      => null()   !< vector with x-coordinates cross section (m)
      real(kind=wp), pointer :: yCoordinates(:)      => null()   !< vector with y-coordinates cross section (m+NAP)
      real(kind=wp), pointer :: roughnessFactors(:)  => null()   !< vector with roughness factors cross section
      real(kind=wp), pointer :: xCoordDiff(:)        => null()   !< vector with differences in x-coordinates (m)
      real(kind=wp), pointer :: yCoordDiff(:)        => null()   !< vector with differences in y-coordinates (m)
      real(kind=wp), pointer :: segmentSlopes(:)     => null()   !< vector with slopes dike segments
      integer, pointer       :: segmentTypes(:)                  !< vector with segment types (1=slope,2=berm,3=other)
      integer                :: NbermSegments                    !< number of berm segments
   end type tpGeometry

   !> tpLoad: structure with load parameters
   type, public              :: tpLoad                  
      real(kind=wp)          :: h                       !< local water level (m+NAP)
      real(kind=wp)          :: Hm0                     !< significant wave height (m)
      real(kind=wp)          :: Tm_10                   !< spectral wave period (s)
      real(kind=wp)          :: phi                     !< wave direction (degrees)
   end type tpLoad
   
   real(kind=wp), parameter :: fRunup1 = 1.65_wp
   real(kind=wp), parameter :: fRunup2 = 4.00_wp
   real(kind=wp), parameter :: fRunup3 = 1.50_wp
   
   !> OvertoppingModelFactors: C-structure with model factors
   type, public, bind(C) :: tpOvertoppingInput
      real(kind=wp) :: factorDeterminationQ_b_f_n         !< model factor for non-breaking waves
      real(kind=wp) :: factorDeterminationQ_b_f_b         !< model factor for breaking waves
      real(kind=wp) :: m_z2                               !< model factor describing the uncertainty of 2% runup height
      real(kind=wp) :: fshallow                           !< model factor for shallow waves
      real(kind=wp) :: ComputedOvertopping                !< model factor computed overtopping
      real(kind=wp) :: CriticalOvertopping                !< model factor critical overtopping
      real(kind=wp) :: relaxationFactor                   !< relaxation factor iteration procedure wave runup
      real(kind=wp) :: reductionFactorForeshore = 0.5_wp  !< reduction factor foreshore
   end type tpOvertoppingInput

   !> tpOvertopping: structure with overtopping results
   type, public             :: tpOvertopping
      real(kind=wp)         :: z2                      !< 2% wave run-up (m)
      real(kind=wp)         :: Qo                      !< wave overtopping discharge (m3/m per s)
   end type

   ! -------------------------------------
   !  Public parameters
   ! -------------------------------------
   real(kind=wp),  parameter :: xDiff_min     = 2.0d-2            !< minimal value distance between x-coordinates (m)
   real(kind=wp),  parameter :: marginDiff    = 1.0d-14           !< margin for minimal distance (m)
   real(kind=wp),  parameter :: berm_min      = 0.0d0             !< minimal value gradient berm segment
   real(kind=wp),  parameter :: berm_max      = 1.0d0/15          !< maximal value gradient berm segment
   real(kind=wp),  parameter :: slope_min     = 1.0d0/8           !< minimal value gradient slope segment
   real(kind=wp),  parameter :: slope_max     = 1.0d0             !< maximal value gradient slope segment
   real(kind=wp),  parameter :: marginGrad    = 0.0025d0          !< margin for minimal and maximal gradients
   real(kind=wp),  parameter :: rFactor_min   = 0.5d0             !< minimal value roughness factor dike segments
   real(kind=wp),  parameter :: rFactor_max   = 1.0d0             !< maximal value roughness factor dike segments
   real(kind=wp),  parameter :: mz2_min       = 0.0d0             !< minimal value model factor of 2% runup height
   real(kind=wp),  parameter :: mz2_max       = huge(mz2_max)     !< maximal value model factor of 2% runup height
   real(kind=wp),  parameter :: fB_min        = 0.0d0             !< minimal value model factor for breaking waves
   real(kind=wp),  parameter :: fB_max        = huge(fB_max)      !< maximal value model factor for breaking waves
   real(kind=wp),  parameter :: fN_min        = 0.0d0             !< minimal value model factor for non-breaking waves
   real(kind=wp),  parameter :: fN_max        = huge(fN_max)      !< maximal value model factor for non-breaking waves
   real(kind=wp),  parameter :: fS_min        = 0.0d0             !< minimal value model factor for shallow waves
   real(kind=wp),  parameter :: fS_max        = huge(fS_max)      !< maximal value model factor for shallow waves
   real(kind=wp),  parameter :: foreshore_min = 0.3d0             !< minimal value reduction factor foreshore
   real(kind=wp),  parameter :: foreshore_max = 1.0d0             !< maximal value reduction factor foreshore
   integer,   parameter      :: z2_iter_max1  =  49               !< maximal number of iterations for calculation z2 part 1
   integer,   parameter      :: z2_iter_max2  =  70               !< maximal number of iterations for calculation z2 part 1 & 2
   real(kind=wp),  parameter :: z2_margin     = 0.001d0           !< margin for convergence criterium calculation z2

!***********************************************************************************************************
   end module typeDefinitionsOvertopping
!***********************************************************************************************************
