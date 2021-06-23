!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

module glc_fields

!BOP
! !MODULE: glc_fields

! !DESCRIPTION:
!  Holds coupling fields and other important data
!
! !REVISION HISTORY:
! 
!  Author: William Lipscomb, LANL
!
! !USES:

  use glc_kinds_mod
  use glad_main, only: glad_params

  implicit none
  save

! !PUBLIC MEMBER FUNCTIONS:

!----------------------------------------------------------------------
!
!   module variables
!
!----------------------------------------------------------------------

  type, public :: glad_bundle ! BK need array of fields for multiple instances

     ! Fields received from CESM coupler
  
     real(r8),dimension(:,:), allocatable ::  & 
        tsfc        ,&! surface temperature (Celsius)
                      ! received from coupler in Kelvin, must be converted
        qsmb          ! flux of new glacier ice (kg/m^2/s)
   
     ! output to coupler
   
     ! the following need to be targets for the sake of the override code in glc_export
!    real(r8),dimension(:,:), allocatable, target ::  &  BK: can't be a target
     real(r8),dimension(:,:), allocatable         ::  &
        ice_covered,&         ! whether each grid cell is ice-covered [0,1]
        topo                  ! glacier surface elevation (m)
   
     real(r8),dimension(:,:), allocatable :: &
        rofi       ,&! ice runoff (calving) flux (kg/m^2/s)
        rofl       ,&! ice runoff (calving) flux (kg/m^2/s)
        hflx         ! heat flux from glacier interior, positive down (W/m^2)
   
     real(r8),dimension(:,:), allocatable :: &
        ice_sheet_grid_mask  ! mask of ice sheet grid coverage

  end type glad_bundle

  type(glad_bundle),pointer,dimension(:) :: cpl_bundles ! BK added array for multiple instances
  type(glad_params) :: ice_sheet   ! Parameters relevant to all model instances

!EOP
!***********************************************************************

 contains

!***********************************************************************
!BOP
! !IROUTINE: glc_allocate_fields
! !INTERFACE:

 subroutine glc_allocate_fields (bundle, nx, ny)

! !DESCRIPTION:
!  Allocate fields declared here
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !USES:
   use glc_kinds_mod

! !INPUT/OUTPUT PARAMETERS:

   type(glad_bundle) :: bundle 
   integer (i4), intent(in) :: &
        nx, ny           ! grid dimensions

!EOP
!BOC

   ! from coupler
   allocate(bundle%tsfc(nx,ny))
   allocate(bundle%qsmb(nx,ny))

   ! to coupler
   allocate(bundle%ice_covered(nx,ny))
   allocate(bundle%topo(nx,ny))
   allocate(bundle%rofi(nx,ny))
   allocate(bundle%rofl(nx,ny))
   allocate(bundle%hflx(nx,ny))
   allocate(bundle%ice_sheet_grid_mask(nx,ny))
   
 end subroutine glc_allocate_fields

!***********************************************************************

!BOP
! !IROUTINE: glc_deallocate_fields
! !INTERFACE:

 subroutine glc_deallocate_fields(bundle)

! !DESCRIPTION:
!  Deallocate global arrays on glc grid.  BK: for all bundles
!
! !USERDOC:
!
! !REFDOC:
!
! !REVISION HISTORY:
!  same as module

! !USES:

! !INPUT/OUTPUT PARAMETERS:
   type(glad_bundle) :: bundle 


!EOP
!BOC

   ! from coupler
   deallocate(bundle%tsfc)
   deallocate(bundle%qsmb)

   ! to coupler
   deallocate(bundle%ice_covered)
   deallocate(bundle%topo)
   deallocate(bundle%rofi)
   deallocate(bundle%rofl)
   deallocate(bundle%hflx)
   deallocate(bundle%ice_sheet_grid_mask)
  
   end subroutine glc_deallocate_fields

!***********************************************************************

 end module glc_fields

!|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
