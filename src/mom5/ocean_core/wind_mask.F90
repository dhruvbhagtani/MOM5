module wind_mask_mod

!
!<CONTACT EMAIL="dhruv.bhagtani@anu.edu.au"> Dhruv Bhagtani
!</CONTACT>
!
!<REVIEWER EMAIL="andy.hogg@anu.edu.au"> Andrew McC Hogg
!</REVIEWER>
!
!<REVIEWER EMAIL="navid.constantinou@anu.edu.au"> Navid Constantinou
!</REVIEWER>
!
!<REVIEWER EMAIL="r.holmes@sydney.edu.au"> Ryan M Holmes
!</REVIEWER>
!
!<OVERVIEW>
! Set up a mask for wind stress term in the momentum equations. Data 
! contains values ranging from 0 to 1.
!</OVERVIEW>
!
!<DESCRIPTION>
! Set up a mask for wind stress term in the momentum equations. This 
! module allows us to run experiments by altering wind forcing 
! spatially in the momentum equations. Data is taken in the form of a
! NETCDF (wind_mask.nc) file, and stores it in Velocity%wmask array. 
! This mask is multiplied wth the surface forcing (Velocity%smf_bgrid) 
! in ocean_sbc.F90.
!</DESCRIPTION>

!<NAMELIST NAME="wind_mask_nml">
!  <DATA NAME="use_this_module" TYPE="logical">
!  Logical switch to input wind mask from NETCF file.  Default is false.
!  </DATA> 
!
!</NAMELIST>

use fms_mod,           only: open_namelist_file, close_file
use fms_mod,           only: check_nml_error
use fms_mod,           only: field_exist, read_data, file_exist
use mpp_mod,           only: input_nml_file, mpp_error
use mpp_mod,           only: FATAL, stdout, stdlog
use mpp_domains_mod,   only: mpp_update_domains

use ocean_domains_mod, only: get_local_indices, get_domain_offsets
use ocean_types_mod,   only: ocean_grid_type, ocean_domain_type
use ocean_types_mod,   only: ocean_velocity_type

implicit none

#include <ocean_memory.h>

! logical switch to input wind mask from NETCF file
logical :: use_this_module = .false.

! In future, add an option to input latitude and longitude ranges 
! instead of NETCDF file for creating a mask in the model itself.

namelist /wind_mask_nml/ use_this_module!, use_nc_file, x_min, 
!                                        x_max, y_,min, y_max

public wind_mask_input

contains

!#######################################################################
! <SUBROUTINE NAME="wind_mask_input">
!
! <DESCRIPTION>
! Input the wind stress mask from a NETCDF file. The code assumes the 
! name of NETCDF file as "wind_mask.nc".
!
! </DESCRIPTION>
!
subroutine wind_mask_input(Domain, Velocity)

  type(ocean_domain_type),   intent(inout) :: Domain
  type(ocean_velocity_type), intent(inout) :: Velocity

  character(len=128) :: wind_mask = "INPUT/wind_mask.nc"

  integer            :: ioun, io_status, ierr
  integer            :: ioff, joff

  integer :: stdoutunit,stdlogunit 
  stdoutunit=stdout();stdlogunit=stdlog() 

  joff = 0
  ioff = 0
  
#ifndef MOM_STATIC_ARRAYS
  call get_local_indices(Domain, isd, ied, jsd, jed, isc, iec, jsc, jec)

  ! allocate (Velocity%wmask(isd:ied,jsd:jed))
#else
  call get_domain_offsets(Domain, ioff, joff)
#endif

    ! provide for namelist over-ride of defaults 
#ifdef INTERNAL_FILE_NML
  read (input_nml_file, nml = wind_mask_nml, iostat = io_status)
  ierr = check_nml_error(io_status, "wind_mask_nml")
#else
  ioun = open_namelist_file()
  read (ioun,wind_mask_nml,IOSTAT=io_status)
  ierr = check_nml_error(io_status, "wind_mask_nml")
  call close_file(ioun)
#endif
    
  write (stdoutunit,'(/)')
  write (stdoutunit,wind_mask_nml)
  write (stdlogunit,wind_mask_nml)

  if(.not. use_this_module) return
  
  if(file_exist(wind_mask)) then
    call read_data(wind_mask, 'mask', Velocity%wmask(isc:iec,jsc:jec), &
      Domain%domain2d)
  else
    call mpp_error(FATAL, 'wind_mask_mod: file '//trim(wind_mask)//' does not exist')
  endif

  ! Check about this.
  ! call mpp_update_domains(Velocity%wmask, Domain%domain2d)

end subroutine wind_mask_input
! </SUBROUTINE> NAME="wind_mask_input"

end module wind_mask_mod