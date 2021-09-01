module wind_mask_mod
#define COMP isc:iec,jsc:jec

!<OVERVIEW>
! Set up wind mask. 
!</OVERVIEW>
!
!<DESCRIPTION>
! Set up wind mask for momentum equation. This mask is multiplied wth the surface 
! forcing (Velocity%smf). Reads information from wind_mask.nc file. 
!</DESCRIPTION>

!<NAMELIST NAME="wind_mask_nml">
!  <DATA NAME="use_this_module" TYPE="logical">
!  If true, use wind mask in momentum equations.
!  </DATA> 

use fms_mod,         only: open_namelist_file, close_file, check_nml_error
use fms_mod,         only: field_exist, read_data, file_exist, write_data
use mpp_mod,         only: input_nml_file, mpp_error, mpp_min, mpp_pe
use mpp_mod,         only: FATAL, WARNING, NOTE, stdout, stdlog
use axis_utils_mod,  only: nearest_index

use ocean_domains_mod,    only: get_local_indices, get_domain_offsets
use ocean_parameters_mod, only: TERRAIN_FOLLOWING, grav
use ocean_types_mod,      only: ocean_grid_type, ocean_domain_type, ocean_velocity_type
!use ocean_util_mod,       only: write_chksum_2d, write_chksum_2d_int
implicit none

#include <ocean_memory.h>

namelist /wind_mask_nml/ use_this_module!, use_nc_file, x_min, x_max, y_,min, y_max

public wind_mask_input

contains

!#######################################################################
! <SUBROUTINE NAME="wind_mask_input">
!
! <DESCRIPTION>
! Input the wind stress mask from NETCDF file.
!
! </DESCRIPTION>
!
subroutine wind_mask_input (Domain, Grid, grid_file, vert_coordinate_type)

    type(ocean_domain_type),   intent(inout)        :: Domain
    type(ocean_grid_type),     intent(inout)        :: Grid
    type(ocean_velocity_type), intent(in)           :: Velocity
    character(len=*),          intent(in), optional :: grid_file
    integer,                   intent(in)           :: vert_coordinate_type

    character(len=128)  :: wind_mask = "INPUT/wind_mask.nc"
    character(len=128)  :: grd_file

    integer             :: imin, jmin, kmin
    integer             :: ioun, io_status, ierr
    integer             :: i, j, ioff, joff
    integer             :: k, kb 

    integer :: stdoutunit,stdlogunit 
    stdoutunit=stdout();stdlogunit=stdlog() 

    joff=0
    ioff=0

#ifndef MOM_STATIC_ARRAYS
    call get_local_indices(Domain, isd, ied, jsd, jed, isc, iec, jsc, jec)
#else
    call get_domain_offsets(Domain,ioff,joff)
#endif

    ! provide for namelist over-ride of defaults 
#ifdef INTERNAL_FILE_NML
    read (input_nml_file, nml = wind_mask_nml, iostat = io_status)
    ierr = check_nml_error(io_status,"wind_mask_nml")
#else
    ioun = open_namelist_file()
    read (ioun,wind_mask_nml,IOSTAT=io_status)
    ierr = check_nml_error(io_status, "wind_mask_nml")
    call close_file(ioun)
#endif
    
    write (stdoutunit,'(/)')
    write (stdoutunit,wind_mask_nml)
    write (stdlogunit,wind_mask_nml)

    ! In future, add an option to input latitude and longitude ranges instead of NETCDF file
    ! for creating a mask in the model itself.
    if(file_exist(wind_mask))
        call read_data(wind_mask, "mask", Velocity%wmask(isc:iec,jsc:jec), Domain%domain2d)
    else
        call mpp_error(FATAL, "wind_mask_mod: mask does not exist in file "//trim(wind_mask)//)
    endif

    call mpp_update_domains(Velocity%wmask(:,:)  , Dom%domain2d)

end subroutine wind_mask_input

end module wind_mask_mod
