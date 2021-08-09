      module param

      implicit none

      character(len=*),parameter :: file_in = "in.nc"
     
      character(len=*),parameter :: t_NAME = "time"
      character(len=*),parameter :: x_NAME = "longitude"
      character(len=*),parameter :: y_NAME = "latitude"
      character(len=*),parameter :: z_NAME = "depth"
      character(len=*),parameter :: rho_NAME = "rhopoto"

      integer, parameter :: nx = 1321, ny = 481, nz = 50, nt = 1

      integer :: i, j, k

      real, parameter :: pi = 3.1415927, gamma = 7.2921e-5, g = 9.81, rho0 = 1026.5

      real, parameter :: missing_val = -32767, sf_sla = 1, af_sla = 0

      real :: T(nt), X(nx), Y(ny), Z(nz), rho(nx,ny,nz,nt), ugeos(nx,ny,nz,nt), vgeos(nx,ny,nz,nt)

      integer :: ncid, retval, tvarid, xvarid, yvarid, zvarid, rhovarid

      end module
