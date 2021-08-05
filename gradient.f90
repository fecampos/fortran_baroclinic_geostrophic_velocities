      subroutine  gradient(nx,ny,missing_val,lon,lat,f,gx,gy)

      implicit none

      integer, intent(in) :: nx,ny

      real,intent(in) :: f(nx,ny), lon(nx), lat(ny), missing_val

      real, intent(out) :: gx(nx,ny), gy(nx,ny)

      real :: dx, dy, g(nx,ny)

      integer :: i, j

      g = f

      where(f.eq.missing_val)
        g = 0
      end where

      !OMP PARALLEL DO
      do i = 2,nx-1
        do j = 1,ny
          call gsw_distance(lon(i+1),lon(i-1),lat(j),lat(j),dx)
          gx(i,j) = (g(i+1,j)-g(i-1,j))/dx        
        end do
      end do
      !OMP END PARALLEL DO
 
      !OMP PARALLEL DO
      do j = 1,ny
        call gsw_distance(lon(2),lon(1),lat(j),lat(j),dx)
        gx(1,j) = (g(2,j)-g(1,j))/dx
        call gsw_distance(lon(nx),lon(nx-1),lat(j),lat(j),dx)
        gx(nx,j) = (g(nx,j)-g(nx-1,j))/dx
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        do j = 2,ny-1
          call gsw_distance(lon(i),lon(i),lat(j+1),lat(j-1),dy)
          gy(i,j) = (g(i,j+1)-g(i,j-1))/dy
        end do
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO
      do i = 1,nx
        call gsw_distance(lon(i),lon(i),lat(2),lat(1),dy)
        gy(i,1) = (g(i,2)-g(i,1))/dy
        call gsw_distance(lon(i),lon(i),lat(ny),lat(ny-1),dy)
        gy(i,ny) = (g(i,ny)-g(i,ny-1))/dy
      end do
      !OMP END PARALLEL DO

      where(f.eq.missing_val)
        gx = missing_val
        gy = missing_val
      end where

      end subroutine
