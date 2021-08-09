      program compute_geos_vel

      use netcdf 

      use param

      implicit none

      real, dimension(nx,ny,nz) :: drhodx, drhody, f, f1, f2

      retval = nf90_open(file_in, NF90_NOWRITE, ncid)
      retval = nf90_inq_varid(ncid, x_NAME, xvarid)
      retval = nf90_get_var(ncid, xvarid, X)      
      retval = nf90_inq_varid(ncid, y_NAME, yvarid)
      retval = nf90_get_var(ncid, yvarid, Y)
      retval = nf90_inq_varid(ncid, z_NAME, zvarid)
      retval = nf90_get_var(ncid, zvarid, Z)
      retval = nf90_inq_varid(ncid, t_NAME, tvarid)
      retval = nf90_get_var(ncid, tvarid, T)
      retval = nf90_inq_varid(ncid, rho_NAME, rhovarid)
      retval = nf90_get_var(ncid, rhovarid, rho)
      retval = nf90_close(ncid)     
    
      where(rho.ne.missing_val)
        rho = rho*sf_sla+af_sla
      end where

      !OMP PARALLEL DO      
      do k = 1,nz
        do i = 1,nx
          f(i,:,k) = 2*gamma*sin(Y*pi/180)
        end do
      end do
      !OMP END PARALLEL DO

      !OMP PARALLEL DO      
      do i = 1,nz
        call gradient(nx,ny,missing_val,X,Y,rho(:,:,i,:),drhodx(:,:,i),drhody(:,:,i))
      end do
      !OMP END PARALLEL DO

      f1 = missing_val
      f2 = missing_val
 
      where(f.ne.0)
        f1 = -(g*drhodx)/(rho0*f)
        f2 = (g*drhody)/(rho0*f)
      end where

      where (rho(:,:,:,1).eq.missing_val)
        f1 = missing_Val
        f2 = missing_val
      end where

      !where ((f1.gt.0.1).or.(f1.lt.-0.1))
      !  f1 = missing_val
      !end where

      !where ((f2.gt.0.1).or.(f2.lt.-0.1))
      !  f2 = missing_val
      !end where

      call cal_vertical(nx,ny,nz,Z,missing_val,f1,ugeos)
      call cal_vertical(nx,ny,nz,Z,missing_val,f2,vgeos)

      call write_tw_geos(nx,ny,nz,nt,X,Y,Z, &
           &T,missing_val,ugeos,vgeos)

      end program
