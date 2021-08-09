      subroutine cal_vertical(nx,ny,nz,Z,missing_val,f,g)

      implicit none

      integer, intent(in) :: nx, ny, nz

      real,intent(in) :: f(nx,ny,nz), Z(nz), missing_val

      real, intent(out) :: g(nx,ny,nz)

      real :: dz(nz-1), msk(nx,ny,nz), outmsk(nx,ny,nz-1)

      real :: f0(nx,ny,nz), f1(nx,ny,nz-1),f2(nx,ny,nz-1)

      integer :: i, j, k, l, msk2d(nx,ny)

      
      msk = 0
      where(f.ne.missing_val)
        msk = 1
      end where

      outmsk = 0.5*(msk(:,:,1:nz-1)+msk(:,:,2:nz))
      where (outmsk.eq.0.5)
        outmsk = 0
      end where

      msk2d = sum(outmsk,3)
      msk2d = msk2d+2
      where(msk2d.gt.nz)
        msk2d = nz 
      end where

      f0 = 0
      where(f.ne.missing_val)
        f0 = f
      end where

      f1 = 0.5*(f0(:,:,1:nz-1)+f0(:,:,2:nz))
      dz = Z(2:nz)-Z(1:nz-1)

      !OMP PARALLEL DO
      do j = 1,ny
        do i = 1,nx
          f1(i,j,:) = f1(i,j,:)*dz
        end do
      end do
      !OMP END PARALLEL DO  

      !OMP PARALLEL DO
      do k = nz-1,1,-1
        do j = 1,ny
          do i = 1,nx
            if (msk2d(i,j).ge.k) then
              g(i,j,k) = g(i,j,k+1)-sum(f1(i,j,k:msk2d(i,j)))
            else 
              g(i,j,k) = 0
            end if
          end do
        end do
      end do
      !OMP END PARALLEL DO

      where(f.eq.missing_val)
        g = missing_val    
      end where

      end subroutine
