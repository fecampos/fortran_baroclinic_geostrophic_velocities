      subroutine cal_vertical(nx,ny,nz,Z,missing_val,f,g)

      implicit none

      integer, intent(in) :: nx, ny, nz

      real,intent(in) :: f(nx,ny,nz), Z(nz), missing_val

      real, intent(out) :: g(nx,ny,nz)

      real :: f0(nx,ny,nz-1)

      integer :: i, j, k, l, msk2d(nx,ny), outmsk(nx,ny,nz-1), msk(nx,ny,nz)

      msk = 0
      where(f.ne.missing_val)
        msk = 1
      end where

      outmsk = 0.5*(msk(:,:,1:nz-1)+msk(:,:,2:nz))
      where (outmsk.eq.0.5)
        outmsk = 0
      end where

      msk2d = sum(outmsk,3)

      f0 = 0

      !OMP PARALLEL DO
      do k = 1,nz-1
        do j = 1,ny
          do i = 1,nx
            if ((f(i,j,k).ne.missing_val).and.(f(i,j,k+1).ne.missing_val)) then
              f0(i,j,k-1) = 0.5*(f(i,j,k)+f(i,j,k+1))*(Z(k+1)-Z(k))
            end if
          end do
        end do
      end do
      !OMP END PARALLEL DO        

      where((f0.gt.1).or.(f0.lt.-1))
        f0 = 0
      end where

      g(:,:,nz) = 0

      !OMP PARALLEL DO
      do k = nz-1,1,-1
        g(:,:,k) = g(:,:,k+1)-f0(:,:,k)
      end do
      !OMP END PARALLEL DO 

      where(msk2d.eq.0)
        msk2d = 1
      end where
      
      !OMP PARALLEL DO
      do j = 1,ny
        do i = 1,nx
          g(i,j,msk2d(i,j):nz) = missing_val
        end do
      end do
      !OMP END PARALLEL DO        

      end subroutine
