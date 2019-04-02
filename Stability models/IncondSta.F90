!
!
!****************************************************************************
!
!  SUBROUTINE: IncondSta
!
!  PURPOSE:  Compute unconditionally stable cells.
!
!****************************************************************************
subroutine IncondSta()
!
!
!
!	Variables globales
	use fslamGlobals_structures
	use fslamGlobals_shared
	use fslamGlobals_private
!
!
	implicit double precision (a-h,o-z)
!
!
!
!	Unconditionally unstable cells
!
!	Variables
    REAL*8  :: normMean
    REAL*8  :: NormalCDF
!
!
!   Initial condition saturated
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,iZone) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	DO j=1,my
		DO i=1,mx
!
!			Static cell values
			iZone = zones(i,j)
!           
!           Check nodata
            IF (iZone .NE. nodata) THEN
!
                Zmax = Gaussh(iZone)%mean
!
!               Saturated soil
                h_wt(i,j) = Zmax
!
            ELSE
                h_wt(i,j) = nodata
            ENDIF
!
        ENDDO
    ENDDO
!    
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!   
!
!   Update FS parameters
    CALL UpdateFsGaussian()
    
!   Compute probability of no failure
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,normMean) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	do j=1,my
		do i=1,mx
! 
!           Check nodata
            IF (FS_mu(i,j) .NE. nodata) THEN
! 
!               Normalize variable
                normMean = (1 - FS_mu(i,j)) / FS_std(i,j)
!
!               Compute cumulative probability for FS = 1  
                UncEst(i,j) = 1.d0 - NormalCDF(normMean);
!
            ELSE
                UncEst(i,j) = nodata
            ENDIF
!
        enddo
    enddo
!    
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!    
!   Write results
    CALL WriteGrid(UncEst, './res/PROB_uncond_stable.txt')!
!
!	Log file
    write(6,'("Computed unconditionally stable cells probability")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed unconditionally stable cells probability")')
	close(100)
!
!
end subroutine
