!
!
!****************************************************************************
!
!  SUBROUTINE: FinalSaturation
!
!  PURPOSE:  Compute probability of unstable.
!
!****************************************************************************
subroutine FinalSaturation()
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
!	FS for initial saturation
!
!	Variables
    REAL*8  :: normMean
    REAL*8  :: NormalCDF
!
    write(6,'("Initiating event rainfall FS")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Initiating event rainfall FS")')
	close(100)    
!
!
!   Parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,iZone,h_z) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	do j=1,my
		do i=1,mx
!
!
!           Static cell values
            iZone = zones(i,j)
!
!           Check null
            IF (iZone .NE. nodata) THEN
!            
!			    Water table depth rising (rainfall in mm), 0.3 = porosity
    		    h_wt(i,j) = h_wt(i,j) + rainfall(i,j) / 1000.d0 / 0.3d0
!
            ELSE
                h_wt(i,j) = nodata
            ENDIF
!	
!
    enddo
!
    enddo
!
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!			
!    
!   Update FS parameters
    CALL UpdateFsGaussian()
    
!   Comput probability of failure
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
                FSGrid(i,j) = NormalCDF(normMean);
!
            ELSE
                FSGrid(i,j) = nodata
            ENDIF
!
        enddo
    enddo
!    
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!    
!   Write results
    CALL WriteGrid(FSGrid, './res/PROB_failure_final_cond.txt')
!
!
!	Log file
    write(6,'("Computed probability of failure under antecedent rainfall")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed probability of failure under antecedent rainfall")')
	close(100)
!
!
end subroutine
