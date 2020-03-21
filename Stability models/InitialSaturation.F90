!
!
!****************************************************************************
!
!  SUBROUTINE: InitialSaturation
!
!  PURPOSE:  Compute unconditionally instable cells.
!
!****************************************************************************
subroutine InitialSaturation()
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
!
    write(6,'("Initiating antecedent rainfall")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Initiating antecedent rainfall")')
	close(100)    
!
!
!   Parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	do j=1,my
		do i=1,mx
!
!
!           Static cell values
			iZone = zones(i,j)
			iLandUse = lulc(i,j)            
!
!           Check null
            IF ((iZone .NE. INT(nodata)) .AND. (iLandUse .NE. INT(nodata))) THEN
!            
                Slope = slopeGrid(i,j)
                Zmax = Gaussh(iZone)%mean
                denss = GaussDens(iZone)%mean
                Area = cumflow(i,j)
                ksh = GaussKs(iZone)%mean
                AntRainInten = Rainfall_ant(i,j) / 1000.d0 / (24.d0 * 3600.d0)
!
!			    Infinite slope stability
			    h_z(i,j) = DMIN1(AntRainInten * Area / ( ksh * Zmax * dx * DSIN(Slope) * DCOS(Slope) ), 1.d0)
!
!			    Water table depth
    		    h_wt(i,j) = h_z(i,j) * Zmax
!
            ELSE
                h_wt(i,j) = nodata
                h_z(i,j) = nodata
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
                PFGrid(i,j) = NormalCDF(normMean)
!                
            ELSE
                PFGrid(i,j) = nodata
            ENDIF
!
        enddo
    enddo
!    
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
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
