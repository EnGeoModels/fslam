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
!   Antecedent rainfall
    AntRainInten = AntRainVec(1)
!
!   Units conversion
    AntRainInten = AntRainInten
!
!
    write(6,'("Initiating antecedent rainfall ",F5.2," mm/day")') (1000.d0*24.d0*3600.d0*AntRainInten)
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Initiating antecedent rainfall ",F5.2," mm/day")') (1000.d0*24.d0*3600.d0*AntRainInten)
	close(100)    
    
    

!
!   Parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,iZone) 
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
                Slope = slopeGrid(i,j)
                Zmax = Gaussh(iZone)%mean
                denss = GaussDens(iZone)%mean
                Area = cumflow(i,j)
                ksh = Gausskh(iZone)%mean
!
!			    Infinite slope stability
			    h_z(i,j) = DMIN1(AntRainInten * Area / ( ksh * Zmax * dx * DSIN(Slope) ), 1.d0)
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
!   Write results
    CALL WriteGrid(h_z, './res/initial_h_z.asc')
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
                FSGrid(i,j) = NormalCDF(normMean)
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
    CALL WriteGrid(FSGrid, './res/PROB_failure_initial_cond.asc')
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
