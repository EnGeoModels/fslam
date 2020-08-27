!
!
!****************************************************************************
!
!  SUBROUTINE: RunOffCalc
!
!  PURPOSE:  Compute effective rainfall and runoff.
!
!****************************************************************************
subroutine RunOffCalc()
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

!
!   Allocate memory for auxiliar raster
   	ALLOCATE(RunOff(mx,my))	        !Runoff values
    ALLOCATE(WeightedSlope(mx,my))  !Average slope in the area
!
!	Compute averaged slope
	write(6,'("RunOffCalc(): Compute weighted slope...",/)')
	call WeightedCumFlowCalc(topo, slopeGrid, WeightedSlope, D8, cumflow)
!   
!   Compute probability of failure
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,normMean) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	DO j=1,my
		DO i=1,mx
!
!           Get averaged area values
            CN = WeightedCN(i,j)
            Area = cumflow(i,j)
            Slope = DTAN(WeightedSlope(i,j))
            Pd_e = WeightedRainfall(i,j)
!
!           Check null
            IF (CN .NE. nodata) THEN
!
!               Convert CN into Ia (Mockus) [mm/day]
                Ia =  5080.d0 / DMAX1(CN, 1.d0) - 50.8d0
!
!               Compute concentration time (Temez) [hours]
                Tc = 0.3d0 * (SQRT(Area) / 1000.d0 / Slope**0.25d0 )**0.76d0
!
!               Minimum concentration time
                Tc = DMAX1(Tc, 0.0833d0)
!
!               Rainfall intensity [mm/hour]
                Ie = Pd_e / 24.d0 * 11.d0**((28.d0**0.1d0 - Tc**0.1d0) / (28.d0**0.1d0 - 1.d0))
!
!			    Infiltrated rainfall (SCS-CN model)       
    		    IF (Pd_e .GT. Ia) THEN
!
!                   Runoff coefficient
                    ROcoeff = (Pd_e - Ia) * (Pd_e + 23.d0 * Ia) / (Pd_e + 11.d0 * Ia)**2.d0
!
                    RunOff(i,j) = ROcoeff * Ie * Area / 1.d6 / 3.6d0
!
                ELSE
                    RunOff(i,j) = 0.d0
                ENDIF      
!
            ELSE
                RunOff(i,j) = nodata
            ENDIF
!
        ENDDO
    ENDDO
!    
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!
!
!	Log file
    write(6,'("Computed runoff")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed runoff")')
	close(100)
!
!   Write runoff results
    CALL WriteGrid(RunOff, './res/runoff.asc')
!
    DEALLOCATE(RunOff)
    DEALLOCATE(WeightedSlope)
!
end subroutine
