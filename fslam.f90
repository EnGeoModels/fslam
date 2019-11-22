!  fslam.f90 
!
!  FUNCTIONS:
!	fslam      - Entry point of console application.
!
!
!	Read dem
!	Read data input
!	Read soil zones raster
!	Read soil properties
!	Read landuse raster
!	Read landuse properties
!	Fill sinks
!	Compute slopes
!	Compute flow accumulation
!   Compute stability
!

!****************************************************************************
!
!  PROGRAM: fslam
!
!  PURPOSE:  Fast Shallow Landslide Assessment Model
!
!****************************************************************************

program fslam
!
!	Variables globales
	use fslamGlobals_structures
	use fslamGlobals_shared
	use fslamGlobals_private
!
	implicit double precision (a-h,o-z)
!
!	Log file
	open(unit=100,file='./res/Log.txt',status='replace')
	close(100)
!
!
!	Printout
	write(6,'("*******************************************************")')
	write(6,'("Probabilistic Safety Factor model for shallow landslide")')
	write(6,'("*******************************************************",/)')
!
!
!	Read input data
	call LecDat()
!
!	Read topo
	write(6,'("Read topo...",/)')
	call LecTopo()  
!
!	Backup initial DEM values
	topo = topoIni    
!
!	Fill sinks process
	write(6,'("Geometry preprocessing...",/)')
	write(6,'("Fill sinks...",/)')
	call FillSinksCalc()
!
!	Compute slopes
	write(6,'("Compute slopes...",/)')
	call SlopeCalc()
!
!	Compute cum area
	write(6,'("Compute flow accumulation...",/)')
	call CumFlowCalc()
!
!	Geoprocesses results output Arcview
    if (iOutput .EQ. 1) THEN
	    write(6,'(/,"GIS Results output:",/)')
	    call GridOut()
    else
	    write(6,'(/,"No GIS Results output:",/)')
    endif
!
!	Read rainfall raster
	write(6,'("Read rainfall...",/)')
	call LecRainfall()
!
!	Read rainfall raster
	write(6,'("Read antecedent rainfall...",/)')
	call LecRainfallAnt() 
!
!	Zones read
	write(6,'("Read zones...",/)')
	call LecZones()
!
!	Read soil data
	write(6,'("Read soils data...",/)')
	call LecZonesDat()
!
!	Zones read
	write(6,'("Read zones...",/)')
	call LecLandUse()
!
!	Read soil data
	write(6,'("Read soils data...",/)')
	call LecLandUsesDat()
!
!	Compute Curve Number
	write(6,'("Compute CN...",/)')    
	call ComputeCN()
!
!   Compute soil data Gaussian parameters
	write(6,'("Compute soils data Gaussian...",/)')
    call GaussianParams()
!
!   Compute infiltration rainfall
    write(6,'("Compute infiltrated rainfall...",/)')
    call Hydrology()
!    
!   Write infiltration results
    CALL WriteGrid(Infiltration, './res/Infiltration.asc')
!
!	Unconditionally instable cells
	write(6,'("Inconditionally unstable cells calculation...",/)')
	call IncondUnst()
!
!	Unconditionally stable cells
	write(6,'("Inconditionally stable cells calculation...",/)')
	call IncondSta()
!
!   Antecedent rainfall failure probability
	write(6,'("Antecent rainfall condition...",/)')
	call InitialSaturation()
!    
!   Write initial saturation degree results
    CALL WriteGrid(h_z, './res/initial_h_z.asc')
!    
!   Write probability of failure under antecedent rainfall
    CALL WriteGrid(PFGrid, './res/PROB_failure_initial_cond.asc')
    CALL WriteGrid(FS_mu, './res/SF_initial_cond.asc')    
!
!   Postevent failure probability
	write(6,'("After event stability computation...",/)')
	call FinalSaturation()
!    
!   Write post event results
    CALL WriteGrid(PFGrid, './res/PROB_failure_final_cond.asc')
    CALL WriteGrid(FS_mu, './res/SF_final_cond.asc')
!
!   Compute histogram
	write(6,'("Compute results histogram...",/)')
	call Histogram('./res/PROB_FAILURE_HIST.csv')
!
!
!   -----------------------------------------------------
!   Apply climate change factor to the 24hr precipitation
!   -----------------------------------------------------
    write(6,'("Apply climate change rainfall factor...",/)')
    Rainfall = climateChangeFactor * Rainfall
!
!
!   Recompute initial watertable position
	call InitialSaturation()
!
!   Compute infiltration rainfall for the climate change
    write(6,'("Compute infiltrated rainfall considering climate change...",/)')
    call Hydrology()
!    
!   Write results
    CALL WriteGrid(Infiltration, './res/Infiltration_CC.asc')
!
!   Postevent failure probability considering climate change
	write(6,'("After event stability computation including climate change...",/)')
	call FinalSaturation()
!    
!   Write post event results
    CALL WriteGrid(PFGrid, './res/PROB_failure_final_cond_CC.asc')
    CALL WriteGrid(FS_mu, './res/SF_final_cond_CC.asc')
!
!   ---------------------
!   For research purposes
!   ---------------------
    IF (iResearch .EQ. 1) THEN
!
!       Principal components of the safety factor
	    write(6,'("Divide FS in its shape functions...",/)')
	    call FsComponents()    
!    
!       Write FS components results
        CALL WriteGrid(FS_C1, './res/SF_static.asc')
        CALL WriteGrid(FS_C2, './res/SF_antecedent.asc')
        CALL WriteGrid(FS_C3, './res/SF_event.asc')
!
    END IF
!
!   Compute histogram
	write(6,'("Compute results histogram for climate change...",/)')
	call Histogram('./res/PROB_FAILURE_HIST_CC.csv')    
!
!
!	Liberamos memoria
	DEALLOCATE(topo)
	DEALLOCATE(topoIni)
	DEALLOCATE(cumflow)
	DEALLOCATE(auxcumflow)
	DEALLOCATE(zones)
    DEALLOCATE(lulc)
	DEALLOCATE(slopeGrid)
	DEALLOCATE(PFGrid)
    DEALLOCATE(Soils)
    DEALLOCATE(LandUses)
	DEALLOCATE(GaussKs)
	DEALLOCATE(GaussC)
    DEALLOCATE(GaussCr)
	DEALLOCATE(Gaussphi)
	DEALLOCATE(Gaussh)
	DEALLOCATE(GaussPor)
	DEALLOCATE(GaussDens)
	DEALLOCATE(Rainfall)
	DEALLOCATE(Rainfall_ant)    
    DEALLOCATE(h_z)
    DEALLOCATE(h_wt)
    DEALLOCATE(Infiltration)
	DEALLOCATE(FS_C1)
	DEALLOCATE(FS_C2)
	DEALLOCATE(FS_C3)
	DEALLOCATE(CNGrid)    
!
!
!
end program fslam

