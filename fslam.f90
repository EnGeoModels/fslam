!  fslam.f90 
!
!  FUNCTIONS:
!	fslam      - Entry point of console application.
!
!
!	Leemos la topografia
!	Leemos el fichero de datos
!	Leemos zonas
!	Leemos propiedades suelo zonas
!	Fill sinks
!	Calculamos pendientes
!	Calculamos acumulado
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
!	Read rainfall raster
	write(6,'("Read rainfall...",/)')
	call LecRainfall() 
!
!	Read rainfall raster
	write(6,'("Read p0...",/)')    
	call LecP0()
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
!	Salida de resultados mediante GRID Arcview
    if (iOutput .EQ. 1) THEN
	    write(6,'(/,"GIS Results output:",/)')
	    call GridOut()
    else
	    write(6,'(/,"No GIS Results output:",/)')
    endif
!
!	Zones read
	write(6,'("Read zones...",/)')
	call LecZones()
!
!	Read soil data
	write(6,'("Read soils data...",/)')
	call LecZonesDat()
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
    CALL WriteGrid(FSGrid, './res/PROB_failure_initial_cond.asc')
!
!   Postevent failure probability
	write(6,'("After event stability computation...",/)')
	call FinalSaturation()
!    
!   Write post event results
    CALL WriteGrid(FSGrid, './res/PROB_failure_final_cond.asc')
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
    CALL WriteGrid(FSGrid, './res/PROB_failure_final_cond_CC.asc')
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
	DEALLOCATE(slopeGrid)
	DEALLOCATE(FSGrid)
	DEALLOCATE(Soils)
	DEALLOCATE(Gausskv)
	DEALLOCATE(Gausskh)
	DEALLOCATE(GaussC)
	DEALLOCATE(Gaussphi)
	DEALLOCATE(Gaussh)
	DEALLOCATE(GaussDiff)
	DEALLOCATE(GaussDens)
	DEALLOCATE(AntRainVec)
	DEALLOCATE(Rainfall)
    DEALLOCATE(h_z)
    DEALLOCATE(h_wt)
    DEALLOCATE(Infiltration)
!
!
!
end program fslam

