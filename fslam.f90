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
!
!	Printout
	write(6,'("*******************************************************")')
	write(6,'("Probabilistic Safety Factor model for shallow landslide")')
	write(6,'("*******************************************************",/)')
!
!   Read input filenames
    call LecFilenames()
!
!	Log file
	open(unit=100,file=(trim(fname_res) // '\\Log.txt'),status='replace')
	close(100)
!
!	Read input data
	call LecDat()
!
!	Read topo
	write(6,'("------------------------------------")')
	write(6,'("Read dem.asc...",/)')
	call LecTopo()  
!
!	Backup initial DEM values
	topo = topoIni    
!
!	Fill sinks process
	write(6,'("Geometry preprocessing...",/)')
	write(6,'("Fill sinks...")')
	call FillSinksCalc()
!
!	Compute slopes
	write(6,'("Compute slopes...")')
	call SlopeCalc()
!
!	Compute cum area
	write(6,'("Compute flow accumulation...",/)')
	call CumFlowCalc(topo, cumflow, Dinf)
!
!	Read rainfall raster
	write(6,'("------------------------------------")')
    write(6,'("Read rain_event.asc...",/)')
	call LecRainfall()
!
!	Read rainfall raster
	write(6,'("------------------------------------")')
	write(6,'("Read antecedent rain_ant.asc...",/)')
	call LecRainfallAnt() 
!
!	Zones read
	write(6,'("------------------------------------")')
	write(6,'("Read soils.asc...",/)')
	call LecZones()
!
!	Read soil data
	write(6,'("Read soils data...",/)')
	call LecZonesDat()
!
!	Zones read
	write(6,'("------------------------------------")')
	write(6,'("Read land use lulc.asc...",/)')
	call LecLandUse()
!
!	Read soil data
	write(6,'("Read land use data...",/)')
	call LecLandUsesDat()
!
!	Compute Curve Number
	write(6,'("------------------------------------")')
	write(6,'("Compute CN...",/)')    
	call ComputeCN()
!
!	Compute averaged antedecent rainfall
	write(6,'("Compute weighted antecedent rainfall...",/)')
	call WeightedCumFlowCalc(topo, Rainfall_ant, WeightedRainfall_ant, Dinf, cumflow)
!
!	Compute averaged event rainfall
	write(6,'("Compute weighted rainfall...",/)')
	call WeightedCumFlowCalc(topo, Rainfall, WeightedRainfall, Dinf, cumflow)
!
!	Compute averaged CN
	write(6,'("Compute weighted CN...",/)')
	call WeightedCumFlowCalc(topo, CNGrid, WeightedCN, Dinf, cumflow)
!
!	Geoprocesses results output Arcview
    if (iOutput .EQ. 1) THEN
	    write(6,'(/,"GIS Results output:",/)')
	    call GridOut()
    else
	    write(6,'(/,"No GIS Results output:",/)')
    endif
!
!   Compute soil data Gaussian parameters
	write(6,'("------------------------------------")')
	write(6,'("Compute soils data Gaussian...",/)')
    call GaussianParams()
!
!   Compute infiltration rainfall
	write(6,'("------------------------------------")')
    write(6,'("Compute infiltrated rainfall...",/)')
    call Hydrology()
!    
!   Write infiltration results
    IF (iInfiltration .EQ. 1) CALL WriteGrid(Infiltration, 'Infiltration.asc')
!
!	Unconditionally instable cells
	write(6,'("------------------------------------")')
	write(6,'("Inconditionally unstable cells calculation...",/)')
	call IncondUnst()
!    
!   Write UU results
    if (iPROB_uncond_unst .EQ. 1) then
        call WriteGrid(UncIns, 'PROB_uncond_unst.asc')
        call Histogram(UncIns, 'PROB_uncond_unst_HIST.csv')
    endif
!
!	Unconditionally stable cells
	write(6,'("------------------------------------")')
	write(6,'("Inconditionally stable cells calculation...",/)')
	call IncondSta()
!    
!   Write US results
    if (iPROB_uncond_stable .EQ. 1) then
        call WriteGrid(UncEst, 'PROB_uncond_stable.asc')
        call Histogram(UncEst, 'PROB_uncond_stable_HIST.csv')
    endif
!
!   Antecedent rainfall failure probability
	write(6,'("------------------------------------")')
	write(6,'("Antecent rainfall condition...",/)')
	call InitialSaturation()
!    
!   Write initial saturation degree results
    IF (iInitial_h_z .EQ. 1) CALL WriteGrid(h_z, 'initial_h_z.asc')
!    
!   Write probability of failure under antecedent rainfall
    if (iPROB_failure_initial_cond .EQ. 1) then
        call WriteGrid(PFGrid, 'PROB_failure_initial_cond.asc')
        call Histogram(PFGrid, 'PROB_failure_initial_cond_HIST.csv')
    endif
!
    if (iSF_initial_cond .EQ. 1) then
        call WriteGrid(FS_mu, 'SF_initial_cond.asc')    
        call Histogram(FS_mu, 'SF_initial_cond_HIST.csv')
    endif
!
!   Postevent failure probability
	write(6,'("------------------------------------")')
	write(6,'("After event stability computation...",/)')
	call FinalSaturation()
!    
!   Write post event results
    if (iPROB_failure_final_cond .EQ. 1) then
        call WriteGrid(PFGrid, 'PROB_failure_final_cond.asc')
        call Histogram(PFGrid, 'PROB_failure_final_cond_HIST.csv')
    endif
!        
    if (iSF_final_cond .EQ. 1) then
        call WriteGrid(FS_mu, 'SF_final_cond.asc')
        call Histogram(FS_mu, 'SF_final_cond_HIST.csv')
    endif
!
!
!   ---------------------
!   For research purposes
!   ---------------------
    IF (iResearch .EQ. 1) THEN
!
!       Allocate memory
        ALLOCATE(FS_C1(mx,my))			        !FS first component	
	    ALLOCATE(FS_C2(mx,my))			        !FS second component
	    ALLOCATE(FS_C3(mx,my))			        !FS third component

        
!       Principal components of the safety factor
    	write(6,'("------------------------------------")')
	    write(6,'("Divide FS in its shape functions...",/)')
	    call FsComponents()    
!    
!       Write FS components results
        CALL WriteGrid(FS_C1, 'SF_static.asc')
        CALL WriteGrid(FS_C2, 'SF_antecedent.asc')
        CALL WriteGrid(FS_C3, 'SF_event.asc')
!
!       Free memory
	    DEALLOCATE(FS_C1)
	    DEALLOCATE(FS_C2)
	    DEALLOCATE(FS_C3)
!
    END IF
!
!
!   Free space required for runoff calculations
	DEALLOCATE(PFGrid)
    DEALLOCATE(h_z)
    DEALLOCATE(h_wt)
    DEALLOCATE(Infiltration)
    DEALLOCATE(WeightedRainfall_ant)
	DEALLOCATE(Rainfall_ant)    
	DEALLOCATE(topoIni)
!
!
!   ---------------------
!   Runoff module
!   ---------------------
    IF (iRunoff .EQ. 1) THEN
!
!       Compute runoff
    	write(6,'("------------------------------------")')
	    write(6,'("Compute event runoff...",/)')
!
!       Allocate memory for auxiliar rasters
        ALLOCATE(longestPath(mx,my))                !Longests upstream path	
	    ALLOCATE(maxElev(mx,my))			        !Maximum elevation of stream
!
!	    Compute cum area using D8
	    write(6,'("Compute flow accumulation using D8 algorithm...",/)')
	    call CumFlowCalc(topo, cumflow, D8)
!
!       Compute averaged event rainfall using D8
        write(6,'("Compute D8 weighted rainfall...",/)')
        call WeightedCumFlowCalc(topo, Rainfall, WeightedRainfall, D8, cumflow)
!
!	    Compute averaged CN using D8
        write(6,'("Compute D8 weighted CN...",/)')
        call WeightedCumFlowCalc(topo, CNGrid, WeightedCN, D8, cumflow)
!
!       Compute parameters for concentration time
        write(6,'("Compute longest path...",/)')
        call LongestPathCalc(topo, longestPath, maxElev)
!
!       Call runoff algorithm
        call RunOffCalc()    
!
!       Allocate memory for auxiliar rasters
        DEALLOCATE(longestPath)
	    DEALLOCATE(maxElev)
!
    END IF
!
!
!	Liberamos memoria
	DEALLOCATE(topo)
	DEALLOCATE(cumflow)
	DEALLOCATE(zones)
    DEALLOCATE(lulc)
	DEALLOCATE(slopeGrid)
    DEALLOCATE(Soils)
    DEALLOCATE(LandUses)
	DEALLOCATE(GaussKs)
	DEALLOCATE(GaussC)
    DEALLOCATE(GaussCr)
	DEALLOCATE(GaussTanPhi)
	DEALLOCATE(Gaussh)
	DEALLOCATE(GaussPor)
	DEALLOCATE(GaussDens)
	DEALLOCATE(Rainfall)
	DEALLOCATE(CNGrid)    
    DEALLOCATE(WeightedRainfall)    
    DEALLOCATE(WeightedCN)
!
!
!
end program fslam

