!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module fslamGlobals_shared
!
!	Variables globales
	use fslamGlobals_structures
!
	implicit double precision (a-h,o-z)
!
!
!   Global data
!
!
!   Enteros
	INTEGER :: itipo		!Coordenadas o malla
	INTEGER :: imaxpend		!Calculo de la maxima pendiente
	INTEGER :: iFricType	!Frictional(1) or viscous(2)
	INTEGER :: mx			!Numero nodos X
	INTEGER :: my			!Numero nodos Y
	INTEGER :: numberZones	!Number of soil zones
	INTEGER :: numberLandUses	!Number of soil zones
    INTEGER :: iOutput      !Create GIS results
    INTEGER :: istat        !iostat variable
    INTEGER :: iResearch    !Output all the results (safety factor and probability of failure)
    INTEGER :: iPROB_uncond_stable          !Create PROB_uncond_stable.asc raster
    INTEGER :: iPROB_uncond_unst            !Create PROB_uncond_unst.asc raster
    INTEGER :: iInfiltration                !Create Infiltration.asc raster
    INTEGER :: iInitial_h_z                 !Create Initial_h_z.asc raster
    INTEGER :: iPROB_failure_initial_cond   !Create PROB_failure_initial_cond.asc raster
    INTEGER :: iPROB_failure_final_cond     !Create PROB_failure_final_cond.asc raster
    INTEGER :: iSF_initial_cond             !Create SF_initial_cond.asc raster
    INTEGER :: iSF_final_cond               !Create SF_final_cond.asc raster
    INTEGER :: iRunoff      !Compute runoff
!
!
!   Booleanas
	LOGICAL :: Stopping
!
!   Reales
	REAL*8 :: nodata		!Nodata ESRI
    REAL*8  :: xcorner		!Coordenada X esquina superior izq.
	REAL*8  :: ycorner		!Coordenada X esquina superior izq.
	REAL*8  :: dx			!Delta X
	REAL*8  :: dy			!Delta Y
	REAL*8	:: densw		!Water density
    REAL*8	:: climateChangeFactor  !Scaling CC factor for 24hr precipitation
!
!
!   Arrays
!
!   Enteros
	INTEGER,  DIMENSION(:,:),   ALLOCATABLE :: zones			            !Zones delimitation grid
	INTEGER,  DIMENSION(:,:),   ALLOCATABLE :: lulc                         !Landuse delimitation grid    
!
!   Reales
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: topo				            !Corrected topo grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: topoIni			            !Topo grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: slopeGrid		            !Slopes grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: cumflow			            !Cumflow grid
	REAL*8,   DIMENSION(:,:,:),	ALLOCATABLE :: auxcumflow		            !Auxiliar grid
    REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: PFGrid			            !Probability of failure
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: Rainfall_ant             !Ant rain intensity grid
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: WeightedRainfall_ant     !Average value of antecedent rainfall in cell contributing area
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: Rainfall                 !Event rain precipitation grid    
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: WeightedRainfall         !Average value of antecedent rainfall in cell contributing area       
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: CNGrid                   !Curve number
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: WeightedCN               !Average value of CN in cell contributing area
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: UncIns                   !Probability of unconditionally instable cells
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: UncEst                   !Probability of unconditionally stable cells
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_mu                    !FS mean
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_std                   !FS Standard deviation
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: h_wt                     !Water table    
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: Infiltration             !Infiltration
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: h_z                      !Saturation degree
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_C1                    !FS first component
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_C2                    !FS second component
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_C3                    !FS third component
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: RunOff                   !Rainfall runoff
    REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: WeightedSlope            !Average value of slope in cell contributing area 
!
!
!	Structures
	TYPE(SoilProperties),		DIMENSION(:),	ALLOCATABLE :: Soils	!Soils properties
	TYPE(LandUseProperties),    DIMENSION(:),	ALLOCATABLE :: LandUses	!Land use properties    
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussKs
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussC
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussphi
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussh
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussPor
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussDens
    TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussCr
!
!
!
!   Texto
	CHARACTER(LEN=14) dummy
	CHARACTER(LEN=128) fname
!
!
    INTERFACE
        SUBROUTINE WriteGrid(GridData, Filename)
            REAL*8, INTENT(IN)   :: GridData(:,:)
            CHARACTER Filename*(*)
        END SUBROUTINE WriteGrid
    END INTERFACE   
!
!
    INTERFACE
        SUBROUTINE Histogram(Filename)
            CHARACTER Filename*(*)
        END SUBROUTINE Histogram
    END INTERFACE   
!
!
end module fslamGlobals_shared
