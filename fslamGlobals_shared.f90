!****************************************************************************
!
!  fslam: Fast Shallow Landslide Assessment Model
!
!  Code developed in the  SMuCPhy project, founded by the Ministerio de 
!  Economia y Competitividad del Gobierno de España and coordinated by UPC
!  BarcelonaTECH
!
!  Coordinated by Department of Civil and Environmental Engineering
!
!****************************************************************************
!
!
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
	INTEGER :: nodata		!Nodata ESRI
	INTEGER :: itipo		!Coordenadas o malla
	INTEGER :: imaxpend		!Calculo de la maxima pendiente
	INTEGER :: iFricType	!Frictional(1) or viscous(2)
	INTEGER :: mx			!Numero nodos X
	INTEGER :: my			!Numero nodos Y
	INTEGER :: numberZones	!Number of soil zones
	INTEGER :: iter			!Numero de iteraciones Montecarlo
	INTEGER	:: iCells		!Number of cells to compute
	INTEGER :: nAntRain		!Nummer of antecedent rains
	INTEGER :: nIntervals	!Number of duration intervals
	INTEGER :: nRainInc		!Number of rain increments
	INTEGER :: LevelsNumber	!Number of TRIGRS levels
	INTEGER :: mmax			!Number of terms in the finite depth infiltration eq.
	INTEGER :: iOutput      !Create GIS results
    INTEGER :: istat        !iostat variable
    INTEGER :: iStochastic  !iStochastic select between analytic probability or fully stochastic soil properties
!
!
!   Booleanas
	LOGICAL :: Stopping
!
!   Reales
	REAL*8  :: xcorner		!Coordenada X esquina superior izq.
	REAL*8  :: ycorner		!Coordenada X esquina superior izq.
	REAL*8  :: dx			!Delta X
	REAL*8  :: dy			!Delta Y
	REAL*8	:: densw		!Water density
	REAL*8	:: MaxAntRain	!Maximum antecedent rain
	REAL*8	:: MaxDuration	!Maximum event duration
	REAL*8	:: MaxRain		!Maximum event rain
	REAL*8	:: MinAntRain	!Minimum antecedent rain
	REAL*8	:: MinDuration	!Minimum event duration
	REAL*8	:: MinRain		!Minimum event rain
	REAL*8	:: AntRainInten	!Antecedent rainfall intensity
	REAL*8	:: RainIntensity !Event rainfall intensity
    REAL*8	:: climateChangeFactor  !Scaling CC factor for 24hr precipitation
!
!
!   Arrays
!
!   Enteros
	INTEGER,  DIMENSION(:,:),   ALLOCATABLE :: zones			!Zones delimitation grid
!
!   Reales
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: topo				!Corrected topo grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: topoIni			!Topo grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: slopeGrid		!Slopes grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: cumflow			!Cumflow grid
	REAL*8,   DIMENSION(:,:,:),	ALLOCATABLE :: auxcumflow		!Auxiliar grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: FSGrid			!Safety factor
	REAL*8,   DIMENSION(:),		ALLOCATABLE :: AntRainVec		!Vector of antecedent rainfall intensity
	REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: Rainfall     !Event rain precipitation grid
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: p0Grid       !Runoff threshold
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: UncIns       !Probability of unconditionally instable cells
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: UncEst       !Probability of unconditionally stable cells
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: FS_mu        !FS mean
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: FS_std       !FS Standard deviation
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: h_wt         !Water table    
	REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: Infiltration !Infiltration   
    REAL*8,   DIMENSION(:,:),	ALLOCATABLE :: h_z          !Saturation degree
    REAL*8,   DIMENSION(:),     ALLOCATABLE :: VectParams   !Vector to create gaussian distribution
    REAL*8,   DIMENSION(:),     ALLOCATABLE :: VectVar      !Vector to create gaussian distribution
    REAL*8,   DIMENSION(:,:,:), ALLOCATABLE :: AuxMat       !Auxiliar matrix to improve code performance
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: AuxSF        !Auxiliar matrix to improve code performance
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: AuxProb      !Auxiliar matrix to improve code performance
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: C_term
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: aux_term
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: sum_term
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: inf_term
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: tan_term
    REAL*8,   DIMENSION(:,:),   ALLOCATABLE :: por_term

!
!
!	Booleans
	LOGICAL,  DIMENSION(:,:),   ALLOCATABLE :: Unconditional	!unconditionally stab-unst

!
!	Structures
	TYPE(SoilProperties),		DIMENSION(:),	ALLOCATABLE :: Soils	!Soils properties
	TYPE(VariablesMatrix),		DIMENSION(:),	ALLOCATABLE :: SoilsVariablesMatrix    !Soils stability matrix
	TYPE(ParameterMatrix),      DIMENSION(:),	ALLOCATABLE :: SoilsParameterMatrix  !Soils parameter matrix
    TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gausskv
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gausskh
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussC
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussphi
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussz
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussDiff
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussDens
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussPor
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
