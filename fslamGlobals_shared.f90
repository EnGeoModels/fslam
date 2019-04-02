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
	INTEGER :: iSolution    !1 Iverson (2000), 2 Savage (TRIGRS v2)
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
	REAL*8	:: Zmin			!Minimum Z for TRIGRS
	REAL*8	:: AntRainInten	!Antecedent rainfall intensity
	REAL*8	:: RainIntensity !Event rainfall intensity
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
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: Rainfall     !Event rain precipitation grid
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: UncIns       !Probability of unconditionally instable cells
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: UncEst       !Probability of unconditionally stable cells
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_mu        !FS mean
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: FS_std       !FS Standard deviation
	REAL*8,   DIMENSION(:,:),		ALLOCATABLE :: h_wt       	    !Water table    
!
!
!	Booleans
	LOGICAL,  DIMENSION(:,:),   ALLOCATABLE :: Unconditional	!unconditionally stab-unst

!
!	Structures
	TYPE(SoilProperties),		DIMENSION(:),	ALLOCATABLE :: Soils	!Soils properties
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gausskv
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gausskh
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussC
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussphi
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: Gaussh
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussDiff
	TYPE(GaussianProperties),	DIMENSION(:),	ALLOCATABLE :: GaussDens
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
end module fslamGlobals_shared
