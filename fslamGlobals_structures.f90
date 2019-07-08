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

module fslamGlobals_structures
!
	implicit double precision (a-h,o-z)
!
!
!
!	Parameters of auxiliar data
	REAL*8, PARAMETER :: PI		 = 3.1415926d0
	REAL*8, PARAMETER :: D_R	 = 57.295780d0
	REAL*8, PARAMETER :: R_D	 = 0.0174532d0
	REAL*8, PARAMETER :: grav    = 9.81d0
    INTEGER,PARAMETER :: nVariables  = 50      !Number of intervals in VariablesMatrix variables
    
!***********************************************
    
    
    INTEGER,PARAMETER :: nParams = 10           !Number of intervals in ParameterMatrix parameters
    REAL*8, PARAMETER :: a_b_min = 0.d0         !Minimum area to cell width ratio
    REAL*8, PARAMETER :: a_b_max = 7000.d0      !Maximum area to cell width ratio
    REAL*8, PARAMETER :: slope_min = 0.d0       !Minimum slope
    REAL*8, PARAMETER :: slope_max = 1.570795d0 !Maximum slope
    REAL*8, PARAMETER :: qe_min = 0.d0 / 1000.d0         !Minimum event infiltration (m)
    REAL*8, PARAMETER :: qe_max = 350.d0 / 1000.d0       !Maximum event infiltration (m)
!
!
!	Structures
    TYPE :: SoilProperties
		REAL*8	:: kvmin
		REAL*8	:: kvmax
		REAL*8	:: khmin
		REAL*8	:: khmax
		REAL*8	:: Diffmin
		REAL*8	:: Diffmax
		REAL*8	:: Cmin
		REAL*8	:: Cmax
		REAL*8	:: phimin
		REAL*8	:: phimax
		REAL*8	:: hmin
		REAL*8	:: hmax
		REAL*8	:: densmin
		REAL*8	:: densmax
        REAL*8  :: porositymin
        REAL*8  :: porositymax
        REAL*8  :: zmin
        REAL*8  :: zmax
    END TYPE SoilProperties
!
!
!	Structures
    TYPE :: VariablesMatrix
       	REAL*8, DIMENSION(nVariables) :: Slope			                !Slope angle (rad)
    	REAL*8, DIMENSION(nVariables) :: a_b				            !Area to cell size ratio
    	REAL*8, DIMENSION(nVariables) :: Infiltration	                !Infiltration
        REAL*8, DIMENSION(nVariables,nVariables,nVariables) :: SF       !SF failure matrix
        REAL*8, DIMENSION(nVariables,nVariables,nVariables) :: Prob     !Probability of failure matrix
    END TYPE VariablesMatrix
!
!
!	Structures
    TYPE :: ParameterMatrix
    	REAL*8, DIMENSION(nParams) :: kh            !Hydraulic conductivity
       	REAL*8, DIMENSION(nParams) :: z             !Soil depth
    	REAL*8, DIMENSION(nParams) :: C	            !Cohesion
    	REAL*8, DIMENSION(nParams) :: phi           !Internal friction angle
       	REAL*8, DIMENSION(nParams) :: porosity      !Porosity
    END TYPE ParameterMatrix
!
!
!	Structures
    TYPE :: RandomCellType
		REAL*8	:: Area
		REAL*8	:: Slope
		INTEGER	:: Zone
    END TYPE RandomCellType
!
!
!
!	Structures
    TYPE :: GaussianProperties
		REAL*8	:: mean
		REAL*8	:: stdde
    END TYPE GaussianProperties
!
!
!
end module fslamGlobals_structures
