!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module fslamGlobals_structures
!
	implicit double precision (a-h,o-z)
!
!
!	Structures
    TYPE :: SoilProperties
		REAL*8	:: Ksmin
		REAL*8	:: Ksmax
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
        character(len=1) :: hsg
    END TYPE SoilProperties
!
!
!	Structures
    TYPE :: LandUseProperties
		REAL*8	:: Crmin
		REAL*8	:: Crmax
		REAL*8	:: A
		REAL*8	:: B
		REAL*8	:: C
		REAL*8	:: D
    END TYPE LandUseProperties
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
!	Parameters of auxiliar data
	REAL*8, PARAMETER :: PI		= 3.1415926d0
	REAL*8, PARAMETER :: D_R	= 57.295780d0
	REAL*8, PARAMETER :: R_D	= 0.0174532d0
	REAL*8, PARAMETER :: grav		= 9.81d0
!
!
end module fslamGlobals_structures
