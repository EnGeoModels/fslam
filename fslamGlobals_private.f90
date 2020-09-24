!****************************************************************************
!  Global data, parameters, and structures 
!****************************************************************************

module fslamGlobals_private
!
	implicit double precision (a-h,o-z)
!
!   Reales
	REAL*8	:: denss		!Solid density
	REAL*8	:: Slope		!Cell slope
	REAL*8	:: Cohesion		!Cohesion
	REAL*8	:: ksv			!Vertical hydraulic conductivity
	REAL*8	:: ksh			!horizontal hydraulic conductivity
	REAL*8	:: Diff			!Pressure difusivity
	REAL*8	:: Zmax			!Soil depth
	REAL*8	:: phi			!Internal friction angle
	REAL*8	:: Area			!Contributing area
    REAL*8  :: Porosity     !Soil porosity
    REAL*8  :: AntRainInten !Antecedent rainfall intensity
    REAL*8  :: CN           !Curve number    
    REAL*8  :: Ia           !Infiltration threshold
    REAL*8  :: Tc           !Concentration time
    REAL*8  :: Ie           !Event rainfall intensity
    REAL*8  :: Pd_e           !Event precipitation
    REAL*8  :: ROcoeff      !Runoff coefficient
    REAL*8  :: UpsLength    !Longest path length
    INTEGER :: iZone        !Cell soil type
    INTEGER :: iLandUse     !Landuse type    
!
!$OMP THREADPRIVATE(denss,Slope,Cohesion,ksv,ksh,Diff,Zmax,Zinc,phi,Area,Porosity,CN,Ia,UpsLength,iZone,AntRainInten,iLandUse)
!
!
end module fslamGlobals_private
