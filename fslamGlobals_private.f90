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
    REAL*8  :: p0           !Infiltration threshold
!
!$OMP THREADPRIVATE(denss,Slope,Cohesion,ksv,ksh,Diff,Zmax,Zinc,phi,Area,Porosity,p0)
!
!
end module fslamGlobals_private
