!
!
!****************************************************************************
!
!  SUBROUTINE: UpdateFsGaussian
!
!  PURPOSE:  Compute FS Gaussian.
!
!****************************************************************************
subroutine UpdateFsGaussian()
!
!
!
!	Variables globales
	use fslamGlobals_structures
	use fslamGlobals_shared
	use fslamGlobals_private
!
!
	implicit double precision (a-h,o-z)
!
!
!	Explicit variables
    REAL*8  :: A		!A parameter
    REAL*8  :: D		!D parameter
!
!   Main parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,WT,A,D) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	DO j=1,my
		DO i=1,mx
!
!			Static cell values
			iZone = zones(i,j)
			iLandUse = lulc(i,j)            
!
!           Check null
            IF ((iZone .NE. nodata) .AND. (iLandUse .NE. nodata)) THEN
!            
			    Slope = slopeGrid(i,j)
                Zmax = Gaussh(iZone)%mean
                denss = GaussDens(iZone)%mean
                phi = Gaussphi(iZone)%mean
                Cohesion = GaussC(iZone)%mean + GaussCr(iLandUse)%mean 
                WT = h_wt(i,j)
!
!               Compute auxiliar
                A = Zmax * DCOS(Slope) * denss * grav * DSIN(Slope)
                D = Zmax * denss * grav * DSIN(Slope) / (Zmax * DCOS(Slope) * denss * grav - Zmax * DCOS(Slope) * (WT/Zmax) * densw * grav)
!
!               Update soil gaussian parameters
                FS_mu(i,j) = DTAN(phi) / D + Cohesion / A
                FS_std(i,j) = DSQRT(A**2 * DTAN(Gaussphi(iZone)%stdde)**2 + D**2 * (GaussC(iZone)%stdde**2 + GaussCr(iLandUse)%stdde**2)) / (D*A)
!            
            ELSE
                FS_mu(i,j) = nodata
                FS_std(i,j) = nodata            
            ENDIF
!
		ENDDO
    ENDDO
!
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!
!
end subroutine