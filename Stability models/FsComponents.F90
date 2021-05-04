!
!
!****************************************************************************
!
!  SUBROUTINE: UpdateFsGaussian
!
!  PURPOSE:  Compute FS principal components.
!
!****************************************************************************
subroutine FsComponents()
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
!   Main parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j) 
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
            IF ((iZone .NE. INT(nodata)) .AND. (iLandUse .NE. INT(nodata))) THEN
!            
			    Slope = slopeGrid(i,j)
                Zmax = Gaussh(iZone)%mean
                denss = GaussDens(iZone)%mean
                phi = DATAN(GaussTanPhi(iZone)%mean)
                Area = cumflow(i,j)
                ksh = GaussKs(iZone)%mean
                Porosity = GaussPor(iZone)%mean
                Cohesion = GaussC(iZone)%mean + GaussCr(iLandUse)%mean
                AntRainInten = WeightedRainfall_ant(i,j) / 1000.d0 / (24.d0 * 3600.d0)
!
!               First component
                FS_C1(i,j) =  DMIN1(Cohesion / (Zmax * DCOS(Slope) * denss * grav * DSIN(Slope)) +  DTAN(phi) / DTAN(Slope), 10.d0)
!
!               Second component
                FS_C2(i,j) = -DMIN1(DMIN1(AntRainInten * Area / (ksh * Zmax * dx * DSIN(Slope) * DCOS(Slope)), 1.d0) * (densw / denss) * (DTAN(phi) / DTAN(Slope)), 10.d0)
!
!               Third component
                FS_C3(i,j) = -DMIN1(DMIN1((Infiltration(i,j) / 1000.d0) * 1.d0 / (Porosity * Zmax), 1.d0) * (densw / denss) * (DTAN(phi) / DTAN(Slope)), 10.d0)
!            
            ELSE
                FS_C1(i,j) = nodata
                FS_C2(i,j) = nodata
                FS_C3(i,j) = nodata
            ENDIF
!
		ENDDO
    ENDDO
!
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!
!
!	Log file
    write(6,'("Computed FS principal components")')
	open(unit=100,file=(trim(fname_res) // '\Log.txt'),access='append')
	write(100,'("Computed FS principal components")')
	close(100)
!
end subroutine