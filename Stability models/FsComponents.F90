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
!
!           Check null
            IF (iZone .NE. nodata) THEN
!            
			    Slope = slopeGrid(i,j)
                Zmax = Gaussh(iZone)%mean
                denss = GaussDens(iZone)%mean
                Area = cumflow(i,j)
                ksh = Gausskh(iZone)%mean
                Porosity = Soils(iZone)%porosity
!
!               First component
                FS_C1(i,j) =  DMIN1(GaussC(iZone)%mean / (Zmax * DCOS(Slope) * denss * grav * DSIN(Slope)) +  DTAN(Gaussphi(iZone)%mean) / DTAN(Slope), 10.d0)
!
!               Second component
                FS_C2(i,j) = -DMIN1(DMIN1(Area / (ksh * Zmax * dx * DSIN(Slope) * DCOS(Slope)), 1.d0) * (densw / denss) * (DTAN(Gaussphi(iZone)%mean) / DTAN(Slope)), 10.d0)
!
!               First component
                FS_C3(i,j) = -DMIN1(DMIN1(1.d0 / (Porosity * Zmax), 1.d0) * (densw / denss) * (DTAN(Gaussphi(iZone)%mean) / DTAN(Slope)), 10.d0)
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
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed FS principal components")')
	close(100)
!
end subroutine