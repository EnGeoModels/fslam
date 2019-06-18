!
!
!****************************************************************************
!
!  SUBROUTINE: Hydrology
!
!  PURPOSE:  Compute effective rainfall and infiltration.
!
!****************************************************************************
subroutine Hydrology()
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
!
!	Unconditionally unstable cells
!
!	Variables
    REAL*8  :: normMean
    REAL*8  :: NormalCDF
!
!   
!   Compute probability of failure
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,normMean) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	DO j=1,my
		DO i=1,mx
!
!           Static cell values
            iZone = zones(i,j)
!
!           Check null
            IF (iZone .NE. nodata) THEN
!
!               Get porosity
                p0 = p0Grid(i,j)
!
!			    Infiltrated rainfall (SCS-CN model)       
    		    IF (Rainfall(i,j) .GT. p0) THEN
                    Infiltration(i,j) = Rainfall(i,j) - (Rainfall(i,j) - p0)**2.d0 / (Rainfall(i,j) + 4.d0 * p0)
                ELSE
                    Infiltration(i,j) = Rainfall(i,j)
                ENDIF
!
            ELSE
                Infiltration(i,j) = nodata
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
    write(6,'("Computed effective infiltration")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed effective infiltration")')
	close(100)

end subroutine
