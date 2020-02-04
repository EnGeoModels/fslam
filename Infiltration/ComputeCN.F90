!
!
!****************************************************************************
!
!  SUBROUTINE: ComputeCN
!
!  PURPOSE:  Computes Curve Number combining soil and landuse.
!
!****************************************************************************
subroutine ComputeCN()
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
!	FS for initial saturation
!
!	Variables
    character(len=1) :: hsg                     !Soil group
!
    write(6,'("Combine soil data and landuse into CN")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Combine soil data and landuse into CN")')
	close(100)    
!
!
!   Parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,hsg) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
	do j=1,my
		do i=1,mx
!
!           Static cell values
            iZone = zones(i,j)
			iLandUse = lulc(i,j)
!
!           Check null
            IF ((iZone .NE. nodata) .AND. (iLandUse .NE. nodata)) THEN
!
!				Get the soil group
				hsg = Soils(iZone)%hsg
!
!				Combine with land use data
				SELECT CASE (hsg)
					CASE ("A")
						CNGrid(i,j) = LandUses(iLandUse)%A
					CASE ("B")
						CNGrid(i,j) = LandUses(iLandUse)%B
					CASE ("C")
						CNGrid(i,j) = LandUses(iLandUse)%C
					CASE ("D")
						CNGrid(i,j) = LandUses(iLandUse)%D
					CASE DEFAULT
						WRITE(6,100) hsg
				END SELECT
!
            ELSE
                CNGrid(i,j) = nodata
            ENDIF
!	
!
        enddo
!
    enddo
!
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!
!
!	Log file
    write(6,'("Computed Curve Number")')
	open(unit=100,file='./res/Log.txt',access='append')
	write(100,'("Computed Curve Number")')
	close(100)
!   
!	Formats
100  format('Landuse data file error. Unknown soil group: ',A5,/)
!  
end subroutine
