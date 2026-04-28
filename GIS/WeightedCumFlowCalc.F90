!
!
!****************************************************************************
!
!  SUBROUTINE: WeightedCumFlowCalc
!
!  PURPOSE:  Compute weigthed grid cumulative area.
!
!****************************************************************************
subroutine WeightedCumFlowCalc(inGridData, weightGridData, outGridData, algorithm, inCumflow)
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
!   Variables
    real*8 inGridData(mx,my)
    real*8 weightGridData(mx,my)
	real*8 outGridData(mx,my)
    real*8 inCumflow(mx,my)
	integer algorithm
!
!   Allocate memory for auxiliar raster
   	ALLOCATE(auxcumflow(mx,my,0:8))	        !Auxiliar grid
!
!	Matrix initialization
	outGridData = nodata
	auxcumflow = nodata

!	Flows computing
	do j = 2, (my-1)
		do i = 2, (mx-1)
            if (algorithm .EQ. Dinf) then
			    call doDInf(i, j, inGridData)
            else
                call doD8(i, j, inGridData)
            endif
		enddo
	end do
!
	do j = 2, (my-1)
		do i = 2, (mx-1)
            
            
!           Static cell values
            iZone = zones(i,j)
			iLandUse = lulc(i,j)
!
!           Check null
            if ((iZone .NE. INT(nodata)) .AND. (iLandUse .NE. INT(nodata))) then
!
!				Get the soil group
				accum_length = Soils(iZone)%accum_length
                n_accum_cells = accum_length / dx
!
!               Compute limited accum flow
                call WeightedGetFlowLim(i, j, inGridData, auxcumflow(:,:,8), weightGridData, 1, n_accum_cells)
!
!               Update result
                outGridData(i,j) = auxcumflow(i,j,8)
!
!               Clear beyond limits
                auxcumflow(MAX(i-n_accum_cells,1):MIN(i+n_accum_cells,mx-1),MAX(j-n_accum_cells,1):MIN(j+n_accum_cells,my-1),8) = nodata
!                
            endif
            
		enddo
	end do
!
!	From cells number to area
	do j = 2, (my-1)
		do i = 2, (mx-1)
			if((outGridData(i, j) .NE. nodata) .AND. (inCumflow(i, j) .GT. 0.d0) ) then
				outGridData(i, j) = outGridData(i, j) * dx * dy / inCumflow(i, j)
			endif
		enddo
    enddo
!
!
!   Allocate memory for auxiliar raster
   	DEALLOCATE(auxcumflow)	        !Auxiliar grid
!
end
!
!
!****************************************************************************
!
!  SUBROUTINE: WeightedGetFlow
!
!  PURPOSE:  Cumulate weighted flows.
!
!****************************************************************************
recursive subroutine WeightedGetFlow(ix, jy, inGridData, outGridData, weightGridData)
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
!   Variables
	real*8 outGridData(mx,my)
    real*8 inGridData(mx,my)
    real*8 weightGridData(mx,my)
!	
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!

	if (outGridData(ix, jy) .NE. nodata) then
		return
	endif
!
	dValue = inGridData(ix, jy)
    dValue2 = weightGridData(ix, jy)
!		
	if((dValue .NE. nodata) .AND. (dValue2 .NE. nodata)) then
!		
		outGridData(ix, jy) = dValue2
!
		j = 4
		do i=0, 7
!
			iix	= ix + iOffsetX(i);
			jjy	= jy + iOffsetY(i);
!
			dValue = inGridData(iix, jjy);
!
			if(dValue .NE. nodata) then
!				
				dFlow = auxcumflow(iix,jjy,j)
!
				if(dFlow .NE. nodata) then
!
				    call WeightedGetFlow(iix,jjy, inGridData, outGridData, weightGridData)
                    
                    if (outGridData(iix, jjy) .NE. nodata) then
					    dValue =  outGridData(iix, jjy) * dFlow
                    else
                        dValue = 0.d0
                    endif
                    
                    if (outGridData(ix, jy) .NE. nodata) then
					    outGridData(ix, jy) = dValue + outGridData(ix, jy)
                    else
                        outGridData(ix, jy) = dValue
                    endif
!                    
				endif
!
			endif
!				
			j = MOD((j+1),8)
!
		enddo
!			
	endif
!
    end subroutine

    
    
!****************************************************************************
!
!  SUBROUTINE: WeightedGetFlowLim
!
!  PURPOSE:  Cumulate limited weighted flows.
!
!****************************************************************************
recursive subroutine WeightedGetFlowLim(ix, jy, inGridData, outGridData, weightGridData, num_rec, n_accum_cells)
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
!   Variables
	real*8 outGridData(mx,my)
    real*8 inGridData(mx,my)
    real*8 weightGridData(mx,my)
!	
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!

	if (outGridData(ix, jy) .NE. nodata) then
		return
	endif
!
	dValue = inGridData(ix, jy)
    dValue2 = weightGridData(ix, jy)
!		
	if((dValue .NE. nodata) .AND. (dValue2 .NE. nodata)) then
!		
		outGridData(ix, jy) = dValue2
!
		j = 4
		do i=0, 7
!
			iix	= ix + iOffsetX(i);
			jjy	= jy + iOffsetY(i);
!
			dValue = inGridData(iix, jjy);
!
			if(dValue .NE. nodata) then
!				
				dFlow = auxcumflow(iix,jjy,j)
!
				if((dFlow .NE. nodata) .AND. (num_rec .LT. n_accum_cells)) then
!
				    call WeightedGetFlowLim(iix,jjy, inGridData, outGridData, weightGridData, num_rec + 1, n_accum_cells)
                    
                    if (outGridData(iix, jjy) .NE. nodata) then
					    dValue =  outGridData(iix, jjy) * dFlow
                    else
                        dValue = 0.d0
                    endif
                    
                    if (outGridData(ix, jy) .NE. nodata) then
					    outGridData(ix, jy) = dValue + outGridData(ix, jy)
                    else
                        outGridData(ix, jy) = dValue
                    endif
!                    
				endif
!
			endif
!				
			j = MOD((j+1),8)
!
		enddo
!			
	endif
!
end subroutine

    