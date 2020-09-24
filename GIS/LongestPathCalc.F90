!
!
!****************************************************************************
!
!  SUBROUTINE: LongestPathCalc
!
!  PURPOSE:  Compute longest path upstream and elevation.
!
!****************************************************************************
subroutine LongestPathCalc(inGridData, outLongGridData, outElevGridData)
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
	real*8 outLongGridData(mx,my)
	real*8 outElevGridData(mx,my)
	integer algorithm
!
!   Allocate memory for auxiliar raster
   	ALLOCATE(auxcumflow(mx,my,0:7))     !Auxiliar grid
	ALLOCATE(auxcumflow2(mx,my,0:7))    !Second uxiliar grid
!
!	Matrix initialization
	outLongGridData = nodata
	outElevGridData = nodata
	auxcumflow = nodata
	auxcumflow2 = nodata
!
!	Flows computing
	do j = 2, (my-1)
		do i = 2, (mx-1)
            call upsD8(i, j, inGridData)
		enddo
	end do
!
	do j = 2, (my-1)
		do i = 2, (mx-1)
			call getLength(i, j, inGridData, outLongGridData, outElevGridData)
		enddo
	end do
!
!   Allocate memory for auxiliar raster
   	DEALLOCATE(auxcumflow)	        !Auxiliar grid
	DEALLOCATE(auxcumflow2)	        !Auxiliar grid
!
end
!
!
!****************************************************************************
!
!  SUBROUTINE: upsD8
!
!  PURPOSE:  Compute D8 upstream.
!
!****************************************************************************
subroutine upsD8(ix, jy, inGridData)
!
!
!
!	Variables globales
	use fslamGlobals_structures
	use fslamGlobals_shared
	use fslamGlobals_private
!
	implicit double precision (a-h,o-z)
!
!   Variables
    real*8 inGridData(mx,my)
    integer :: getDirToNextDownslopeCell
!
	integer iDirection
!
	iDirection = getDirToNextDownslopeCell(ix, jy, inGridData)
!	
	if((iDirection .NE. nodata) .AND. (iDirection .GE. 0)) then
!
!		Check if diagonal
		if( MOD(iDirection,2) .NE. 0) then	!i => diagonal
			auxcumflow(ix,jy,MOD(iDirection,8)) = DSQRT(2.d0) * dx
		else								! i => orthogonal
			auxcumflow(ix,jy,MOD(iDirection,8)) = dx
		end if
!
		auxcumflow2(ix,jy,MOD(iDirection,8))  =  inGridData(ix, jy)
!
	endif
!
end subroutine
!
!
!****************************************************************************
!
!  SUBROUTINE: getLength
!
!  PURPOSE:  Cumulate length.
!
!****************************************************************************
recursive subroutine getLength(ix, jy, inGridData, outLongGridData, outElevGridData)
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
	real*8 outLongGridData(mx,my)
	real*8 outElevGridData(mx,my)
    real*8 inGridData(mx,my)
!	
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
!
!   Harcoded offsets
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!
!   Already calculated
	if (outLongGridData(ix, jy) .NE. nodata) then
		return
    endif
!
!   DEM
	dValue = inGridData(ix, jy)
!		
	if(dValue .NE. nodata) then
!		
		outLongGridData(ix, jy) = 1.d0
		outElevGridData(ix, jy) = dValue
		dLengthAux = 0.d0;
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
				dLength = auxcumflow(iix,jjy,j)
				dElev   = auxcumflow2(iix,jjy,j)
!
!				Check flow
				if(dLength .NE. nodata) then
!
!					Recursivity
					call getLength(iix,jjy, inGridData, outLongGridData, outElevGridData)
!
!					Length candidate
					dLengthAux =  outLongGridData(iix, jjy) + dLength
!
!					Check higher
					if (dLengthAux .GT. outLongGridData(ix, jy)) then
						outLongGridData(ix, jy) = dLengthAux
						outElevGridData(ix, jy) = DMAX1(dElev, outElevGridData(iix, jjy))
					end if
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
