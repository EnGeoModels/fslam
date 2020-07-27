!
!
!****************************************************************************
!
!  SUBROUTINE: CumFlowCalc
!
!  PURPOSE:  Compute grid cumulative area.
!
!****************************************************************************
subroutine CumFlowCalc(inGridData, outGridData, algorithm)
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
	real*8 outGridData(mx,my)
	integer algorithm
!
!   Allocate memory for auxiliar raster
   	ALLOCATE(auxcumflow(mx,my,0:7))	        !Auxiliar grid
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
			call getFlow(i, j, inGridData, outGridData)
		enddo
	end do
!
!	From cells number to area
	do j = 2, (my-1)
		do i = 2, (mx-1)
			if(outGridData(i, j) .NE. nodata) then
				outGridData(i, j) = outGridData(i, j) * dx * dy
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
!  SUBROUTINE: doDInf
!
!  PURPOSE:  Compute Tarboton Dinf.
!
!****************************************************************************
subroutine doDInf(ix, jy, inGridData)
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
!
	dAspect = getAspect(ix, jy, inGridData)
!	
	if(dAspect .NE. nodata) then
		dAspect = dAspect * D_R	!In degrees
		iDirection	= INT(dAspect / 45.d0)
		dAspect	= MOD(dAspect,45.d0) / 45.d0
		auxcumflow(ix,jy,MOD(iDirection,8))  =  1.d0 - dAspect
		auxcumflow(ix,jy,MOD(iDirection+1,8))= dAspect
	endif
!
end subroutine
!
!
!****************************************************************************
!
!  SUBROUTINE: doD8
!
!  PURPOSE:  Compute D8.
!
!****************************************************************************
subroutine doD8(ix, jy, inGridData)
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
		auxcumflow(ix,jy,MOD(iDirection,8))  =  1.d0
	endif
!
end subroutine
!
!
!****************************************************************************
!
!  SUBROUTINE: getDirToNextDownslopeCell
!
!  PURPOSE:  Find next downslope cell.
!
!****************************************************************************
integer function getDirToNextDownslopeCell(ix, jy, inGridData)
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
!   Variables
    real*8 :: z
    real*8 :: z2
    real*8 :: dMaxSlope
    real*8 :: dSlope
    real*8 :: rDist
    integer :: iDir
    integer :: i
    real*8 inGridData(mx,my)
    integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)

!    
    z = inGridData(ix, jy)
!
	if(z .EQ. nodata) then
		getDirToNextDownslopeCell = -1
        return
	else
        dMaxSlope = 0.d0
        iDir = -1
        i = 0
        do while (i .LT. 8)
            z2 = inGridData(ix + iOffsetX(i), jy + iOffsetY(i))
!
            if (z2 .EQ. nodata) then
                getDirToNextDownslopeCell = -1
                return
            else
                rDist = SQRT((iOffsetX(i) * dx)**2 + (iOffsetY(i) * dx)**2)
                dSlope = (z - z2) / rDist
                if (dSlope .GT. dMaxSlope) then
                    iDir = i
                    dMaxSlope = dSlope
                endif
                i = i + 1
            endif
        enddo
!    
        getDirToNextDownslopeCell = iDir
        return

    endif
!
end function            
!
!
!****************************************************************************
!
!  FUNCTION: getAspect
!
!  PURPOSE:  Calculate girs aspect.
!
!****************************************************************************
double precision function getAspect(ix, jy, inGridData)
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
	real*8,	dimension(0:3) :: zm
	logical :: getSubMatrix3x3
    real*8 inGridData(mx,my)
!
		if(getSubMatrix3x3(ix, jy, zm, inGridData)) then
			G	=  (zm(0) - zm(2)) / 2.d0 * dx
	        H	=  (zm(1) - zm(3)) / 2.d0 * dx
!
			if( G .NE. 0.d0 ) then
				dAspect = PI + DATAN2(H,G)
			else
				if(H > 0.d0) then
				
					if(H .GT. 0.d0) then
						dAspect = R_D * 270.d0
					elseif(H .LT. 0.d0) then
						dAspect = R_D * 90.d0
					else
						dAspect = nodata					
					endif				
				
				endif
			endif
!
			getAspect = dAspect
			return
!
		else
			getAspect = nodata
			return 
		endif
!
end function
!
!
!****************************************************************************
!
!  FUNCTION: getSubMatrix3x3
!
!  PURPOSE:  Get D8 matrix.
!
!****************************************************************************
logical function getSubMatrix3x3(ix, jy, SubMatrix, inGridData)
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
	real*8,	dimension(0:3) :: SubMatrix
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
    real*8 inGridData(mx,my)
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!
	z = inGridData(ix, jy)
!
	if(z .EQ. nodata) then
		getSubMatrix3x3 = .FALSE.
		return
	else
!
		do i=0,3
!
			iDir = 2 * i
			z2 = inGridData(ix + iOffsetX(iDir), jy + iOffsetY(iDir))
!
			if(z2 .NE. nodata) then
				SubMatrix(i)	=  z2 - z
			else
				z2 = inGridData(ix + iOffsetX(MOD((iDir + 4),8)), jy + iOffsetY(MOD((iDir  + 4),8)));
!
				if(z2 .NE. nodata) then
					SubMatrix(i)	= z - z2
				else
					SubMatrix(i)	= 0.d0
				endif
			endif
!
		enddo
!
		getSubMatrix3x3 = .TRUE.
		return
!
	endif
!
end function
!
!
!****************************************************************************
!
!  SUBROUTINE: getFlow
!
!  PURPOSE:  Cumulate flows.
!
!****************************************************************************
recursive subroutine getFlow(ix, jy, inGridData, outGridData)
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
!		
	if(dValue .NE. nodata) then
!		
		outGridData(ix, jy) = 1.d0
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
					call getFlow(iix,jjy, inGridData, outGridData)
					dValue =  outGridData(iix, jjy) * dFlow
					dValue2 =  outGridData(ix, jy)
					outGridData(ix, jy) = dValue + dValue2
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
