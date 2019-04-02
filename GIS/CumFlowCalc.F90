!
!
!****************************************************************************
!
!  SUBROUTINE: CumFlowCalc
!
!  PURPOSE:  Compute grid cumulative area.
!
!****************************************************************************
subroutine CumFlowCalc()
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
!	Matrix initialization
	cumflow = nodata
	auxcumflow = nodata

!	Flows computing
	do j = 2, (my-1)
		do i = 2, (mx-1)
			call doDInf(i, j)
		enddo
	end do
!
	do j = 2, (my-1)
		do i = 2, (mx-1)
			call getFlow(i, j)
		enddo
	end do
!
!	From cells number to area
	do j = 2, (my-1)
		do i = 2, (mx-1)
			if(cumflow(i, j) .NE. nodata) then
				cumflow(i, j) = cumflow(i, j) * dx * dy
			endif
		enddo
	enddo
!
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
subroutine doDInf(ix, jy)
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
	real*8 iSlope
!
	dAspect = getAspect(ix, jy)
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
!  FUNCTION: getAspect
!
!  PURPOSE:  Calculate girs aspect.
!
!****************************************************************************
double precision function getAspect(ix, jy)
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
	real*8,	dimension(0:3) :: zm
	logical :: getSubMatrix3x3
!
		if(getSubMatrix3x3(ix, jy, zm)) then
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
logical function getSubMatrix3x3(ix, jy, SubMatrix)
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
	real*8,	dimension(0:3) :: SubMatrix
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY	
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!
	z = topo(ix, jy)
!
	if(z .EQ. nodata) then
		getSubMatrix3x3 = .FALSE.
		return
	else
!
		do i=0,3
!
			iDir = 2 * i
			z2 = topo(ix + iOffsetX(iDir), jy + iOffsetY(iDir))
!
			if(z2 .NE. nodata) then
				SubMatrix(i)	=  z2 - z
			else
				z2 = topo(ix + iOffsetX(MOD((iDir + 4),8)), jy + iOffsetY(MOD((iDir  + 4),8)));
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
recursive subroutine getFlow(ix, jy)
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
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!

	if (cumflow(ix, jy) .NE. nodata) then
		return
	endif
!
	dValue = topo(ix, jy)
!		
	if(dValue .NE. nodata) then
!		
		cumflow(ix, jy) = 1.d0
!
		j = 4
		do i=0, 7
!
			iix	= ix + iOffsetX(i);
			jjy	= jy + iOffsetY(i);
!
			dValue = topo(iix, jjy);
!
			if(dValue .NE. nodata) then
!				
				dFlow = auxcumflow(iix,jjy,j)
!
				if(dFlow .NE. nodata) then
					call getFlow(iix,jjy)
					dValue =  cumflow(iix, jjy) * dFlow
					dValue2 =  cumflow(ix, jy)
					cumflow(ix, jy) = dValue + dValue2
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
