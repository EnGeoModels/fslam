!
!
!****************************************************************************
!
!  SUBROUTINE: SlopeCalc
!
!  PURPOSE:  Compute grid slopes.
!
!****************************************************************************
subroutine SlopeCalc()
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
!	Slopes computing (null boundary)
	slopeGrid(:, 1)  = nodata
	slopeGrid(:, my) = nodata
	slopeGrid(1, :)  = nodata
	slopeGrid(mx, :) = nodata
	zones(:, 1)  = INT(nodata)
	zones(:, my) = INT(nodata)
	zones(1, :)  = INT(nodata)
	zones(mx, :) = INT(nodata)	
	
	do j = 2, (my-1)
		do i = 2, (mx-1)
!
			call TarbotonSlope(i, j)
!
		enddo
	end do
!
end subroutine
!
!
!****************************************************************************
!
!  SUBROUTINE: TarbotonSlope
!
!  PURPOSE:  Calculate Tarboton slopes.
!
!****************************************************************************
subroutine TarbotonSlope(ix, jy)
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
	real*8,	dimension(0:7) :: zm
	integer,dimension(0:7) :: iOffsetX
	integer,dimension(0:7) :: iOffsetY
	real*8 iSlope
!
	iOffsetX = (/  0,  1,  1,  1,  0, -1, -1, -1/)
	iOffsetY = (/  1,  1,  0, -1, -1, -1,  0,  1/)
!
!	Get topo value
	z = topo(ix, jy)
!
	if(z .EQ. nodata) then
			slopeGrid(ix, jy) = nodata
	else
!
		do i = 0,7
!
			z2 = topo(ix + iOffsetX(i), jy + iOffsetY(i))
!
			if(z2 .NE. nodata) then
				zm(i) = z2
			else
				z2 = topo( ix + iOffsetX(MOD(i+4,8)) , jy + iOffsetY(MOD(i+4,8)) )
!
				if(z2 .NE. nodata) then
					zm(i) = z - (z2 - z)
				else
					zm(i) = z
				endif
!
			endif
		enddo

        SlopeCell	=  0.d0

		j = 1
		do i = 0,7
!
			if( MOD(i,2) .NE. 0 ) then	!i => diagonal
				G		= (z		- zm(j)) / dx
				H		= (zm(j)	- zm(i)) / dx
			else						! i => orthogonal
				G		= (z		- zm(i)) / dx
				H		= (zm(i)	- zm(j)) / dx
			endif

			if( H .LT. 0.d0 ) then
				iSlope	= G;
			elseif( H .GT. G ) then
				if(MOD(i, 2) .NE. 0) then
					iSlope = (z - zm(i)) / (DSQRT(2.d0) * dx)
				else
					iSlope = (z - zm(j)) / (DSQRT(2.d0) * dx)
				endif
			else
				iSlope	= DSQRT(G*G + H*H);
			endif

			if( iSlope .GT. SlopeCell ) then
				SlopeCell	= iSlope;
			endif
!
			j = MOD(j+1,8)
!
		enddo

		slopeGrid(ix, jy) = DMAX1(DATAN(SlopeCell),0.0001)

	endif
!
end subroutine

