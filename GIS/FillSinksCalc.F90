!
!
!****************************************************************************
!
!  SUBROUTINE: FillSinksCalc
!
!  PURPOSE:  Compute grid cumulative area.
!
!****************************************************************************
subroutine FillSinksCalc()
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
	integer xloc, yloc, sinkflag
!
!	initialisation of other variables */
	outside = nodata
	itermax = 10000
	inimin = 2000000
	numiter = 0
	numsin = -9999
	inix = 0
	iniy = 0
!
!	find the outlet = cell with minimum elevation */
	cotamin = 100000.d0
	do j=1,my
		do i=1,mx
			if((topo(i,j) .NE. nodata) .AND. (topo(i,j) .LT. cotamin)) then
				cotamin = topo(i,j)
				inix = i
				iniy = j
			endif
		enddo
	enddo
!				
!
!	main loop for sink filling */
	do while((numsin .NE. 0) .AND. (numiter .LT. itermax))
!
		numiter = numiter + 1
		numsin = 0
!
		do j=2,(my-1)
			do i=2,(mx-1)
				sinkflag = 1
				numoutside = 0
!
!				skip cells outside and the outlet
				if((topo(i,j) .EQ. outside) .OR. ((i .EQ. inix) .AND. (j .EQ. iniy))) then
					continue
					sinkflag = 0
				else
					do yloc = -1,1
						do xloc = -1,1
							if((xloc .NE. 0) .OR. (yloc .NE. 0)) then
								if((xloc .EQ. 0) .OR. (yloc .EQ. 0)) then
									grad = DTAN(R_D/100.D0) * dx
								else
									grad = DTAN(R_D/100.D0) * DSQRT(dx**2 + dy**2)
								endif
!							
								if(topo(i,j) .GE. topo(i+xloc,j+yloc)) then
									sinkflag = 0
								endif
!
!								why numiter > 10 ????	
								if((topo(i+xloc,j+yloc) .EQ. outside) .AND. (numiter > 10)) then
									numoutside = numoutside + 1
								endif
							endif
						enddo
					enddo
				endif
!
!
!				if sinkflag ==1, then we have a sink except if it borders an outside cell */
				if((sinkflag .EQ. 1) .AND. (numoutside .EQ. 0)) then
					numsin = numsin + 1
					elemin = 999999999
!
!					look for the minimum elevation around a cell and increase the elevation of sink cell
					do yloc=-1,1
						do xloc=-1,1
							if( (topo(i+xloc,j+yloc) .NE. outside) .AND. ((xloc .NE. 0) .OR. (yloc .NE. 0))) then
								if(topo(i+xloc,j+yloc) .LT. elemin) then
									elemin = topo(i+xloc,j+yloc)
									if((xloc .EQ. 0) .OR. (yloc .EQ. 0)) then
										grad = DTAN(R_D/100.D0) * dx
									else
										grad = DTAN(R_D/100.D0) * DSQRT(dx**2 + dy**2)
									endif
								endif
							endif
						enddo
					enddo
!
					topo(i,j) = elemin + grad
!
				endif
!
			enddo
		enddo
!
!		Output
		write(6,'("Number of fillsinks iteration:  ",I10)') numiter
        write(6,'("Numer of sinks:  ",I10,/)') numsin
!
	enddo
!
!	Output
	write(6,'("Numer of sinks:  ",I10,/)') numsin
!
!
end subroutine
