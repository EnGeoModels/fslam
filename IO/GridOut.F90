!
!
!****************************************************************************
!
!  SUBROUTINE: GridOut
!
!  PURPOSE:  Write results.
!
!****************************************************************************
subroutine GridOut()
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
!	Salida de resultados velocidad mediante GRID Arcview
	write(6,'("Topo output: topo.asc",/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/topo.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (topoIni(i,j), i =1,mx-1)
		write(100,1004) topoIni(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados velocidad mediante GRID Arcview
	write(6,'("Fill topo output: fill.asc",/)')   
!
!	Escribimos resultados
    fname = (trim(fname_res) // '/fill.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (topo(i,j), i =1,mx-1)
		write(100,1004) topo(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados velocidad mediante GRID Arcview
	write(6,'("Slopes output: slopes.asc",/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/slopes.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (slopeGrid(i,j), i =1,mx-1)
		write(100,1004) slopeGrid(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados mediante GRID Arcview
	write(6,'("Flow accumulation output: cumflow.asc",/,/,/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/cumflow.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (cumflow(i,j), i =1,mx-1)
		write(100,1004) cumflow(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados mediante GRID Arcview
	write(6,'("Averaged antecent rainfall output: rain_ant_averaged.asc",/,/,/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/rain_ant_averaged.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (WeightedRainfall_ant(i,j), i =1,mx-1)
		write(100,1004) WeightedRainfall_ant(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados mediante GRID Arcview
	write(6,'("Averaged rainfall output: rain_averaged.asc",/,/,/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/rain_averaged.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (WeightedRainfall(i,j), i =1,mx-1)
		write(100,1004) WeightedRainfall(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Salida de resultados mediante GRID Arcview
	write(6,'("Averaged CN output: CN_averaged.asc",/,/,/)')
!   
!	Escribimos resultados
    fname = (trim(fname_res) // '/CN_averaged.asc')
!
	open(unit=100,file=fname,status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
!
!	Keywords de la GRID
	write(100,1000) 'ncols         ', mx
	write(100,1000) 'nrows         ', my  
!
	write(100,1001) 'xllcorner     ', xcorner
	write(100,1001) 'yllcorner     ', ycorner
!    
	write(100,1001) 'cellsize      ', dx
	write(100,1001) 'NODATA_value  ', nodata
!
!	Escribimos la malla
	do j = 1,my
		write(100,1002) (WeightedCN(i,j), i =1,mx-1)
		write(100,1004) WeightedCN(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
!
!
!	Formatos de Keyword
1000 format(A14, I10)
1001 format(A14, F16.6)
1002 format(F15.4, $)
1003 format(/)    
1004 format(F15.4)
!
!
!
end subroutine
