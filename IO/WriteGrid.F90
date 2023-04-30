!
!
!****************************************************************************
!
!  SUBROUTINE: WriteGrid
!
!  PURPOSE:  Write results.
!
!****************************************************************************
subroutine WriteGrid(GridData, Filename)
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
    REAL*8 GridData(:,:)
    CHARACTER Filename*(*)
!
!
!
!	Write raster in ASCII format
	write(6,'("Raster output (ASCII): ",A30,/)') Filename
!   
!	Escribimos resultados
	open(unit=100,file=(trim(fname_res) // '\' // Filename),status='unknown',form='formatted',buffered='YES',buffercount=10,blocksize=32768)
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
		write(100,1002) (GridData(i,j), i =1,mx-1)
		write(100,1004) GridData(mx,j)
	end do
!    
!	Cerramos fichero
	close(100)
!
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
