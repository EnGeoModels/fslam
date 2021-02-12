!
!
!****************************************************************************
!
!  SUBROUTINE: LecLandUsesDat
!
!  PURPOSE:  Read land use data.
!
!****************************************************************************
subroutine LecLandUsesDat()
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
    TYPE(ReadLandUseProperties) :: read_aux
!
!
!	Abrimos ficheros de entrada de datos de control
	open(100,file=fname_hmtu,status='old',form='formatted',err=1000)
!
!	first & second line
	read(100,*)	!index,Cr_max,Cr_min,A,B,C,D
	read(100,*) !(),(kPa),(kPa),CN,CN,CN,CN
!
!
	do i=1, numberLandUses
!	
!		Read the landuse properties:
		read(100,*) read_aux
!
!		Corregimos unidades
		LandUses(read_aux%index)%Crmin = read_aux%Crmin * 1000.d0
		LandUses(read_aux%index)%Crmax = read_aux%Crmax * 1000.d0
		LandUses(read_aux%index)%A = read_aux%A
		LandUses(read_aux%index)%B = read_aux%B
		LandUses(read_aux%index)%C = read_aux%C
		LandUses(read_aux%index)%D = read_aux%D
!
	enddo
!
!	Cerramos el fichero
    close(100)
!
!
!
!	No error
	goto 2000
!
!	Error
!
!	Se ha producido un error en la malla, no es igual a la de topo
1000 write(6,100) numberLandUses
100  format('Landuse data file error. Number of land use areas: ',I5,/)
!
!
2000 continue
      
end subroutine
