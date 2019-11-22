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
!
!
!	Abrimos ficheros de entrada de datos de control
	open(100,file='./data/hmtu.dat',status='old',form='formatted',err=1000)
!
!	first & second line
	read(100,*)	!index	Cr_max	Cr_min	A	B	C	D
	read(100,*) !()	    (kPa)	(kPa)	CN	CN	CN	CN
!
!
	do i=1, numberLandUses
!	
!		Read the landuse properties:
		read(100,*) index,Crmax,Crmin,A,B,C,D
!
!		Corregimos unidades
		LandUses(index)%Crmin = Crmin * 1000.d0
		LandUses(index)%Crmax = Crmax * 1000.d0
		LandUses(index)%A = A
		LandUses(index)%B = B
		LandUses(index)%C = C
		LandUses(index)%D = D
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
