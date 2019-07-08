!****************************************************************************
!
!  fslam: Fast Shallow Landslide Assessment Model
!
!  Code developed in the  SMuCPhy project, founded by the Ministerio de 
!  Economia y Competitividad del Gobierno de España and coordinated by UPC
!  BarcelonaTECH
!
!  Coordinated by Department of Civil and Environmental Engineering
!
!****************************************************************************
!
!
!****************************************************************************
!
!  SUBROUTINE: LecZonesDat
!
!  PURPOSE:  Read soil data.
!
!****************************************************************************
subroutine LecZonesDat()
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
	open(100,file='./data/soils.dat',status='old',form='formatted',err=1000)
!
!	first & second line
	read(100,*)	!index     kvmax     kvmin     khmax     khmin     Diffmax   Diffmin   Cmax      Cmin      phimax    phimin    hmax      hmin      densmax   densmin    porositymax porositymin
	read(100,*) !          (m/s)     (m/s)     (m/s)     (m/s)     (m2/s)    (m2/s)    (kPa)     (kPa)     (degree)  (degree)  (m)       (m)       (kg/m3)   (kg/m3)    (m3/m3)     (m3/m3)
!
!
	do i=1, numberZones
!	
!		Entrada de parametros de input.dat:
		read(100,*) index,rkvmax,rkvmin,rkhmax,rkhmin,Diffmax,Diffmin,Cmax,Cmin,phimax,phimin,zmax,zmin,densmax,densmin,rporositymax,rporositymin
!
!		Corregimos unidades
		Soils(index)%kvmin = rkvmin
		Soils(index)%kvmax = rkvmax
		Soils(index)%khmin = rkhmin
		Soils(index)%khmax = rkhmax
		Soils(index)%Diffmin = Diffmin
		Soils(index)%Diffmax = Diffmax
		Soils(index)%Cmin = Cmin * 1000.d0
		Soils(index)%Cmax = Cmax * 1000.d0
		Soils(index)%phimin = phimin * R_D
		Soils(index)%phimax = phimax * R_D
		Soils(index)%zmin = zmin
		Soils(index)%zmax = zmax
		Soils(index)%densmin = densmin
		Soils(index)%densmax = densmax
        Soils(index)%porositymin = rporositymin
        Soils(index)%porositymax = rporositymax
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
1000 write(6,100) numberZones
100  format('Soils data file error. Number of zones: ',I5,/)
!
!
2000 continue
      
end subroutine
