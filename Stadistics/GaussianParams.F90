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
!  SUBROUTINE: GaussianParams
!
!  PURPOSE:  Generate random soil properties.
!
!****************************************************************************
subroutine GaussianParams()
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
!	Explicit variables
	real   :: rand
    real*8 :: InverseNormal
!
!
!	Gaussian properties calculation
	do i = 1, numberZones
		Gausskv(i) = GaussianProperties( (Soils(i)%kvmax+Soils(i)%kvmin)/2.d0,(Soils(i)%kvmax-Soils(i)%kvmin)/4.d0 )
		Gausskh(i) = GaussianProperties( (Soils(i)%khmax+Soils(i)%khmin)/2.d0,(Soils(i)%khmax-Soils(i)%khmin)/4.d0 )
		GaussC(i) = GaussianProperties( (Soils(i)%Cmax+Soils(i)%Cmin)/2.d0,(Soils(i)%Cmax-Soils(i)%Cmin)/4.d0 )
		Gaussphi(i)= GaussianProperties( (Soils(i)%phimax+Soils(i)%phimin)/2.d0,(Soils(i)%phimax-Soils(i)%phimin)/4.d0 )
		Gaussz(i) = GaussianProperties( (Soils(i)%zmax+Soils(i)%zmin)/2.d0,(Soils(i)%zmax-Soils(i)%zmin)/4.d0 )
		GaussDiff(i) = GaussianProperties( (Soils(i)%Diffmax+Soils(i)%Diffmin)/2.d0,(Soils(i)%Diffmax-Soils(i)%Diffmin)/4.d0 )
		GaussDens(i) = GaussianProperties( (Soils(i)%Densmax+Soils(i)%Densmin)/2.d0,(Soils(i)%Densmax-Soils(i)%Densmin)/4.d0 )
        GaussPor(i) = GaussianProperties( (Soils(i)%porositymax+Soils(i)%porositymin)/2.d0,(Soils(i)%porositymax-Soils(i)%porositymin)/4.d0 )
    enddo
!
!   Stochastic properties of parameters
    if (iStochastic .EQ. 1) then
!
!       Compute parameters normal vector
        do i = 1, nParams
            VectParams(i) = InverseNormal( DBLE(i) / (DBLE(nParams) + 1.d0))
        enddo
!
!       Fill the probability matrix
    	do i = 1, numberZones
            SoilsParameterMatrix(i) = ParameterMatrix((VectParams*Gausskh(i)%stdde)+Gausskh(i)%mean, (VectParams*Gaussz(i)%stdde)+Gaussz(i)%mean, (VectParams*GaussC(i)%stdde)+GaussC(i)%mean, (VectParams*Gaussphi(i)%stdde)+Gaussphi(i)%mean, (VectParams*GaussPor(i)%stdde)+GaussPor(i)%mean)
        enddo    
!
!       Compute variables discretization vector
        do i = 1, nVariables
            VectVar(i) = DBLE(i) / DBLE(nVariables)
        enddo
!
!       Fill the stability matrix
    	do i = 1, numberZones
            SoilsVariablesMatrix(i) = VariablesMatrix(a_b = VectVar*(a_b_max - a_b_min) + a_b_min, Slope = VectVar*(slope_max - slope_min) + slope_min, Infiltration = (VectVar - VectVar(1))*(qe_max - qe_min) + qe_min, SF = 0.d0, Prob = 0.d0)
!
!           Fill the auxiliar matrix, using low performance tripple loop (TODO: improve)
            index_vect = 1
            do i_inf = 1, nVariables                
                do i_a_b = 1, nVariables
                    do i_slp = 1, nVariables    
                        AuxMat(i,index_vect,1) = DCOS(SoilsVariablesMatrix(i)%Slope(i_slp))*DSIN(SoilsVariablesMatrix(i)%Slope(i_slp))
                        AuxMat(i,index_vect,2) = DTAN(SoilsVariablesMatrix(i)%Slope(i_slp))
                        AuxMat(i,index_vect,3) = SoilsVariablesMatrix(i)%a_b(i_a_b)   
                        AuxMat(i,index_vect,4) = SoilsVariablesMatrix(i)%Infiltration(i_inf)
!
!                       Update index
                        index_vect = index_vect + 1
                    enddo
                enddo
            enddo
!
        enddo 
!
    endif
!
!
end subroutine
