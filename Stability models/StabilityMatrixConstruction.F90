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
!  SUBROUTINE: StabilityMatrixConstruction
!
!  PURPOSE:  Compute the failure probability matrix.
!
!****************************************************************************
subroutine StabilityMatrixConstruction()
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
!	Explicit variables
	real*8 :: R
!
!
!   Antecedent rainfall
    AntRainInten = AntRainVec(1)
!    
!   Initialize
    AuxSF(:,:) = 0.d0
    AuxProb(:,:) = 0.d0
    
!
!
!   Parallel loop
    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,i_C,i_z,i_phi,i_kh,i_por,R,i_inf,i_a_b,i_slp,ii,index_vect,fname) 
    !$OMP DO SCHEDULE(DYNAMIC)
!
!   Loop for all the soils
	do i = 1, numberZones
!
!       Constants
        R = densw / GaussDens(i)%mean
!
!       Initialize probability
!       Five parameters loop to comute the stability matrix
        do i_C = 1, nParams
            do i_z = 1, nParams
!
!               Constants at this level
                aux_term(i,:) = 1.d0 / (SoilsParameterMatrix(i)%z(i_z) * AuxMat(i,:,1))
                C_term(i,:) = SoilsParameterMatrix(i)%C(i_C) * aux_term(i,:) / (grav * GaussDens(i)%mean)
!
                do i_phi = 1, nParams
!
!                   Constants at this level
                    tan_term(i,:) = DTAN(SoilsParameterMatrix(i)%phi(i_phi)) / AuxMat(i,:,2)
!
                    do i_kh = 1, nParams
!
!                       Constants at this level
                        inf_term(i,:) = AntRainInten * aux_term(i,:) * R * AuxMat(i,:,3) / SoilsParameterMatrix(i)%kh(i_kh)
!                        
                        do i_por = 1, nParams
!
!                           Constants at this level
                            por_term(i,:) = R * AuxMat(i,:,4) / (SoilsParameterMatrix(i)%z(i_z) * SoilsParameterMatrix(i)%porosity(i_por))
!
!                           Terms assembly
                            sum_term(i,:) = C_term(i,:) + tan_term(i,:) * (1.d0 - DMIN1(inf_term(i,:) + por_term(i,:), R))
!
!                           Averaged SF
                            AuxSF(i,:) = AuxSF(i,:) + sum_term(i,:)
!
!                           Probability of failure
                            AuxProb(i,:) = AuxProb(i,:) - DBLE(sum_term(i,:) .LT. 1.d0)
!                        
                        enddo
                    enddo
                enddo
            enddo
        enddo
!
!       Normalize probability
        AuxSF(i,:) = DMIN1(AuxSF(i,:) / DBLE(nParams)**5.d0, 2.d0)
        !AuxSF(i,:) = AuxSF(i,:) / DBLE(nParams)**5.d0
        AuxProb(i,:) = AuxProb(i,:) / DBLE(nParams)**5.d0
!
!       Reshape results
        index_vect = 1
        do i_inf = 1, nVariables
            do i_a_b = 1, nVariables
                do i_slp = 1, nVariables
                    SoilsVariablesMatrix(i)%SF(i_slp,i_a_b,i_inf) = AuxSF(i,index_vect)
                    SoilsVariablesMatrix(i)%Prob(i_slp,i_a_b,i_inf) = AuxProb(i,index_vect)
!
!                   Update index
                    index_vect = index_vect + 1
                enddo
            enddo
        enddo
!
!
!	    Write stability matrix result
	    write(6,'("Soil type: ",I3,". Stability matrix result writting",/)') i
        write(fname,'("./res/SF_matrix_",I0.3,".TXT")') i
!   
!	    Open file
	    open(unit=(100+i),file=fname,status='unknown',form='formatted')
!
!       Write matrix
        do ii = 1, nVariables
		    write((100+i),1002) (AuxSF(i,j), j = (ii - 1) * nVariables * nVariables + 1,(ii * nVariables*nVariables - 1))
		    write((100+i),1004) AuxSF(i,ii * nVariables*nVariables)
        enddo
!    
!	    Cerramos fichero
	    close(100+i)
!
!       Probability matrix
        write(fname,'("./res/PROB_matrix_",I0.3,".TXT")') i
!   
!	    Open file
	    open(unit=(100+i),file=fname,status='unknown',form='formatted')
!
!       Write matrix
        do ii = 1, nVariables
		    write((100+i),1002) (AuxProb(i,j), j = (ii - 1) * nVariables * nVariables + 1,(ii * nVariables*nVariables - 1))
		    write((100+i),1004) AuxProb(i,ii * nVariables*nVariables)
        enddo
!    
!	    Cerramos fichero
	    close(100+i)
!
!
    enddo
!
!
    !$OMP END DO NOWAIT
    !$OMP END PARALLEL
!
!	Formatos de Keyword
1000 format(A14, I10)
1001 format(A14, F14.6)
1002 format(F15.4, $)
1003 format(/)    
1004 format(F15.4)
!
!
end subroutine
