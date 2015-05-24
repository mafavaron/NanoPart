! particles.f90 - Module, supporting particle emission and management.
!
! Written by: Mauri Favaron
!
MODULE Particles
	
	IMPLICIT NONE
	
	PRIVATE
	
	! Steering parameters (change if required)
	INTEGER, PARAMETER	:: ONE_HOUR					= 3600	! s/h
	INTEGER, PARAMETER	:: EMISSION_PERIOD			=    1	! 1 s/emission
	INTEGER, PARAMETER	:: FREQUENCY				=   10	! Hz
	INTEGER, PARAMETER	:: NUM_EMITTED				=    1	! 1 particles/emission
	INTEGER, PARAMETER	:: NUM_SOURCES				=    1	! 1 source
	INTEGER, PARAMETER	:: PARTICLE_POOL_SIZE		= 2*ONE_HOUR*NUM_EMITTED*NUM_SOURCES/EMISSION_PERIOD
	REAL, DIMENSION(NUM_SOURCES), PARAMETER	:: X0	=   [ 0.]
	REAL, DIMENSION(NUM_SOURCES), PARAMETER	:: Y0	=   [ 0.]
	REAL, DIMENSION(NUM_SOURCES), PARAMETER	:: Z0	=   [10.]
	
	! Public interface
	PUBLIC	:: ParticleType
	
	! Other useful constants (please do not change)
	INTEGER, PARAMETER	:: SVR_INFO    = 0
	INTEGER, PARAMETER	:: SVR_WARNING = 1
	INTEGER, PARAMETER	:: SVR_ERROR   = 2
	
	! Data types
	
	TYPE ParticleType
		! Type management "standard" variables
		LOGICAL, PRIVATE								:: lDebug
		CHARACTER(LEN=256), PRIVATE					:: sErrorMsg
		! Particle pool
		REAL, DIMENSION(PARTICLE_POOL_SIZE), PRIVATE		:: X
		REAL, DIMENSION(PARTICLE_POOL_SIZE), PRIVATE		:: Y
		REAL, DIMENSION(PARTICLE_POOL_SIZE), PRIVATE		:: Z
		REAL, DIMENSION(PARTICLE_POOL_SIZE), PRIVATE		:: Mass
		REAL, DIMENSION(PARTICLE_POOL_SIZE), PRIVATE		:: Age
		INTEGER, PRIVATE									:: iSize
		INTEGER, PRIVATE									:: iDataIndex
		! Grid related information
		REAL, PRIVATE										:: rX0
		REAL, PRIVATE										:: rY0
		INTEGER, PRIVATE									:: iNx
		INTEGER, PRIVATE									:: iNy
		REAL, PRIVATE										:: rDx
		REAL, PRIVATE										:: rDy
		REAL, PRIVATE										:: rMaxZ
	CONTAINS
		! Particles dynamics
		PROCEDURE	:: Initialize    => ParticlesClean
		PROCEDURE	:: Emit          => ParticlesEmit
		PROCEDURE	:: Move          => ParticlesMove
		! Particles count with respect to grid
		PROCEDURE	:: LocateOnGrid  => ParticlesCount
		! Parameter access functions
		PROCEDURE	:: GetPoolSize   => ParticlesGetPoolSize
		PROCEDURE	:: GetNumber     => ParticlesGetNumber
		PROCEDURE	:: GetNumSources => ParticlesGetNumSources
	END TYPE ParticleType
	
CONTAINS
	
	FUNCTION ParticlesClean(this, rX0, rY0, iNx, iNy, rDx, rDy, rMaxZ, lDebug) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ParticleType)				:: this
		REAL, INTENT(IN)				:: rX0
		REAL, INTENT(IN)				:: rY0
		INTEGER, INTENT(IN)				:: iNx
		INTEGER, INTENT(IN)				:: iNy
		REAL, INTENT(IN)				:: rDx
		REAL, INTENT(IN)				:: rDy
		REAL, INTENT(IN)				:: rMaxZ
		LOGICAL, INTENT(IN), OPTIONAL	:: lDebug
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Get optional parameters
		IF(PRESENT(lDebug)) THEN
			this % lDebug = lDebug
		ELSE
			this % lDebug = .FALSE.
		END IF
		
		! Check grid information makes sense
		IF(iNx <= 0 .OR. iNy <= 0 .OR. rDx <= 0. .OR. rDy <= 0. .OR. rMaxZ <= 0.) THEN
			iRetCode = 1
			this % sErrorMsg = "Invalid grid specification"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Clean out data space
		this % X          = 0.
		this % Y          = 0.
		this % Z          = 0.
		this % Mass       = 0.
		this % Age        = 0.
		this % iSize      = 0
		this % iDataIndex = 0
		
		! Assign grid information
		this % rX0   = rX0
		this % rY0   = rY0
		this % iNx   = iNx
		this % iNy   = iNy
		this % rDx   = rDx
		this % rDy   = rDy
		this % rMaxZ = rMaxZ
		
	END FUNCTION ParticlesClean
	
	
	FUNCTION ParticlesEmit(this, rvMass) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ParticleType)							:: this
		REAL, DIMENSION(NUM_SOURCES), INTENT(IN)	:: rvMass	! Mass per particle, which is a *fraction* of total emitted mass
		INTEGER										:: iRetCode
		
		! Locals
		INTEGER	:: iSource
		INTEGER	:: iParticle
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Add new particles, all sharing their initial positions and with age 0
		DO iSource = 1, NUM_SOURCES
			DO iParticle = 1, NUM_EMITTED
				this % iSize      = MIN(this % iSize + 1, PARTICLE_POOL_SIZE)
				this % iDataIndex = this % iDataIndex + 1
				IF(this % iDataIndex > PARTICLE_POOL_SIZE) this % iDataIndex = 1
				this % X(this % iDataIndex)    = X0(iSource)
				this % Y(this % iDataIndex)    = Y0(iSource)
				this % Z(this % iDataIndex)    = Z0(iSource)
				this % Age(this % iDataIndex)  = 0.
				this % Mass(this % iDataIndex) = rvMass(iSource)
			END DO
		END DO
		
	END FUNCTION ParticlesEmit
	
	
	FUNCTION ParticlesMove(this, Vx, Vy, Vz) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ParticleType)								:: this
		REAL, DIMENSION(PARTICLE_POOL_SIZE), INTENT(IN)	:: Vx
		REAL, DIMENSION(PARTICLE_POOL_SIZE), INTENT(IN)	:: Vy
		REAL, DIMENSION(PARTICLE_POOL_SIZE), INTENT(IN)	:: Vz
		INTEGER											:: iRetCode
		
		! Locals
		INTEGER	:: iParticle
		REAL	:: dT
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Move all existing particles, based on current wind sample
		dT = 1./FREQUENCY
		DO iParticle = 1, this % iSize
			this % X(iParticle)    = this % X(iParticle) + dT*Vx(iParticle)
			this % Y(iParticle)    = this % Y(iParticle) + dT*Vy(iParticle)
			this % Z(iParticle)    = ABS(this % Z(iParticle) + dT*Vz(iParticle))
			this % Mass(iParticle) = this % Mass(iParticle)	! Leave unchanged, as a cautionary measure
			this % Age(iParticle)  = this % Age(iParticle)  + 1./FREQUENCY
		END DO
		
	END FUNCTION ParticlesMove
	
	
	FUNCTION ParticlesCount(this) RESULT(rmConc)
	
		! Routine arguments
		CLASS(ParticleType)						:: this
		REAL, DIMENSION(this % iNx, this % iNy)	:: rmConc
		
		! Locals
		INTEGER	:: iParticle
		INTEGER	:: iP
		INTEGER	:: jP
		
		! Compute index for each particle with respect to grid
		rmConc = 0.
		DO iParticle = 1, this % iSize
			iP = FLOOR((this % X(iParticle) - this % rX0 + 0.5*this % rDx) / this % rDx) + 1
			jP = FLOOR((this % Y(iParticle) - this % rY0 + 0.5*this % rDy) / this % rDy) + 1
			IF(1 <= iP .AND. iP <= this % iNx .AND. 1 <= jP .AND. jP <= this % iNy) THEN
				IF(this % Z(iParticle) <= this % rMaxZ) THEN
					rmConc(iP,jP) = rmConc(iP,jP) + this % Mass(iParticle)
				END IF
			END IF
		END DO
		rmConc = rmConc / (this % rDx * this % rDy)	! Cautionary: rMaxZ is not accounted for,
													! so concentration looks as if all particles are within 1m
													! from ground. To date. In future might change.
		
	END FUNCTION ParticlesCount
	
	
	FUNCTION ParticlesGetPoolSize(this) RESULT(iPoolSize)
	
		! Routine arguments
		CLASS(ParticleType)	:: this
		INTEGER			:: iPoolSize
		
		! Locals
		! -none-
		
		! Get the information desired
		iPoolSize = PARTICLE_POOL_SIZE
		
	END FUNCTION ParticlesGetPoolSize
	
	
	FUNCTION ParticlesGetNumber(this) RESULT(iNumParticles)
	
		! Routine arguments
		CLASS(ParticleType)	:: this
		INTEGER			:: iNumParticles
		
		! Locals
		! -none-
		
		! Get the information desired
		iNumParticles = this % iSize
		
	END FUNCTION ParticlesGetNumber
	
	
	FUNCTION ParticlesGetNumSources(this) RESULT(iPoolSize)
	
		! Routine arguments
		CLASS(ParticleType)	:: this
		INTEGER			:: iPoolSize
		
		! Locals
		! -none-
		
		! Get the information desired
		iPoolSize = NUM_SOURCES
		
	END FUNCTION ParticlesGetNumSources
	
	! *********************
	! * Internal routines *
	! *********************
	
	SUBROUTINE ErrorReport(iSeverity, sErrMsg)
	
		! Routine arguments
		INTEGER, INTENT(IN)			:: iSeverity
		CHARACTER(LEN=*), INTENT(IN)	:: sErrMsg
		
		! Locals
		CHARACTER(LEN=8)	:: sSeverity
		
		! Print message
		SELECT CASE(iSeverity)
		CASE(SVR_INFO)
			sSeverity = "info"
		CASE(SVR_WARNING)
			sSeverity = "warning"
		CASE(SVR_ERROR)
			sSeverity = "error"
		END SELECT
		PRINT *,TRIM(sSeverity), "::", TRIM(sErrMsg)
		
		! Stop, if severity prevents to proceed
		IF(iSeverity >= SVR_ERROR) STOP
		
	END SUBROUTINE ErrorReport

END MODULE Particles
