PROGRAM NanoPart

	USE wind
	USE particles
	use concentration

	IMPLICIT NONE
	
	! Constants (to become configuration parameters in a next release)
	REAL, PARAMETER		:: rX0   = -1500.
	REAL, PARAMETER		:: rY0   = -1500.
	INTEGER, PARAMETER	:: iNx   =    31
	INTEGER, PARAMETER	:: iNy   =    31
	REAL, PARAMETER		:: rDx   =   100.
	REAL, PARAMETER		:: rDy   =   100.
	REAL, PARAMETER		:: rMaxZ = HUGE(rMaxZ)
	
	! Locals
	CHARACTER(LEN=256)	:: sDataPath
	CHARACTER(LEN=256)	:: sResultsPath
	CHARACTER(LEN=256)	:: sOutFile
	CHARACTER(LEN=10)	:: sDebugMode
	CHARACTER(LEN=10)	:: sBeginDate
	CHARACTER(LEN=10)	:: sBuffer
	INTEGER				:: iRetCode
	INTEGER				:: iSimulationTime
	TYPE(WindType)		:: tWind
	TYPE(ParticleType)	:: tParticles
	TYPE(ConcType)		:: tConcentration
	INTEGER				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
	INTEGER				:: iBeginYear, iBeginMonth, iBeginDay
	INTEGER				:: iNumDays
	LOGICAL				:: lDebug
	REAL				:: rU, rV, rW
	INTEGER				:: iNumSources
	INTEGER				:: iNumParticles
	INTEGER				:: i, j
	REAL, DIMENSION(:), ALLOCATABLE		:: rvMass
	REAL, DIMENSION(:), ALLOCATABLE		:: rvU
	REAL, DIMENSION(:), ALLOCATABLE		:: rvV
	REAL, DIMENSION(:), ALLOCATABLE		:: rvW
	REAL, DIMENSION(:,:), ALLOCATABLE	:: rmConc
	REAL, DIMENSION(:,:), ALLOCATABLE	:: rmAvg
	REAL, DIMENSION(:,:), ALLOCATABLE	:: rmStdDev
	
	! Get parameters
	IF(COMMAND_ARGUMENT_COUNT() /= 4 .AND. COMMAND_ARGUMENT_COUNT() /= 5) THEN
		PRINT *,'nanopart - Data driven particle model'
		PRINT *
		PRINT *,'Usage:'
		PRINT *
		PRINT *,'    nanopart <DataPath> <BeginDate> <NumDays> <ResultsPath> [<Debug>]'
		PRINT *
		PRINT *,'where'
		PRINT *
		PRINT *,'    <Debug> ::= WIND_QA | VERBOSE'
		PRINT *
		PRINT *,'This is open-source software, distributed under the MIT license'
		PRINT *
		PRINT *,'Written by: Mauri Favaron (University of Milan, Physics Department)'
		PRINT *,'Credits: Stefano Cesco, Roberta Vecchi'
		PRINT *
		STOP
	END IF
	CALL GET_COMMAND_ARGUMENT(1, sDataPath)
	CALL GET_COMMAND_ARGUMENT(2, sBeginDate)
	READ(sBeginDate, "(i4,1x,i2,1x,i2)") iBeginYear, iBeginMonth, iBeginDay
	CALL GET_COMMAND_ARGUMENT(3, sBuffer)
	READ(sBuffer, *) iNumDays
	CALL GET_COMMAND_ARGUMENT(4, sResultsPath)
	lDebug = (COMMAND_ARGUMENT_COUNT() == 5)
	IF(lDebug) THEN
		CALL GET_COMMAND_ARGUMENT(5, sDebugMode)
	END IF
	
	! Set up simulation
	iRetCode      = tWind % Initialize(10, sDataPath, iBeginYear, iBeginMonth, iBeginDay, iNumDays, lDebug)
	iRetCode      = tParticles % Initialize(rX0, rY0, iNx, iNy, rDx, rDy, rMaxZ, lDebug)
	iRetCode      = tConcentration % Initialize(sResultsPath, iNx, iNy, lDebug)
	iNumSources   = tParticles % GetNumSources()
	iNumParticles = tParticles % GetPoolSize()
	ALLOCATE( &
		rvMass(iNumSources), &
		rvU(iNumParticles), &
		rvV(iNumParticles), &
		rvW(iNumParticles), &
		rmConc(iNx, iNy), &
		rmAvg(iNx, iNy), &
		rmStdDev(iNx, iNy) &
	)
	rvMass = 1.
	
	! Do wind quality assurance computations, if requested
	IF(sDebugMode == "WIND_QA") THEN
		iRetCode = tWind % QualityCheck(10, "./QA.dat")
	END IF
	
	! Main loop: iterate over available wind
	DO
		iRetCode = tWind % Get(10, sDataPath, rU, rV, rW)
		IF(iRetCode < 0) EXIT
		IF(tWind % IsTimeToEmit()) iRetCode = tParticles % Emit(rvMass)
		iNumParticles = tParticles % GetNumber()
		iRetCode = tWind % Sample(iNumParticles, rvU, rvV, rvW)
		iRetCode = tParticles % Move(rvU, rvV, rvW)
		IF(tWind % IsTimeToEmit()) THEN
			rmConc = tParticles % LocateOnGrid()
			iRetCode = tConcentration % Add(rmConc)
		END IF
		IF(tWind % IsTimeToCount()) THEN
			iSimulationTime = tWind % GetTime(iYear, iMonth, iDay, iHour, iMinute, iSecond)
			iRetCode        = tConcentration % Stats(rmAvg, rmStdDev)
			WRITE(sOutFile, "(a,'/conc',i4.4,3i2.2,'.csv')") TRIM(sResultsPath), iYear, iMonth, iDay, iHour
			OPEN(10, FILE=sOutFile, STATUS='UNKNOWN', ACTION='WRITE')
			DO i = 1, iNx
				DO j = 1, iNy
					WRITE(10,"(i3,',',i3,2(',',e15.7))") i, j, rmAvg(i,j), rmStdDev(i,j)
				END DO
			END DO
			CLOSE(10)
		END IF
	END DO

	! Leave
	DEALLOCATE(rvMass, rvU, rvV, rvW, rmConc, rmAvg, rmStdDev)
	PRINT *,"*** End job."
	
END PROGRAM NanoPart

