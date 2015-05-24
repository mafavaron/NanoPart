! wind.f90 - Module, supporting wind reading from ultrasonic anemometer files
!            and window-based sampling.
!
! Written by: Mauri Favaron
!
MODULE Wind

	USE Calendar

	IMPLICIT NONE
	
	PRIVATE
	
	! Public interface
	PUBLIC	:: WindType
	
	! Steering parameters (change if required)
	INTEGER, PARAMETER	:: ONE_HOUR			= 3600	! s/h
	INTEGER, PARAMETER	:: EMISSION_PERIOD	=    1	! s/emission  (How often are particles emitted)
	INTEGER, PARAMETER	:: COUNT_PERIOD		= 3600	! s/averaging (How often are fields generated)
	INTEGER, PARAMETER	:: FREQUENCY		=   10	! Hz
	INTEGER, PARAMETER	:: WINDOW_DURATION	=   20	! s
	INTEGER, PARAMETER	:: WIND_POOL_SIZE	= FREQUENCY * 2 * ONE_HOUR
	INTEGER, PARAMETER	:: WINDOW_SIZE		= FREQUENCY * WINDOW_DURATION	! Must be strictly less than WIND_POOL_SIZE
	
	! Other useful constants (please do not change)
	INTEGER, PARAMETER	:: SVR_INFO    = 0
	INTEGER, PARAMETER	:: SVR_WARNING = 1
	INTEGER, PARAMETER	:: SVR_ERROR   = 2
	
	! Data types
	
	TYPE WindType
		! Type management "standard" variables
		LOGICAL, PRIVATE							:: lDebug
		CHARACTER(LEN=256), PRIVATE					:: sErrorMsg
		! Sonic data set
		REAL, PRIVATE, DIMENSION(WIND_POOL_SIZE)	:: U
		REAL, PRIVATE, DIMENSION(WIND_POOL_SIZE)	:: V
		REAL, PRIVATE, DIMENSION(WIND_POOL_SIZE)	:: W
		INTEGER, PRIVATE							:: iSize
		INTEGER, PRIVATE							:: iDataIndex
		! Buffer data set
		REAL, PRIVATE, DIMENSION(WINDOW_SIZE)		:: BufU
		REAL, PRIVATE, DIMENSION(WINDOW_SIZE)		:: BufV
		REAL, PRIVATE, DIMENSION(WINDOW_SIZE)		:: BufW
		INTEGER, PRIVATE							:: iBufSize
		INTEGER, PRIVATE							:: iBufIndexNextValue
		! Time support
		INTEGER, PRIVATE							:: iCurrentTime
		INTEGER, PRIVATE							:: iBaseTime
		INTEGER, PRIVATE							:: iNumDays
		INTEGER, PRIVATE							:: iNumStepsMade
		! Other auxiliaries
		CHARACTER(LEN=256), PRIVATE					:: sDataPath
	CONTAINS
		PROCEDURE	:: Initialize    => WindPoolClean
		PROCEDURE	:: ReadSonic     => ReadSonicFile
		PROCEDURE	:: Get           => GetWind
		PROCEDURE	:: Sample        => SampleWind
		PROCEDURE	:: GetWindowSize => GetWindowSize
		PROCEDURE	:: GetTime       => GetTime
		PROCEDURE	:: IsTimeToEmit  => IsTimeToEmit
		PROCEDURE	:: IsTimeToCount => IsTimeToCount
		PROCEDURE	:: QualityCheck  => QualityCheck
	END TYPE WindType
	
CONTAINS

	FUNCTION WindPoolClean(this, iLUN, sDataPath, iBeginYear, iBeginMonth, iBeginDay, iNumDays, lDebug) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(WindType)					:: this
		INTEGER, INTENT(IN)				:: iLUN
		CHARACTER(LEN=*), INTENT(IN)	:: sDataPath
		INTEGER, INTENT(IN)				:: iBeginYear
		INTEGER, INTENT(IN)				:: iBeginMonth
		INTEGER, INTENT(IN)				:: iBeginDay
		INTEGER, INTENT(IN)				:: iNumDays
		LOGICAL, INTENT(IN), OPTIONAL	:: lDebug
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER				:: iErrCode
		INTEGER				:: iYear
		INTEGER				:: iMonth
		INTEGER				:: iDay
		INTEGER				:: iHour
		INTEGER				:: iMinute
		INTEGER				:: iSecond
		INTEGER				:: iCurTime
		LOGICAL				:: lFileExists
		CHARACTER(LEN=256)	:: sFileName
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Get optional parameters
		IF(PRESENT(lDebug)) THEN
			this % lDebug = lDebug
		ELSE
			this % lDebug = .FALSE.
		END IF
		
		! Check constant values
		IF(WINDOW_SIZE >= WIND_POOL_SIZE) THEN
			iRetCode = 1
			this % sErrorMsg = "Wind sampling window size is larger than wind pool"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Check number of days to be compatible with INTEGER(4) signed number,
		! when expressed in tenths of a second (an INTEGER(4) is used to track
		! the number of tenths of seconds since the simulation start)
		IF(iNumDays <= 0 .OR. iNumDays > 6*366) THEN
			iRetCode = 1
			this % sErrorMsg = "A simulation cannot have length of 0 days or less, or longer than 6 years"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Iterate over all possible file names, and check all of them are present
		CALL PackTime(this % iBaseTime, iBeginYear, iBeginMonth, iBeginDay, 0, 0, 0)
		DO iCurTime = this % iBaseTime, this % iBaseTime + iNumDays * ONE_HOUR * 24 - 1, ONE_HOUR
			CALL UnpackTime(iCurTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			WRITE(sFileName, "(a,'/',i4.4,2i2.2,'.',i2.2,'.csv')") TRIM(sDataPath), iYear, iMonth, iDay, iHour
			INQUIRE(FILE=sFileName, EXIST=lFileExists)
			IF(.NOT.lFileExists) THEN
				iRetCode = 3
				this % sErrorMsg = "Expected data file, '" // TRIM(sFileName) // "', is missing from data set"
				CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
				RETURN
			END IF
		END DO
		IF(this % lDebug) &
			CALL ErrorReport( &
				SVR_INFO, &
				"All files expected have been found (this does not mean they contain valid data)" &
			)
		
		! All rights. Perform initialization, starting with wind
		this % U = 0.
		this % V = 0.
		this % W = 0.
		this % iCurrentTime  = this % iBaseTime
		this % iNumDays      = iNumDays
		this % iDataIndex    = 0
		this % sDataPath     = sDataPath
		this % iNumStepsMade = 0
		
		! Initialization of circular buffer
		this % BufU = 0.
		this % BufV = 0.
		this % BufW = 0.
		this % iBufIndexNextValue = 0
		this % iBufSize           = 0
		
		! Read the first sonic file
		WRITE(sFileName, "(a,'/',i4.4,2i2.2,'.',i2.2,'.csv')") TRIM(sDataPath), iBeginYear, iBeginMonth, iBeginDay, 0
		IF(this % lDebug) PRINT *,"New file: ",TRIM(sFileName)
		iErrCode = this % ReadSonic(iLUN, sFileName)
		IF(iErrCode /= 0) THEN
			iRetCode = 3
			this % sErrorMsg = "Error accessing first sonic file"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		
	END FUNCTION WindPoolClean
	
	
	FUNCTION ReadSonicFile(this, iLUN, sFileName) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(WindType)					:: this
		INTEGER, INTENT(IN)				:: iLUN
		CHARACTER(LEN=*), INTENT(IN)	:: sFileName
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER				:: iErrCode
		CHARACTER(LEN=64)	:: sBuffer
		REAL				:: rTimeStamp
		REAL				:: rU
		REAL				:: rV
		REAL				:: rW
		INTEGER				:: i
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Attempt reading file into wind circular buffer
		this % iSize = 0
		OPEN(iLUN, FILE=sFileName, STATUS='OLD', ACTION='READ', IOSTAT=iErrCode)
		IF(iErrCode /= 0) THEN
			iRetCode = 1
			this % sErrorMsg = "Impossible to open sonic file (maybe, it does not exist)"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		READ(iLUN, "(a)", IOSTAT=iErrCode) sBuffer
		IF(iErrCode /= 0) THEN
			iRetCode = 2
			CLOSE(iLUN)
			this % sErrorMsg = "Empty sonic file"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		DO i = 1, WIND_POOL_SIZE
			READ(iLUN, *, IOSTAT=iErrCode) rTimeStamp, rU, rV, rW
			IF(iErrCode /= 0) EXIT
			this % iSize = this % iSize + 1
			this % U(this % iSize) = rU
			this % V(this % iSize) = rV
			this % W(this % iSize) = rW
		END DO
		CLOSE(iLUN)
		
		! Reset data index
		IF(this % iSize <= 0) THEN
			iRetCode = 3
			this % sErrorMsg = "Sonic file contains only header information: no data"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		this % iDataIndex = 1
		
	END FUNCTION ReadSonicFile
	
	
	FUNCTION GetWind(this, iLUN, sDataPath, rU, rV, rW) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(WindType)					:: this
		INTEGER, INTENT(IN)			:: iLUN
		CHARACTER(LEN=*), INTENT(IN)	:: sDataPath
		REAL, INTENT(OUT)				:: rU
		REAL, INTENT(OUT)				:: rV
		REAL, INTENT(OUT)				:: rW
		INTEGER						:: iRetCode
		
		! Locals
		INTEGER			:: iErrCode
		CHARACTER(LEN=256)	:: sFileName
		INTEGER			:: iYear
		INTEGER			:: iMonth
		INTEGER			:: iDay
		INTEGER			:: iHour
		INTEGER			:: iMinute
		INTEGER			:: iSecond
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Prepare data counter to get the next wind vector; in case
		! it is larger than data size, load next file (and implicitly reset
		! data counter)
		this % iDataIndex = this % iDataIndex + 1
		IF(this % iDataIndex > this % iSize) THEN
			this % iCurrentTime = this % iCurrentTime + 3600
			CALL UnpackTime(this % iCurrentTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			IF(this % iCurrentTime >= this % iBaseTime + 24 * ONE_HOUR * this % iNumDays) THEN
				iRetCode = -1
				this % sErrorMsg = "** End of desired run **"
				IF(this % lDebug) CALL ErrorReport(SVR_INFO, TRIM(this % sErrorMsg))
				RETURN
			END IF
			WRITE(sFileName, "(a,'/',i4.4,2i2.2,'.',i2.2,'.csv')") TRIM(sDataPath), iYear, iMonth, iDay, iHour
			IF(this % lDebug) PRINT *,"New file: ",TRIM(sFileName)
			iErrCode = this % ReadSonic(iLUN, sFileName)
			IF(iErrCode /= 0) THEN
				iRetCode = 1
				this % sErrorMsg = "Error accessing sonic file (reason in previous line)"
				CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
				RETURN
			END IF
		END IF
		
		! Update step counter
		this % iNumStepsMade = this % iNumStepsMade + 1
		
		! Get wind at current index
		rU = this % U(this % iDataIndex)
		rV = this % V(this % iDataIndex)
		rW = this % W(this % iDataIndex)
		
		! Save wind to circular buffer
		this % iBufSize = MIN(this % iBufSize + 1, WINDOW_SIZE)
		this % iBufIndexNextValue = this % iBufIndexNextValue + 1
		IF(this % iBufIndexNextValue > WINDOW_SIZE) this % iBufIndexNextValue = 1
		this % BufU(this % iBufIndexNextValue) = rU
		this % BufV(this % iBufIndexNextValue) = rV
		this % BufW(this % iBufIndexNextValue) = rW
		
	END FUNCTION GetWind
	
	
	FUNCTION SampleWind(this, iSampleSize, rvU, rvV, rvW) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(WindType)					:: this
		INTEGER, INTENT(IN)				:: iSampleSize
		REAL, DIMENSION(:), INTENT(OUT)	:: rvU
		REAL, DIMENSION(:), INTENT(OUT)	:: rvV
		REAL, DIMENSION(:), INTENT(OUT)	:: rvW
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER, DIMENSION(iSampleSize)	:: ivSample
		REAL, DIMENSION(iSampleSize)	:: rvSample
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Check sample size to match vector sizes
		IF(iSampleSize > MIN(SIZE(rvU),SIZE(rvV),SIZE(rvW))) THEN
			iRetCode = 1
			this % sErrorMsg = "Desired sample size is larger than passed vector size(s)"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		
		! Extract sample, of the right size
		CALL RANDOM_NUMBER(rvSample)
		ivSample           = INT(this % iBufSize * rvSample) + 1
		rvU(1:iSampleSize) = this % BufU(ivSample)
		rvV(1:iSampleSize) = this % BufV(ivSample)
		rvW(1:iSampleSize) = this % BufW(ivSample)
		
	END FUNCTION SampleWind
	
	
	FUNCTION GetWindowSize(this) RESULT(iWindowSize)
	
		! Routine arguments
		CLASS(WindType)	:: this
		INTEGER			:: iWindowSize
		
		! Locals
		! -none-
		
		! Get the information required
		iWindowSize = WINDOW_SIZE
		
	END FUNCTION GetWindowSize
	
	
	FUNCTION GetTime(this, iYear, iMonth, iDay, iHour, iMinute, iSecond) RESULT(iSimulationTime)
	
		! Routine arguments
		CLASS(WindType)			:: this
		INTEGER, INTENT(OUT)	:: iYear
		INTEGER, INTENT(OUT)	:: iMonth
		INTEGER, INTENT(OUT)	:: iDay
		INTEGER, INTENT(OUT)	:: iHour
		INTEGER, INTENT(OUT)	:: iMinute
		INTEGER, INTENT(OUT)	:: iSecond
		INTEGER				:: iSimulationTime
		
		! Locals
		INTEGER	:: iSecondsSinceStart
		
		! Get the information required
		iSecondsSinceStart = this % iNumStepsMade / FREQUENCY
		iSimulationTime    = this % iBaseTime + iSecondsSinceStart
		CALL UnpackTime(iSimulationTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
		
	END FUNCTION GetTime
	
	
	FUNCTION IsTimeToEmit(this) RESULT(lIsTime)
	
		! Routine arguments
		CLASS(WindType)	:: this
		LOGICAL		:: lIsTime
		
		! Locals
		! -none-
		
		! Get the information required
		lIsTime = MOD(this % iNumStepsMade, FREQUENCY*EMISSION_PERIOD) == 0
		
	END FUNCTION IsTimeToEmit
	
	
	FUNCTION IsTimeToCount(this) RESULT(lIsTime)
	
		! Routine arguments
		CLASS(WindType)	:: this
		LOGICAL		:: lIsTime
		
		! Locals
		! -none-
		
		! Get the information required
		lIsTime = MOD(this % iNumStepsMade, FREQUENCY*COUNT_PERIOD) == 0
		
	END FUNCTION IsTimeToCount
	
	
	FUNCTION QualityCheck(this, iLUN, sOutFile) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(WindType)					:: this
		INTEGER, INTENT(IN)				:: iLUN
		CHARACTER(LEN=*), INTENT(IN)	:: sOutFile
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER				:: iErrCode
		INTEGER				:: iCurrentTime
		INTEGER				:: iYear, iMonth, iDay, iHour, iMinute, iSecond
		INTEGER				:: iSecondInHour
		CHARACTER(LEN=256)	:: sDataPath
		CHARACTER(LEN=256)	:: sFileName
		CHARACTER(LEN=256)	:: sBuffer
		INTEGER				:: iNumHours
		INTEGER				:: i
		INTEGER				:: iPos
		INTEGER				:: iData
		INTEGER				:: iMinDataPerSecond
		INTEGER				:: iMaxDataPerSecond
		REAL				:: rTime, rU, rV, rW
		LOGICAL				:: lIsTimeContinuous
		REAL				:: rNominalFrequency
		REAL				:: rActualFrequency
		INTEGER				:: iNumBlocks
		INTEGER				:: iBlock
		INTEGER				:: iBlockBegin
		INTEGER				:: iBlockEnd
		REAL				:: rMeanBlockU
		REAL				:: rMeanBlockV
		REAL				:: rMeanBlockW
		REAL				:: rMeanBlockUU
		REAL				:: rMeanBlockVV
		REAL				:: rMeanBlockWW
		REAL				:: rMeanBlockUV
		REAL				:: rMeanBlockUW
		REAL				:: rMeanBlockVW
		REAL				:: rStdDevBlockU
		REAL				:: rStdDevBlockV
		REAL				:: rStdDevBlockW
		REAL				:: rStdDevBlockUU
		REAL				:: rStdDevBlockVV
		REAL				:: rStdDevBlockWW
		REAL				:: rStdDevBlockUV
		REAL				:: rStdDevBlockUW
		REAL				:: rStdDevBlockVW
		REAL				:: rInfU
		REAL				:: rInfV
		REAL				:: rInfW
		REAL				:: rInfUU
		REAL				:: rInfVV
		REAL				:: rInfWW
		REAL				:: rInfUV
		REAL				:: rInfUW
		REAL				:: rInfVW
		REAL				:: rSupU
		REAL				:: rSupV
		REAL				:: rSupW
		REAL				:: rSupUU
		REAL				:: rSupVV
		REAL				:: rSupWW
		REAL				:: rSupUV
		REAL				:: rSupUW
		REAL				:: rSupVW
		REAL				:: rU0
		REAL				:: rV0
		REAL				:: rW0
		INTEGER				:: iOutU
		INTEGER				:: iOutV
		INTEGER				:: iOutW
		INTEGER				:: iOutUU
		INTEGER				:: iOutVV
		INTEGER				:: iOutWW
		INTEGER				:: iOutUV
		INTEGER				:: iOutUW
		INTEGER				:: iOutVW
		INTEGER				:: iOutTotal
		INTEGER, DIMENSION(:), ALLOCATABLE	:: ivTimeStamp
		LOGICAL, DIMENSION(:), ALLOCATABLE	:: lvFileExists
		INTEGER, DIMENSION(:), ALLOCATABLE	:: ivNumData
		REAL, DIMENSION(:), ALLOCATABLE		:: rvNominalFrequency
		REAL, DIMENSION(:), ALLOCATABLE		:: rvActualFrequency
		LOGICAL, DIMENSION(:), ALLOCATABLE	:: lvIsTimeContinuous
		INTEGER, DIMENSION(:), ALLOCATABLE	:: ivNonStationarityScore
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockUU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockVV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockWW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockUV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockUW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevBlockVW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvVectorVel
		REAL, DIMENSION(:), ALLOCATABLE		:: rvScalarVel
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanResU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanResV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanResW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevResU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevResV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvStdDevResW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvSkewU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvSkewV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvSkewW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvKurtU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvKurtV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvKurtW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvTime
		REAL, DIMENSION(:), ALLOCATABLE		:: rvU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvResU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvResV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvResW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvMeanW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvVel
		REAL, DIMENSION(:), ALLOCATABLE		:: rvDir
		INTEGER, DIMENSION(3600)			:: ivDataPerSecond
		INTEGER, DIMENSION(:), ALLOCATABLE	:: ivDataFrequency
		INTEGER, DIMENSION(1)				:: ivPos
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockUU
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockVV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockWW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockUV
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockUW
		REAL, DIMENSION(:), ALLOCATABLE		:: rvBlockVW
		
		! Internal constants
		REAL, PARAMETER	:: CRITICAL_T = 1.65
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Reserve workspace
		iNumHours = 24 * this % iNumDays
		ALLOCATE(& 
			ivTimeStamp(iNumHours), &
			lvFileExists(iNumHours), &
			ivNumData(iNumHours), &
			rvNominalFrequency(iNumHours), &
			rvActualFrequency(iNumHours), &
			lvIsTimeContinuous(iNumHours), &
			ivNonStationarityScore(iNumHours), &
			rvStdDevBlockU(iNumHours), &
			rvStdDevBlockV(iNumHours), &
			rvStdDevBlockW(iNumHours), &
			rvStdDevBlockUU(iNumHours), &
			rvStdDevBlockVV(iNumHours), &
			rvStdDevBlockWW(iNumHours), &
			rvStdDevBlockUV(iNumHours), &
			rvStdDevBlockUW(iNumHours), &
			rvStdDevBlockVW(iNumHours), &
			rvVectorVel(iNumHours), &
			rvScalarVel(iNumHours), &
			rvMeanResU(iNumHours), &
			rvMeanResV(iNumHours), &
			rvMeanResW(iNumHours), &
			rvStdDevResU(iNumHours), &
			rvStdDevResV(iNumHours), &
			rvStdDevResW(iNumHours), &
			rvSkewU(iNumHours), &
			rvSkewV(iNumHours), &
			rvSkewW(iNumHours), &
			rvKurtU(iNumHours), &
			rvKurtV(iNumHours), &
			rvKurtW(iNumHours), &
			STAT=iErrCode &
		)
		IF(iErrCode /= 0) THEN
			iRetCode = 1
			this % sErrorMsg = "Impossible to allocate workspace for data quality check"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		
		! Set allocated vectors initial values
		ivTimeStamp            = 0
		lvFileExists           = .FALSE.
		ivNumData              = 0
		rvNominalFrequency     = -9999.9
		rvActualFrequency      = -9999.9
		lvIsTimeContinuous     = .FALSE.
		ivNonStationarityScore = -9999
		rvStdDevBlockU         = -9999.9
		rvStdDevBlockV         = -9999.9
		rvStdDevBlockW         = -9999.9
		rvStdDevBlockUU        = -9999.9
		rvStdDevBlockVV        = -9999.9
		rvStdDevBlockWW        = -9999.9
		rvStdDevBlockUV        = -9999.9
		rvStdDevBlockUW        = -9999.9
		rvStdDevBlockVW        = -9999.9
		rvVectorVel            = -9999.9
		rvScalarVel            = -9999.9
		rvMeanResU             = -9999.9
		rvMeanResV             = -9999.9
		rvMeanResW             = -9999.9
		rvStdDevResU           = -9999.9
		rvStdDevResV           = -9999.9
		rvStdDevResW           = -9999.9
		rvSkewU                = -9999.9
		rvSkewV                = -9999.9
		rvSkewW                = -9999.9
		rvKurtU                = -9999.9
		rvKurtV                = -9999.9
		rvKurtW                = -9999.9
		
		! Inspect data files in time sequence
		i = 0
		DO iCurrentTime = this % iBaseTime, this % iBaseTime + 24*3600 * this % iNumDays - 3600, 3600
		
			! Build file name
			CALL UnpackTime(iCurrentTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
			WRITE(sFileName, "(a,'/',i4.4,2i2.2,'.',i2.2,'.csv')") TRIM(this % sDataPath), iYear, iMonth, iDay, iHour
			i = i + 1
			ivTimeStamp(i) = iCurrentTime
			IF(this % lDebug) PRINT *,"Evaluating file: ",TRIM(sFileName)
			
			! Count data in file, and use this information to reserve workspace
			lvFileExists(i) = .TRUE.
			ivNumData(i)    = -1	! Account for header
			OPEN(iLUN, FILE=sFileName, STATUS='OLD', ACTION='READ', IOSTAT=iErrCode)
			IF(iErrCode /= 0) THEN
				print *,trim(sFileName)
				lvFileExists(i) = .FALSE.
				CYCLE
			END IF
			DO
				READ(iLUN, "(a)", IOSTAT=iErrCode) sBuffer
				IF(iErrCode /= 0) EXIT
				READ(sBuffer, *, IOSTAT=iErrCode) rTime, rU, rV, rW
				IF(iErrCode /= 0) CYCLE
				ivNumData(i) = ivNumData(i) + 1
			END DO
			IF(ivNumData(i) > 0) THEN
				
				! Reserve workspace
				ALLOCATE( &
					rvTime(ivNumData(i)), &
					rvU(ivNumData(i)), &
					rvV(ivNumData(i)), &
					rvW(ivNumData(i)), &
					rvResU(ivNumData(i)), &
					rvResV(ivNumData(i)), &
					rvResW(ivNumData(i)), &
					rvMeanU(ivNumData(i)), &
					rvMeanV(ivNumData(i)), &
					rvMeanW(ivNumData(i)), &
					rvVel(ivNumData(i)), &
					rvDir(ivNumData(i)), &
					STAT=iErrCode &
				)
				IF(iErrCode /= 0) THEN
					iRetCode = 2
					this % sErrorMsg = "Impossible to allocate workspace for data quality check"
					CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
					RETURN
				END IF
				
				! Fill workspace with actual data
				REWIND(iLUN)
				READ(iLUN, "(a)") sBuffer
				iData = 0
				rU0 = 0.
				rV0 = 0.
				rW0 = 0.
				DO
				
					! Get wind data
					READ(iLUN, "(a)", IOSTAT=iErrCode) sBuffer
					IF(iErrCode /= 0) EXIT
					READ(sBuffer, *, IOSTAT=iErrCode) rTime, rU, rV, rW
					IF(iErrCode /= 0) CYCLE
					iData = iData + 1
					rvTime(iData) = rTime
					rvU(iData)    = rU
					rvV(iData)    = rV
					rvW(iData)    = rW
					
					! Cope with invalid data
					IF(rvU(iData) > -9999. .AND. rvV(iData) > -9999. .AND. rvW(iData) > -9999.) THEN
					
						! Compute vector horizontal speed and direction
						rvVel(iData) = SQRT(rvU(iData)**2 + rvV(iData)**2)
						rvDir(iData) = 180./3.1415927 * ATAN2(-rvU(iData),-rvV(iData))
						
						! Apply mcMillen filter with beta=0.02 to velocity components
						rvMeanU(iData) = 0.98*rU0 + 0.02*rvU(iData)
						rvMeanV(iData) = 0.98*rV0 + 0.02*rvV(iData)
						rvMeanW(iData) = 0.98*rW0 + 0.02*rvW(iData)
						rU0            = rvMeanU(iData)
						rV0            = rvMeanV(iData)
						rW0            = rvMeanW(iData)
						rvResU(iData)  = rvU(iData) - rvMeanU(iData)
						rvResV(iData)  = rvV(iData) - rvMeanV(iData)
						rvResW(iData)  = rvW(iData) - rvMeanW(iData)
						
					ELSE
					
						rvU(iData)     = -9999.9
						rvV(iData)     = -9999.9
						rvW(iData)     = -9999.9
						
						rvVel(iData)   = -9999.9
						rvDir(iData)   = -9999.9
						rvResU(iData)  = -9999.9
						rvResV(iData)  = -9999.9
						rvResW(iData)  = -9999.9
						rvMeanU(iData) = -9999.9
						rvMeanV(iData) = -9999.9
						rvMeanW(iData) = -9999.9
					
					END IF
					
				END DO
				
				! Compute time-continuity index
				lIsTimeContinuous = .TRUE.
				DO iData = 2, ivNumData(i)
					IF(rvTime(iData) - rvTime(iData-1) > 1.0) THEN
						lIsTimeContinuous = .FALSE.
						EXIT
					END IF
				END DO
				
				! Infer nominal and actual frequency from data time stamps
				ivDataPerSecond = 0
				DO iData = 1, ivNumData(i)
					iPos = FLOOR(rvTime(iData)) + 1
					IF(iPos >= 1 .AND. iPos <= 3600) THEN
						ivDataPerSecond(iPos) = ivDataPerSecond(iPos) + 1
					END IF
				END DO
				iMinDataPerSecond = MINVAL(ivDataPerSecond)
				iMaxDataPerSecond = MAXVAL(ivDataPerSecond)
				ALLOCATE(ivDataFrequency(iMinDataPerSecond:iMaxDataPerSecond), STAT=iErrCode)
				IF(iErrCode /= 0) THEN
					iRetCode = 3
					this % sErrorMsg = "Impossible to allocate workspace for data quality check"
					CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
					RETURN
				END IF
				ivDataFrequency = 0
				DO iSecondInHour = 1, 3600
					ivDataFrequency(ivDataPerSecond(iSecondInHour)) = ivDataFrequency(ivDataPerSecond(iSecondInHour)) + 1
				END DO
				ivPos = MAXLOC(ivDataFrequency)
				rNominalFrequency = FLOAT(ivPos(1) + iMinDataPerSecond - 1)
				rActualFrequency  = FLOAT(ivNumData(i)) / 3600.
				DEALLOCATE(ivDataFrequency)
				rvNominalFrequency(i) = rNominalFrequency
				rvActualFrequency(i)  = rActualFrequency
				lvIsTimeContinuous(i) = lIsTimeContinuous
				
				! Compute stationarity indices: actually, only weak stationarity
				! of order 2 is checked
				iNumBlocks = ivNumData(i) / 200 + 1
				IF(iNumBlocks > 0) THEN
					ALLOCATE( &
						rvBlockU(iNumBlocks), &
						rvBlockV(iNumBlocks), &
						rvBlockW(iNumBlocks), &
						rvBlockUU(iNumBlocks), &
						rvBlockVV(iNumBlocks), &
						rvBlockWW(iNumBlocks), &
						rvBlockUV(iNumBlocks), &
						rvBlockUW(iNumBlocks), &
						rvBlockVW(iNumBlocks), &
						STAT = iErrCode &
					)
					IF(iErrCode /= 0) THEN
						iRetCode = 4
						this % sErrorMsg = "Impossible to allocate workspace for stationarity evaluation"
						CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
						RETURN
					END IF
					! -1- Compute block means and (co-)variances
					iBlockBegin = 1
					iBlockEnd   = MIN(iBlockBegin + 200 - 1, ivNumData(i))
					DO iBlock = 1, iNumBlocks
						rvBlockU(iBlock)  = SUM(rvU(iBlockBegin:iBlockEnd), MASK=rvU(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvU(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockV(iBlock)  = SUM(rvV(iBlockBegin:iBlockEnd), MASK=rvV(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvV(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockW(iBlock)  = SUM(rvW(iBlockBegin:iBlockEnd), MASK=rvW(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvW(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockUU(iBlock) = SUM( &
							(rvU(iBlockBegin:iBlockEnd)-rvBlockU(iBlock))**2, MASK=rvU(iBlockBegin:iBlockEnd) > -9990.) / &
								COUNT(rvU(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockVV(iBlock) = SUM( &
							(rvV(iBlockBegin:iBlockEnd)-rvBlockV(iBlock))**2, MASK=rvV(iBlockBegin:iBlockEnd) > -9990.) / &
								COUNT(rvV(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockWW(iBlock) = SUM( &
							(rvW(iBlockBegin:iBlockEnd)-rvBlockW(iBlock))**2, MASK=rvW(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvW(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockUV(iBlock) = SUM( &
							(rvU(iBlockBegin:iBlockEnd)-rvBlockU(iBlock))* &
							(rvV(iBlockBegin:iBlockEnd)-rvBlockV(iBlock)), &
								MASK=rvU(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvU(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockUW(iBlock) = SUM( &
							(rvU(iBlockBegin:iBlockEnd)-rvBlockU(iBlock))* &
							(rvW(iBlockBegin:iBlockEnd)-rvBlockW(iBlock)), &
								MASK=rvU(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvU(iBlockBegin:iBlockEnd) > -9990.)
						rvBlockVW(iBlock) = SUM( &
							(rvV(iBlockBegin:iBlockEnd)-rvBlockV(iBlock))* &
							(rvW(iBlockBegin:iBlockEnd)-rvBlockW(iBlock)), &
								MASK=rvV(iBlockBegin:iBlockEnd) > -9990.) / &
							COUNT(rvW(iBlockBegin:iBlockEnd) > -9990.)
						iBlockBegin = iBlockBegin + 200
						iBlockEnd   = MIN(iBlockEnd + 200, ivNumData(i))
					END DO
					! -1- Compute standard deviation of block means and (co-)variances
					rMeanBlockU    = SUM(rvBlockU) / iNumBlocks
					rMeanBlockV    = SUM(rvBlockV) / iNumBlocks
					rMeanBlockW    = SUM(rvBlockW) / iNumBlocks
					rMeanBlockUU   = SUM(rvBlockUU) / iNumBlocks
					rMeanBlockVV   = SUM(rvBlockVV) / iNumBlocks
					rMeanBlockWW   = SUM(rvBlockWW) / iNumBlocks
					rMeanBlockUV   = SUM(rvBlockUv) / iNumBlocks
					rMeanBlockUW   = SUM(rvBlockUW) / iNumBlocks
					rMeanBlockVW   = SUM(rvBlockVW) / iNumBlocks
					rStdDevBlockU  = SQRT(SUM((rvBlockU-rMeanBlockU)**2) / iNumBlocks)
					rStdDevBlockV  = SQRT(SUM((rvBlockV-rMeanBlockV)**2) / iNumBlocks)
					rStdDevBlockW  = SQRT(SUM((rvBlockW-rMeanBlockW)**2) / iNumBlocks)
					rStdDevBlockUU = SQRT(SUM((rvBlockUU-rMeanBlockUU)**2) / iNumBlocks)
					rStdDevBlockVV = SQRT(SUM((rvBlockVV-rMeanBlockVV)**2) / iNumBlocks)
					rStdDevBlockWW = SQRT(SUM((rvBlockWW-rMeanBlockWW)**2) / iNumBlocks)
					rStdDevBlockUV = SQRT(SUM((rvBlockUV-rMeanBlockUV)**2) / iNumBlocks)
					rStdDevBlockUW = SQRT(SUM((rvBlockUW-rMeanBlockUW)**2) / iNumBlocks)
					rStdDevBlockVW = SQRT(SUM((rvBlockVW-rMeanBlockVW)**2) / iNumBlocks)
					! -1- Perform a statistical test of equality of variances and covariances
					rInfU  = rMeanBlockU - CRITICAL_T*rStdDevBlockU/SQRT(FLOAT(iNumBlocks))
					rSupU  = rMeanBlockU + CRITICAL_T*rStdDevBlockU/SQRT(FLOAT(iNumBlocks))
					iOutU  = COUNT(rvBlockU < rInfU .OR. rvBlockU > rSupU)
					rInfV  = rMeanBlockV - CRITICAL_T*rStdDevBlockV/SQRT(FLOAT(iNumBlocks))
					rSupV  = rMeanBlockV + CRITICAL_T*rStdDevBlockV/SQRT(FLOAT(iNumBlocks))
					iOutV  = COUNT(rvBlockV < rInfV .OR. rvBlockV > rSupV)
					rInfW  = rMeanBlockW - CRITICAL_T*rStdDevBlockW/SQRT(FLOAT(iNumBlocks))
					rSupW  = rMeanBlockW + CRITICAL_T*rStdDevBlockW/SQRT(FLOAT(iNumBlocks))
					iOutW  = COUNT(rvBlockW < rInfW .OR. rvBlockW > rSupW)
					rInfUU = rMeanBlockUU - CRITICAL_T*rStdDevBlockUU/SQRT(FLOAT(iNumBlocks))
					rSupUU = rMeanBlockUU + CRITICAL_T*rStdDevBlockUU/SQRT(FLOAT(iNumBlocks))
					iOutUU = COUNT(rvBlockUU < rInfUU .OR. rvBlockUU > rSupU)
					rInfVV = rMeanBlockVV - CRITICAL_T*rStdDevBlockVV/SQRT(FLOAT(iNumBlocks))
					rSupVV = rMeanBlockVV + CRITICAL_T*rStdDevBlockVV/SQRT(FLOAT(iNumBlocks))
					iOutVV = COUNT(rvBlockVV < rInfVV .OR. rvBlockVV > rSupVV)
					rInfWW = rMeanBlockWW - CRITICAL_T*rStdDevBlockWW/SQRT(FLOAT(iNumBlocks))
					rSupWW = rMeanBlockWW + CRITICAL_T*rStdDevBlockWW/SQRT(FLOAT(iNumBlocks))
					iOutWW = COUNT(rvBlockWW < rInfWW .OR. rvBlockWW > rSupWW)
					rInfUV = rMeanBlockUV - CRITICAL_T*rStdDevBlockUV/SQRT(FLOAT(iNumBlocks))
					rSupUV = rMeanBlockUV + CRITICAL_T*rStdDevBlockUV/SQRT(FLOAT(iNumBlocks))
					iOutUV = COUNT(rvBlockUV < rInfUV .OR. rvBlockUV > rSupUV)
					rInfUW = rMeanBlockUW - CRITICAL_T*rStdDevBlockUW/SQRT(FLOAT(iNumBlocks))
					rSupUW = rMeanBlockUW + CRITICAL_T*rStdDevBlockUW/SQRT(FLOAT(iNumBlocks))
					iOutUW = COUNT(rvBlockUW < rInfUW .OR. rvBlockUW > rSupUW)
					rInfVW = rMeanBlockVW - CRITICAL_T*rStdDevBlockVW/SQRT(FLOAT(iNumBlocks))
					rSupVW = rMeanBlockVW + CRITICAL_T*rStdDevBlockVW/SQRT(FLOAT(iNumBlocks))
					iOutVW = COUNT(rvBlockVW < rInfVW .OR. rvBlockVW > rSupVW)
					iOutTotal = &
						SIGN(1,iOutU) + &
						SIGN(1,iOutV) + &
						SIGN(1,iOutW) + &
						SIGN(1,iOutUU) + &
						SIGN(1,iOutVV) + &
						SIGN(1,iOutWW) + &
						SIGN(1,iOutUV) + &
						SIGN(1,iOutUW) + &
						SIGN(1,iOutVW)
					ivNonStationarityScore(i) = iOutTotal
					rvStdDevBlockU(i) = rStdDevBlockU
					rvStdDevBlockV(i) = rStdDevBlockV
					rvStdDevBlockW(i) = rStdDevBlockW
					rvStdDevBlockUU(i) = rStdDevBlockUU
					rvStdDevBlockVV(i) = rStdDevBlockVV
					rvStdDevBlockWW(i) = rStdDevBlockWW
					rvStdDevBlockUV(i) = rStdDevBlockUV
					rvStdDevBlockUW(i) = rStdDevBlockUW
					rvStdDevBlockVW(i) = rStdDevBlockVW
					rvVectorVel(i) = SQRT(SUM(rvU, MASK=rvU>-9999.0)**2 + SUM(rvV, MASK=rvU>-9999.0)**2) / COUNT(rvU>-9999.0)
					rvScalarVel(i) = SUM(rvVel, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0)
					rvMeanResU(i) = SUM(rvResU, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0)
					rvMeanResV(i) = SUM(rvResV, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0)
					rvMeanResW(i) = SUM(rvResW, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0)
					rvStdDevResU(i) = SQRT(SUM((rvResU - rvMeanResU(i))**2, MASK=rvU>-9999.0) / (COUNT(rvU>-9999.0) - 1))
					rvStdDevResV(i) = SQRT(SUM((rvResV - rvMeanResV(i))**2, MASK=rvU>-9999.0) / (COUNT(rvU>-9999.0) - 1))
					rvStdDevResW(i) = SQRT(SUM((rvResW - rvMeanResW(i))**2, MASK=rvU>-9999.0) / (COUNT(rvU>-9999.0) - 1))
					rvSkewU(i) = (SUM((rvResU - rvMeanResU(i))**3, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResU(i)**3
					rvSkewV(i) = (SUM((rvResV - rvMeanResV(i))**3, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResV(i)**3
					rvSkewW(i) = (SUM((rvResW - rvMeanResW(i))**3, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResW(i)**3
					rvKurtU(i) = (SUM((rvResU - rvMeanResU(i))**4, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResU(i)**4 - 3.
					rvKurtV(i) = (SUM((rvResV - rvMeanResV(i))**4, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResV(i)**4 - 3.
					rvKurtW(i) = (SUM((rvResW - rvMeanResW(i))**4, MASK=rvU>-9999.0) / COUNT(rvU>-9999.0))/rvStdDevResW(i)**4 - 3.
					
					DEALLOCATE( &
						rvBlockU, &
						rvBlockV, &
						rvBlockW, &
						rvBlockUU, &
						rvBlockVV, &
						rvBlockWW, &
						rvBlockUV, &
						rvBlockUW, &
						rvBlockVW &
					)
				END IF
				
				! Clean up
				DEALLOCATE( &
					rvTime, &
					rvU, &
					rvV, &
					rvW, &
					rvResU, &
					rvResV, &
					rvResW, &
					rvMeanU, &
					rvMeanV, &
					rvMeanW, &
					rvVel, &
					rvDir &
				)
				
			END IF
			CLOSE(iLUN)
			
		END DO
		
		! Evaluate results, and stop if conditions are not mature for processing
		! (saying something on "which")
		IF(ANY(lvFileExists==.FALSE.)) THEN
			iRetCode = 2
			this % sErrorMsg = "Some data files do not exist"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		IF(ANY(ivNumData <= 0)) THEN
			iRetCode = 3
			this % sErrorMsg = "Some data file contains no data"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		IF(ANY( &
			MAX(rvActualFrequency,rvNominalFrequency)-MIN(rvActualFrequency,rvNominalFrequency) > &
			0.5*MIN(rvActualFrequency,rvNominalFrequency) &
		)) THEN
			iRetCode = 4
			this % sErrorMsg = "Some data files do not exist"
			CALL ErrorReport(SVR_ERROR, TRIM(this % sErrorMsg))
			RETURN
		END IF
		
		! Write evaluation results
		OPEN(iLUN, FILE=sOutFile, STATUS="UNKNOWN", ACTION="WRITE")
		WRITE(iLUN, "(a,26(',',a))") &
			'T.Stamp', &
			'F', 'Non.Stat.Score', &
			'ns.U', 'ns.V', 'ns.W', &
			'ns.UU', 'ns.VV', 'ns.WW', &
			'ns.UV', 'ns.UW', 'ns.VW', &
			'Vel', 'S.Vel', 'Vel.R', &
			'Mean.Res.U', 'StdDev.Res.U', 'Skewness.Res.U', 'Excess.Kurtosis.Res.U', &
			'Mean.Res.V', 'StdDev.Res.V', 'Skewness.Res.V', 'Excess.Kurtosis.Res.V', &
			'Mean.Res.W', 'StdDev.Res.W', 'Skewness.Res.W', 'Excess.Kurtosis.Res.W'
		DO i = 1, iNumHours
			CALL UnpackTime(ivTimeStamp(i), iYear, iMonth, iDay, iHour, iMinute, iSecond)
			WRITE(iLUN, "(1x,i4.4,2('-',i2.2),1x,i2.2,':00:00,',f7.1,',',i5,24(',',f9.3))") &
				iYear, iMonth, iDay, iHour, &
				rvNominalFrequency(i), &
				ivNonStationarityScore(i), &
				rvStdDevBlockU(i), &
				rvStdDevBlockV(i), &
				rvStdDevBlockW(i), &
				rvStdDevBlockUU(i), &
				rvStdDevBlockVV(i), &
				rvStdDevBlockWW(i), &
				rvStdDevBlockUV(i), &
				rvStdDevBlockUW(i), &
				rvStdDevBlockVW(i), &
				rvVectorVel(i), &
				rvScalarVel(i), &
				rvVectorVel(i)/rvScalarVel(i), &
				rvMeanResU(i), &
				rvStdDevResU(i), &
				rvSkewU(i), &
				rvKurtU(i), &
				rvMeanResV(i), &
				rvStdDevResV(i), &
				rvSkewV(i), &
				rvKurtV(i), &
				rvMeanResW(i), &
				rvStdDevResW(i), &
				rvSkewW(i), &
				rvKurtW(i)
				
		END DO
		CLOSE(iLUN)
		
	END FUNCTION QualityCheck
	
	! *********************
	! * Internal routines *
	! *********************
	
	SUBROUTINE ErrorReport(iSeverity, sErrMsg)
	
		! Routine arguments
		INTEGER, INTENT(IN)				:: iSeverity
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

END MODULE Wind
