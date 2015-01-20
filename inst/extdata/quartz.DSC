#DESC	1.2 * DESCRIPTOR INFORMATION ***********************
*
*	Dataset Type and Format:
*
DSRC	EXP
BSEQ	BIG
IKKF	REAL
XTYP	IDX
YTYP	NODATA
ZTYP	NODATA
*
*	Item Formats:
*
IRFMT	D
*
*	Data Ranges and Resolutions:
*
XPTS	1024
XMIN	3450.000000
XWID	100.000000
*
*	Documentational Text:
*
TITL	'DoLe29b_17_Ti_103K_1scan_20mW.DSC'
IRNAM	'Intensity'
XNAM	'Field'
IRUNI	''
XUNI	'G'
*
************************************************************
*
#SPL	1.2 * STANDARD PARAMETER LAYER
*
OPER    xuser
DATE    02/20/14
TIME    15:28:45
CMNT    
SAMP    
SFOR    
STAG    C
EXPT    CW
OXS1    IADC
AXS1    B0VL
AXS2    NONE
AXS3    
A1CT    0.35
A1SW    0.01
MWFQ    9.447463e+09
MWPW    0.02
AVGS    1
RESO    shqew10604
SPTP    0.16384
RCAG    50
RCHM    1
B0MA    0.0001
B0MF    100000
RCPH    0.0
RCOF    0.0
A1RS    1024
RCTC    0.00512
STMP    103.2
*
************************************************************
*
#DSL	1.0 * DEVICE SPECIFIC LAYER
*

.DVC     acqStart, 1.0


.DVC     fieldCtrl, 1.0

AllegroMode        False
CenterField        3500.00 G
Delay              0.0 s
FieldFlyback       On
FieldWait          Wait LED off
GFactor            2.000000
SetToSampleG       False
StaticFieldMon     3350.000 G
SweepDirection     Up
SweepWidth         100.0 G

.DVC     fieldSweep, 1.0


.DVC     freqCounter, 1.0

FrequencyMon       9.447463 GHz
QMonitBridge       On

.DVC     mwBridge, 1.0

AcqFineTuning      Never
AcqScanFTuning     Off
AcqSliceFTuning    Off
BridgeCalib        100.0
Power              20.00 mW
PowerAtten         10 dB
QValue             5600

.DVC     recorder, 1.0

BaselineCorr       On
NbScansAcc         1
NbScansDone        1
NbScansToDo        1
ReplaceMode        On
SmoothMode         Auto
SmoothPoints       1

.DVC     scanEnd, 1.0


.DVC     signalChannel, 1.0

AFCTrap            True
AllegroMode        False
AllowShortCt       False
Calibrated         True
ConvTime           163.84 ms
DModAFCTrap        True
DModAmp            1.00 G
DModCalibrated     True
DModDetectSCT      First
DModEliDelay       1.0 us
DModExtLockIn      False
DModExtTrigger     False
DModFieldMod       First
DModGain           60 dB
DModHighPass       True
DModIntegrator     True
DModModOutput      Internal
DModSignalInput    Internal
DModTimeConst      1.28 ms
DoubleModFreq      5.00 kHz
DoubleModPhase     0.0
DoubleMode         False
EliDelay           1.0 us
EnableImag         Disable
ExtLockIn          False
ExtTrigger         False
Gain               50 dB
Harmonic           1
HighPass           True
Integrator         False
IsCalibExp         False
ModAmp             1.000 G
ModFreq            100.00 kHz
ModInput           Internal
ModOutput          Internal
ModPhase           0.0
Offset             0.0 %
QuadMode           False
QuadPhase          90.0
Resolution         1024
Resonator          1
SctNorm            False
SctRevision        DSC2
SignalInput        Internal
SpuExtension       False
SweepTime          167.77 s
TimeConst          5.12 ms
TimeExp            False
TuneCaps           37

.DVC     tempCtrl, 1.0

AcqWaitTime        5.0 s
Temperature        100.00 K
Tolerance          5.00 K

*
************************************************************
