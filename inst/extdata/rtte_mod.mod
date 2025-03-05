;; 1. Based on:
;; 2. Description: repeated time-to-event with weibull distribution
;; 3. Label:
$PROBLEM repeated time-to-event with weibull distribution
$INPUT REF ID STUDYIDN TYPE CMT EVID DV AMT ADDL II TSFDH TSFDD TSFDW TIME TSLDH DAY TRTN DOSEN SEXN RACEN RACEL RACE2N RACE3N RACE5N WTKGBL COSEAS EVCOUNT INCLFIRST LAMBDA SHAPE ETA4 ETA1 ETA2 ETA3 KA CL VC CMIN1 CMINSS ENDTIME TSFDDCUT TSEVD TSEVW INCL ADTM
; NOTE - DROP COLUMNS NEEDS TO BE ADDED MANUALLY
$DATA DAT-1a-PMX-RTTEWorkshop-PFPMX-1.csv
        IGNORE=@
; NOTE - ADDITIONAL IGNORE NEEDS TO BE ADDED MANUALLY
$SUBROUTINE ADVAN=13 TOL=9
$MODEL      COMP=(HAZARD)
$PK

IF (NEWIND.NE.2) TIMEP = 0	; New subject, reset previous event time

    LAM=THETA(1)*EXP(ETA(1)) ; Scale parameter
    GAM=THETA(2) ; Shape parameter
$DES

    DEL=1E-12 ;Protect against events as T=0
    DADT(1)=LAM*GAM*(LAM*(T-TIMEP)+DEL)**(GAM-1) ; Weibull distribution
$ERROR
DELX=1E-12 ;Protect against events as T=0

  IF (NEWIND.NE.2) OCHZ=0   ; Reset old cumulative hazard for new subjects

  IF(EVID.EQ.0) THEN     ; if event observation
    CHZ = A(1)-OCHZ      ; cumulative hazard from previous event time in dataset
    OCHZ = A(1)          ; store CHZ from previous event as OCHZ
    SUR = EXP(-CHZ)      ; survival prob
    HAZNOW = LAM*GAM*(LAM*(TIME-TIMEP)+DELX)**(GAM-1)  ; hazard at event time
    TIMEP = TIME ; save event time
  ENDIF

  IF (EVID.EQ.0.AND.DV.EQ.0) Y=SUR            ; censoring (survival probability)
  IF (EVID.EQ.0.AND.DV.NE.0) Y = SUR*HAZNOW   ; pdf of event variable
$THETA (0,1) ; Scale parameter (1/time unit)
$THETA (0,1) ; Shape parameter
$OMEGA 0.1 ; IIV of scale parameter
$ESTIMATION MAXEVAL=9999 METHOD=1 LAPLACE LIKELIHOOD SIGL=9 NSIG=3 PRINT=1
    MSFO=msfb_rtte_mod
$COVARIANCE PRINT=E MATRIX=R UNCONDITIONAL
$TABLE REF ID STUDYIDN TYPE CMT EVID DV AMT ADDL II TSFDH TSFDD TSFDW TIME TSLDH DAY TRTN DOSEN SEXN RACEN RACEL RACE2N RACE3N RACE5N WTKGBL COSEAS EVCOUNT INCLFIRST LAMBDA SHAPE ETA4 ETA1 ETA2 ETA3 KA CL VC CMIN1 CMINSS ENDTIME TSFDDCUT TSEVD TSEVW INCL ADTM
      NOPRINT ONEHEADER FILE=xptab_rtte_mod
