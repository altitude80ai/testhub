      ********************************************************
      *            PPHOLDAR (HOLD AREA) COPYBOOK             *
      *                   FOR IPPS PRICER                    *
      *------------------------------------------------------*
      *  FIRST CREATED FOR THE JULY 2015 UPDATE              *
      *  (VERSION 2015.4)                                    *
      *  TO BE USED IN ALL SUBSEQUENT PRICER RELEASES        *
      *  UPDATE PER CHANGES REQUESTED FOR PRICER             *
      *------------------------------------------------------*
      *                                                      *
      *  COPYBOOK CREATED THROUGH ALTERATION OF IPPS PRICER  *
      *  PROGRAMS SO AS TO FACILITATE FISS INSTALLATION OF   *
      *  IPPS PRICER. CONTAINS PAYMENT RELATED VARIABLES     *
      *  POPULATED IN PPCAL***.                              *
      *                                                      *
      *  SUMMARY OF ASSOCIATED CHANGES:                      *
      *    1. COMMENT OUT THE 'HOLD-AREA' RECORD IN PPCAL*** *
      *       AND MOVE IT INTO A NEW COPYBOOK (PPHOLDAR).    *
      *    2. COPY THIS COPYBOOK INTO THE WORKING-STORAGE    *
      *       AREA OF THE PPMGR*** MODULE.                   *
      *    3. COPY THIS COPYBOOK INTO THE LINKAGE AREA OF    *
      *       PPOPN***, PPDRV***, AND THE PPCAL*** MODULES.  *
      *    4. INITIALIZE THE PPHOLDAR AREA IN PPDRV** PRIOR  *
      *       TO CALLING THE PPCAL*** MODULE.                *
      *    5. CHANGE THE CALL/USING STATEMENTS TO INCLUDE    *
      *       THIS PPHOLDAR AREA.                            *
      *                                                      *
      ********************************************************
       01  PPHOLDAR-HOLD-AREA.
           02  HOLD-PPS-COMPONENTS.
               05  H-OPER-SHARE-DOLL-THRESHOLD  PIC 9(07)V9(09).
               05  H-CAPI-SHARE-DOLL-THRESHOLD  PIC 9(07)V9(09).
               05  H-OPER-HSP-PART              PIC 9(06)V9(09).
               05  H-CAPI-HSP-PART              PIC 9(06)V9(09).
               05  H-OPER-FSP-PART              PIC 9(06)V9(09).
               05  H-CAPI-FSP-PART              PIC 9(06)V9(09).
               05  H-CAPI2-B-FSP-PART           PIC 9(06)V9(09).
               05  H-OPER-OUTLIER-PART          PIC 9(07)V9(09).
               05  H-CAPI-OUTLIER-PART          PIC 9(07)V9(09).
               05  H-CAPI2-B-OUTLIER-PART       PIC 9(07)V9(09).
               05  H-OPER-OUTDAY-PART           PIC 9(07)V9(09).
               05  H-CAPI-OUTDAY-PART           PIC 9(07)V9(09).
               05  H-OPER-OUTCST-PART           PIC 9(07)V9(09).
               05  H-CAPI-OUTCST-PART           PIC 9(07)V9(09).
               05  H-OPER-CSTCHG-RATIO          PIC 9(01)V9(03).
               05  H-CAPI-CSTCHG-RATIO          PIC 9(01)V9(03).
               05  H-OPER-IME-TEACH             PIC 9(06)V9(09).
               05  H-CAPI-PAYCDE-PCT1           PIC 9(01)V9(02).
               05  H-CAPI-PAYCDE-PCT2           PIC 9(01)V9(02).
               05  H-CAPI-COST-OUTLIER          PIC 9(07)V9(09).
               05  H-CAPI-BILL-COSTS            PIC 9(07)V9(09).
               05  H-CAPI-DOLLAR-THRESHOLD      PIC 9(07)V9(09).
               05  H-CAPI-COLA                  PIC 9(01)V9(03).
               05  H-CAPI-SCH                   PIC 9(05)V9(02).
               05  H-CAPI-BUD-NEUTRALITY        PIC 9(01)V9(04).
               05  H-CAPI-OLD-HARMLESS          PIC 9(09)V9(02).
               05  H-CAPI-FED-RATE              PIC 9(05)V9(04).
               05  H-CAPI-FULL-PROS             PIC 9(05)V9(04).
               05  H-CAPI-LARG-URBAN            PIC 9(01)V9(02).
               05  H-CAPI-GAF                   PIC 9(05)V9(04).
               05  H-PR-CAPI-GAF                PIC 9(05)V9(04).
               05  H-BLEND-GAF                  PIC 9(05)V9(04).
               05  H-WAGE-INDEX                 PIC 9(02)V9(04).
               05  H-COV-DAYS                   PIC 9(3).
               05  H-PERDIEM-DAYS               PIC 9(3).
               05  H-REG-DAYS                   PIC 9(3).
               05  H-LTR-DAYS                   PIC 9(3).
               05  H-DSCHG-FRCTN                PIC 9(3)V9999.
               05  H-DRG-WT-FRCTN               PIC 9(2)V9999.
               05  H-ALOS                       PIC 9(02)V9(01).
               05  H-DAYS-CUTOFF                PIC 9(02)V9(01).
               05  H-DAYOUT-PCT                 PIC 9(01)V9(02).
               05  H-CSTOUT-PCT                 PIC 9(01)V9(02).
               05  H-CST-THRESH                 PIC 9(05)V9(02).
               05  H-OPER-BASE                  PIC 9(05)V9(02).
               05  H-CAPI-BASE                  PIC 9(05)V9(02).
               05  H-OPER-BILL-STDZ-COSTS       PIC 9(07)V9(02).
               05  H-CAPI-BILL-STDZ-COSTS       PIC 9(07)V9(02).
               05  H-OPER-STDZ-COST-OUTLIER     PIC 9(07)V9(09).
               05  H-CAPI-STDZ-COST-OUTLIER     PIC 9(07)V9(09).
               05  H-OPER-STDZ-DOLLAR-THRESHOLD PIC 9(07)V9(09).
               05  H-CAPI-STDZ-DOLLAR-THRESHOLD PIC 9(07)V9(02).
               05  H-STANDARD-ALLOWED-AMOUNT    PIC 9(07)V9(02).
               05  H-EHR-SUBS                   PIC 9(05)V9(02).
               05  H-PRE-CAPI-THRESH            PIC 9(05)V9(02).
               05  H-BUDG-NUTR01                PIC 9(01)V9(06).
               05  H-BUDG-NUTR02                PIC 9(01)V9(06).
               05  H-BUDG-NUTR03                PIC 9(01)V9(06).
               05  H-BUDG-NUTR04                PIC 9(01)V9(06).
               05  H-BUDG-NUTR05                PIC 9(01)V9(06).
               05  H-BUDG-NUTR06                PIC 9(01)V9(06).
               05  H-BUDG-NUTR07                PIC 9(01)V9(06).
               05  H-BUDG-NUTR08                PIC 9(01)V9(06).
               05  H-BUDG-NUTR09                PIC 9(01)V9(06).
               05  H-BUDG-NUTR10                PIC 9(01)V9(06).
               05  H-BUDG-NUTR109               PIC 9(01)V9(06).
               05  H-BUDG-NUTR100               PIC 9(01)V9(06).
               05  H-BUDG-NUTR113               PIC 9(01)V9(06).
               05  H-BUDG-NUTR120               PIC 9(01)V9(06).
               05  H-BUDG-NUTR130               PIC 9(01)V9(06).
               05  H-BUDG-NUTR140               PIC 9(01)V9(06).
               05  H-BUDG-NUTR150               PIC 9(01)V9(06).
               05  H-BUDG-NUTR160               PIC 9(01)V9(06).
               05  H-BUDG-NUTR170               PIC 9(01)V9(06).
               05  H-BUDG-NUTR180               PIC 9(01)V9(06).
               05  H-BUDG-NUTR190               PIC 9(01)V9(06).
               05  H-BUDG-NUTR200               PIC 9(01)V9(06).
               05  H-BUDG-NUTR210               PIC 9(01)V9(06).
               05  H-CASE-MIX-ADJ               PIC 9(01)V9(04).
               05  H-SHORT-STAY-ADJ             PIC 9(01)V9(04).
               05  H-UPDATE-01                  PIC 9(01)V9(04).
               05  H-UPDATE-02                  PIC 9(01)V9(04).
               05  H-UPDATE-03                  PIC 9(01)V9(04).
               05  H-UPDATE-04                  PIC 9(01)V9(04).
               05  H-UPDATE-05                  PIC 9(01)V9(04).
               05  H-UPDATE-06                  PIC 9(01)V9(04).
               05  H-UPDATE-07                  PIC 9(01)V9(04).
               05  H-UPDATE-08                  PIC 9(01)V9(04).
               05  H-UPDATE-09                  PIC 9(01)V9(04).
               05  H-UPDATE-10                  PIC 9(01)V9(04).
               05  H-UPDATE-109                 PIC 9(01)V9(04).
               05  H-UPDATE-113                 PIC 9(01)V9(04).
               05  H-UPDATE-120                 PIC 9(01)V9(04).
               05  H-UPDATE-130                 PIC 9(01)V9(04).
               05  H-UPDATE-140                 PIC 9(01)V9(05).
               05  H-UPDATE-150                 PIC 9(01)V9(06).
               05  H-UPDATE-160                 PIC 9(01)V9(06).
               05  H-UPDATE-170                 PIC 9(01)V9(06).
               05  H-UPDATE-180                 PIC 9(01)V9(06).
               05  H-UPDATE-190                 PIC 9(01)V9(06).
               05  H-UPDATE-200                 PIC 9(01)V9(06).
               05  H-UPDATE-210                 PIC 9(01)V9(06).
               05  H-ACCUM-TO-HSP               PIC 9(01)V9(04).
               05  H-HSP-UPDATE94               PIC 9(01)V9(04).
               05  H-HSP-UPDATE95               PIC 9(01)V9(04).
               05  H-HSP-UPDATE96               PIC 9(01)V9(04).
               05  H-HSP-UPDATE97               PIC 9(01)V9(04).
               05  H-HSP-UPDATE98               PIC 9(01)V9(04).
               05  H-HSP-UPDATE99               PIC 9(01)V9(04).
               05  H-HSP-UPDATE00               PIC 9(01)V9(04).
               05  H-HSP-UPDATE01               PIC 9(01)V9(04).
               05  H-PUERTO-RICO-RATE           PIC 9(04)V9(02).
               05  H-FEDERAL-RATE               PIC 9(04)V9(02).
               05  H-LABOR-PCT                  PIC 9(01)V9(04).
               05  H-NONLABOR-PCT               PIC 9(01)V9(04).
               05  H-PR-LABOR-PCT               PIC 9(01)V9(04).
               05  H-PR-NONLABOR-PCT            PIC 9(01)V9(04).
               05  H-HSP-RATE                   PIC 9(08)V9(09).
               05  H-FSP-RATE                   PIC 9(08)V9(09).
               05  H-OUTLIER-OFFSET-NAT         PIC 9(01)V9(06).
               05  H-OUTLIER-OFFSET-PR          PIC 9(01)V9(06).
               05  H-WK-OPER-DSH                PIC 9(01)V9(04).
               05  H-WK-CAPI-IME-TEACH          PIC 9(06)V9(09).
               05  H-OPER-PR-DOLLAR-THRESHOLD   PIC 9(07)V9(09).
               05  H-CAPI-PR-DOLLAR-THRESHOLD   PIC 9(07)V9(09).
               05  H-DSH-REDUCT-FACTOR          PIC 9(01)V9(04).
               05  H-WK-PASS-AMT-PLUS-MISC      PIC 9(06)V99.
               05  H-BASE-DRG-PAYMENT           PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-CAP         PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-CAPDIF      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-NEURO       PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-GRAFT       PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-X-STOP      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-HRTIMP-STOP PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-SPIRAT-STOP PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-SPIRAT      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-AUTOLITT    PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-DIFICID     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-ZENITH      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-VORAXAZE    PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-ARGUS       PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-KCENTRA     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-ZILVER      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-CARDIO      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-MITRACLP    PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-RNSSYS      PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-BLINATU     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-LUTONIX     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-DEFITELIO   PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-GORE        PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-IDARUCIZ    PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-MAGEC       PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-VISTOGARD   PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-STELARA     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-ZINPLAVA    PIC S9(07)V99.
               05  H-NEW-TECH-ADDON-EDWARDS     PIC S9(07)V99.
               05  H-NEW-TECH-ADDON             PIC S9(07)V99.
               05  H-TECH-ADDON-ISLET-CNTR      PIC S9(02).
               05  H-TECH-ADDON-ISLET-CNTR2     PIC S9(02).
      *        05  H-READMIS-ADJUST-AMT         PIC S9(07)V99.
      *        05  H-VAL-BASED-PURCH-ADJUST-AMT PIC S9(07)V99.
      *        05  H-BUNDLE-ADJUST-AMT          PIC S9(07)V99.
               05  H-LESSER-NEURO-1             PIC S9(07)V99.
               05  H-LESSER-NEURO-2             PIC S9(07)V99.
               05  H-LESSER-GRAFT-1             PIC S9(07)V99.
               05  H-LESSER-GRAFT-2             PIC S9(07)V99.
               05  H-LESSER-X-STOP-1            PIC S9(07)V99.
               05  H-LESSER-X-STOP-2            PIC S9(07)V99.
               05  H-LESSER-HRTIMP-STOP-1       PIC S9(07)V99.
               05  H-LESSER-HRTIMP-STOP-2       PIC S9(07)V99.
               05  H-LESSER-SPIRAT-STOP-1       PIC S9(07)V99.
               05  H-LESSER-SPIRAT-STOP-2       PIC S9(07)V99.
               05  H-LESSER-AUTOLITT-STOP-1     PIC S9(07)V99.
               05  H-LESSER-AUTOLITT-STOP-2     PIC S9(07)V99.
               05  H-LESSER-DIFICID-STOP-1      PIC S9(07)V99.
               05  H-LESSER-DIFICID-STOP-2      PIC S9(07)V99.
               05  H-LESSER-ZENITH-STOP-1       PIC S9(07)V99.
               05  H-LESSER-ZENITH-STOP-2       PIC S9(07)V99.
               05  H-LESSER-VORAXAZE-STOP-1     PIC S9(07)V99.
               05  H-LESSER-VORAXAZE-STOP-2     PIC S9(07)V99.
               05  H-LESSER-ARGUS-STOP-1        PIC S9(07)V99.
               05  H-LESSER-ARGUS-STOP-2        PIC S9(07)V99.
               05  H-LESSER-KCENTRA-STOP-1      PIC S9(07)V99.
               05  H-LESSER-KCENTRA-STOP-2      PIC S9(07)V99.
               05  H-LESSER-ZILVER-STOP-1       PIC S9(07)V99.
               05  H-LESSER-ZILVER-STOP-2       PIC S9(07)V99.
               05  H-LESSER-CARDIO-STOP-1       PIC S9(07)V99.
               05  H-LESSER-CARDIO-STOP-2       PIC S9(07)V99.
               05  H-LESSER-MITRACLP-STOP-1     PIC S9(07)V99.
               05  H-LESSER-MITRACLP-STOP-2     PIC S9(07)V99.
               05  H-LESSER-RNSSYS-STOP-1       PIC S9(07)V99.
               05  H-LESSER-RNSSYS-STOP-2       PIC S9(07)V99.
               05  H-LESSER-BLINATU-STOP-1      PIC S9(07)V99.
               05  H-LESSER-BLINATU-STOP-2      PIC S9(07)V99.
               05  H-LESSER-LUTONIX-STOP-1      PIC S9(07)V99.
               05  H-LESSER-LUTONIX-STOP-2      PIC S9(07)V99.
               05  H-LESSER-DEFITELIO-STOP-1    PIC S9(07)V99.
               05  H-LESSER-DEFITELIO-STOP-2    PIC S9(07)V99.
               05  H-LESSER-GORE-STOP-1         PIC S9(07)V99.
               05  H-LESSER-GORE-STOP-2         PIC S9(07)V99.
               05  H-LESSER-IDARUCIZ-STOP-1     PIC S9(07)V99.
               05  H-LESSER-IDARUCIZ-STOP-2     PIC S9(07)V99.
               05  H-LESSER-MAGEC-STOP-1        PIC S9(07)V99.
               05  H-LESSER-MAGEC-STOP-2        PIC S9(07)V99.
               05  H-LESSER-VISTOGARD-STOP-1    PIC S9(07)V99.
               05  H-LESSER-VISTOGARD-STOP-2    PIC S9(07)V99.
               05  H-LESSER-STELARA-STOP-1      PIC S9(07)V99.
               05  H-LESSER-STELARA-STOP-2      PIC S9(07)V99.
               05  H-LESSER-ZINPLAVA-STOP-1     PIC S9(07)V99.
               05  H-LESSER-ZINPLAVA-STOP-2     PIC S9(07)V99.
               05  H-LESSER-EDWARDS-STOP-1      PIC S9(07)V99.
               05  H-LESSER-EDWARDS-STOP-2      PIC S9(07)V99.
               05  H-LESSER-STOP-1              PIC S9(07)V99.
               05  H-LESSER-STOP-2              PIC S9(07)V99.
               05  H-LESSER-VAL-BASED-PUR-STOP-1 PIC S9(07)V99.
               05  H-LESSER-VAL-BASED-PUR-STOP-2 PIC S9(07)V99.
               05  H-CSTMED-NEURO               PIC S9(07)V99.
               05  H-CSTMED-GRAFT               PIC S9(07)V99.
               05  H-CSTMED-X-STOP              PIC S9(07)V99.
               05  H-CSTMED-HRTIMP-STOP         PIC S9(07)V99.
               05  H-CSTMED-SPIRAT-STOP         PIC S9(07)V99.
               05  H-CSTMED-AUTOLITT-STOP       PIC S9(07)V99.
               05  H-CSTMED-DIFICID-STOP        PIC S9(07)V99.
               05  H-CSTMED-ZENITH-STOP         PIC S9(07)V99.
               05  H-CSTMED-VORAXAZE-STOP       PIC S9(07)V99.
               05  H-CSTMED-ARGUS-STOP          PIC S9(07)V99.
               05  H-CSTMED-KCENTRA-STOP        PIC S9(07)V99.
               05  H-CSTMED-ZILVER-STOP         PIC S9(07)V99.
               05  H-CSTMED-CARDIO-STOP         PIC S9(07)V99.
               05  H-CSTMED-MITRACLP-STOP       PIC S9(07)V99.
               05  H-CSTMED-RNSSYS-STOP         PIC S9(07)V99.
               05  H-CSTMED-BLINATU-STOP        PIC S9(07)V99.
               05  H-CSTMED-LUTONIX-STOP        PIC S9(07)V99.
               05  H-CSTMED-DEFITELIO-STOP      PIC S9(07)V99.
               05  H-CSTMED-GORE-STOP           PIC S9(07)V99.
               05  H-CSTMED-IDARUCIZ-STOP       PIC S9(07)V99.
               05  H-CSTMED-MAGEC-STOP          PIC S9(07)V99.
               05  H-CSTMED-VISTOGARD-STOP      PIC S9(07)V99.
               05  H-CSTMED-STELARA-STOP        PIC S9(07)V99.
               05  H-CSTMED-ZINPLAVA-STOP       PIC S9(07)V99.
               05  H-CSTMED-EDWARDS-STOP        PIC S9(07)V99.
               05  H-CSTMED-STOP                PIC S9(07)V99.
               05  H-NEW-TECH-PCT               PIC 9(01)V9(02).
               05  H-OPER-CHARGE-THRESHOLD      PIC 9(07)V9(09).
           02  HOLD-ADDITIONAL-VARIABLES.
               05  H-OPER-HSP-PCT               PIC 9(01)V9(02).
               05  H-OPER-FSP-PCT               PIC 9(01)V9(02).
               05  H-NAT-PCT                    PIC 9(01)V9(02).
               05  H-REG-PCT                    PIC 9(01)V9(02).
               05  H-FAC-SPEC-RATE              PIC 9(05)V9(02).
               05  H-UPDATE-FACTOR              PIC 9(01)V9(05).
               05  H-DRG-WT                     PIC 9(02)V9(04).
               05  H-NAT-LABOR                  PIC 9(05)V9(02).
               05  H-NAT-NONLABOR               PIC 9(05)V9(02).
               05  H-REG-LABOR                  PIC 9(05)V9(02).
               05  H-REG-NONLABOR               PIC 9(05)V9(02).
               05  H-OPER-COLA                  PIC 9(01)V9(03).
               05  H-INTERN-RATIO               PIC 9(01)V9(04).
               05  H-OPER-COST-OUTLIER          PIC 9(07)V9(09).
               05  H-OPER-BILL-COSTS            PIC 9(07)V9(09).
               05  H-OPER-DOLLAR-THRESHOLD      PIC 9(07)V9(09).
           02  HOLD-CAPITAL-VARIABLES.
               05  H-CAPI-TOTAL-PAY             PIC 9(07)V9(02).
               05  H-CAPI-HSP                   PIC 9(07)V9(02).
               05  H-CAPI-FSP                   PIC 9(07)V9(02).
               05  H-CAPI-OUTLIER               PIC 9(07)V9(02).
               05  H-CAPI-OLD-HARM              PIC 9(07)V9(02).
               05  H-CAPI-DSH-ADJ               PIC 9(07)V9(02).
               05  H-CAPI-IME-ADJ               PIC 9(07)V9(02).
               05  H-CAPI-EXCEPTIONS            PIC 9(07)V9(02).
           02  HOLD-CAPITAL2-VARIABLES.
               05  H-CAPI2-PAY-CODE             PIC X(1).
               05  H-CAPI2-B-FSP                PIC 9(07)V9(02).
               05  H-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
           02  HOLD-OTHER-VARIABLES.
               05  H-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
               05  H-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
               05  H-NEW-TECH-ADDON-ISLET       PIC 9(07)V9(02).
               05  H-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
               05  H-VAL-BASED-PURCH-PARTIPNT   PIC X.
               05  H-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
               05  H-HOSP-READMISSION-REDU      PIC X.
               05  H-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
               05  H-OPERATNG-DATA.
                   10  H-MODEL1-BUNDLE-DISPRCNT    PIC V999.
                   10  H-OPER-BASE-DRG-PAY         PIC 9(08)V99.
                   10  H-OPER-HSP-AMT              PIC 9(08)V99.
           02  HOLD-PC-OTH-VARIABLES.
               05  H-OPER-DSH                   PIC 9(01)V9(04).
               05  H-CAPI-DSH                   PIC 9(01)V9(04).
               05  H-CAPI-HSP-PCT               PIC 9(01)V9(02).
               05  H-CAPI-FSP-PCT               PIC 9(01)V9(04).
               05  H-ARITH-ALOS                 PIC 9(02)V9(01).
               05  H-PR-WAGE-INDEX              PIC 9(02)V9(04).
               05  H-TRANSFER-ADJ               PIC 9(01)V9(04).
               05  H-PC-HMO-FLAG                PIC X(01).
               05  H-PC-COT-FLAG                PIC X(01).
               05  H-OPER-HSP-PART2             PIC 9(07)V9(02).
               05  H-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
           02  H-ADDITIONAL-PAY-INFO-DATA.
               05 H-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
               05 H-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
               05 H-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
               05 H-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
           02  H-ADDITIONAL-PAY-INFO-DATA2.
               05  H-HAC-PROG-REDUC-IND         PIC X.
               05  H-EHR-PROG-REDUC-IND         PIC X.
               05  H-EHR-ADJUST-AMT             PIC S9(07)V9(02).
               05  H-STNDRD-VALUE               PIC S9(07)V9(02).
               05  H-HAC-PAYMENT-AMT            PIC S9(07)V9(02).
               05  H-FLX7-PAYMENT               PIC S9(07)V9(02).
           02 H-FILLER                          PIC X(0906).
