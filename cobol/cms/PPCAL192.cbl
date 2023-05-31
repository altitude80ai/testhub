000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL192.
000300*REVISED.                   08-15-2019.
000400*AUTHOR.                    DDS TEAM.
000500*REMARKS.                   CMS.
000600 DATE-COMPILED.
000700
000800 ENVIRONMENT DIVISION.
000900 CONFIGURATION SECTION.
001000 SOURCE-COMPUTER.            IBM-370.
001100 OBJECT-COMPUTER.            IBM-370.
001200 INPUT-OUTPUT SECTION.
001300 FILE-CONTROL.
001400
001500 DATA DIVISION.
001600 FILE SECTION.
001700
001800 WORKING-STORAGE SECTION.
001900 01  W-STORAGE-REF                  PIC X(46)  VALUE
002000     'PPCAL192      - W O R K I N G   S T O R A G E'.
002100 01  CAL-VERSION                    PIC X(05)  VALUE 'C19.2'.
002200 01  HMO-FLAG                       PIC X      VALUE 'N'.
002300 01  HMO-TAG                        PIC X      VALUE SPACE.
002400 01  OUTLIER-RECON-FLAG             PIC X      VALUE 'N'.
002500 01  TEMP-RELIEF-FLAG               PIC X      VALUE 'N'.
002600 01  NON-TEMP-RELIEF-PAYMENT        PIC 9(07)V9(02) VALUE ZEROES.
002700 01  WK-H-OPER-DOLLAR-THRESHOLD     PIC 9(07)V9(09) VALUE ZEROES.
002800 01  WK-LOW-VOL25PCT                PIC 99V999999 VALUE 01.000.
002900 01  WK-LOW-VOL-ADDON               PIC 9(07)V9(02).
003000 01  WK-MODEL1-BUNDLE-DISPRCNT      PIC S9(01)V9(03).
003100 01  WK-HAC-TOTAL-PAYMENT           PIC 9(07)V9(02).
003200 01  WK-HAC-AMOUNT                  PIC S9(07)V9(02).
003300 01  R1                             PIC S9(04) COMP SYNC.
003400 01  R2                             PIC S9(04) COMP SYNC.
003500 01  R3                             PIC S9(04) COMP SYNC.
003600 01  R4                             PIC S9(04) COMP SYNC.
003700 01  H-OPER-DSH-SCH                 PIC 9(01)V9(04).
003800 01  H-OPER-DSH-RRC                 PIC 9(01)V9(04).
003900 01  H-MB-RATIO-EHR-FULL            PIC 9(01)V9(09).
004000 01  H-MB-RATIO-EHR-QUAL-FULL       PIC 9(01)V9(09).
004100 01  H-EHR-SUBSAV-QUANT             PIC S9(07)V9(02).
004200 01  H-EHR-SUBSAV-LV                PIC S9(07)V9(02).
004300 01  H-EHR-SUBSAV-QUANT-INCLV       PIC S9(07)V9(02).
004400 01  H-EHR-RESTORE-FULL-QUANT       PIC S9(07)V9(02).
004500 01  IDX-TECH                       PIC 9(02).
004600
004700*-----------------------------------------------------*
004800* LABOR & NON-LABOR RATES TABLE                       *
004900*-----------------------------------------------------*
005000
005100 COPY RATEX190.
005200
005300*---------------------------------------------------------*
005400* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE (EFF. FY'19) *
005500*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE                 *
005600*---------------------------------------------------------*
005700
005800 COPY DRGSX190.
005900
006000*---------------------------------------------------------------*
006100* TWO MIDNIGHT STAY POLICY ADJUSTMENT FACTOR TABLE (EFF. FY'01) *
006200*---------------------------------------------------------------*
006300
006400 COPY MIDNIGHT.
006500
006600*-----------------------------------------------------*
006700* NEW TECHNOLOGY ADD-ON PAYMENT ELIGIBILITY VARIABLES *
006800*-----------------------------------------------------*
006900
007000 COPY NTECH192.
007100
007200***********************************************************
007300***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
007400***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
007500***********************************************************
007600
007700 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
007800 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
007900 01  MES-PPS-PROV                   PIC X(06).
008000 01  MES-PPS-STATE                  PIC X(02).
008100 01  MES-INTRO                      PIC X(53) VALUE SPACES.
008200 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
008300 01  MES-SSRFBN.
008400     05 MES-SSRFBN-STATE PIC 99.
008500     05 FILLER           PIC XX.
008600     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
008700     05 FILLER           PIC XX.
008800     05 MES-SSRFBN-CODE2 PIC 99.
008900     05 FILLER           PIC XX.
009000     05 MES-SSRFBN-STNAM PIC X(20).
009100     05 MES-SSRFBN-REST  PIC X(22).
009200
009300 01 WK-HLDDRG-DATA.
009400     05  HLDDRG-DATA.
009500         10  HLDDRG-DRGX               PIC X(03).
009600         10  FILLER1                   PIC X(01).
009700         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
009800         10  FILLER2                   PIC X(01).
009900         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
010000         10  FILLER3                   PIC X(05).
010100         10  HLDDRG-LOW                PIC X(01).
010200         10  FILLER5                   PIC X(01).
010300         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
010400         10  FILLER6                   PIC X(02).
010500         10  HLDDRG-PAC                PIC X(01).
010600         10  FILLER7                   PIC X(01).
010700         10  HLDDRG-SPPAC              PIC X(01).
010800         10  FILLER8                   PIC X(02).
010900         10  HLDDRG-DESC               PIC X(26).
011000
011100 01 WK-HLDDRG-DATA2.
011200     05  HLDDRG-DATA2.
011300         10  HLDDRG-DRGX2               PIC X(03).
011400         10  FILLER21                   PIC X(01).
011500         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
011600         10  FILLER22                   PIC X(01).
011700         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
011800         10  FILLER23                   PIC X(05).
011900         10  HLDDRG-LOW2                PIC X(01).
012000         10  FILLER25                   PIC X(01).
012100         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
012200         10  FILLER26                   PIC X(02).
012300         10  HLDDRG-TRANS-FLAGS.
012400                   88  D-DRG-POSTACUTE-50-50
012500                   VALUE 'Y Y'.
012600                   88  D-DRG-POSTACUTE-PERDIEM
012700                   VALUE 'Y  '.
012800             15  HLDDRG-PAC2            PIC X(01).
012900             15  FILLER27               PIC X(01).
013000             15  HLDDRG-SPPAC2          PIC X(01).
013100         10  FILLER28                   PIC X(02).
013200         10  HLDDRG-DESC2               PIC X(26).
013300         10  HLDDRG-VALID               PIC X(01).
013400
013500 01  MES-LOWVOL.
013600     05  MES-LOWVOL-PROV             PIC X(6).
013700     05  FILLER                      PIC XXX.
013800     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
013900
014000 01  WK-UNCOMP-CARE.
014100     05  WK-UNCOMP-CARE-PROV         PIC X(6).
014200     05  FILLER                      PIC X.
014300     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
014400
014500 01 WK-HLD-MID-DATA.
014600     05  HLD-MID-DATA.
014700         10  HLD-MID-MSAX              PIC X(04).
014800         10  FILLER1                   PIC X(01).
014900         10  HLD-MID-ADJ-FACT          PIC 9(02)V9(06).
015000
015100 01  HLD-PPS-DATA.
015200         10  HLD-PPS-RTC                PIC 9(02).
015300         10  HLD-PPS-WAGE-INDX          PIC 9(02)V9(04).
015400         10  HLD-PPS-OUTLIER-DAYS       PIC 9(03).
015500         10  HLD-PPS-AVG-LOS            PIC 9(02)V9(01).
015600         10  HLD-PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
015700         10  HLD-PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
015800         10  HLD-PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
015900         10  HLD-PPS-OPER-HSP-PART      PIC 9(06)V9(02).
016000         10  HLD-PPS-OPER-FSP-PART      PIC 9(06)V9(02).
016100         10  HLD-PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
016200         10  HLD-PPS-REG-DAYS-USED      PIC 9(03).
016300         10  HLD-PPS-LTR-DAYS-USED      PIC 9(02).
016400         10  HLD-PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
016500         10  HLD-PPS-CALC-VERS          PIC X(05).
016600
016700 LINKAGE SECTION.
016800
016900************************************************************************
017000* REVIEW CODES DIRECT THE PPCAL SUBROUTINE IN HOW TO PAY THE BILL.     *
017100*                                                                      *
017200* COMMENTS:                                                            *
017300* CLAIMS WITH CONDITION CODE 66 SHOULD BE PROCESSED UNDER REVIEW CODE  *
017400* 06, 07, OR 11 AS APPROPRIATE TO EXCLUDE ANY OUTLIER COMPUTATION.     *
017500*                                                                      *
017600* REVIEW-CODE:                                                         *
017700*   00: PAY-WITH-OUTLIER.                                              *
017800*    + WILL CALCULATE THE STANDARD PAYMENT.                            *
017900*    + WILL ALSO ATTEMPT TO PAY ONLY COST OUTLIERS;                    *
018000*      DAY OUTLIERS EXPIRED 10/01/97                                   *
018100*                                                                      *
018200*   03: PAY-PERDIEM-DAYS.                                              *
018300*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
018400*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
018500*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
018600*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
018700*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
018800*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
018900*                                                                      *
019000*   06: PAY-XFER-NO-COST                                               *
019100*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
019200*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
019300*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
019400*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
019500*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
019600*                                                                      *
019700*   07: PAY-WITHOUT-COST.                                              *
019800*    + WILL CALCULATE THE STANDARD PAYMENT WITHOUT THE COST PORTION.   *
019900*                                                                      *
020000*   09: PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS                       *
020100*    + 50-50                                                           *
020200*      - NOW USES Y INDICATORS ON DRGS                                 *
020300*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
020400*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
020500*    + FULL PERDIEM                                                    *
020600*      - NOW USES Y INDICATORS ON DRGS                                 *
020700*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
020800*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
020900*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
021000*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
021100*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
021200*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
021300*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
021400*                                                                      *
021500*   11: PAY-XFER-SPEC-DRG-NO-COST - POST-ACUTE TRANSFERS               *
021600*    + 50-50                                                           *
021700*      - NOW USES Y INDICATORS ON DRGS                                 *
021800*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
021900*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
022000*    + FULL PERDIEM                                                    *
022100*      - NOW USES Y INDICATORS ON DRGS                                 *
022200*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
022300*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
022400*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
022500*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
022600*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
022700*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
022800************************************************************************
022900
023000************************************************************************
023100* NEW BILL FORMAT (MILLINNIUM COMPATIBLE)                              *
023200*                                                                      *
023300* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL001 PROGRAM  *
023400* AND AFTER FOR PROCESSING IN THE NEW FORMAT.                          *
023500*                                                                      *
023600* B-CHARGES-CLAIMED = TOTAL COVERED CHARGES ON THE 0001 (TOTALS        *
023700* LINE) MINUS BLOOD CLOT COST, KIDNEY COSTS, ACQUISITION COSTS AND     *
023800* TECHNICAL PROVIDER CHARGES.                                          *
023900************************************************************************
024000 01  BILL-NEW-DATA.
024100         10  B-NPI10.
024200             15  B-NPI8             PIC X(08).
024300             15  B-NPI-FILLER       PIC X(02).
024400         10  B-PROVIDER-NO          PIC X(06).
024500             88  B-FORMER-MDH-PROVIDERS
024600                                      VALUE '080006' '140184'
024700                                            '390072' '420019'
024800                                            '440031' '450451'
024900                                            '490019' '510062'.
025000         10  B-REVIEW-CODE          PIC 9(02).
025100             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
025200             88  PAY-WITH-OUTLIER     VALUE 00 07.
025300             88  PAY-PERDIEM-DAYS     VALUE 03.
025400             88  PAY-XFER-NO-COST     VALUE 06.
025500             88  PAY-WITHOUT-COST     VALUE 07.
025600             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
025700             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
025800         10  B-DRG                  PIC 9(03).
025900
026000* ======================================================
026100* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRG'S
026200* ======================================================
026300*
026400*            88  B-DRG-POSTACUTE-PERDIEM
026500*                         VALUE  NOW USES Y INDICATORS ON DRGS
026600*                         SEE TABLE 5
026700*                         D-DRG-POSTACUTE-PERDIEM
026800
026900         10  B-LOS                  PIC 9(03).
027000         10  B-COVERED-DAYS         PIC 9(03).
027100         10  B-LTR-DAYS             PIC 9(02).
027200         10  B-DISCHARGE-DATE.
027300             15  B-DISCHG-CC        PIC 9(02).
027400             15  B-DISCHG-YY        PIC 9(02).
027500             15  B-DISCHG-MM        PIC 9(02).
027600             15  B-DISCHG-DD        PIC 9(02).
027700         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
027800         10  B-PROCEDURE-CODE-TABLE.
027900             15  B-PROCEDURE-CODE    PIC X(07) OCCURS 25 TIMES
028000                 INDEXED BY IDX-PROC.
028100         10  B-DIAGNOSIS-CODE-TABLE.
028200             15  B-DIAGNOSIS-CODE    PIC X(07) OCCURS 25 TIMES
028300                 INDEXED BY IDX-DIAG.
028400         10  B-DEMO-DATA.
028500             15  B-DEMO-CODE1           PIC X(02).
028600             15  B-DEMO-CODE2           PIC X(02).
028700             15  B-DEMO-CODE3           PIC X(02).
028800             15  B-DEMO-CODE4           PIC X(02).
028900         10  B-NDC-DATA.
029000             15  B-NDC-NUMBER           PIC X(11).
029100         10  FILLER                     PIC X(73).
029200
029300************************************************************************
029400* RETURN CODES (PPS-RTC) NOTE HOW THE BILL WAS/WAS NOT PAID.           *
029500*   00-49: HOW THE BILL WAS PAID                                       *
029600*   50-99: WHY THE BILL WAS NOT PAID                                   *
029700*  ----------------------------------------------------------          *
029800*   00,30:                                                             *
029900*    + PAID NORMAL DRG PAYMENT                                         *
030000*                                                                      *
030100*   01:                                                                *
030200*    + PAID AS A DAY-OUTLIER.                                          *
030300*      - DAY-OUTLIER NO LONGER BEING PAID AS OF 10/01/97               *
030400*                                                                      *
030500*   02:                                                                *
030600*    + PAID AS A COST-OUTLIER.                                         *
030700*                                                                      *
030800*   03,33:                                                             *
030900*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
031000*                                                                      *
031100*   05:                                                                *
031200*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
031300*    + QUALIFIED FOR A COST OUTLIER PAYMENT                            *
031400*                                                                      *
031500*   06:                                                                *
031600*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
031700*    + PROVIDER REFUSED COST OUTLIER PAYMENT                           *
031800*                                                                      *
031900*   10,40:                                                             *
032000*    + POST-ACUTE TRANSFER                                             *
032100*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
032200*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRGS         *
032300*                                                                      *
032400*   12,42:                                                             *
032500*    + POST-ACAUTE TRANSFER WITH SPECIFIC DRGS                         *
032600*      - NOW USES Y INDICATORS ON DRGS                                 *
032700*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
032800*      - D-DRG-POSTACUTE-PERDIEM                                       *
032900*                                                                      *
033000*   14,44:                                                             *
033100*    + PAID NORMAL DRG PAYMENT WITH PERDIEM DAYS = OR > GM ALOS        *
033200*                                                                      *
033300*   16:                                                                *
033400*    + PAID AS A COST-OUTLIER WITH PERDIEM DAYS = OR > GM ALOS         *
033500*                                                                      *
033600*   30,33,40,42,44:                                                    *
033700*    + OUTLIER RECONCILIATION                                          *
033800*                                                                      *
033900*   51:                                                                *
034000*    + NO PROVIDER SPECIFIC INFO FOUND                                 *
034100*                                                                      *
034200*   52:                                                                *
034300*    + INVALID CBSA# IN PROVIDER FILE OR                               *
034400*    + INVALID WAGE INDEX OR                                           *
034500*    + INVALID PROVIDER TYPES ON PROVIDER FILE                         *
034600*                                                                      *
034700*   53:                                                                *
034800*    + WAIVER STATE - NOT CALCULATED BY PPS OR                         *
034900*    + INVALID STATE CODE IN COMBINATION WITH HAC FLAG                 *
035000*                                                                      *
035100*   54:                                                                *
035200*    + INVALID DRG                                                     *
035300*                                                                      *
035400*   55:                                                                *
035500*    + DISCHARGE DATE < PROVIDER EFF START DATE OR                     *
035600*    + DISCHARGE DATE < CBSA EFF START DATE FOR PPS OR                 *
035700*    + PROVIDER HAS BEEN TERMINATED ON OR BEFORE DISCHARGE DATE        *
035800*                                                                      *
035900*   56:                                                                *
036000*    + INVALID LENGTH OF STAY                                          *
036100*                                                                      *
036200*   57:                                                                *
036300*    + REVIEW CODE INVALID (NOT 00 03 06 07 09 11)                     *
036400*                                                                      *
036500*   58:                                                                *
036600*    + TOTAL CHARGES NOT NUMERIC                                       *
036700*                                                                      *
036800*   61:                                                                *
036900*    + LIFETIME RESERVE DAYS NOT NUMERIC OR BILL-LTR-DAYS > 60         *
037000*                                                                      *
037100*   62:                                                                *
037200*    + INVALID NUMBER OF COVERED DAYS                                  *
037300*                                                                      *
037400*   65:                                                                *
037500*    + PAY-CODE NOT = A, B OR C ON PSF FOR CAPITAL OR                  *
037600*    + INVALID READMISSION FLAG IN PSF FILE OR                         *
037700*    + BLANK READMISSION FLAG IN PSF FILE OR                           *
037800*    + READMISSION ADJUSTMENT IS INVALID / OUT OF RANGE IN PSF FILE OR *
037900*    + BLANK READMISSION ADJUSTMENT IN PSF FILE OR                     *
038000*    + INVALID STATE CODE IN COMBO W/ READMISSION FLAG IN PSF FILE OR  *
038100*    + INVALID EHR FLAG IN PSF FILE (MUST BE A "Y" OR BLANK)           *
038200*                                                                      *
038300*   67:                                                                *
038400*    + COST OUTLIER WITH LOS > COVERED DAYS OR                         *
038500*      COST OUTLIER THRESHOLD CALUCULATION                             *
038600*                                                                      *
038700*   68:                                                                *
038800*    + INVALID VALUE BASED PURCHASE FLAG IN PSF FILE OR                *
038900*    + BLANK VALUE BASED PURCHASE FLAG IN PSF FILE OR                  *
039000*    + VALUE BASED PURCHASE ADJUSTMEMT IS INVALID OR OUT OF RANGE IN   *
039100*      PSF FILE INDICATOR OR                                           *
039200*    + BLANK VALUE BASED PURCHASE ADJUSTMEMT IN PSF FILE OR            *
039300*    + INVALID COMBINATION OF HOSPITAL QUALITY INDICATOR AND VALUE     *
039400*      BASED PURCHASE FLAG IN PSF FILE OR                              *
039500*    + INVALID STATE CODE IN COMBINATION WITH VALUE BASED PURCHASE     *
039600*      FLAG IN PSF FILE                                                *
039700*                                                                      *
039800*   98: CANNOT PROCESS BILL OLDER THAN 5 YEARS                         *
039900************************************************************************
040000 01  PPS-DATA.
040100         10  PPS-RTC                PIC 9(02).
040200         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
040300         10  PPS-OUTLIER-DAYS       PIC 9(03).
040400         10  PPS-AVG-LOS            PIC 9(02)V9(01).
040500         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
040600         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
040700         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
040800         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
040900         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
041000         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
041100         10  PPS-REG-DAYS-USED      PIC 9(03).
041200         10  PPS-LTR-DAYS-USED      PIC 9(02).
041300         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
041400         10  PPS-CALC-VERS          PIC X(05).
041500
041600*****************************************************************
041700*            THESE ARE THE VERSIONS OF THE PPCAL
041800*           PROGRAMS THAT WILL BE PASSED BACK----
041900*          ASSOCIATED WITH THE BILL BEING PROCESSED
042000*****************************************************************
042100 01  PRICER-OPT-VERS-SW.
042200     02  PRICER-OPTION-SW          PIC X(01).
042300         88  ALL-TABLES-PASSED          VALUE 'A'.
042400         88  PROV-RECORD-PASSED         VALUE 'P'.
042500         88  ADDITIONAL-VARIABLES       VALUE 'M'.
042600         88  PC-PRICER                  VALUE 'C'.
042700     02  PPS-VERSIONS.
042800         10  PPDRV-VERSION         PIC X(05).
042900
043000*****************************************************************
043100*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
043200*          ASSOCIATED WITH THE BILL BEING PROCESSED
043300*****************************************************************
043400 01  PPS-ADDITIONAL-VARIABLES.
043500     05  PPS-HSP-PCT                PIC 9(01)V9(02).
043600     05  PPS-FSP-PCT                PIC 9(01)V9(02).
043700     05  PPS-NAT-PCT                PIC 9(01)V9(02).
043800     05  PPS-REG-PCT                PIC 9(01)V9(02).
043900     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
044000     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
044100     05  PPS-DRG-WT                 PIC 9(02)V9(04).
044200     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
044300     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
044400     05  PPS-REG-LABOR              PIC 9(05)V9(02).
044500     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
044600     05  PPS-OPER-COLA              PIC 9(01)V9(03).
044700     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
044800     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
044900     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
045000     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
045100     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
045200     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
045300     05  PPS-CAPITAL-VARIABLES.
045400         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
045500         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
045600         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
045700         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
045800         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
045900         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
046000         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
046100         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
046200     05  PPS-CAPITAL2-VARIABLES.
046300         10  PPS-CAPI2-PAY-CODE             PIC X(1).
046400         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
046500         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
046600     05  PPS-OTHER-VARIABLES.
046700         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
046800         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
046900         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
047000         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
047100         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
047200         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
047300         10  PPS-HOSP-READMISSION-REDU      PIC X.
047400         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
047500         10  PPS-OPERATNG-DATA.
047600             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
047700             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
047800             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
047900     05  PPS-PC-OTH-VARIABLES.
048000         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
048100         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
048200         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
048300         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
048400         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
048500         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
048600         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
048700         10  PPS-PC-HMO-FLAG                PIC X(01).
048800         10  PPS-PC-COT-FLAG                PIC X(01).
048900         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
049000         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
049100     05  PPS-ADDITIONAL-PAY-INFO-DATA.
049200         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
049300         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
049400         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
049500         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
049600     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
049700         10  PPS-HAC-PROG-REDUC-IND      PIC X.
049800         10  PPS-EHR-PROG-REDUC-IND      PIC X.
049900         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
050000         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
050100         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
050200         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
050300     05 PPS-FILLER                       PIC X(0897).
050400
050500 01  PROV-NEW-HOLD.
050600     02  PROV-NEWREC-HOLD1.
050700         05  P-NEW-NPI10.
050800             10  P-NEW-NPI8             PIC X(08).
050900             10  P-NEW-NPI-FILLER       PIC X(02).
051000         05  P-NEW-PROVIDER-NO.
051100             88  P-NEW-DSH-ADJ-PROVIDERS
051200                             VALUE '180049' '190044' '190144'
051300                                   '190191' '330047' '340085'
051400                                   '370016' '370149' '420043'.
051500             10  P-NEW-STATE            PIC 9(02).
051600                 88  P-VBP-INVALID-STATE
051700                             VALUE 21 80 40 84.
051800                 88  P-READ-INVALID-STATE
051900                             VALUE 40 84.
052000                 88  P-HAC-INVALID-STATE
052100                             VALUE 40 84.
052200                 88  P-PR-NEW-STATE
052300                             VALUE 40 84.
052400             10  FILLER                 PIC X(04).
052500         05  P-NEW-DATE-DATA.
052600             10  P-NEW-EFF-DATE.
052700                 15  P-NEW-EFF-DT-CC    PIC 9(02).
052800                 15  P-NEW-EFF-DT-YY    PIC 9(02).
052900                 15  P-NEW-EFF-DT-MM    PIC 9(02).
053000                 15  P-NEW-EFF-DT-DD    PIC 9(02).
053100             10  P-NEW-FY-BEGIN-DATE.
053200                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
053300                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
053400                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
053500                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
053600             10  P-NEW-REPORT-DATE.
053700                 15  P-NEW-REPORT-DT-CC PIC 9(02).
053800                 15  P-NEW-REPORT-DT-YY PIC 9(02).
053900                 15  P-NEW-REPORT-DT-MM PIC 9(02).
054000                 15  P-NEW-REPORT-DT-DD PIC 9(02).
054100             10  P-NEW-TERMINATION-DATE.
054200                 15  P-NEW-TERM-DT-CC   PIC 9(02).
054300                 15  P-NEW-TERM-DT-YY   PIC 9(02).
054400                 15  P-NEW-TERM-DT-MM   PIC 9(02).
054500                 15  P-NEW-TERM-DT-DD   PIC 9(02).
054600         05  P-NEW-WAIVER-CODE          PIC X(01).
054700             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
054800         05  P-NEW-INTER-NO             PIC 9(05).
054900         05  P-NEW-PROVIDER-TYPE        PIC X(02).
055000             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
055100             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
055200                                                  '15' '17'
055300                                                  '22'.
055400             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
055500             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
055600             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
055700             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
055800             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
055900             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
056000             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
056100             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
056200             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
056300             88  P-N-EACH                   VALUE '21' '22'.
056400             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
056500             88  P-N-NHCMQ-II-SNF           VALUE '32'.
056600             88  P-N-NHCMQ-III-SNF          VALUE '33'.
056700             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
056800         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
056900             88  P-N-NEW-ENGLAND            VALUE  1.
057000             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
057100             88  P-N-SOUTH-ATLANTIC         VALUE  3.
057200             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
057300             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
057400             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
057500             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
057600             88  P-N-MOUNTAIN               VALUE  8.
057700             88  P-N-PACIFIC                VALUE  9.
057800         05  P-NEW-CURRENT-DIV   REDEFINES
057900                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
058000             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
058100         05  P-NEW-MSA-DATA.
058200             10  P-NEW-CHG-CODE-INDEX       PIC X.
058300             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
058400             10  P-NEW-GEO-LOC-MSA9   REDEFINES
058500                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
058600             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
058700             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
058800             10  P-NEW-STAND-AMT-LOC-MSA9
058900       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
059000                 15  P-NEW-RURAL-1ST.
059100                     20  P-NEW-STAND-RURAL  PIC XX.
059200                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
059300                 15  P-NEW-RURAL-2ND        PIC XX.
059400         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
059500                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
059600                 88  P-NEW-SCH-YR82       VALUE   '82'.
059700                 88  P-NEW-SCH-YR87       VALUE   '87'.
059800         05  P-NEW-LUGAR                    PIC X.
059900         05  P-NEW-TEMP-RELIEF-IND          PIC X.
060000         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
060100         05  P-NEW-STATE-CODE               PIC 9(02).
060200         05  P-NEW-STATE-CODE-X REDEFINES
060300             P-NEW-STATE-CODE               PIC X(02).
060400         05  FILLER                         PIC X(03).
060500     02  PROV-NEWREC-HOLD2.
060600         05  P-NEW-VARIABLES.
060700             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
060800             10  P-NEW-COLA              PIC  9(01)V9(03).
060900             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
061000             10  P-NEW-BED-SIZE          PIC  9(05).
061100             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
061200             10  P-NEW-CMI               PIC  9(01)V9(04).
061300             10  P-NEW-SSI-RATIO         PIC  V9(04).
061400             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
061500             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
061600             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
061700             10  P-NEW-DSH-PERCENT       PIC  V9(04).
061800             10  P-NEW-FYE-DATE          PIC  X(08).
061900         05  P-NEW-CBSA-DATA.
062000             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
062100             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
062200             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
062300             10  P-NEW-CBSA-GEO-RURAL REDEFINES
062400                 P-NEW-CBSA-GEO-LOC.
062500                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
062600                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
062700                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
062800
062900             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
063000             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
063100             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
063200     02  PROV-NEWREC-HOLD3.
063300         05  P-NEW-PASS-AMT-DATA.
063400             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
063500             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
063600             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
063700             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
063800         05  P-NEW-CAPI-DATA.
063900             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
064000             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
064100             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
064200             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
064300             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
064400             15  P-NEW-CAPI-NEW-HOSP       PIC X.
064500             15  P-NEW-CAPI-IME            PIC 9V9999.
064600             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
064700         05  P-HVBP-HRR-DATA.
064800             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
064900             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
065000             15  P-HOSP-READMISSION-REDU    PIC X.
065100             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
065200         05  P-MODEL1-BUNDLE-DATA.
065300             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
065400             15  P-HAC-REDUC-IND            PIC X.
065500             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
065600             15  P-EHR-REDUC-IND            PIC X.
065700             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
065800         05  P-NEW-COUNTY-CODE              PIC 9(05).
065900         05  P-NEW-COUNTY-CODE-X REDEFINES
066000             P-NEW-COUNTY-CODE              PIC X(05).
066100         05  FILLER                         PIC X(47).
066200
066300*****************************************************************
066400 01  WAGE-NEW-CBSA-INDEX-RECORD.
066500     05  W-CBSA                        PIC X(5).
066600     05  W-CBSA-SIZE                   PIC X.
066700         88  LARGE-URBAN       VALUE 'L'.
066800         88  OTHER-URBAN       VALUE 'O'.
066900         88  ALL-RURAL         VALUE 'R'.
067000     05  W-CBSA-EFF-DATE               PIC X(8).
067100     05  FILLER                        PIC X.
067200     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
067300     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
067400
067500*******************************************************
067600*    HOLD VARIABLES POPULATED IN PPCAL___***          *
067700*******************************************************
067800 COPY PPHOLDAR.
067900
068000******************************************************************
068100 PROCEDURE DIVISION  USING BILL-NEW-DATA
068200                           PPS-DATA
068300                           PRICER-OPT-VERS-SW
068400                           PPS-ADDITIONAL-VARIABLES
068500                           PROV-NEW-HOLD
068600                           WAGE-NEW-CBSA-INDEX-RECORD
068700                           PPHOLDAR-HOLD-AREA.
068800
068900***************************************************************
069000*    PROCESSING:                                              *
069100*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE
069200*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
069300*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
069400*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
069500*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
069600*           GOBACK.                                           *
069700*        D. ASSEMBLE PRICING COMPONENTS.                      *
069800*        E. CALCULATE THE PRICE.                              *
069900***************************************************************
070000     INITIALIZE WK-HLDDRG-DATA
070100                WK-HLDDRG-DATA2
070200                WK-HLD-MID-DATA
070300                WK-NEW-TECH-VARIABLES.
070400
070500     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
070600     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
070700     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
070800     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
070900     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
071000     MOVE 'N' TO TEMP-RELIEF-FLAG.
071100     MOVE 'N' TO OUTLIER-RECON-FLAG.
071200     MOVE ZEROES TO WK-HAC-AMOUNT.
071300     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
071400     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
071500     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
071600     MOVE ZEROES TO PPS-ISLET-ISOL-PAY-ADD-ON.
071700
071800     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
071900
072000     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
072100     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
072200     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
072300     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
072400     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
072500     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
072600     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
072700     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
072800     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
072900                            PPS-ADDITIONAL-PAY-INFO-DATA.
073000     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
073100                            PPS-ADDITIONAL-PAY-INFO-DATA2.
073200
073300     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
073400     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
073500     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
073600     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
073700                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
073800     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
073900     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
074000                               PPS-MODEL1-BUNDLE-DISPRCNT.
074100
074200     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
074300     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
074400     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
074500*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
074600     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
074700     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
074800     MOVE 0     TO    PPS-FLX7-PAYMENT.
074900
075000     IF (PPS-RTC = '00' OR '03' OR '10' OR
075100                   '12' OR '14')
075200        MOVE 'Y' TO OUTLIER-RECON-FLAG
075300        MOVE PPS-DATA TO HLD-PPS-DATA
075400        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
075500        MOVE HLD-PPS-DATA TO PPS-DATA.
075600
075700     IF  PPS-RTC < 50
075800         IF  P-NEW-WAIVER-STATE
075900             MOVE 53 TO PPS-RTC
076000             MOVE ALL '0' TO PPS-OPER-HSP-PART
076100                             PPS-OPER-FSP-PART
076200                             PPS-OPER-OUTLIER-PART
076300                             PPS-OUTLIER-DAYS
076400                             PPS-REG-DAYS-USED
076500                             PPS-LTR-DAYS-USED
076600                             PPS-TOTAL-PAYMENT
076700                             WK-HAC-TOTAL-PAYMENT
076800                             PPS-OPER-DSH-ADJ
076900                             PPS-OPER-IME-ADJ
077000                             H-DSCHG-FRCTN
077100                             H-DRG-WT-FRCTN
077200                             HOLD-ADDITIONAL-VARIABLES
077300                             HOLD-CAPITAL-VARIABLES
077400                             HOLD-CAPITAL2-VARIABLES
077500                             HOLD-OTHER-VARIABLES
077600                             HOLD-PC-OTH-VARIABLES
077700                             H-ADDITIONAL-PAY-INFO-DATA
077800                             H-ADDITIONAL-PAY-INFO-DATA2.
077900     GOBACK.
078000
078100 0200-MAINLINE-CONTROL.
078200
078300     MOVE 'N' TO HMO-TAG.
078400
078500     IF PPS-PC-HMO-FLAG = 'Y' OR
078600               HMO-FLAG = 'Y'
078700        MOVE 'Y' TO HMO-TAG.
078800
078900     MOVE ALL '0' TO PPS-DATA
079000                     H-OPER-DSH-SCH
079100                     H-OPER-DSH-RRC
079200                     HOLD-PPS-COMPONENTS
079300                     HOLD-PPS-COMPONENTS
079400                     HOLD-ADDITIONAL-VARIABLES
079500                     HOLD-CAPITAL-VARIABLES
079600                     HOLD-CAPITAL2-VARIABLES
079700                     HOLD-OTHER-VARIABLES
079800                     HOLD-PC-OTH-VARIABLES
079900                     H-ADDITIONAL-PAY-INFO-DATA
080000                     H-ADDITIONAL-PAY-INFO-DATA2
080100                     H-EHR-SUBSAV-QUANT
080200                     H-EHR-SUBSAV-LV
080300                     H-EHR-SUBSAV-QUANT-INCLV
080400                     H-EHR-RESTORE-FULL-QUANT
080500                     H-OPER-BILL-STDZ-COSTS
080600                     H-CAPI-BILL-STDZ-COSTS
080700                     H-OPER-STDZ-COST-OUTLIER
080800                     H-CAPI-STDZ-COST-OUTLIER
080900                     H-OPER-STDZ-DOLLAR-THRESHOLD
081000                     H-CAPI-STDZ-DOLLAR-THRESHOLD
081100                     WK-LOW-VOL-ADDON
081200                     WK-HAC-AMOUNT
081300                     WK-HAC-TOTAL-PAYMENT.
081400
081500     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
081600        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
081700
081800     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
081900        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
082000
082100     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
082200        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
082300
082400     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
082500        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
082600
082700     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
082800        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
082900
083000     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
083100        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
083200
083300     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
083400        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
083500
083600     PERFORM 1000-EDIT-THE-BILL-INFO.
083700
083800     IF  PPS-RTC = 00
083900         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
084000
084100     IF  PPS-RTC = 00
084200         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
084300
084400     IF OUTLIER-RECON-FLAG = 'Y'
084500        MOVE 'N' TO OUTLIER-RECON-FLAG
084600        GO TO 0200-EXIT.
084700
084800     IF PPS-RTC = 00
084900        IF H-PERDIEM-DAYS = H-ALOS OR
085000           H-PERDIEM-DAYS > H-ALOS
085100           MOVE 14 TO PPS-RTC.
085200
085300     IF PPS-RTC = 02
085400        IF H-PERDIEM-DAYS = H-ALOS OR
085500           H-PERDIEM-DAYS > H-ALOS
085600           MOVE 16 TO PPS-RTC.
085700
085800 0200-EXIT.   EXIT.
085900
086000 1000-EDIT-THE-BILL-INFO.
086100
086200     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
086300     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
086400
086500**   IF  PPS-RTC = 00
086600*        IF  P-NEW-WAIVER-STATE
086700*            MOVE 53 TO PPS-RTC.
086800
086900     IF  PPS-RTC = 00
087000         IF   HLDDRG-VALID = 'I'
087100             MOVE 54 TO PPS-RTC.
087200
087300     IF  PPS-RTC = 00
087400            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
087500                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
087600                MOVE 55 TO PPS-RTC.
087700
087800     IF  PPS-RTC = 00
087900         IF P-NEW-TERMINATION-DATE > 00000000
088000            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
088100                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
088200                  MOVE 55 TO PPS-RTC.
088300
088400     IF  PPS-RTC = 00
088500         IF  B-LOS NOT NUMERIC
088600             MOVE 56 TO PPS-RTC
088700         ELSE
088800         IF  B-LOS = 0
088900             IF B-REVIEW-CODE NOT = 00 AND
089000                              NOT = 03 AND
089100                              NOT = 06 AND
089200                              NOT = 07 AND
089300                              NOT = 09 AND
089400                              NOT = 11
089500             MOVE 56 TO PPS-RTC.
089600
089700     IF  PPS-RTC = 00
089800         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
089900             MOVE 61 TO PPS-RTC
090000         ELSE
090100             MOVE B-LTR-DAYS TO H-LTR-DAYS.
090200
090300     IF  PPS-RTC = 00
090400         IF  B-COVERED-DAYS NOT NUMERIC
090500             MOVE 62 TO PPS-RTC
090600         ELSE
090700         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
090800             MOVE 62 TO PPS-RTC
090900         ELSE
091000             MOVE B-COVERED-DAYS TO H-COV-DAYS.
091100
091200     IF  PPS-RTC = 00
091300         IF  H-LTR-DAYS  > H-COV-DAYS
091400             MOVE 62 TO PPS-RTC
091500         ELSE
091600             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
091700
091800     IF  PPS-RTC = 00
091900         IF  NOT VALID-REVIEW-CODE
092000             MOVE 57 TO PPS-RTC.
092100
092200     IF  PPS-RTC = 00
092300         IF  B-CHARGES-CLAIMED NOT NUMERIC
092400             MOVE 58 TO PPS-RTC.
092500
092600     IF PPS-RTC = 00
092700           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
092800                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
092900                                            NOT = 'C'
093000                 MOVE 65 TO PPS-RTC.
093100
093200***  MDH PROVISION ENDS 9/30/2018
093300***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
093400
093500     IF PPS-RTC = 00 AND
093600        B-DISCHARGE-DATE > 20220930 AND
093700        P-N-INVALID-PROV-TYPES
093800                 MOVE 52 TO PPS-RTC.
093900
094000 2000-ASSEMBLE-PPS-VARIABLES.
094100***  GET THE PROVIDER SPECIFIC VARIABLES.
094200
094300     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
094400     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
094500
094600     IF (P-NEW-STATE = 02 OR 12)
094700        MOVE P-NEW-COLA TO H-OPER-COLA
094800     ELSE
094900        MOVE 1.000 TO H-OPER-COLA.
095000
095100***************************************************************
095200***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
095300
095400     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
095500
095600     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
095700
095800     MOVE P-NEW-STATE            TO MES-PPS-STATE.
095900
096000*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
096100** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
096200** FOR FY 2011
096300***************************************************************
096400*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
096500*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
096600***************************************************************
096700***  GET THE WAGE-INDEX
096800
096900     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
097000     MOVE P-NEW-STATE            TO MES-PPS-STATE.
097100
097200***************************************************************
097300* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
097400* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
097500***************************************************************
097600
097700     PERFORM 2050-RATES-TB THRU 2050-EXIT.
097800
097900     IF P-NEW-GEO-LOC-MSA9 >= 9400 AND
098000        P-NEW-GEO-LOC-MSA9 <= 9900
098100        PERFORM 2100-MIDNIGHT-FACTORS THRU 2100-EXIT
098200     ELSE
098300        MOVE 1 TO HLD-MID-ADJ-FACT
098400        GO TO 2000-EXIT.
098500
098600 2000-EXIT.  EXIT.
098700
098800 2050-RATES-TB.
098900     MOVE 1 TO R2
099000     MOVE 1 TO R4.
099100
099200     IF LARGE-URBAN
099300         MOVE 1 TO R3
099400     ELSE
099500         MOVE 2 TO R3.
099600
099700     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
099800        (P-EHR-REDUC-IND = ' ')           AND
099900        (H-WAGE-INDEX > 01.0000))
100000        PERFORM 2300-GET-LAB-NONLAB-TB1-RATES
100100           THRU 2300-GET-LAB-NONLAB-TB1-EXIT
100200             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
100300
100400     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
100500        (P-EHR-REDUC-IND = ' ')               AND
100600         (H-WAGE-INDEX > 01.0000))
100700        PERFORM 2300-GET-LAB-NONLAB-TB2-RATES
100800           THRU 2300-GET-LAB-NONLAB-TB2-EXIT
100900             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
101000
101100     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
101200        (P-EHR-REDUC-IND = ' ')            AND
101300         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
101400        PERFORM 2300-GET-LAB-NONLAB-TB3-RATES
101500           THRU 2300-GET-LAB-NONLAB-TB3-EXIT
101600             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
101700
101800     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
101900        (P-EHR-REDUC-IND = ' ')               AND
102000         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
102100        PERFORM 2300-GET-LAB-NONLAB-TB4-RATES
102200           THRU 2300-GET-LAB-NONLAB-TB4-EXIT
102300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
102400
102500     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
102600        (P-EHR-REDUC-IND = 'Y')           AND
102700        (H-WAGE-INDEX > 01.0000))
102800        PERFORM 2300-GET-LAB-NONLAB-TB5-RATES
102900           THRU 2300-GET-LAB-NONLAB-TB5-EXIT
103000             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
103100
103200     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
103300        (P-EHR-REDUC-IND = 'Y')               AND
103400         (H-WAGE-INDEX > 01.0000))
103500        PERFORM 2300-GET-LAB-NONLAB-TB6-RATES
103600           THRU 2300-GET-LAB-NONLAB-TB6-EXIT
103700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
103800
103900     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
104000        (P-EHR-REDUC-IND = 'Y')            AND
104100         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
104200        PERFORM 2300-GET-LAB-NONLAB-TB7-RATES
104300           THRU 2300-GET-LAB-NONLAB-TB7-EXIT
104400             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
104500
104600     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
104700        (P-EHR-REDUC-IND = 'Y')               AND
104800         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
104900        PERFORM 2300-GET-LAB-NONLAB-TB8-RATES
105000           THRU 2300-GET-LAB-NONLAB-TB8-EXIT
105100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
105200
105300***************************************************************
105400* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
105500***************************************************************
105600
105700     MOVE 0.00  TO H-OPER-HSP-PCT.
105800     MOVE 1.00  TO H-OPER-FSP-PCT.
105900
106000***************************************************************
106100*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
106200***************************************************************
106300
106400      MOVE 1.00 TO H-NAT-PCT.
106500      MOVE 0.00 TO H-REG-PCT.
106600
106700     IF  P-N-SCH-REBASED-FY90 OR
106800         P-N-EACH OR
106900         P-N-MDH-REBASED-FY90
107000         MOVE 1.00 TO H-OPER-HSP-PCT.
107100
107200 2050-EXIT.   EXIT.
107300
107400***************************************************************
107500*  APPLY THE TWO MIDNIGHT POLICY ADJUSTMENT FACTORS           *
107600***************************************************************
107700 2100-MIDNIGHT-FACTORS.
107800
107900     INITIALIZE HLD-MID-ADJ-FACT.
108000
108100     SET MID-IDX TO 1.
108200
108300     SEARCH MID-TAB VARYING MID-IDX
108400     WHEN WK-MID-MSAX(MID-IDX) = P-NEW-GEO-LOC-MSA9
108500       MOVE MID-DATA-TAB(MID-IDX) TO HLD-MID-DATA.
108600
108700 2100-EXIT.   EXIT.
108800
108900***************************************************************
109000* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
109100* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
109200***************************************************************
109300 2300-GET-LAB-NONLAB-TB1-RATES.
109400
109500     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
109600         MOVE TB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
109700         MOVE TB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
109800         MOVE TB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
109900         MOVE TB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
110000
110100 2300-GET-LAB-NONLAB-TB1-EXIT.   EXIT.
110200
110300 2300-GET-LAB-NONLAB-TB2-RATES.
110400
110500     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
110600         MOVE TB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
110700         MOVE TB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
110800         MOVE TB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
110900         MOVE TB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
111000
111100 2300-GET-LAB-NONLAB-TB2-EXIT.   EXIT.
111200
111300 2300-GET-LAB-NONLAB-TB3-RATES.
111400
111500     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
111600         MOVE TB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
111700         MOVE TB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
111800         MOVE TB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
111900         MOVE TB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
112000
112100 2300-GET-LAB-NONLAB-TB3-EXIT.   EXIT.
112200
112300 2300-GET-LAB-NONLAB-TB4-RATES.
112400
112500     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
112600         MOVE TB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
112700         MOVE TB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
112800         MOVE TB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
112900         MOVE TB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
113000
113100 2300-GET-LAB-NONLAB-TB4-EXIT.   EXIT.
113200
113300 2300-GET-LAB-NONLAB-TB5-RATES.
113400
113500     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
113600         MOVE TB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
113700         MOVE TB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
113800         MOVE TB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
113900         MOVE TB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
114000
114100 2300-GET-LAB-NONLAB-TB5-EXIT.   EXIT.
114200
114300 2300-GET-LAB-NONLAB-TB6-RATES.
114400
114500     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
114600         MOVE TB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
114700         MOVE TB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
114800         MOVE TB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
114900         MOVE TB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
115000
115100 2300-GET-LAB-NONLAB-TB6-EXIT.   EXIT.
115200
115300 2300-GET-LAB-NONLAB-TB7-RATES.
115400
115500     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
115600         MOVE TB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
115700         MOVE TB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
115800         MOVE TB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
115900         MOVE TB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
116000
116100 2300-GET-LAB-NONLAB-TB7-EXIT.   EXIT.
116200
116300 2300-GET-LAB-NONLAB-TB8-RATES.
116400
116500     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
116600         MOVE TB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
116700         MOVE TB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
116800         MOVE TB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
116900         MOVE TB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
117000
117100 2300-GET-LAB-NONLAB-TB8-EXIT.   EXIT.
117200
117300***************************************************************
117400* OBTAIN THE APPLICABLE DRG WEIGHTS                           *
117500***************************************************************
117600 2600-GET-DRG-WEIGHT.
117700
117800     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
117900     SET DRG-IDX TO 1
118000     SEARCH DRG-TAB VARYING DRG-IDX
118100         AT END
118200           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
118300           MOVE 'I' TO  HLDDRG-VALID
118400           MOVE 0 TO HLDDRG-WEIGHT
118500           MOVE 54 TO PPS-RTC
118600           GO TO 2600-EXIT
118700       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
118800         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
118900
119000
119100     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
119200     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
119300     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
119400                                  H-DRG-WT.
119500     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
119600                                  H-ALOS.
119700     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
119800     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
119900                                  H-ARITH-ALOS.
120000     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
120100     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
120200     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
120300     MOVE  'V'                 TO HLDDRG-VALID.
120400     MOVE ZEROES               TO H-DAYS-CUTOFF.
120500
120600 2600-EXIT.   EXIT.
120700
120800*
120900 3000-CALC-PAYMENT.
121000***************************************************************
121100
121200     PERFORM 3100-CALC-STAY-UTILIZATION.
121300     PERFORM 3300-CALC-OPER-FSP-AMT.
121400     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
121500
121600***********************************************************
121700***  OPERATING IME CALCULATION
121800
121900     COMPUTE H-OPER-IME-TEACH ROUNDED =
122000            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
122100
122200***********************************************************
122300
122400     MOVE 00                 TO  PPS-RTC.
122500     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
122600     MOVE H-ALOS             TO  PPS-AVG-LOS.
122700     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
122800
122900     MOVE B-LOS TO H-PERDIEM-DAYS.
123000     IF H-PERDIEM-DAYS < 1
123100         MOVE 1 TO H-PERDIEM-DAYS.
123200     ADD 1 TO H-PERDIEM-DAYS.
123300
123400     MOVE 1 TO H-DSCHG-FRCTN.
123500
123600     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
123700
123800     IF (PAY-PERDIEM-DAYS  OR
123900         PAY-XFER-NO-COST) OR
124000        (PAY-XFER-SPEC-DRG AND
124100         D-DRG-POSTACUTE-PERDIEM)
124200       IF H-ALOS > 0
124300         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
124400         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
124500         IF H-DSCHG-FRCTN > 1
124600              MOVE 1 TO H-DSCHG-FRCTN
124700              MOVE 1 TO H-TRANSFER-ADJ
124800         ELSE
124900              COMPUTE H-DRG-WT-FRCTN ROUNDED =
125000                  H-TRANSFER-ADJ * H-DRG-WT
125100         END-IF
125200        END-IF
125300     END-IF.
125400
125500
125600     IF (PAY-XFER-SPEC-DRG AND
125700         D-DRG-POSTACUTE-50-50) AND
125800         H-ALOS > 0
125900         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
126000         COMPUTE H-DSCHG-FRCTN  ROUNDED =
126100                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
126200         IF H-DSCHG-FRCTN > 1
126300              MOVE 1 TO H-DSCHG-FRCTN
126400              MOVE 1 TO H-TRANSFER-ADJ
126500         ELSE
126600              COMPUTE H-DRG-WT-FRCTN ROUNDED =
126700            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
126800
126900
127000***********************************************************
127100***  CAPITAL DSH CALCULATION
127200
127300     MOVE 0 TO H-CAPI-DSH.
127400
127500     IF P-NEW-BED-SIZE NOT NUMERIC
127600         MOVE 0 TO P-NEW-BED-SIZE.
127700
127800     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
127900         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
128000                  (.2025 * (P-NEW-SSI-RATIO
128100                          + P-NEW-MEDICAID-RATIO)) - 1.
128200
128300***********************************************************
128400***  CAPITAL IME TEACH CALCULATION
128500
128600     MOVE 0 TO H-WK-CAPI-IME-TEACH.
128700
128800     IF P-NEW-CAPI-IME NUMERIC
128900        IF P-NEW-CAPI-IME > 1.5000
129000           MOVE 1.5000 TO P-NEW-CAPI-IME.
129100
129200*****YEARCHANGE 2009.5 ****************************************
129300***
129400***  PER POLICY, WE REMOVED THE .5 MULTIPLER
129500***
129600***********************************************************
129700     IF P-NEW-CAPI-IME NUMERIC
129800        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
129900         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
130000
130100*****YEARCHANGE 2009.5 ****************************************
130200***********************************************************
130300     MOVE 0.00 TO H-DAYOUT-PCT.
130400     MOVE 0.80 TO H-CSTOUT-PCT.
130500
130600*****************************************************************
130700**
130800** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
130900**
131000*****************************************************************
131100
131200     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
131300             MOVE 0.90 TO H-CSTOUT-PCT.
131400
131500*****YEARCHANGE 2018.0 *******************************************
131600* NATIONAL PERCENTAGE                                            *
131700******************************************************************
131800
131900       MOVE 0.6830 TO H-LABOR-PCT.
132000       MOVE 0.3170 TO H-NONLABOR-PCT.
132100
132200     IF (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000)
132300       MOVE 0.6200 TO H-LABOR-PCT
132400       MOVE 0.3800 TO H-NONLABOR-PCT.
132500
132600     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
132700             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
132800     ELSE
132900             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
133000
133100     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
133200             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
133300     ELSE
133400             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
133500
133600***********************************************************
133700*****YEARCHANGE 2010.0 ************************************
133800***  CAPITAL PAYMENT METHOD B - YEARCHNG
133900***  CAPITAL PAYMENT METHOD B
134000
134100     IF W-CBSA-SIZE = 'L'
134200        MOVE 1.00 TO H-CAPI-LARG-URBAN
134300     ELSE
134400        MOVE 1.00 TO H-CAPI-LARG-URBAN.
134500
134600     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
134700
134800*****YEARCHANGE 2018.0 ************************************
134900
135000     COMPUTE H-FEDERAL-RATE ROUNDED =
135100                              (0459.41 * H-CAPI-GAF).
135200
135300*****YEARCHANGE 2015.1 ************************************
135400
135500     COMPUTE H-CAPI-COLA ROUNDED =
135600                     (.3152 * (H-OPER-COLA - 1) + 1).
135700
135800     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
135900
136000***********************************************************
136100* CAPITAL FSP CALCULATION                                 *
136200***********************************************************
136300
136400     COMPUTE H-CAPI-FSP-PART ROUNDED =
136500                               H-DRG-WT       *
136600                               H-CAPI-FED-RATE *
136700                               H-CAPI-COLA *
136800                               H-CAPI-LARG-URBAN *
136900                               HLD-MID-ADJ-FACT.
137000
137100***********************************************************
137200***  CAPITAL PAYMENT METHOD A
137300***  CAPITAL PAYMENT METHOD A
137400
137500     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
137600        MOVE 1.00 TO H-CAPI-SCH
137700     ELSE
137800        MOVE 0.85 TO H-CAPI-SCH.
137900
138000***********************************************************
138100***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
138200***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
138300
138400     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
138500                    (P-NEW-CAPI-OLD-HARM-RATE *
138600                    H-CAPI-SCH).
138700
138800***********************************************************
138900        IF PAY-PERDIEM-DAYS
139000            IF  H-PERDIEM-DAYS < H-ALOS
139100                IF  NOT (B-DRG = 789)
139200                    PERFORM 3500-CALC-PERDIEM-AMT
139300                    MOVE 03 TO PPS-RTC.
139400
139500        IF PAY-XFER-SPEC-DRG
139600            IF  H-PERDIEM-DAYS < H-ALOS
139700                IF  NOT (B-DRG = 789)
139800                    PERFORM 3550-CALC-PERDIEM-AMT.
139900
140000        IF  PAY-XFER-NO-COST
140100            MOVE 00 TO PPS-RTC
140200            IF H-PERDIEM-DAYS < H-ALOS
140300               IF  NOT (B-DRG = 789)
140400                   PERFORM 3500-CALC-PERDIEM-AMT
140500                   MOVE 06 TO PPS-RTC.
140600
140700     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
140800
140900     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
141000
141100     IF PPS-RTC = 65 OR 67 OR 68
141200               GO TO 3000-CONTINUE.
141300
141400     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
141500
141600     IF PPS-RTC = 65 OR 67 OR 68
141700               GO TO 3000-CONTINUE.
141800
141900     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
142000
142100     IF PPS-RTC = 65 OR 67 OR 68
142200               GO TO 3000-CONTINUE.
142300
142400     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
142500
142600     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
142700
142800     IF PPS-RTC = 65 OR 67 OR 68
142900               GO TO 3000-CONTINUE.
143000
143100        IF PAY-XFER-SPEC-DRG
143200            IF  H-PERDIEM-DAYS < H-ALOS
143300                IF  NOT (B-DRG = 789)
143400                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
143500
143600
143700        IF  PAY-PERDIEM-DAYS
143800            IF  H-OPER-OUTCST-PART > 0
143900                MOVE H-OPER-OUTCST-PART TO
144000                     H-OPER-OUTLIER-PART
144100                MOVE 05 TO PPS-RTC
144200            ELSE
144300            IF  PPS-RTC NOT = 03
144400                MOVE 00 TO PPS-RTC
144500                MOVE 0  TO H-OPER-OUTLIER-PART.
144600
144700        IF  PAY-PERDIEM-DAYS
144800            IF  H-CAPI-OUTCST-PART > 0
144900                MOVE H-CAPI-OUTCST-PART TO
145000                     H-CAPI-OUTLIER-PART
145100                MOVE 05 TO PPS-RTC
145200            ELSE
145300            IF  PPS-RTC NOT = 03
145400                MOVE 0  TO H-CAPI-OUTLIER-PART.
145500
145600
145700     IF P-N-SCH-REBASED-FY90 OR
145800        P-N-EACH OR
145900        P-N-MDH-REBASED-FY90
146000         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
146100
146200 3000-CONTINUE.
146300
146400***********************************************************
146500***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
146600***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
146700
146800     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
146900
147000***********************************************************
147100
147200     IF  PPS-RTC = 67
147300         MOVE H-OPER-DOLLAR-THRESHOLD TO
147400              WK-H-OPER-DOLLAR-THRESHOLD.
147500
147600     IF  PPS-RTC < 50
147700         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
147800
147900     IF  PPS-RTC < 50
148000         NEXT SENTENCE
148100     ELSE
148200         MOVE ALL '0' TO PPS-OPER-HSP-PART
148300                         PPS-OPER-FSP-PART
148400                         PPS-OPER-OUTLIER-PART
148500                         PPS-OUTLIER-DAYS
148600                         PPS-REG-DAYS-USED
148700                         PPS-LTR-DAYS-USED
148800                         PPS-TOTAL-PAYMENT
148900                         WK-HAC-TOTAL-PAYMENT
149000                         PPS-OPER-DSH-ADJ
149100                         PPS-OPER-IME-ADJ
149200                         H-DSCHG-FRCTN
149300                         H-DRG-WT-FRCTN
149400                         HOLD-ADDITIONAL-VARIABLES
149500                         HOLD-CAPITAL-VARIABLES
149600                         HOLD-CAPITAL2-VARIABLES
149700                         HOLD-OTHER-VARIABLES
149800                         HOLD-PC-OTH-VARIABLES
149900                        H-ADDITIONAL-PAY-INFO-DATA
150000                        H-ADDITIONAL-PAY-INFO-DATA2.
150100
150200     IF  PPS-RTC = 67
150300         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
150400                 H-OPER-DOLLAR-THRESHOLD.
150500
150600 3000-EXIT.  EXIT.
150700
150800 3100-CALC-STAY-UTILIZATION.
150900
151000     MOVE 0 TO PPS-REG-DAYS-USED.
151100     MOVE 0 TO PPS-LTR-DAYS-USED.
151200
151300     IF H-REG-DAYS > 0
151400        IF H-REG-DAYS > B-LOS
151500           MOVE B-LOS TO PPS-REG-DAYS-USED
151600        ELSE
151700           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
151800     ELSE
151900        IF H-LTR-DAYS > B-LOS
152000           MOVE B-LOS TO PPS-LTR-DAYS-USED
152100        ELSE
152200           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
152300
152400
152500
152600 3300-CALC-OPER-FSP-AMT.
152700***********************************************************
152800*  OPERATING FSP CALCULATION                              *
152900***********************************************************
153000
153100     COMPUTE H-OPER-FSP-PART ROUNDED =
153200       (H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
153300        H-NAT-NONLABOR * H-OPER-COLA) * H-DRG-WT *
153400        HLD-MID-ADJ-FACT)
153500           ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
153600
153700 3500-CALC-PERDIEM-AMT.
153800***********************************************************
153900***  REVIEW CODE = 03 OR 06
154000***  OPERATING PERDIEM-AMT CALCULATION
154100***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
154200
154300        COMPUTE H-OPER-FSP-PART ROUNDED =
154400        H-OPER-FSP-PART * H-TRANSFER-ADJ
154500        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
154600
154700***********************************************************
154800***********************************************************
154900***  REVIEW CODE = 03 OR 06
155000***  CAPITAL   PERDIEM-AMT CALCULATION
155100***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
155200
155300        COMPUTE H-CAPI-FSP-PART ROUNDED =
155400        H-CAPI-FSP-PART * H-TRANSFER-ADJ
155500        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
155600
155700***********************************************************
155800***  REVIEW CODE = 03 OR 06
155900***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
156000***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
156100
156200        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
156300        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
156400        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
156500
156600 3550-CALC-PERDIEM-AMT.
156700***********************************************************
156800***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
156900***  OPERATING PERDIEM-AMT CALCULATION
157000***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
157100
157200     IF (D-DRG-POSTACUTE-50-50)
157300        MOVE 10 TO PPS-RTC
157400        COMPUTE H-OPER-FSP-PART ROUNDED =
157500        H-OPER-FSP-PART * H-DSCHG-FRCTN
157600        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
157700
157800     IF (D-DRG-POSTACUTE-PERDIEM)
157900        MOVE 12 TO PPS-RTC
158000        COMPUTE H-OPER-FSP-PART ROUNDED =
158100        H-OPER-FSP-PART *  H-TRANSFER-ADJ
158200        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
158300
158400***********************************************************
158500***  CAPITAL PERDIEM-AMT CALCULATION
158600***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
158700
158800     IF (D-DRG-POSTACUTE-50-50)
158900        MOVE 10 TO PPS-RTC
159000        COMPUTE H-CAPI-FSP-PART ROUNDED =
159100        H-CAPI-FSP-PART * H-DSCHG-FRCTN
159200        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
159300
159400     IF (D-DRG-POSTACUTE-PERDIEM)
159500        MOVE 12 TO PPS-RTC
159600        COMPUTE H-CAPI-FSP-PART ROUNDED =
159700        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
159800        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
159900
160000***********************************************************
160100***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
160200***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
160300
160400     IF (D-DRG-POSTACUTE-50-50)
160500        MOVE 10 TO PPS-RTC
160600        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
160700        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
160800        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
160900
161000     IF (D-DRG-POSTACUTE-PERDIEM)
161100        MOVE 12 TO PPS-RTC
161200        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
161300        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
161400        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
161500
161600 3560-CHECK-RTN-CODE.
161700
161800     IF (D-DRG-POSTACUTE-50-50)
161900        MOVE 10 TO PPS-RTC.
162000     IF (D-DRG-POSTACUTE-PERDIEM)
162100        MOVE 12 TO PPS-RTC.
162200
162300 3560-EXIT.    EXIT.
162400
162500***********************************************************
162600 3600-CALC-OUTLIER.
162700***********************************************************
162800*---------------------------------------------------------*
162900* (YEARCHANGE 2016.0)
163000* COST OUTLIER OPERATING AND CAPITAL CALCULATION
163100*---------------------------------------------------------*
163200
163300     IF OUTLIER-RECON-FLAG = 'Y'
163400        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
163500               (H-OPER-CSTCHG-RATIO + .2).
163600
163700     IF H-CAPI-CSTCHG-RATIO > 0 OR
163800        H-OPER-CSTCHG-RATIO > 0
163900        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
164000                H-OPER-CSTCHG-RATIO /
164100               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
164200        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
164300                H-CAPI-CSTCHG-RATIO /
164400               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
164500     ELSE
164600        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
164700                  H-CAPI-SHARE-DOLL-THRESHOLD.
164800
164900*-----------------------------*
165000* (YEARCHANGE 2018.0)         *
165100* OUTLIER THRESHOLD AMOUNTS   *
165200*-----------------------------*
165300
165400     MOVE 25743.00 TO H-CST-THRESH.
165500
165600     IF (B-REVIEW-CODE = '03') AND
165700         H-PERDIEM-DAYS < H-ALOS
165800        COMPUTE H-CST-THRESH ROUNDED =
165900                      (H-CST-THRESH * H-TRANSFER-ADJ)
166000                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
166100
166200     IF ((B-REVIEW-CODE = '09') AND
166300         (H-PERDIEM-DAYS < H-ALOS))
166400         IF (D-DRG-POSTACUTE-PERDIEM)
166500            COMPUTE H-CST-THRESH ROUNDED =
166600                      (H-CST-THRESH * H-TRANSFER-ADJ)
166700                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
166800
166900     IF ((B-REVIEW-CODE = '09') AND
167000         (H-PERDIEM-DAYS < H-ALOS))
167100         IF (D-DRG-POSTACUTE-50-50)
167200           COMPUTE H-CST-THRESH ROUNDED =
167300          H-CST-THRESH * H-DSCHG-FRCTN
167400                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
167500
167600     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
167700        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
167800         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
167900          H-OPER-SHARE-DOLL-THRESHOLD.
168000
168100***********************************************************
168200
168300     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
168400          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
168500          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
168600
168700***********************************************************
168800******NOW INCLUDES UNCOMPENSATED CARE**********************
168900
169000     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
169100         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
169200                       +
169300           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
169400                       +
169500             H-OPER-DOLLAR-THRESHOLD
169600                       +
169700                WK-UNCOMP-CARE-AMOUNT
169800                       +
169900                 H-NEW-TECH-PAY-ADD-ON.
170000
170100     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
170200      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
170300                       +
170400             H-CAPI-DOLLAR-THRESHOLD.
170500
170600     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
170700         MOVE 0 TO H-CAPI-COST-OUTLIER.
170800
170900
171000***********************************************************
171100***  OPERATING COST CALCULATION
171200
171300     COMPUTE H-OPER-BILL-COSTS ROUNDED =
171400         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
171500         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
171600
171700
171800     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
171900         COMPUTE H-OPER-OUTCST-PART ROUNDED =
172000         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
172100                         H-OPER-COST-OUTLIER).
172200
172300     IF PAY-WITHOUT-COST OR
172400        PAY-XFER-NO-COST OR
172500        PAY-XFER-SPEC-DRG-NO-COST
172600         MOVE 0 TO H-OPER-OUTCST-PART.
172700
172800***********************************************************
172900***  CAPITAL COST CALCULATION
173000
173100     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
173200             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
173300         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
173400
173500     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
173600         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
173700         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
173800                         H-CAPI-COST-OUTLIER).
173900
174000***********************************************************
174100***  'A' NOT VALID FY 2015 ON
174200
174300*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
174400*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
174500*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
174600
174700     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
174800        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
174900               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
175000
175100     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
175200        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
175300        MOVE 0 TO H-CAPI-OUTCST-PART
175400                  H-OPER-OUTCST-PART.
175500
175600     IF PAY-WITHOUT-COST OR
175700        PAY-XFER-NO-COST OR
175800        PAY-XFER-SPEC-DRG-NO-COST
175900         MOVE 0 TO H-CAPI-OUTCST-PART.
176000
176100***********************************************************
176200***  DETERMINES THE BILL TO BE COST  OUTLIER
176300
176400     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
176500         MOVE 0 TO H-CAPI-OUTDAY-PART
176600                   H-CAPI-OUTCST-PART.
176700
176800     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
176900                 MOVE H-OPER-OUTCST-PART TO
177000                      H-OPER-OUTLIER-PART
177100                 MOVE H-CAPI-OUTCST-PART TO
177200                      H-CAPI-OUTLIER-PART
177300                 MOVE 02 TO PPS-RTC.
177400
177500     IF OUTLIER-RECON-FLAG = 'Y'
177600        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
177700           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
177800           GO TO 3600-EXIT
177900        ELSE
178000           GO TO 3600-EXIT
178100     ELSE
178200        NEXT SENTENCE.
178300
178400
178500***********************************************************
178600***  DETERMINES IF COST OUTLIER
178700***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
178800***         RETURN CODE OF 02
178900
179000     MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
179100
179200     IF PPS-RTC = 02
179300       IF H-CAPI-CSTCHG-RATIO > 0 OR
179400          H-OPER-CSTCHG-RATIO > 0
179500             COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
179600                     (H-CAPI-COST-OUTLIER  +
179700                      H-OPER-COST-OUTLIER)
179800                             /
179900                    (H-CAPI-CSTCHG-RATIO  +
180000                     H-OPER-CSTCHG-RATIO)
180100             ON SIZE ERROR MOVE 0 TO H-OPER-DOLLAR-THRESHOLD
180200       ELSE MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
180300
180400***********************************************************
180500***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
180600***         RETURN CODE OF 67
180700
180800     IF PPS-RTC = 02
180900         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
181000            PPS-PC-COT-FLAG = 'Y'
181100             MOVE 67 TO PPS-RTC.
181200***********************************************************
181300
181400***********************************************************
181500***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
181600***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
181700***********************************************************
181800*
181900***********************************************************
182000***  'A' NOT VALID FY 2015 ON
182100*
182200*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
182300*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
182400*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
182500*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
182600
182700     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
182800        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
182900                H-CAPI-OUTLIER-PART.
183000
183100     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
183200        H-CAPI-PAYCDE-PCT1 > 0
183300        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
183400                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
183500         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
183600     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
183700
183800 3600-EXIT.   EXIT.
183900
184000***********************************************************
184100 3450-CALC-ADDITIONAL-HSP.
184200***********************************************************
184300*---------------------------------------------------------*
184400* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
184500* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
184600* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
184700*---------------------------------------------------------*
184800***  GET THE RBN UPDATING FACTOR
184900
185000*****YEARCHANGE 2019.0 ****************************************
185100     MOVE 0.997190 TO H-BUDG-NUTR190.
185200
185300
185400***  GET THE MARKET BASKET UPDATE FACTOR
185500*****YEARCHANGE 2019.0 ****************************************
185600        MOVE 1.01350 TO H-UPDATE-190.
185700
185800*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
185900*****YEARCHANGE 2019.0 ****************************************
186000     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
186100        P-EHR-REDUC-IND = ' '
186200        MOVE 1.01350 TO H-UPDATE-190.
186300
186400*****YEARCHANGE 2019.0 ****************************************
186500     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
186600        P-EHR-REDUC-IND = 'Y'
186700        MOVE 0.99175 TO H-UPDATE-190.
186800
186900*****YEARCHANGE 2019.0 ****************************************
187000     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
187100        P-EHR-REDUC-IND = ' '
187200        MOVE 1.00625 TO H-UPDATE-190.
187300
187400*****YEARCHANGE 2019.0 ****************************************
187500     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
187600        P-EHR-REDUC-IND = 'Y'
187700        MOVE 0.98450 TO H-UPDATE-190.
187800
187900
188000********YEARCHANGE 2019.0 *************************************
188100
188200     COMPUTE H-UPDATE-FACTOR ROUNDED =
188300                       (H-UPDATE-190 *
188400                        H-BUDG-NUTR190 *
188500                        HLD-MID-ADJ-FACT).
188600
188700     COMPUTE H-HSP-RATE ROUNDED =
188800         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT.
188900***************************************************************
189000*
189100*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
189200*       COMPUTE H-HSP-RATE ROUNDED =
189300*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
189400*    ELSE
189500*       COMPUTE H-HSP-RATE ROUNDED =
189600*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
189700*
189800***************************************************************
189900********YEARCHANGE 2011.0 *************************************
190000***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
190100***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
190200***     IN THE HSP COMPARRISON
190300
190400********YEARCHANGE 2014.0 *XXXXXX******************************
190500*      THE HSP BUCKET FOR SCH                      ************
190600*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
190700***************************************************************
190800     COMPUTE H-FSP-RATE ROUNDED =
190900        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
191000         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN *
191100         HLD-MID-ADJ-FACT) *
191200             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
191300                               +
191400                         H-OPER-OUTLIER-PART
191500                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
191600
191700****************************************************************
191800****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
191900*****        CHOICE
192000
192100     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
192200           COMPUTE H-OPER-HSP-PART ROUNDED =
192300             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
192400                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
192500     ELSE
192600         MOVE 0 TO H-OPER-HSP-PART.
192700
192800***************************************************************
192900***  YEARCHANGE TURNING MDH BACK ON ***************************
193000***************************************************************
193100***  GET THE MDH REBASE
193200
193300     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
193400         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
193500           COMPUTE H-OPER-HSP-PART ROUNDED =
193600         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
193700                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
193800
193900***************************************************************
194000***  TRANSITIONAL PAYMENT FOR FORMER MDHS                     *
194100***************************************************************
194200
194300***  HSP PAYMENT FOR CLAIMS BETWEEN 10/01/2016 - 09/30/2017
194400
194500*    IF  B-FORMER-MDH-PROVIDERS       AND
194600*       (B-DISCHARGE-DATE > 20160930  AND
194700*        B-DISCHARGE-DATE < 20171001)
194800*      IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
194900*        COMPUTE H-OPER-HSP-PART ROUNDED =
195000*          ((H-HSP-RATE - (H-FSP-RATE +
195100*              WK-UNCOMP-CARE-AMOUNT))* 0.75)*(1 / 3)
195200*            ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
195300*      END-IF
195400*    END-IF.
195500
195600 3450-EXIT.   EXIT.
195700
195800***********************************************************
195900 3800-CALC-TOT-AMT.
196000***********************************************************
196100***  CALCULATE TOTALS FOR CAPITAL
196200
196300     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
196400
196500***********************************************************
196600***  'A' NOT VALID FY 2015 ON
196700*
196800*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
196900*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
197000*       MOVE 0.00 TO H-CAPI-HSP-PCT.
197100
197200     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
197300        MOVE 0    TO H-CAPI-OLD-HARMLESS
197400        MOVE 1.00 TO H-CAPI-FSP-PCT
197500        MOVE 0.00 TO H-CAPI-HSP-PCT.
197600
197700     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
197800        MOVE 0    TO H-CAPI-OLD-HARMLESS
197900        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
198000        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
198100
198200     COMPUTE H-CAPI-HSP ROUNDED =
198300         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
198400
198500     COMPUTE H-CAPI-FSP ROUNDED =
198600         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
198700
198800     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
198900
199000     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
199100
199200     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
199300             H-CAPI-FSP
199400              * H-CAPI-DSH.
199500
199600     COMPUTE H-CAPI-IME-ADJ ROUNDED =
199700          H-CAPI-FSP *
199800                 H-WK-CAPI-IME-TEACH.
199900
200000     COMPUTE H-CAPI-OUTLIER ROUNDED =
200100             1.00 * H-CAPI-OUTLIER-PART.
200200
200300     COMPUTE H-CAPI2-B-FSP ROUNDED =
200400             1.00 * H-CAPI2-B-FSP-PART.
200500
200600     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
200700             1.00 * H-CAPI2-B-OUTLIER-PART.
200800***********************************************************
200900***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
201000***        THIS ZEROES OUT ALL CAPITAL DATA
201100
201200     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
201300        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
201400***********************************************************
201500
201600***********************************************************
201700***  CALCULATE FINAL TOTALS FOR OPERATING
201800
201900     IF (H-CAPI-OUTLIER > 0 AND
202000         PPS-OPER-OUTLIER-PART = 0)
202100            COMPUTE PPS-OPER-OUTLIER-PART =
202200                    PPS-OPER-OUTLIER-PART + .01.
202300
202400***********************************************************
202500*LOW VOLUME CALCULATIONS
202600***********************************************************
202700*---------------------------------------------------------*
202800* (YEARCHANGE 2016.0)
202900* LOW VOLUME PAYMENT ADD-ON PERCENT
203000*---------------------------------------------------------*
203100
203200     MOVE ZERO TO PPS-OPER-DSH-ADJ.
203300************************************************
203400* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
203500* EMPERICAL DSH
203600************************************************
203700     IF  H-OPER-DSH NUMERIC
203800         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
203900                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
204000
204100     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
204200                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
204300
204400
204500     COMPUTE PPS-OPER-FSP-PART ROUNDED =
204600                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
204700
204800     COMPUTE PPS-OPER-HSP-PART ROUNDED =
204900                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
205000
205100     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
205200                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
205300
205400     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
205500                                H-NEW-TECH-PAY-ADD-ON.
205600
205700     COMPUTE PPS-ISLET-ISOL-PAY-ADD-ON ROUNDED =
205800                                H-NEW-TECH-ADDON-ISLET.
205900
206000     IF P-NEW-TEMP-RELIEF-IND = 'Y'
206100        AND P-LV-ADJ-FACTOR > 0.00
206200        AND P-LV-ADJ-FACTOR <= 0.25
206300     COMPUTE WK-LOW-VOL-ADDON ROUNDED =
206400       (PPS-OPER-HSP-PART +
206500        PPS-OPER-FSP-PART +
206600        PPS-OPER-IME-ADJ +
206700        PPS-OPER-DSH-ADJ +
206800        PPS-OPER-OUTLIER-PART +
206900        H-CAPI-FSP +
207000        H-CAPI-IME-ADJ +
207100        H-CAPI-DSH-ADJ +
207200        H-CAPI-OUTLIER +
207300        WK-UNCOMP-CARE-AMOUNT +
207400        PPS-NEW-TECH-PAY-ADD-ON) * P-LV-ADJ-FACTOR
207500     ELSE
207600     COMPUTE WK-LOW-VOL-ADDON ROUNDED = 0.
207700
207800     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
207900     IF HMO-TAG  = 'Y'
208000        PERFORM 3850-HMO-IME-ADJ.
208100
208200***********************************************************
208300***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
208400
208500     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
208600             H-CAPI-FSP + H-CAPI-IME-ADJ +
208700             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
208800
208900         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
209000         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
209100
209200***********************************************************
209300* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
209400***********************************************************
209500*---------------------------------------------------------*
209600* (YEARCHANGE 2016.0)
209700* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
209800*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
209900*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
210000*   + BASED ON INDICATOR FROM THE PPS FILE
210100*   + NOT VALID IN PUERTO RICO
210200*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
210300*---------------------------------------------------------*
210400
210500     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
210600        PPS-OPER-HSP-PART +
210700        PPS-OPER-FSP-PART +
210800        PPS-OPER-IME-ADJ +
210900        PPS-OPER-DSH-ADJ +
211000        PPS-OPER-OUTLIER-PART +
211100        H-CAPI-TOTAL-PAY +
211200        WK-UNCOMP-CARE-AMOUNT +
211300        PPS-NEW-TECH-PAY-ADD-ON +
211400        WK-LOW-VOL-ADDON +
211500        H-READMIS-ADJUST-AMT +
211600        H-VAL-BASED-PURCH-ADJUST-AMT.
211700
211800     MOVE ZERO TO WK-HAC-AMOUNT.
211900
212000     IF P-PR-NEW-STATE AND
212100        P-HAC-REDUC-IND = 'Y'
212200           MOVE 53 TO PPS-RTC
212300           GO TO 3800-EXIT.
212400
212500     IF  P-HAC-REDUC-IND = 'Y'
212600         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
212700                   WK-HAC-TOTAL-PAYMENT * -0.01
212800     ELSE
212900         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
213000
213100***********************************************************
213200***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
213300************************************************
213400     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
213500                 WK-HAC-TOTAL-PAYMENT
213600                           +
213700                 H-WK-PASS-AMT-PLUS-MISC
213800                           +
213900                 H-BUNDLE-ADJUST-AMT
214000                           +
214100                 WK-HAC-AMOUNT
214200                           +
214300                 H-NEW-TECH-ADDON-ISLET.
214400
214500     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
214600              H-VAL-BASED-PURCH-PARTIPNT.
214700
214800     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
214900              H-VAL-BASED-PURCH-ADJUST.
215000
215100     MOVE     P-HOSP-READMISSION-REDU    TO
215200              H-HOSP-READMISSION-REDU.
215300
215400     MOVE     P-HOSP-HRR-ADJUSTMT        TO
215500              H-HOSP-HRR-ADJUSTMT.
215600
215700 3800-EXIT.   EXIT.
215800
215900 3850-HMO-IME-ADJ.
216000***********************************************************
216100***  HMO CALC FOR PASS-THRU ADDON
216200
216300     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
216400          (P-NEW-PASS-AMT-PLUS-MISC -
216500          (P-NEW-PASS-AMT-ORGAN-ACQ +
216600           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
216700
216800***********************************************************
216900***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
217000
217100     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
217200                   PPS-OPER-IME-ADJ * .0.
217300
217400***********************************************************
217500
217600
217700 3900A-CALC-OPER-DSH.
217800
217900***  OPERATING DSH CALCULATION
218000
218100      MOVE 0.0000 TO H-OPER-DSH.
218200
218300      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
218400                                     + P-NEW-MEDICAID-RATIO).
218500
218600***********************************************************
218700**1**    0-99 BEDS
218800***  NOT TO EXCEED 12%
218900
219000      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
219100                               AND H-WK-OPER-DSH > .1499
219200                               AND H-WK-OPER-DSH < .2020
219300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
219400                                      * .65 + .025
219500        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
219600
219700      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
219800                               AND H-WK-OPER-DSH > .2019
219900        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
220000                                      * .825 + .0588
220100        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
220200
220300***********************************************************
220400**2**   100 + BEDS
220500***  NO CAP >> CAN EXCEED 12%
220600
220700      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
220800                               AND H-WK-OPER-DSH > .1499
220900                               AND H-WK-OPER-DSH < .2020
221000        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
221100                                      * .65 + .025.
221200
221300      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
221400                               AND H-WK-OPER-DSH > .2019
221500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
221600                                      * .825 + .0588.
221700
221800***********************************************************
221900**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
222000***  NOT TO EXCEED 12%
222100
222200      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
222300                               AND H-WK-OPER-DSH > .1499
222400                               AND H-WK-OPER-DSH < .2020
222500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
222600                                 * .65 + .025
222700        IF H-OPER-DSH > .1200
222800              MOVE .1200 TO H-OPER-DSH.
222900
223000      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
223100                               AND H-WK-OPER-DSH > .2019
223200        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
223300                                 * .825 + .0588
223400        IF H-OPER-DSH > .1200
223500                 MOVE .1200 TO H-OPER-DSH.
223600***********************************************************
223700**4**   OTHER RURAL HOSPITALS 500 BEDS +
223800***  NO CAP >> CAN EXCEED 12%
223900
224000      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
224100                               AND H-WK-OPER-DSH > .1499
224200                               AND H-WK-OPER-DSH < .2020
224300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
224400                                 * .65 + .025.
224500
224600      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
224700                               AND H-WK-OPER-DSH > .2019
224800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
224900                                 * .825 + .0588.
225000
225100***********************************************************
225200**7**   RURAL HOSPITALS SCH
225300***  NOT TO EXCEED 12%
225400
225500      IF W-CBSA-SIZE = 'R'
225600         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
225700                               AND H-WK-OPER-DSH > .1499
225800                               AND H-WK-OPER-DSH < .2020
225900         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
226000                                 * .65 + .025
226100        IF H-OPER-DSH > .1200
226200                 MOVE .1200 TO H-OPER-DSH.
226300
226400      IF W-CBSA-SIZE = 'R'
226500         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
226600                               AND H-WK-OPER-DSH > .2019
226700         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
226800                                 * .825 + .0588
226900        IF H-OPER-DSH > .1200
227000                 MOVE .1200 TO H-OPER-DSH.
227100
227200***********************************************************
227300**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
227400***  RRC OVERRIDES SCH CAP
227500***  NO CAP >> CAN EXCEED 12%
227600
227700         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
227800                                   '17' OR '22')
227900                               AND H-WK-OPER-DSH > .1499
228000                               AND H-WK-OPER-DSH < .2020
228100         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
228200                                 * .65 + .025.
228300
228400         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
228500                                   '17' OR '22')
228600                               AND H-WK-OPER-DSH > .2019
228700         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
228800                                 * .825 + .0588.
228900
229000      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
229100
229200 3900A-EXIT.   EXIT.
229300
229400 4000-CALC-TECH-ADDON.
229500
229600***********************************************************
229700***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
229800
229900     COMPUTE PPS-OPER-HSP-PART ROUNDED =
230000         H-OPER-HSP-PCT * H-OPER-HSP-PART.
230100
230200     COMPUTE PPS-OPER-FSP-PART ROUNDED =
230300         H-OPER-FSP-PCT * H-OPER-FSP-PART.
230400
230500     MOVE ZERO TO PPS-OPER-DSH-ADJ.
230600
230700     IF  H-OPER-DSH NUMERIC
230800             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
230900             (PPS-OPER-FSP-PART
231000              * H-OPER-DSH) * .25.
231100
231200     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
231300             PPS-OPER-FSP-PART *
231400             H-OPER-IME-TEACH.
231500
231600     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
231700             PPS-OPER-FSP-PART +
231800             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
231900             WK-UNCOMP-CARE-AMOUNT.
232000
232100***********************************************************
232200* NEW TECHNOLOGY ADD-ON CODE *
232300***********************************************************
232400     MOVE 1 TO IDX-TECH.
232500     INITIALIZE H-CSTMED-STOP.
232600     INITIALIZE H-TECH-ADDON-ISLET-CNTR.
232700
232800     PERFORM 4010-FLAG-NEW-TECH THRU 4010-EXIT
232900      VARYING IDX-TECH FROM 1 BY 1 UNTIL IDX-TECH > 25.
233000
233100     IF PROC-ANDEXXA-FLAG = 'Y'
233200       MOVE  14062.50 TO H-CSTMED-STOP.
233300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
233400
233500     IF PROC-AQUABEAM-FLAG = 'Y'
233600       MOVE   1250.00 TO H-CSTMED-STOP.
233700       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
233800
233900     IF PROC-DEFITELIO-FLAG = 'Y'
234000       MOVE  80500.00 TO H-CSTMED-STOP.
234100       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
234200
234300     IF PROC-GIAPREZA-FLAG = 'Y'
234400       MOVE   1500.00 TO H-CSTMED-STOP.
234500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
234600
234700     IF DIAG-ISLET-FLAG = 'Y' AND PROC-ISLET-FLAG = 'Y'
234800       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
234900     ELSE
235000       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
235100
235200     IF PROC-KYMRIAH-FLAG = 'Y'
235300       MOVE 186500.00 TO H-CSTMED-STOP.
235400       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
235500
235600     IF PROC-PLAZO-FLAG = 'Y'
235700       MOVE   2722.50 TO H-CSTMED-STOP.
235800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
235900
236000     IF PROC-REMEDE1-FLAG = 'Y' AND PROC-REMEDE2-FLAG = 'Y'
236100       AND PROC-REMEDE3-FLAG = 'Y'
236200       MOVE  17250.00 TO H-CSTMED-STOP.
236300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
236400
236500     IF PROC-SENTINEL-FLAG = 'Y'
236600       MOVE  1400.00 TO H-CSTMED-STOP.
236700       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
236800
236900     IF PROC-STELARA-FLAG = 'Y'
237000       MOVE  2400.00 TO H-CSTMED-STOP.
237100       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
237200
237300     IF NDC-VABOMERE-FLAG = 'Y'
237400       MOVE  5544.00 TO H-CSTMED-STOP.
237500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
237600
237700     IF PROC-VYXEOS-FLAG = 'Y'
237800       MOVE 36425.00 TO H-CSTMED-STOP.
237900       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
238000
238100     IF PROC-ZINPLAVA-FLAG = 'Y'
238200       MOVE  1900.00 TO H-CSTMED-STOP.
238300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
238400
238500***********************************************************
238600*  ALL NEW TECH MUST BE CALCULATED BEFORE
238700*  5500-CAP-CALC-TECH-ADD-ON
238800***********************************************************
238900     PERFORM 5500-CAP-CALC-TECH-ADD-ON THRU 5500-EXIT.
239000
239100     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
239200             H-OPER-FSP-PART +
239300             H-NEW-TECH-PAY-ADD-ON.
239400
239500 4000-EXIT.    EXIT.
239600
239700************************************
239800* NEW TECHNOLOGY ADD-ON FLAG LOGIC *
239900************************************
240000 4010-FLAG-NEW-TECH.
240100
240200     MOVE B-PROCEDURE-CODE(IDX-TECH) TO WK-PROC-NEW-TECH.
240300     MOVE B-DIAGNOSIS-CODE(IDX-TECH) TO WK-DIAG-NEW-TECH.
240400     MOVE B-NDC-NUMBER TO WK-NDC-NEW-TECH.
240500
240600     IF PROC-ANDEXXA
240700       MOVE 'Y' TO PROC-ANDEXXA-FLAG.
240800
240900     IF PROC-AQUABEAM
241000       MOVE 'Y' TO PROC-AQUABEAM-FLAG.
241100
241200     IF PROC-DEFITELIO
241300       MOVE 'Y' TO PROC-DEFITELIO-FLAG.
241400
241500     IF PROC-GIAPREZA
241600       MOVE 'Y' TO PROC-GIAPREZA-FLAG.
241700
241800     IF PROC-ISLET
241900       MOVE 'Y' TO PROC-ISLET-FLAG
242000       COMPUTE H-TECH-ADDON-ISLET-CNTR =
242100          H-TECH-ADDON-ISLET-CNTR + 1.
242200
242300     IF PROC-KYMRIAH
242400       MOVE 'Y' TO PROC-KYMRIAH-FLAG.
242500
242600     IF PROC-PLAZO
242700       MOVE 'Y' TO PROC-PLAZO-FLAG.
242800
242900     IF PROC-REMEDE1
243000       MOVE 'Y' TO PROC-REMEDE1-FLAG.
243100
243200     IF PROC-REMEDE2
243300       MOVE 'Y' TO PROC-REMEDE2-FLAG.
243400
243500     IF PROC-REMEDE3
243600       MOVE 'Y' TO PROC-REMEDE3-FLAG.
243700
243800     IF PROC-SENTINEL
243900       MOVE 'Y' TO PROC-SENTINEL-FLAG.
244000
244100     IF PROC-STELARA
244200       MOVE 'Y' TO PROC-STELARA-FLAG.
244300
244400     IF PROC-VYXEOS
244500       MOVE 'Y' TO PROC-VYXEOS-FLAG.
244600
244700     IF PROC-ZINPLAVA
244800       MOVE 'Y' TO PROC-ZINPLAVA-FLAG.
244900
245000     IF DIAG-ISLET
245100       MOVE 'Y' TO DIAG-ISLET-FLAG.
245200
245300     IF NDC-VABOMERE
245400       MOVE 'Y' TO NDC-VABOMERE-FLAG.
245500
245600 4010-EXIT.   EXIT.
245700
245800*******************************************
245900* NEW TECHNOLOGY ADD-ON CALCULATION LOGIC *
246000*******************************************
246100 4020-NEW-TECH-ADD-ON.
246200
246300     MOVE 0 TO H-NEW-TECH-ADDON
246400               H-LESSER-STOP-1
246500               H-LESSER-STOP-2.
246600
246700     COMPUTE H-LESSER-STOP-1 ROUNDED =
246800                  H-CSTMED-STOP.
246900
247000     COMPUTE H-LESSER-STOP-2 ROUNDED =
247100          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
247200                     H-BASE-DRG-PAYMENT)) * .5.
247300
247400     IF H-LESSER-STOP-2 > 0
247500        IF H-LESSER-STOP-1 < H-LESSER-STOP-2
247600         MOVE H-LESSER-STOP-1 TO
247700                                H-NEW-TECH-ADDON
247800        ELSE
247900         MOVE H-LESSER-STOP-2 TO
248000                                H-NEW-TECH-ADDON
248100     ELSE
248200        MOVE ZEROES          TO H-NEW-TECH-ADDON.
248300
248400     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
248500             H-NEW-TECH-PAY-ADD-ON +
248600             H-NEW-TECH-ADDON.
248700
248800     MOVE 0 TO H-NEW-TECH-ADDON
248900               H-LESSER-STOP-1
249000               H-LESSER-STOP-2
249100               H-CSTMED-STOP.
249200
249300 4020-EXIT.    EXIT.
249400
249500***********************************************************
249600* TECHNICAL TRANSPLANTATION OF CELLS                      *
249700***********************************************************
249800 4100-ISLET-ISOLATION-ADD-ON.
249900
250000     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
250100
250200     IF  H-TECH-ADDON-ISLET-CNTR = 1
250300     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
250400           GO TO 4100-EXIT.
250500
250600     IF  H-TECH-ADDON-ISLET-CNTR > 1
250700     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
250800           GO TO 4100-EXIT.
250900
251000 4100-EXIT.    EXIT.
251100
251200***********************************************************
251300* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
251400* DISCHARGE COUNTS.
251500***********************************************************
251600*4400-LOWVOL-CODE-RTN.
251700*
251800*    SET LOWVOL-IDX TO 1.
251900*    SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
252000*        AT END
252100*          MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
252200*          MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
252300*      WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
252400*        MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
252500*                           TO MESWK-LOWVOL-PROV-DISCHG.
252600*
252700*4400-EXIT.   EXIT.
252800
252900*****************************************************************
253000* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR DISCHARGE *
253100* COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER FILE       *
253200*****************************************************************
253300 4410-UNCOMP-CARE-CODE-RTN.
253400
253500*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
253600*
253700*    SET UNCOMP-CARE-IDX TO 1.
253800*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
253900*        AT END
254000*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
254100*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
254200*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
254300*                           TO WK-UNCOMP-CARE-AMOUNT.
254400*
254500        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
254600
254700        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
254800
254900 4410-EXIT.   EXIT.
255000
255100
255200**************************************************************
255300* CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM *
255400**************************************************************
255500 5500-CAP-CALC-TECH-ADD-ON.
255600
255700     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
255800     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
255900
256000     COMPUTE H-OPER-BILL-COSTS ROUNDED =
256100         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
256200         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
256300
256400     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
256500                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
256600
256700     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
256800                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
256900
257000     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
257100         H-NEW-TECH-ADDON-CAPDIF  > 0
257200        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
257300             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
257400
257500 5500-EXIT.    EXIT.
257600
257700***********************************************************
257800 6000-CALC-READMIS-REDU.
257900***********************************************************
258000*---------------------------------------------------------*
258100* (YEARCHANGE 2016.0)
258200* READMISSIONS PROCESS ADJUSTMENTS
258300*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
258400*---------------------------------------------------------*
258500
258600     MOVE 0 TO H-READMIS-ADJUST-AMT.
258700
258800     IF P-HOSP-READMISSION-REDU = '1'
258900           GO TO 6000-EDIT-READMISN
259000     ELSE
259100           NEXT SENTENCE.
259200
259300     IF P-HOSP-READMISSION-REDU = '0' AND
259400        P-HOSP-HRR-ADJUSTMT = 0.0000
259500           MOVE ZEROES TO H-READMIS-ADJUST-AMT
259600           GO TO 6000-EXIT.
259700
259800     IF P-HOSP-READMISSION-REDU = '0' AND
259900        P-HOSP-HRR-ADJUSTMT > 0.0000
260000           MOVE 65 TO PPS-RTC
260100           MOVE ZEROES TO H-READMIS-ADJUST-AMT
260200           GO TO 6000-EXIT.
260300
260400     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
260500                                  '6' OR '7' OR '8' OR
260600                                  '9' OR ' '
260700           MOVE 65 TO PPS-RTC
260800           MOVE ZEROES TO H-READMIS-ADJUST-AMT
260900           GO TO 6000-EXIT.
261000
261100 6000-EDIT-READMISN.
261200
261300     IF P-HOSP-HRR-ADJUSTMT < 0.9700
261400           MOVE 65 TO PPS-RTC
261500           MOVE ZEROES TO H-READMIS-ADJUST-AMT
261600           GO TO 6000-EXIT.
261700
261800     IF P-HOSP-HRR-ADJUSTMT > 1.0000
261900           MOVE 65 TO PPS-RTC
262000           MOVE ZEROES TO H-READMIS-ADJUST-AMT
262100           GO TO 6000-EXIT.
262200
262300     IF P-READ-INVALID-STATE
262400           MOVE 65 TO PPS-RTC
262500           MOVE ZEROES TO H-READMIS-ADJUST-AMT
262600           GO TO 6000-EXIT.
262700
262800 6000-COMPUTE-READMISN.
262900
263000        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
263100              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
263200                H-OPER-BASE-DRG-PAY).
263300
263400 6000-EXIT.    EXIT.
263500
263600***********************************************************
263700 7000-CALC-VALUE-BASED-PURCH.
263800***********************************************************
263900*---------------------------------------------------------*
264000* (YEARCHANGE 2016.0)
264100* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
264200*   + FY17: RANGE OF ALLOWABLE FACTORS (< 0.98 OR > 2.0)
264300*---------------------------------------------------------*
264400
264500     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
264600
264700     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
264800           NEXT SENTENCE
264900     ELSE
265000           MOVE 68 TO PPS-RTC
265100           GO TO 7000-EXIT.
265200
265300     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
265400           GO TO 7000-EXIT.
265500
265600     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
265700         P-NEW-CBSA-HOSP-QUAL-IND = '1'
265800           NEXT SENTENCE
265900     ELSE
266000           MOVE 68 TO PPS-RTC
266100           GO TO 7000-EXIT.
266200
266300     IF  P-VBP-INVALID-STATE
266400           MOVE 68 TO PPS-RTC
266500           GO TO 7000-EXIT
266600     ELSE
266700           NEXT SENTENCE.
266800
266900     IF P-VAL-BASED-PURCH-ADJUST < 0.9800000000 OR
267000        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
267100           MOVE 68 TO PPS-RTC
267200           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
267300           GO TO 7000-EXIT
267400     ELSE
267500           GO TO 7000-COMPUTE-VAL-BASED-PUR.
267600
267700 7000-COMPUTE-VAL-BASED-PUR.
267800
267900     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
268000              ((P-VAL-BASED-PURCH-ADJUST *
268100                  H-OPER-BASE-DRG-PAY) -
268200                  H-OPER-BASE-DRG-PAY).
268300
268400 7000-EXIT.    EXIT.
268500
268600***********************************************************
268700 8000-CALC-BUNDLE-REDU.
268800***********************************************************
268900***** CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
269000***********************************************************
269100
269200     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
269300     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
269400
269500     IF '61' =  B-DEMO-CODE1  OR
269600                B-DEMO-CODE2  OR
269700                B-DEMO-CODE3  OR
269800                B-DEMO-CODE4
269900         NEXT SENTENCE
270000     ELSE
270100         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
270200           GO TO 8000-EXIT.
270300
270400     IF P-MODEL1-BUNDLE-DISPRCNT > .00
270500           GO TO 8000-COMPUTE-BUNDLE
270600     ELSE
270700           NEXT SENTENCE.
270800
270900     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
271000           GO TO 8000-EXIT.
271100
271200 8000-COMPUTE-BUNDLE.
271300
271400     IF  B-DISCHARGE-DATE < 20140401 AND
271500         P-MODEL1-BUNDLE-DISPRCNT = .01
271600         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
271700          (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
271800     ELSE
271900         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
272000          (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
272100
272200        COMPUTE H-BUNDLE-ADJUST-AMT      ROUNDED =
272300              ((WK-MODEL1-BUNDLE-DISPRCNT *
272400                                     H-OPER-BASE-DRG-PAY) -
272500                H-OPER-BASE-DRG-PAY).
272600
272700        COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
272800
272900 8000-EXIT.    EXIT.
273000
273100***********************************************************
273200 9000-CALC-EHR-SAVING.
273300***********************************************************
273400*---------------------------------------------------------*
273500* (YEARCHANGE 2017.0)
273600* CASES INVOLVING EHR SAVINGS
273700*   + FY17: ANNUAL UPDATE TO BELOW VALUES
273800*   + EHR-FULL = FULL MB / NO EHR MB
273900*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
274000*---------------------------------------------------------*
274100
274200     MOVE 1.021930930 TO H-MB-RATIO-EHR-FULL.
274300     MOVE 1.022092433 TO H-MB-RATIO-EHR-QUAL-FULL.
274400     MOVE 0 TO H-EHR-SUBSAV-QUANT.
274500     MOVE 0 TO H-EHR-SUBSAV-LV.
274600     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
274700     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
274800
274900     IF P-EHR-REDUC-IND = 'Y'
275000         NEXT SENTENCE
275100     ELSE
275200         GO TO 9000-EXIT.
275300
275400 9000-COMPUTE-EHR.
275500
275600* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
275700* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
275800* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
275900* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
276000* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
276100* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
276200* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
276300* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
276400* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
276500* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
276600* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
276700* OUT.
276800*
276900* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
277000* SAVINGS CALCULATION SO CALCULATE AN OPERATING
277100* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
277200* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
277300* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
277400
277500      COMPUTE H-EHR-SUBSAV-QUANT =
277600           (PPS-OPER-HSP-PART +
277700            PPS-OPER-FSP-PART +
277800            PPS-OPER-DSH-ADJ +
277900            PPS-OPER-IME-ADJ +
278000            H-READMIS-ADJUST-AMT +
278100            H-VAL-BASED-PURCH-ADJUST-AMT +
278200            H-BUNDLE-ADJUST-AMT).
278300
278400* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
278500* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
278600* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
278700* READM VBP AND BUNDLE
278800* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
278900* FOR EHR SAVINGS FIELD;
279000
279100      MOVE 0 TO H-EHR-SUBSAV-LV.
279200
279300      IF P-NEW-TEMP-RELIEF-IND = 'Y'
279400         AND P-LV-ADJ-FACTOR > 0.00
279500         AND P-LV-ADJ-FACTOR <= 0.25
279600      COMPUTE H-EHR-SUBSAV-LV =
279700          (PPS-OPER-HSP-PART +
279800           PPS-OPER-FSP-PART +
279900           PPS-OPER-DSH-ADJ +
280000           PPS-OPER-IME-ADJ ) * P-LV-ADJ-FACTOR.
280100
280200      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
280300           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
280400
280500* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
280600* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
280700* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
280800* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
280900* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
281000* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
281100* OF .625 ONLY.
281200
281300       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
281400       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
281500
281600     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
281700        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
281800          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
281900
282000        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
282100          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
282200
282300 9000-EXIT.    EXIT.
282400
282500*---------------------------------------------------------*
282600* (YEARCHANGE 2016.0)
282700*---------------------------------------------------------*
282800 9010-CALC-STANDARD-CHG.
282900
283000***********************************************************
283100***ÝCM-P3¨ STANDARDIZED OPERATING COST CALCULATION
283200
283300     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
283400               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
283500        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
283600        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
283700        ((H-LABOR-PCT * H-WAGE-INDEX) +
283800               (H-NONLABOR-PCT * H-OPER-COLA))
283900     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
284000
284100***********************************************************
284200***ÝCM-P3¨ STANDARDIZED CAPITAL COST CALCULATION
284300
284400     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
284500       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
284600        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
284700               (H-CAPI-GAF * H-CAPI-COLA)
284800     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
284900
285000***********************************************************
285100***ÝCM-P3¨ STANDARDIZED OPERATING TRESHOLD
285200
285300     MOVE 5646.08 TO H-OPER-BASE.
285400
285500     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
285600      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
285700                        +
285800           (H-OPER-BASE * H-DRG-WT-FRCTN)
285900                        +
286000              H-NEW-TECH-PAY-ADD-ON.
286100
286200******************************************************
286300***ÝCM-P3¨ STANDARDIZED CAPITAL TRESHOLD
286400
286500     MOVE 459.41 TO H-CAPI-BASE.
286600
286700     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
286800     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
286900                     +
287000     (H-CAPI-BASE * H-DRG-WT-FRCTN).
287100
287200******************************************************
287300***ÝCM-P3¨ STANDARDIZED OPERATING OUTLIER CALCULATION
287400
287500     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
287600        (H-OPER-STDZ-DOLLAR-THRESHOLD +
287700                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
287800                          AND
287900         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
288000
288100       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
288200        (H-CSTOUT-PCT  *
288300        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
288400
288500     ELSE
288600       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
288700
288800******************************************************
288900***ÝCM-P3¨ STANDARDIZED CAPITAL OUTLIER CALCULATION
289000
289100     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
289200        (H-OPER-STDZ-DOLLAR-THRESHOLD +
289300                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
289400                          AND
289500         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
289600
289700      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
289800      (H-CSTOUT-PCT  *
289900      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
290000     ELSE
290100      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
290200
290300*******************************************************
290400***ÝCM-P3¨ STANDARDIZED ALLOWED AMOUNT CALCULATION
290500
290600      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
290700       (H-OPER-BASE + H-CAPI-BASE)
290800                 *
290900       H-DRG-WT-FRCTN
291000                 +
291100       H-OPER-STDZ-COST-OUTLIER
291200                 +
291300       H-CAPI-STDZ-COST-OUTLIER
291400                 +
291500       H-NEW-TECH-PAY-ADD-ON.
291600
291700 9010-EXIT.    EXIT.
291800
291900************************************************************************
