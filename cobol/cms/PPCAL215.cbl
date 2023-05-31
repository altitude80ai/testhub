000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL215.
000300*AUTHOR.                    DDS TEAM.
000400*REMARKS.                   CMS.
000500 DATE-COMPILED.
000600
000700 ENVIRONMENT DIVISION.
000800 CONFIGURATION SECTION.
000900 SOURCE-COMPUTER.            IBM-370.
001000 OBJECT-COMPUTER.            IBM-370.
001100 INPUT-OUTPUT SECTION.
001200 FILE-CONTROL.
001300
001400 DATA DIVISION.
001500 FILE SECTION.
001600
001700 WORKING-STORAGE SECTION.
001800 01  W-STORAGE-REF                  PIC X(46)  VALUE
001900     'PPCAL215      - W O R K I N G   S T O R A G E'.
002000 01  CAL-VERSION                    PIC X(05)  VALUE 'C21.5'.
002100 01  HMO-FLAG                       PIC X      VALUE 'N'.
002200 01  HMO-TAG                        PIC X      VALUE SPACE.
002300 01  OUTLIER-RECON-FLAG             PIC X      VALUE 'N'.
002400 01  TEMP-RELIEF-FLAG               PIC X      VALUE 'N'.
002500 01  NON-TEMP-RELIEF-PAYMENT        PIC 9(07)V9(02) VALUE ZEROES.
002600 01  WK-H-OPER-DOLLAR-THRESHOLD     PIC 9(07)V9(09) VALUE ZEROES.
002700 01  WK-LOW-VOL25PCT                PIC 99V999999 VALUE 01.000.
002800 01  WK-LOW-VOL-ADDON               PIC 9(07)V9(02).
002900 01  WK-MODEL1-BUNDLE-DISPRCNT      PIC S9(01)V9(03).
003000 01  WK-HAC-TOTAL-PAYMENT           PIC 9(07)V9(02).
003100 01  WK-HAC-AMOUNT                  PIC S9(07)V9(02).
003200 01  R1                             PIC S9(04) COMP SYNC.
003300 01  R2                             PIC S9(04) COMP SYNC.
003400 01  R3                             PIC S9(04) COMP SYNC.
003500 01  R4                             PIC S9(04) COMP SYNC.
003600 01  H-OPER-DSH-SCH                 PIC 9(01)V9(04).
003700 01  H-OPER-DSH-RRC                 PIC 9(01)V9(04).
003800 01  H-MB-RATIO-EHR-FULL            PIC 9(01)V9(09).
003900 01  H-MB-RATIO-EHR-QUAL-FULL       PIC 9(01)V9(09).
004000 01  H-EHR-SUBSAV-QUANT             PIC S9(07)V9(02).
004100 01  H-EHR-SUBSAV-LV                PIC S9(07)V9(02).
004200 01  H-EHR-SUBSAV-QUANT-INCLV       PIC S9(07)V9(02).
004300 01  H-EHR-RESTORE-FULL-QUANT       PIC S9(07)V9(02).
004400 01  IDX-TECH                       PIC 9(02).
004500
004600*-----------------------------------------------------*
004700* LABOR & NON-LABOR RATES TABLE                       *
004800*-----------------------------------------------------*
004900
005000 COPY RATEX213.
005100
005200*---------------------------------------------------------*
005300* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE (EFF. FY'19) *
005400*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE                 *
005500*---------------------------------------------------------*
005600
005700 COPY DRGSX211.
005800
005900*---------------------------------------------------------------*
006000* TWO MIDNIGHT STAY POLICY ADJUSTMENT FACTOR TABLE (EFF. FY'01) *
006100*---------------------------------------------------------------*
006200
006300 COPY MIDNIGHT.
006400
006500*-----------------------------------------------------*
006600* NEW TECHNOLOGY ADD-ON PAYMENT ELIGIBILITY VARIABLES *
006700*-----------------------------------------------------*
006800
006900 COPY NTECH211.
007000
007100*-----------------------------------------------------*
007200* COVID-19 DRG ADJUSTMENT ELIGIBILITY VARIABLES       *
007300*-----------------------------------------------------*
007400
007500 01  IDX-COVID-DIAG                 PIC 9(02).
007600 01  IDX-COVID-PROC                 PIC 9(02).
007700 01  IDX-COVID-COND                 PIC 9(02).
007800 01  WK-COVID19-VARIABLES.
007900     05  WK-DIAG-COVID19            PIC X(07).
008000         88  DIAG-COVID1
008100               VALUE 'B9729  '.
008200         88  DIAG-COVID2
008300               VALUE 'U071   '.
008400     05  WK-PROC-COVID19            PIC X(07).
008500         88  PROC-COVID1
008600               VALUE 'XW033E5' 'XW043E5' 'XW13325' 'XW14325'.
008700         88  PROC-COVID2
008800               VALUE 'XW0DXF5' '3E0G7GC' '3E0H7GC'.
008900         88  PROC-COVID3
009000               VALUE 'XW0DXM6' 'XW0G7M6' 'XW0H7M6'.
009100     05  WK-COND-COVID19            PIC X(02).
009200         88  COND-COVID19-NOADJ
009300               VALUE 'ZA'.
009400     05  WK-COVID19-FLAGS.
009500         10  DIAG-COVID1-FLAG       PIC X(01).
009600         10  DIAG-COVID2-FLAG       PIC X(01).
009700         10  PROC-COVID1-FLAG       PIC X(01).
009800         10  PROC-COVID2-FLAG       PIC X(01).
009900         10  PROC-COVID3-FLAG       PIC X(01).
010000         10  COND-COVID1-FLAG       PIC X(01).
010100 01  COVID-ADJ                      PIC 9(01)V9(01).
010200 01  NCTAP-ADD-ON                   PIC 9(06)V9(02).
010300 01  NCTAP-ADD-ON-FLAG              PIC X(01).
010400
010500*-----------------------------------------------------*
010600* CAR-T & CLIN TRIAL REDUCTION ELIGIBILITY VARIABLES  *
010700* NO COST PRODUCT DETERMINATION FOR FY 2021           *
010800*-----------------------------------------------------*
010900
011000 01  IDX-CLIN                       PIC 9(02).
011100 01  IDX-CART                       PIC 9(02).
011200 01  WK-CLIN-VARIABLES.
011300     05  WK-DIAG-CLIN               PIC X(07).
011400         88  DIAG-CLIN
011500               VALUE 'Z006   '.
011600     05  WK-CLIN-FLAGS.
011700         10  DIAG-CLIN-FLAG         PIC X(01).
011800 01  WK-CART-VARIABLES.
011900     05  WK-COND-CART               PIC X(02).
012000         88  COND-CART-NCP
012100               VALUE 'ZB'.
012200         88  COND-CART-NONCP
012300               VALUE 'ZC'.
012400     05  WK-CART-FLAGS.
012500         10  COND-CART-NCP-FLAG     PIC X(01).
012600         10  COND-CART-NONCP-FLAG   PIC X(01).
012700 01  NO-COST-PRODUCT                PIC 9(01)V9(02).
012800
012900***********************************************************
013000***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
013100***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
013200***********************************************************
013300
013400 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
013500 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
013600 01  MES-PPS-PROV                   PIC X(06).
013700 01  MES-PPS-STATE                  PIC X(02).
013800 01  MES-INTRO                      PIC X(53) VALUE SPACES.
013900 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
014000 01  MES-SSRFBN.
014100     05 MES-SSRFBN-STATE PIC 99.
014200     05 FILLER           PIC XX.
014300     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
014400     05 FILLER           PIC XX.
014500     05 MES-SSRFBN-CODE2 PIC 99.
014600     05 FILLER           PIC XX.
014700     05 MES-SSRFBN-STNAM PIC X(20).
014800     05 MES-SSRFBN-REST  PIC X(22).
014900
015000 01 WK-HLDDRG-DATA.
015100     05  HLDDRG-DATA.
015200         10  HLDDRG-DRGX               PIC X(03).
015300         10  FILLER1                   PIC X(01).
015400         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
015500         10  FILLER2                   PIC X(01).
015600         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
015700         10  FILLER3                   PIC X(05).
015800         10  HLDDRG-LOW                PIC X(01).
015900         10  FILLER5                   PIC X(01).
016000         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
016100         10  FILLER6                   PIC X(02).
016200         10  HLDDRG-PAC                PIC X(01).
016300         10  FILLER7                   PIC X(01).
016400         10  HLDDRG-SPPAC              PIC X(01).
016500         10  FILLER8                   PIC X(02).
016600         10  HLDDRG-DESC               PIC X(26).
016700
016800 01 WK-HLDDRG-DATA2.
016900     05  HLDDRG-DATA2.
017000         10  HLDDRG-DRGX2               PIC X(03).
017100         10  FILLER21                   PIC X(01).
017200         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
017300         10  FILLER22                   PIC X(01).
017400         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
017500         10  FILLER23                   PIC X(05).
017600         10  HLDDRG-LOW2                PIC X(01).
017700         10  FILLER25                   PIC X(01).
017800         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
017900         10  FILLER26                   PIC X(02).
018000         10  HLDDRG-TRANS-FLAGS.
018100                   88  D-DRG-POSTACUTE-50-50
018200                   VALUE 'Y Y'.
018300                   88  D-DRG-POSTACUTE-PERDIEM
018400                   VALUE 'Y  '.
018500             15  HLDDRG-PAC2            PIC X(01).
018600             15  FILLER27               PIC X(01).
018700             15  HLDDRG-SPPAC2          PIC X(01).
018800         10  FILLER28                   PIC X(02).
018900         10  HLDDRG-DESC2               PIC X(26).
019000         10  HLDDRG-VALID               PIC X(01).
019100
019200 01  MES-LOWVOL.
019300     05  MES-LOWVOL-PROV             PIC X(6).
019400     05  FILLER                      PIC XXX.
019500     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
019600
019700 01  WK-UNCOMP-CARE.
019800     05  WK-UNCOMP-CARE-PROV         PIC X(6).
019900     05  FILLER                      PIC X.
020000     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
020100
020200 01 WK-HLD-MID-DATA.
020300     05  HLD-MID-DATA.
020400         10  HLD-MID-MSAX              PIC X(04).
020500         10  FILLER1                   PIC X(01).
020600         10  HLD-MID-ADJ-FACT          PIC 9(02)V9(06).
020700
020800 01  HLD-PPS-DATA.
020900         10  HLD-PPS-RTC                PIC 9(02).
021000         10  HLD-PPS-WAGE-INDX          PIC 9(02)V9(04).
021100         10  HLD-PPS-OUTLIER-DAYS       PIC 9(03).
021200         10  HLD-PPS-AVG-LOS            PIC 9(02)V9(01).
021300         10  HLD-PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
021400         10  HLD-PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
021500         10  HLD-PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
021600         10  HLD-PPS-OPER-HSP-PART      PIC 9(06)V9(02).
021700         10  HLD-PPS-OPER-FSP-PART      PIC 9(06)V9(02).
021800         10  HLD-PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
021900         10  HLD-PPS-REG-DAYS-USED      PIC 9(03).
022000         10  HLD-PPS-LTR-DAYS-USED      PIC 9(02).
022100         10  HLD-PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
022200         10  HLD-PPS-CALC-VERS          PIC X(05).
022300
022400 LINKAGE SECTION.
022500
022600************************************************************************
022700* REVIEW CODES DIRECT THE PPCAL SUBROUTINE IN HOW TO PAY THE BILL.     *
022800*                                                                      *
022900* COMMENTS:                                                            *
023000* CLAIMS WITH CONDITION CODE 66 SHOULD BE PROCESSED UNDER REVIEW CODE  *
023100* 06, 07, OR 11 AS APPROPRIATE TO EXCLUDE ANY OUTLIER COMPUTATION.     *
023200*                                                                      *
023300* REVIEW-CODE:                                                         *
023400*   00: PAY-WITH-OUTLIER.                                              *
023500*    + WILL CALCULATE THE STANDARD PAYMENT.                            *
023600*    + WILL ALSO ATTEMPT TO PAY ONLY COST OUTLIERS;                    *
023700*      DAY OUTLIERS EXPIRED 10/01/97                                   *
023800*                                                                      *
023900*   03: PAY-PERDIEM-DAYS.                                              *
024000*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
024100*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
024200*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
024300*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
024400*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
024500*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
024600*                                                                      *
024700*   06: PAY-XFER-NO-COST                                               *
024800*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
024900*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
025000*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
025100*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
025200*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
025300*                                                                      *
025400*   07: PAY-WITHOUT-COST.                                              *
025500*    + WILL CALCULATE THE STANDARD PAYMENT WITHOUT THE COST PORTION.   *
025600*                                                                      *
025700*   09: PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS                       *
025800*    + 50-50                                                           *
025900*      - NOW USES Y INDICATORS ON DRGS                                 *
026000*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
026100*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
026200*    + FULL PERDIEM                                                    *
026300*      - NOW USES Y INDICATORS ON DRGS                                 *
026400*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
026500*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
026600*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
026700*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
026800*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
026900*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
027000*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
027100*                                                                      *
027200*   11: PAY-XFER-SPEC-DRG-NO-COST - POST-ACUTE TRANSFERS               *
027300*    + 50-50                                                           *
027400*      - NOW USES Y INDICATORS ON DRGS                                 *
027500*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
027600*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
027700*    + FULL PERDIEM                                                    *
027800*      - NOW USES Y INDICATORS ON DRGS                                 *
027900*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
028000*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
028100*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
028200*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
028300*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
028400*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
028500************************************************************************
028600
028700************************************************************************
028800* NEW BILL FORMAT (MILLINNIUM COMPATIBLE)                              *
028900*                                                                      *
029000* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL001 PROGRAM  *
029100* AND AFTER FOR PROCESSING IN THE NEW FORMAT.                          *
029200*                                                                      *
029300* B-CHARGES-CLAIMED = TOTAL COVERED CHARGES ON THE 0001 (TOTALS        *
029400* LINE) MINUS BLOOD CLOT COST, KIDNEY COSTS, ACQUISITION COSTS AND     *
029500* TECHNICAL PROVIDER CHARGES.                                          *
029600************************************************************************
029700 01  BILL-DATA-2021.
029800         10  B-NPI10.
029900             15  B-NPI8             PIC X(08).
030000             15  B-NPI-FILLER       PIC X(02).
030100         10  B-PROVIDER-NO          PIC X(06).
030200             88  B-FORMER-MDH-PROVIDERS
030300                                      VALUE '080006' '140184'
030400                                            '390072' '420019'
030500                                            '440031' '450451'
030600                                            '490019' '510062'.
030700         10  B-REVIEW-CODE          PIC 9(02).
030800             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
030900             88  PAY-WITH-OUTLIER     VALUE 00 07.
031000             88  PAY-PERDIEM-DAYS     VALUE 03.
031100             88  PAY-XFER-NO-COST     VALUE 06.
031200             88  PAY-WITHOUT-COST     VALUE 07.
031300             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
031400             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
031500         10  B-DRG                  PIC 9(03).
031600
031700* ======================================================
031800* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRG'S
031900* ======================================================
032000*
032100*            88  B-DRG-POSTACUTE-PERDIEM
032200*                         VALUE  NOW USES Y INDICATORS ON DRGS
032300*                         SEE TABLE 5
032400*                         D-DRG-POSTACUTE-PERDIEM
032500
032600         10  B-LOS                  PIC 9(03).
032700         10  B-COVERED-DAYS         PIC 9(03).
032800         10  B-LTR-DAYS             PIC 9(02).
032900         10  B-DISCHARGE-DATE.
033000             15  B-DISCHG-CC        PIC 9(02).
033100             15  B-DISCHG-YY        PIC 9(02).
033200             15  B-DISCHG-MM        PIC 9(02).
033300             15  B-DISCHG-DD        PIC 9(02).
033400         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
033500         10  B-PROCEDURE-CODE-TABLE.
033600             15  B-PROCEDURE-CODE    PIC X(07) OCCURS 25 TIMES
033700                 INDEXED BY IDX-PROC.
033800         10  B-DIAGNOSIS-CODE-TABLE.
033900             15  B-DIAGNOSIS-CODE    PIC X(07) OCCURS 25 TIMES
034000                 INDEXED BY IDX-DIAG.
034100         10  B-DEMO-DATA.
034200             15  B-DEMO-CODE1           PIC X(02).
034300             15  B-DEMO-CODE2           PIC X(02).
034400             15  B-DEMO-CODE3           PIC X(02).
034500             15  B-DEMO-CODE4           PIC X(02).
034600         10  B-NDC-DATA.
034700             15  B-NDC-NUMBER        PIC X(11) OCCURS 10 TIMES
034800                 INDEXED BY IDX-NDC.
034900         10  B-CONDITION-CODE-TABLE.
035000             15  B-CONDITION-CODE    PIC X(02) OCCURS 5 TIMES
035100                 INDEXED BY IDX-COND.
035200
035300************************************************************************
035400* RETURN CODES (PPS-RTC) NOTE HOW THE BILL WAS/WAS NOT PAID.           *
035500*   00-49: HOW THE BILL WAS PAID                                       *
035600*   50-99: WHY THE BILL WAS NOT PAID                                   *
035700*  ----------------------------------------------------------          *
035800*   00,30:                                                             *
035900*    + PAID NORMAL DRG PAYMENT                                         *
036000*                                                                      *
036100*   01:                                                                *
036200*    + PAID AS A DAY-OUTLIER.                                          *
036300*      - DAY-OUTLIER NO LONGER BEING PAID AS OF 10/01/97               *
036400*                                                                      *
036500*   02:                                                                *
036600*    + PAID AS A COST-OUTLIER.                                         *
036700*                                                                      *
036800*   03,33:                                                             *
036900*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
037000*                                                                      *
037100*   05:                                                                *
037200*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
037300*    + QUALIFIED FOR A COST OUTLIER PAYMENT                            *
037400*                                                                      *
037500*   06:                                                                *
037600*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
037700*    + PROVIDER REFUSED COST OUTLIER PAYMENT                           *
037800*                                                                      *
037900*   10,40:                                                             *
038000*    + POST-ACUTE TRANSFER                                             *
038100*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
038200*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRGS         *
038300*                                                                      *
038400*   12,42:                                                             *
038500*    + POST-ACAUTE TRANSFER WITH SPECIFIC DRGS                         *
038600*      - NOW USES Y INDICATORS ON DRGS                                 *
038700*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
038800*      - D-DRG-POSTACUTE-PERDIEM                                       *
038900*                                                                      *
039000*   14,44:                                                             *
039100*    + PAID NORMAL DRG PAYMENT WITH PERDIEM DAYS = OR > GM ALOS        *
039200*                                                                      *
039300*   16:                                                                *
039400*    + PAID AS A COST-OUTLIER WITH PERDIEM DAYS = OR > GM ALOS         *
039500*                                                                      *
039600*   30,33,40,42,44:                                                    *
039700*    + OUTLIER RECONCILIATION                                          *
039800*                                                                      *
039900*   51:                                                                *
040000*    + NO PROVIDER SPECIFIC INFO FOUND                                 *
040100*                                                                      *
040200*   52:                                                                *
040300*    + INVALID CBSA# IN PROVIDER FILE OR                               *
040400*    + INVALID WAGE INDEX OR                                           *
040500*    + INVALID PROVIDER TYPES ON PROVIDER FILE                         *
040600*    + INVALID SUPPLEMENTAL WAGE INDEX FLAG OR                         *
040700*      SUPPLEMENTAL WAGE INDEX                                         *
040800*                                                                      *
040900*   53:                                                                *
041000*    + WAIVER STATE - NOT CALCULATED BY PPS OR                         *
041100*    + INVALID STATE CODE IN COMBINATION WITH HAC FLAG                 *
041200*                                                                      *
041300*   54:                                                                *
041400*    + INVALID DRG                                                     *
041500*                                                                      *
041600*   55:                                                                *
041700*    + DISCHARGE DATE < PROVIDER EFF START DATE OR                     *
041800*    + DISCHARGE DATE < CBSA EFF START DATE FOR PPS OR                 *
041900*    + PROVIDER HAS BEEN TERMINATED ON OR BEFORE DISCHARGE DATE        *
042000*                                                                      *
042100*   56:                                                                *
042200*    + INVALID LENGTH OF STAY                                          *
042300*                                                                      *
042400*   57:                                                                *
042500*    + REVIEW CODE INVALID (NOT 00 03 06 07 09 11)                     *
042600*                                                                      *
042700*   58:                                                                *
042800*    + TOTAL CHARGES NOT NUMERIC                                       *
042900*                                                                      *
043000*   61:                                                                *
043100*    + LIFETIME RESERVE DAYS NOT NUMERIC OR BILL-LTR-DAYS > 60         *
043200*                                                                      *
043300*   62:                                                                *
043400*    + INVALID NUMBER OF COVERED DAYS                                  *
043500*                                                                      *
043600*   65:                                                                *
043700*    + PAY-CODE NOT = A, B OR C ON PSF FOR CAPITAL OR                  *
043800*    + INVALID READMISSION FLAG IN PSF FILE OR                         *
043900*    + BLANK READMISSION FLAG IN PSF FILE OR                           *
044000*    + READMISSION ADJUSTMENT IS INVALID / OUT OF RANGE IN PSF FILE OR *
044100*    + BLANK READMISSION ADJUSTMENT IN PSF FILE OR                     *
044200*    + INVALID STATE CODE IN COMBO W/ READMISSION FLAG IN PSF FILE OR  *
044300*    + INVALID EHR FLAG IN PSF FILE (MUST BE A "Y" OR BLANK)           *
044400*                                                                      *
044500*   67:                                                                *
044600*    + COST OUTLIER WITH LOS > COVERED DAYS OR                         *
044700*      COST OUTLIER THRESHOLD CALUCULATION                             *
044800*                                                                      *
044900*   68:                                                                *
045000*    + INVALID VALUE BASED PURCHASE FLAG IN PSF FILE OR                *
045100*    + BLANK VALUE BASED PURCHASE FLAG IN PSF FILE OR                  *
045200*    + VALUE BASED PURCHASE ADJUSTMEMT IS INVALID OR OUT OF RANGE IN   *
045300*      PSF FILE INDICATOR OR                                           *
045400*    + BLANK VALUE BASED PURCHASE ADJUSTMEMT IN PSF FILE OR            *
045500*    + INVALID COMBINATION OF HOSPITAL QUALITY INDICATOR AND VALUE     *
045600*      BASED PURCHASE FLAG IN PSF FILE OR                              *
045700*    + INVALID STATE CODE IN COMBINATION WITH VALUE BASED PURCHASE     *
045800*      FLAG IN PSF FILE                                                *
045900*                                                                      *
046000*   98: CANNOT PROCESS BILL OLDER THAN 5 YEARS                         *
046100************************************************************************
046200 01  PPS-DATA.
046300         10  PPS-RTC                PIC 9(02).
046400         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
046500         10  PPS-OUTLIER-DAYS       PIC 9(03).
046600         10  PPS-AVG-LOS            PIC 9(02)V9(01).
046700         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
046800         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
046900         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
047000         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
047100         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
047200         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
047300         10  PPS-REG-DAYS-USED      PIC 9(03).
047400         10  PPS-LTR-DAYS-USED      PIC 9(02).
047500         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
047600         10  PPS-CALC-VERS          PIC X(05).
047700
047800*****************************************************************
047900*            THESE ARE THE VERSIONS OF THE PPCAL
048000*           PROGRAMS THAT WILL BE PASSED BACK----
048100*          ASSOCIATED WITH THE BILL BEING PROCESSED
048200*****************************************************************
048300 01  PRICER-OPT-VERS-SW.
048400     02  PRICER-OPTION-SW          PIC X(01).
048500         88  ALL-TABLES-PASSED          VALUE 'A'.
048600         88  PROV-RECORD-PASSED         VALUE 'P'.
048700         88  ADDITIONAL-VARIABLES       VALUE 'M'.
048800         88  PC-PRICER                  VALUE 'C'.
048900     02  PPS-VERSIONS.
049000         10  PPDRV-VERSION         PIC X(05).
049100
049200*****************************************************************
049300*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
049400*          ASSOCIATED WITH THE BILL BEING PROCESSED
049500*****************************************************************
049600 01  PPS-ADDITIONAL-VARIABLES.
049700     05  PPS-HSP-PCT                PIC 9(01)V9(02).
049800     05  PPS-FSP-PCT                PIC 9(01)V9(02).
049900     05  PPS-NAT-PCT                PIC 9(01)V9(02).
050000     05  PPS-REG-PCT                PIC 9(01)V9(02).
050100     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
050200     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
050300     05  PPS-DRG-WT                 PIC 9(02)V9(04).
050400     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
050500     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
050600     05  PPS-REG-LABOR              PIC 9(05)V9(02).
050700     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
050800     05  PPS-OPER-COLA              PIC 9(01)V9(03).
050900     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
051000     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
051100     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
051200     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
051300     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
051400     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
051500     05  PPS-CAPITAL-VARIABLES.
051600         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
051700         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
051800         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
051900         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
052000         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
052100         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
052200         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
052300         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
052400     05  PPS-CAPITAL2-VARIABLES.
052500         10  PPS-CAPI2-PAY-CODE             PIC X(1).
052600         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
052700         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
052800     05  PPS-OTHER-VARIABLES.
052900         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
053000         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
053100         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
053200         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
053300         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
053400         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
053500         10  PPS-HOSP-READMISSION-REDU      PIC X.
053600         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
053700         10  PPS-OPERATNG-DATA.
053800             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
053900             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
054000             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
054100     05  PPS-PC-OTH-VARIABLES.
054200         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
054300         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
054400         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
054500         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
054600         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
054700         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
054800         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
054900         10  PPS-PC-HMO-FLAG                PIC X(01).
055000         10  PPS-PC-COT-FLAG                PIC X(01).
055100         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
055200         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
055300     05  PPS-ADDITIONAL-PAY-INFO-DATA.
055400         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
055500         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
055600         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
055700         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
055800     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
055900         10  PPS-HAC-PROG-REDUC-IND      PIC X.
056000         10  PPS-EHR-PROG-REDUC-IND      PIC X.
056100         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
056200         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
056300         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
056400         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
056500     05 PPS-FILLER                       PIC X(0897).
056600
056700 01  PROV-NEW-HOLD.
056800     02  PROV-NEWREC-HOLD1.
056900         05  P-NEW-NPI10.
057000             10  P-NEW-NPI8             PIC X(08).
057100             10  P-NEW-NPI-FILLER       PIC X(02).
057200         05  P-NEW-PROVIDER-NO.
057300             88  P-NEW-DSH-ADJ-PROVIDERS
057400                             VALUE '180049' '190044' '190144'
057500                                   '190191' '330047' '340085'
057600                                   '370016' '370149' '420043'.
057700             10  P-NEW-STATE            PIC X(02).
057800                 88  P-VBP-INVALID-STATE
057900                             VALUE '21' '80' '40' '84'.
058000                 88  P-READ-INVALID-STATE
058100                             VALUE '40' '84'.
058200                 88  P-HAC-INVALID-STATE
058300                             VALUE '40' '84'.
058400                 88  P-PR-NEW-STATE
058500                             VALUE '40' '84'.
058600             10  FILLER                 PIC X(04).
058700         05  P-NEW-DATE-DATA.
058800             10  P-NEW-EFF-DATE.
058900                 15  P-NEW-EFF-DT-CC    PIC 9(02).
059000                 15  P-NEW-EFF-DT-YY    PIC 9(02).
059100                 15  P-NEW-EFF-DT-MM    PIC 9(02).
059200                 15  P-NEW-EFF-DT-DD    PIC 9(02).
059300             10  P-NEW-FY-BEGIN-DATE.
059400                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
059500                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
059600                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
059700                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
059800             10  P-NEW-REPORT-DATE.
059900                 15  P-NEW-REPORT-DT-CC PIC 9(02).
060000                 15  P-NEW-REPORT-DT-YY PIC 9(02).
060100                 15  P-NEW-REPORT-DT-MM PIC 9(02).
060200                 15  P-NEW-REPORT-DT-DD PIC 9(02).
060300             10  P-NEW-TERMINATION-DATE.
060400                 15  P-NEW-TERM-DT-CC   PIC 9(02).
060500                 15  P-NEW-TERM-DT-YY   PIC 9(02).
060600                 15  P-NEW-TERM-DT-MM   PIC 9(02).
060700                 15  P-NEW-TERM-DT-DD   PIC 9(02).
060800         05  P-NEW-WAIVER-CODE          PIC X(01).
060900             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
061000         05  P-NEW-INTER-NO             PIC 9(05).
061100         05  P-NEW-PROVIDER-TYPE        PIC X(02).
061200             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
061300             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
061400                                                  '15' '17'
061500                                                  '22'.
061600             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
061700             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
061800             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
061900             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
062000             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
062100             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
062200             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
062300             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
062400             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
062500             88  P-N-EACH                   VALUE '21' '22'.
062600             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
062700             88  P-N-NHCMQ-II-SNF           VALUE '32'.
062800             88  P-N-NHCMQ-III-SNF          VALUE '33'.
062900             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
063000         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
063100             88  P-N-NEW-ENGLAND            VALUE  1.
063200             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
063300             88  P-N-SOUTH-ATLANTIC         VALUE  3.
063400             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
063500             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
063600             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
063700             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
063800             88  P-N-MOUNTAIN               VALUE  8.
063900             88  P-N-PACIFIC                VALUE  9.
064000         05  P-NEW-CURRENT-DIV   REDEFINES
064100                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
064200             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
064300         05  P-NEW-MSA-DATA.
064400             10  P-NEW-CHG-CODE-INDEX       PIC X.
064500             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
064600             10  P-NEW-GEO-LOC-MSA9   REDEFINES
064700                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
064800             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
064900             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
065000             10  P-NEW-STAND-AMT-LOC-MSA9
065100       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
065200                 15  P-NEW-RURAL-1ST.
065300                     20  P-NEW-STAND-RURAL  PIC XX.
065400                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
065500                 15  P-NEW-RURAL-2ND        PIC XX.
065600         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
065700                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
065800                 88  P-NEW-SCH-YR82       VALUE   '82'.
065900                 88  P-NEW-SCH-YR87       VALUE   '87'.
066000         05  P-NEW-LUGAR                    PIC X.
066100         05  P-NEW-TEMP-RELIEF-IND          PIC X.
066200         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
066300         05  P-NEW-STATE-CODE               PIC 9(02).
066400         05  P-NEW-STATE-CODE-X REDEFINES
066500             P-NEW-STATE-CODE               PIC X(02).
066600         05  FILLER                         PIC X(03).
066700     02  PROV-NEWREC-HOLD2.
066800         05  P-NEW-VARIABLES.
066900             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
067000             10  P-NEW-COLA              PIC  9(01)V9(03).
067100             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
067200             10  P-NEW-BED-SIZE          PIC  9(05).
067300             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
067400             10  P-NEW-CMI               PIC  9(01)V9(04).
067500             10  P-NEW-SSI-RATIO         PIC  V9(04).
067600             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
067700             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
067800             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
067900             10  P-NEW-DSH-PERCENT       PIC  V9(04).
068000             10  P-NEW-FYE-DATE          PIC  X(08).
068100         05  P-NEW-CBSA-DATA.
068200             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
068300             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
068400             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
068500             10  P-NEW-CBSA-GEO-RURAL REDEFINES
068600                 P-NEW-CBSA-GEO-LOC.
068700                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
068800                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
068900                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
069000
069100             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
069200             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
069300             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
069400     02  PROV-NEWREC-HOLD3.
069500         05  P-NEW-PASS-AMT-DATA.
069600             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
069700             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
069800             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
069900             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
070000         05  P-NEW-CAPI-DATA.
070100             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
070200             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
070300             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
070400             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
070500             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
070600             15  P-NEW-CAPI-NEW-HOSP       PIC X.
070700             15  P-NEW-CAPI-IME            PIC 9V9999.
070800             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
070900         05  P-HVBP-HRR-DATA.
071000             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
071100             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
071200             15  P-HOSP-READMISSION-REDU    PIC X.
071300             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
071400         05  P-MODEL1-BUNDLE-DATA.
071500             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
071600             15  P-HAC-REDUC-IND            PIC X.
071700             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
071800             15  P-EHR-REDUC-IND            PIC X.
071900             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
072000         05  P-NEW-COUNTY-CODE              PIC 9(05).
072100         05  P-NEW-COUNTY-CODE-X REDEFINES
072200             P-NEW-COUNTY-CODE              PIC X(05).
072300         05  P-NEW-SUPPLEMENTAL-WI.
072400             10  P-NEW-SUPP-WI-IND          PIC X.
072500                 88  P-NEW-IND-PRIOR-YEAR   VALUE '1'.
072600             10  P-NEW-SUPP-WI              PIC 9(02)V9(04).
072700         05  P-PASS-THRU-ALLO-STEM-CELL     PIC 9(07)V9(02).
072800         05  FILLER                         PIC X(31).
072900
073000*****************************************************************
073100 01  WAGE-NEW-CBSA-INDEX-RECORD.
073200     05  W-CBSA                        PIC X(5).
073300     05  W-CBSA-SIZE                   PIC X.
073400         88  LARGE-URBAN       VALUE 'L'.
073500         88  OTHER-URBAN       VALUE 'O'.
073600         88  ALL-RURAL         VALUE 'R'.
073700     05  W-CBSA-EFF-DATE               PIC X(8).
073800     05  FILLER                        PIC X.
073900     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
074000     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
074100
074200*******************************************************
074300*    HOLD VARIABLES POPULATED IN PPCAL___***          *
074400*******************************************************
074500 COPY PPHOLDAR.
074600
074700******************************************************************
074800 PROCEDURE DIVISION  USING BILL-DATA-2021
074900                           PPS-DATA
075000                           PRICER-OPT-VERS-SW
075100                           PPS-ADDITIONAL-VARIABLES
075200                           PROV-NEW-HOLD
075300                           WAGE-NEW-CBSA-INDEX-RECORD
075400                           PPHOLDAR-HOLD-AREA.
075500
075600***************************************************************
075700*    PROCESSING:                                              *
075800*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE        *
075900*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
076000*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
076100*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
076200*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
076300*           GOBACK.                                           *
076400*        D. ASSEMBLE PRICING COMPONENTS.                      *
076500*        E. CALCULATE THE PRICE.                              *
076600***************************************************************
076700     INITIALIZE WK-HLDDRG-DATA
076800                WK-HLDDRG-DATA2
076900                WK-HLD-MID-DATA
077000                WK-NEW-TECH-VARIABLES
077100                WK-COVID19-VARIABLES
077200                WK-CLIN-VARIABLES
077300                WK-CART-VARIABLES.
077400
077500     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
077600     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
077700     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
077800     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
077900     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
078000     MOVE 'N' TO TEMP-RELIEF-FLAG.
078100     MOVE 'N' TO OUTLIER-RECON-FLAG.
078200     MOVE ZEROES TO WK-HAC-AMOUNT.
078300     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
078400     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
078500     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
078600     MOVE ZEROES TO PPS-ISLET-ISOL-PAY-ADD-ON.
078700
078800     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
078900
079000     COMPUTE H-DRG-WT ROUNDED = H-DRG-WT * COVID-ADJ *
079100                                NO-COST-PRODUCT.
079200
079300     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DRG-WT-FRCTN * COVID-ADJ *
079400                                      NO-COST-PRODUCT.
079500
079600     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
079700     MOVE H-OPER-CHARGE-THRESHOLD   TO  PPS-DOLLAR-THRESHOLD.
079800     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
079900     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
080000     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
080100     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
080200     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
080300     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
080400     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
080500     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
080600                            PPS-ADDITIONAL-PAY-INFO-DATA.
080700     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
080800                            PPS-ADDITIONAL-PAY-INFO-DATA2.
080900
081000     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
081100     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
081200     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
081300     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
081400                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
081500     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
081600     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
081700                               PPS-MODEL1-BUNDLE-DISPRCNT.
081800
081900     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
082000     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
082100     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
082200*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
082300     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
082400     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
082500     MOVE 0     TO    PPS-FLX7-PAYMENT.
082600
082700     IF (PPS-RTC = '00' OR '03' OR '10' OR
082800                   '12' OR '14')
082900        MOVE 'Y' TO OUTLIER-RECON-FLAG
083000        MOVE PPS-DATA TO HLD-PPS-DATA
083100        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
083200        MOVE HLD-PPS-DATA TO PPS-DATA.
083300
083400     IF  PPS-RTC < 50
083500         IF  P-NEW-WAIVER-STATE
083600             MOVE 53 TO PPS-RTC
083700             MOVE ALL '0' TO PPS-OPER-HSP-PART
083800                             PPS-OPER-FSP-PART
083900                             PPS-OPER-OUTLIER-PART
084000                             PPS-OUTLIER-DAYS
084100                             PPS-REG-DAYS-USED
084200                             PPS-LTR-DAYS-USED
084300                             PPS-TOTAL-PAYMENT
084400                             WK-HAC-TOTAL-PAYMENT
084500                             PPS-OPER-DSH-ADJ
084600                             PPS-OPER-IME-ADJ
084700                             H-DSCHG-FRCTN
084800                             H-DRG-WT-FRCTN
084900                             HOLD-ADDITIONAL-VARIABLES
085000                             HOLD-CAPITAL-VARIABLES
085100                             HOLD-CAPITAL2-VARIABLES
085200                             HOLD-OTHER-VARIABLES
085300                             HOLD-PC-OTH-VARIABLES
085400                             H-ADDITIONAL-PAY-INFO-DATA
085500                             H-ADDITIONAL-PAY-INFO-DATA2.
085600     GOBACK.
085700
085800 0200-MAINLINE-CONTROL.
085900
086000     MOVE 'N' TO HMO-TAG.
086100
086200     IF PPS-PC-HMO-FLAG = 'Y' OR
086300               HMO-FLAG = 'Y'
086400        MOVE 'Y' TO HMO-TAG.
086500
086600     MOVE ALL '0' TO PPS-DATA
086700                     H-OPER-DSH-SCH
086800                     H-OPER-DSH-RRC
086900                     HOLD-PPS-COMPONENTS
087000                     HOLD-PPS-COMPONENTS
087100                     HOLD-ADDITIONAL-VARIABLES
087200                     HOLD-CAPITAL-VARIABLES
087300                     HOLD-CAPITAL2-VARIABLES
087400                     HOLD-OTHER-VARIABLES
087500                     HOLD-PC-OTH-VARIABLES
087600                     H-ADDITIONAL-PAY-INFO-DATA
087700                     H-ADDITIONAL-PAY-INFO-DATA2
087800                     H-EHR-SUBSAV-QUANT
087900                     H-EHR-SUBSAV-LV
088000                     H-EHR-SUBSAV-QUANT-INCLV
088100                     H-EHR-RESTORE-FULL-QUANT
088200                     H-OPER-BILL-STDZ-COSTS
088300                     H-CAPI-BILL-STDZ-COSTS
088400                     H-OPER-STDZ-COST-OUTLIER
088500                     H-CAPI-STDZ-COST-OUTLIER
088600                     H-OPER-STDZ-DOLLAR-THRESHOLD
088700                     H-CAPI-STDZ-DOLLAR-THRESHOLD
088800                     WK-LOW-VOL-ADDON
088900                     WK-HAC-AMOUNT
089000                     WK-HAC-TOTAL-PAYMENT.
089100
089200     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
089300        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
089400
089500     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
089600        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
089700
089800     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
089900        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
090000
090100     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
090200        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
090300
090400     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
090500        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
090600
090700     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
090800        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
090900
091000     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
091100        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
091200
091300     PERFORM 1000-EDIT-THE-BILL-INFO.
091400
091500     IF  PPS-RTC = 00
091600         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
091700
091800     IF  PPS-RTC = 00
091900         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
092000
092100     IF OUTLIER-RECON-FLAG = 'Y'
092200        MOVE 'N' TO OUTLIER-RECON-FLAG
092300        GO TO 0200-EXIT.
092400
092500     IF PPS-RTC = 00
092600        IF H-PERDIEM-DAYS = H-ALOS OR
092700           H-PERDIEM-DAYS > H-ALOS
092800           MOVE 14 TO PPS-RTC.
092900
093000     IF PPS-RTC = 02
093100        IF H-PERDIEM-DAYS = H-ALOS OR
093200           H-PERDIEM-DAYS > H-ALOS
093300           MOVE 16 TO PPS-RTC.
093400
093500 0200-EXIT.   EXIT.
093600
093700 1000-EDIT-THE-BILL-INFO.
093800
093900     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
094000     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
094100
094200**   IF  PPS-RTC = 00
094300*        IF  P-NEW-WAIVER-STATE
094400*            MOVE 53 TO PPS-RTC.
094500
094600     IF  PPS-RTC = 00
094700         IF   HLDDRG-VALID = 'I'
094800             MOVE 54 TO PPS-RTC.
094900
095000     IF  PPS-RTC = 00
095100            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
095200                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
095300                MOVE 55 TO PPS-RTC.
095400
095500     IF  PPS-RTC = 00
095600         IF P-NEW-TERMINATION-DATE > 00000000
095700            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
095800                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
095900                  MOVE 55 TO PPS-RTC.
096000
096100     IF  PPS-RTC = 00
096200         IF  B-LOS NOT NUMERIC
096300             MOVE 56 TO PPS-RTC
096400         ELSE
096500         IF  B-LOS = 0
096600             IF B-REVIEW-CODE NOT = 00 AND
096700                              NOT = 03 AND
096800                              NOT = 06 AND
096900                              NOT = 07 AND
097000                              NOT = 09 AND
097100                              NOT = 11
097200             MOVE 56 TO PPS-RTC.
097300
097400     IF  PPS-RTC = 00
097500         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
097600             MOVE 61 TO PPS-RTC
097700         ELSE
097800             MOVE B-LTR-DAYS TO H-LTR-DAYS.
097900
098000     IF  PPS-RTC = 00
098100         IF  B-COVERED-DAYS NOT NUMERIC
098200             MOVE 62 TO PPS-RTC
098300         ELSE
098400         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
098500             MOVE 62 TO PPS-RTC
098600         ELSE
098700             MOVE B-COVERED-DAYS TO H-COV-DAYS.
098800
098900     IF  PPS-RTC = 00
099000         IF  H-LTR-DAYS  > H-COV-DAYS
099100             MOVE 62 TO PPS-RTC
099200         ELSE
099300             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
099400
099500     IF  PPS-RTC = 00
099600         IF  NOT VALID-REVIEW-CODE
099700             MOVE 57 TO PPS-RTC.
099800
099900     IF  PPS-RTC = 00
100000         IF  B-CHARGES-CLAIMED NOT NUMERIC
100100             MOVE 58 TO PPS-RTC.
100200
100300     IF PPS-RTC = 00
100400           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
100500                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
100600                                            NOT = 'C'
100700                 MOVE 65 TO PPS-RTC.
100800
100900***  MDH PROVISION ENDS 9/30/2018
101000***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
101100
101200     IF PPS-RTC = 00 AND
101300        B-DISCHARGE-DATE > 20220930 AND
101400        P-N-INVALID-PROV-TYPES
101500                 MOVE 52 TO PPS-RTC.
101600
101700 2000-ASSEMBLE-PPS-VARIABLES.
101800***  GET THE PROVIDER SPECIFIC VARIABLES.
101900
102000     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
102100     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
102200
102300     IF (P-NEW-STATE = 02 OR 12)
102400        MOVE P-NEW-COLA TO H-OPER-COLA
102500     ELSE
102600        MOVE 1.000 TO H-OPER-COLA.
102700
102800***************************************************************
102900***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
103000
103100     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
103200
103300     PERFORM 2700-COVID-DRG-ADJ THRU 2700-EXIT.
103400
103500     PERFORM 2800-CART-CLIN-TRIAL-REDUC THRU 2800-EXIT.
103600
103700     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
103800
103900     MOVE P-NEW-STATE            TO MES-PPS-STATE.
104000
104100*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
104200** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
104300** FOR FY 2011
104400***************************************************************
104500*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
104600*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
104700***************************************************************
104800***  GET THE WAGE-INDEX
104900
105000     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
105100     MOVE P-NEW-STATE            TO MES-PPS-STATE.
105200
105300***************************************************************
105400* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
105500* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
105600***************************************************************
105700
105800     PERFORM 2050-RATES-TB THRU 2050-EXIT.
105900
106000     IF P-NEW-GEO-LOC-MSA9 >= 9400 AND
106100        P-NEW-GEO-LOC-MSA9 <= 9900
106200        PERFORM 2100-MIDNIGHT-FACTORS THRU 2100-EXIT
106300     ELSE
106400        MOVE 1 TO HLD-MID-ADJ-FACT
106500        GO TO 2000-EXIT.
106600
106700 2000-EXIT.  EXIT.
106800
106900 2050-RATES-TB.
107000     MOVE 1 TO R2
107100     MOVE 1 TO R4.
107200
107300     IF LARGE-URBAN
107400         MOVE 1 TO R3
107500     ELSE
107600         MOVE 2 TO R3.
107700
107800     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
107900        (P-EHR-REDUC-IND = ' ')           AND
108000        (H-WAGE-INDEX > 01.0000))
108100        PERFORM 2300-GET-LAB-NONLAB-TB1-RATES
108200           THRU 2300-GET-LAB-NONLAB-TB1-EXIT
108300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
108400
108500     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
108600        (P-EHR-REDUC-IND = ' ')               AND
108700         (H-WAGE-INDEX > 01.0000))
108800        PERFORM 2300-GET-LAB-NONLAB-TB2-RATES
108900           THRU 2300-GET-LAB-NONLAB-TB2-EXIT
109000             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
109100
109200     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
109300        (P-EHR-REDUC-IND = ' ')            AND
109400         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
109500        PERFORM 2300-GET-LAB-NONLAB-TB3-RATES
109600           THRU 2300-GET-LAB-NONLAB-TB3-EXIT
109700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
109800
109900     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
110000        (P-EHR-REDUC-IND = ' ')               AND
110100         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
110200        PERFORM 2300-GET-LAB-NONLAB-TB4-RATES
110300           THRU 2300-GET-LAB-NONLAB-TB4-EXIT
110400             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
110500
110600     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
110700        (P-EHR-REDUC-IND = 'Y')           AND
110800        (H-WAGE-INDEX > 01.0000))
110900        PERFORM 2300-GET-LAB-NONLAB-TB5-RATES
111000           THRU 2300-GET-LAB-NONLAB-TB5-EXIT
111100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
111200
111300     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
111400        (P-EHR-REDUC-IND = 'Y')               AND
111500         (H-WAGE-INDEX > 01.0000))
111600        PERFORM 2300-GET-LAB-NONLAB-TB6-RATES
111700           THRU 2300-GET-LAB-NONLAB-TB6-EXIT
111800             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
111900
112000     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
112100        (P-EHR-REDUC-IND = 'Y')            AND
112200         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
112300        PERFORM 2300-GET-LAB-NONLAB-TB7-RATES
112400           THRU 2300-GET-LAB-NONLAB-TB7-EXIT
112500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
112600
112700     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
112800        (P-EHR-REDUC-IND = 'Y')               AND
112900         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
113000        PERFORM 2300-GET-LAB-NONLAB-TB8-RATES
113100           THRU 2300-GET-LAB-NONLAB-TB8-EXIT
113200             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
113300
113400***************************************************************
113500* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
113600***************************************************************
113700
113800     MOVE 0.00  TO H-OPER-HSP-PCT.
113900     MOVE 1.00  TO H-OPER-FSP-PCT.
114000
114100***************************************************************
114200*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
114300***************************************************************
114400
114500      MOVE 1.00 TO H-NAT-PCT.
114600      MOVE 0.00 TO H-REG-PCT.
114700
114800     IF  P-N-SCH-REBASED-FY90 OR
114900         P-N-EACH OR
115000         P-N-MDH-REBASED-FY90
115100         MOVE 1.00 TO H-OPER-HSP-PCT.
115200
115300 2050-EXIT.   EXIT.
115400
115500***************************************************************
115600*  APPLY THE TWO MIDNIGHT POLICY ADJUSTMENT FACTORS           *
115700***************************************************************
115800 2100-MIDNIGHT-FACTORS.
115900
116000     INITIALIZE HLD-MID-ADJ-FACT.
116100
116200     SET MID-IDX TO 1.
116300
116400     SEARCH MID-TAB VARYING MID-IDX
116500     WHEN WK-MID-MSAX(MID-IDX) = P-NEW-GEO-LOC-MSA9
116600       MOVE MID-DATA-TAB(MID-IDX) TO HLD-MID-DATA.
116700
116800 2100-EXIT.   EXIT.
116900
117000***************************************************************
117100* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
117200* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
117300***************************************************************
117400 2300-GET-LAB-NONLAB-TB1-RATES.
117500
117600     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
117700         MOVE TB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
117800         MOVE TB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
117900         MOVE TB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
118000         MOVE TB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
118100
118200 2300-GET-LAB-NONLAB-TB1-EXIT.   EXIT.
118300
118400 2300-GET-LAB-NONLAB-TB2-RATES.
118500
118600     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
118700         MOVE TB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
118800         MOVE TB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
118900         MOVE TB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
119000         MOVE TB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
119100
119200 2300-GET-LAB-NONLAB-TB2-EXIT.   EXIT.
119300
119400 2300-GET-LAB-NONLAB-TB3-RATES.
119500
119600     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
119700         MOVE TB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
119800         MOVE TB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
119900         MOVE TB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
120000         MOVE TB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
120100
120200 2300-GET-LAB-NONLAB-TB3-EXIT.   EXIT.
120300
120400 2300-GET-LAB-NONLAB-TB4-RATES.
120500
120600     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
120700         MOVE TB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
120800         MOVE TB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
120900         MOVE TB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
121000         MOVE TB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
121100
121200 2300-GET-LAB-NONLAB-TB4-EXIT.   EXIT.
121300
121400 2300-GET-LAB-NONLAB-TB5-RATES.
121500
121600     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
121700         MOVE TB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
121800         MOVE TB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
121900         MOVE TB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
122000         MOVE TB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
122100
122200 2300-GET-LAB-NONLAB-TB5-EXIT.   EXIT.
122300
122400 2300-GET-LAB-NONLAB-TB6-RATES.
122500
122600     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
122700         MOVE TB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
122800         MOVE TB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
122900         MOVE TB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
123000         MOVE TB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
123100
123200 2300-GET-LAB-NONLAB-TB6-EXIT.   EXIT.
123300
123400 2300-GET-LAB-NONLAB-TB7-RATES.
123500
123600     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
123700         MOVE TB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
123800         MOVE TB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
123900         MOVE TB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
124000         MOVE TB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
124100
124200 2300-GET-LAB-NONLAB-TB7-EXIT.   EXIT.
124300
124400 2300-GET-LAB-NONLAB-TB8-RATES.
124500
124600     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
124700         MOVE TB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
124800         MOVE TB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
124900         MOVE TB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
125000         MOVE TB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
125100
125200 2300-GET-LAB-NONLAB-TB8-EXIT.   EXIT.
125300
125400***************************************************************
125500* OBTAIN THE APPLICABLE DRG WEIGHTS                           *
125600***************************************************************
125700 2600-GET-DRG-WEIGHT.
125800
125900     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
126000     SET DRG-IDX TO 1
126100     SEARCH DRG-TAB VARYING DRG-IDX
126200         AT END
126300           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
126400           MOVE 'I' TO  HLDDRG-VALID
126500           MOVE 0 TO HLDDRG-WEIGHT
126600           MOVE 54 TO PPS-RTC
126700           GO TO 2600-EXIT
126800       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
126900         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
127000
127100     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
127200     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
127300     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
127400                                  H-DRG-WT.
127500     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
127600                                  H-ALOS.
127700     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
127800     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
127900                                  H-ARITH-ALOS.
128000     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
128100     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
128200     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
128300     MOVE  'V'                 TO HLDDRG-VALID.
128400     MOVE ZEROES               TO H-DAYS-CUTOFF.
128500
128600 2600-EXIT.   EXIT.
128700
128800***************************************************************
128900 2700-COVID-DRG-ADJ.
129000***************************************************************
129100* ADJUSTMENT TO DRG WEIGHT PER COVID-19 DIAGNOSIS
129200*   + 20% INCREASE TO OPERATING DRG PAYMENTS
129300*----------------------------------------------------------------*
129400
129500     MOVE 1 TO IDX-COVID-DIAG.
129600     MOVE 1 TO IDX-COVID-COND.
129700     MOVE 1.0 TO COVID-ADJ.
129800
129900     PERFORM 10000-COVID19-DIAG-FLAG THRU 10000-EXIT
130000     VARYING IDX-COVID-DIAG FROM 1 BY 1 UNTIL IDX-COVID-DIAG > 25.
130100
130200     PERFORM 10100-COVID19-COND-FLAG THRU 10100-EXIT
130300     VARYING IDX-COVID-COND FROM 1 BY 1 UNTIL IDX-COVID-COND > 5.
130400
130500     IF B-DISCHARGE-DATE > 20200331
130600        IF DIAG-COVID2-FLAG = 'Y'
130700           IF COND-COVID1-FLAG = 'Y'
130800              GO TO 2700-EXIT
130900           ELSE
131000              MOVE 1.2 TO COVID-ADJ.
131100
131200 2700-EXIT.   EXIT.
131300
131400***************************************************************
131500 2800-CART-CLIN-TRIAL-REDUC.
131600***************************************************************
131700* CAR-T AND CLINICAL TRIAL CASE REDUCTION FACTOR TO DRG RATE
131800*   + NO COST PRODUCT/PAYMENT ADJUSTMENT FACTOR OF 0.17 FOR FY2021
131900*   + MS-DRG 018, DIAGNOSIS CODE Z00.6 IN 2-25, AND CONDITION CODE
132000*     OF "ZB" NOT "ZC"
132100*------------------------------------------------------------------*
132200
132300     MOVE 1 TO IDX-CLIN.
132400     MOVE 1 TO IDX-CART.
132500     MOVE 1.0 TO NO-COST-PRODUCT.
132600
132700     PERFORM 10200-CLIN-FLAG THRU 10200-EXIT
132800      VARYING IDX-CLIN FROM 1 BY 1 UNTIL IDX-CLIN > 25.
132900
133000     PERFORM 10300-CART-FLAG THRU 10300-EXIT
133100      VARYING IDX-CART FROM 1 BY 1 UNTIL IDX-CART > 5.
133200
133300     IF B-DRG = 018
133400        IF (DIAG-CLIN-FLAG = 'Y' AND
133500            COND-CART-NONCP-FLAG NOT = 'Y') OR
133600            COND-CART-NCP-FLAG = 'Y'
133700        MOVE 0.17 TO NO-COST-PRODUCT.
133800
133900 2800-EXIT.   EXIT.
134000
134100***************************************************************
134200 3000-CALC-PAYMENT.
134300***************************************************************
134400
134500     PERFORM 3100-CALC-STAY-UTILIZATION.
134600     PERFORM 3300-CALC-OPER-FSP-AMT.
134700     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
134800
134900***********************************************************
135000***  OPERATING IME CALCULATION
135100
135200     COMPUTE H-OPER-IME-TEACH ROUNDED =
135300            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
135400
135500***********************************************************
135600
135700     MOVE 00                 TO  PPS-RTC.
135800     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
135900     MOVE H-ALOS             TO  PPS-AVG-LOS.
136000     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
136100
136200     MOVE B-LOS TO H-PERDIEM-DAYS.
136300     IF H-PERDIEM-DAYS < 1
136400         MOVE 1 TO H-PERDIEM-DAYS.
136500     ADD 1 TO H-PERDIEM-DAYS.
136600
136700     MOVE 1 TO H-DSCHG-FRCTN.
136800
136900     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
137000
137100     IF (PAY-PERDIEM-DAYS  OR
137200         PAY-XFER-NO-COST) OR
137300        (PAY-XFER-SPEC-DRG AND
137400         D-DRG-POSTACUTE-PERDIEM)
137500       IF H-ALOS > 0
137600         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
137700         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
137800         IF H-DSCHG-FRCTN > 1
137900              MOVE 1 TO H-DSCHG-FRCTN
138000              MOVE 1 TO H-TRANSFER-ADJ
138100         ELSE
138200              COMPUTE H-DRG-WT-FRCTN ROUNDED =
138300                  H-TRANSFER-ADJ * H-DRG-WT
138400         END-IF
138500        END-IF
138600     END-IF.
138700
138800
138900     IF (PAY-XFER-SPEC-DRG AND
139000         D-DRG-POSTACUTE-50-50) AND
139100         H-ALOS > 0
139200         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
139300         COMPUTE H-DSCHG-FRCTN  ROUNDED =
139400                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
139500         IF H-DSCHG-FRCTN > 1
139600              MOVE 1 TO H-DSCHG-FRCTN
139700              MOVE 1 TO H-TRANSFER-ADJ
139800         ELSE
139900              COMPUTE H-DRG-WT-FRCTN ROUNDED =
140000            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
140100
140200
140300***********************************************************
140400***  CAPITAL DSH CALCULATION
140500
140600     MOVE 0 TO H-CAPI-DSH.
140700
140800     IF P-NEW-BED-SIZE NOT NUMERIC
140900         MOVE 0 TO P-NEW-BED-SIZE.
141000
141100     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
141200         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
141300                  (.2025 * (P-NEW-SSI-RATIO
141400                          + P-NEW-MEDICAID-RATIO)) - 1.
141500
141600***********************************************************
141700***  CAPITAL IME TEACH CALCULATION
141800
141900     MOVE 0 TO H-WK-CAPI-IME-TEACH.
142000
142100     IF P-NEW-CAPI-IME NUMERIC
142200        IF P-NEW-CAPI-IME > 1.5000
142300           MOVE 1.5000 TO P-NEW-CAPI-IME.
142400
142500*****YEARCHANGE 2009.5 ****************************************
142600***
142700***  PER POLICY, WE REMOVED THE .5 MULTIPLER
142800***
142900***********************************************************
143000     IF P-NEW-CAPI-IME NUMERIC
143100        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
143200         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
143300
143400*****YEARCHANGE 2009.5 ****************************************
143500***********************************************************
143600     MOVE 0.00 TO H-DAYOUT-PCT.
143700     MOVE 0.80 TO H-CSTOUT-PCT.
143800
143900*****************************************************************
144000**
144100** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
144200**
144300*****************************************************************
144400
144500     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
144600             MOVE 0.90 TO H-CSTOUT-PCT.
144700
144800*****YEARCHANGE 2018.0 *******************************************
144900* NATIONAL PERCENTAGE                                            *
145000******************************************************************
145100
145200       MOVE 0.6830 TO H-LABOR-PCT.
145300       MOVE 0.3170 TO H-NONLABOR-PCT.
145400
145500     IF (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000)
145600       MOVE 0.6200 TO H-LABOR-PCT
145700       MOVE 0.3800 TO H-NONLABOR-PCT.
145800
145900     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
146000             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
146100     ELSE
146200             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
146300
146400     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
146500             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
146600     ELSE
146700             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
146800
146900***********************************************************
147000*****YEARCHANGE 2010.0 ************************************
147100***  CAPITAL PAYMENT METHOD B - YEARCHNG
147200***  CAPITAL PAYMENT METHOD B
147300
147400     IF W-CBSA-SIZE = 'L'
147500        MOVE 1.00 TO H-CAPI-LARG-URBAN
147600     ELSE
147700        MOVE 1.00 TO H-CAPI-LARG-URBAN.
147800
147900     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
148000
148100*****YEARCHANGE 2018.0 ************************************
148200
148300     COMPUTE H-FEDERAL-RATE ROUNDED =
148400                              (0466.21 * H-CAPI-GAF).
148500
148600*****YEARCHANGE 2015.1 ************************************
148700
148800     COMPUTE H-CAPI-COLA ROUNDED =
148900                     (.3152 * (H-OPER-COLA - 1) + 1).
149000
149100     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
149200
149300***********************************************************
149400* CAPITAL FSP CALCULATION                                 *
149500***********************************************************
149600
149700     COMPUTE H-CAPI-FSP-PART ROUNDED =
149800                               H-DRG-WT       *
149900                               H-CAPI-FED-RATE *
150000                               H-CAPI-COLA *
150100                               H-CAPI-LARG-URBAN *
150200                               HLD-MID-ADJ-FACT *
150300                               NO-COST-PRODUCT.
150400
150500***********************************************************
150600***  CAPITAL PAYMENT METHOD A
150700***  CAPITAL PAYMENT METHOD A
150800
150900     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
151000        MOVE 1.00 TO H-CAPI-SCH
151100     ELSE
151200        MOVE 0.85 TO H-CAPI-SCH.
151300
151400***********************************************************
151500***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
151600***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
151700
151800     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
151900                    (P-NEW-CAPI-OLD-HARM-RATE *
152000                    H-CAPI-SCH).
152100
152200***********************************************************
152300        IF PAY-PERDIEM-DAYS
152400            IF  H-PERDIEM-DAYS < H-ALOS
152500                IF  NOT (B-DRG = 789)
152600                    PERFORM 3500-CALC-PERDIEM-AMT
152700                    MOVE 03 TO PPS-RTC.
152800
152900        IF PAY-XFER-SPEC-DRG
153000            IF  H-PERDIEM-DAYS < H-ALOS
153100                IF  NOT (B-DRG = 789)
153200                    PERFORM 3550-CALC-PERDIEM-AMT.
153300
153400        IF  PAY-XFER-NO-COST
153500            MOVE 00 TO PPS-RTC
153600            IF H-PERDIEM-DAYS < H-ALOS
153700               IF  NOT (B-DRG = 789)
153800                   PERFORM 3500-CALC-PERDIEM-AMT
153900                   MOVE 06 TO PPS-RTC.
154000
154100     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
154200
154300     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
154400
154500     PERFORM 3650-NEW-COVID19-ADD-ON-PAY THRU 3650-EXIT.
154600
154700     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
154800
154900     IF PPS-RTC = 65 OR 67 OR 68
155000               GO TO 3000-CONTINUE.
155100
155200     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
155300
155400     IF PPS-RTC = 65 OR 67 OR 68
155500               GO TO 3000-CONTINUE.
155600
155700     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
155800
155900     IF PPS-RTC = 65 OR 67 OR 68
156000               GO TO 3000-CONTINUE.
156100
156200     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
156300
156400     IF PPS-RTC = 65 OR 67 OR 68
156500               GO TO 3000-CONTINUE.
156600
156700        IF PAY-XFER-SPEC-DRG
156800            IF  H-PERDIEM-DAYS < H-ALOS
156900                IF  NOT (B-DRG = 789)
157000                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
157100
157200        IF  PAY-PERDIEM-DAYS
157300            IF  H-OPER-OUTCST-PART > 0
157400                MOVE H-OPER-OUTCST-PART TO
157500                     H-OPER-OUTLIER-PART
157600                MOVE 05 TO PPS-RTC
157700            ELSE
157800            IF  PPS-RTC NOT = 03
157900                MOVE 00 TO PPS-RTC
158000                MOVE 0  TO H-OPER-OUTLIER-PART.
158100
158200        IF  PAY-PERDIEM-DAYS
158300            IF  H-CAPI-OUTCST-PART > 0
158400                MOVE H-CAPI-OUTCST-PART TO
158500                     H-CAPI-OUTLIER-PART
158600                MOVE 05 TO PPS-RTC
158700            ELSE
158800            IF  PPS-RTC NOT = 03
158900                MOVE 0  TO H-CAPI-OUTLIER-PART.
159000
159100     IF P-N-SCH-REBASED-FY90 OR
159200        P-N-EACH OR
159300        P-N-MDH-REBASED-FY90
159400         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
159500
159600 3000-CONTINUE.
159700
159800***********************************************************
159900***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
160000***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
160100
160200     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
160300
160400***********************************************************
160500
160600     IF  PPS-RTC = 67
160700         MOVE H-OPER-DOLLAR-THRESHOLD TO
160800              WK-H-OPER-DOLLAR-THRESHOLD.
160900
161000     IF  PPS-RTC < 50
161100         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
161200
161300     IF  PPS-RTC < 50
161400         NEXT SENTENCE
161500     ELSE
161600         MOVE ALL '0' TO PPS-OPER-HSP-PART
161700                         PPS-OPER-FSP-PART
161800                         PPS-OPER-OUTLIER-PART
161900                         PPS-OUTLIER-DAYS
162000                         PPS-REG-DAYS-USED
162100                         PPS-LTR-DAYS-USED
162200                         PPS-TOTAL-PAYMENT
162300                         WK-HAC-TOTAL-PAYMENT
162400                         PPS-OPER-DSH-ADJ
162500                         PPS-OPER-IME-ADJ
162600                         H-DSCHG-FRCTN
162700                         H-DRG-WT-FRCTN
162800                         HOLD-ADDITIONAL-VARIABLES
162900                         HOLD-CAPITAL-VARIABLES
163000                         HOLD-CAPITAL2-VARIABLES
163100                         HOLD-OTHER-VARIABLES
163200                         HOLD-PC-OTH-VARIABLES
163300                        H-ADDITIONAL-PAY-INFO-DATA
163400                        H-ADDITIONAL-PAY-INFO-DATA2.
163500
163600     IF  PPS-RTC = 67
163700         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
163800                 H-OPER-DOLLAR-THRESHOLD.
163900
164000 3000-EXIT.  EXIT.
164100
164200 3100-CALC-STAY-UTILIZATION.
164300
164400     MOVE 0 TO PPS-REG-DAYS-USED.
164500     MOVE 0 TO PPS-LTR-DAYS-USED.
164600
164700     IF H-REG-DAYS > 0
164800        IF H-REG-DAYS > B-LOS
164900           MOVE B-LOS TO PPS-REG-DAYS-USED
165000        ELSE
165100           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
165200     ELSE
165300        IF H-LTR-DAYS > B-LOS
165400           MOVE B-LOS TO PPS-LTR-DAYS-USED
165500        ELSE
165600           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
165700
165800
165900
166000 3300-CALC-OPER-FSP-AMT.
166100***********************************************************
166200*  OPERATING FSP CALCULATION                              *
166300***********************************************************
166400
166500     COMPUTE H-OPER-FSP-PART ROUNDED =
166600       ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
166700        H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT *
166800        HLD-MID-ADJ-FACT * COVID-ADJ * NO-COST-PRODUCT)
166900           ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
167000
167100 3500-CALC-PERDIEM-AMT.
167200***********************************************************
167300***  REVIEW CODE = 03 OR 06
167400***  OPERATING PERDIEM-AMT CALCULATION
167500***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
167600
167700        COMPUTE H-OPER-FSP-PART ROUNDED =
167800        H-OPER-FSP-PART * H-TRANSFER-ADJ
167900        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
168000
168100***********************************************************
168200***********************************************************
168300***  REVIEW CODE = 03 OR 06
168400***  CAPITAL   PERDIEM-AMT CALCULATION
168500***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
168600
168700        COMPUTE H-CAPI-FSP-PART ROUNDED =
168800        H-CAPI-FSP-PART * H-TRANSFER-ADJ
168900        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
169000
169100***********************************************************
169200***  REVIEW CODE = 03 OR 06
169300***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
169400***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
169500
169600        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
169700        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
169800        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
169900
170000 3550-CALC-PERDIEM-AMT.
170100***********************************************************
170200***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
170300***  OPERATING PERDIEM-AMT CALCULATION
170400***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
170500
170600     IF (D-DRG-POSTACUTE-50-50)
170700        MOVE 10 TO PPS-RTC
170800        COMPUTE H-OPER-FSP-PART ROUNDED =
170900        H-OPER-FSP-PART * H-DSCHG-FRCTN
171000        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
171100
171200     IF (D-DRG-POSTACUTE-PERDIEM)
171300        MOVE 12 TO PPS-RTC
171400        COMPUTE H-OPER-FSP-PART ROUNDED =
171500        H-OPER-FSP-PART *  H-TRANSFER-ADJ
171600        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
171700
171800***********************************************************
171900***  CAPITAL PERDIEM-AMT CALCULATION
172000***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
172100
172200     IF (D-DRG-POSTACUTE-50-50)
172300        MOVE 10 TO PPS-RTC
172400        COMPUTE H-CAPI-FSP-PART ROUNDED =
172500        H-CAPI-FSP-PART * H-DSCHG-FRCTN
172600        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
172700
172800     IF (D-DRG-POSTACUTE-PERDIEM)
172900        MOVE 12 TO PPS-RTC
173000        COMPUTE H-CAPI-FSP-PART ROUNDED =
173100        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
173200        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
173300
173400***********************************************************
173500***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
173600***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
173700
173800     IF (D-DRG-POSTACUTE-50-50)
173900        MOVE 10 TO PPS-RTC
174000        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
174100        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
174200        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
174300
174400     IF (D-DRG-POSTACUTE-PERDIEM)
174500        MOVE 12 TO PPS-RTC
174600        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
174700        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
174800        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
174900
175000 3560-CHECK-RTN-CODE.
175100
175200     IF (D-DRG-POSTACUTE-50-50)
175300        MOVE 10 TO PPS-RTC.
175400     IF (D-DRG-POSTACUTE-PERDIEM)
175500        MOVE 12 TO PPS-RTC.
175600
175700 3560-EXIT.    EXIT.
175800
175900***********************************************************
176000 3600-CALC-OUTLIER.
176100***********************************************************
176200*---------------------------------------------------------*
176300* (YEARCHANGE 2016.0)
176400* COST OUTLIER OPERATING AND CAPITAL CALCULATION
176500*---------------------------------------------------------*
176600
176700     IF OUTLIER-RECON-FLAG = 'Y'
176800        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
176900               (H-OPER-CSTCHG-RATIO + .2).
177000
177100     IF H-CAPI-CSTCHG-RATIO > 0 OR
177200        H-OPER-CSTCHG-RATIO > 0
177300        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
177400                H-OPER-CSTCHG-RATIO /
177500               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
177600        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
177700                H-CAPI-CSTCHG-RATIO /
177800               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
177900     ELSE
178000        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
178100                  H-CAPI-SHARE-DOLL-THRESHOLD.
178200
178300*-----------------------------*
178400* (YEARCHANGE 2020.0)         *
178500* OUTLIER THRESHOLD AMOUNTS   *
178600*-----------------------------*
178700
178800     MOVE 29064.00 TO H-CST-THRESH.
178900
179000     IF (B-REVIEW-CODE = '03') AND
179100         H-PERDIEM-DAYS < H-ALOS
179200        COMPUTE H-CST-THRESH ROUNDED =
179300                      (H-CST-THRESH * H-TRANSFER-ADJ)
179400                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
179500
179600     IF ((B-REVIEW-CODE = '09') AND
179700         (H-PERDIEM-DAYS < H-ALOS))
179800         IF (D-DRG-POSTACUTE-PERDIEM)
179900            COMPUTE H-CST-THRESH ROUNDED =
180000                      (H-CST-THRESH * H-TRANSFER-ADJ)
180100                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
180200
180300     IF ((B-REVIEW-CODE = '09') AND
180400         (H-PERDIEM-DAYS < H-ALOS))
180500         IF (D-DRG-POSTACUTE-50-50)
180600           COMPUTE H-CST-THRESH ROUNDED =
180700          H-CST-THRESH * H-DSCHG-FRCTN
180800                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
180900
181000     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
181100        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
181200         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
181300          H-OPER-SHARE-DOLL-THRESHOLD.
181400
181500***********************************************************
181600
181700     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
181800          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
181900          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
182000
182100***********************************************************
182200******NOW INCLUDES UNCOMPENSATED CARE**********************
182300
182400     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
182500         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
182600                       +
182700           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
182800                       +
182900             H-OPER-DOLLAR-THRESHOLD
183000                       +
183100                WK-UNCOMP-CARE-AMOUNT
183200                       +
183300                 H-NEW-TECH-PAY-ADD-ON.
183400
183500     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
183600      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
183700                       +
183800             H-CAPI-DOLLAR-THRESHOLD.
183900
184000     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
184100         MOVE 0 TO H-CAPI-COST-OUTLIER.
184200
184300
184400***********************************************************
184500***  OPERATING COST CALCULATION
184600
184700     COMPUTE H-OPER-BILL-COSTS ROUNDED =
184800         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
184900         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
185000
185100
185200     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
185300         COMPUTE H-OPER-OUTCST-PART ROUNDED =
185400         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
185500                         H-OPER-COST-OUTLIER).
185600
185700     IF PAY-WITHOUT-COST OR
185800        PAY-XFER-NO-COST OR
185900        PAY-XFER-SPEC-DRG-NO-COST
186000         MOVE 0 TO H-OPER-OUTCST-PART.
186100
186200***********************************************************
186300***  CAPITAL COST CALCULATION
186400
186500     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
186600             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
186700         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
186800
186900     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
187000         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
187100         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
187200                         H-CAPI-COST-OUTLIER).
187300
187400***********************************************************
187500***  'A' NOT VALID FY 2015 ON
187600
187700*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
187800*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
187900*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
188000
188100     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
188200        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
188300               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
188400
188500     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
188600        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
188700        MOVE 0 TO H-CAPI-OUTCST-PART
188800                  H-OPER-OUTCST-PART.
188900
189000     IF PAY-WITHOUT-COST OR
189100        PAY-XFER-NO-COST OR
189200        PAY-XFER-SPEC-DRG-NO-COST
189300         MOVE 0 TO H-CAPI-OUTCST-PART.
189400
189500***********************************************************
189600***  DETERMINES THE BILL TO BE COST  OUTLIER
189700
189800     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
189900         MOVE 0 TO H-CAPI-OUTDAY-PART
190000                   H-CAPI-OUTCST-PART.
190100
190200     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
190300                 MOVE H-OPER-OUTCST-PART TO
190400                      H-OPER-OUTLIER-PART
190500                 MOVE H-CAPI-OUTCST-PART TO
190600                      H-CAPI-OUTLIER-PART
190700                 MOVE 02 TO PPS-RTC.
190800
190900     IF OUTLIER-RECON-FLAG = 'Y'
191000        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
191100           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
191200           GO TO 3600-EXIT
191300        ELSE
191400           GO TO 3600-EXIT
191500     ELSE
191600        NEXT SENTENCE.
191700
191800
191900***********************************************************
192000***  DETERMINES IF COST OUTLIER
192100***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
192200***         RETURN CODE OF 02
192300
192400     MOVE 0 TO H-OPER-CHARGE-THRESHOLD.
192500
192600     IF PPS-RTC = 02
192700       IF H-CAPI-CSTCHG-RATIO > 0 OR
192800          H-OPER-CSTCHG-RATIO > 0
192900             COMPUTE H-OPER-CHARGE-THRESHOLD ROUNDED =
193000                     (H-CAPI-COST-OUTLIER  +
193100                      H-OPER-COST-OUTLIER)
193200                             /
193300                    (H-CAPI-CSTCHG-RATIO  +
193400                     H-OPER-CSTCHG-RATIO)
193500             ON SIZE ERROR MOVE 0 TO H-OPER-CHARGE-THRESHOLD
193600       ELSE MOVE 0 TO H-OPER-CHARGE-THRESHOLD.
193700
193800***********************************************************
193900***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
194000***         RETURN CODE OF 67
194100
194200     IF PPS-RTC = 02
194300         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
194400            PPS-PC-COT-FLAG = 'Y'
194500             MOVE 67 TO PPS-RTC.
194600***********************************************************
194700
194800***********************************************************
194900***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
195000***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
195100***********************************************************
195200*
195300***********************************************************
195400***  'A' NOT VALID FY 2015 ON
195500*
195600*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
195700*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
195800*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
195900*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
196000
196100     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
196200        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
196300                H-CAPI-OUTLIER-PART.
196400
196500     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
196600        H-CAPI-PAYCDE-PCT1 > 0
196700        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
196800                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
196900         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
197000     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
197100
197200 3600-EXIT.   EXIT.
197300
197400***************************************************************
197500 3650-NEW-COVID19-ADD-ON-PAY.
197600***************************************************************
197700* NEW COVID-19 TREATMENTS ADD-ON PAYMENT (NCTAP)
197800*----------------------------------------------------------------*
197900
198000     MOVE 'N' TO NCTAP-ADD-ON-FLAG.
198100     MOVE 1 TO IDX-COVID-DIAG.
198200     MOVE 1 TO IDX-COVID-PROC.
198300     MOVE 1 TO IDX-COVID-COND.
198400     MOVE ZEROES TO NCTAP-ADD-ON.
198500
198600     PERFORM 10000-COVID19-DIAG-FLAG THRU 10000-EXIT
198700     VARYING IDX-COVID-DIAG FROM 1 BY 1 UNTIL IDX-COVID-DIAG > 25.
198800
198900     PERFORM 10050-COVID19-PROC-FLAG THRU 10050-EXIT
199000     VARYING IDX-COVID-PROC FROM 1 BY 1 UNTIL IDX-COVID-PROC > 25.
199100
199200     PERFORM 10100-COVID19-COND-FLAG THRU 10100-EXIT
199300     VARYING IDX-COVID-COND FROM 1 BY 1 UNTIL IDX-COVID-COND > 5.
199400
199500     IF B-DISCHARGE-DATE > 20201101 AND
199600        B-DISCHARGE-DATE < 20201119
199700        IF DIAG-COVID2-FLAG = 'Y' AND
199800           PROC-COVID1-FLAG = 'Y' AND
199900           COND-COVID1-FLAG NOT = 'Y'
200000              MOVE 'Y' TO NCTAP-ADD-ON-FLAG.
200100
200200     IF B-DISCHARGE-DATE > 20201118 AND
200300        B-DISCHARGE-DATE < 20210101
200400        IF DIAG-COVID2-FLAG = 'Y' AND
200500           (PROC-COVID1-FLAG = 'Y' OR
200600            PROC-COVID2-FLAG = 'Y') AND
200700           COND-COVID1-FLAG NOT = 'Y'
200800              MOVE 'Y' TO NCTAP-ADD-ON-FLAG.
200900
201000     IF B-DISCHARGE-DATE > 20201231
201100        IF DIAG-COVID2-FLAG = 'Y' AND
201200           (PROC-COVID1-FLAG = 'Y' OR
201300            PROC-COVID3-FLAG = 'Y') AND
201400           COND-COVID1-FLAG NOT = 'Y'
201500              MOVE 'Y' TO NCTAP-ADD-ON-FLAG.
201600
201700     IF NCTAP-ADD-ON-FLAG = 'Y'
201800        PERFORM 10400-NCTAP-ADD-ON THRU 10400-EXIT.
201900
202000     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
202100             H-OPER-BASE-DRG-PAY + NCTAP-ADD-ON.
202200
202300     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
202400             H-NEW-TECH-PAY-ADD-ON + NCTAP-ADD-ON.
202500
202600 3650-EXIT.   EXIT.
202700
202800***********************************************************
202900 3450-CALC-ADDITIONAL-HSP.
203000***********************************************************
203100*---------------------------------------------------------*
203200* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
203300* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
203400* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
203500*---------------------------------------------------------*
203600***  GET THE RBN UPDATING FACTOR
203700
203800*****YEARCHANGE 2019.0 ****************************************
203900     MOVE 0.997190 TO H-BUDG-NUTR190.
204000
204100*****YEARCHANGE 2020.0 ****************************************
204200     MOVE 0.996859 TO H-BUDG-NUTR200.
204300
204400*****YEARCHANGE 2021.1 ****************************************
204500     MOVE 0.997975 TO H-BUDG-NUTR210.
204600
204700
204800***  GET THE MARKET BASKET UPDATE FACTOR
204900*****YEARCHANGE 2019.0 ****************************************
205000        MOVE 1.01350 TO H-UPDATE-190.
205100
205200*****YEARCHANGE 2020.0 ****************************************
205300        MOVE 1.02600 TO H-UPDATE-200.
205400
205500*****YEARCHANGE 2021.0 ****************************************
205600        MOVE 1.02400 TO H-UPDATE-210.
205700
205800*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
205900*****YEARCHANGE 2021.0 ****************************************
206000     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
206100        P-EHR-REDUC-IND = ' '
206200        MOVE 1.02400 TO H-UPDATE-210.
206300
206400*****YEARCHANGE 2021.0 ****************************************
206500     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
206600        P-EHR-REDUC-IND = 'Y'
206700        MOVE 1.00600 TO H-UPDATE-210.
206800
206900*****YEARCHANGE 2021.0 ****************************************
207000     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
207100        P-EHR-REDUC-IND = ' '
207200        MOVE 1.01800 TO H-UPDATE-210.
207300
207400*****YEARCHANGE 2021.0 ****************************************
207500     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
207600        P-EHR-REDUC-IND = 'Y'
207700        MOVE 1.00000 TO H-UPDATE-210.
207800
207900********YEARCHANGE 2020.0 *************************************
208000
208100     COMPUTE H-UPDATE-FACTOR ROUNDED =
208200                       (H-UPDATE-190 *
208300                        H-UPDATE-200 *
208400                        H-UPDATE-210 *
208500                        H-BUDG-NUTR190 *
208600                        H-BUDG-NUTR200 *
208700                        H-BUDG-NUTR210 *
208800                        HLD-MID-ADJ-FACT).
208900
209000     COMPUTE H-HSP-RATE ROUNDED =
209100         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT * COVID-ADJ
209200         * NO-COST-PRODUCT.
209300
209400***************************************************************
209500*
209600*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
209700*       COMPUTE H-HSP-RATE ROUNDED =
209800*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
209900*    ELSE
210000*       COMPUTE H-HSP-RATE ROUNDED =
210100*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
210200*
210300***************************************************************
210400********YEARCHANGE 2011.0 *************************************
210500***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
210600***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
210700***     IN THE HSP COMPARRISON
210800
210900********YEARCHANGE 2014.0 *XXXXXX******************************
211000*      THE HSP BUCKET FOR SCH                      ************
211100*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
211200***************************************************************
211300
211400     COMPUTE H-FSP-RATE ROUNDED =
211500        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
211600         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN *
211700         HLD-MID-ADJ-FACT * COVID-ADJ * NO-COST-PRODUCT) *
211800             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
211900                               +
212000                         H-OPER-OUTLIER-PART
212100                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
212200
212300****************************************************************
212400****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
212500*****        CHOICE
212600
212700     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
212800           COMPUTE H-OPER-HSP-PART ROUNDED =
212900             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
213000                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
213100     ELSE
213200         MOVE 0 TO H-OPER-HSP-PART.
213300
213400***************************************************************
213500***  YEARCHANGE TURNING MDH BACK ON ***************************
213600***************************************************************
213700***  GET THE MDH REBASE
213800
213900     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
214000         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
214100           COMPUTE H-OPER-HSP-PART ROUNDED =
214200         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
214300                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
214400
214500***************************************************************
214600***  TRANSITIONAL PAYMENT FOR FORMER MDHS                     *
214700***************************************************************
214800
214900***  HSP PAYMENT FOR CLAIMS BETWEEN 10/01/2016 - 09/30/2017
215000
215100*    IF  B-FORMER-MDH-PROVIDERS       AND
215200*       (B-DISCHARGE-DATE > 20160930  AND
215300*        B-DISCHARGE-DATE < 20171001)
215400*      IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
215500*        COMPUTE H-OPER-HSP-PART ROUNDED =
215600*          ((H-HSP-RATE - (H-FSP-RATE +
215700*              WK-UNCOMP-CARE-AMOUNT))* 0.75)*(1 / 3)
215800*            ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
215900*      END-IF
216000*    END-IF.
216100
216200 3450-EXIT.   EXIT.
216300
216400***********************************************************
216500 3800-CALC-TOT-AMT.
216600***********************************************************
216700***  CALCULATE TOTALS FOR CAPITAL
216800
216900     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
217000
217100***********************************************************
217200***  'A' NOT VALID FY 2015 ON
217300*
217400*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
217500*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
217600*       MOVE 0.00 TO H-CAPI-HSP-PCT.
217700
217800     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
217900        MOVE 0    TO H-CAPI-OLD-HARMLESS
218000        MOVE 1.00 TO H-CAPI-FSP-PCT
218100        MOVE 0.00 TO H-CAPI-HSP-PCT.
218200
218300     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
218400        MOVE 0    TO H-CAPI-OLD-HARMLESS
218500        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
218600        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
218700
218800     COMPUTE H-CAPI-HSP ROUNDED =
218900         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
219000
219100     COMPUTE H-CAPI-FSP ROUNDED =
219200         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
219300
219400     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
219500
219600     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
219700
219800     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
219900             H-CAPI-FSP
220000              * H-CAPI-DSH.
220100
220200     COMPUTE H-CAPI-IME-ADJ ROUNDED =
220300          H-CAPI-FSP *
220400                 H-WK-CAPI-IME-TEACH.
220500
220600     COMPUTE H-CAPI-OUTLIER ROUNDED =
220700             1.00 * H-CAPI-OUTLIER-PART.
220800
220900     COMPUTE H-CAPI2-B-FSP ROUNDED =
221000             1.00 * H-CAPI2-B-FSP-PART.
221100
221200     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
221300             1.00 * H-CAPI2-B-OUTLIER-PART.
221400***********************************************************
221500***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
221600***        THIS ZEROES OUT ALL CAPITAL DATA
221700
221800     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
221900        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
222000***********************************************************
222100
222200***********************************************************
222300***  CALCULATE FINAL TOTALS FOR OPERATING
222400
222500     IF (H-CAPI-OUTLIER > 0 AND
222600         PPS-OPER-OUTLIER-PART = 0)
222700            COMPUTE PPS-OPER-OUTLIER-PART =
222800                    PPS-OPER-OUTLIER-PART + .01.
222900
223000***********************************************************
223100*LOW VOLUME CALCULATIONS
223200***********************************************************
223300*---------------------------------------------------------*
223400* (YEARCHANGE 2016.0)
223500* LOW VOLUME PAYMENT ADD-ON PERCENT
223600*---------------------------------------------------------*
223700
223800     MOVE ZERO TO PPS-OPER-DSH-ADJ.
223900************************************************
224000* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
224100* EMPERICAL DSH
224200************************************************
224300     IF  H-OPER-DSH NUMERIC
224400         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
224500                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
224600
224700     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
224800                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
224900
225000     COMPUTE PPS-OPER-FSP-PART ROUNDED =
225100                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
225200
225300     COMPUTE PPS-OPER-HSP-PART ROUNDED =
225400                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
225500
225600     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
225700                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
225800
225900     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
226000                                H-NEW-TECH-PAY-ADD-ON.
226100
226200     COMPUTE PPS-ISLET-ISOL-PAY-ADD-ON ROUNDED =
226300                                H-NEW-TECH-ADDON-ISLET.
226400
226500     IF P-NEW-TEMP-RELIEF-IND = 'Y'
226600        AND P-LV-ADJ-FACTOR > 0.00
226700        AND P-LV-ADJ-FACTOR <= 0.25
226800     COMPUTE WK-LOW-VOL-ADDON ROUNDED =
226900       (PPS-OPER-HSP-PART +
227000        PPS-OPER-FSP-PART +
227100        PPS-OPER-IME-ADJ +
227200        PPS-OPER-DSH-ADJ +
227300        PPS-OPER-OUTLIER-PART +
227400        H-CAPI-FSP +
227500        H-CAPI-IME-ADJ +
227600        H-CAPI-DSH-ADJ +
227700        H-CAPI-OUTLIER +
227800        WK-UNCOMP-CARE-AMOUNT +
227900        PPS-NEW-TECH-PAY-ADD-ON) * P-LV-ADJ-FACTOR
228000     ELSE
228100     COMPUTE WK-LOW-VOL-ADDON ROUNDED = 0.
228200
228300     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
228400     IF HMO-TAG  = 'Y'
228500        PERFORM 3850-HMO-IME-ADJ.
228600
228700***********************************************************
228800***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
228900
229000     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
229100             H-CAPI-FSP + H-CAPI-IME-ADJ +
229200             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
229300
229400         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
229500         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
229600
229700***********************************************************
229800* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
229900***********************************************************
230000*---------------------------------------------------------*
230100* (YEARCHANGE 2016.0)
230200* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
230300*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
230400*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
230500*   + BASED ON INDICATOR FROM THE PPS FILE
230600*   + NOT VALID IN PUERTO RICO
230700*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
230800*---------------------------------------------------------*
230900
231000     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
231100        PPS-OPER-HSP-PART +
231200        PPS-OPER-FSP-PART +
231300        PPS-OPER-IME-ADJ +
231400        PPS-OPER-DSH-ADJ +
231500        PPS-OPER-OUTLIER-PART +
231600        H-CAPI-TOTAL-PAY +
231700        WK-UNCOMP-CARE-AMOUNT +
231800        PPS-NEW-TECH-PAY-ADD-ON +
231900        WK-LOW-VOL-ADDON +
232000        H-READMIS-ADJUST-AMT +
232100        H-VAL-BASED-PURCH-ADJUST-AMT.
232200
232300     MOVE ZERO TO WK-HAC-AMOUNT.
232400
232500     IF P-PR-NEW-STATE AND
232600        P-HAC-REDUC-IND = 'Y'
232700           MOVE 53 TO PPS-RTC
232800           GO TO 3800-EXIT.
232900
233000     IF  P-HAC-REDUC-IND = 'Y'
233100         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
233200                   WK-HAC-TOTAL-PAYMENT * -0.01
233300     ELSE
233400         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
233500
233600***********************************************************
233700***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
233800************************************************
233900     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
234000                 WK-HAC-TOTAL-PAYMENT
234100                           +
234200                 H-WK-PASS-AMT-PLUS-MISC
234300                           +
234400                 H-BUNDLE-ADJUST-AMT
234500                           +
234600                 WK-HAC-AMOUNT
234700                           +
234800                 H-NEW-TECH-ADDON-ISLET.
234900
235000     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
235100              H-VAL-BASED-PURCH-PARTIPNT.
235200
235300     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
235400              H-VAL-BASED-PURCH-ADJUST.
235500
235600     MOVE     P-HOSP-READMISSION-REDU    TO
235700              H-HOSP-READMISSION-REDU.
235800
235900     MOVE     P-HOSP-HRR-ADJUSTMT        TO
236000              H-HOSP-HRR-ADJUSTMT.
236100
236200 3800-EXIT.   EXIT.
236300
236400 3850-HMO-IME-ADJ.
236500***********************************************************
236600***  HMO CALC FOR PASS-THRU ADDON
236700
236800     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
236900          (P-NEW-PASS-AMT-PLUS-MISC -
237000          (P-NEW-PASS-AMT-ORGAN-ACQ +
237100           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
237200
237300***********************************************************
237400***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
237500
237600     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
237700                   PPS-OPER-IME-ADJ * .0.
237800
237900***********************************************************
238000
238100
238200 3900A-CALC-OPER-DSH.
238300
238400***  OPERATING DSH CALCULATION
238500
238600      MOVE 0.0000 TO H-OPER-DSH.
238700
238800      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
238900                                     + P-NEW-MEDICAID-RATIO).
239000
239100***********************************************************
239200**1**    0-99 BEDS
239300***  NOT TO EXCEED 12%
239400
239500      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
239600                               AND H-WK-OPER-DSH > .1499
239700                               AND H-WK-OPER-DSH < .2020
239800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
239900                                      * .65 + .025
240000        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
240100
240200      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
240300                               AND H-WK-OPER-DSH > .2019
240400        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
240500                                      * .825 + .0588
240600        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
240700
240800***********************************************************
240900**2**   100 + BEDS
241000***  NO CAP >> CAN EXCEED 12%
241100
241200      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
241300                               AND H-WK-OPER-DSH > .1499
241400                               AND H-WK-OPER-DSH < .2020
241500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
241600                                      * .65 + .025.
241700
241800      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
241900                               AND H-WK-OPER-DSH > .2019
242000        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
242100                                      * .825 + .0588.
242200
242300***********************************************************
242400**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
242500***  NOT TO EXCEED 12%
242600
242700      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
242800                               AND H-WK-OPER-DSH > .1499
242900                               AND H-WK-OPER-DSH < .2020
243000        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
243100                                 * .65 + .025
243200        IF H-OPER-DSH > .1200
243300              MOVE .1200 TO H-OPER-DSH.
243400
243500      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
243600                               AND H-WK-OPER-DSH > .2019
243700        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
243800                                 * .825 + .0588
243900        IF H-OPER-DSH > .1200
244000                 MOVE .1200 TO H-OPER-DSH.
244100***********************************************************
244200**4**   OTHER RURAL HOSPITALS 500 BEDS +
244300***  NO CAP >> CAN EXCEED 12%
244400
244500      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
244600                               AND H-WK-OPER-DSH > .1499
244700                               AND H-WK-OPER-DSH < .2020
244800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
244900                                 * .65 + .025.
245000
245100      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
245200                               AND H-WK-OPER-DSH > .2019
245300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
245400                                 * .825 + .0588.
245500
245600***********************************************************
245700**7**   RURAL HOSPITALS SCH
245800***  NOT TO EXCEED 12%
245900
246000      IF W-CBSA-SIZE = 'R'
246100         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
246200                               AND H-WK-OPER-DSH > .1499
246300                               AND H-WK-OPER-DSH < .2020
246400         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
246500                                 * .65 + .025
246600        IF H-OPER-DSH > .1200
246700                 MOVE .1200 TO H-OPER-DSH.
246800
246900      IF W-CBSA-SIZE = 'R'
247000         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
247100                               AND H-WK-OPER-DSH > .2019
247200         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
247300                                 * .825 + .0588
247400        IF H-OPER-DSH > .1200
247500                 MOVE .1200 TO H-OPER-DSH.
247600
247700***********************************************************
247800**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
247900***  RRC OVERRIDES SCH CAP
248000***  NO CAP >> CAN EXCEED 12%
248100
248200         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
248300                                   '17' OR '22')
248400                               AND H-WK-OPER-DSH > .1499
248500                               AND H-WK-OPER-DSH < .2020
248600         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
248700                                 * .65 + .025.
248800
248900         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
249000                                   '17' OR '22')
249100                               AND H-WK-OPER-DSH > .2019
249200         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
249300                                 * .825 + .0588.
249400
249500      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
249600
249700 3900A-EXIT.   EXIT.
249800
249900 4000-CALC-TECH-ADDON.
250000
250100***********************************************************
250200***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
250300
250400     COMPUTE PPS-OPER-HSP-PART ROUNDED =
250500         H-OPER-HSP-PCT * H-OPER-HSP-PART.
250600
250700     COMPUTE PPS-OPER-FSP-PART ROUNDED =
250800         H-OPER-FSP-PCT * H-OPER-FSP-PART.
250900
251000     MOVE ZERO TO PPS-OPER-DSH-ADJ.
251100
251200     IF  H-OPER-DSH NUMERIC
251300             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
251400             (PPS-OPER-FSP-PART
251500              * H-OPER-DSH) * .25.
251600
251700     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
251800             PPS-OPER-FSP-PART *
251900             H-OPER-IME-TEACH.
252000
252100     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
252200             PPS-OPER-FSP-PART +
252300             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
252400             WK-UNCOMP-CARE-AMOUNT.
252500
252600***********************************************************
252700* NEW TECHNOLOGY ADD-ON CODE *
252800***********************************************************
252900     MOVE 1 TO IDX-TECH.
253000     INITIALIZE H-CSTMED-STOP.
253100     INITIALIZE H-NEW-TECH-PCT.
253200     INITIALIZE H-TECH-ADDON-ISLET-CNTR.
253300
253400     PERFORM 4010-FLAG-NEW-TECH THRU 4010-EXIT
253500      VARYING IDX-TECH FROM 1 BY 1 UNTIL IDX-TECH > 25.
253600
253700     IF PROC-ANDEXXA-FLAG = 'Y'
253800       MOVE  18281.25 TO H-CSTMED-STOP.
253900       MOVE 0.65 TO H-NEW-TECH-PCT.
254000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
254100
254200     IF PROC-AZEDRA-FLAG = 'Y'
254300       MOVE  98150.00 TO H-CSTMED-STOP.
254400       MOVE 0.65 TO H-NEW-TECH-PCT.
254500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
254600
254700     IF PROC-BALVERSA-FLAG = 'Y'
254800       MOVE   3563.23 TO H-CSTMED-STOP.
254900       MOVE 0.65 TO H-NEW-TECH-PCT.
255000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
255100
255200     IF PROC-BAROSTIM1-FLAG = 'Y' AND PROC-BAROSTIM2-FLAG
255300       MOVE  22750.00 TO H-CSTMED-STOP.
255400       MOVE 0.65 TO H-NEW-TECH-PCT.
255500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
255600
255700     IF PROC-CABLIVI-FLAG = 'Y'
255800       MOVE  33215.00 TO H-CSTMED-STOP.
255900       MOVE 0.65 TO H-NEW-TECH-PCT.
256000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
256100
256200     IF PROC-CONTACT-FLAG = 'Y'
256300       MOVE   1040.00 TO H-CSTMED-STOP.
256400       MOVE 0.65 TO H-NEW-TECH-PCT.
256500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
256600
256700     IF PROC-ELUVIA-FLAG = 'Y'
256800       MOVE   3646.50 TO H-CSTMED-STOP.
256900       MOVE 0.65 TO H-NEW-TECH-PCT.
257000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
257100
257200     IF PROC-ELZONRIS-FLAG = 'Y'
257300       MOVE 125448.05 TO H-CSTMED-STOP.
257400       MOVE 0.65 TO H-NEW-TECH-PCT.
257500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
257600
257700     IF PROC-FETROJA-FLAG = 'Y'
257800       MOVE   7919.86 TO H-CSTMED-STOP.
257900       MOVE 0.75 TO H-NEW-TECH-PCT.
258000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
258100
258200     IF PROC-HEMOSPRAY-FLAG = 'Y'
258300       MOVE   1625.00 TO H-CSTMED-STOP.
258400       MOVE 0.65 TO H-NEW-TECH-PCT.
258500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
258600
258700     IF PROC-IMFINZI-FLAG = 'Y'
258800       MOVE   6875.90 TO H-CSTMED-STOP.
258900       MOVE 0.65 TO H-NEW-TECH-PCT.
259000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
259100
259200     IF DIAG-ISLET-FLAG = 'Y' AND PROC-ISLET-FLAG = 'Y'
259300       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
259400     ELSE
259500       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
259600
259700     IF PROC-JAKAFI-FLAG = 'Y'
259800       MOVE   4096.21 TO H-CSTMED-STOP.
259900       MOVE 0.65 TO H-NEW-TECH-PCT.
260000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
260100
260200     IF PROC-NUZYRA-FLAG = 'Y'
260300       MOVE   1552.50 TO H-CSTMED-STOP.
260400       MOVE 0.75 TO H-NEW-TECH-PCT.
260500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
260600
260700     IF PROC-OPTIMIZER-FLAG = 'Y'
260800       MOVE  14950.00 TO H-CSTMED-STOP.
260900       MOVE 0.65 TO H-NEW-TECH-PCT.
261000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
261100
261200     IF PROC-PLAZO-FLAG = 'Y'
261300       MOVE   4083.75 TO H-CSTMED-STOP.
261400       MOVE 0.75 TO H-NEW-TECH-PCT.
261500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
261600
261700     IF PROC-RECARBIO-FLAG = 'Y'
261800       MOVE   3532.78 TO H-CSTMED-STOP.
261900       MOVE 0.75 TO H-NEW-TECH-PCT.
262000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
262100
262200     IF PROC-SOLIRIS-FLAG = 'Y'
262300       MOVE  21199.75 TO H-CSTMED-STOP.
262400       MOVE 0.65 TO H-NEW-TECH-PCT.
262500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
262600
262700     IF PROC-SPINEJACK-FLAG = 'Y'
262800       MOVE   3654.72 TO H-CSTMED-STOP.
262900       MOVE 0.65 TO H-NEW-TECH-PCT.
263000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
263100
263200     IF PROC-SPRAVATO-FLAG = 'Y'
263300       MOVE   1014.79 TO H-CSTMED-STOP.
263400       MOVE 0.65 TO H-NEW-TECH-PCT.
263500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
263600
263700     IF PROC-T2-FLAG = 'Y'
263800       MOVE     97.50 TO H-CSTMED-STOP.
263900       MOVE 0.65 TO H-NEW-TECH-PCT.
264000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
264100
264200     IF PROC-TECENTRIQ-FLAG = 'Y'
264300       MOVE   6875.90 TO H-CSTMED-STOP.
264400       MOVE 0.65 TO H-NEW-TECH-PCT.
264500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
264600
264700     IF PROC-XENLETA-FLAG = 'Y'
264800       MOVE   1275.75 TO H-CSTMED-STOP.
264900       MOVE 0.75 TO H-NEW-TECH-PCT.
265000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
265100
265200     IF PROC-XOSPATA-FLAG = 'Y'
265300       MOVE   7312.50 TO H-CSTMED-STOP.
265400       MOVE 0.65 TO H-NEW-TECH-PCT.
265500       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
265600
265700     IF PROC-ZERBAXA-FLAG = 'Y'
265800       MOVE   1836.98 TO H-CSTMED-STOP.
265900       MOVE 0.75 TO H-NEW-TECH-PCT.
266000       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
266100
266200***********************************************************
266300*  ALL NEW TECH MUST BE CALCULATED BEFORE
266400*  5500-CAP-CALC-TECH-ADD-ON
266500***********************************************************
266600     PERFORM 5500-CAP-CALC-TECH-ADD-ON THRU 5500-EXIT.
266700
266800     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
266900             H-OPER-FSP-PART +
267000             H-NEW-TECH-PAY-ADD-ON.
267100
267200 4000-EXIT.    EXIT.
267300
267400************************************
267500* NEW TECHNOLOGY ADD-ON FLAG LOGIC *
267600************************************
267700 4010-FLAG-NEW-TECH.
267800
267900     MOVE B-PROCEDURE-CODE(IDX-TECH) TO WK-PROC-NEW-TECH.
268000     MOVE B-DIAGNOSIS-CODE(IDX-TECH) TO WK-DIAG-NEW-TECH.
268100*    MOVE B-NDC-NUMBER TO WK-NDC-NEW-TECH.
268200
268300     IF PROC-ANDEXXA
268400       MOVE 'Y' TO PROC-ANDEXXA-FLAG.
268500
268600     IF PROC-AZEDRA
268700       MOVE 'Y' TO PROC-AZEDRA-FLAG.
268800
268900     IF PROC-BALVERSA
269000       MOVE 'Y' TO PROC-BALVERSA-FLAG.
269100
269200     IF PROC-BAROSTIM1
269300       MOVE 'Y' TO PROC-BAROSTIM1-FLAG.
269400
269500     IF PROC-BAROSTIM2
269600       MOVE 'Y' TO PROC-BAROSTIM2-FLAG.
269700
269800     IF PROC-CABLIVI
269900       MOVE 'Y' TO PROC-CABLIVI-FLAG.
270000
270100     IF PROC-CONTACT
270200       MOVE 'Y' TO PROC-CONTACT-FLAG.
270300
270400     IF PROC-ELUVIA
270500       MOVE 'Y' TO PROC-ELUVIA-FLAG.
270600
270700     IF PROC-ELZONRIS
270800       MOVE 'Y' TO PROC-ELZONRIS-FLAG.
270900
271000     IF PROC-FETROJA
271100       MOVE 'Y' TO PROC-FETROJA-FLAG.
271200
271300     IF PROC-ISLET
271400       MOVE 'Y' TO PROC-ISLET-FLAG
271500       COMPUTE H-TECH-ADDON-ISLET-CNTR =
271600          H-TECH-ADDON-ISLET-CNTR + 1.
271700
271800     IF PROC-HEMOSPRAY
271900       MOVE 'Y' TO PROC-HEMOSPRAY-FLAG.
272000
272100     IF PROC-IMFINZI
272200       MOVE 'Y' TO PROC-IMFINZI-FLAG.
272300
272400     IF PROC-JAKAFI
272500       MOVE 'Y' TO PROC-JAKAFI-FLAG.
272600
272700     IF PROC-NUZYRA
272800       MOVE 'Y' TO PROC-NUZYRA-FLAG.
272900
273000     IF PROC-OPTIMIZER
273100       MOVE 'Y' TO PROC-OPTIMIZER-FLAG.
273200
273300     IF PROC-PLAZO
273400       MOVE 'Y' TO PROC-PLAZO-FLAG.
273500
273600     IF PROC-RECARBIO
273700       MOVE 'Y' TO PROC-RECARBIO-FLAG.
273800
273900     IF PROC-SOLIRIS
274000       MOVE 'Y' TO PROC-SOLIRIS-FLAG.
274100
274200     IF PROC-SPINEJACK
274300       MOVE 'Y' TO PROC-SPINEJACK-FLAG.
274400
274500     IF PROC-SPRAVATO
274600       MOVE 'Y' TO PROC-SPRAVATO-FLAG.
274700
274800     IF PROC-T2
274900       MOVE 'Y' TO PROC-T2-FLAG.
275000
275100     IF PROC-TECENTRIQ
275200       MOVE 'Y' TO PROC-TECENTRIQ-FLAG.
275300
275400     IF PROC-XENLETA
275500       MOVE 'Y' TO PROC-XENLETA-FLAG.
275600
275700     IF PROC-XOSPATA
275800       MOVE 'Y' TO PROC-XOSPATA-FLAG.
275900
276000     IF PROC-ZERBAXA
276100       MOVE 'Y' TO PROC-ZERBAXA-FLAG.
276200
276300     IF DIAG-ISLET
276400       MOVE 'Y' TO DIAG-ISLET-FLAG.
276500
276600 4010-EXIT.   EXIT.
276700
276800*******************************************
276900* NEW TECHNOLOGY ADD-ON CALCULATION LOGIC *
277000*******************************************
277100 4020-NEW-TECH-ADD-ON.
277200
277300     MOVE 0 TO H-NEW-TECH-ADDON
277400               H-LESSER-STOP-1
277500               H-LESSER-STOP-2.
277600
277700     COMPUTE H-LESSER-STOP-1 ROUNDED =
277800                  H-CSTMED-STOP.
277900
278000     COMPUTE H-LESSER-STOP-2 ROUNDED =
278100          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
278200             H-BASE-DRG-PAYMENT)) * H-NEW-TECH-PCT.
278300
278400     IF H-LESSER-STOP-2 > 0
278500        IF H-LESSER-STOP-1 < H-LESSER-STOP-2
278600         MOVE H-LESSER-STOP-1 TO
278700                                H-NEW-TECH-ADDON
278800        ELSE
278900         MOVE H-LESSER-STOP-2 TO
279000                                H-NEW-TECH-ADDON
279100     ELSE
279200        MOVE ZEROES          TO H-NEW-TECH-ADDON.
279300
279400     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
279500             H-NEW-TECH-PAY-ADD-ON +
279600             H-NEW-TECH-ADDON.
279700
279800     MOVE 0 TO H-NEW-TECH-ADDON
279900               H-LESSER-STOP-1
280000               H-LESSER-STOP-2
280100               H-CSTMED-STOP.
280200
280300 4020-EXIT.    EXIT.
280400
280500***********************************************************
280600* TECHNICAL TRANSPLANTATION OF CELLS                      *
280700***********************************************************
280800 4100-ISLET-ISOLATION-ADD-ON.
280900
281000     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
281100
281200     IF  H-TECH-ADDON-ISLET-CNTR = 1
281300     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
281400           GO TO 4100-EXIT.
281500
281600     IF  H-TECH-ADDON-ISLET-CNTR > 1
281700     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
281800           GO TO 4100-EXIT.
281900
282000 4100-EXIT.    EXIT.
282100
282200***********************************************************
282300* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
282400* DISCHARGE COUNTS.
282500***********************************************************
282600*4400-LOWVOL-CODE-RTN.
282700*
282800*    SET LOWVOL-IDX TO 1.
282900*    SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
283000*        AT END
283100*          MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
283200*          MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
283300*      WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
283400*        MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
283500*                           TO MESWK-LOWVOL-PROV-DISCHG.
283600*
283700*4400-EXIT.   EXIT.
283800
283900*****************************************************************
284000* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR DISCHARGE *
284100* COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER FILE       *
284200*****************************************************************
284300 4410-UNCOMP-CARE-CODE-RTN.
284400
284500*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
284600*
284700*    SET UNCOMP-CARE-IDX TO 1.
284800*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
284900*        AT END
285000*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
285100*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
285200*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
285300*                           TO WK-UNCOMP-CARE-AMOUNT.
285400*
285500        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
285600
285700        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
285800
285900 4410-EXIT.   EXIT.
286000
286100**************************************************************
286200* CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM *
286300**************************************************************
286400 5500-CAP-CALC-TECH-ADD-ON.
286500
286600     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
286700     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
286800
286900     COMPUTE H-OPER-BILL-COSTS ROUNDED =
287000         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
287100         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
287200
287300     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
287400                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
287500
287600     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
287700                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
287800
287900     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
288000         H-NEW-TECH-ADDON-CAPDIF  > 0
288100        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
288200             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
288300
288400 5500-EXIT.    EXIT.
288500
288600***********************************************************
288700 6000-CALC-READMIS-REDU.
288800***********************************************************
288900*---------------------------------------------------------*
289000* (YEARCHANGE 2016.0)
289100* READMISSIONS PROCESS ADJUSTMENTS
289200*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
289300*---------------------------------------------------------*
289400
289500     MOVE 0 TO H-READMIS-ADJUST-AMT.
289600
289700     IF P-HOSP-READMISSION-REDU = '1'
289800           GO TO 6000-EDIT-READMISN
289900     ELSE
290000           NEXT SENTENCE.
290100
290200     IF P-HOSP-READMISSION-REDU = '0' AND
290300        P-HOSP-HRR-ADJUSTMT = 0.0000
290400           MOVE ZEROES TO H-READMIS-ADJUST-AMT
290500           GO TO 6000-EXIT.
290600
290700     IF P-HOSP-READMISSION-REDU = '0' AND
290800        P-HOSP-HRR-ADJUSTMT > 0.0000
290900           MOVE 65 TO PPS-RTC
291000           MOVE ZEROES TO H-READMIS-ADJUST-AMT
291100           GO TO 6000-EXIT.
291200
291300     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
291400                                  '6' OR '7' OR '8' OR
291500                                  '9' OR ' '
291600           MOVE 65 TO PPS-RTC
291700           MOVE ZEROES TO H-READMIS-ADJUST-AMT
291800           GO TO 6000-EXIT.
291900
292000 6000-EDIT-READMISN.
292100
292200     IF P-HOSP-HRR-ADJUSTMT < 0.9700
292300           MOVE 65 TO PPS-RTC
292400           MOVE ZEROES TO H-READMIS-ADJUST-AMT
292500           GO TO 6000-EXIT.
292600
292700     IF P-HOSP-HRR-ADJUSTMT > 1.0000
292800           MOVE 65 TO PPS-RTC
292900           MOVE ZEROES TO H-READMIS-ADJUST-AMT
293000           GO TO 6000-EXIT.
293100
293200     IF P-READ-INVALID-STATE
293300           MOVE 65 TO PPS-RTC
293400           MOVE ZEROES TO H-READMIS-ADJUST-AMT
293500           GO TO 6000-EXIT.
293600
293700 6000-COMPUTE-READMISN.
293800
293900        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
294000              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
294100                H-OPER-BASE-DRG-PAY).
294200
294300 6000-EXIT.    EXIT.
294400
294500***********************************************************
294600 7000-CALC-VALUE-BASED-PURCH.
294700***********************************************************
294800*---------------------------------------------------------*
294900* (YEARCHANGE 2016.0)
295000* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
295100*   + FY17: RANGE OF ALLOWABLE FACTORS (< 0.98 OR > 2.0)
295200*---------------------------------------------------------*
295300
295400     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
295500
295600     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
295700           NEXT SENTENCE
295800     ELSE
295900           MOVE 68 TO PPS-RTC
296000           GO TO 7000-EXIT.
296100
296200     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
296300           GO TO 7000-EXIT.
296400
296500     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
296600         P-NEW-CBSA-HOSP-QUAL-IND = '1'
296700           NEXT SENTENCE
296800     ELSE
296900           MOVE 68 TO PPS-RTC
297000           GO TO 7000-EXIT.
297100
297200     IF  P-VBP-INVALID-STATE
297300           MOVE 68 TO PPS-RTC
297400           GO TO 7000-EXIT
297500     ELSE
297600           NEXT SENTENCE.
297700
297800     IF P-VAL-BASED-PURCH-ADJUST < 0.9800000000 OR
297900        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
298000           MOVE 68 TO PPS-RTC
298100           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
298200           GO TO 7000-EXIT
298300     ELSE
298400           GO TO 7000-COMPUTE-VAL-BASED-PUR.
298500
298600 7000-COMPUTE-VAL-BASED-PUR.
298700
298800     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
298900              ((P-VAL-BASED-PURCH-ADJUST *
299000                  H-OPER-BASE-DRG-PAY) -
299100                  H-OPER-BASE-DRG-PAY).
299200
299300 7000-EXIT.    EXIT.
299400
299500***********************************************************
299600 8000-CALC-BUNDLE-REDU.
299700***********************************************************
299800* CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
299900* SUMMARY: BPCI CLASSIC CMMI MODEL THAT HAD FOUR PARTS
300000*          RAN BETWEEN 2013 AND 2018
300100***********************************************************
300200
300300     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
300400     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
300500
300600     IF '61' =  B-DEMO-CODE1  OR
300700                B-DEMO-CODE2  OR
300800                B-DEMO-CODE3  OR
300900                B-DEMO-CODE4
301000         NEXT SENTENCE
301100     ELSE
301200         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
301300           GO TO 8000-EXIT.
301400
301500     IF P-MODEL1-BUNDLE-DISPRCNT > .00
301600           GO TO 8000-COMPUTE-BUNDLE
301700     ELSE
301800           NEXT SENTENCE.
301900
302000     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
302100           GO TO 8000-EXIT.
302200
302300 8000-COMPUTE-BUNDLE.
302400
302500     IF B-DISCHARGE-DATE < 20140401 AND
302600        P-MODEL1-BUNDLE-DISPRCNT = .01
302700          COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
302800            (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
302900
303000     IF B-DISCHARGE-DATE > 20140331 AND
303100        B-DISCHARGE-DATE < 20170101
303200          COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
303300            (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
303400
303500     IF B-DISCHARGE-DATE > 20161231
303600          COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
303700            (1 - (P-MODEL1-BUNDLE-DISPRCNT * 0)).
303800
303900     COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED =
304000       ((WK-MODEL1-BUNDLE-DISPRCNT * H-OPER-BASE-DRG-PAY) -
304100         H-OPER-BASE-DRG-PAY).
304200
304300     COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
304400
304500 8000-EXIT.    EXIT.
304600
304700***********************************************************
304800 9000-CALC-EHR-SAVING.
304900***********************************************************
305000*---------------------------------------------------------*
305100* (YEARCHANGE 2021.0)
305200* CASES INVOLVING EHR SAVINGS
305300*   + FY20: ANNUAL UPDATE TO BELOW VALUES
305400*   + EHR-FULL = FULL MB / NO EHR MB
305500*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
305600*---------------------------------------------------------*
305700
305800     MOVE 1.017892644 TO H-MB-RATIO-EHR-FULL.
305900     MOVE 1.018000000 TO H-MB-RATIO-EHR-QUAL-FULL.
306000     MOVE 0 TO H-EHR-SUBSAV-QUANT.
306100     MOVE 0 TO H-EHR-SUBSAV-LV.
306200     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
306300     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
306400
306500     IF P-EHR-REDUC-IND = 'Y'
306600         NEXT SENTENCE
306700     ELSE
306800         GO TO 9000-EXIT.
306900
307000 9000-COMPUTE-EHR.
307100
307200* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
307300* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
307400* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
307500* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
307600* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
307700* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
307800* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
307900* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
308000* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
308100* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
308200* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
308300* OUT.
308400*
308500* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
308600* SAVINGS CALCULATION SO CALCULATE AN OPERATING
308700* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
308800* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
308900* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
309000
309100      COMPUTE H-EHR-SUBSAV-QUANT =
309200           (PPS-OPER-HSP-PART +
309300            PPS-OPER-FSP-PART +
309400            PPS-OPER-DSH-ADJ +
309500            PPS-OPER-IME-ADJ +
309600            H-READMIS-ADJUST-AMT +
309700            H-VAL-BASED-PURCH-ADJUST-AMT +
309800            H-BUNDLE-ADJUST-AMT).
309900
310000* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
310100* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
310200* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
310300* READM VBP AND BUNDLE
310400* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
310500* FOR EHR SAVINGS FIELD;
310600
310700      MOVE 0 TO H-EHR-SUBSAV-LV.
310800
310900      IF P-NEW-TEMP-RELIEF-IND = 'Y'
311000         AND P-LV-ADJ-FACTOR > 0.00
311100         AND P-LV-ADJ-FACTOR <= 0.25
311200      COMPUTE H-EHR-SUBSAV-LV =
311300          (PPS-OPER-HSP-PART +
311400           PPS-OPER-FSP-PART +
311500           PPS-OPER-DSH-ADJ +
311600           PPS-OPER-IME-ADJ ) * P-LV-ADJ-FACTOR.
311700
311800      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
311900           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
312000
312100* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
312200* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
312300* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
312400* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
312500* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
312600* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
312700* OF .625 ONLY.
312800
312900       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
313000       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
313100
313200     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
313300        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
313400          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
313500
313600        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
313700          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
313800
313900 9000-EXIT.    EXIT.
314000
314100*---------------------------------------------------------*
314200* (YEARCHANGE 2016.0)
314300*---------------------------------------------------------*
314400 9010-CALC-STANDARD-CHG.
314500
314600***********************************************************
314700***ÝCM-P3¨ STANDARDIZED OPERATING COST CALCULATION
314800
314900     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
315000               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
315100        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
315200        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
315300        ((H-LABOR-PCT * H-WAGE-INDEX) +
315400               (H-NONLABOR-PCT * H-OPER-COLA))
315500     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
315600
315700***********************************************************
315800***ÝCM-P3¨ STANDARDIZED CAPITAL COST CALCULATION
315900
316000     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
316100       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
316200        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
316300               (H-CAPI-GAF * H-CAPI-COLA)
316400     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
316500
316600***********************************************************
316700***ÝCM-P3¨ STANDARDIZED OPERATING TRESHOLD
316800
316900     MOVE 5961.40 TO H-OPER-BASE.
317000
317100     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
317200      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
317300                        +
317400           (H-OPER-BASE * H-DRG-WT-FRCTN)
317500                        +
317600              H-NEW-TECH-PAY-ADD-ON.
317700
317800******************************************************
317900***ÝCM-P3¨ STANDARDIZED CAPITAL TRESHOLD
318000
318100     MOVE 466.21 TO H-CAPI-BASE.
318200
318300     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
318400     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
318500                     +
318600     (H-CAPI-BASE * H-DRG-WT-FRCTN).
318700
318800******************************************************
318900***ÝCM-P3¨ STANDARDIZED OPERATING OUTLIER CALCULATION
319000
319100     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
319200        (H-OPER-STDZ-DOLLAR-THRESHOLD +
319300                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
319400                          AND
319500         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
319600
319700       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
319800        (H-CSTOUT-PCT  *
319900        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
320000
320100     ELSE
320200       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
320300
320400******************************************************
320500***ÝCM-P3¨ STANDARDIZED CAPITAL OUTLIER CALCULATION
320600
320700     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
320800        (H-OPER-STDZ-DOLLAR-THRESHOLD +
320900                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
321000                          AND
321100         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
321200
321300      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
321400      (H-CSTOUT-PCT  *
321500      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
321600     ELSE
321700      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
321800
321900*******************************************************
322000***ÝCM-P3¨ STANDARDIZED ALLOWED AMOUNT CALCULATION
322100
322200      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
322300       (H-OPER-BASE + H-CAPI-BASE)
322400                 *
322500       H-DRG-WT-FRCTN
322600                 +
322700       H-OPER-STDZ-COST-OUTLIER
322800                 +
322900       H-CAPI-STDZ-COST-OUTLIER
323000                 +
323100       H-NEW-TECH-PAY-ADD-ON.
323200
323300 9010-EXIT.    EXIT.
323400
323500************************************************************************
323600 10000-COVID19-DIAG-FLAG.
323700************************************************************************
323800
323900     MOVE B-DIAGNOSIS-CODE(IDX-COVID-DIAG) TO WK-DIAG-COVID19.
324000
324100     IF DIAG-COVID2
324200       MOVE 'Y' TO DIAG-COVID2-FLAG.
324300
324400 10000-EXIT.    EXIT.
324500
324600************************************************************************
324700 10050-COVID19-PROC-FLAG.
324800************************************************************************
324900
325000     MOVE B-PROCEDURE-CODE(IDX-COVID-PROC) TO WK-PROC-COVID19.
325100
325200     IF PROC-COVID1
325300       MOVE 'Y' TO PROC-COVID1-FLAG.
325400
325500     IF PROC-COVID2
325600       MOVE 'Y' TO PROC-COVID2-FLAG.
325700
325800     IF PROC-COVID3
325900       MOVE 'Y' TO PROC-COVID3-FLAG.
326000
326100 10050-EXIT.    EXIT.
326200
326300************************************************************************
326400 10100-COVID19-COND-FLAG.
326500************************************************************************
326600
326700     MOVE B-CONDITION-CODE(IDX-COVID-COND) TO WK-COND-COVID19.
326800
326900     IF COND-COVID19-NOADJ
327000       MOVE 'Y' TO COND-COVID1-FLAG.
327100
327200 10100-EXIT.    EXIT.
327300
327400************************************************************************
327500 10200-CLIN-FLAG.
327600************************************************************************
327700
327800     IF IDX-CLIN = 1
327900       GO TO 10200-EXIT.
328000
328100     MOVE B-DIAGNOSIS-CODE(IDX-CLIN) TO WK-DIAG-CLIN.
328200
328300     IF DIAG-CLIN
328400       MOVE 'Y' TO DIAG-CLIN-FLAG.
328500
328600 10200-EXIT.    EXIT.
328700
328800************************************************************************
328900 10300-CART-FLAG.
329000************************************************************************
329100
329200     MOVE B-CONDITION-CODE(IDX-CART) TO WK-COND-CART.
329300
329400     IF COND-CART-NCP
329500       MOVE 'Y' TO COND-CART-NCP-FLAG.
329600
329700     IF COND-CART-NONCP
329800       MOVE 'Y' TO COND-CART-NONCP-FLAG.
329900
330000 10300-EXIT.    EXIT.
330100                                                                  ******
330200************************************************************************
330300 10400-NCTAP-ADD-ON.
330400************************************************************************
330500
330600     MOVE 0 TO H-LESSER-STOP-1
330700               H-LESSER-STOP-2.
330800
330900     COMPUTE H-LESSER-STOP-1 ROUNDED =
331000             H-OPER-DOLLAR-THRESHOLD * 0.65.
331100
331200     COMPUTE H-LESSER-STOP-2 ROUNDED =
331300            (H-OPER-BILL-COSTS - (H-OPER-COST-OUTLIER -
331400             H-OPER-DOLLAR-THRESHOLD)) * 0.65.
331500
331600     IF H-OPER-BILL-COSTS >
331700       (H-OPER-COST-OUTLIER - H-OPER-DOLLAR-THRESHOLD)
331800        IF H-LESSER-STOP-1 < H-LESSER-STOP-2
331900           MOVE H-LESSER-STOP-1 TO NCTAP-ADD-ON
332000        ELSE
332100           MOVE H-LESSER-STOP-2 TO NCTAP-ADD-ON
332200     ELSE
332300        MOVE ZEROES TO NCTAP-ADD-ON.
332400
332500     MOVE 0 TO H-LESSER-STOP-1
332600               H-LESSER-STOP-2.
332700
332800 10400-EXIT.    EXIT.
