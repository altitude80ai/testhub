000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL204.
000300*REVISED.                   09-10-2020.
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
002000     'PPCAL204      - W O R K I N G   S T O R A G E'.
002100 01  CAL-VERSION                    PIC X(05)  VALUE 'C20.4'.
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
005100 COPY RATEX200.
005200
005300*---------------------------------------------------------*
005400* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE (EFF. FY'19) *
005500*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE                 *
005600*---------------------------------------------------------*
005700
005800 COPY DRGSX200.
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
007000 COPY NTECH200.
007100
007200*-----------------------------------------------------*
007300* COVID-19 DRG ADJUSTMENT ELIGIBILITY VARIABLES       *
007400*-----------------------------------------------------*
007500
007600 01  IDX-COVID                      PIC 9(02).
007700 01  IDX-COVID-COND                 PIC 9(02).
007800 01  WK-COVID19-VARIABLES.
007900     05  WK-DIAG-COVID19            PIC X(07).
008000         88  DIAG-COVID1
008100               VALUE 'B9729  '.
008200         88  DIAG-COVID2
008300               VALUE 'U071   '.
008400     05  WK-COND-COVID19            PIC X(02).
008500         88  COND-COVID19-NOADJ
008600               VALUE 'ZA'.
008700     05  WK-COVID19-FLAGS.
008800         10  DIAG-COVID1-FLAG       PIC X(01).
008900         10  DIAG-COVID2-FLAG       PIC X(01).
009000         10  COND-COVID1-FLAG       PIC X(01).
009100 01  COVID-ADJ                      PIC 9(01)V9(01).
009200
009300***********************************************************
009400***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
009500***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
009600***********************************************************
009700
009800 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
009900 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
010000 01  MES-PPS-PROV                   PIC X(06).
010100 01  MES-PPS-STATE                  PIC X(02).
010200 01  MES-INTRO                      PIC X(53) VALUE SPACES.
010300 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
010400 01  MES-SSRFBN.
010500     05 MES-SSRFBN-STATE PIC 99.
010600     05 FILLER           PIC XX.
010700     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
010800     05 FILLER           PIC XX.
010900     05 MES-SSRFBN-CODE2 PIC 99.
011000     05 FILLER           PIC XX.
011100     05 MES-SSRFBN-STNAM PIC X(20).
011200     05 MES-SSRFBN-REST  PIC X(22).
011300
011400 01 WK-HLDDRG-DATA.
011500     05  HLDDRG-DATA.
011600         10  HLDDRG-DRGX               PIC X(03).
011700         10  FILLER1                   PIC X(01).
011800         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
011900         10  FILLER2                   PIC X(01).
012000         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
012100         10  FILLER3                   PIC X(05).
012200         10  HLDDRG-LOW                PIC X(01).
012300         10  FILLER5                   PIC X(01).
012400         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
012500         10  FILLER6                   PIC X(02).
012600         10  HLDDRG-PAC                PIC X(01).
012700         10  FILLER7                   PIC X(01).
012800         10  HLDDRG-SPPAC              PIC X(01).
012900         10  FILLER8                   PIC X(02).
013000         10  HLDDRG-DESC               PIC X(26).
013100
013200 01 WK-HLDDRG-DATA2.
013300     05  HLDDRG-DATA2.
013400         10  HLDDRG-DRGX2               PIC X(03).
013500         10  FILLER21                   PIC X(01).
013600         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
013700         10  FILLER22                   PIC X(01).
013800         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
013900         10  FILLER23                   PIC X(05).
014000         10  HLDDRG-LOW2                PIC X(01).
014100         10  FILLER25                   PIC X(01).
014200         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
014300         10  FILLER26                   PIC X(02).
014400         10  HLDDRG-TRANS-FLAGS.
014500                   88  D-DRG-POSTACUTE-50-50
014600                   VALUE 'Y Y'.
014700                   88  D-DRG-POSTACUTE-PERDIEM
014800                   VALUE 'Y  '.
014900             15  HLDDRG-PAC2            PIC X(01).
015000             15  FILLER27               PIC X(01).
015100             15  HLDDRG-SPPAC2          PIC X(01).
015200         10  FILLER28                   PIC X(02).
015300         10  HLDDRG-DESC2               PIC X(26).
015400         10  HLDDRG-VALID               PIC X(01).
015500
015600 01  MES-LOWVOL.
015700     05  MES-LOWVOL-PROV             PIC X(6).
015800     05  FILLER                      PIC XXX.
015900     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
016000
016100 01  WK-UNCOMP-CARE.
016200     05  WK-UNCOMP-CARE-PROV         PIC X(6).
016300     05  FILLER                      PIC X.
016400     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
016500
016600 01 WK-HLD-MID-DATA.
016700     05  HLD-MID-DATA.
016800         10  HLD-MID-MSAX              PIC X(04).
016900         10  FILLER1                   PIC X(01).
017000         10  HLD-MID-ADJ-FACT          PIC 9(02)V9(06).
017100
017200 01  HLD-PPS-DATA.
017300         10  HLD-PPS-RTC                PIC 9(02).
017400         10  HLD-PPS-WAGE-INDX          PIC 9(02)V9(04).
017500         10  HLD-PPS-OUTLIER-DAYS       PIC 9(03).
017600         10  HLD-PPS-AVG-LOS            PIC 9(02)V9(01).
017700         10  HLD-PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
017800         10  HLD-PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
017900         10  HLD-PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
018000         10  HLD-PPS-OPER-HSP-PART      PIC 9(06)V9(02).
018100         10  HLD-PPS-OPER-FSP-PART      PIC 9(06)V9(02).
018200         10  HLD-PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
018300         10  HLD-PPS-REG-DAYS-USED      PIC 9(03).
018400         10  HLD-PPS-LTR-DAYS-USED      PIC 9(02).
018500         10  HLD-PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
018600         10  HLD-PPS-CALC-VERS          PIC X(05).
018700
018800 LINKAGE SECTION.
018900
019000************************************************************************
019100* REVIEW CODES DIRECT THE PPCAL SUBROUTINE IN HOW TO PAY THE BILL.     *
019200*                                                                      *
019300* COMMENTS:                                                            *
019400* CLAIMS WITH CONDITION CODE 66 SHOULD BE PROCESSED UNDER REVIEW CODE  *
019500* 06, 07, OR 11 AS APPROPRIATE TO EXCLUDE ANY OUTLIER COMPUTATION.     *
019600*                                                                      *
019700* REVIEW-CODE:                                                         *
019800*   00: PAY-WITH-OUTLIER.                                              *
019900*    + WILL CALCULATE THE STANDARD PAYMENT.                            *
020000*    + WILL ALSO ATTEMPT TO PAY ONLY COST OUTLIERS;                    *
020100*      DAY OUTLIERS EXPIRED 10/01/97                                   *
020200*                                                                      *
020300*   03: PAY-PERDIEM-DAYS.                                              *
020400*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
020500*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
020600*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
020700*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
020800*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
020900*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
021000*                                                                      *
021100*   06: PAY-XFER-NO-COST                                               *
021200*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD PAYMENT  *
021300*      IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
021400*      FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE LENGTH *
021500*      OF STAY, THE STANDARD PAYMENT IS CALCULATED.                    *
021600*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
021700*                                                                      *
021800*   07: PAY-WITHOUT-COST.                                              *
021900*    + WILL CALCULATE THE STANDARD PAYMENT WITHOUT THE COST PORTION.   *
022000*                                                                      *
022100*   09: PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS                       *
022200*    + 50-50                                                           *
022300*      - NOW USES Y INDICATORS ON DRGS                                 *
022400*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
022500*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
022600*    + FULL PERDIEM                                                    *
022700*      - NOW USES Y INDICATORS ON DRGS                                 *
022800*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
022900*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
023000*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
023100*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
023200*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
023300*    + WILL ALSO CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT IF  *
023400*      THE ADJUSTED CHARGES ON THE BILL EXCEED THE COST THRESHOLD.     *
023500*                                                                      *
023600*   11: PAY-XFER-SPEC-DRG-NO-COST - POST-ACUTE TRANSFERS               *
023700*    + 50-50                                                           *
023800*      - NOW USES Y INDICATORS ON DRGS                                 *
023900*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
024000*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRGS      *
024100*    + FULL PERDIEM                                                    *
024200*      - NOW USES Y INDICATORS ON DRGS                                 *
024300*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
024400*    + WILL CALCULATE A PERDIEM PAYMENT BASED ON THE STANDARD DRG      *
024500*      PAYMENT IF THE COVERED DAYS ARE LESS THAN THE AVERAGE LENGTH OF *
024600*      STAY FOR THE DRG. IF COVERED DAYS EQUAL OR EXCEED THE AVERAGE   *
024700*      LENGTH OF STAY, THE STANDARD PAYMENT IS CALCULATED.             *
024800*    + WILL NOT CALCULATE THE COST OUTLIER PORTION OF THE PAYMENT.     *
024900************************************************************************
025000
025100************************************************************************
025200* NEW BILL FORMAT (MILLINNIUM COMPATIBLE)                              *
025300*                                                                      *
025400* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL001 PROGRAM  *
025500* AND AFTER FOR PROCESSING IN THE NEW FORMAT.                          *
025600*                                                                      *
025700* B-CHARGES-CLAIMED = TOTAL COVERED CHARGES ON THE 0001 (TOTALS        *
025800* LINE) MINUS BLOOD CLOT COST, KIDNEY COSTS, ACQUISITION COSTS AND     *
025900* TECHNICAL PROVIDER CHARGES.                                          *
026000************************************************************************
026100 01  BILL-NEW-DATA.
026200         10  B-NPI10.
026300             15  B-NPI8             PIC X(08).
026400             15  B-NPI-FILLER       PIC X(02).
026500         10  B-PROVIDER-NO          PIC X(06).
026600             88  B-FORMER-MDH-PROVIDERS
026700                                      VALUE '080006' '140184'
026800                                            '390072' '420019'
026900                                            '440031' '450451'
027000                                            '490019' '510062'.
027100         10  B-REVIEW-CODE          PIC 9(02).
027200             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
027300             88  PAY-WITH-OUTLIER     VALUE 00 07.
027400             88  PAY-PERDIEM-DAYS     VALUE 03.
027500             88  PAY-XFER-NO-COST     VALUE 06.
027600             88  PAY-WITHOUT-COST     VALUE 07.
027700             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
027800             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
027900         10  B-DRG                  PIC 9(03).
028000
028100* ======================================================
028200* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRG'S
028300* ======================================================
028400*
028500*            88  B-DRG-POSTACUTE-PERDIEM
028600*                         VALUE  NOW USES Y INDICATORS ON DRGS
028700*                         SEE TABLE 5
028800*                         D-DRG-POSTACUTE-PERDIEM
028900
029000         10  B-LOS                  PIC 9(03).
029100         10  B-COVERED-DAYS         PIC 9(03).
029200         10  B-LTR-DAYS             PIC 9(02).
029300         10  B-DISCHARGE-DATE.
029400             15  B-DISCHG-CC        PIC 9(02).
029500             15  B-DISCHG-YY        PIC 9(02).
029600             15  B-DISCHG-MM        PIC 9(02).
029700             15  B-DISCHG-DD        PIC 9(02).
029800         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
029900         10  B-PROCEDURE-CODE-TABLE.
030000             15  B-PROCEDURE-CODE    PIC X(07) OCCURS 25 TIMES
030100                 INDEXED BY IDX-PROC.
030200         10  B-DIAGNOSIS-CODE-TABLE.
030300             15  B-DIAGNOSIS-CODE    PIC X(07) OCCURS 25 TIMES
030400                 INDEXED BY IDX-DIAG.
030500         10  B-DEMO-DATA.
030600             15  B-DEMO-CODE1           PIC X(02).
030700             15  B-DEMO-CODE2           PIC X(02).
030800             15  B-DEMO-CODE3           PIC X(02).
030900             15  B-DEMO-CODE4           PIC X(02).
031000         10  B-NDC-DATA.
031100             15  B-NDC-NUMBER           PIC X(11).
031200         10  B-CONDITION-CODE-TABLE.
031300             15  B-CONDITION-CODE    PIC X(02) OCCURS 5 TIMES
031400                 INDEXED BY IDX-COND.
031500         10  FILLER                     PIC X(63).
031600
031700************************************************************************
031800* RETURN CODES (PPS-RTC) NOTE HOW THE BILL WAS/WAS NOT PAID.           *
031900*   00-49: HOW THE BILL WAS PAID                                       *
032000*   50-99: WHY THE BILL WAS NOT PAID                                   *
032100*  ----------------------------------------------------------          *
032200*   00,30:                                                             *
032300*    + PAID NORMAL DRG PAYMENT                                         *
032400*                                                                      *
032500*   01:                                                                *
032600*    + PAID AS A DAY-OUTLIER.                                          *
032700*      - DAY-OUTLIER NO LONGER BEING PAID AS OF 10/01/97               *
032800*                                                                      *
032900*   02:                                                                *
033000*    + PAID AS A COST-OUTLIER.                                         *
033100*                                                                      *
033200*   03,33:                                                             *
033300*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
033400*                                                                      *
033500*   05:                                                                *
033600*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
033700*    + QUALIFIED FOR A COST OUTLIER PAYMENT                            *
033800*                                                                      *
033900*   06:                                                                *
034000*    + TRANSFER PAID ON PERDIEM BASIS UP TO AND INCLUDING THE FULL DRG *
034100*    + PROVIDER REFUSED COST OUTLIER PAYMENT                           *
034200*                                                                      *
034300*   10,40:                                                             *
034400*    + POST-ACUTE TRANSFER                                             *
034500*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
034600*      - THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE DRGS         *
034700*                                                                      *
034800*   12,42:                                                             *
034900*    + POST-ACAUTE TRANSFER WITH SPECIFIC DRGS                         *
035000*      - NOW USES Y INDICATORS ON DRGS                                 *
035100*      - SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE                       *
035200*      - D-DRG-POSTACUTE-PERDIEM                                       *
035300*                                                                      *
035400*   14,44:                                                             *
035500*    + PAID NORMAL DRG PAYMENT WITH PERDIEM DAYS = OR > GM ALOS        *
035600*                                                                      *
035700*   16:                                                                *
035800*    + PAID AS A COST-OUTLIER WITH PERDIEM DAYS = OR > GM ALOS         *
035900*                                                                      *
036000*   30,33,40,42,44:                                                    *
036100*    + OUTLIER RECONCILIATION                                          *
036200*                                                                      *
036300*   51:                                                                *
036400*    + NO PROVIDER SPECIFIC INFO FOUND                                 *
036500*                                                                      *
036600*   52:                                                                *
036700*    + INVALID CBSA# IN PROVIDER FILE OR                               *
036800*    + INVALID WAGE INDEX OR                                           *
036900*    + INVALID PROVIDER TYPES ON PROVIDER FILE                         *
037000*                                                                      *
037100*   53:                                                                *
037200*    + WAIVER STATE - NOT CALCULATED BY PPS OR                         *
037300*    + INVALID STATE CODE IN COMBINATION WITH HAC FLAG                 *
037400*                                                                      *
037500*   54:                                                                *
037600*    + INVALID DRG                                                     *
037700*                                                                      *
037800*   55:                                                                *
037900*    + DISCHARGE DATE < PROVIDER EFF START DATE OR                     *
038000*    + DISCHARGE DATE < CBSA EFF START DATE FOR PPS OR                 *
038100*    + PROVIDER HAS BEEN TERMINATED ON OR BEFORE DISCHARGE DATE        *
038200*                                                                      *
038300*   56:                                                                *
038400*    + INVALID LENGTH OF STAY                                          *
038500*                                                                      *
038600*   57:                                                                *
038700*    + REVIEW CODE INVALID (NOT 00 03 06 07 09 11)                     *
038800*                                                                      *
038900*   58:                                                                *
039000*    + TOTAL CHARGES NOT NUMERIC                                       *
039100*                                                                      *
039200*   61:                                                                *
039300*    + LIFETIME RESERVE DAYS NOT NUMERIC OR BILL-LTR-DAYS > 60         *
039400*                                                                      *
039500*   62:                                                                *
039600*    + INVALID NUMBER OF COVERED DAYS                                  *
039700*                                                                      *
039800*   65:                                                                *
039900*    + PAY-CODE NOT = A, B OR C ON PSF FOR CAPITAL OR                  *
040000*    + INVALID READMISSION FLAG IN PSF FILE OR                         *
040100*    + BLANK READMISSION FLAG IN PSF FILE OR                           *
040200*    + READMISSION ADJUSTMENT IS INVALID / OUT OF RANGE IN PSF FILE OR *
040300*    + BLANK READMISSION ADJUSTMENT IN PSF FILE OR                     *
040400*    + INVALID STATE CODE IN COMBO W/ READMISSION FLAG IN PSF FILE OR  *
040500*    + INVALID EHR FLAG IN PSF FILE (MUST BE A "Y" OR BLANK)           *
040600*                                                                      *
040700*   67:                                                                *
040800*    + COST OUTLIER WITH LOS > COVERED DAYS OR                         *
040900*      COST OUTLIER THRESHOLD CALUCULATION                             *
041000*                                                                      *
041100*   68:                                                                *
041200*    + INVALID VALUE BASED PURCHASE FLAG IN PSF FILE OR                *
041300*    + BLANK VALUE BASED PURCHASE FLAG IN PSF FILE OR                  *
041400*    + VALUE BASED PURCHASE ADJUSTMEMT IS INVALID OR OUT OF RANGE IN   *
041500*      PSF FILE INDICATOR OR                                           *
041600*    + BLANK VALUE BASED PURCHASE ADJUSTMEMT IN PSF FILE OR            *
041700*    + INVALID COMBINATION OF HOSPITAL QUALITY INDICATOR AND VALUE     *
041800*      BASED PURCHASE FLAG IN PSF FILE OR                              *
041900*    + INVALID STATE CODE IN COMBINATION WITH VALUE BASED PURCHASE     *
042000*      FLAG IN PSF FILE                                                *
042100*                                                                      *
042200*   98: CANNOT PROCESS BILL OLDER THAN 5 YEARS                         *
042300************************************************************************
042400 01  PPS-DATA.
042500         10  PPS-RTC                PIC 9(02).
042600         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
042700         10  PPS-OUTLIER-DAYS       PIC 9(03).
042800         10  PPS-AVG-LOS            PIC 9(02)V9(01).
042900         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
043000         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
043100         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
043200         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
043300         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
043400         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
043500         10  PPS-REG-DAYS-USED      PIC 9(03).
043600         10  PPS-LTR-DAYS-USED      PIC 9(02).
043700         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
043800         10  PPS-CALC-VERS          PIC X(05).
043900
044000*****************************************************************
044100*            THESE ARE THE VERSIONS OF THE PPCAL
044200*           PROGRAMS THAT WILL BE PASSED BACK----
044300*          ASSOCIATED WITH THE BILL BEING PROCESSED
044400*****************************************************************
044500 01  PRICER-OPT-VERS-SW.
044600     02  PRICER-OPTION-SW          PIC X(01).
044700         88  ALL-TABLES-PASSED          VALUE 'A'.
044800         88  PROV-RECORD-PASSED         VALUE 'P'.
044900         88  ADDITIONAL-VARIABLES       VALUE 'M'.
045000         88  PC-PRICER                  VALUE 'C'.
045100     02  PPS-VERSIONS.
045200         10  PPDRV-VERSION         PIC X(05).
045300
045400*****************************************************************
045500*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
045600*          ASSOCIATED WITH THE BILL BEING PROCESSED
045700*****************************************************************
045800 01  PPS-ADDITIONAL-VARIABLES.
045900     05  PPS-HSP-PCT                PIC 9(01)V9(02).
046000     05  PPS-FSP-PCT                PIC 9(01)V9(02).
046100     05  PPS-NAT-PCT                PIC 9(01)V9(02).
046200     05  PPS-REG-PCT                PIC 9(01)V9(02).
046300     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
046400     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
046500     05  PPS-DRG-WT                 PIC 9(02)V9(04).
046600     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
046700     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
046800     05  PPS-REG-LABOR              PIC 9(05)V9(02).
046900     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
047000     05  PPS-OPER-COLA              PIC 9(01)V9(03).
047100     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
047200     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
047300     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
047400     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
047500     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
047600     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
047700     05  PPS-CAPITAL-VARIABLES.
047800         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
047900         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
048000         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
048100         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
048200         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
048300         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
048400         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
048500         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
048600     05  PPS-CAPITAL2-VARIABLES.
048700         10  PPS-CAPI2-PAY-CODE             PIC X(1).
048800         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
048900         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
049000     05  PPS-OTHER-VARIABLES.
049100         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
049200         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
049300         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
049400         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
049500         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
049600         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
049700         10  PPS-HOSP-READMISSION-REDU      PIC X.
049800         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
049900         10  PPS-OPERATNG-DATA.
050000             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
050100             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
050200             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
050300     05  PPS-PC-OTH-VARIABLES.
050400         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
050500         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
050600         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
050700         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
050800         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
050900         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
051000         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
051100         10  PPS-PC-HMO-FLAG                PIC X(01).
051200         10  PPS-PC-COT-FLAG                PIC X(01).
051300         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
051400         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
051500     05  PPS-ADDITIONAL-PAY-INFO-DATA.
051600         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
051700         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
051800         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
051900         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
052000     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
052100         10  PPS-HAC-PROG-REDUC-IND      PIC X.
052200         10  PPS-EHR-PROG-REDUC-IND      PIC X.
052300         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
052400         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
052500         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
052600         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
052700     05 PPS-FILLER                       PIC X(0897).
052800
052900 01  PROV-NEW-HOLD.
053000     02  PROV-NEWREC-HOLD1.
053100         05  P-NEW-NPI10.
053200             10  P-NEW-NPI8             PIC X(08).
053300             10  P-NEW-NPI-FILLER       PIC X(02).
053400         05  P-NEW-PROVIDER-NO.
053500             88  P-NEW-DSH-ADJ-PROVIDERS
053600                             VALUE '180049' '190044' '190144'
053700                                   '190191' '330047' '340085'
053800                                   '370016' '370149' '420043'.
053900             10  P-NEW-STATE            PIC X(02).
054000                 88  P-VBP-INVALID-STATE
054100                             VALUE '21' '80' '40' '84'.
054200                 88  P-READ-INVALID-STATE
054300                             VALUE '40' '84'.
054400                 88  P-HAC-INVALID-STATE
054500                             VALUE '40' '84'.
054600                 88  P-PR-NEW-STATE
054700                             VALUE '40' '84'.
054800             10  FILLER                 PIC X(04).
054900         05  P-NEW-DATE-DATA.
055000             10  P-NEW-EFF-DATE.
055100                 15  P-NEW-EFF-DT-CC    PIC 9(02).
055200                 15  P-NEW-EFF-DT-YY    PIC 9(02).
055300                 15  P-NEW-EFF-DT-MM    PIC 9(02).
055400                 15  P-NEW-EFF-DT-DD    PIC 9(02).
055500             10  P-NEW-FY-BEGIN-DATE.
055600                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
055700                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
055800                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
055900                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
056000             10  P-NEW-REPORT-DATE.
056100                 15  P-NEW-REPORT-DT-CC PIC 9(02).
056200                 15  P-NEW-REPORT-DT-YY PIC 9(02).
056300                 15  P-NEW-REPORT-DT-MM PIC 9(02).
056400                 15  P-NEW-REPORT-DT-DD PIC 9(02).
056500             10  P-NEW-TERMINATION-DATE.
056600                 15  P-NEW-TERM-DT-CC   PIC 9(02).
056700                 15  P-NEW-TERM-DT-YY   PIC 9(02).
056800                 15  P-NEW-TERM-DT-MM   PIC 9(02).
056900                 15  P-NEW-TERM-DT-DD   PIC 9(02).
057000         05  P-NEW-WAIVER-CODE          PIC X(01).
057100             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
057200         05  P-NEW-INTER-NO             PIC 9(05).
057300         05  P-NEW-PROVIDER-TYPE        PIC X(02).
057400             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
057500             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
057600                                                  '15' '17'
057700                                                  '22'.
057800             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
057900             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
058000             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
058100             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
058200             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
058300             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
058400             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
058500             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
058600             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
058700             88  P-N-EACH                   VALUE '21' '22'.
058800             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
058900             88  P-N-NHCMQ-II-SNF           VALUE '32'.
059000             88  P-N-NHCMQ-III-SNF          VALUE '33'.
059100             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
059200         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
059300             88  P-N-NEW-ENGLAND            VALUE  1.
059400             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
059500             88  P-N-SOUTH-ATLANTIC         VALUE  3.
059600             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
059700             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
059800             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
059900             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
060000             88  P-N-MOUNTAIN               VALUE  8.
060100             88  P-N-PACIFIC                VALUE  9.
060200         05  P-NEW-CURRENT-DIV   REDEFINES
060300                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
060400             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
060500         05  P-NEW-MSA-DATA.
060600             10  P-NEW-CHG-CODE-INDEX       PIC X.
060700             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
060800             10  P-NEW-GEO-LOC-MSA9   REDEFINES
060900                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
061000             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
061100             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
061200             10  P-NEW-STAND-AMT-LOC-MSA9
061300       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
061400                 15  P-NEW-RURAL-1ST.
061500                     20  P-NEW-STAND-RURAL  PIC XX.
061600                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
061700                 15  P-NEW-RURAL-2ND        PIC XX.
061800         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
061900                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
062000                 88  P-NEW-SCH-YR82       VALUE   '82'.
062100                 88  P-NEW-SCH-YR87       VALUE   '87'.
062200         05  P-NEW-LUGAR                    PIC X.
062300         05  P-NEW-TEMP-RELIEF-IND          PIC X.
062400         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
062500         05  P-NEW-STATE-CODE               PIC 9(02).
062600         05  P-NEW-STATE-CODE-X REDEFINES
062700             P-NEW-STATE-CODE               PIC X(02).
062800         05  FILLER                         PIC X(03).
062900     02  PROV-NEWREC-HOLD2.
063000         05  P-NEW-VARIABLES.
063100             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
063200             10  P-NEW-COLA              PIC  9(01)V9(03).
063300             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
063400             10  P-NEW-BED-SIZE          PIC  9(05).
063500             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
063600             10  P-NEW-CMI               PIC  9(01)V9(04).
063700             10  P-NEW-SSI-RATIO         PIC  V9(04).
063800             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
063900             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
064000             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
064100             10  P-NEW-DSH-PERCENT       PIC  V9(04).
064200             10  P-NEW-FYE-DATE          PIC  X(08).
064300         05  P-NEW-CBSA-DATA.
064400             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
064500             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
064600             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
064700             10  P-NEW-CBSA-GEO-RURAL REDEFINES
064800                 P-NEW-CBSA-GEO-LOC.
064900                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
065000                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
065100                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
065200
065300             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
065400             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
065500             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
065600     02  PROV-NEWREC-HOLD3.
065700         05  P-NEW-PASS-AMT-DATA.
065800             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
065900             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
066000             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
066100             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
066200         05  P-NEW-CAPI-DATA.
066300             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
066400             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
066500             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
066600             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
066700             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
066800             15  P-NEW-CAPI-NEW-HOSP       PIC X.
066900             15  P-NEW-CAPI-IME            PIC 9V9999.
067000             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
067100         05  P-HVBP-HRR-DATA.
067200             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
067300             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
067400             15  P-HOSP-READMISSION-REDU    PIC X.
067500             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
067600         05  P-MODEL1-BUNDLE-DATA.
067700             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
067800             15  P-HAC-REDUC-IND            PIC X.
067900             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
068000             15  P-EHR-REDUC-IND            PIC X.
068100             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
068200         05  P-NEW-COUNTY-CODE              PIC 9(05).
068300         05  P-NEW-COUNTY-CODE-X REDEFINES
068400             P-NEW-COUNTY-CODE              PIC X(05).
068500         05  FILLER                         PIC X(47).
068600
068700*****************************************************************
068800 01  WAGE-NEW-CBSA-INDEX-RECORD.
068900     05  W-CBSA                        PIC X(5).
069000     05  W-CBSA-SIZE                   PIC X.
069100         88  LARGE-URBAN       VALUE 'L'.
069200         88  OTHER-URBAN       VALUE 'O'.
069300         88  ALL-RURAL         VALUE 'R'.
069400     05  W-CBSA-EFF-DATE               PIC X(8).
069500     05  FILLER                        PIC X.
069600     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
069700     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
069800
069900*******************************************************
070000*    HOLD VARIABLES POPULATED IN PPCAL___***          *
070100*******************************************************
070200 COPY PPHOLDAR.
070300
070400******************************************************************
070500 PROCEDURE DIVISION  USING BILL-NEW-DATA
070600                           PPS-DATA
070700                           PRICER-OPT-VERS-SW
070800                           PPS-ADDITIONAL-VARIABLES
070900                           PROV-NEW-HOLD
071000                           WAGE-NEW-CBSA-INDEX-RECORD
071100                           PPHOLDAR-HOLD-AREA.
071200
071300***************************************************************
071400*    PROCESSING:                                              *
071500*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE
071600*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
071700*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
071800*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
071900*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
072000*           GOBACK.                                           *
072100*        D. ASSEMBLE PRICING COMPONENTS.                      *
072200*        E. CALCULATE THE PRICE.                              *
072300***************************************************************
072400     INITIALIZE WK-HLDDRG-DATA
072500                WK-HLDDRG-DATA2
072600                WK-HLD-MID-DATA
072700                WK-NEW-TECH-VARIABLES
072800                WK-COVID19-VARIABLES.
072900
073000     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
073100     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
073200     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
073300     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
073400     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
073500     MOVE 'N' TO TEMP-RELIEF-FLAG.
073600     MOVE 'N' TO OUTLIER-RECON-FLAG.
073700     MOVE ZEROES TO WK-HAC-AMOUNT.
073800     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
073900     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
074000     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
074100     MOVE ZEROES TO PPS-ISLET-ISOL-PAY-ADD-ON.
074200
074300     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
074400
074500     COMPUTE H-DRG-WT ROUNDED = H-DRG-WT * COVID-ADJ.
074600
074700     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DRG-WT-FRCTN * COVID-ADJ.
074800
074900     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
075000     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
075100     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
075200     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
075300     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
075400     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
075500     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
075600     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
075700     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
075800                            PPS-ADDITIONAL-PAY-INFO-DATA.
075900     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
076000                            PPS-ADDITIONAL-PAY-INFO-DATA2.
076100
076200     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
076300     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
076400     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
076500     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
076600                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
076700     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
076800     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
076900                               PPS-MODEL1-BUNDLE-DISPRCNT.
077000
077100     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
077200     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
077300     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
077400*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
077500     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
077600     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
077700     MOVE 0     TO    PPS-FLX7-PAYMENT.
077800
077900     IF (PPS-RTC = '00' OR '03' OR '10' OR
078000                   '12' OR '14')
078100        MOVE 'Y' TO OUTLIER-RECON-FLAG
078200        MOVE PPS-DATA TO HLD-PPS-DATA
078300        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
078400        MOVE HLD-PPS-DATA TO PPS-DATA.
078500
078600     IF  PPS-RTC < 50
078700         IF  P-NEW-WAIVER-STATE
078800             MOVE 53 TO PPS-RTC
078900             MOVE ALL '0' TO PPS-OPER-HSP-PART
079000                             PPS-OPER-FSP-PART
079100                             PPS-OPER-OUTLIER-PART
079200                             PPS-OUTLIER-DAYS
079300                             PPS-REG-DAYS-USED
079400                             PPS-LTR-DAYS-USED
079500                             PPS-TOTAL-PAYMENT
079600                             WK-HAC-TOTAL-PAYMENT
079700                             PPS-OPER-DSH-ADJ
079800                             PPS-OPER-IME-ADJ
079900                             H-DSCHG-FRCTN
080000                             H-DRG-WT-FRCTN
080100                             HOLD-ADDITIONAL-VARIABLES
080200                             HOLD-CAPITAL-VARIABLES
080300                             HOLD-CAPITAL2-VARIABLES
080400                             HOLD-OTHER-VARIABLES
080500                             HOLD-PC-OTH-VARIABLES
080600                             H-ADDITIONAL-PAY-INFO-DATA
080700                             H-ADDITIONAL-PAY-INFO-DATA2.
080800     GOBACK.
080900
081000 0200-MAINLINE-CONTROL.
081100
081200     MOVE 'N' TO HMO-TAG.
081300
081400     IF PPS-PC-HMO-FLAG = 'Y' OR
081500               HMO-FLAG = 'Y'
081600        MOVE 'Y' TO HMO-TAG.
081700
081800     MOVE ALL '0' TO PPS-DATA
081900                     H-OPER-DSH-SCH
082000                     H-OPER-DSH-RRC
082100                     HOLD-PPS-COMPONENTS
082200                     HOLD-PPS-COMPONENTS
082300                     HOLD-ADDITIONAL-VARIABLES
082400                     HOLD-CAPITAL-VARIABLES
082500                     HOLD-CAPITAL2-VARIABLES
082600                     HOLD-OTHER-VARIABLES
082700                     HOLD-PC-OTH-VARIABLES
082800                     H-ADDITIONAL-PAY-INFO-DATA
082900                     H-ADDITIONAL-PAY-INFO-DATA2
083000                     H-EHR-SUBSAV-QUANT
083100                     H-EHR-SUBSAV-LV
083200                     H-EHR-SUBSAV-QUANT-INCLV
083300                     H-EHR-RESTORE-FULL-QUANT
083400                     H-OPER-BILL-STDZ-COSTS
083500                     H-CAPI-BILL-STDZ-COSTS
083600                     H-OPER-STDZ-COST-OUTLIER
083700                     H-CAPI-STDZ-COST-OUTLIER
083800                     H-OPER-STDZ-DOLLAR-THRESHOLD
083900                     H-CAPI-STDZ-DOLLAR-THRESHOLD
084000                     WK-LOW-VOL-ADDON
084100                     WK-HAC-AMOUNT
084200                     WK-HAC-TOTAL-PAYMENT.
084300
084400     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
084500        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
084600
084700     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
084800        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
084900
085000     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
085100        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
085200
085300     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
085400        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
085500
085600     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
085700        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
085800
085900     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
086000        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
086100
086200     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
086300        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
086400
086500     PERFORM 1000-EDIT-THE-BILL-INFO.
086600
086700     IF  PPS-RTC = 00
086800         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
086900
087000     IF  PPS-RTC = 00
087100         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
087200
087300     IF OUTLIER-RECON-FLAG = 'Y'
087400        MOVE 'N' TO OUTLIER-RECON-FLAG
087500        GO TO 0200-EXIT.
087600
087700     IF PPS-RTC = 00
087800        IF H-PERDIEM-DAYS = H-ALOS OR
087900           H-PERDIEM-DAYS > H-ALOS
088000           MOVE 14 TO PPS-RTC.
088100
088200     IF PPS-RTC = 02
088300        IF H-PERDIEM-DAYS = H-ALOS OR
088400           H-PERDIEM-DAYS > H-ALOS
088500           MOVE 16 TO PPS-RTC.
088600
088700 0200-EXIT.   EXIT.
088800
088900 1000-EDIT-THE-BILL-INFO.
089000
089100     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
089200     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
089300
089400**   IF  PPS-RTC = 00
089500*        IF  P-NEW-WAIVER-STATE
089600*            MOVE 53 TO PPS-RTC.
089700
089800     IF  PPS-RTC = 00
089900         IF   HLDDRG-VALID = 'I'
090000             MOVE 54 TO PPS-RTC.
090100
090200     IF  PPS-RTC = 00
090300            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
090400                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
090500                MOVE 55 TO PPS-RTC.
090600
090700     IF  PPS-RTC = 00
090800         IF P-NEW-TERMINATION-DATE > 00000000
090900            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
091000                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
091100                  MOVE 55 TO PPS-RTC.
091200
091300     IF  PPS-RTC = 00
091400         IF  B-LOS NOT NUMERIC
091500             MOVE 56 TO PPS-RTC
091600         ELSE
091700         IF  B-LOS = 0
091800             IF B-REVIEW-CODE NOT = 00 AND
091900                              NOT = 03 AND
092000                              NOT = 06 AND
092100                              NOT = 07 AND
092200                              NOT = 09 AND
092300                              NOT = 11
092400             MOVE 56 TO PPS-RTC.
092500
092600     IF  PPS-RTC = 00
092700         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
092800             MOVE 61 TO PPS-RTC
092900         ELSE
093000             MOVE B-LTR-DAYS TO H-LTR-DAYS.
093100
093200     IF  PPS-RTC = 00
093300         IF  B-COVERED-DAYS NOT NUMERIC
093400             MOVE 62 TO PPS-RTC
093500         ELSE
093600         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
093700             MOVE 62 TO PPS-RTC
093800         ELSE
093900             MOVE B-COVERED-DAYS TO H-COV-DAYS.
094000
094100     IF  PPS-RTC = 00
094200         IF  H-LTR-DAYS  > H-COV-DAYS
094300             MOVE 62 TO PPS-RTC
094400         ELSE
094500             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
094600
094700     IF  PPS-RTC = 00
094800         IF  NOT VALID-REVIEW-CODE
094900             MOVE 57 TO PPS-RTC.
095000
095100     IF  PPS-RTC = 00
095200         IF  B-CHARGES-CLAIMED NOT NUMERIC
095300             MOVE 58 TO PPS-RTC.
095400
095500     IF PPS-RTC = 00
095600           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
095700                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
095800                                            NOT = 'C'
095900                 MOVE 65 TO PPS-RTC.
096000
096100***  MDH PROVISION ENDS 9/30/2018
096200***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
096300
096400     IF PPS-RTC = 00 AND
096500        B-DISCHARGE-DATE > 20220930 AND
096600        P-N-INVALID-PROV-TYPES
096700                 MOVE 52 TO PPS-RTC.
096800
096900 2000-ASSEMBLE-PPS-VARIABLES.
097000***  GET THE PROVIDER SPECIFIC VARIABLES.
097100
097200     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
097300     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
097400
097500     IF (P-NEW-STATE = 02 OR 12)
097600        MOVE P-NEW-COLA TO H-OPER-COLA
097700     ELSE
097800        MOVE 1.000 TO H-OPER-COLA.
097900
098000***************************************************************
098100***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
098200
098300     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
098400
098500     PERFORM 2700-COVID-DRG-ADJ THRU 2700-EXIT.
098600
098700     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
098800
098900     MOVE P-NEW-STATE            TO MES-PPS-STATE.
099000
099100*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
099200** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
099300** FOR FY 2011
099400***************************************************************
099500*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
099600*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
099700***************************************************************
099800***  GET THE WAGE-INDEX
099900
100000     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
100100     MOVE P-NEW-STATE            TO MES-PPS-STATE.
100200
100300***************************************************************
100400* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
100500* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
100600***************************************************************
100700
100800     PERFORM 2050-RATES-TB THRU 2050-EXIT.
100900
101000     IF P-NEW-GEO-LOC-MSA9 >= 9400 AND
101100        P-NEW-GEO-LOC-MSA9 <= 9900
101200        PERFORM 2100-MIDNIGHT-FACTORS THRU 2100-EXIT
101300     ELSE
101400        MOVE 1 TO HLD-MID-ADJ-FACT
101500        GO TO 2000-EXIT.
101600
101700 2000-EXIT.  EXIT.
101800
101900 2050-RATES-TB.
102000     MOVE 1 TO R2
102100     MOVE 1 TO R4.
102200
102300     IF LARGE-URBAN
102400         MOVE 1 TO R3
102500     ELSE
102600         MOVE 2 TO R3.
102700
102800     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
102900        (P-EHR-REDUC-IND = ' ')           AND
103000        (H-WAGE-INDEX > 01.0000))
103100        PERFORM 2300-GET-LAB-NONLAB-TB1-RATES
103200           THRU 2300-GET-LAB-NONLAB-TB1-EXIT
103300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
103400
103500     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
103600        (P-EHR-REDUC-IND = ' ')               AND
103700         (H-WAGE-INDEX > 01.0000))
103800        PERFORM 2300-GET-LAB-NONLAB-TB2-RATES
103900           THRU 2300-GET-LAB-NONLAB-TB2-EXIT
104000             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
104100
104200     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
104300        (P-EHR-REDUC-IND = ' ')            AND
104400         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
104500        PERFORM 2300-GET-LAB-NONLAB-TB3-RATES
104600           THRU 2300-GET-LAB-NONLAB-TB3-EXIT
104700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
104800
104900     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
105000        (P-EHR-REDUC-IND = ' ')               AND
105100         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
105200        PERFORM 2300-GET-LAB-NONLAB-TB4-RATES
105300           THRU 2300-GET-LAB-NONLAB-TB4-EXIT
105400             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
105500
105600     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
105700        (P-EHR-REDUC-IND = 'Y')           AND
105800        (H-WAGE-INDEX > 01.0000))
105900        PERFORM 2300-GET-LAB-NONLAB-TB5-RATES
106000           THRU 2300-GET-LAB-NONLAB-TB5-EXIT
106100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
106200
106300     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
106400        (P-EHR-REDUC-IND = 'Y')               AND
106500         (H-WAGE-INDEX > 01.0000))
106600        PERFORM 2300-GET-LAB-NONLAB-TB6-RATES
106700           THRU 2300-GET-LAB-NONLAB-TB6-EXIT
106800             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
106900
107000     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
107100        (P-EHR-REDUC-IND = 'Y')            AND
107200         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
107300        PERFORM 2300-GET-LAB-NONLAB-TB7-RATES
107400           THRU 2300-GET-LAB-NONLAB-TB7-EXIT
107500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
107600
107700     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
107800        (P-EHR-REDUC-IND = 'Y')               AND
107900         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
108000        PERFORM 2300-GET-LAB-NONLAB-TB8-RATES
108100           THRU 2300-GET-LAB-NONLAB-TB8-EXIT
108200             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
108300
108400***************************************************************
108500* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
108600***************************************************************
108700
108800     MOVE 0.00  TO H-OPER-HSP-PCT.
108900     MOVE 1.00  TO H-OPER-FSP-PCT.
109000
109100***************************************************************
109200*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
109300***************************************************************
109400
109500      MOVE 1.00 TO H-NAT-PCT.
109600      MOVE 0.00 TO H-REG-PCT.
109700
109800     IF  P-N-SCH-REBASED-FY90 OR
109900         P-N-EACH OR
110000         P-N-MDH-REBASED-FY90
110100         MOVE 1.00 TO H-OPER-HSP-PCT.
110200
110300 2050-EXIT.   EXIT.
110400
110500***************************************************************
110600*  APPLY THE TWO MIDNIGHT POLICY ADJUSTMENT FACTORS           *
110700***************************************************************
110800 2100-MIDNIGHT-FACTORS.
110900
111000     INITIALIZE HLD-MID-ADJ-FACT.
111100
111200     SET MID-IDX TO 1.
111300
111400     SEARCH MID-TAB VARYING MID-IDX
111500     WHEN WK-MID-MSAX(MID-IDX) = P-NEW-GEO-LOC-MSA9
111600       MOVE MID-DATA-TAB(MID-IDX) TO HLD-MID-DATA.
111700
111800 2100-EXIT.   EXIT.
111900
112000***************************************************************
112100* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
112200* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
112300***************************************************************
112400 2300-GET-LAB-NONLAB-TB1-RATES.
112500
112600     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
112700         MOVE TB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
112800         MOVE TB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
112900         MOVE TB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
113000         MOVE TB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
113100
113200 2300-GET-LAB-NONLAB-TB1-EXIT.   EXIT.
113300
113400 2300-GET-LAB-NONLAB-TB2-RATES.
113500
113600     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
113700         MOVE TB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
113800         MOVE TB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
113900         MOVE TB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
114000         MOVE TB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
114100
114200 2300-GET-LAB-NONLAB-TB2-EXIT.   EXIT.
114300
114400 2300-GET-LAB-NONLAB-TB3-RATES.
114500
114600     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
114700         MOVE TB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
114800         MOVE TB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
114900         MOVE TB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
115000         MOVE TB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
115100
115200 2300-GET-LAB-NONLAB-TB3-EXIT.   EXIT.
115300
115400 2300-GET-LAB-NONLAB-TB4-RATES.
115500
115600     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
115700         MOVE TB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
115800         MOVE TB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
115900         MOVE TB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
116000         MOVE TB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
116100
116200 2300-GET-LAB-NONLAB-TB4-EXIT.   EXIT.
116300
116400 2300-GET-LAB-NONLAB-TB5-RATES.
116500
116600     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
116700         MOVE TB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
116800         MOVE TB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
116900         MOVE TB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
117000         MOVE TB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
117100
117200 2300-GET-LAB-NONLAB-TB5-EXIT.   EXIT.
117300
117400 2300-GET-LAB-NONLAB-TB6-RATES.
117500
117600     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
117700         MOVE TB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
117800         MOVE TB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
117900         MOVE TB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
118000         MOVE TB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
118100
118200 2300-GET-LAB-NONLAB-TB6-EXIT.   EXIT.
118300
118400 2300-GET-LAB-NONLAB-TB7-RATES.
118500
118600     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
118700         MOVE TB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
118800         MOVE TB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
118900         MOVE TB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
119000         MOVE TB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
119100
119200 2300-GET-LAB-NONLAB-TB7-EXIT.   EXIT.
119300
119400 2300-GET-LAB-NONLAB-TB8-RATES.
119500
119600     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
119700         MOVE TB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
119800         MOVE TB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
119900         MOVE TB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
120000         MOVE TB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
120100
120200 2300-GET-LAB-NONLAB-TB8-EXIT.   EXIT.
120300
120400***************************************************************
120500* OBTAIN THE APPLICABLE DRG WEIGHTS                           *
120600***************************************************************
120700 2600-GET-DRG-WEIGHT.
120800
120900     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
121000     SET DRG-IDX TO 1
121100     SEARCH DRG-TAB VARYING DRG-IDX
121200         AT END
121300           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
121400           MOVE 'I' TO  HLDDRG-VALID
121500           MOVE 0 TO HLDDRG-WEIGHT
121600           MOVE 54 TO PPS-RTC
121700           GO TO 2600-EXIT
121800       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
121900         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
122000
122100     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
122200     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
122300     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
122400                                  H-DRG-WT.
122500     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
122600                                  H-ALOS.
122700     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
122800     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
122900                                  H-ARITH-ALOS.
123000     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
123100     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
123200     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
123300     MOVE  'V'                 TO HLDDRG-VALID.
123400     MOVE ZEROES               TO H-DAYS-CUTOFF.
123500
123600 2600-EXIT.   EXIT.
123700
123800***************************************************************
123900 2700-COVID-DRG-ADJ.
124000***************************************************************
124100* ADJUSTMENT TO DRG WEIGHT PER COVID-19 DIAGNOSIS
124200*   + 20% INCREASE TO OPERATING DRG PAYMENTS
124300*   + COND-COVID1-FLAG = 'Y' IS EFFECTIVE FOR ADMISSIONS STARTING
124400*     9/1/2020. MACS WILL MAKE SURE TO CHECK THE ADMISSION DATE.
124500*----------------------------------------------------------------*
124600
124700     MOVE 1 TO IDX-COVID.
124800     MOVE 1 TO IDX-COVID-COND.
124900     MOVE 1.0 TO COVID-ADJ.
125000
125100     PERFORM 10000-COVID19-FLAG THRU 10000-EXIT
125200      VARYING IDX-COVID FROM 1 BY 1 UNTIL IDX-COVID > 25.
125300
125400     PERFORM 10100-COVID19-COND-FLAG THRU 10100-EXIT
125500      VARYING IDX-COVID-COND FROM 1 BY 1 UNTIL IDX-COVID-COND > 5.
125600
125700     IF B-DISCHARGE-DATE > 20200331
125800        IF DIAG-COVID2-FLAG = 'Y'
125900           IF COND-COVID1-FLAG = 'Y'
126000              GO TO 2700-EXIT
126100           ELSE
126200              MOVE 1.2 TO COVID-ADJ.
126300
126400     IF (B-DISCHARGE-DATE > 20200126 AND
126500        B-DISCHARGE-DATE < 20200401)
126600        IF DIAG-COVID1-FLAG = 'Y'
126700           MOVE 1.2 TO COVID-ADJ.
126800
126900 2700-EXIT.   EXIT.
127000
127100***************************************************************
127200 3000-CALC-PAYMENT.
127300***************************************************************
127400
127500     PERFORM 3100-CALC-STAY-UTILIZATION.
127600     PERFORM 3300-CALC-OPER-FSP-AMT.
127700     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
127800
127900***********************************************************
128000***  OPERATING IME CALCULATION
128100
128200     COMPUTE H-OPER-IME-TEACH ROUNDED =
128300            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
128400
128500***********************************************************
128600
128700     MOVE 00                 TO  PPS-RTC.
128800     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
128900     MOVE H-ALOS             TO  PPS-AVG-LOS.
129000     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
129100
129200     MOVE B-LOS TO H-PERDIEM-DAYS.
129300     IF H-PERDIEM-DAYS < 1
129400         MOVE 1 TO H-PERDIEM-DAYS.
129500     ADD 1 TO H-PERDIEM-DAYS.
129600
129700     MOVE 1 TO H-DSCHG-FRCTN.
129800
129900     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
130000
130100     IF (PAY-PERDIEM-DAYS  OR
130200         PAY-XFER-NO-COST) OR
130300        (PAY-XFER-SPEC-DRG AND
130400         D-DRG-POSTACUTE-PERDIEM)
130500       IF H-ALOS > 0
130600         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
130700         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
130800         IF H-DSCHG-FRCTN > 1
130900              MOVE 1 TO H-DSCHG-FRCTN
131000              MOVE 1 TO H-TRANSFER-ADJ
131100         ELSE
131200              COMPUTE H-DRG-WT-FRCTN ROUNDED =
131300                  H-TRANSFER-ADJ * H-DRG-WT
131400         END-IF
131500        END-IF
131600     END-IF.
131700
131800
131900     IF (PAY-XFER-SPEC-DRG AND
132000         D-DRG-POSTACUTE-50-50) AND
132100         H-ALOS > 0
132200         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
132300         COMPUTE H-DSCHG-FRCTN  ROUNDED =
132400                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
132500         IF H-DSCHG-FRCTN > 1
132600              MOVE 1 TO H-DSCHG-FRCTN
132700              MOVE 1 TO H-TRANSFER-ADJ
132800         ELSE
132900              COMPUTE H-DRG-WT-FRCTN ROUNDED =
133000            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
133100
133200
133300***********************************************************
133400***  CAPITAL DSH CALCULATION
133500
133600     MOVE 0 TO H-CAPI-DSH.
133700
133800     IF P-NEW-BED-SIZE NOT NUMERIC
133900         MOVE 0 TO P-NEW-BED-SIZE.
134000
134100     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
134200         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
134300                  (.2025 * (P-NEW-SSI-RATIO
134400                          + P-NEW-MEDICAID-RATIO)) - 1.
134500
134600***********************************************************
134700***  CAPITAL IME TEACH CALCULATION
134800
134900     MOVE 0 TO H-WK-CAPI-IME-TEACH.
135000
135100     IF P-NEW-CAPI-IME NUMERIC
135200        IF P-NEW-CAPI-IME > 1.5000
135300           MOVE 1.5000 TO P-NEW-CAPI-IME.
135400
135500*****YEARCHANGE 2009.5 ****************************************
135600***
135700***  PER POLICY, WE REMOVED THE .5 MULTIPLER
135800***
135900***********************************************************
136000     IF P-NEW-CAPI-IME NUMERIC
136100        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
136200         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
136300
136400*****YEARCHANGE 2009.5 ****************************************
136500***********************************************************
136600     MOVE 0.00 TO H-DAYOUT-PCT.
136700     MOVE 0.80 TO H-CSTOUT-PCT.
136800
136900*****************************************************************
137000**
137100** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
137200**
137300*****************************************************************
137400
137500     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
137600             MOVE 0.90 TO H-CSTOUT-PCT.
137700
137800*****YEARCHANGE 2018.0 *******************************************
137900* NATIONAL PERCENTAGE                                            *
138000******************************************************************
138100
138200       MOVE 0.6830 TO H-LABOR-PCT.
138300       MOVE 0.3170 TO H-NONLABOR-PCT.
138400
138500     IF (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000)
138600       MOVE 0.6200 TO H-LABOR-PCT
138700       MOVE 0.3800 TO H-NONLABOR-PCT.
138800
138900     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
139000             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
139100     ELSE
139200             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
139300
139400     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
139500             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
139600     ELSE
139700             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
139800
139900***********************************************************
140000*****YEARCHANGE 2010.0 ************************************
140100***  CAPITAL PAYMENT METHOD B - YEARCHNG
140200***  CAPITAL PAYMENT METHOD B
140300
140400     IF W-CBSA-SIZE = 'L'
140500        MOVE 1.00 TO H-CAPI-LARG-URBAN
140600     ELSE
140700        MOVE 1.00 TO H-CAPI-LARG-URBAN.
140800
140900     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
141000
141100*****YEARCHANGE 2018.0 ************************************
141200
141300     COMPUTE H-FEDERAL-RATE ROUNDED =
141400                              (0462.33 * H-CAPI-GAF).
141500
141600*****YEARCHANGE 2015.1 ************************************
141700
141800     COMPUTE H-CAPI-COLA ROUNDED =
141900                     (.3152 * (H-OPER-COLA - 1) + 1).
142000
142100     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
142200
142300***********************************************************
142400* CAPITAL FSP CALCULATION                                 *
142500***********************************************************
142600
142700     COMPUTE H-CAPI-FSP-PART ROUNDED =
142800                               H-DRG-WT       *
142900                               H-CAPI-FED-RATE *
143000                               H-CAPI-COLA *
143100                               H-CAPI-LARG-URBAN *
143200                               HLD-MID-ADJ-FACT.
143300
143400***********************************************************
143500***  CAPITAL PAYMENT METHOD A
143600***  CAPITAL PAYMENT METHOD A
143700
143800     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
143900        MOVE 1.00 TO H-CAPI-SCH
144000     ELSE
144100        MOVE 0.85 TO H-CAPI-SCH.
144200
144300***********************************************************
144400***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
144500***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
144600
144700     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
144800                    (P-NEW-CAPI-OLD-HARM-RATE *
144900                    H-CAPI-SCH).
145000
145100***********************************************************
145200        IF PAY-PERDIEM-DAYS
145300            IF  H-PERDIEM-DAYS < H-ALOS
145400                IF  NOT (B-DRG = 789)
145500                    PERFORM 3500-CALC-PERDIEM-AMT
145600                    MOVE 03 TO PPS-RTC.
145700
145800        IF PAY-XFER-SPEC-DRG
145900            IF  H-PERDIEM-DAYS < H-ALOS
146000                IF  NOT (B-DRG = 789)
146100                    PERFORM 3550-CALC-PERDIEM-AMT.
146200
146300        IF  PAY-XFER-NO-COST
146400            MOVE 00 TO PPS-RTC
146500            IF H-PERDIEM-DAYS < H-ALOS
146600               IF  NOT (B-DRG = 789)
146700                   PERFORM 3500-CALC-PERDIEM-AMT
146800                   MOVE 06 TO PPS-RTC.
146900
147000     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
147100
147200     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
147300
147400     IF PPS-RTC = 65 OR 67 OR 68
147500               GO TO 3000-CONTINUE.
147600
147700     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
147800
147900     IF PPS-RTC = 65 OR 67 OR 68
148000               GO TO 3000-CONTINUE.
148100
148200     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
148300
148400     IF PPS-RTC = 65 OR 67 OR 68
148500               GO TO 3000-CONTINUE.
148600
148700     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
148800
148900     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
149000
149100     IF PPS-RTC = 65 OR 67 OR 68
149200               GO TO 3000-CONTINUE.
149300
149400        IF PAY-XFER-SPEC-DRG
149500            IF  H-PERDIEM-DAYS < H-ALOS
149600                IF  NOT (B-DRG = 789)
149700                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
149800
149900
150000        IF  PAY-PERDIEM-DAYS
150100            IF  H-OPER-OUTCST-PART > 0
150200                MOVE H-OPER-OUTCST-PART TO
150300                     H-OPER-OUTLIER-PART
150400                MOVE 05 TO PPS-RTC
150500            ELSE
150600            IF  PPS-RTC NOT = 03
150700                MOVE 00 TO PPS-RTC
150800                MOVE 0  TO H-OPER-OUTLIER-PART.
150900
151000        IF  PAY-PERDIEM-DAYS
151100            IF  H-CAPI-OUTCST-PART > 0
151200                MOVE H-CAPI-OUTCST-PART TO
151300                     H-CAPI-OUTLIER-PART
151400                MOVE 05 TO PPS-RTC
151500            ELSE
151600            IF  PPS-RTC NOT = 03
151700                MOVE 0  TO H-CAPI-OUTLIER-PART.
151800
151900
152000     IF P-N-SCH-REBASED-FY90 OR
152100        P-N-EACH OR
152200        P-N-MDH-REBASED-FY90
152300         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
152400
152500 3000-CONTINUE.
152600
152700***********************************************************
152800***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
152900***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
153000
153100     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
153200
153300***********************************************************
153400
153500     IF  PPS-RTC = 67
153600         MOVE H-OPER-DOLLAR-THRESHOLD TO
153700              WK-H-OPER-DOLLAR-THRESHOLD.
153800
153900     IF  PPS-RTC < 50
154000         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
154100
154200     IF  PPS-RTC < 50
154300         NEXT SENTENCE
154400     ELSE
154500         MOVE ALL '0' TO PPS-OPER-HSP-PART
154600                         PPS-OPER-FSP-PART
154700                         PPS-OPER-OUTLIER-PART
154800                         PPS-OUTLIER-DAYS
154900                         PPS-REG-DAYS-USED
155000                         PPS-LTR-DAYS-USED
155100                         PPS-TOTAL-PAYMENT
155200                         WK-HAC-TOTAL-PAYMENT
155300                         PPS-OPER-DSH-ADJ
155400                         PPS-OPER-IME-ADJ
155500                         H-DSCHG-FRCTN
155600                         H-DRG-WT-FRCTN
155700                         HOLD-ADDITIONAL-VARIABLES
155800                         HOLD-CAPITAL-VARIABLES
155900                         HOLD-CAPITAL2-VARIABLES
156000                         HOLD-OTHER-VARIABLES
156100                         HOLD-PC-OTH-VARIABLES
156200                        H-ADDITIONAL-PAY-INFO-DATA
156300                        H-ADDITIONAL-PAY-INFO-DATA2.
156400
156500     IF  PPS-RTC = 67
156600         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
156700                 H-OPER-DOLLAR-THRESHOLD.
156800
156900 3000-EXIT.  EXIT.
157000
157100 3100-CALC-STAY-UTILIZATION.
157200
157300     MOVE 0 TO PPS-REG-DAYS-USED.
157400     MOVE 0 TO PPS-LTR-DAYS-USED.
157500
157600     IF H-REG-DAYS > 0
157700        IF H-REG-DAYS > B-LOS
157800           MOVE B-LOS TO PPS-REG-DAYS-USED
157900        ELSE
158000           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
158100     ELSE
158200        IF H-LTR-DAYS > B-LOS
158300           MOVE B-LOS TO PPS-LTR-DAYS-USED
158400        ELSE
158500           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
158600
158700
158800
158900 3300-CALC-OPER-FSP-AMT.
159000***********************************************************
159100*  OPERATING FSP CALCULATION                              *
159200***********************************************************
159300
159400     COMPUTE H-OPER-FSP-PART ROUNDED =
159500       ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
159600        H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT *
159700        HLD-MID-ADJ-FACT * COVID-ADJ)
159800           ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
159900
160000 3500-CALC-PERDIEM-AMT.
160100***********************************************************
160200***  REVIEW CODE = 03 OR 06
160300***  OPERATING PERDIEM-AMT CALCULATION
160400***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
160500
160600        COMPUTE H-OPER-FSP-PART ROUNDED =
160700        H-OPER-FSP-PART * H-TRANSFER-ADJ
160800        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
160900
161000***********************************************************
161100***********************************************************
161200***  REVIEW CODE = 03 OR 06
161300***  CAPITAL   PERDIEM-AMT CALCULATION
161400***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
161500
161600        COMPUTE H-CAPI-FSP-PART ROUNDED =
161700        H-CAPI-FSP-PART * H-TRANSFER-ADJ
161800        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
161900
162000***********************************************************
162100***  REVIEW CODE = 03 OR 06
162200***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
162300***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
162400
162500        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
162600        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
162700        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
162800
162900 3550-CALC-PERDIEM-AMT.
163000***********************************************************
163100***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
163200***  OPERATING PERDIEM-AMT CALCULATION
163300***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
163400
163500     IF (D-DRG-POSTACUTE-50-50)
163600        MOVE 10 TO PPS-RTC
163700        COMPUTE H-OPER-FSP-PART ROUNDED =
163800        H-OPER-FSP-PART * H-DSCHG-FRCTN
163900        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
164000
164100     IF (D-DRG-POSTACUTE-PERDIEM)
164200        MOVE 12 TO PPS-RTC
164300        COMPUTE H-OPER-FSP-PART ROUNDED =
164400        H-OPER-FSP-PART *  H-TRANSFER-ADJ
164500        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
164600
164700***********************************************************
164800***  CAPITAL PERDIEM-AMT CALCULATION
164900***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
165000
165100     IF (D-DRG-POSTACUTE-50-50)
165200        MOVE 10 TO PPS-RTC
165300        COMPUTE H-CAPI-FSP-PART ROUNDED =
165400        H-CAPI-FSP-PART * H-DSCHG-FRCTN
165500        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
165600
165700     IF (D-DRG-POSTACUTE-PERDIEM)
165800        MOVE 12 TO PPS-RTC
165900        COMPUTE H-CAPI-FSP-PART ROUNDED =
166000        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
166100        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
166200
166300***********************************************************
166400***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
166500***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
166600
166700     IF (D-DRG-POSTACUTE-50-50)
166800        MOVE 10 TO PPS-RTC
166900        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
167000        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
167100        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
167200
167300     IF (D-DRG-POSTACUTE-PERDIEM)
167400        MOVE 12 TO PPS-RTC
167500        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
167600        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
167700        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
167800
167900 3560-CHECK-RTN-CODE.
168000
168100     IF (D-DRG-POSTACUTE-50-50)
168200        MOVE 10 TO PPS-RTC.
168300     IF (D-DRG-POSTACUTE-PERDIEM)
168400        MOVE 12 TO PPS-RTC.
168500
168600 3560-EXIT.    EXIT.
168700
168800***********************************************************
168900 3600-CALC-OUTLIER.
169000***********************************************************
169100*---------------------------------------------------------*
169200* (YEARCHANGE 2016.0)
169300* COST OUTLIER OPERATING AND CAPITAL CALCULATION
169400*---------------------------------------------------------*
169500
169600     IF OUTLIER-RECON-FLAG = 'Y'
169700        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
169800               (H-OPER-CSTCHG-RATIO + .2).
169900
170000     IF H-CAPI-CSTCHG-RATIO > 0 OR
170100        H-OPER-CSTCHG-RATIO > 0
170200        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
170300                H-OPER-CSTCHG-RATIO /
170400               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
170500        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
170600                H-CAPI-CSTCHG-RATIO /
170700               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
170800     ELSE
170900        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
171000                  H-CAPI-SHARE-DOLL-THRESHOLD.
171100
171200*-----------------------------*
171300* (YEARCHANGE 2020.0)         *
171400* OUTLIER THRESHOLD AMOUNTS   *
171500*-----------------------------*
171600
171700     MOVE 26552.00 TO H-CST-THRESH.
171800
171900     IF (B-REVIEW-CODE = '03') AND
172000         H-PERDIEM-DAYS < H-ALOS
172100        COMPUTE H-CST-THRESH ROUNDED =
172200                      (H-CST-THRESH * H-TRANSFER-ADJ)
172300                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
172400
172500     IF ((B-REVIEW-CODE = '09') AND
172600         (H-PERDIEM-DAYS < H-ALOS))
172700         IF (D-DRG-POSTACUTE-PERDIEM)
172800            COMPUTE H-CST-THRESH ROUNDED =
172900                      (H-CST-THRESH * H-TRANSFER-ADJ)
173000                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
173100
173200     IF ((B-REVIEW-CODE = '09') AND
173300         (H-PERDIEM-DAYS < H-ALOS))
173400         IF (D-DRG-POSTACUTE-50-50)
173500           COMPUTE H-CST-THRESH ROUNDED =
173600          H-CST-THRESH * H-DSCHG-FRCTN
173700                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
173800
173900     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
174000        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
174100         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
174200          H-OPER-SHARE-DOLL-THRESHOLD.
174300
174400***********************************************************
174500
174600     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
174700          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
174800          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
174900
175000***********************************************************
175100******NOW INCLUDES UNCOMPENSATED CARE**********************
175200
175300     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
175400         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
175500                       +
175600           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
175700                       +
175800             H-OPER-DOLLAR-THRESHOLD
175900                       +
176000                WK-UNCOMP-CARE-AMOUNT
176100                       +
176200                 H-NEW-TECH-PAY-ADD-ON.
176300
176400     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
176500      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
176600                       +
176700             H-CAPI-DOLLAR-THRESHOLD.
176800
176900     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
177000         MOVE 0 TO H-CAPI-COST-OUTLIER.
177100
177200
177300***********************************************************
177400***  OPERATING COST CALCULATION
177500
177600     COMPUTE H-OPER-BILL-COSTS ROUNDED =
177700         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
177800         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
177900
178000
178100     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
178200         COMPUTE H-OPER-OUTCST-PART ROUNDED =
178300         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
178400                         H-OPER-COST-OUTLIER).
178500
178600     IF PAY-WITHOUT-COST OR
178700        PAY-XFER-NO-COST OR
178800        PAY-XFER-SPEC-DRG-NO-COST
178900         MOVE 0 TO H-OPER-OUTCST-PART.
179000
179100***********************************************************
179200***  CAPITAL COST CALCULATION
179300
179400     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
179500             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
179600         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
179700
179800     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
179900         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
180000         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
180100                         H-CAPI-COST-OUTLIER).
180200
180300***********************************************************
180400***  'A' NOT VALID FY 2015 ON
180500
180600*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
180700*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
180800*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
180900
181000     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
181100        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
181200               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
181300
181400     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
181500        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
181600        MOVE 0 TO H-CAPI-OUTCST-PART
181700                  H-OPER-OUTCST-PART.
181800
181900     IF PAY-WITHOUT-COST OR
182000        PAY-XFER-NO-COST OR
182100        PAY-XFER-SPEC-DRG-NO-COST
182200         MOVE 0 TO H-CAPI-OUTCST-PART.
182300
182400***********************************************************
182500***  DETERMINES THE BILL TO BE COST  OUTLIER
182600
182700     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
182800         MOVE 0 TO H-CAPI-OUTDAY-PART
182900                   H-CAPI-OUTCST-PART.
183000
183100     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
183200                 MOVE H-OPER-OUTCST-PART TO
183300                      H-OPER-OUTLIER-PART
183400                 MOVE H-CAPI-OUTCST-PART TO
183500                      H-CAPI-OUTLIER-PART
183600                 MOVE 02 TO PPS-RTC.
183700
183800     IF OUTLIER-RECON-FLAG = 'Y'
183900        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
184000           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
184100           GO TO 3600-EXIT
184200        ELSE
184300           GO TO 3600-EXIT
184400     ELSE
184500        NEXT SENTENCE.
184600
184700
184800***********************************************************
184900***  DETERMINES IF COST OUTLIER
185000***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
185100***         RETURN CODE OF 02
185200
185300     MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
185400
185500     IF PPS-RTC = 02
185600       IF H-CAPI-CSTCHG-RATIO > 0 OR
185700          H-OPER-CSTCHG-RATIO > 0
185800             COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
185900                     (H-CAPI-COST-OUTLIER  +
186000                      H-OPER-COST-OUTLIER)
186100                             /
186200                    (H-CAPI-CSTCHG-RATIO  +
186300                     H-OPER-CSTCHG-RATIO)
186400             ON SIZE ERROR MOVE 0 TO H-OPER-DOLLAR-THRESHOLD
186500       ELSE MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
186600
186700***********************************************************
186800***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
186900***         RETURN CODE OF 67
187000
187100     IF PPS-RTC = 02
187200         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
187300            PPS-PC-COT-FLAG = 'Y'
187400             MOVE 67 TO PPS-RTC.
187500***********************************************************
187600
187700***********************************************************
187800***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
187900***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
188000***********************************************************
188100*
188200***********************************************************
188300***  'A' NOT VALID FY 2015 ON
188400*
188500*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
188600*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
188700*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
188800*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
188900
189000     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
189100        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
189200                H-CAPI-OUTLIER-PART.
189300
189400     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
189500        H-CAPI-PAYCDE-PCT1 > 0
189600        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
189700                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
189800         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
189900     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
190000
190100 3600-EXIT.   EXIT.
190200
190300***********************************************************
190400 3450-CALC-ADDITIONAL-HSP.
190500***********************************************************
190600*---------------------------------------------------------*
190700* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
190800* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
190900* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
191000*---------------------------------------------------------*
191100***  GET THE RBN UPDATING FACTOR
191200
191300*****YEARCHANGE 2019.0 ****************************************
191400     MOVE 0.997190 TO H-BUDG-NUTR190.
191500
191600*****YEARCHANGE 2020.0 ****************************************
191700     MOVE 0.996859 TO H-BUDG-NUTR200.
191800
191900
192000***  GET THE MARKET BASKET UPDATE FACTOR
192100*****YEARCHANGE 2019.0 ****************************************
192200        MOVE 1.01350 TO H-UPDATE-190.
192300
192400*****YEARCHANGE 2020.0 ****************************************
192500        MOVE 1.02600 TO H-UPDATE-200.
192600
192700*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
192800*****YEARCHANGE 2020.0 ****************************************
192900     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
193000        P-EHR-REDUC-IND = ' '
193100        MOVE 1.02600 TO H-UPDATE-200.
193200
193300*****YEARCHANGE 2020.0 ****************************************
193400     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
193500        P-EHR-REDUC-IND = 'Y'
193600        MOVE 1.00350 TO H-UPDATE-200.
193700
193800*****YEARCHANGE 2020.0 ****************************************
193900     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
194000        P-EHR-REDUC-IND = ' '
194100        MOVE 1.01850 TO H-UPDATE-200.
194200
194300*****YEARCHANGE 2020.0 ****************************************
194400     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
194500        P-EHR-REDUC-IND = 'Y'
194600        MOVE 0.99600 TO H-UPDATE-200.
194700
194800
194900********YEARCHANGE 2020.0 *************************************
195000
195100     COMPUTE H-UPDATE-FACTOR ROUNDED =
195200                       (H-UPDATE-190 *
195300                        H-UPDATE-200 *
195400                        H-BUDG-NUTR190 *
195500                        H-BUDG-NUTR200 *
195600                        HLD-MID-ADJ-FACT).
195700
195800     COMPUTE H-HSP-RATE ROUNDED =
195900         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT * COVID-ADJ.
196000
196100***************************************************************
196200*
196300*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
196400*       COMPUTE H-HSP-RATE ROUNDED =
196500*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
196600*    ELSE
196700*       COMPUTE H-HSP-RATE ROUNDED =
196800*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
196900*
197000***************************************************************
197100********YEARCHANGE 2011.0 *************************************
197200***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
197300***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
197400***     IN THE HSP COMPARRISON
197500
197600********YEARCHANGE 2014.0 *XXXXXX******************************
197700*      THE HSP BUCKET FOR SCH                      ************
197800*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
197900***************************************************************
198000
198100     COMPUTE H-FSP-RATE ROUNDED =
198200        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
198300         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN *
198400         HLD-MID-ADJ-FACT * COVID-ADJ) *
198500             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
198600                               +
198700                         H-OPER-OUTLIER-PART
198800                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
198900
199000****************************************************************
199100****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
199200*****        CHOICE
199300
199400     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
199500           COMPUTE H-OPER-HSP-PART ROUNDED =
199600             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
199700                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
199800     ELSE
199900         MOVE 0 TO H-OPER-HSP-PART.
200000
200100***************************************************************
200200***  YEARCHANGE TURNING MDH BACK ON ***************************
200300***************************************************************
200400***  GET THE MDH REBASE
200500
200600     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
200700         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
200800           COMPUTE H-OPER-HSP-PART ROUNDED =
200900         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
201000                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
201100
201200***************************************************************
201300***  TRANSITIONAL PAYMENT FOR FORMER MDHS                     *
201400***************************************************************
201500
201600***  HSP PAYMENT FOR CLAIMS BETWEEN 10/01/2016 - 09/30/2017
201700
201800*    IF  B-FORMER-MDH-PROVIDERS       AND
201900*       (B-DISCHARGE-DATE > 20160930  AND
202000*        B-DISCHARGE-DATE < 20171001)
202100*      IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
202200*        COMPUTE H-OPER-HSP-PART ROUNDED =
202300*          ((H-HSP-RATE - (H-FSP-RATE +
202400*              WK-UNCOMP-CARE-AMOUNT))* 0.75)*(1 / 3)
202500*            ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
202600*      END-IF
202700*    END-IF.
202800
202900 3450-EXIT.   EXIT.
203000
203100***********************************************************
203200 3800-CALC-TOT-AMT.
203300***********************************************************
203400***  CALCULATE TOTALS FOR CAPITAL
203500
203600     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
203700
203800***********************************************************
203900***  'A' NOT VALID FY 2015 ON
204000*
204100*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
204200*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
204300*       MOVE 0.00 TO H-CAPI-HSP-PCT.
204400
204500     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
204600        MOVE 0    TO H-CAPI-OLD-HARMLESS
204700        MOVE 1.00 TO H-CAPI-FSP-PCT
204800        MOVE 0.00 TO H-CAPI-HSP-PCT.
204900
205000     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
205100        MOVE 0    TO H-CAPI-OLD-HARMLESS
205200        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
205300        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
205400
205500     COMPUTE H-CAPI-HSP ROUNDED =
205600         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
205700
205800     COMPUTE H-CAPI-FSP ROUNDED =
205900         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
206000
206100     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
206200
206300     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
206400
206500     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
206600             H-CAPI-FSP
206700              * H-CAPI-DSH.
206800
206900     COMPUTE H-CAPI-IME-ADJ ROUNDED =
207000          H-CAPI-FSP *
207100                 H-WK-CAPI-IME-TEACH.
207200
207300     COMPUTE H-CAPI-OUTLIER ROUNDED =
207400             1.00 * H-CAPI-OUTLIER-PART.
207500
207600     COMPUTE H-CAPI2-B-FSP ROUNDED =
207700             1.00 * H-CAPI2-B-FSP-PART.
207800
207900     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
208000             1.00 * H-CAPI2-B-OUTLIER-PART.
208100***********************************************************
208200***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
208300***        THIS ZEROES OUT ALL CAPITAL DATA
208400
208500     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
208600        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
208700***********************************************************
208800
208900***********************************************************
209000***  CALCULATE FINAL TOTALS FOR OPERATING
209100
209200     IF (H-CAPI-OUTLIER > 0 AND
209300         PPS-OPER-OUTLIER-PART = 0)
209400            COMPUTE PPS-OPER-OUTLIER-PART =
209500                    PPS-OPER-OUTLIER-PART + .01.
209600
209700***********************************************************
209800*LOW VOLUME CALCULATIONS
209900***********************************************************
210000*---------------------------------------------------------*
210100* (YEARCHANGE 2016.0)
210200* LOW VOLUME PAYMENT ADD-ON PERCENT
210300*---------------------------------------------------------*
210400
210500     MOVE ZERO TO PPS-OPER-DSH-ADJ.
210600************************************************
210700* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
210800* EMPERICAL DSH
210900************************************************
211000     IF  H-OPER-DSH NUMERIC
211100         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
211200                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
211300
211400     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
211500                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
211600
211700
211800     COMPUTE PPS-OPER-FSP-PART ROUNDED =
211900                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
212000
212100     COMPUTE PPS-OPER-HSP-PART ROUNDED =
212200                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
212300
212400     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
212500                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
212600
212700     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
212800                                H-NEW-TECH-PAY-ADD-ON.
212900
213000     COMPUTE PPS-ISLET-ISOL-PAY-ADD-ON ROUNDED =
213100                                H-NEW-TECH-ADDON-ISLET.
213200
213300     IF P-NEW-TEMP-RELIEF-IND = 'Y'
213400        AND P-LV-ADJ-FACTOR > 0.00
213500        AND P-LV-ADJ-FACTOR <= 0.25
213600     COMPUTE WK-LOW-VOL-ADDON ROUNDED =
213700       (PPS-OPER-HSP-PART +
213800        PPS-OPER-FSP-PART +
213900        PPS-OPER-IME-ADJ +
214000        PPS-OPER-DSH-ADJ +
214100        PPS-OPER-OUTLIER-PART +
214200        H-CAPI-FSP +
214300        H-CAPI-IME-ADJ +
214400        H-CAPI-DSH-ADJ +
214500        H-CAPI-OUTLIER +
214600        WK-UNCOMP-CARE-AMOUNT +
214700        PPS-NEW-TECH-PAY-ADD-ON) * P-LV-ADJ-FACTOR
214800     ELSE
214900     COMPUTE WK-LOW-VOL-ADDON ROUNDED = 0.
215000
215100     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
215200     IF HMO-TAG  = 'Y'
215300        PERFORM 3850-HMO-IME-ADJ.
215400
215500***********************************************************
215600***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
215700
215800     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
215900             H-CAPI-FSP + H-CAPI-IME-ADJ +
216000             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
216100
216200         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
216300         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
216400
216500***********************************************************
216600* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
216700***********************************************************
216800*---------------------------------------------------------*
216900* (YEARCHANGE 2016.0)
217000* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
217100*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
217200*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
217300*   + BASED ON INDICATOR FROM THE PPS FILE
217400*   + NOT VALID IN PUERTO RICO
217500*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
217600*---------------------------------------------------------*
217700
217800     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
217900        PPS-OPER-HSP-PART +
218000        PPS-OPER-FSP-PART +
218100        PPS-OPER-IME-ADJ +
218200        PPS-OPER-DSH-ADJ +
218300        PPS-OPER-OUTLIER-PART +
218400        H-CAPI-TOTAL-PAY +
218500        WK-UNCOMP-CARE-AMOUNT +
218600        PPS-NEW-TECH-PAY-ADD-ON +
218700        WK-LOW-VOL-ADDON +
218800        H-READMIS-ADJUST-AMT +
218900        H-VAL-BASED-PURCH-ADJUST-AMT.
219000
219100     MOVE ZERO TO WK-HAC-AMOUNT.
219200
219300     IF P-PR-NEW-STATE AND
219400        P-HAC-REDUC-IND = 'Y'
219500           MOVE 53 TO PPS-RTC
219600           GO TO 3800-EXIT.
219700
219800     IF  P-HAC-REDUC-IND = 'Y'
219900         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
220000                   WK-HAC-TOTAL-PAYMENT * -0.01
220100     ELSE
220200         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
220300
220400***********************************************************
220500***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
220600************************************************
220700     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
220800                 WK-HAC-TOTAL-PAYMENT
220900                           +
221000                 H-WK-PASS-AMT-PLUS-MISC
221100                           +
221200                 H-BUNDLE-ADJUST-AMT
221300                           +
221400                 WK-HAC-AMOUNT
221500                           +
221600                 H-NEW-TECH-ADDON-ISLET.
221700
221800     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
221900              H-VAL-BASED-PURCH-PARTIPNT.
222000
222100     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
222200              H-VAL-BASED-PURCH-ADJUST.
222300
222400     MOVE     P-HOSP-READMISSION-REDU    TO
222500              H-HOSP-READMISSION-REDU.
222600
222700     MOVE     P-HOSP-HRR-ADJUSTMT        TO
222800              H-HOSP-HRR-ADJUSTMT.
222900
223000 3800-EXIT.   EXIT.
223100
223200 3850-HMO-IME-ADJ.
223300***********************************************************
223400***  HMO CALC FOR PASS-THRU ADDON
223500
223600     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
223700          (P-NEW-PASS-AMT-PLUS-MISC -
223800          (P-NEW-PASS-AMT-ORGAN-ACQ +
223900           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
224000
224100***********************************************************
224200***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
224300
224400     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
224500                   PPS-OPER-IME-ADJ * .0.
224600
224700***********************************************************
224800
224900
225000 3900A-CALC-OPER-DSH.
225100
225200***  OPERATING DSH CALCULATION
225300
225400      MOVE 0.0000 TO H-OPER-DSH.
225500
225600      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
225700                                     + P-NEW-MEDICAID-RATIO).
225800
225900***********************************************************
226000**1**    0-99 BEDS
226100***  NOT TO EXCEED 12%
226200
226300      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
226400                               AND H-WK-OPER-DSH > .1499
226500                               AND H-WK-OPER-DSH < .2020
226600        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
226700                                      * .65 + .025
226800        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
226900
227000      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
227100                               AND H-WK-OPER-DSH > .2019
227200        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
227300                                      * .825 + .0588
227400        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
227500
227600***********************************************************
227700**2**   100 + BEDS
227800***  NO CAP >> CAN EXCEED 12%
227900
228000      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
228100                               AND H-WK-OPER-DSH > .1499
228200                               AND H-WK-OPER-DSH < .2020
228300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
228400                                      * .65 + .025.
228500
228600      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
228700                               AND H-WK-OPER-DSH > .2019
228800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
228900                                      * .825 + .0588.
229000
229100***********************************************************
229200**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
229300***  NOT TO EXCEED 12%
229400
229500      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
229600                               AND H-WK-OPER-DSH > .1499
229700                               AND H-WK-OPER-DSH < .2020
229800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
229900                                 * .65 + .025
230000        IF H-OPER-DSH > .1200
230100              MOVE .1200 TO H-OPER-DSH.
230200
230300      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
230400                               AND H-WK-OPER-DSH > .2019
230500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
230600                                 * .825 + .0588
230700        IF H-OPER-DSH > .1200
230800                 MOVE .1200 TO H-OPER-DSH.
230900***********************************************************
231000**4**   OTHER RURAL HOSPITALS 500 BEDS +
231100***  NO CAP >> CAN EXCEED 12%
231200
231300      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
231400                               AND H-WK-OPER-DSH > .1499
231500                               AND H-WK-OPER-DSH < .2020
231600        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
231700                                 * .65 + .025.
231800
231900      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
232000                               AND H-WK-OPER-DSH > .2019
232100        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
232200                                 * .825 + .0588.
232300
232400***********************************************************
232500**7**   RURAL HOSPITALS SCH
232600***  NOT TO EXCEED 12%
232700
232800      IF W-CBSA-SIZE = 'R'
232900         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
233000                               AND H-WK-OPER-DSH > .1499
233100                               AND H-WK-OPER-DSH < .2020
233200         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
233300                                 * .65 + .025
233400        IF H-OPER-DSH > .1200
233500                 MOVE .1200 TO H-OPER-DSH.
233600
233700      IF W-CBSA-SIZE = 'R'
233800         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
233900                               AND H-WK-OPER-DSH > .2019
234000         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
234100                                 * .825 + .0588
234200        IF H-OPER-DSH > .1200
234300                 MOVE .1200 TO H-OPER-DSH.
234400
234500***********************************************************
234600**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
234700***  RRC OVERRIDES SCH CAP
234800***  NO CAP >> CAN EXCEED 12%
234900
235000         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
235100                                   '17' OR '22')
235200                               AND H-WK-OPER-DSH > .1499
235300                               AND H-WK-OPER-DSH < .2020
235400         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
235500                                 * .65 + .025.
235600
235700         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
235800                                   '17' OR '22')
235900                               AND H-WK-OPER-DSH > .2019
236000         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
236100                                 * .825 + .0588.
236200
236300      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
236400
236500 3900A-EXIT.   EXIT.
236600
236700 4000-CALC-TECH-ADDON.
236800
236900***********************************************************
237000***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
237100
237200     COMPUTE PPS-OPER-HSP-PART ROUNDED =
237300         H-OPER-HSP-PCT * H-OPER-HSP-PART.
237400
237500     COMPUTE PPS-OPER-FSP-PART ROUNDED =
237600         H-OPER-FSP-PCT * H-OPER-FSP-PART.
237700
237800     MOVE ZERO TO PPS-OPER-DSH-ADJ.
237900
238000     IF  H-OPER-DSH NUMERIC
238100             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
238200             (PPS-OPER-FSP-PART
238300              * H-OPER-DSH) * .25.
238400
238500     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
238600             PPS-OPER-FSP-PART *
238700             H-OPER-IME-TEACH.
238800
238900     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
239000             PPS-OPER-FSP-PART +
239100             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
239200             WK-UNCOMP-CARE-AMOUNT.
239300
239400***********************************************************
239500* NEW TECHNOLOGY ADD-ON CODE *
239600***********************************************************
239700     MOVE 1 TO IDX-TECH.
239800     INITIALIZE H-CSTMED-STOP.
239900     INITIALIZE H-NEW-TECH-PCT.
240000     INITIALIZE H-TECH-ADDON-ISLET-CNTR.
240100
240200     PERFORM 4010-FLAG-NEW-TECH THRU 4010-EXIT
240300      VARYING IDX-TECH FROM 1 BY 1 UNTIL IDX-TECH > 25.
240400
240500     IF PROC-ANDEXXA-FLAG = 'Y'
240600       MOVE  18281.25 TO H-CSTMED-STOP.
240700       MOVE 0.65 TO H-NEW-TECH-PCT.
240800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
240900
241000     IF PROC-AQUABEAM-FLAG = 'Y'
241100       MOVE   1625.00 TO H-CSTMED-STOP.
241200       MOVE 0.65 TO H-NEW-TECH-PCT.
241300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
241400
241500     IF PROC-AZEDRA-FLAG = 'Y'
241600       MOVE  98150.00 TO H-CSTMED-STOP.
241700       MOVE 0.65 TO H-NEW-TECH-PCT.
241800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
241900
242000     IF PROC-BALVERSA-FLAG = 'Y'
242100       MOVE   3563.23 TO H-CSTMED-STOP.
242200       MOVE 0.65 TO H-NEW-TECH-PCT.
242300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
242400
242500     IF PROC-CABLIVI-FLAG = 'Y'
242600       MOVE  33215.00 TO H-CSTMED-STOP.
242700       MOVE 0.65 TO H-NEW-TECH-PCT.
242800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
242900
243000     IF PROC-ELZONRIS-FLAG = 'Y'
243100       MOVE 125448.05 TO H-CSTMED-STOP.
243200       MOVE 0.65 TO H-NEW-TECH-PCT.
243300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
243400
243500     IF PROC-ERLEADA-FLAG = 'Y'
243600       MOVE   1858.25 TO H-CSTMED-STOP.
243700       MOVE 0.65 TO H-NEW-TECH-PCT.
243800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
243900
244000     IF PROC-GIAPREZA-FLAG = 'Y'
244100       MOVE   1950.00 TO H-CSTMED-STOP.
244200       MOVE 0.65 TO H-NEW-TECH-PCT.
244300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
244400
244500     IF DIAG-ISLET-FLAG = 'Y' AND PROC-ISLET-FLAG = 'Y'
244600       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
244700     ELSE
244800       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
244900
245000     IF PROC-JAKAFI-FLAG = 'Y'
245100       MOVE   3977.06 TO H-CSTMED-STOP.
245200       MOVE 0.65 TO H-NEW-TECH-PCT.
245300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
245400
245500     IF PROC-KYMRIAH-FLAG = 'Y'
245600       MOVE 242450.00 TO H-CSTMED-STOP.
245700       MOVE 0.65 TO H-NEW-TECH-PCT.
245800       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
245900
246000     IF PROC-PLAZO-FLAG = 'Y'
246100       MOVE   4083.75 TO H-CSTMED-STOP.
246200       MOVE 0.75 TO H-NEW-TECH-PCT.
246300       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
246400
246500     IF PROC-REMEDE1-FLAG = 'Y' AND PROC-REMEDE2-FLAG = 'Y'
246600       AND PROC-REMEDE3-FLAG = 'Y'
246700       MOVE  22425.00 TO H-CSTMED-STOP.
246800       MOVE 0.65 TO H-NEW-TECH-PCT.
246900       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
247000
247100     IF PROC-SENTINEL-FLAG = 'Y'
247200       MOVE  1820.00 TO H-CSTMED-STOP.
247300       MOVE 0.65 TO H-NEW-TECH-PCT.
247400       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
247500
247600     IF PROC-SPRAVATO-FLAG = 'Y'
247700       MOVE  1014.79 TO H-CSTMED-STOP.
247800       MOVE 0.65 TO H-NEW-TECH-PCT.
247900       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
248000
248100     IF PROC-T2-FLAG = 'Y'
248200       MOVE    97.50 TO H-CSTMED-STOP.
248300       MOVE 0.65 TO H-NEW-TECH-PCT.
248400       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
248500
248600     IF PROC-VABOMERE-FLAG = 'Y' OR NDC-VABOMERE-FLAG = 'Y'
248700       MOVE  8316.00 TO H-CSTMED-STOP.
248800       MOVE 0.75 TO H-NEW-TECH-PCT.
248900       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
249000
249100     IF PROC-VYXEOS-FLAG = 'Y'
249200       MOVE 47352.50 TO H-CSTMED-STOP.
249300       MOVE 0.65 TO H-NEW-TECH-PCT.
249400       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
249500
249600     IF PROC-XOSPATA-FLAG = 'Y'
249700       MOVE  7312.50 TO H-CSTMED-STOP.
249800       MOVE 0.65 TO H-NEW-TECH-PCT.
249900       PERFORM 4020-NEW-TECH-ADD-ON THRU 4020-EXIT.
250000
250100***********************************************************
250200*  ALL NEW TECH MUST BE CALCULATED BEFORE
250300*  5500-CAP-CALC-TECH-ADD-ON
250400***********************************************************
250500     PERFORM 5500-CAP-CALC-TECH-ADD-ON THRU 5500-EXIT.
250600
250700     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
250800             H-OPER-FSP-PART +
250900             H-NEW-TECH-PAY-ADD-ON.
251000
251100 4000-EXIT.    EXIT.
251200
251300************************************
251400* NEW TECHNOLOGY ADD-ON FLAG LOGIC *
251500************************************
251600 4010-FLAG-NEW-TECH.
251700
251800     MOVE B-PROCEDURE-CODE(IDX-TECH) TO WK-PROC-NEW-TECH.
251900     MOVE B-DIAGNOSIS-CODE(IDX-TECH) TO WK-DIAG-NEW-TECH.
252000     MOVE B-NDC-NUMBER TO WK-NDC-NEW-TECH.
252100
252200     IF PROC-ANDEXXA
252300       MOVE 'Y' TO PROC-ANDEXXA-FLAG.
252400
252500     IF PROC-AQUABEAM
252600       MOVE 'Y' TO PROC-AQUABEAM-FLAG.
252700
252800     IF PROC-AZEDRA
252900       MOVE 'Y' TO PROC-AZEDRA-FLAG.
253000
253100     IF PROC-BALVERSA
253200       MOVE 'Y' TO PROC-BALVERSA-FLAG.
253300
253400     IF PROC-CABLIVI
253500       MOVE 'Y' TO PROC-CABLIVI-FLAG.
253600
253700     IF PROC-ELZONRIS
253800       MOVE 'Y' TO PROC-ELZONRIS-FLAG.
253900
254000     IF PROC-ERLEADA
254100       MOVE 'Y' TO PROC-ERLEADA-FLAG.
254200
254300     IF PROC-GIAPREZA
254400       MOVE 'Y' TO PROC-GIAPREZA-FLAG.
254500
254600     IF PROC-ISLET
254700       MOVE 'Y' TO PROC-ISLET-FLAG
254800       COMPUTE H-TECH-ADDON-ISLET-CNTR =
254900          H-TECH-ADDON-ISLET-CNTR + 1.
255000
255100     IF PROC-JAKAFI
255200       MOVE 'Y' TO PROC-JAKAFI-FLAG.
255300
255400     IF PROC-KYMRIAH
255500       MOVE 'Y' TO PROC-KYMRIAH-FLAG.
255600
255700     IF PROC-PLAZO
255800       MOVE 'Y' TO PROC-PLAZO-FLAG.
255900
256000     IF PROC-REMEDE1
256100       MOVE 'Y' TO PROC-REMEDE1-FLAG.
256200
256300     IF PROC-REMEDE2
256400       MOVE 'Y' TO PROC-REMEDE2-FLAG.
256500
256600     IF PROC-REMEDE3
256700       MOVE 'Y' TO PROC-REMEDE3-FLAG.
256800
256900     IF PROC-SENTINEL
257000       MOVE 'Y' TO PROC-SENTINEL-FLAG.
257100
257200     IF PROC-SPRAVATO
257300       MOVE 'Y' TO PROC-SPRAVATO-FLAG.
257400
257500     IF PROC-T2
257600       MOVE 'Y' TO PROC-T2-FLAG.
257700
257800     IF PROC-VABOMERE
257900       MOVE 'Y' TO PROC-VABOMERE-FLAG.
258000
258100     IF PROC-VYXEOS
258200       MOVE 'Y' TO PROC-VYXEOS-FLAG.
258300
258400     IF PROC-XOSPATA
258500       MOVE 'Y' TO PROC-XOSPATA-FLAG.
258600
258700     IF DIAG-ISLET
258800       MOVE 'Y' TO DIAG-ISLET-FLAG.
258900
259000     IF NDC-VABOMERE
259100       MOVE 'Y' TO NDC-VABOMERE-FLAG.
259200
259300 4010-EXIT.   EXIT.
259400
259500*******************************************
259600* NEW TECHNOLOGY ADD-ON CALCULATION LOGIC *
259700*******************************************
259800 4020-NEW-TECH-ADD-ON.
259900
260000     MOVE 0 TO H-NEW-TECH-ADDON
260100               H-LESSER-STOP-1
260200               H-LESSER-STOP-2.
260300
260400     COMPUTE H-LESSER-STOP-1 ROUNDED =
260500                  H-CSTMED-STOP.
260600
260700     COMPUTE H-LESSER-STOP-2 ROUNDED =
260800          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
260900             H-BASE-DRG-PAYMENT)) * H-NEW-TECH-PCT.
261000
261100     IF H-LESSER-STOP-2 > 0
261200        IF H-LESSER-STOP-1 < H-LESSER-STOP-2
261300         MOVE H-LESSER-STOP-1 TO
261400                                H-NEW-TECH-ADDON
261500        ELSE
261600         MOVE H-LESSER-STOP-2 TO
261700                                H-NEW-TECH-ADDON
261800     ELSE
261900        MOVE ZEROES          TO H-NEW-TECH-ADDON.
262000
262100     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
262200             H-NEW-TECH-PAY-ADD-ON +
262300             H-NEW-TECH-ADDON.
262400
262500     MOVE 0 TO H-NEW-TECH-ADDON
262600               H-LESSER-STOP-1
262700               H-LESSER-STOP-2
262800               H-CSTMED-STOP.
262900
263000 4020-EXIT.    EXIT.
263100
263200***********************************************************
263300* TECHNICAL TRANSPLANTATION OF CELLS                      *
263400***********************************************************
263500 4100-ISLET-ISOLATION-ADD-ON.
263600
263700     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
263800
263900     IF  H-TECH-ADDON-ISLET-CNTR = 1
264000     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
264100           GO TO 4100-EXIT.
264200
264300     IF  H-TECH-ADDON-ISLET-CNTR > 1
264400     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
264500           GO TO 4100-EXIT.
264600
264700 4100-EXIT.    EXIT.
264800
264900***********************************************************
265000* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
265100* DISCHARGE COUNTS.
265200***********************************************************
265300*4400-LOWVOL-CODE-RTN.
265400*
265500*    SET LOWVOL-IDX TO 1.
265600*    SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
265700*        AT END
265800*          MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
265900*          MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
266000*      WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
266100*        MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
266200*                           TO MESWK-LOWVOL-PROV-DISCHG.
266300*
266400*4400-EXIT.   EXIT.
266500
266600*****************************************************************
266700* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR DISCHARGE *
266800* COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER FILE       *
266900*****************************************************************
267000 4410-UNCOMP-CARE-CODE-RTN.
267100
267200*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
267300*
267400*    SET UNCOMP-CARE-IDX TO 1.
267500*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
267600*        AT END
267700*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
267800*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
267900*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
268000*                           TO WK-UNCOMP-CARE-AMOUNT.
268100*
268200        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
268300
268400        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
268500
268600 4410-EXIT.   EXIT.
268700
268800
268900**************************************************************
269000* CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM *
269100**************************************************************
269200 5500-CAP-CALC-TECH-ADD-ON.
269300
269400     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
269500     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
269600
269700     COMPUTE H-OPER-BILL-COSTS ROUNDED =
269800         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
269900         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
270000
270100     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
270200                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
270300
270400     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
270500                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
270600
270700     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
270800         H-NEW-TECH-ADDON-CAPDIF  > 0
270900        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
271000             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
271100
271200 5500-EXIT.    EXIT.
271300
271400***********************************************************
271500 6000-CALC-READMIS-REDU.
271600***********************************************************
271700*---------------------------------------------------------*
271800* (YEARCHANGE 2016.0)
271900* READMISSIONS PROCESS ADJUSTMENTS
272000*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
272100*---------------------------------------------------------*
272200
272300     MOVE 0 TO H-READMIS-ADJUST-AMT.
272400
272500     IF P-HOSP-READMISSION-REDU = '1'
272600           GO TO 6000-EDIT-READMISN
272700     ELSE
272800           NEXT SENTENCE.
272900
273000     IF P-HOSP-READMISSION-REDU = '0' AND
273100        P-HOSP-HRR-ADJUSTMT = 0.0000
273200           MOVE ZEROES TO H-READMIS-ADJUST-AMT
273300           GO TO 6000-EXIT.
273400
273500     IF P-HOSP-READMISSION-REDU = '0' AND
273600        P-HOSP-HRR-ADJUSTMT > 0.0000
273700           MOVE 65 TO PPS-RTC
273800           MOVE ZEROES TO H-READMIS-ADJUST-AMT
273900           GO TO 6000-EXIT.
274000
274100     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
274200                                  '6' OR '7' OR '8' OR
274300                                  '9' OR ' '
274400           MOVE 65 TO PPS-RTC
274500           MOVE ZEROES TO H-READMIS-ADJUST-AMT
274600           GO TO 6000-EXIT.
274700
274800 6000-EDIT-READMISN.
274900
275000     IF P-HOSP-HRR-ADJUSTMT < 0.9700
275100           MOVE 65 TO PPS-RTC
275200           MOVE ZEROES TO H-READMIS-ADJUST-AMT
275300           GO TO 6000-EXIT.
275400
275500     IF P-HOSP-HRR-ADJUSTMT > 1.0000
275600           MOVE 65 TO PPS-RTC
275700           MOVE ZEROES TO H-READMIS-ADJUST-AMT
275800           GO TO 6000-EXIT.
275900
276000     IF P-READ-INVALID-STATE
276100           MOVE 65 TO PPS-RTC
276200           MOVE ZEROES TO H-READMIS-ADJUST-AMT
276300           GO TO 6000-EXIT.
276400
276500 6000-COMPUTE-READMISN.
276600
276700        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
276800              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
276900                H-OPER-BASE-DRG-PAY).
277000
277100 6000-EXIT.    EXIT.
277200
277300***********************************************************
277400 7000-CALC-VALUE-BASED-PURCH.
277500***********************************************************
277600*---------------------------------------------------------*
277700* (YEARCHANGE 2016.0)
277800* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
277900*   + FY17: RANGE OF ALLOWABLE FACTORS (< 0.98 OR > 2.0)
278000*---------------------------------------------------------*
278100
278200     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
278300
278400     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
278500           NEXT SENTENCE
278600     ELSE
278700           MOVE 68 TO PPS-RTC
278800           GO TO 7000-EXIT.
278900
279000     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
279100           GO TO 7000-EXIT.
279200
279300     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
279400         P-NEW-CBSA-HOSP-QUAL-IND = '1'
279500           NEXT SENTENCE
279600     ELSE
279700           MOVE 68 TO PPS-RTC
279800           GO TO 7000-EXIT.
279900
280000     IF  P-VBP-INVALID-STATE
280100           MOVE 68 TO PPS-RTC
280200           GO TO 7000-EXIT
280300     ELSE
280400           NEXT SENTENCE.
280500
280600     IF P-VAL-BASED-PURCH-ADJUST < 0.9800000000 OR
280700        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
280800           MOVE 68 TO PPS-RTC
280900           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
281000           GO TO 7000-EXIT
281100     ELSE
281200           GO TO 7000-COMPUTE-VAL-BASED-PUR.
281300
281400 7000-COMPUTE-VAL-BASED-PUR.
281500
281600     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
281700              ((P-VAL-BASED-PURCH-ADJUST *
281800                  H-OPER-BASE-DRG-PAY) -
281900                  H-OPER-BASE-DRG-PAY).
282000
282100 7000-EXIT.    EXIT.
282200
282300***********************************************************
282400 8000-CALC-BUNDLE-REDU.
282500***********************************************************
282600***** CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
282700***********************************************************
282800
282900     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
283000     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
283100
283200     IF '61' =  B-DEMO-CODE1  OR
283300                B-DEMO-CODE2  OR
283400                B-DEMO-CODE3  OR
283500                B-DEMO-CODE4
283600         NEXT SENTENCE
283700     ELSE
283800         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
283900           GO TO 8000-EXIT.
284000
284100     IF P-MODEL1-BUNDLE-DISPRCNT > .00
284200           GO TO 8000-COMPUTE-BUNDLE
284300     ELSE
284400           NEXT SENTENCE.
284500
284600     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
284700           GO TO 8000-EXIT.
284800
284900 8000-COMPUTE-BUNDLE.
285000
285100     IF  B-DISCHARGE-DATE < 20140401 AND
285200         P-MODEL1-BUNDLE-DISPRCNT = .01
285300         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
285400          (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
285500     ELSE
285600         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
285700          (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
285800
285900        COMPUTE H-BUNDLE-ADJUST-AMT      ROUNDED =
286000              ((WK-MODEL1-BUNDLE-DISPRCNT *
286100                                     H-OPER-BASE-DRG-PAY) -
286200                H-OPER-BASE-DRG-PAY).
286300
286400        COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
286500
286600 8000-EXIT.    EXIT.
286700
286800***********************************************************
286900 9000-CALC-EHR-SAVING.
287000***********************************************************
287100*---------------------------------------------------------*
287200* (YEARCHANGE 2020.0)
287300* CASES INVOLVING EHR SAVINGS
287400*   + FY20: ANNUAL UPDATE TO BELOW VALUES
287500*   + EHR-FULL = FULL MB / NO EHR MB
287600*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
287700*---------------------------------------------------------*
287800
287900     MOVE 1.022421525 TO H-MB-RATIO-EHR-FULL.
288000     MOVE 1.022590361 TO H-MB-RATIO-EHR-QUAL-FULL.
288100     MOVE 0 TO H-EHR-SUBSAV-QUANT.
288200     MOVE 0 TO H-EHR-SUBSAV-LV.
288300     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
288400     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
288500
288600     IF P-EHR-REDUC-IND = 'Y'
288700         NEXT SENTENCE
288800     ELSE
288900         GO TO 9000-EXIT.
289000
289100 9000-COMPUTE-EHR.
289200
289300* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
289400* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
289500* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
289600* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
289700* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
289800* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
289900* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
290000* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
290100* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
290200* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
290300* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
290400* OUT.
290500*
290600* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
290700* SAVINGS CALCULATION SO CALCULATE AN OPERATING
290800* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
290900* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
291000* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
291100
291200      COMPUTE H-EHR-SUBSAV-QUANT =
291300           (PPS-OPER-HSP-PART +
291400            PPS-OPER-FSP-PART +
291500            PPS-OPER-DSH-ADJ +
291600            PPS-OPER-IME-ADJ +
291700            H-READMIS-ADJUST-AMT +
291800            H-VAL-BASED-PURCH-ADJUST-AMT +
291900            H-BUNDLE-ADJUST-AMT).
292000
292100* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
292200* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
292300* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
292400* READM VBP AND BUNDLE
292500* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
292600* FOR EHR SAVINGS FIELD;
292700
292800      MOVE 0 TO H-EHR-SUBSAV-LV.
292900
293000      IF P-NEW-TEMP-RELIEF-IND = 'Y'
293100         AND P-LV-ADJ-FACTOR > 0.00
293200         AND P-LV-ADJ-FACTOR <= 0.25
293300      COMPUTE H-EHR-SUBSAV-LV =
293400          (PPS-OPER-HSP-PART +
293500           PPS-OPER-FSP-PART +
293600           PPS-OPER-DSH-ADJ +
293700           PPS-OPER-IME-ADJ ) * P-LV-ADJ-FACTOR.
293800
293900      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
294000           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
294100
294200* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
294300* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
294400* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
294500* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
294600* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
294700* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
294800* OF .625 ONLY.
294900
295000       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
295100       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
295200
295300     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
295400        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
295500          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
295600
295700        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
295800          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
295900
296000 9000-EXIT.    EXIT.
296100
296200*---------------------------------------------------------*
296300* (YEARCHANGE 2016.0)
296400*---------------------------------------------------------*
296500 9010-CALC-STANDARD-CHG.
296600
296700***********************************************************
296800***ÝCM-P3¨ STANDARDIZED OPERATING COST CALCULATION
296900
297000     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
297100               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
297200        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
297300        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
297400        ((H-LABOR-PCT * H-WAGE-INDEX) +
297500               (H-NONLABOR-PCT * H-OPER-COLA))
297600     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
297700
297800***********************************************************
297900***ÝCM-P3¨ STANDARDIZED CAPITAL COST CALCULATION
298000
298100     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
298200       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
298300        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
298400               (H-CAPI-GAF * H-CAPI-COLA)
298500     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
298600
298700***********************************************************
298800***ÝCM-P3¨ STANDARDIZED OPERATING TRESHOLD
298900
299000     MOVE 5796.63 TO H-OPER-BASE.
299100
299200     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
299300      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
299400                        +
299500           (H-OPER-BASE * H-DRG-WT-FRCTN)
299600                        +
299700              H-NEW-TECH-PAY-ADD-ON.
299800
299900******************************************************
300000***ÝCM-P3¨ STANDARDIZED CAPITAL TRESHOLD
300100
300200     MOVE 462.33 TO H-CAPI-BASE.
300300
300400     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
300500     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
300600                     +
300700     (H-CAPI-BASE * H-DRG-WT-FRCTN).
300800
300900******************************************************
301000***ÝCM-P3¨ STANDARDIZED OPERATING OUTLIER CALCULATION
301100
301200     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
301300        (H-OPER-STDZ-DOLLAR-THRESHOLD +
301400                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
301500                          AND
301600         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
301700
301800       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
301900        (H-CSTOUT-PCT  *
302000        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
302100
302200     ELSE
302300       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
302400
302500******************************************************
302600***ÝCM-P3¨ STANDARDIZED CAPITAL OUTLIER CALCULATION
302700
302800     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
302900        (H-OPER-STDZ-DOLLAR-THRESHOLD +
303000                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
303100                          AND
303200         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
303300
303400      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
303500      (H-CSTOUT-PCT  *
303600      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
303700     ELSE
303800      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
303900
304000*******************************************************
304100***ÝCM-P3¨ STANDARDIZED ALLOWED AMOUNT CALCULATION
304200
304300      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
304400       (H-OPER-BASE + H-CAPI-BASE)
304500                 *
304600       H-DRG-WT-FRCTN
304700                 +
304800       H-OPER-STDZ-COST-OUTLIER
304900                 +
305000       H-CAPI-STDZ-COST-OUTLIER
305100                 +
305200       H-NEW-TECH-PAY-ADD-ON.
305300
305400 9010-EXIT.    EXIT.
305500
305600************************************************************************
305700 10000-COVID19-FLAG.
305800************************************************************************
305900
306000     MOVE B-DIAGNOSIS-CODE(IDX-COVID) TO WK-DIAG-COVID19.
306100
306200     IF DIAG-COVID1
306300       MOVE 'Y' TO DIAG-COVID1-FLAG.
306400
306500     IF DIAG-COVID2
306600       MOVE 'Y' TO DIAG-COVID2-FLAG.
306700
306800 10000-EXIT.    EXIT.
306900
307000************************************************************************
307100 10100-COVID19-COND-FLAG.
307200************************************************************************
307300
307400     MOVE B-CONDITION-CODE(IDX-COVID-COND) TO WK-COND-COVID19.
307500
307600     IF COND-COVID19-NOADJ
307700       MOVE 'Y' TO COND-COVID1-FLAG.
307800
307900 10100-EXIT.    EXIT.
308000************************************************************************
