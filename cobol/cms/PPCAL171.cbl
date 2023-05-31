000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL171.
000300*REVISED.                   06-02-2016.
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
002000     'PPCAL171      - W O R K I N G   S T O R A G E'.
002100 01  CAL-VERSION                    PIC X(05)  VALUE 'C17.1'.
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
004500 01  IDX-TECH                       PIC S9(02).
004600
004700*---------------------------------------------------------*
004800* (YEARCHANGE 2017.0)
004900* LABOR & NON-LABOR RATES TABLE
005000*---------------------------------------------------------*
005100
005200 COPY RATEX170.
005300
005400*---------------------------------------------------------*
005500* (YEARCHANGE 2017.0)
005600* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE
005700*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE
005800*---------------------------------------------------------*
005900
006000 COPY DRGSX170.
006100
006200*---------------------------------------------------------*
006300* (YEARCHANGE 2017.0)
006400* TWO MIDNIGHT STAY POLICY ADJUSTMENT FACTOR TABLE
006500*---------------------------------------------------------*
006600
006700 COPY MIDNIGHT.
006800
006900***********************************************************
007000*****YEARCHANGE 2015.0 ************************************
007100***********************************************************
007200***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
007300***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
007400***********************************************************
007500
007600 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
007700 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
007800 01  MES-PPS-PROV                   PIC X(06).
007900 01  MES-PPS-STATE                  PIC X(02).
008000 01  MES-INTRO                      PIC X(53) VALUE SPACES.
008100 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
008200 01  MES-SSRFBN.
008300     05 MES-SSRFBN-STATE PIC 99.
008400     05 FILLER           PIC XX.
008500     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
008600     05 FILLER           PIC XX.
008700     05 MES-SSRFBN-CODE2 PIC 99.
008800     05 FILLER           PIC XX.
008900     05 MES-SSRFBN-STNAM PIC X(20).
009000     05 MES-SSRFBN-REST  PIC X(22).
009100
009200 01 WK-HLDDRG-DATA.
009300     05  HLDDRG-DATA.
009400         10  HLDDRG-DRGX               PIC X(03).
009500         10  FILLER1                   PIC X(01).
009600         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
009700         10  FILLER2                   PIC X(01).
009800         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
009900         10  FILLER3                   PIC X(05).
010000         10  HLDDRG-LOW                PIC X(01).
010100         10  FILLER5                   PIC X(01).
010200         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
010300         10  FILLER6                   PIC X(02).
010400         10  HLDDRG-PAC                PIC X(01).
010500         10  FILLER7                   PIC X(01).
010600         10  HLDDRG-SPPAC              PIC X(01).
010700         10  FILLER8                   PIC X(02).
010800         10  HLDDRG-DESC               PIC X(26).
010900
011000 01 WK-HLDDRG-DATA2.
011100     05  HLDDRG-DATA2.
011200         10  HLDDRG-DRGX2               PIC X(03).
011300         10  FILLER21                   PIC X(01).
011400         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
011500         10  FILLER22                   PIC X(01).
011600         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
011700         10  FILLER23                   PIC X(05).
011800         10  HLDDRG-LOW2                PIC X(01).
011900         10  FILLER25                   PIC X(01).
012000         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
012100         10  FILLER26                   PIC X(02).
012200         10  HLDDRG-TRANS-FLAGS.
012300                   88  D-DRG-POSTACUTE-50-50
012400                   VALUE 'Y Y'.
012500                   88  D-DRG-POSTACUTE-PERDIEM
012600                   VALUE 'Y  '.
012700             15  HLDDRG-PAC2            PIC X(01).
012800             15  FILLER27               PIC X(01).
012900             15  HLDDRG-SPPAC2          PIC X(01).
013000         10  FILLER28                   PIC X(02).
013100         10  HLDDRG-DESC2               PIC X(26).
013200         10  HLDDRG-VALID               PIC X(01).
013300
013400 01  MES-LOWVOL.
013500     05  MES-LOWVOL-PROV             PIC X(6).
013600     05  FILLER                      PIC XXX.
013700     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
013800
013900 01  WK-UNCOMP-CARE.
014000     05  WK-UNCOMP-CARE-PROV         PIC X(6).
014100     05  FILLER                      PIC X.
014200     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
014300
014400 01 WK-HLD-MID-DATA.
014500     05  HLD-MID-DATA.
014600         10  HLD-MID-MSAX              PIC X(04).
014700         10  FILLER1                   PIC X(01).
014800         10  HLD-MID-ADJ-FACT          PIC 9(02)V9(06).
014900
015000 01  HLD-PPS-DATA.
015100         10  HLD-PPS-RTC                PIC 9(02).
015200         10  HLD-PPS-WAGE-INDX          PIC 9(02)V9(04).
015300         10  HLD-PPS-OUTLIER-DAYS       PIC 9(03).
015400         10  HLD-PPS-AVG-LOS            PIC 9(02)V9(01).
015500         10  HLD-PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
015600         10  HLD-PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
015700         10  HLD-PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
015800         10  HLD-PPS-OPER-HSP-PART      PIC 9(06)V9(02).
015900         10  HLD-PPS-OPER-FSP-PART      PIC 9(06)V9(02).
016000         10  HLD-PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
016100         10  HLD-PPS-REG-DAYS-USED      PIC 9(03).
016200         10  HLD-PPS-LTR-DAYS-USED      PIC 9(02).
016300         10  HLD-PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
016400         10  HLD-PPS-CALC-VERS          PIC X(05).
016500
016600 01  WK-NEW-TECH-VARIABLES.
016700     05  WK-PROC-NEW-TECH       PIC X(07).
016800             88  PROC-ARGUS
016900                   VALUE '08H005Z' '08H105Z'.
017000             88  PROC-BLINATU
017100                   VALUE 'XW03351' 'XW04351'.
017200             88  PROC-CARDIO
017300                   VALUE '02HQ30Z' '02HR30Z'.
017400             88  PROC-DEFITELIO
017500                   VALUE 'XW03392' 'XW04392'.
017600             88  PROC-GORE
017700                   VALUE '04VC0EZ' '04VC0FZ' '04VC3EZ' '04VC3FZ'
017800                         '04VC4EZ' '04VC4FZ' '04VD0EZ' '04VD0FZ'
017900                         '04VD3EZ' '04VD3FZ' '04VD4EZ' '04VD4FZ'.
018000             88  PROC-IDARUCIZ
018100                   VALUE 'XW03331' 'XW04331'.
018200             88  PROC-ISLET
018300                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
018400                         '3E0J8U1'.
018500             88  PROC-KCENTRA
018600                   VALUE '30283B1'.
018700             88  PROC-LUTONIX
018800                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
018900                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
019000                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
019100                         '047L341' '047L3D1' '047L3Z1' '047L441'
019200                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
019300                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
019400                         '047M441' '047M4D1' '047M4Z1' '047N041'
019500                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
019600                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
019700             88  PROC-MAGEC
019800                   VALUE 'XNS0032' 'XNS0432' 'XNS3032' 'XNS3432'
019900                         'XNS4032' 'XNS4432'.
020000             88  PROC-MITRACLP
020100                   VALUE '02UG3JZ'.
020200             88  PROC-RNSSYS1
020300                   VALUE '0NH00NZ'.
020400             88  PROC-RNSSYS2
020500                   VALUE '00H00MZ'.
020600             88  PROC-VISTOGARD
020700                   VALUE 'XW0DX82'.
020800             88  PROC-VORAXAZE
020900                   VALUE '3E033GQ' '3E043GQ'.
021000             88  PROC-ZENITH
021100                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
021200             88  PROC-ZILVER
021300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
021400                         '047L34Z' '047L44Z'.
021500
021600     05  WK-DIAG-NEW-TECH       PIC X(07).
021700             88  DIAG-AUTOLITT
021800                   VALUE '1910   ' '1911   ' '1912   ' '1913  '
021900                         '1914   ' '1915   ' '1916   ' '1917  '
022000                         '1918   ' '1919   ' 'C710   ' 'C711  '
022100                         'C712   ' 'C713   ' 'C714   ' 'C715  '
022200                         'C716   ' 'C717   ' 'C718   ' 'C719  '.
022300             88  DIAG-ISLET
022400                   VALUE 'Z006   '.
022500             88  DIAG-KCENTRA
022600                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
022700                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
022800                         'D6832  ' 'D684   '.
022900             88  DIAG-VISTOGARD
023000                   VALUE 'T451X1A' 'T451X1D' 'T451X1S' 'T451X5A'
023100                         'T451X5D' 'T451X5S'.
023200
023300     05  WK-NEW-TECH-FLAGS.
023400         10  PROC-ARGUS-FLAG         PIC X(01).
023500         10  PROC-BLINATU-FLAG       PIC X(01).
023600         10  PROC-CARDIO-FLAG        PIC X(01).
023700         10  PROC-DEFITELIO-FLAG     PIC X(01).
023800         10  PROC-GORE-FLAG          PIC X(01).
023900         10  PROC-IDARUCIZ-FLAG      PIC X(01).
024000         10  PROC-ISLET-FLAG         PIC X(01).
024100         10  PROC-KCENTRA-FLAG       PIC X(01).
024200         10  PROC-LUTONIX-FLAG       PIC X(01).
024300         10  PROC-MAGEC-FLAG         PIC X(01).
024400         10  PROC-MITRACLP-FLAG      PIC X(01).
024500         10  PROC-RNSSYS1-FLAG       PIC X(01).
024600         10  PROC-RNSSYS2-FLAG       PIC X(01).
024700         10  PROC-VISTOGARD-FLAG     PIC X(01).
024800         10  PROC-VORAXAZE-FLAG      PIC X(01).
024900         10  PROC-ZENITH-FLAG        PIC X(01).
025000         10  PROC-ZILVER-FLAG        PIC X(01).
025100         10  DIAG-AUTOLITT-FLAG      PIC X(01).
025200         10  DIAG-ISLET-FLAG         PIC X(01).
025300         10  DIAG-KCENTRA-FLAG       PIC X(01).
025400         10  DIAG-VISTOGARD-FLAG     PIC X(01).
025500
025600 LINKAGE SECTION.
025700***************************************************************
025800*                 * * * * * * * * *                           *
025900*    REVIEW CODES ARE USED TO DIRECT THE PPCAL  SUBROUTINE    *
026000*    IN HOW TO PAY THE BILL.                                  *
026100*                         *****                               *
026200*    COMMENTS  ** CLAIMS RECEIVED WITH CONDITION CODE 66      *
026300*                 SHOULD BE PROCESSED UNDER REVIEW CODE 06,   *
026400*                 07 OR 11 AS APPROPRIATE TO EXCLUDE ANY      *
026500*                 OUTLIER COMPUTATION.                        *
026600*                         *****                               *
026700*         REVIEW-CODE:                                        *
026800*            00 = PAY-WITH-OUTLIER.                           *
026900*                 WILL CALCULATE THE STANDARD PAYMENT.        *
027000*                 WILL ALSO ATTEMPT TO PAY ONLY COST          *
027100*                 OUTLIERS, DAY OUTLIERS EXPIRED 10/01/97     *
027200*            03 = PAY-PERDIEM-DAYS.                           *
027300*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
027400*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
027500*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
027600*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
027700*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
027800*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
027900*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
028000*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
028100*                 BILL EXCEED THE COST THRESHOLD.             *
028200*            06 = PAY-XFER-NO-COST                            *
028300*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
028400*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
028500*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
028600*                 FOR THE DRG.  IF COVERED DAYS EQUAL OR      *
028700*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
028800*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
028900*                 CALCULATE ANY COST OUTLIER PORTION          *
029000*                 OF THE PAYMENT.                             *
029100*            07 = PAY-WITHOUT-COST.                           *
029200*                 WILL CALCULATE THE STANDARD PAYMENT         *
029300*                 WITHOUT COST PORTION.                       *
029400*            09 = PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS    *
029500*                 50-50> NOW USES Y INDICATORS ON DRGS
029600*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
029700*                        RULE
029800* =======================================================
029900* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
030000* =======================================================
030100*
030200*
030300*     FULL PERDIEM >   NOW USES Y INDICATORS ON DRGS
030400*                      SEE TABLE 5 FROM ANNUAL IPPS FINAL
030500*                      RULE
030600*
030700*                               POST-ACUTE TRANSFERS          *
030800*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
030900*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
031000*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
031100*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
031200*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
031300*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
031400*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
031500*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
031600*                 BILL EXCEED THE COST THRESHOLD.             *
031700*            11 = PAY-XFER-SPEC-DRG-NO-COST                   *
031800*                 POST-ACUTE TRANSFERS                        *
031900*                 50-50> NOW USES Y INDICATORS ON DRGS
032000*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
032100*                        RULE
032200* =======================================================
032300* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
032400* =======================================================
032500*
032600*     FULL PERDIEM >  NOW USES Y INDICATORS ON DRGS
032700*                     SEE TABLE 5
032800*
032900*
033000*                               POST-ACUTE TRANSFERS          *
033100*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
033200*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
033300*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
033400*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
033500*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
033600*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
033700*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
033800*                 PAYMENT.                                    *
033900***************************************************************
034000
034100**************************************************************
034200*      MILLINNIUM COMPATIBLE                                 *
034300*      THIS IS THE BILL-RECORD THAT WILL BE PASSED BACK FROM *
034400*      THE PPCAL001 PROGRAM AND AFTER FOR PROCESSING         *
034500*      IN THE NEW FORMAT                                     *
034600**************************************************************
034700 01  BILL-NEW-DATA.
034800         10  B-NPI10.
034900             15  B-NPI8             PIC X(08).
035000             15  B-NPI-FILLER       PIC X(02).
035100         10  B-PROVIDER-NO          PIC X(06).
035200             88  B-FORMER-MDH-PROVIDERS
035300                                      VALUE '080006' '140184'
035400                                            '390072' '420019'
035500                                            '440031' '450451'
035600                                            '490019' '510062'.
035700         10  B-REVIEW-CODE          PIC 9(02).
035800             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
035900             88  PAY-WITH-OUTLIER     VALUE 00 07.
036000             88  PAY-PERDIEM-DAYS     VALUE 03.
036100             88  PAY-XFER-NO-COST     VALUE 06.
036200             88  PAY-WITHOUT-COST     VALUE 07.
036300             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
036400             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
036500         10  B-DRG                  PIC 9(03).
036600             88  B-DRG-SPIRATN-DRG
036700                   VALUE 163 164 165.
036800             88  B-DRG-SPIRATN-DRG11
036900                   VALUE 199 200 201.
037000             88  B-DRG-AUTOLITT-DRG
037100                   VALUE 25 26 27.
037200
037300* =======================================================
037400* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
037500* =======================================================
037600*
037700*            88  B-DRG-POSTACUTE-PERDIEM
037800*                         VALUE  NOW USES Y INDICATORS ON DRGS
037900*                         SEE TABLE 5
038000*                         D-DRG-POSTACUTE-PERDIEM
038100
038200         10  B-LOS                  PIC 9(03).
038300         10  B-COVERED-DAYS         PIC 9(03).
038400         10  B-LTR-DAYS             PIC 9(02).
038500         10  B-DISCHARGE-DATE.
038600             15  B-DISCHG-CC        PIC 9(02).
038700             15  B-DISCHG-YY        PIC 9(02).
038800             15  B-DISCHG-MM        PIC 9(02).
038900             15  B-DISCHG-DD        PIC 9(02).
039000         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
039100         10  B-PROCEDURE-CODE-TABLE.
039200             15  B-PROCEDURE-CODE    PIC X(07) OCCURS 25 TIMES
039300                 INDEXED BY IDX-DIAG.
039400         10  B-DIAGNOSIS-CODE-TABLE.
039500             15  B-DIAGNOSIS-CODE    PIC X(07) OCCURS 25 TIMES
039600                 INDEXED BY IDX-PROC.
039700         10  B-DEMO-DATA.
039800             15  B-DEMO-CODE1           PIC X(02).
039900             15  B-DEMO-CODE2           PIC X(02).
040000             15  B-DEMO-CODE3           PIC X(02).
040100             15  B-DEMO-CODE4           PIC X(02).
040200         10  B-NDC-DATA.
040300             15  B-NDC-NUMBER           PIC X(11).
040400               88  B-NDC-DIFICID-NDC
040500                   VALUE '52015008001'.
040600         10  FILLER                     PIC X(73).
040700
040800
040900***************************************************************
041000*    THIS DATA IS CALCULATED BY THIS PPCAL  SUBROUTINE        *
041100*    AND PASSED BACK TO THE CALLING PROGRAM                   *
041200*            RETURN CODE VALUES (PPS-RTC)                     *
041300*                                                             *
041400*            PPS-RTC 00-49 = HOW THE BILL WAS PAID            *
041500*                                                             *
041600*      PPS-RTC 30,33,40,42,44  = OUTLIER RECONCILIATION       *
041700*                                                             *
041800*           30,00 = PAID NORMAL DRG PAYMENT                   *
041900*                                                             *
042000*              01 = PAID AS A DAY-OUTLIER.                    *
042100*                   NOTE:                                     *
042200*                     DAY-OUTLIER NO LONGER BEING PAID        *
042300*                         AS OF 10/01/97                      *
042400*                                                             *
042500*              02 = PAID AS A COST-OUTLIER.                   *
042600*                                                             *
042700*           33,03 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
042800*                   AND INCLUDING THE FULL DRG.               *
042900*              05 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
043000*                   AND INCLUDING THE FULL DRG WHICH ALSO     *
043100*                   QUALIFIED FOR A COST OUTLIER PAYMENT.     *
043200*              06 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
043300*                   AND INCLUDING THE FULL DRG. PROVIDER      *
043400*                   REFUSED COST OUTLIER.                     *
043500*           40,10 = POST-ACUTE TRANSFER                       *
043600*                   SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
043700*
043800* =======================================================
043900* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
044000* =======================================================
044100*
044200*           42,12 = POST-ACAUTE TRANSFER WITH SPECIFIC DRGS   *
044300*                       THE FOLLOWING DRG'S                   *
044400*                   DRG =  VALUE  NOW USES Y INDICATORS ON DRGS
044500*                       SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
044600*                          D-DRG-POSTACUTE-PERDIEM
044700*
044800*           44,14 = PAID NORMAL DRG PAYMENT WITH              *
044900*                    PERDIEM DAYS = OR > GM  ALOS             *
045000*              16 = PAID AS A COST-OUTLIER WITH               *
045100*                    PERDIEM DAYS = OR > GM  ALOS             *
045200*                                                             *
045300*            PPS-RTC 50-99 = WHY THE BILL WAS NOT PAID        *
045400*              51 = NO PROVIDER SPECIFIC INFO FOUND           *
045500*              52 = INVALID CBSA# IN PROVIDER FILE            *
045600*                   OR INVALID WAGE INDEX                     *
045700*                                      OR                     *
045800*                   INVALID PROVIDER TYPES ON PROVIDER FILE   *
045900*              53 = WAIVER STATE - NOT CALCULATED BY PPS OR   *
046000*                   OR                                         *
046100*                   INVALID STATE CODE IN COMBINATION WITH     *
046200*                   HAC FLAG                                  *
046300*              54 = INVALID DRG                               *
046400*              55 = DISCHARGE DATE < PROVIDER EFF START DATE  *
046500*                                      OR                     *
046600*                   DISCHARGE DATE < CBSA EFF START DATE      *
046700*                   FOR PPS                                   *
046800*                                      OR                     *
046900*                   PROVIDER HAS BEEN TERMINATED ON OR BEFORE *
047000*                   DISCHARGE DATE                            *
047100*              56 = INVALID LENGTH OF STAY                    *
047200*              57 = REVIEW CODE INVALID (NOT 00 03 06 07 09   *
047300*                                        NOT 11)              *
047400*              58 = TOTAL CHARGES NOT NUMERIC                 *
047500*              61 = LIFETIME RESERVE DAYS NOT NUMERIC         *
047600*                   OR BILL-LTR-DAYS > 60                     *
047700*              62 = INVALID NUMBER OF COVERED DAYS            *
047800*              65 = PAY-CODE NOT = A,B OR C ON PROVIDER        *
047900*                   SPECIFIC FILE FOR CAPITAL                  *
048000*                   OR                                         *
048100*                   INVALID READMISSION FLAG IN PSF FILE       *
048200*                   OR                                         *
048300*                   BLANK READMISSION FLAG IN PSF FILE         *
048400*                   OR                                         *
048500*                   READMISSION ADJUSTMENT IS INVALID OR       *
048600*                   OUT OF RANGE IN PSF FILE                   *
048700*                   OR                                         *
048800*                   BLANK READMISSION ADJUSTMENT IN PSF FILE   *
048900*                   OR                                         *
049000*                   INVALID STATE CODE IN COMBINATION WITH     *
049100*                   READMISSION FLAG IN PSF FILE               *
049200*                   OR                                         *
049300*                   INVALID EHR FLAG IN PSF FILE               *
049400*                   (MUST BE A "Y" OR BLANK)                   *
049500*              67 = COST OUTLIER WITH LOS > COVERED DAYS      **
049600*                   OR COST OUTLIER THRESHOLD CALUCULATION    **
049700*              68 = INVALID VALUE BASED PURCHASE FLAG IN PSF   *
049800*                   FILE                                       *
049900*                   OR                                         *
050000*                   BLANK VALUE BASED PURCHASE FLAG IN PSF FILE*
050100*                   OR                                         *
050200*                   VALUE BASED PURCHASE ADJUSTMEMT IS INVALID *
050300*                   OR OUT OF RANGE IN PSF FILE                *
050400*                   INDICATOR                                  *
050500*                   OR                                         *
050600*                   BLANK VALUE BASED PURCHASE ADJUSTMEMT IN   *
050700*                   PSF FILE                                   *
050800*                   OR                                         *
050900*                   INVALID COMBINATION OF HOSPITAL QUALITY    *
051000*                   INDICATOR                                  *
051100*                   AND VALUE BASED PURCHASE FLAG IN PSF FILE  *
051200*                   OR                                         *
051300*                   INVALID STATE CODE IN COMBINATION WITH VALUE
051400*                   BASED PURCHASE FLAG IN PSF FILE            *
051500*              98 = CANNOT PROCESS BILL OLDER THAN 5 YEARS    *
051600***************************************************************
051700 01  PPS-DATA.
051800         10  PPS-RTC                PIC 9(02).
051900         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
052000         10  PPS-OUTLIER-DAYS       PIC 9(03).
052100         10  PPS-AVG-LOS            PIC 9(02)V9(01).
052200         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
052300         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
052400         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
052500         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
052600         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
052700         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
052800         10  PPS-REG-DAYS-USED      PIC 9(03).
052900         10  PPS-LTR-DAYS-USED      PIC 9(02).
053000         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
053100         10  PPS-CALC-VERS          PIC X(05).
053200
053300*****************************************************************
053400*            THESE ARE THE VERSIONS OF THE PPCAL
053500*           PROGRAMS THAT WILL BE PASSED BACK----
053600*          ASSOCIATED WITH THE BILL BEING PROCESSED
053700*****************************************************************
053800 01  PRICER-OPT-VERS-SW.
053900     02  PRICER-OPTION-SW          PIC X(01).
054000         88  ALL-TABLES-PASSED          VALUE 'A'.
054100         88  PROV-RECORD-PASSED         VALUE 'P'.
054200         88  ADDITIONAL-VARIABLES       VALUE 'M'.
054300         88  PC-PRICER                  VALUE 'C'.
054400     02  PPS-VERSIONS.
054500         10  PPDRV-VERSION         PIC X(05).
054600
054700*****************************************************************
054800*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
054900*          ASSOCIATED WITH THE BILL BEING PROCESSED
055000*****************************************************************
055100 01  PPS-ADDITIONAL-VARIABLES.
055200     05  PPS-HSP-PCT                PIC 9(01)V9(02).
055300     05  PPS-FSP-PCT                PIC 9(01)V9(02).
055400     05  PPS-NAT-PCT                PIC 9(01)V9(02).
055500     05  PPS-REG-PCT                PIC 9(01)V9(02).
055600     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
055700     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
055800     05  PPS-DRG-WT                 PIC 9(02)V9(04).
055900     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
056000     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
056100     05  PPS-REG-LABOR              PIC 9(05)V9(02).
056200     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
056300     05  PPS-OPER-COLA              PIC 9(01)V9(03).
056400     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
056500     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
056600     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
056700     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
056800     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
056900     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
057000     05  PPS-CAPITAL-VARIABLES.
057100         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
057200         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
057300         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
057400         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
057500         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
057600         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
057700         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
057800         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
057900     05  PPS-CAPITAL2-VARIABLES.
058000         10  PPS-CAPI2-PAY-CODE             PIC X(1).
058100         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
058200         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
058300     05  PPS-OTHER-VARIABLES.
058400         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
058500         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
058600         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
058700         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
058800         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
058900         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
059000         10  PPS-HOSP-READMISSION-REDU      PIC X.
059100         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
059200         10  PPS-OPERATNG-DATA.
059300             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
059400             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
059500             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
059600     05  PPS-PC-OTH-VARIABLES.
059700         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
059800         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
059900         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
060000         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
060100         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
060200         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
060300         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
060400         10  PPS-PC-HMO-FLAG                PIC X(01).
060500         10  PPS-PC-COT-FLAG                PIC X(01).
060600         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
060700         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
060800     05  PPS-ADDITIONAL-PAY-INFO-DATA.
060900         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
061000         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
061100         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
061200         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
061300     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
061400         10  PPS-HAC-PROG-REDUC-IND      PIC X.
061500         10  PPS-EHR-PROG-REDUC-IND      PIC X.
061600         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
061700         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
061800         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
061900         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
062000     05 PPS-FILLER                       PIC X(0897).
062100
062200 01  PROV-NEW-HOLD.
062300     02  PROV-NEWREC-HOLD1.
062400         05  P-NEW-NPI10.
062500             10  P-NEW-NPI8             PIC X(08).
062600             10  P-NEW-NPI-FILLER       PIC X(02).
062700         05  P-NEW-PROVIDER-NO.
062800             88  P-NEW-DSH-ADJ-PROVIDERS
062900                             VALUE '180049' '190044' '190144'
063000                                   '190191' '330047' '340085'
063100                                   '370016' '370149' '420043'.
063200             10  P-NEW-STATE            PIC 9(02).
063300                 88  P-VBP-INVALID-STATE
063400                             VALUE 21 80 40 84.
063500                 88  P-READ-INVALID-STATE
063600                             VALUE 40 84.
063700                 88  P-HAC-INVALID-STATE
063800                             VALUE 40 84.
063900                 88  P-PR-NEW-STATE
064000                             VALUE 40 84.
064100             10  FILLER                 PIC X(04).
064200         05  P-NEW-DATE-DATA.
064300             10  P-NEW-EFF-DATE.
064400                 15  P-NEW-EFF-DT-CC    PIC 9(02).
064500                 15  P-NEW-EFF-DT-YY    PIC 9(02).
064600                 15  P-NEW-EFF-DT-MM    PIC 9(02).
064700                 15  P-NEW-EFF-DT-DD    PIC 9(02).
064800             10  P-NEW-FY-BEGIN-DATE.
064900                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
065000                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
065100                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
065200                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
065300             10  P-NEW-REPORT-DATE.
065400                 15  P-NEW-REPORT-DT-CC PIC 9(02).
065500                 15  P-NEW-REPORT-DT-YY PIC 9(02).
065600                 15  P-NEW-REPORT-DT-MM PIC 9(02).
065700                 15  P-NEW-REPORT-DT-DD PIC 9(02).
065800             10  P-NEW-TERMINATION-DATE.
065900                 15  P-NEW-TERM-DT-CC   PIC 9(02).
066000                 15  P-NEW-TERM-DT-YY   PIC 9(02).
066100                 15  P-NEW-TERM-DT-MM   PIC 9(02).
066200                 15  P-NEW-TERM-DT-DD   PIC 9(02).
066300         05  P-NEW-WAIVER-CODE          PIC X(01).
066400             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
066500         05  P-NEW-INTER-NO             PIC 9(05).
066600         05  P-NEW-PROVIDER-TYPE        PIC X(02).
066700             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
066800             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
066900                                                  '15' '17'
067000                                                  '22'.
067100             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
067200             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
067300             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
067400             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
067500             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
067600             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
067700             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
067800             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
067900             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
068000             88  P-N-EACH                   VALUE '21' '22'.
068100             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
068200             88  P-N-NHCMQ-II-SNF           VALUE '32'.
068300             88  P-N-NHCMQ-III-SNF          VALUE '33'.
068400             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
068500         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
068600             88  P-N-NEW-ENGLAND            VALUE  1.
068700             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
068800             88  P-N-SOUTH-ATLANTIC         VALUE  3.
068900             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
069000             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
069100             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
069200             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
069300             88  P-N-MOUNTAIN               VALUE  8.
069400             88  P-N-PACIFIC                VALUE  9.
069500         05  P-NEW-CURRENT-DIV   REDEFINES
069600                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
069700             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
069800         05  P-NEW-MSA-DATA.
069900             10  P-NEW-CHG-CODE-INDEX       PIC X.
070000             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
070100             10  P-NEW-GEO-LOC-MSA9   REDEFINES
070200                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
070300             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
070400             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
070500             10  P-NEW-STAND-AMT-LOC-MSA9
070600       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
070700                 15  P-NEW-RURAL-1ST.
070800                     20  P-NEW-STAND-RURAL  PIC XX.
070900                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
071000                 15  P-NEW-RURAL-2ND        PIC XX.
071100         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
071200                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
071300                 88  P-NEW-SCH-YR82       VALUE   '82'.
071400                 88  P-NEW-SCH-YR87       VALUE   '87'.
071500         05  P-NEW-LUGAR                    PIC X.
071600         05  P-NEW-TEMP-RELIEF-IND          PIC X.
071700         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
071800         05  P-NEW-STATE-CODE               PIC 9(02).
071900         05  P-NEW-STATE-CODE-X REDEFINES
072000             P-NEW-STATE-CODE               PIC X(02).
072100         05  FILLER                         PIC X(03).
072200     02  PROV-NEWREC-HOLD2.
072300         05  P-NEW-VARIABLES.
072400             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
072500             10  P-NEW-COLA              PIC  9(01)V9(03).
072600             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
072700             10  P-NEW-BED-SIZE          PIC  9(05).
072800             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
072900             10  P-NEW-CMI               PIC  9(01)V9(04).
073000             10  P-NEW-SSI-RATIO         PIC  V9(04).
073100             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
073200             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
073300             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
073400             10  P-NEW-DSH-PERCENT       PIC  V9(04).
073500             10  P-NEW-FYE-DATE          PIC  X(08).
073600         05  P-NEW-CBSA-DATA.
073700             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
073800             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
073900             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
074000             10  P-NEW-CBSA-GEO-RURAL REDEFINES
074100                 P-NEW-CBSA-GEO-LOC.
074200                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
074300                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
074400                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
074500
074600             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
074700             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
074800             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
074900     02  PROV-NEWREC-HOLD3.
075000         05  P-NEW-PASS-AMT-DATA.
075100             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
075200             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
075300             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
075400             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
075500         05  P-NEW-CAPI-DATA.
075600             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
075700             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
075800             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
075900             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
076000             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
076100             15  P-NEW-CAPI-NEW-HOSP       PIC X.
076200             15  P-NEW-CAPI-IME            PIC 9V9999.
076300             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
076400         05  P-HVBP-HRR-DATA.
076500             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
076600             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
076700             15  P-HOSP-READMISSION-REDU    PIC X.
076800             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
076900         05  P-MODEL1-BUNDLE-DATA.
077000             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
077100             15  P-HAC-REDUC-IND            PIC X.
077200             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
077300             15  P-EHR-REDUC-IND            PIC X.
077400             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
077500         05  P-NEW-COUNTY-CODE              PIC 9(05).
077600         05  FILLER                         PIC X(47).
077700
077800*****************************************************************
077900 01  WAGE-NEW-CBSA-INDEX-RECORD.
078000     05  W-CBSA                        PIC X(5).
078100     05  W-CBSA-SIZE                   PIC X.
078200         88  LARGE-URBAN       VALUE 'L'.
078300         88  OTHER-URBAN       VALUE 'O'.
078400         88  ALL-RURAL         VALUE 'R'.
078500     05  W-CBSA-EFF-DATE               PIC X(8).
078600     05  FILLER                        PIC X.
078700     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
078800     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
078900
079000*******************************************************
079100*    HOLD VARIABLES POPULATED IN PPCAL___***          *
079200*******************************************************
079300 COPY PPHOLDAR.
079400
079500******************************************************************
079600 PROCEDURE DIVISION  USING BILL-NEW-DATA
079700                           PPS-DATA
079800                           PRICER-OPT-VERS-SW
079900                           PPS-ADDITIONAL-VARIABLES
080000                           PROV-NEW-HOLD
080100                           WAGE-NEW-CBSA-INDEX-RECORD
080200                           PPHOLDAR-HOLD-AREA.
080300
080400***************************************************************
080500*    PROCESSING:                                              *
080600*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE
080700*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
080800*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
080900*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
081000*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
081100*           GOBACK.                                           *
081200*        D. ASSEMBLE PRICING COMPONENTS.                      *
081300*        E. CALCULATE THE PRICE.                              *
081400***************************************************************
081500     INITIALIZE WK-HLDDRG-DATA
081600                WK-HLDDRG-DATA2
081700                WK-HLD-MID-DATA
081800                WK-NEW-TECH-VARIABLES.
081900
082000     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
082100     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
082200     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
082300     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
082400     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
082500     MOVE 'N' TO TEMP-RELIEF-FLAG.
082600     MOVE 'N' TO OUTLIER-RECON-FLAG.
082700     MOVE ZEROES TO WK-HAC-AMOUNT.
082800     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
082900     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
083000     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
083100     MOVE ZEROES TO PPS-ISLET-ISOL-PAY-ADD-ON.
083200
083300     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
083400
083500     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
083600     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
083700     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
083800     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
083900     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
084000     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
084100     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
084200     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
084300     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
084400                            PPS-ADDITIONAL-PAY-INFO-DATA.
084500     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
084600                            PPS-ADDITIONAL-PAY-INFO-DATA2.
084700
084800     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
084900     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
085000     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
085100     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
085200                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
085300     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
085400     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
085500                               PPS-MODEL1-BUNDLE-DISPRCNT.
085600
085700     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
085800     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
085900     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
086000*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
086100     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
086200     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
086300     MOVE 0     TO    PPS-FLX7-PAYMENT.
086400
086500     IF (PPS-RTC = '00' OR '03' OR '10' OR
086600                   '12' OR '14')
086700        MOVE 'Y' TO OUTLIER-RECON-FLAG
086800        MOVE PPS-DATA TO HLD-PPS-DATA
086900        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
087000        MOVE HLD-PPS-DATA TO PPS-DATA.
087100
087200     IF  PPS-RTC < 50
087300         IF  P-NEW-WAIVER-STATE
087400             MOVE 53 TO PPS-RTC
087500             MOVE ALL '0' TO PPS-OPER-HSP-PART
087600                             PPS-OPER-FSP-PART
087700                             PPS-OPER-OUTLIER-PART
087800                             PPS-OUTLIER-DAYS
087900                             PPS-REG-DAYS-USED
088000                             PPS-LTR-DAYS-USED
088100                             PPS-TOTAL-PAYMENT
088200                             WK-HAC-TOTAL-PAYMENT
088300                             PPS-OPER-DSH-ADJ
088400                             PPS-OPER-IME-ADJ
088500                             H-DSCHG-FRCTN
088600                             H-DRG-WT-FRCTN
088700                             HOLD-ADDITIONAL-VARIABLES
088800                             HOLD-CAPITAL-VARIABLES
088900                             HOLD-CAPITAL2-VARIABLES
089000                             HOLD-OTHER-VARIABLES
089100                             HOLD-PC-OTH-VARIABLES
089200                             H-ADDITIONAL-PAY-INFO-DATA
089300                             H-ADDITIONAL-PAY-INFO-DATA2.
089400     GOBACK.
089500
089600 0200-MAINLINE-CONTROL.
089700
089800     MOVE 'N' TO HMO-TAG.
089900
090000     IF PPS-PC-HMO-FLAG = 'Y' OR
090100               HMO-FLAG = 'Y'
090200        MOVE 'Y' TO HMO-TAG.
090300
090400     MOVE ALL '0' TO PPS-DATA
090500                     H-OPER-DSH-SCH
090600                     H-OPER-DSH-RRC
090700                     HOLD-PPS-COMPONENTS
090800                     HOLD-PPS-COMPONENTS
090900                     HOLD-ADDITIONAL-VARIABLES
091000                     HOLD-CAPITAL-VARIABLES
091100                     HOLD-CAPITAL2-VARIABLES
091200                     HOLD-OTHER-VARIABLES
091300                     HOLD-PC-OTH-VARIABLES
091400                     H-ADDITIONAL-PAY-INFO-DATA
091500                     H-ADDITIONAL-PAY-INFO-DATA2
091600                     H-EHR-SUBSAV-QUANT
091700                     H-EHR-SUBSAV-LV
091800                     H-EHR-SUBSAV-QUANT-INCLV
091900                     H-EHR-RESTORE-FULL-QUANT
092000                     H-OPER-BILL-STDZ-COSTS
092100                     H-CAPI-BILL-STDZ-COSTS
092200                     H-OPER-STDZ-COST-OUTLIER
092300                     H-CAPI-STDZ-COST-OUTLIER
092400                     H-OPER-STDZ-DOLLAR-THRESHOLD
092500                     H-CAPI-STDZ-DOLLAR-THRESHOLD
092600                     WK-LOW-VOL-ADDON
092700                     WK-HAC-AMOUNT
092800                     WK-HAC-TOTAL-PAYMENT.
092900
093000     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
093100        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
093200
093300     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
093400        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
093500
093600     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
093700        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
093800
093900     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
094000        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
094100
094200     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
094300        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
094400
094500     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
094600        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
094700
094800     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
094900        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
095000
095100     PERFORM 1000-EDIT-THE-BILL-INFO.
095200
095300     IF  PPS-RTC = 00
095400         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
095500
095600     IF  PPS-RTC = 00
095700         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
095800
095900     IF OUTLIER-RECON-FLAG = 'Y'
096000        MOVE 'N' TO OUTLIER-RECON-FLAG
096100        GO TO 0200-EXIT.
096200
096300     IF PPS-RTC = 00
096400        IF H-PERDIEM-DAYS = H-ALOS OR
096500           H-PERDIEM-DAYS > H-ALOS
096600           MOVE 14 TO PPS-RTC.
096700
096800     IF PPS-RTC = 02
096900        IF H-PERDIEM-DAYS = H-ALOS OR
097000           H-PERDIEM-DAYS > H-ALOS
097100           MOVE 16 TO PPS-RTC.
097200
097300 0200-EXIT.   EXIT.
097400
097500 1000-EDIT-THE-BILL-INFO.
097600
097700     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
097800     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
097900
098000**   IF  PPS-RTC = 00
098100*        IF  P-NEW-WAIVER-STATE
098200*            MOVE 53 TO PPS-RTC.
098300
098400     IF  PPS-RTC = 00
098500         IF   HLDDRG-VALID = 'I'
098600             MOVE 54 TO PPS-RTC.
098700
098800     IF  PPS-RTC = 00
098900            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
099000                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
099100                MOVE 55 TO PPS-RTC.
099200
099300     IF  PPS-RTC = 00
099400         IF P-NEW-TERMINATION-DATE > 00000000
099500            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
099600                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
099700                  MOVE 55 TO PPS-RTC.
099800
099900     IF  PPS-RTC = 00
100000         IF  B-LOS NOT NUMERIC
100100             MOVE 56 TO PPS-RTC
100200         ELSE
100300         IF  B-LOS = 0
100400             IF B-REVIEW-CODE NOT = 00 AND
100500                              NOT = 03 AND
100600                              NOT = 06 AND
100700                              NOT = 07 AND
100800                              NOT = 09 AND
100900                              NOT = 11
101000             MOVE 56 TO PPS-RTC.
101100
101200     IF  PPS-RTC = 00
101300         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
101400             MOVE 61 TO PPS-RTC
101500         ELSE
101600             MOVE B-LTR-DAYS TO H-LTR-DAYS.
101700
101800     IF  PPS-RTC = 00
101900         IF  B-COVERED-DAYS NOT NUMERIC
102000             MOVE 62 TO PPS-RTC
102100         ELSE
102200         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
102300             MOVE 62 TO PPS-RTC
102400         ELSE
102500             MOVE B-COVERED-DAYS TO H-COV-DAYS.
102600
102700     IF  PPS-RTC = 00
102800         IF  H-LTR-DAYS  > H-COV-DAYS
102900             MOVE 62 TO PPS-RTC
103000         ELSE
103100             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
103200
103300     IF  PPS-RTC = 00
103400         IF  NOT VALID-REVIEW-CODE
103500             MOVE 57 TO PPS-RTC.
103600
103700     IF  PPS-RTC = 00
103800         IF  B-CHARGES-CLAIMED NOT NUMERIC
103900             MOVE 58 TO PPS-RTC.
104000
104100     IF PPS-RTC = 00
104200           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
104300                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
104400                                            NOT = 'C'
104500                 MOVE 65 TO PPS-RTC.
104600
104700***  MDH PROVISION ENDS 9/30/2017
104800***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
104900
105000     IF PPS-RTC = 00 AND
105100        B-DISCHARGE-DATE > 20170930 AND
105200        P-N-INVALID-PROV-TYPES
105300                 MOVE 52 TO PPS-RTC.
105400
105500 2000-ASSEMBLE-PPS-VARIABLES.
105600***  GET THE PROVIDER SPECIFIC VARIABLES.
105700
105800     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
105900     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
106000
106100     IF  (P-NEW-STATE = 02 OR 12)
106200         MOVE P-NEW-COLA TO H-OPER-COLA
106300     ELSE
106400         MOVE 1.000  TO H-OPER-COLA.
106500
106600***************************************************************
106700***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
106800
106900     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
107000
107100     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
107200
107300     MOVE P-NEW-STATE            TO MES-PPS-STATE.
107400
107500*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
107600** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
107700** FOR FY 2011
107800***************************************************************
107900*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
108000*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
108100***************************************************************
108200***  GET THE WAGE-INDEX
108300
108400     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
108500     MOVE P-NEW-STATE            TO MES-PPS-STATE.
108600
108700***************************************************************
108800* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
108900* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
109000***************************************************************
109100
109200     PERFORM 2050-RATES-TB THRU 2050-EXIT.
109300
109400     IF P-NEW-GEO-LOC-MSA9 >= 9400 AND
109500        P-NEW-GEO-LOC-MSA9 <= 9900
109600        PERFORM 2100-MIDNIGHT-FACTORS THRU 2100-EXIT
109700     ELSE
109800        MOVE 1 TO HLD-MID-ADJ-FACT
109900        GO TO 2000-EXIT.
110000
110100 2000-EXIT.  EXIT.
110200
110300 2050-RATES-TB.
110400     MOVE 1 TO R2
110500     MOVE 1 TO R4.
110600
110700     IF LARGE-URBAN
110800         MOVE 1 TO R3
110900     ELSE
111000         MOVE 2 TO R3.
111100
111200     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
111300        (P-EHR-REDUC-IND = ' ')           AND
111400        (H-WAGE-INDEX > 01.0000))
111500        PERFORM 2300-GET-LAB-NONLAB-TB1-RATES
111600           THRU 2300-GET-LAB-NONLAB-TB1-EXIT
111700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
111800
111900     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
112000        (P-EHR-REDUC-IND = ' ')               AND
112100         (H-WAGE-INDEX > 01.0000))
112200        PERFORM 2300-GET-LAB-NONLAB-TB2-RATES
112300           THRU 2300-GET-LAB-NONLAB-TB2-EXIT
112400             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
112500
112600     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
112700        (P-EHR-REDUC-IND = ' ')            AND
112800         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
112900        PERFORM 2300-GET-LAB-NONLAB-TB3-RATES
113000           THRU 2300-GET-LAB-NONLAB-TB3-EXIT
113100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
113200
113300     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
113400        (P-EHR-REDUC-IND = ' ')               AND
113500         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
113600        PERFORM 2300-GET-LAB-NONLAB-TB4-RATES
113700           THRU 2300-GET-LAB-NONLAB-TB4-EXIT
113800             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
113900
114000     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
114100        (P-EHR-REDUC-IND = 'Y')           AND
114200        (H-WAGE-INDEX > 01.0000))
114300        PERFORM 2300-GET-LAB-NONLAB-TB5-RATES
114400           THRU 2300-GET-LAB-NONLAB-TB5-EXIT
114500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
114600
114700     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
114800        (P-EHR-REDUC-IND = 'Y')               AND
114900         (H-WAGE-INDEX > 01.0000))
115000        PERFORM 2300-GET-LAB-NONLAB-TB6-RATES
115100           THRU 2300-GET-LAB-NONLAB-TB6-EXIT
115200             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
115300
115400     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
115500        (P-EHR-REDUC-IND = 'Y')            AND
115600         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
115700        PERFORM 2300-GET-LAB-NONLAB-TB7-RATES
115800           THRU 2300-GET-LAB-NONLAB-TB7-EXIT
115900             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
116000
116100     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
116200        (P-EHR-REDUC-IND = 'Y')               AND
116300         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
116400        PERFORM 2300-GET-LAB-NONLAB-TB8-RATES
116500           THRU 2300-GET-LAB-NONLAB-TB8-EXIT
116600             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
116700
116800***************************************************************
116900* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
117000***************************************************************
117100
117200     MOVE 0.00  TO H-OPER-HSP-PCT.
117300     MOVE 1.00  TO H-OPER-FSP-PCT.
117400
117500***************************************************************
117600*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
117700***************************************************************
117800
117900      MOVE 1.00 TO H-NAT-PCT.
118000      MOVE 0.00 TO H-REG-PCT.
118100
118200     IF  P-N-SCH-REBASED-FY90 OR
118300         P-N-EACH OR
118400         P-N-MDH-REBASED-FY90 OR
118500         B-FORMER-MDH-PROVIDERS
118600         MOVE 1.00 TO H-OPER-HSP-PCT.
118700
118800 2050-EXIT.   EXIT.
118900
119000***************************************************************
119100*  APPLY THE TWO MIDNIGHT POLICY ADJUSTMENT FACTORS           *
119200***************************************************************
119300 2100-MIDNIGHT-FACTORS.
119400
119500     INITIALIZE HLD-MID-ADJ-FACT.
119600
119700     SET MID-IDX TO 1.
119800
119900     SEARCH MID-TAB VARYING MID-IDX
120000     WHEN WK-MID-MSAX(MID-IDX) = P-NEW-GEO-LOC-MSA9
120100       MOVE MID-DATA-TAB(MID-IDX) TO HLD-MID-DATA.
120200
120300 2100-EXIT.   EXIT.
120400
120500***************************************************************
120600* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
120700* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
120800***************************************************************
120900 2300-GET-LAB-NONLAB-TB1-RATES.
121000
121100     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
121200         MOVE TB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
121300         MOVE TB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
121400         MOVE TB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
121500         MOVE TB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
121600
121700 2300-GET-LAB-NONLAB-TB1-EXIT.   EXIT.
121800
121900 2300-GET-LAB-NONLAB-TB2-RATES.
122000
122100     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
122200         MOVE TB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
122300         MOVE TB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
122400         MOVE TB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
122500         MOVE TB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
122600
122700 2300-GET-LAB-NONLAB-TB2-EXIT.   EXIT.
122800
122900 2300-GET-LAB-NONLAB-TB3-RATES.
123000
123100     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
123200         MOVE TB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
123300         MOVE TB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
123400         MOVE TB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
123500         MOVE TB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
123600
123700 2300-GET-LAB-NONLAB-TB3-EXIT.   EXIT.
123800
123900 2300-GET-LAB-NONLAB-TB4-RATES.
124000
124100     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
124200         MOVE TB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
124300         MOVE TB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
124400         MOVE TB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
124500         MOVE TB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
124600
124700 2300-GET-LAB-NONLAB-TB4-EXIT.   EXIT.
124800
124900 2300-GET-LAB-NONLAB-TB5-RATES.
125000
125100     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
125200         MOVE TB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
125300         MOVE TB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
125400         MOVE TB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
125500         MOVE TB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
125600
125700 2300-GET-LAB-NONLAB-TB5-EXIT.   EXIT.
125800
125900 2300-GET-LAB-NONLAB-TB6-RATES.
126000
126100     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
126200         MOVE TB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
126300         MOVE TB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
126400         MOVE TB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
126500         MOVE TB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
126600
126700 2300-GET-LAB-NONLAB-TB6-EXIT.   EXIT.
126800
126900 2300-GET-LAB-NONLAB-TB7-RATES.
127000
127100     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
127200         MOVE TB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
127300         MOVE TB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
127400         MOVE TB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
127500         MOVE TB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
127600
127700 2300-GET-LAB-NONLAB-TB7-EXIT.   EXIT.
127800
127900 2300-GET-LAB-NONLAB-TB8-RATES.
128000
128100     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
128200         MOVE TB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
128300         MOVE TB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
128400         MOVE TB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
128500         MOVE TB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
128600
128700 2300-GET-LAB-NONLAB-TB8-EXIT.   EXIT.
128800
128900***************************************************************
129000* OBTAIN THE APPLICABLE DRG WEIGHTS                           *
129100***************************************************************
129200 2600-GET-DRG-WEIGHT.
129300
129400     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
129500     SET DRG-IDX TO 1
129600     SEARCH DRG-TAB VARYING DRG-IDX
129700         AT END
129800           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
129900           MOVE 'I' TO  HLDDRG-VALID
130000           MOVE 0 TO HLDDRG-WEIGHT
130100           MOVE 54 TO PPS-RTC
130200           GO TO 2600-EXIT
130300       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
130400         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
130500
130600
130700     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
130800     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
130900     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
131000                                  H-DRG-WT.
131100     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
131200                                  H-ALOS.
131300     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
131400     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
131500                                  H-ARITH-ALOS.
131600     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
131700     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
131800     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
131900     MOVE  'V'                 TO HLDDRG-VALID.
132000     MOVE ZEROES               TO H-DAYS-CUTOFF.
132100
132200 2600-EXIT.   EXIT.
132300
132400*
132500 3000-CALC-PAYMENT.
132600***************************************************************
132700
132800     PERFORM 3100-CALC-STAY-UTILIZATION.
132900     PERFORM 3300-CALC-OPER-FSP-AMT.
133000     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
133100
133200***********************************************************
133300***  OPERATING IME CALCULATION
133400
133500     COMPUTE H-OPER-IME-TEACH ROUNDED =
133600            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
133700
133800***********************************************************
133900
134000     MOVE 00                 TO  PPS-RTC.
134100     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
134200     MOVE H-ALOS             TO  PPS-AVG-LOS.
134300     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
134400
134500     MOVE B-LOS TO H-PERDIEM-DAYS.
134600     IF H-PERDIEM-DAYS < 1
134700         MOVE 1 TO H-PERDIEM-DAYS.
134800     ADD 1 TO H-PERDIEM-DAYS.
134900
135000     MOVE 1 TO H-DSCHG-FRCTN.
135100
135200     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
135300
135400     IF (PAY-PERDIEM-DAYS  OR
135500         PAY-XFER-NO-COST) OR
135600        (PAY-XFER-SPEC-DRG AND
135700         D-DRG-POSTACUTE-PERDIEM)
135800       IF H-ALOS > 0
135900         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
136000         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
136100         IF H-DSCHG-FRCTN > 1
136200              MOVE 1 TO H-DSCHG-FRCTN
136300              MOVE 1 TO H-TRANSFER-ADJ
136400         ELSE
136500              COMPUTE H-DRG-WT-FRCTN ROUNDED =
136600                  H-TRANSFER-ADJ * H-DRG-WT
136700         END-IF
136800        END-IF
136900     END-IF.
137000
137100
137200     IF (PAY-XFER-SPEC-DRG AND
137300         D-DRG-POSTACUTE-50-50) AND
137400         H-ALOS > 0
137500         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
137600         COMPUTE H-DSCHG-FRCTN  ROUNDED =
137700                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
137800         IF H-DSCHG-FRCTN > 1
137900              MOVE 1 TO H-DSCHG-FRCTN
138000              MOVE 1 TO H-TRANSFER-ADJ
138100         ELSE
138200              COMPUTE H-DRG-WT-FRCTN ROUNDED =
138300            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
138400
138500
138600***********************************************************
138700***  CAPITAL DSH CALCULATION
138800
138900     MOVE 0 TO H-CAPI-DSH.
139000
139100     IF P-NEW-BED-SIZE NOT NUMERIC
139200         MOVE 0 TO P-NEW-BED-SIZE.
139300
139400     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
139500         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
139600                  (.2025 * (P-NEW-SSI-RATIO
139700                          + P-NEW-MEDICAID-RATIO)) - 1.
139800
139900***********************************************************
140000***  CAPITAL IME TEACH CALCULATION
140100
140200     MOVE 0 TO H-WK-CAPI-IME-TEACH.
140300
140400     IF P-NEW-CAPI-IME NUMERIC
140500        IF P-NEW-CAPI-IME > 1.5000
140600           MOVE 1.5000 TO P-NEW-CAPI-IME.
140700
140800*****YEARCHANGE 2009.5 ****************************************
140900***
141000***  PER POLICY, WE REMOVED THE .5 MULTIPLER
141100***
141200***********************************************************
141300     IF P-NEW-CAPI-IME NUMERIC
141400        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
141500         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
141600
141700*****YEARCHANGE 2009.5 ****************************************
141800***********************************************************
141900     MOVE 0.00 TO H-DAYOUT-PCT.
142000     MOVE 0.80 TO H-CSTOUT-PCT.
142100
142200*****************************************************************
142300**
142400** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
142500**
142600*****************************************************************
142700
142800     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
142900             MOVE 0.90 TO H-CSTOUT-PCT.
143000
143100*****YEARCHANGE 2015.0 ****************************************
143200***     NATIONAL PERCENTAGE
143300     MOVE 0.6960   TO H-LABOR-PCT.
143400     MOVE 0.3040   TO H-NONLABOR-PCT.
143500
143600*****YEARCHANGE 2015.0 ****************************************
143700
143800     IF (H-WAGE-INDEX < 01.0000 OR
143900         H-WAGE-INDEX = 01.0000)
144000        MOVE 0.6200 TO H-LABOR-PCT
144100        MOVE 0.3800 TO H-NONLABOR-PCT.
144200
144300     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
144400             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
144500     ELSE
144600             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
144700
144800     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
144900             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
145000     ELSE
145100             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
145200
145300***********************************************************
145400*****YEARCHANGE 2010.0 ************************************
145500***  CAPITAL PAYMENT METHOD B - YEARCHNG
145600***  CAPITAL PAYMENT METHOD B
145700
145800     IF W-CBSA-SIZE = 'L'
145900        MOVE 1.00 TO H-CAPI-LARG-URBAN
146000     ELSE
146100        MOVE 1.00 TO H-CAPI-LARG-URBAN.
146200
146300     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
146400
146500*****YEARCHANGE 2016.0 ************************************
146600
146700     COMPUTE H-FEDERAL-RATE ROUNDED =
146800                              (0446.79 * H-CAPI-GAF).
146900
147000*****YEARCHANGE 2015.1 ************************************
147100
147200     COMPUTE H-CAPI-COLA ROUNDED =
147300                     (.3152 * (H-OPER-COLA - 1) + 1).
147400
147500     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
147600
147700***********************************************************
147800* CAPITAL FSP CALCULATION                                 *
147900***********************************************************
148000
148100     COMPUTE H-CAPI-FSP-PART ROUNDED =
148200                               H-DRG-WT       *
148300                               H-CAPI-FED-RATE *
148400                               H-CAPI-COLA *
148500                               H-CAPI-LARG-URBAN *
148600                               HLD-MID-ADJ-FACT.
148700
148800***********************************************************
148900***  CAPITAL PAYMENT METHOD A
149000***  CAPITAL PAYMENT METHOD A
149100
149200     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
149300        MOVE 1.00 TO H-CAPI-SCH
149400     ELSE
149500        MOVE 0.85 TO H-CAPI-SCH.
149600
149700***********************************************************
149800***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
149900***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
150000
150100     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
150200                    (P-NEW-CAPI-OLD-HARM-RATE *
150300                    H-CAPI-SCH).
150400
150500***********************************************************
150600        IF PAY-PERDIEM-DAYS
150700            IF  H-PERDIEM-DAYS < H-ALOS
150800                IF  NOT (B-DRG = 789)
150900                    PERFORM 3500-CALC-PERDIEM-AMT
151000                    MOVE 03 TO PPS-RTC.
151100
151200        IF PAY-XFER-SPEC-DRG
151300            IF  H-PERDIEM-DAYS < H-ALOS
151400                IF  NOT (B-DRG = 789)
151500                    PERFORM 3550-CALC-PERDIEM-AMT.
151600
151700        IF  PAY-XFER-NO-COST
151800            MOVE 00 TO PPS-RTC
151900            IF H-PERDIEM-DAYS < H-ALOS
152000               IF  NOT (B-DRG = 789)
152100                   PERFORM 3500-CALC-PERDIEM-AMT
152200                   MOVE 06 TO PPS-RTC.
152300
152400     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
152500
152600     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
152700
152800     IF PPS-RTC = 65 OR 67 OR 68
152900               GO TO 3000-CONTINUE.
153000
153100     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
153200
153300     IF PPS-RTC = 65 OR 67 OR 68
153400               GO TO 3000-CONTINUE.
153500
153600     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
153700
153800     IF PPS-RTC = 65 OR 67 OR 68
153900               GO TO 3000-CONTINUE.
154000
154100     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
154200
154300     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
154400
154500     IF PPS-RTC = 65 OR 67 OR 68
154600               GO TO 3000-CONTINUE.
154700
154800        IF PAY-XFER-SPEC-DRG
154900            IF  H-PERDIEM-DAYS < H-ALOS
155000                IF  NOT (B-DRG = 789)
155100                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
155200
155300
155400        IF  PAY-PERDIEM-DAYS
155500            IF  H-OPER-OUTCST-PART > 0
155600                MOVE H-OPER-OUTCST-PART TO
155700                     H-OPER-OUTLIER-PART
155800                MOVE 05 TO PPS-RTC
155900            ELSE
156000            IF  PPS-RTC NOT = 03
156100                MOVE 00 TO PPS-RTC
156200                MOVE 0  TO H-OPER-OUTLIER-PART.
156300
156400        IF  PAY-PERDIEM-DAYS
156500            IF  H-CAPI-OUTCST-PART > 0
156600                MOVE H-CAPI-OUTCST-PART TO
156700                     H-CAPI-OUTLIER-PART
156800                MOVE 05 TO PPS-RTC
156900            ELSE
157000            IF  PPS-RTC NOT = 03
157100                MOVE 0  TO H-CAPI-OUTLIER-PART.
157200
157300
157400     IF P-N-SCH-REBASED-FY90 OR
157500        P-N-EACH OR
157600        P-N-MDH-REBASED-FY90 OR
157700        B-FORMER-MDH-PROVIDERS
157800         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
157900
158000
158100 3000-CONTINUE.
158200
158300***********************************************************
158400***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
158500***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
158600
158700     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
158800
158900***********************************************************
159000
159100     IF  PPS-RTC = 67
159200         MOVE H-OPER-DOLLAR-THRESHOLD TO
159300              WK-H-OPER-DOLLAR-THRESHOLD.
159400
159500     IF  PPS-RTC < 50
159600         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
159700
159800     IF  PPS-RTC < 50
159900         NEXT SENTENCE
160000     ELSE
160100         MOVE ALL '0' TO PPS-OPER-HSP-PART
160200                         PPS-OPER-FSP-PART
160300                         PPS-OPER-OUTLIER-PART
160400                         PPS-OUTLIER-DAYS
160500                         PPS-REG-DAYS-USED
160600                         PPS-LTR-DAYS-USED
160700                         PPS-TOTAL-PAYMENT
160800                         WK-HAC-TOTAL-PAYMENT
160900                         PPS-OPER-DSH-ADJ
161000                         PPS-OPER-IME-ADJ
161100                         H-DSCHG-FRCTN
161200                         H-DRG-WT-FRCTN
161300                         HOLD-ADDITIONAL-VARIABLES
161400                         HOLD-CAPITAL-VARIABLES
161500                         HOLD-CAPITAL2-VARIABLES
161600                         HOLD-OTHER-VARIABLES
161700                         HOLD-PC-OTH-VARIABLES
161800                        H-ADDITIONAL-PAY-INFO-DATA
161900                        H-ADDITIONAL-PAY-INFO-DATA2.
162000
162100     IF  PPS-RTC = 67
162200         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
162300                 H-OPER-DOLLAR-THRESHOLD.
162400
162500 3000-EXIT.  EXIT.
162600
162700 3100-CALC-STAY-UTILIZATION.
162800
162900     MOVE 0 TO PPS-REG-DAYS-USED.
163000     MOVE 0 TO PPS-LTR-DAYS-USED.
163100
163200     IF H-REG-DAYS > 0
163300        IF H-REG-DAYS > B-LOS
163400           MOVE B-LOS TO PPS-REG-DAYS-USED
163500        ELSE
163600           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
163700     ELSE
163800        IF H-LTR-DAYS > B-LOS
163900           MOVE B-LOS TO PPS-LTR-DAYS-USED
164000        ELSE
164100           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
164200
164300
164400
164500 3300-CALC-OPER-FSP-AMT.
164600***********************************************************
164700*  OPERATING FSP CALCULATION                              *
164800***********************************************************
164900
165000     COMPUTE H-OPER-FSP-PART ROUNDED =
165100       (H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
165200        H-NAT-NONLABOR * H-OPER-COLA) * H-DRG-WT *
165300        HLD-MID-ADJ-FACT)
165400           ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
165500
165600 3500-CALC-PERDIEM-AMT.
165700***********************************************************
165800***  REVIEW CODE = 03 OR 06
165900***  OPERATING PERDIEM-AMT CALCULATION
166000***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
166100
166200        COMPUTE H-OPER-FSP-PART ROUNDED =
166300        H-OPER-FSP-PART * H-TRANSFER-ADJ
166400        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
166500
166600***********************************************************
166700***********************************************************
166800***  REVIEW CODE = 03 OR 06
166900***  CAPITAL   PERDIEM-AMT CALCULATION
167000***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
167100
167200        COMPUTE H-CAPI-FSP-PART ROUNDED =
167300        H-CAPI-FSP-PART * H-TRANSFER-ADJ
167400        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
167500
167600***********************************************************
167700***  REVIEW CODE = 03 OR 06
167800***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
167900***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
168000
168100        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
168200        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
168300        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
168400
168500 3550-CALC-PERDIEM-AMT.
168600***********************************************************
168700***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
168800***  OPERATING PERDIEM-AMT CALCULATION
168900***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
169000
169100     IF (D-DRG-POSTACUTE-50-50)
169200        MOVE 10 TO PPS-RTC
169300        COMPUTE H-OPER-FSP-PART ROUNDED =
169400        H-OPER-FSP-PART * H-DSCHG-FRCTN
169500        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
169600
169700     IF (D-DRG-POSTACUTE-PERDIEM)
169800        MOVE 12 TO PPS-RTC
169900        COMPUTE H-OPER-FSP-PART ROUNDED =
170000        H-OPER-FSP-PART *  H-TRANSFER-ADJ
170100        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
170200
170300***********************************************************
170400***  CAPITAL PERDIEM-AMT CALCULATION
170500***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
170600
170700     IF (D-DRG-POSTACUTE-50-50)
170800        MOVE 10 TO PPS-RTC
170900        COMPUTE H-CAPI-FSP-PART ROUNDED =
171000        H-CAPI-FSP-PART * H-DSCHG-FRCTN
171100        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
171200
171300     IF (D-DRG-POSTACUTE-PERDIEM)
171400        MOVE 12 TO PPS-RTC
171500        COMPUTE H-CAPI-FSP-PART ROUNDED =
171600        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
171700        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
171800
171900***********************************************************
172000***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
172100***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
172200
172300     IF (D-DRG-POSTACUTE-50-50)
172400        MOVE 10 TO PPS-RTC
172500        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
172600        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
172700        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
172800
172900     IF (D-DRG-POSTACUTE-PERDIEM)
173000        MOVE 12 TO PPS-RTC
173100        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
173200        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
173300        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
173400
173500 3560-CHECK-RTN-CODE.
173600
173700     IF (D-DRG-POSTACUTE-50-50)
173800        MOVE 10 TO PPS-RTC.
173900     IF (D-DRG-POSTACUTE-PERDIEM)
174000        MOVE 12 TO PPS-RTC.
174100
174200 3560-EXIT.    EXIT.
174300
174400***********************************************************
174500 3600-CALC-OUTLIER.
174600***********************************************************
174700*---------------------------------------------------------*
174800* (YEARCHANGE 2016.0)
174900* COST OUTLIER OPERATING AND CAPITAL CALCULATION
175000*---------------------------------------------------------*
175100
175200     IF OUTLIER-RECON-FLAG = 'Y'
175300        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
175400               (H-OPER-CSTCHG-RATIO + .2).
175500
175600     IF H-CAPI-CSTCHG-RATIO > 0 OR
175700        H-OPER-CSTCHG-RATIO > 0
175800        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
175900                H-OPER-CSTCHG-RATIO /
176000               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
176100        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
176200                H-CAPI-CSTCHG-RATIO /
176300               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
176400     ELSE
176500        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
176600                  H-CAPI-SHARE-DOLL-THRESHOLD.
176700
176800*---------------------------------------------------------*
176900* (YEARCHANGE 2016.0)
177000* OUTLIER THRESHOLD AMOUNTS
177100*---------------------------------------------------------*
177200
177300     MOVE 23573.00 TO H-CST-THRESH.
177400
177500     IF (B-REVIEW-CODE = '03') AND
177600         H-PERDIEM-DAYS < H-ALOS
177700        COMPUTE H-CST-THRESH ROUNDED =
177800                      (H-CST-THRESH * H-TRANSFER-ADJ)
177900                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
178000
178100     IF ((B-REVIEW-CODE = '09') AND
178200         (H-PERDIEM-DAYS < H-ALOS))
178300         IF (D-DRG-POSTACUTE-PERDIEM)
178400            COMPUTE H-CST-THRESH ROUNDED =
178500                      (H-CST-THRESH * H-TRANSFER-ADJ)
178600                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
178700
178800     IF ((B-REVIEW-CODE = '09') AND
178900         (H-PERDIEM-DAYS < H-ALOS))
179000         IF (D-DRG-POSTACUTE-50-50)
179100           COMPUTE H-CST-THRESH ROUNDED =
179200          H-CST-THRESH * H-DSCHG-FRCTN
179300                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
179400
179500     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
179600        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
179700         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
179800          H-OPER-SHARE-DOLL-THRESHOLD.
179900
180000***********************************************************
180100
180200     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
180300          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
180400          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
180500
180600***********************************************************
180700******NOW INCLUDES UNCOMPENSATED CARE**********************
180800
180900     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
181000         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
181100                       +
181200           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
181300                       +
181400             H-OPER-DOLLAR-THRESHOLD
181500                       +
181600                WK-UNCOMP-CARE-AMOUNT
181700                       +
181800                 H-NEW-TECH-PAY-ADD-ON.
181900
182000     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
182100      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
182200                       +
182300             H-CAPI-DOLLAR-THRESHOLD.
182400
182500     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
182600         MOVE 0 TO H-CAPI-COST-OUTLIER.
182700
182800
182900***********************************************************
183000***  OPERATING COST CALCULATION
183100
183200     COMPUTE H-OPER-BILL-COSTS ROUNDED =
183300         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
183400         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
183500
183600
183700     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
183800         COMPUTE H-OPER-OUTCST-PART ROUNDED =
183900         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
184000                         H-OPER-COST-OUTLIER).
184100
184200     IF PAY-WITHOUT-COST OR
184300        PAY-XFER-NO-COST OR
184400        PAY-XFER-SPEC-DRG-NO-COST
184500         MOVE 0 TO H-OPER-OUTCST-PART.
184600
184700***********************************************************
184800***  CAPITAL COST CALCULATION
184900
185000     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
185100             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
185200         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
185300
185400     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
185500         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
185600         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
185700                         H-CAPI-COST-OUTLIER).
185800
185900***********************************************************
186000***  'A' NOT VALID FY 2015 ON
186100
186200*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
186300*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
186400*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
186500
186600     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
186700        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
186800               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
186900
187000     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
187100        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
187200        MOVE 0 TO H-CAPI-OUTCST-PART
187300                  H-OPER-OUTCST-PART.
187400
187500     IF PAY-WITHOUT-COST OR
187600        PAY-XFER-NO-COST OR
187700        PAY-XFER-SPEC-DRG-NO-COST
187800         MOVE 0 TO H-CAPI-OUTCST-PART.
187900
188000***********************************************************
188100***  DETERMINES THE BILL TO BE COST  OUTLIER
188200
188300     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
188400         MOVE 0 TO H-CAPI-OUTDAY-PART
188500                   H-CAPI-OUTCST-PART.
188600
188700     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
188800                 MOVE H-OPER-OUTCST-PART TO
188900                      H-OPER-OUTLIER-PART
189000                 MOVE H-CAPI-OUTCST-PART TO
189100                      H-CAPI-OUTLIER-PART
189200                 MOVE 02 TO PPS-RTC.
189300
189400     IF OUTLIER-RECON-FLAG = 'Y'
189500        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
189600           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
189700           GO TO 3600-EXIT
189800        ELSE
189900           GO TO 3600-EXIT
190000     ELSE
190100        NEXT SENTENCE.
190200
190300
190400***********************************************************
190500***  DETERMINES IF COST OUTLIER
190600***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
190700***         RETURN CODE OF 02
190800
190900     MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
191000
191100     IF PPS-RTC = 02
191200       IF H-CAPI-CSTCHG-RATIO > 0 OR
191300          H-OPER-CSTCHG-RATIO > 0
191400             COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
191500                     (H-CAPI-COST-OUTLIER  +
191600                      H-OPER-COST-OUTLIER)
191700                             /
191800                    (H-CAPI-CSTCHG-RATIO  +
191900                     H-OPER-CSTCHG-RATIO)
192000             ON SIZE ERROR MOVE 0 TO H-OPER-DOLLAR-THRESHOLD
192100       ELSE MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
192200
192300***********************************************************
192400***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
192500***         RETURN CODE OF 67
192600
192700     IF PPS-RTC = 02
192800         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
192900            PPS-PC-COT-FLAG = 'Y'
193000             MOVE 67 TO PPS-RTC.
193100***********************************************************
193200
193300***********************************************************
193400***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
193500***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
193600***********************************************************
193700*
193800***********************************************************
193900***  'A' NOT VALID FY 2015 ON
194000*
194100*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
194200*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
194300*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
194400*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
194500
194600     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
194700        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
194800                H-CAPI-OUTLIER-PART.
194900
195000     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
195100        H-CAPI-PAYCDE-PCT1 > 0
195200        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
195300                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
195400         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
195500     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
195600
195700 3600-EXIT.   EXIT.
195800
195900***********************************************************
196000 3450-CALC-ADDITIONAL-HSP.
196100***********************************************************
196200*---------------------------------------------------------*
196300* (YEARCHANGE 2016.0)
196400* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
196500* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
196600* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
196700*---------------------------------------------------------*
196800***  GET THE RBN UPDATING FACTOR
196900
197000*****YEARCHANGE 2013.0 ****************************************
197100     MOVE 0.998431 TO H-BUDG-NUTR130.
197200
197300*****YEARCHANGE 2014.0 ****************************************
197400     MOVE 0.997989 TO H-BUDG-NUTR140.
197500
197600*****YEARCHANGE 2015.1 ****************************************
197700     MOVE 0.998761 TO H-BUDG-NUTR150.
197800
197900*****YEARCHANGE 2016.0 ****************************************
198000
198100     IF B-DISCHARGE-DATE < 20160101
198200        MOVE 0.998405 TO H-BUDG-NUTR160
198300     ELSE
198400        MOVE 0.998404 TO H-BUDG-NUTR160
198500     END-IF.
198600
198700*****YEARCHANGE 2017.0 ****************************************
198800     MOVE 0.999078 TO H-BUDG-NUTR170.
198900
199000
199100***  GET THE MARKET BASKET UPDATE FACTOR
199200*****YEARCHANGE 2013.0 ****************************************
199300        MOVE 1.0180 TO H-UPDATE-130.
199400
199500*****YEARCHANGE 2014.0 ****************************************
199600        MOVE 1.0170 TO H-UPDATE-140.
199700
199800*****YEARCHANGE 2015.0 ****************************************
199900        MOVE 1.02200 TO H-UPDATE-150.
200000
200100*****YEARCHANGE 2016.0 ****************************************
200200        MOVE 1.01700 TO H-UPDATE-160.
200300
200400*****YEARCHANGE 2016.0 ****************************************
200500        MOVE 1.01650 TO H-UPDATE-170.
200600
200700
200800*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
200900*****YEARCHANGE 2017.0 ****************************************
201000     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
201100        P-EHR-REDUC-IND = ' '
201200        MOVE 1.01650 TO H-UPDATE-170.
201300
201400*****YEARCHANGE 2017.0 ****************************************
201500     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
201600        P-EHR-REDUC-IND = 'Y'
201700        MOVE 0.99625 TO H-UPDATE-170.
201800
201900*****YEARCHANGE 2017.0 ****************************************
202000     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
202100        P-EHR-REDUC-IND = ' '
202200        MOVE 1.00975 TO H-UPDATE-170.
202300
202400*****YEARCHANGE 2017.0 ****************************************
202500     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
202600        P-EHR-REDUC-IND = 'Y'
202700        MOVE 0.98950 TO H-UPDATE-170.
202800
202900
203000********YEARCHANGE 2016.0 *************************************
203100*** CASE MIX ADJUSTMENT AS OF FY 2015
203200*** SHORT STAY FIX OF 1.006 FOR FY 2017 (1.0 FOR FY 2018)
203300
203400     MOVE 0.9480 TO H-CASE-MIX-ADJ.
203500     MOVE 1.0060 TO H-SHORT-STAY-ADJ.
203600
203700     COMPUTE H-UPDATE-FACTOR ROUNDED =
203800                       (H-UPDATE-130 *
203900                        H-UPDATE-140 *
204000                        H-UPDATE-150 *
204100                        H-UPDATE-160 *
204200                        H-UPDATE-170 *
204300                        H-BUDG-NUTR130 *
204400                        H-BUDG-NUTR140 *
204500                        H-BUDG-NUTR150 *
204600                        H-BUDG-NUTR160 *
204700                        H-BUDG-NUTR170 *
204800                        HLD-MID-ADJ-FACT *
204900                        H-CASE-MIX-ADJ * H-SHORT-STAY-ADJ).
205000
205100     COMPUTE H-HSP-RATE ROUNDED =
205200         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT.
205300***************************************************************
205400*
205500*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
205600*       COMPUTE H-HSP-RATE ROUNDED =
205700*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
205800*    ELSE
205900*       COMPUTE H-HSP-RATE ROUNDED =
206000*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
206100*
206200***************************************************************
206300********YEARCHANGE 2011.0 *************************************
206400***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
206500***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
206600***     IN THE HSP COMPARRISON
206700
206800********YEARCHANGE 2014.0 *XXXXXX******************************
206900*      THE HSP BUCKET FOR SCH                      ************
207000*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
207100***************************************************************
207200     COMPUTE H-FSP-RATE ROUNDED =
207300        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
207400         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN *
207500         HLD-MID-ADJ-FACT) *
207600             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
207700                               +
207800                         H-OPER-OUTLIER-PART
207900                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
208000
208100****************************************************************
208200****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
208300*****        CHOICE
208400
208500     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
208600           COMPUTE H-OPER-HSP-PART ROUNDED =
208700             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
208800                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
208900     ELSE
209000         MOVE 0 TO H-OPER-HSP-PART.
209100
209200***************************************************************
209300***  YEARCHANGE TURNING MDH BACK ON ***************************
209400***************************************************************
209500***  GET THE MDH REBASE
209600
209700     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
209800         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
209900           COMPUTE H-OPER-HSP-PART ROUNDED =
210000         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
210100                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
210200
210300***************************************************************
210400***  TRANSITIONAL PAYMENT FOR FORMER MDHS                     *
210500***************************************************************
210600
210700***  HSP PAYMENT FOR CLAIMS BETWEEN 10/01/2016 - 09/30/2017
210800
210900     IF  B-FORMER-MDH-PROVIDERS       AND
211000        (B-DISCHARGE-DATE > 20160930  AND
211100         B-DISCHARGE-DATE < 20171001)
211200       IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
211300         COMPUTE H-OPER-HSP-PART ROUNDED =
211400           ((H-HSP-RATE - (H-FSP-RATE +
211500               WK-UNCOMP-CARE-AMOUNT))* 0.75)*(1 / 3)
211600             ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
211700       END-IF
211800     END-IF.
211900
212000 3450-EXIT.   EXIT.
212100
212200***********************************************************
212300 3800-CALC-TOT-AMT.
212400***********************************************************
212500***  CALCULATE TOTALS FOR CAPITAL
212600
212700     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
212800
212900***********************************************************
213000***  'A' NOT VALID FY 2015 ON
213100*
213200*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
213300*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
213400*       MOVE 0.00 TO H-CAPI-HSP-PCT.
213500
213600     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
213700        MOVE 0    TO H-CAPI-OLD-HARMLESS
213800        MOVE 1.00 TO H-CAPI-FSP-PCT
213900        MOVE 0.00 TO H-CAPI-HSP-PCT.
214000
214100     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
214200        MOVE 0    TO H-CAPI-OLD-HARMLESS
214300        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
214400        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
214500
214600     COMPUTE H-CAPI-HSP ROUNDED =
214700         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
214800
214900     COMPUTE H-CAPI-FSP ROUNDED =
215000         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
215100
215200     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
215300
215400     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
215500
215600     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
215700             H-CAPI-FSP
215800              * H-CAPI-DSH.
215900
216000     COMPUTE H-CAPI-IME-ADJ ROUNDED =
216100          H-CAPI-FSP *
216200                 H-WK-CAPI-IME-TEACH.
216300
216400     COMPUTE H-CAPI-OUTLIER ROUNDED =
216500             1.00 * H-CAPI-OUTLIER-PART.
216600
216700     COMPUTE H-CAPI2-B-FSP ROUNDED =
216800             1.00 * H-CAPI2-B-FSP-PART.
216900
217000     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
217100             1.00 * H-CAPI2-B-OUTLIER-PART.
217200***********************************************************
217300***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
217400***        THIS ZEROES OUT ALL CAPITAL DATA
217500
217600     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
217700        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
217800***********************************************************
217900
218000***********************************************************
218100***  CALCULATE FINAL TOTALS FOR OPERATING
218200
218300     IF (H-CAPI-OUTLIER > 0 AND
218400         PPS-OPER-OUTLIER-PART = 0)
218500            COMPUTE PPS-OPER-OUTLIER-PART =
218600                    PPS-OPER-OUTLIER-PART + .01.
218700
218800***********************************************************
218900*LOW VOLUME CALCULATIONS
219000***********************************************************
219100*---------------------------------------------------------*
219200* (YEARCHANGE 2016.0)
219300* LOW VOLUME PAYMENT ADD-ON PERCENT
219400*---------------------------------------------------------*
219500
219600     MOVE ZERO TO PPS-OPER-DSH-ADJ.
219700************************************************
219800* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
219900* EMPERICAL DSH
220000************************************************
220100     IF  H-OPER-DSH NUMERIC
220200         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
220300                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
220400
220500     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
220600                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
220700
220800
220900     COMPUTE PPS-OPER-FSP-PART ROUNDED =
221000                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
221100
221200     COMPUTE PPS-OPER-HSP-PART ROUNDED =
221300                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
221400
221500     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
221600                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
221700
221800     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
221900                                H-NEW-TECH-PAY-ADD-ON.
222000
222100     COMPUTE PPS-ISLET-ISOL-PAY-ADD-ON ROUNDED =
222200                                H-NEW-TECH-ADDON-ISLET.
222300
222400     IF P-NEW-TEMP-RELIEF-IND = 'Y'
222500        AND P-LV-ADJ-FACTOR > 0.00
222600        AND P-LV-ADJ-FACTOR <= 0.25
222700     COMPUTE WK-LOW-VOL-ADDON ROUNDED =
222800       (PPS-OPER-HSP-PART +
222900        PPS-OPER-FSP-PART +
223000        PPS-OPER-IME-ADJ +
223100        PPS-OPER-DSH-ADJ +
223200        PPS-OPER-OUTLIER-PART +
223300        H-CAPI-FSP +
223400        H-CAPI-IME-ADJ +
223500        H-CAPI-DSH-ADJ +
223600        H-CAPI-OUTLIER +
223700        WK-UNCOMP-CARE-AMOUNT +
223800        PPS-NEW-TECH-PAY-ADD-ON) * P-LV-ADJ-FACTOR
223900     ELSE
224000     COMPUTE WK-LOW-VOL-ADDON ROUNDED = 0.
224100
224200     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
224300     IF HMO-TAG  = 'Y'
224400        PERFORM 3850-HMO-IME-ADJ.
224500
224600***********************************************************
224700***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
224800
224900     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
225000             H-CAPI-FSP + H-CAPI-IME-ADJ +
225100             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
225200
225300         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
225400         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
225500
225600***********************************************************
225700* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
225800***********************************************************
225900*---------------------------------------------------------*
226000* (YEARCHANGE 2016.0)
226100* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
226200*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
226300*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
226400*   + BASED ON INDICATOR FROM THE PPS FILE
226500*   + NOT VALID IN PUERTO RICO
226600*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
226700*---------------------------------------------------------*
226800
226900     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
227000        PPS-OPER-HSP-PART +
227100        PPS-OPER-FSP-PART +
227200        PPS-OPER-IME-ADJ +
227300        PPS-OPER-DSH-ADJ +
227400        PPS-OPER-OUTLIER-PART +
227500        H-CAPI-TOTAL-PAY +
227600        WK-UNCOMP-CARE-AMOUNT +
227700        PPS-NEW-TECH-PAY-ADD-ON +
227800        WK-LOW-VOL-ADDON +
227900        H-READMIS-ADJUST-AMT +
228000        H-VAL-BASED-PURCH-ADJUST-AMT.
228100
228200     MOVE ZERO TO WK-HAC-AMOUNT.
228300
228400     IF P-PR-NEW-STATE AND
228500        P-HAC-REDUC-IND = 'Y'
228600           MOVE 53 TO PPS-RTC
228700           GO TO 3800-EXIT.
228800
228900     IF  P-HAC-REDUC-IND = 'Y'
229000         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
229100                   WK-HAC-TOTAL-PAYMENT * -0.01
229200     ELSE
229300         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
229400
229500***********************************************************
229600***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
229700************************************************
229800     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
229900                 WK-HAC-TOTAL-PAYMENT
230000                           +
230100                 H-WK-PASS-AMT-PLUS-MISC
230200                           +
230300                 H-BUNDLE-ADJUST-AMT
230400                           +
230500                 WK-HAC-AMOUNT
230600                           +
230700                 H-NEW-TECH-ADDON-ISLET.
230800
230900     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
231000              H-VAL-BASED-PURCH-PARTIPNT.
231100
231200     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
231300              H-VAL-BASED-PURCH-ADJUST.
231400
231500     MOVE     P-HOSP-READMISSION-REDU    TO
231600              H-HOSP-READMISSION-REDU.
231700
231800     MOVE     P-HOSP-HRR-ADJUSTMT        TO
231900              H-HOSP-HRR-ADJUSTMT.
232000
232100 3800-EXIT.   EXIT.
232200
232300 3850-HMO-IME-ADJ.
232400***********************************************************
232500***  HMO CALC FOR PASS-THRU ADDON
232600
232700     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
232800          (P-NEW-PASS-AMT-PLUS-MISC -
232900          (P-NEW-PASS-AMT-ORGAN-ACQ +
233000           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
233100
233200***********************************************************
233300***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
233400
233500     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
233600                   PPS-OPER-IME-ADJ * .0.
233700
233800***********************************************************
233900
234000
234100 3900A-CALC-OPER-DSH.
234200
234300***  OPERATING DSH CALCULATION
234400
234500      MOVE 0.0000 TO H-OPER-DSH.
234600
234700      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
234800                                     + P-NEW-MEDICAID-RATIO).
234900
235000***********************************************************
235100**1**    0-99 BEDS
235200***  NOT TO EXCEED 12%
235300
235400      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
235500                               AND H-WK-OPER-DSH > .1499
235600                               AND H-WK-OPER-DSH < .2020
235700        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
235800                                      * .65 + .025
235900        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
236000
236100      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
236200                               AND H-WK-OPER-DSH > .2019
236300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
236400                                      * .825 + .0588
236500        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
236600
236700***********************************************************
236800**2**   100 + BEDS
236900***  NO CAP >> CAN EXCEED 12%
237000
237100      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
237200                               AND H-WK-OPER-DSH > .1499
237300                               AND H-WK-OPER-DSH < .2020
237400        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
237500                                      * .65 + .025.
237600
237700      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
237800                               AND H-WK-OPER-DSH > .2019
237900        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
238000                                      * .825 + .0588.
238100
238200***********************************************************
238300**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
238400***  NOT TO EXCEED 12%
238500
238600      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
238700                               AND H-WK-OPER-DSH > .1499
238800                               AND H-WK-OPER-DSH < .2020
238900        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
239000                                 * .65 + .025
239100        IF H-OPER-DSH > .1200
239200              MOVE .1200 TO H-OPER-DSH.
239300
239400      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
239500                               AND H-WK-OPER-DSH > .2019
239600        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
239700                                 * .825 + .0588
239800        IF H-OPER-DSH > .1200
239900                 MOVE .1200 TO H-OPER-DSH.
240000***********************************************************
240100**4**   OTHER RURAL HOSPITALS 500 BEDS +
240200***  NO CAP >> CAN EXCEED 12%
240300
240400      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
240500                               AND H-WK-OPER-DSH > .1499
240600                               AND H-WK-OPER-DSH < .2020
240700        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
240800                                 * .65 + .025.
240900
241000      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
241100                               AND H-WK-OPER-DSH > .2019
241200        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
241300                                 * .825 + .0588.
241400
241500***********************************************************
241600**7**   RURAL HOSPITALS SCH
241700***  NOT TO EXCEED 12%
241800
241900      IF W-CBSA-SIZE = 'R'
242000         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
242100                               AND H-WK-OPER-DSH > .1499
242200                               AND H-WK-OPER-DSH < .2020
242300         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
242400                                 * .65 + .025
242500        IF H-OPER-DSH > .1200
242600                 MOVE .1200 TO H-OPER-DSH.
242700
242800      IF W-CBSA-SIZE = 'R'
242900         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
243000                               AND H-WK-OPER-DSH > .2019
243100         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
243200                                 * .825 + .0588
243300        IF H-OPER-DSH > .1200
243400                 MOVE .1200 TO H-OPER-DSH.
243500
243600***********************************************************
243700**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
243800***  RRC OVERRIDES SCH CAP
243900***  NO CAP >> CAN EXCEED 12%
244000
244100         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
244200                                   '17' OR '22')
244300                               AND H-WK-OPER-DSH > .1499
244400                               AND H-WK-OPER-DSH < .2020
244500         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
244600                                 * .65 + .025.
244700
244800         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
244900                                   '17' OR '22')
245000                               AND H-WK-OPER-DSH > .2019
245100         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
245200                                 * .825 + .0588.
245300
245400      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
245500
245600 3900A-EXIT.   EXIT.
245700
245800 4000-CALC-TECH-ADDON.
245900
246000***********************************************************
246100***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
246200
246300     COMPUTE PPS-OPER-HSP-PART ROUNDED =
246400         H-OPER-HSP-PCT * H-OPER-HSP-PART.
246500
246600     COMPUTE PPS-OPER-FSP-PART ROUNDED =
246700         H-OPER-FSP-PCT * H-OPER-FSP-PART.
246800
246900     MOVE ZERO TO PPS-OPER-DSH-ADJ.
247000
247100     IF  H-OPER-DSH NUMERIC
247200             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
247300             (PPS-OPER-FSP-PART
247400              * H-OPER-DSH) * .25.
247500
247600     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
247700             PPS-OPER-FSP-PART *
247800             H-OPER-IME-TEACH.
247900
248000     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
248100             PPS-OPER-FSP-PART +
248200             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
248300             WK-UNCOMP-CARE-AMOUNT.
248400
248500***********************************************************
248600***********************************************************
248700* PUT NEW CHECK HERE IF H-NEW-TECH ZERO PERFORM
248800
248900*    IF   B-DIAG-AUTOLITT-DIAG AND
249000*         B-DRG-AUTOLITT-DRG
249100*       PERFORM 4500-AUTOLIT-TECH-ADD-ON THRU 4500-EXIT.
249200
249300***********************************************************
249400*  DIFICID DISCONTINUED FOR FY 2015
249500*    IF   B-NDC-DIFICID-NDC
249600*      PERFORM 4600-DIFICID-TECH-ADD-ON THRU 4600-EXIT.
249700
249800***********************************************************
249900* NEW TECH ADD ON CODE *
250000***********************************************************
250100     MOVE 1 TO IDX-TECH.
250200     INITIALIZE H-TECH-ADDON-ISLET-CNTR.
250300
250400     PERFORM 4010-NEW-TECH-ADD-ON THRU 4010-EXIT
250500      VARYING IDX-TECH FROM 1 BY 1 UNTIL IDX-TECH > 25.
250600
250700*    IF PROC-ARGUS-FLAG = 'Y'
250800*      PERFORM 4810-ARGUS-TECH-ADD-ON THRU 4810-EXIT
250900*    ELSE
251000*      MOVE ZEROES TO H-NEW-TECH-ADDON-ARGUS.
251100
251200     IF PROC-BLINATU-FLAG = 'Y'
251300       PERFORM 4900-BLINATU-TECH-ADD-ON THRU 4900-EXIT
251400     ELSE
251500       MOVE ZEROES TO H-NEW-TECH-ADDON-BLINATU.
251600
251700     IF PROC-CARDIO-FLAG = 'Y'
251800       PERFORM 5010-CARDIO-MEMES-ADD-ON THRU 5010-EXIT
251900     ELSE
252000       MOVE ZEROES TO H-NEW-TECH-ADDON-CARDIO.
252100
252200     IF PROC-DEFITELIO-FLAG = 'Y'
252300       PERFORM 5040-DEFITELIO-TECH-ADD-ON THRU 5040-EXIT
252400     ELSE
252500       MOVE ZEROES TO H-NEW-TECH-ADDON-DEFITELIO.
252600
252700     IF PROC-GORE-FLAG = 'Y'
252800       PERFORM 5050-GORE-TECH-ADD-ON THRU 5050-EXIT
252900     ELSE
253000       MOVE ZEROES TO H-NEW-TECH-ADDON-GORE.
253100
253200     IF PROC-IDARUCIZ-FLAG = 'Y'
253300       PERFORM 5060-IDARUCIZ-TECH-ADD-ON THRU 5060-EXIT
253400     ELSE
253500       MOVE ZEROES TO H-NEW-TECH-ADDON-IDARUCIZ.
253600
253700     IF DIAG-ISLET-FLAG = 'Y' AND PROC-ISLET-FLAG = 'Y'
253800       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
253900     ELSE
254000       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
254100
254200*    IF DIAG-KCENTRA-FLAG = 'Y' AND PROC-KCENTRA-FLAG = 'Y'
254300*      MOVE ZEROES TO H-NEW-TECH-ADDON-KCENTRA
254400*    ELSE
254500*      PERFORM 4820-KCENTRA-TECH-ADD-ON THRU 4820-EXIT.
254600
254700     IF PROC-LUTONIX-FLAG = 'Y'
254800       PERFORM 4910-LUTONIX-TECH-ADD-ON THRU 4910-EXIT
254900     ELSE
255000       MOVE ZEROES TO H-NEW-TECH-ADDON-LUTONIX.
255100
255200     IF PROC-MAGEC-FLAG = 'Y'
255300       PERFORM 5070-MAGEC-TECH-ADD-ON THRU 5070-EXIT
255400     ELSE
255500       MOVE ZEROES TO H-NEW-TECH-ADDON-MAGEC.
255600
255700*    IF PROC-MITRACLP-FLAG = 'Y'
255800*      PERFORM 5020-MITRA-CLIP-ADD-ON THRU 5020-EXIT
255900*    ELSE
256000*      MOVE ZEROES TO H-NEW-TECH-ADDON-MITRACLP.
256100
256200*    IF PROC-RNSSYS1-FLAG = 'Y' AND PROC-RNSSYS2-FLAG = 'Y'
256300*      PERFORM 5030-RNS-SYS-ADD-ON THRU 5030-EXIT
256400*    ELSE
256500*      MOVE ZEROES TO H-NEW-TECH-ADDON-RNSSYS.
256600
256700     IF DIAG-VISTOGARD-FLAG = 'Y' AND PROC-VISTOGARD-FLAG = 'Y'
256800       PERFORM 5080-VISTOGARD-TECH-ADD-ON THRU 5080-EXIT
256900     ELSE
257000       MOVE ZEROES TO H-NEW-TECH-ADDON-VISTOGARD.
257100
257200*    IF PROC-VORAXAZE-FLAG = 'Y'
257300*      PERFORM 4800-VORAXAZE-TECH-ADD-ON THRU 4800-EXIT
257400*    ELSE
257500*      MOVE ZEROES TO H-NEW-TECH-ADDON-VORAXAZE.
257600
257700*    IF PROC-ZENITH-FLAG = 'Y'
257800*      PERFORM 4700-ZENITH-TECH-ADD-ON THRU 4700-EXIT
257900*    ELSE
258000*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZENITH.
258100
258200*    IF PROC-ZILVER-FLAG = 'Y'
258300*      PERFORM 4830-ZILVER-TECH-ADD-ON THRU 4830-EXIT
258400*    ELSE
258500*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZILVER.
258600
258700***********************************************************
258800*  ALL NEW TECH MUST BE CALCULATED BEFORE
258900*  5500-CAP-CALC-TECH-ADD-ON
259000***********************************************************
259100     PERFORM 5500-CAP-CALC-TECH-ADD-ON THRU 5500-EXIT.
259200
259300     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
259400             H-OPER-FSP-PART +
259500             H-NEW-TECH-PAY-ADD-ON.
259600
259700*
259800 4000-EXIT.    EXIT.
259900***********************************************************
260000
260100 4010-NEW-TECH-ADD-ON.
260200
260300     MOVE B-PROCEDURE-CODE(IDX-TECH) TO WK-PROC-NEW-TECH.
260400     MOVE B-DIAGNOSIS-CODE(IDX-TECH) TO WK-DIAG-NEW-TECH.
260500
260600*    IF PROC-ARGUS
260700*      MOVE 'Y' TO PROC-ARGUS-FLAG.
260800
260900     IF PROC-BLINATU
261000       MOVE 'Y' TO PROC-BLINATU-FLAG.
261100
261200     IF PROC-CARDIO
261300       MOVE 'Y' TO PROC-CARDIO-FLAG.
261400
261500     IF PROC-DEFITELIO
261600       MOVE 'Y' TO PROC-DEFITELIO-FLAG.
261700
261800     IF PROC-GORE
261900       MOVE 'Y' TO PROC-GORE-FLAG.
262000
262100     IF PROC-IDARUCIZ
262200       MOVE 'Y' TO PROC-IDARUCIZ-FLAG.
262300
262400     IF PROC-ISLET
262500       MOVE 'Y' TO PROC-ISLET-FLAG
262600       COMPUTE H-TECH-ADDON-ISLET-CNTR =
262700          H-TECH-ADDON-ISLET-CNTR + 1.
262800
262900*    IF PROC-KCENTRA
263000*      MOVE 'Y' TO PROC-KCENTRA-FLAG.
263100
263200     IF PROC-LUTONIX
263300       MOVE 'Y' TO PROC-LUTONIX-FLAG.
263400
263500     IF PROC-MAGEC
263600       MOVE 'Y' TO PROC-MAGEC-FLAG.
263700
263800*    IF PROC-MITRACLP
263900*      MOVE 'Y' TO PROC-MITRACLP-FLAG.
264000
264100*    IF PROC-RNSSYS1
264200*      MOVE 'Y' TO PROC-RNSSYS1-FLAG.
264300
264400*    IF PROC-RNSSYS2
264500*      MOVE 'Y' TO PROC-RNSSYS2-FLAG.
264600
264700     IF PROC-VISTOGARD
264800       MOVE 'Y' TO PROC-VISTOGARD-FLAG.
264900
265000*    IF PROC-VORAXAZE
265100*      MOVE 'Y' TO PROC-VORAXAZE-FLAG.
265200
265300*    IF PROC-ZENITH
265400*      MOVE 'Y' TO PROC-ZENITH-FLAG.
265500
265600*    IF PROC-ZILVER
265700*      MOVE 'Y' TO PROC-ZILVER-FLAG.
265800
265900     IF DIAG-ISLET
266000       MOVE 'Y' TO DIAG-ISLET-FLAG.
266100
266200*    IF DIAG-KCENTRA
266300*      MOVE 'Y' TO DIAG-KCENTRA-FLAG.
266400
266500     IF DIAG-VISTOGARD
266600       MOVE 'Y' TO DIAG-VISTOGARD-FLAG.
266700
266800 4010-EXIT.   EXIT.
266900
267000***********************************************************
267100* TECHNICAL TRANSPLANTATION OF CELLS                      *
267200***********************************************************
267300 4100-ISLET-ISOLATION-ADD-ON.
267400
267500     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
267600
267700     IF  H-TECH-ADDON-ISLET-CNTR = 1
267800     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
267900           GO TO 4100-EXIT.
268000
268100     IF  H-TECH-ADDON-ISLET-CNTR > 1
268200     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
268300           GO TO 4100-EXIT.
268400
268500 4100-EXIT.    EXIT.
268600
268700***********************************************************
268800* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
268900* DISCHARGE COUNTS.
269000***********************************************************
269100*4400-LOWVOL-CODE-RTN.
269200*
269300*    SET LOWVOL-IDX TO 1.
269400*    SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
269500*        AT END
269600*          MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
269700*          MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
269800*      WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
269900*        MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
270000*                           TO MESWK-LOWVOL-PROV-DISCHG.
270100*
270200*4400-EXIT.   EXIT.
270300
270400*****************************************************************
270500* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR DISCHARGE *
270600* COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER FILE       *
270700*****************************************************************
270800 4410-UNCOMP-CARE-CODE-RTN.
270900
271000*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
271100*
271200*    SET UNCOMP-CARE-IDX TO 1.
271300*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
271400*        AT END
271500*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
271600*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
271700*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
271800*                           TO WK-UNCOMP-CARE-AMOUNT.
271900*
272000        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
272100
272200        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
272300
272400 4410-EXIT.   EXIT.
272500
272600***********************************************************
272700* CASES INVOLVING AUTOLITT                                *
272800***********************************************************
272900*4500-AUTOLIT-TECH-ADD-ON.
273000*
273100*    MOVE 0 TO H-NEW-TECH-ADDON-AUTOLITT
273200*              H-LESSER-AUTOLITT-STOP-1
273300*              H-LESSER-AUTOLITT-STOP-2
273400*              H-CSTMED-AUTOLITT-STOP.
273500*
273600*    IF '1761   ' =  B-PRIN-PROC-CODE     OR
273700*                    B-OTHER-PROC-CODE1   OR
273800*                    B-OTHER-PROC-CODE2   OR
273900*                    B-OTHER-PROC-CODE3   OR
274000*                    B-OTHER-PROC-CODE4   OR
274100*                    B-OTHER-PROC-CODE5   OR
274200*                    B-OTHER-PROC-CODE6   OR
274300*                    B-OTHER-PROC-CODE7   OR
274400*                    B-OTHER-PROC-CODE8   OR
274500*                    B-OTHER-PROC-CODE9   OR
274600*                    B-OTHER-PROC-CODE10  OR
274700*                    B-OTHER-PROC-CODE11  OR
274800*                    B-OTHER-PROC-CODE12  OR
274900*                    B-OTHER-PROC-CODE13  OR
275000*                    B-OTHER-PROC-CODE14  OR
275100*                    B-OTHER-PROC-CODE15  OR
275200*                    B-OTHER-PROC-CODE16  OR
275300*                    B-OTHER-PROC-CODE17  OR
275400*                    B-OTHER-PROC-CODE18  OR
275500*                    B-OTHER-PROC-CODE19  OR
275600*                    B-OTHER-PROC-CODE20  OR
275700*                    B-OTHER-PROC-CODE21  OR
275800*                    B-OTHER-PROC-CODE22  OR
275900*                    B-OTHER-PROC-CODE23  OR
276000*                    B-OTHER-PROC-CODE24
276100*          GO TO 4500-COMPUTE-AUTOLITT
276200*    ELSE
276300*          NEXT SENTENCE.
276400*
276500*          MOVE ZEROES TO H-NEW-TECH-ADDON-AUTOLITT.
276600*          GO TO 4500-ADD-TECH-CASES.
276700*
276800*4500-COMPUTE-AUTOLITT.
276900*
277000*    MOVE  5300.00 TO H-CSTMED-AUTOLITT-STOP.
277100*
277200*    COMPUTE H-LESSER-AUTOLITT-STOP-1 ROUNDED =
277300*                 H-CSTMED-AUTOLITT-STOP.
277400*
277500*    COMPUTE H-LESSER-AUTOLITT-STOP-2 ROUNDED =
277600*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
277700*                    H-BASE-DRG-PAYMENT)) * .5.
277800*
277900*    IF H-LESSER-AUTOLITT-STOP-2 > 0
278000*       IF H-LESSER-AUTOLITT-STOP-1 < H-LESSER-AUTOLITT-STOP-2
278100*        MOVE H-LESSER-AUTOLITT-STOP-1 TO
278200*                               H-NEW-TECH-ADDON-AUTOLITT
278300*       ELSE
278400*        MOVE H-LESSER-AUTOLITT-STOP-2 TO
278500*                               H-NEW-TECH-ADDON-AUTOLITT
278600*    ELSE
278700*       MOVE ZEROES          TO H-NEW-TECH-ADDON-AUTOLITT.
278800*
278900*
279000*4500-ADD-TECH-CASES.
279100*
279200*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
279300*            H-NEW-TECH-PAY-ADD-ON +
279400*            H-NEW-TECH-ADDON-AUTOLITT.
279500*
279600*4500-EXIT.    EXIT.
279700*
279800***********************************************************
279900* CASES INVOLVING DIFICID                                 *
280000***********************************************************
280100*4600-DIFICID-TECH-ADD-ON.
280200*
280300*    MOVE 0 TO H-NEW-TECH-ADDON-DIFICID
280400*              H-LESSER-DIFICID-STOP-1
280500*              H-LESSER-DIFICID-STOP-2
280600*              H-CSTMED-DIFICID-STOP.
280700*
280800*    IF '00845  ' =  B-OTHER-DIAG-CODE1   OR
280900*                    B-OTHER-DIAG-CODE2   OR
281000*                    B-OTHER-DIAG-CODE3   OR
281100*                    B-OTHER-DIAG-CODE4   OR
281200*                    B-OTHER-DIAG-CODE5   OR
281300*                    B-OTHER-DIAG-CODE6   OR
281400*                    B-OTHER-DIAG-CODE7   OR
281500*                    B-OTHER-DIAG-CODE8   OR
281600*                    B-OTHER-DIAG-CODE9   OR
281700*                    B-OTHER-DIAG-CODE10  OR
281800*                    B-OTHER-DIAG-CODE11  OR
281900*                    B-OTHER-DIAG-CODE12  OR
282000*                    B-OTHER-DIAG-CODE13  OR
282100*                    B-OTHER-DIAG-CODE14  OR
282200*                    B-OTHER-DIAG-CODE15  OR
282300*                    B-OTHER-DIAG-CODE16  OR
282400*                    B-OTHER-DIAG-CODE17  OR
282500*                    B-OTHER-DIAG-CODE18  OR
282600*                    B-OTHER-DIAG-CODE19  OR
282700*                    B-OTHER-DIAG-CODE20  OR
282800*                    B-OTHER-DIAG-CODE21  OR
282900*                    B-OTHER-DIAG-CODE22  OR
283000*                    B-OTHER-DIAG-CODE23  OR
283100*                    B-OTHER-DIAG-CODE24  OR
283200*                    B-OTHER-DIAG-CODE25
283300*          GO TO 4600-COMPUTE-DIFICID
283400*    ELSE
283500*          NEXT SENTENCE.
283600*
283700*          MOVE ZEROES TO H-NEW-TECH-ADDON-DIFICID.
283800*          GO TO 4600-ADD-TECH-CASES.
283900*
284000*4600-COMPUTE-DIFICID.
284100*
284200*    MOVE  868.00 TO H-CSTMED-DIFICID-STOP.
284300*
284400*    COMPUTE H-LESSER-DIFICID-STOP-1 ROUNDED =
284500*                 H-CSTMED-DIFICID-STOP.
284600*
284700*    COMPUTE H-LESSER-DIFICID-STOP-2 ROUNDED =
284800*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
284900*                    H-BASE-DRG-PAYMENT)) * .5.
285000*
285100*    IF H-LESSER-DIFICID-STOP-2 > 0
285200*       IF H-LESSER-DIFICID-STOP-1 < H-LESSER-DIFICID-STOP-2
285300*        MOVE H-LESSER-DIFICID-STOP-1 TO
285400*                               H-NEW-TECH-ADDON-DIFICID
285500*       ELSE
285600*        MOVE H-LESSER-DIFICID-STOP-2 TO
285700*                               H-NEW-TECH-ADDON-DIFICID
285800*    ELSE
285900*       MOVE ZEROES          TO H-NEW-TECH-ADDON-DIFICID.
286000*
286100*
286200*
286300*4600-ADD-TECH-CASES.
286400*
286500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
286600*            H-NEW-TECH-PAY-ADD-ON +
286700*            H-NEW-TECH-ADDON-DIFICID.
286800*
286900*4600-EXIT.    EXIT.
287000
287100***********************************************************
287200* CASES INVOLVING ZENITH                                  *
287300***********************************************************
287400*4700-ZENITH-TECH-ADD-ON.
287500*
287600*    MOVE 0 TO H-NEW-TECH-ADDON-ZENITH
287700*              H-LESSER-ZENITH-STOP-1
287800*              H-LESSER-ZENITH-STOP-2
287900*              H-CSTMED-ZENITH-STOP.
288000*
288100*4700-COMPUTE-ZENITH.
288200*
288300*    MOVE  8171.50 TO H-CSTMED-ZENITH-STOP.
288400*
288500*    COMPUTE H-LESSER-ZENITH-STOP-1 ROUNDED =
288600*                 H-CSTMED-ZENITH-STOP.
288700*
288800*    COMPUTE H-LESSER-ZENITH-STOP-2 ROUNDED =
288900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
289000*                    H-BASE-DRG-PAYMENT)) * .5.
289100*
289200*    IF H-LESSER-ZENITH-STOP-2 > 0
289300*       IF H-LESSER-ZENITH-STOP-1 < H-LESSER-ZENITH-STOP-2
289400*        MOVE H-LESSER-ZENITH-STOP-1 TO
289500*                               H-NEW-TECH-ADDON-ZENITH
289600*       ELSE
289700*        MOVE H-LESSER-ZENITH-STOP-2 TO
289800*                               H-NEW-TECH-ADDON-ZENITH
289900*    ELSE
290000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZENITH.
290100*
290200*
290300*4700-ADD-TECH-CASES.
290400*
290500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
290600*            H-NEW-TECH-PAY-ADD-ON +
290700*            H-NEW-TECH-ADDON-ZENITH.
290800*
290900*4700-EXIT.    EXIT.
291000
291100***********************************************************
291200* CASES INVOLVING VORAXAZE                                *
291300***********************************************************
291400*4800-VORAXAZE-TECH-ADD-ON.
291500*
291600*    MOVE 0 TO H-NEW-TECH-ADDON-VORAXAZE
291700*              H-LESSER-VORAXAZE-STOP-1
291800*              H-LESSER-VORAXAZE-STOP-2
291900*              H-CSTMED-VORAXAZE-STOP.
292000*
292100*4800-COMPUTE-VORAXAZE.
292200*
292300*    MOVE  47250.00 TO H-CSTMED-VORAXAZE-STOP.
292400*
292500*    COMPUTE H-LESSER-VORAXAZE-STOP-1 ROUNDED =
292600*                 H-CSTMED-VORAXAZE-STOP.
292700*
292800*    COMPUTE H-LESSER-VORAXAZE-STOP-2 ROUNDED =
292900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
293000*                    H-BASE-DRG-PAYMENT)) * .5.
293100*
293200*    IF H-LESSER-VORAXAZE-STOP-2 > 0
293300*       IF H-LESSER-VORAXAZE-STOP-1 < H-LESSER-VORAXAZE-STOP-2
293400*        MOVE H-LESSER-VORAXAZE-STOP-1 TO
293500*                               H-NEW-TECH-ADDON-VORAXAZE
293600*       ELSE
293700*        MOVE H-LESSER-VORAXAZE-STOP-2 TO
293800*                               H-NEW-TECH-ADDON-VORAXAZE
293900*    ELSE
294000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-VORAXAZE.
294100*
294200*
294300*4800-ADD-TECH-CASES.
294400*
294500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
294600*            H-NEW-TECH-PAY-ADD-ON +
294700*            H-NEW-TECH-ADDON-VORAXAZE.
294800*
294900*4800-EXIT.    EXIT.
295000
295100***********************************************************
295200* CASES INVOLVING ARGUS                                   *
295300***********************************************************
295400*4810-ARGUS-TECH-ADD-ON.
295500*
295600*    MOVE 0 TO H-NEW-TECH-ADDON-ARGUS
295700*              H-LESSER-ARGUS-STOP-1
295800*              H-LESSER-ARGUS-STOP-2
295900*              H-CSTMED-ARGUS-STOP.
296000*
296100*4810-COMPUTE-ARGUS.
296200*
296300*    MOVE  72028.75 TO H-CSTMED-ARGUS-STOP.
296400*
296500*    COMPUTE H-LESSER-ARGUS-STOP-1 ROUNDED =
296600*                 H-CSTMED-ARGUS-STOP.
296700*
296800*    COMPUTE H-LESSER-ARGUS-STOP-2 ROUNDED =
296900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
297000*                    H-BASE-DRG-PAYMENT)) * .5.
297100*
297200*    IF H-LESSER-ARGUS-STOP-2 > 0
297300*       IF H-LESSER-ARGUS-STOP-1 < H-LESSER-ARGUS-STOP-2
297400*        MOVE H-LESSER-ARGUS-STOP-1 TO
297500*                               H-NEW-TECH-ADDON-ARGUS
297600*       ELSE
297700*        MOVE H-LESSER-ARGUS-STOP-2 TO
297800*                               H-NEW-TECH-ADDON-ARGUS
297900*    ELSE
298000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ARGUS.
298100*
298200*
298300*4810-ADD-TECH-CASES.
298400*
298500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
298600*            H-NEW-TECH-PAY-ADD-ON +
298700*            H-NEW-TECH-ADDON-ARGUS.
298800*
298900*4810-EXIT.    EXIT.
299000*
299100***********************************************************
299200* CASES INVOLVING KCENTRA                                 *
299300***********************************************************
299400*4820-KCENTRA-TECH-ADD-ON.
299500*
299600*    MOVE 0 TO H-NEW-TECH-ADDON-KCENTRA
299700*              H-LESSER-KCENTRA-STOP-1
299800*              H-LESSER-KCENTRA-STOP-2
299900*              H-CSTMED-KCENTRA-STOP.
300000*
300100*4820-COMPUTE-KCENTRA.
300200*
300300*    MOVE  01587.50 TO H-CSTMED-KCENTRA-STOP.
300400*
300500*    COMPUTE H-LESSER-KCENTRA-STOP-1 ROUNDED =
300600*                 H-CSTMED-KCENTRA-STOP.
300700*
300800*    COMPUTE H-LESSER-KCENTRA-STOP-2 ROUNDED =
300900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
301000*                    H-BASE-DRG-PAYMENT)) * .5.
301100*
301200*    IF H-LESSER-KCENTRA-STOP-2 > 0
301300*       IF H-LESSER-KCENTRA-STOP-1 < H-LESSER-KCENTRA-STOP-2
301400*        MOVE H-LESSER-KCENTRA-STOP-1 TO
301500*                               H-NEW-TECH-ADDON-KCENTRA
301600*       ELSE
301700*        MOVE H-LESSER-KCENTRA-STOP-2 TO
301800*                               H-NEW-TECH-ADDON-KCENTRA
301900*    ELSE
302000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-KCENTRA.
302100*
302200*
302300*4820-ADD-TECH-CASES.
302400*
302500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
302600*            H-NEW-TECH-PAY-ADD-ON +
302700*            H-NEW-TECH-ADDON-KCENTRA.
302800*
302900*4820-EXIT.    EXIT.
303000
303100***********************************************************
303200* CASES INVOLVING ZILVER                                  *
303300***********************************************************
303400*4830-ZILVER-TECH-ADD-ON.
303500*
303600*    MOVE 0 TO H-NEW-TECH-ADDON-ZILVER
303700*              H-LESSER-ZILVER-STOP-1
303800*              H-LESSER-ZILVER-STOP-2
303900*              H-CSTMED-ZILVER-STOP.
304000*
304100*4830-COMPUTE-ZILVER.
304200*
304300*    MOVE  01705.25 TO H-CSTMED-ZILVER-STOP.
304400*
304500*    COMPUTE H-LESSER-ZILVER-STOP-1 ROUNDED =
304600*                 H-CSTMED-ZILVER-STOP.
304700*
304800*    COMPUTE H-LESSER-ZILVER-STOP-2 ROUNDED =
304900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
305000*                    H-BASE-DRG-PAYMENT)) * .5.
305100*
305200*    IF H-LESSER-ZILVER-STOP-2 > 0
305300*       IF H-LESSER-ZILVER-STOP-1 < H-LESSER-ZILVER-STOP-2
305400*        MOVE H-LESSER-ZILVER-STOP-1 TO
305500*                               H-NEW-TECH-ADDON-ZILVER
305600*       ELSE
305700*        MOVE H-LESSER-ZILVER-STOP-2 TO
305800*                               H-NEW-TECH-ADDON-ZILVER
305900*    ELSE
306000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZILVER.
306100*
306200*
306300*4830-ADD-TECH-CASES.
306400*
306500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
306600*            H-NEW-TECH-PAY-ADD-ON +
306700*            H-NEW-TECH-ADDON-ZILVER.
306800*
306900*4830-EXIT.    EXIT.
307000
307100***********************************************************
307200* CASES INVOLVING BLINATUMOMAB                            *
307300***********************************************************
307400 4900-BLINATU-TECH-ADD-ON.
307500
307600     MOVE 0 TO H-NEW-TECH-ADDON-BLINATU
307700               H-LESSER-BLINATU-STOP-1
307800               H-LESSER-BLINATU-STOP-2
307900               H-CSTMED-BLINATU-STOP.
308000
308100 4900-COMPUTE-BLINATU.
308200
308300     MOVE  27017.85 TO H-CSTMED-BLINATU-STOP.
308400
308500     COMPUTE H-LESSER-BLINATU-STOP-1 ROUNDED =
308600                  H-CSTMED-BLINATU-STOP.
308700
308800     COMPUTE H-LESSER-BLINATU-STOP-2 ROUNDED =
308900          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
309000                     H-BASE-DRG-PAYMENT)) * .5.
309100
309200     IF H-LESSER-BLINATU-STOP-2 > 0
309300        IF H-LESSER-BLINATU-STOP-1 < H-LESSER-BLINATU-STOP-2
309400         MOVE H-LESSER-BLINATU-STOP-1 TO
309500                                H-NEW-TECH-ADDON-BLINATU
309600        ELSE
309700         MOVE H-LESSER-BLINATU-STOP-2 TO
309800                                H-NEW-TECH-ADDON-BLINATU
309900     ELSE
310000        MOVE ZEROES          TO H-NEW-TECH-ADDON-BLINATU.
310100
310200 4900-ADD-TECH-CASES.
310300
310400     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
310500             H-NEW-TECH-PAY-ADD-ON +
310600             H-NEW-TECH-ADDON-BLINATU.
310700
310800 4900-EXIT.    EXIT.
310900
311000***********************************************************
311100* CASES INVOLVING LUTONIX DRUG COATED BALLOON (DCB)       *
311200* PERCUTANEOUS TRANSLUMINAL ANGIOPLASTY (PTA) AND IN.PACT *
311300* ADMIRAL PACLIAXEL COATED PERCUTANEOUS TRANSLUMINAL      *
311400* ANGIOPLASTY (PTA) BALLOON CATHETER                      *
311500***********************************************************
311600 4910-LUTONIX-TECH-ADD-ON.
311700
311800     MOVE 0 TO H-NEW-TECH-ADDON-LUTONIX
311900               H-LESSER-LUTONIX-STOP-1
312000               H-LESSER-LUTONIX-STOP-2
312100               H-CSTMED-LUTONIX-STOP.
312200
312300 4910-COMPUTE-LUTONIX.
312400
312500     MOVE  01035.72 TO H-CSTMED-LUTONIX-STOP.
312600
312700     COMPUTE H-LESSER-LUTONIX-STOP-1 ROUNDED =
312800                  H-CSTMED-LUTONIX-STOP.
312900
313000     COMPUTE H-LESSER-LUTONIX-STOP-2 ROUNDED =
313100          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
313200                     H-BASE-DRG-PAYMENT)) * .5.
313300
313400     IF H-LESSER-LUTONIX-STOP-2 > 0
313500        IF H-LESSER-LUTONIX-STOP-1 < H-LESSER-LUTONIX-STOP-2
313600         MOVE H-LESSER-LUTONIX-STOP-1 TO
313700                                H-NEW-TECH-ADDON-LUTONIX
313800        ELSE
313900         MOVE H-LESSER-LUTONIX-STOP-2 TO
314000                                H-NEW-TECH-ADDON-LUTONIX
314100     ELSE
314200        MOVE ZEROES          TO H-NEW-TECH-ADDON-LUTONIX.
314300
314400 4910-ADD-TECH-CASES.
314500
314600     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
314700             H-NEW-TECH-PAY-ADD-ON +
314800             H-NEW-TECH-ADDON-LUTONIX.
314900
315000 4910-EXIT.    EXIT.
315100
315200**************************************************************
315300* CASES INVOLVING CARDIO MEMES                               *
315400**************************************************************
315500 5010-CARDIO-MEMES-ADD-ON.
315600
315700     MOVE 0 TO H-NEW-TECH-ADDON-CARDIO
315800               H-LESSER-CARDIO-STOP-1
315900               H-LESSER-CARDIO-STOP-2
316000               H-CSTMED-CARDIO-STOP.
316100
316200 5010-COMPUTE-CARDIO.
316300
316400     MOVE  08875.00 TO H-CSTMED-CARDIO-STOP.
316500
316600     COMPUTE H-LESSER-CARDIO-STOP-1 ROUNDED =
316700                  H-CSTMED-CARDIO-STOP.
316800
316900     COMPUTE H-LESSER-CARDIO-STOP-2 ROUNDED =
317000          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
317100                     H-BASE-DRG-PAYMENT)) * .5.
317200
317300     IF H-LESSER-CARDIO-STOP-2 > 0
317400        IF H-LESSER-CARDIO-STOP-1 < H-LESSER-CARDIO-STOP-2
317500         MOVE H-LESSER-CARDIO-STOP-1 TO
317600                                H-NEW-TECH-ADDON-CARDIO
317700        ELSE
317800         MOVE H-LESSER-CARDIO-STOP-2 TO
317900                                H-NEW-TECH-ADDON-CARDIO
318000     ELSE
318100        MOVE ZEROES          TO H-NEW-TECH-ADDON-CARDIO.
318200
318300 5010-ADD-TECH-CASES.
318400
318500     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
318600             H-NEW-TECH-PAY-ADD-ON +
318700             H-NEW-TECH-ADDON-CARDIO.
318800
318900 5010-EXIT.    EXIT.
319000
319100***********************************************************
319200* CASES INVOLVING MITRACLIP                               *
319300***********************************************************
319400*5020-MITRA-CLIP-ADD-ON.
319500*
319600*    MOVE 0 TO H-NEW-TECH-ADDON-MITRACLP
319700*              H-LESSER-MITRACLP-STOP-1
319800*              H-LESSER-MITRACLP-STOP-2
319900*              H-CSTMED-MITRACLP-STOP.
320000*
320100*5020-COMPUTE-MITRACLP.
320200*
320300*    MOVE  15000.00 TO H-CSTMED-MITRACLP-STOP.
320400*
320500*    COMPUTE H-LESSER-MITRACLP-STOP-1 ROUNDED =
320600*                 H-CSTMED-MITRACLP-STOP.
320700*
320800*    COMPUTE H-LESSER-MITRACLP-STOP-2 ROUNDED =
320900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
321000*                    H-BASE-DRG-PAYMENT)) * .5.
321100*
321200*    IF H-LESSER-MITRACLP-STOP-2 > 0
321300*       IF H-LESSER-MITRACLP-STOP-1 < H-LESSER-MITRACLP-STOP-2
321400*        MOVE H-LESSER-MITRACLP-STOP-1 TO
321500*                               H-NEW-TECH-ADDON-MITRACLP
321600*       ELSE
321700*        MOVE H-LESSER-MITRACLP-STOP-2 TO
321800*                               H-NEW-TECH-ADDON-MITRACLP
321900*    ELSE
322000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-MITRACLP.
322100*
322200*5020-ADD-TECH-CASES.
322300*
322400*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
322500*            H-NEW-TECH-PAY-ADD-ON +
322600*            H-NEW-TECH-ADDON-MITRACLP.
322700*
322800*5020-EXIT.    EXIT.
322900
323000***********************************************************
323100* CASES INVOLVING RNS                                     *
323200***********************************************************
323300*5030-RNS-SYS-ADD-ON.
323400*
323500*    MOVE 0 TO H-NEW-TECH-ADDON-RNSSYS
323600*              H-LESSER-RNSSYS-STOP-1
323700*              H-LESSER-RNSSYS-STOP-2
323800*              H-CSTMED-RNSSYS-STOP.
323900*
324000*5030-COMPUTE-RNSSYS.
324100*
324200*    MOVE  18475.00 TO H-CSTMED-RNSSYS-STOP.
324300*
324400*    COMPUTE H-LESSER-RNSSYS-STOP-1 ROUNDED =
324500*                 H-CSTMED-RNSSYS-STOP.
324600*
324700*    COMPUTE H-LESSER-RNSSYS-STOP-2 ROUNDED =
324800*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
324900*                    H-BASE-DRG-PAYMENT)) * .5.
325000*
325100*    IF H-LESSER-RNSSYS-STOP-2 > 0
325200*       IF H-LESSER-RNSSYS-STOP-1 < H-LESSER-RNSSYS-STOP-2
325300*        MOVE H-LESSER-RNSSYS-STOP-1 TO
325400*                               H-NEW-TECH-ADDON-RNSSYS
325500*       ELSE
325600*        MOVE H-LESSER-RNSSYS-STOP-2 TO
325700*                               H-NEW-TECH-ADDON-RNSSYS
325800*    ELSE
325900*       MOVE ZEROES          TO H-NEW-TECH-ADDON-RNSSYS.
326000*
326100*5030-ADD-TECH-CASES.
326200*
326300*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
326400*            H-NEW-TECH-PAY-ADD-ON +
326500*            H-NEW-TECH-ADDON-RNSSYS.
326600*
326700*5030-EXIT.    EXIT.
326800
326900***********************************************************
327000* CASES INVOLVING DEFITELIO                               *
327100***********************************************************
327200 5040-DEFITELIO-TECH-ADD-ON.
327300
327400     MOVE 0 TO H-NEW-TECH-ADDON-DEFITELIO
327500               H-LESSER-DEFITELIO-STOP-1
327600               H-LESSER-DEFITELIO-STOP-2
327700               H-CSTMED-DEFITELIO-STOP.
327800
327900 5040-COMPUTE-DEFITELIO.
328000
328100     MOVE  75900.00 TO H-CSTMED-DEFITELIO-STOP.
328200
328300     COMPUTE H-LESSER-DEFITELIO-STOP-1 ROUNDED =
328400                  H-CSTMED-DEFITELIO-STOP.
328500
328600     COMPUTE H-LESSER-DEFITELIO-STOP-2 ROUNDED =
328700          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
328800                     H-BASE-DRG-PAYMENT)) * .5.
328900
329000     IF H-LESSER-DEFITELIO-STOP-2 > 0
329100        IF H-LESSER-DEFITELIO-STOP-1 < H-LESSER-DEFITELIO-STOP-2
329200         MOVE H-LESSER-DEFITELIO-STOP-1 TO
329300                                H-NEW-TECH-ADDON-DEFITELIO
329400        ELSE
329500         MOVE H-LESSER-DEFITELIO-STOP-2 TO
329600                                H-NEW-TECH-ADDON-DEFITELIO
329700     ELSE
329800        MOVE ZEROES          TO H-NEW-TECH-ADDON-DEFITELIO.
329900
330000 5040-ADD-TECH-CASES.
330100
330200     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
330300             H-NEW-TECH-PAY-ADD-ON +
330400             H-NEW-TECH-ADDON-DEFITELIO.
330500
330600 5040-EXIT.    EXIT.
330700
330800***********************************************************
330900* CASES INVOLVING GORE EXCLUDER                           *
331000***********************************************************
331100 5050-GORE-TECH-ADD-ON.
331200
331300     MOVE 0 TO H-NEW-TECH-ADDON-GORE
331400               H-LESSER-GORE-STOP-1
331500               H-LESSER-GORE-STOP-2
331600               H-CSTMED-GORE-STOP.
331700
331800 5050-COMPUTE-GORE.
331900
332000     MOVE  05250.00 TO H-CSTMED-GORE-STOP.
332100
332200     COMPUTE H-LESSER-GORE-STOP-1 ROUNDED =
332300                  H-CSTMED-GORE-STOP.
332400
332500     COMPUTE H-LESSER-GORE-STOP-2 ROUNDED =
332600          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
332700                     H-BASE-DRG-PAYMENT)) * .5.
332800
332900     IF H-LESSER-GORE-STOP-2 > 0
333000        IF H-LESSER-GORE-STOP-1 < H-LESSER-GORE-STOP-2
333100         MOVE H-LESSER-GORE-STOP-1 TO
333200                                H-NEW-TECH-ADDON-GORE
333300        ELSE
333400         MOVE H-LESSER-GORE-STOP-2 TO
333500                                H-NEW-TECH-ADDON-GORE
333600     ELSE
333700        MOVE ZEROES          TO H-NEW-TECH-ADDON-GORE.
333800
333900 5050-ADD-TECH-CASES.
334000
334100     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
334200             H-NEW-TECH-PAY-ADD-ON +
334300             H-NEW-TECH-ADDON-GORE.
334400
334500 5050-EXIT.    EXIT.
334600
334700***********************************************************
334800* CASES INVOLVING IDARUCIZUMAB                            *
334900***********************************************************
335000 5060-IDARUCIZ-TECH-ADD-ON.
335100
335200     MOVE 0 TO H-NEW-TECH-ADDON-IDARUCIZ
335300               H-LESSER-IDARUCIZ-STOP-1
335400               H-LESSER-IDARUCIZ-STOP-2
335500               H-CSTMED-IDARUCIZ-STOP.
335600
335700 5060-COMPUTE-IDARUCIZ.
335800
335900     MOVE  01750.00 TO H-CSTMED-IDARUCIZ-STOP.
336000
336100     COMPUTE H-LESSER-IDARUCIZ-STOP-1 ROUNDED =
336200                  H-CSTMED-IDARUCIZ-STOP.
336300
336400     COMPUTE H-LESSER-IDARUCIZ-STOP-2 ROUNDED =
336500          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
336600                     H-BASE-DRG-PAYMENT)) * .5.
336700
336800     IF H-LESSER-IDARUCIZ-STOP-2 > 0
336900        IF H-LESSER-IDARUCIZ-STOP-1 < H-LESSER-IDARUCIZ-STOP-2
337000         MOVE H-LESSER-IDARUCIZ-STOP-1 TO
337100                                H-NEW-TECH-ADDON-IDARUCIZ
337200        ELSE
337300         MOVE H-LESSER-IDARUCIZ-STOP-2 TO
337400                                H-NEW-TECH-ADDON-IDARUCIZ
337500     ELSE
337600        MOVE ZEROES          TO H-NEW-TECH-ADDON-IDARUCIZ.
337700
337800 5060-ADD-TECH-CASES.
337900
338000     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
338100             H-NEW-TECH-PAY-ADD-ON +
338200             H-NEW-TECH-ADDON-IDARUCIZ.
338300
338400 5060-EXIT.    EXIT.
338500
338600***********************************************************
338700* CASES INVOLVING MAGEC SPINE                             *
338800***********************************************************
338900 5070-MAGEC-TECH-ADD-ON.
339000
339100     MOVE 0 TO H-NEW-TECH-ADDON-MAGEC
339200               H-LESSER-MAGEC-STOP-1
339300               H-LESSER-MAGEC-STOP-2
339400               H-CSTMED-MAGEC-STOP.
339500
339600 5070-COMPUTE-MAGEC.
339700
339800     MOVE  15750.00 TO H-CSTMED-MAGEC-STOP.
339900
340000     COMPUTE H-LESSER-MAGEC-STOP-1 ROUNDED =
340100                  H-CSTMED-MAGEC-STOP.
340200
340300     COMPUTE H-LESSER-MAGEC-STOP-2 ROUNDED =
340400          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
340500                     H-BASE-DRG-PAYMENT)) * .5.
340600
340700     IF H-LESSER-MAGEC-STOP-2 > 0
340800        IF H-LESSER-MAGEC-STOP-1 < H-LESSER-MAGEC-STOP-2
340900         MOVE H-LESSER-MAGEC-STOP-1 TO
341000                                H-NEW-TECH-ADDON-MAGEC
341100        ELSE
341200         MOVE H-LESSER-MAGEC-STOP-2 TO
341300                                H-NEW-TECH-ADDON-MAGEC
341400     ELSE
341500        MOVE ZEROES          TO H-NEW-TECH-ADDON-MAGEC.
341600
341700 5070-ADD-TECH-CASES.
341800
341900     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
342000             H-NEW-TECH-PAY-ADD-ON +
342100             H-NEW-TECH-ADDON-MAGEC.
342200
342300 5070-EXIT.    EXIT.
342400
342500***********************************************************
342600* CASES INVOLVING VISTOGARD                               *
342700***********************************************************
342800 5080-VISTOGARD-TECH-ADD-ON.
342900
343000     MOVE 0 TO H-NEW-TECH-ADDON-VISTOGARD
343100               H-LESSER-VISTOGARD-STOP-1
343200               H-LESSER-VISTOGARD-STOP-2
343300               H-CSTMED-VISTOGARD-STOP.
343400
343500 5080-COMPUTE-VISTOGARD.
343600
343700     MOVE  37500.00 TO H-CSTMED-VISTOGARD-STOP.
343800
343900     COMPUTE H-LESSER-VISTOGARD-STOP-1 ROUNDED =
344000                  H-CSTMED-VISTOGARD-STOP.
344100
344200     COMPUTE H-LESSER-VISTOGARD-STOP-2 ROUNDED =
344300          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
344400                     H-BASE-DRG-PAYMENT)) * .5.
344500
344600     IF H-LESSER-VISTOGARD-STOP-2 > 0
344700        IF H-LESSER-VISTOGARD-STOP-1 < H-LESSER-VISTOGARD-STOP-2
344800         MOVE H-LESSER-VISTOGARD-STOP-1 TO
344900                                H-NEW-TECH-ADDON-VISTOGARD
345000        ELSE
345100         MOVE H-LESSER-VISTOGARD-STOP-2 TO
345200                                H-NEW-TECH-ADDON-VISTOGARD
345300     ELSE
345400        MOVE ZEROES          TO H-NEW-TECH-ADDON-VISTOGARD.
345500
345600 5080-ADD-TECH-CASES.
345700
345800     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
345900             H-NEW-TECH-PAY-ADD-ON +
346000             H-NEW-TECH-ADDON-VISTOGARD.
346100
346200 5080-EXIT.    EXIT.
346300
346400**************************************************************
346500* CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM *
346600**************************************************************
346700 5500-CAP-CALC-TECH-ADD-ON.
346800
346900     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
347000     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
347100
347200     COMPUTE H-OPER-BILL-COSTS ROUNDED =
347300         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
347400         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
347500
347600     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
347700                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
347800
347900     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
348000                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
348100
348200     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
348300         H-NEW-TECH-ADDON-CAPDIF  > 0
348400        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
348500             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
348600
348700 5500-EXIT.    EXIT.
348800
348900***********************************************************
349000 6000-CALC-READMIS-REDU.
349100***********************************************************
349200*---------------------------------------------------------*
349300* (YEARCHANGE 2016.0)
349400* READMISSIONS PROCESS ADJUSTMENTS
349500*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
349600*---------------------------------------------------------*
349700
349800     MOVE 0 TO H-READMIS-ADJUST-AMT.
349900
350000     IF P-HOSP-READMISSION-REDU = '1'
350100           GO TO 6000-EDIT-READMISN
350200     ELSE
350300           NEXT SENTENCE.
350400
350500     IF P-HOSP-READMISSION-REDU = '0' AND
350600        P-HOSP-HRR-ADJUSTMT = 0.0000
350700           MOVE ZEROES TO H-READMIS-ADJUST-AMT
350800           GO TO 6000-EXIT.
350900
351000     IF P-HOSP-READMISSION-REDU = '0' AND
351100        P-HOSP-HRR-ADJUSTMT > 0.0000
351200           MOVE 65 TO PPS-RTC
351300           MOVE ZEROES TO H-READMIS-ADJUST-AMT
351400           GO TO 6000-EXIT.
351500
351600     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
351700                                  '6' OR '7' OR '8' OR
351800                                  '9' OR ' '
351900           MOVE 65 TO PPS-RTC
352000           MOVE ZEROES TO H-READMIS-ADJUST-AMT
352100           GO TO 6000-EXIT.
352200
352300 6000-EDIT-READMISN.
352400
352500     IF P-HOSP-HRR-ADJUSTMT < 0.9700
352600           MOVE 65 TO PPS-RTC
352700           MOVE ZEROES TO H-READMIS-ADJUST-AMT
352800           GO TO 6000-EXIT.
352900
353000     IF P-HOSP-HRR-ADJUSTMT > 1.0000
353100           MOVE 65 TO PPS-RTC
353200           MOVE ZEROES TO H-READMIS-ADJUST-AMT
353300           GO TO 6000-EXIT.
353400
353500     IF P-READ-INVALID-STATE
353600           MOVE 65 TO PPS-RTC
353700           MOVE ZEROES TO H-READMIS-ADJUST-AMT
353800           GO TO 6000-EXIT.
353900
354000 6000-COMPUTE-READMISN.
354100
354200        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
354300              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
354400                H-OPER-BASE-DRG-PAY).
354500
354600 6000-EXIT.    EXIT.
354700
354800***********************************************************
354900 7000-CALC-VALUE-BASED-PURCH.
355000***********************************************************
355100*---------------------------------------------------------*
355200* (YEARCHANGE 2016.0)
355300* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
355400*   + FY17: RANGE OF ALLOWABLE FACTORS (< 0.98 OR > 2.0)
355500*---------------------------------------------------------*
355600
355700     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
355800
355900     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
356000           NEXT SENTENCE
356100     ELSE
356200           MOVE 68 TO PPS-RTC
356300           GO TO 7000-EXIT.
356400
356500     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
356600           GO TO 7000-EXIT.
356700
356800     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
356900         P-NEW-CBSA-HOSP-QUAL-IND = '1'
357000           NEXT SENTENCE
357100     ELSE
357200           MOVE 68 TO PPS-RTC
357300           GO TO 7000-EXIT.
357400
357500     IF  P-VBP-INVALID-STATE
357600           MOVE 68 TO PPS-RTC
357700           GO TO 7000-EXIT
357800     ELSE
357900           NEXT SENTENCE.
358000
358100     IF P-VAL-BASED-PURCH-ADJUST < 0.9800000000 OR
358200        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
358300           MOVE 68 TO PPS-RTC
358400           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
358500           GO TO 7000-EXIT
358600     ELSE
358700           GO TO 7000-COMPUTE-VAL-BASED-PUR.
358800
358900 7000-COMPUTE-VAL-BASED-PUR.
359000
359100     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
359200              ((P-VAL-BASED-PURCH-ADJUST *
359300                  H-OPER-BASE-DRG-PAY) -
359400                  H-OPER-BASE-DRG-PAY).
359500
359600 7000-EXIT.    EXIT.
359700
359800***********************************************************
359900 8000-CALC-BUNDLE-REDU.
360000***********************************************************
360100***** CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
360200***********************************************************
360300
360400     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
360500     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
360600
360700     IF '61' =  B-DEMO-CODE1  OR
360800                B-DEMO-CODE2  OR
360900                B-DEMO-CODE3  OR
361000                B-DEMO-CODE4
361100         NEXT SENTENCE
361200     ELSE
361300         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
361400           GO TO 8000-EXIT.
361500
361600     IF P-MODEL1-BUNDLE-DISPRCNT > .00
361700           GO TO 8000-COMPUTE-BUNDLE
361800     ELSE
361900           NEXT SENTENCE.
362000
362100     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
362200           GO TO 8000-EXIT.
362300
362400 8000-COMPUTE-BUNDLE.
362500
362600     IF  B-DISCHARGE-DATE < 20140401 AND
362700         P-MODEL1-BUNDLE-DISPRCNT = .01
362800         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
362900          (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
363000     ELSE
363100         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
363200          (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
363300
363400        COMPUTE H-BUNDLE-ADJUST-AMT      ROUNDED =
363500              ((WK-MODEL1-BUNDLE-DISPRCNT *
363600                                     H-OPER-BASE-DRG-PAY) -
363700                H-OPER-BASE-DRG-PAY).
363800
363900        COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
364000
364100 8000-EXIT.    EXIT.
364200
364300***********************************************************
364400 9000-CALC-EHR-SAVING.
364500***********************************************************
364600*---------------------------------------------------------*
364700* (YEARCHANGE 2017.0)
364800* CASES INVOLVING EHR SAVINGS
364900*   + FY17: ANNUAL UPDATE TO BELOW VALUES
365000*   + EHR-FULL = FULL MB / NO EHR MB
365100*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
365200*---------------------------------------------------------*
365300
365400     MOVE 1.020326223 TO H-MB-RATIO-EHR-FULL.
365500     MOVE 1.020464881 TO H-MB-RATIO-EHR-QUAL-FULL.
365600     MOVE 0 TO H-EHR-SUBSAV-QUANT.
365700     MOVE 0 TO H-EHR-SUBSAV-LV.
365800     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
365900     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
366000
366100     IF P-EHR-REDUC-IND = 'Y'
366200         NEXT SENTENCE
366300     ELSE
366400         GO TO 9000-EXIT.
366500
366600 9000-COMPUTE-EHR.
366700
366800* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
366900* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
367000* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
367100* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
367200* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
367300* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
367400* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
367500* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
367600* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
367700* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
367800* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
367900* OUT.
368000*
368100* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
368200* SAVINGS CALCULATION SO CALCULATE AN OPERATING
368300* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
368400* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
368500* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
368600
368700      COMPUTE H-EHR-SUBSAV-QUANT =
368800           (PPS-OPER-HSP-PART +
368900            PPS-OPER-FSP-PART +
369000            PPS-OPER-DSH-ADJ +
369100            PPS-OPER-IME-ADJ +
369200            H-READMIS-ADJUST-AMT +
369300            H-VAL-BASED-PURCH-ADJUST-AMT +
369400            H-BUNDLE-ADJUST-AMT).
369500
369600* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
369700* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
369800* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
369900* READM VBP AND BUNDLE
370000* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
370100* FOR EHR SAVINGS FIELD;
370200
370300      MOVE 0 TO H-EHR-SUBSAV-LV.
370400
370500      IF P-NEW-TEMP-RELIEF-IND = 'Y'
370600         AND P-LV-ADJ-FACTOR > 0.00
370700         AND P-LV-ADJ-FACTOR <= 0.25
370800      COMPUTE H-EHR-SUBSAV-LV =
370900          (PPS-OPER-HSP-PART +
371000           PPS-OPER-FSP-PART +
371100           PPS-OPER-DSH-ADJ +
371200           PPS-OPER-IME-ADJ ) * P-LV-ADJ-FACTOR.
371300
371400      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
371500           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
371600
371700* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
371800* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
371900* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
372000* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
372100* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
372200* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
372300* OF .625 ONLY.
372400
372500       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
372600       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
372700
372800     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
372900        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
373000          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
373100
373200        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
373300          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
373400
373500 9000-EXIT.    EXIT.
373600
373700*---------------------------------------------------------*
373800* (YEARCHANGE 2016.0)
373900*---------------------------------------------------------*
374000 9010-CALC-STANDARD-CHG.
374100
374200***********************************************************
374300***ÝCM-P3¨ STANDARDIZED OPERATING COST CALCULATION
374400
374500     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
374600               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
374700        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
374800        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
374900        ((H-LABOR-PCT * H-WAGE-INDEX) +
375000               (H-NONLABOR-PCT * H-OPER-COLA))
375100     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
375200
375300***********************************************************
375400***ÝCM-P3¨ STANDARDIZED CAPITAL COST CALCULATION
375500
375600     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
375700       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
375800        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
375900               (H-CAPI-GAF * H-CAPI-COLA)
376000     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
376100
376200***********************************************************
376300***ÝCM-P3¨ STANDARDIZED OPERATING TRESHOLD
376400
376500     MOVE 5516.14 TO H-OPER-BASE.
376600
376700     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
376800      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
376900                        +
377000           (H-OPER-BASE * H-DRG-WT-FRCTN)
377100                        +
377200              H-NEW-TECH-PAY-ADD-ON.
377300
377400******************************************************
377500***ÝCM-P3¨ STANDARDIZED CAPITAL TRESHOLD
377600
377700     MOVE 446.79 TO H-CAPI-BASE.
377800
377900     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
378000     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
378100                     +
378200     (H-CAPI-BASE * H-DRG-WT-FRCTN).
378300
378400******************************************************
378500***ÝCM-P3¨ STANDARDIZED OPERATING OUTLIER CALCULATION
378600
378700     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
378800        (H-OPER-STDZ-DOLLAR-THRESHOLD +
378900                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
379000                          AND
379100         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
379200
379300       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
379400        (H-CSTOUT-PCT  *
379500        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
379600
379700     ELSE
379800       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
379900
380000******************************************************
380100***ÝCM-P3¨ STANDARDIZED CAPITAL OUTLIER CALCULATION
380200
380300     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
380400        (H-OPER-STDZ-DOLLAR-THRESHOLD +
380500                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
380600                          AND
380700         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
380800
380900      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
381000      (H-CSTOUT-PCT  *
381100      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
381200     ELSE
381300      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
381400
381500*******************************************************
381600***ÝCM-P3¨ STANDARDIZED ALLOWED AMOUNT CALCULATION
381700
381800      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
381900       (H-OPER-BASE + H-CAPI-BASE)
382000                 *
382100       H-DRG-WT-FRCTN
382200                 +
382300       H-OPER-STDZ-COST-OUTLIER
382400                 +
382500       H-CAPI-STDZ-COST-OUTLIER
382600                 +
382700       H-NEW-TECH-PAY-ADD-ON.
382800
382900 9010-EXIT.    EXIT.
383000
383100***********************************************************
383200******        L A S T   S O U R C E   S T A T E M E N T   *****
