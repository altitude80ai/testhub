000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL182.
000300*REVISED.                   09-20-2017.
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
002000     'PPCAL182      - W O R K I N G   S T O R A G E'.
002100 01  CAL-VERSION                    PIC X(05)  VALUE 'C18.2'.
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
004800* (YEARCHANGE 2018.0)
004900* LABOR & NON-LABOR RATES TABLE
005000*---------------------------------------------------------*
005100
005200 COPY RATEX181.
005300
005400*---------------------------------------------------------*
005500* (YEARCHANGE 2018.0)
005600* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE
005700*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE
005800*---------------------------------------------------------*
005900
006000 COPY DRGSX180.
006100
006200*---------------------------------------------------------*
006300* (YEARCHANGE 2018.0)
006400* TWO MIDNIGHT STAY POLICY ADJUSTMENT FACTOR TABLE
006500*---------------------------------------------------------*
006600
006700 COPY MIDNIGHT.
006800
006900***********************************************************
007000***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
007100***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
007200***********************************************************
007300
007400 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
007500 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
007600 01  MES-PPS-PROV                   PIC X(06).
007700 01  MES-PPS-STATE                  PIC X(02).
007800 01  MES-INTRO                      PIC X(53) VALUE SPACES.
007900 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
008000 01  MES-SSRFBN.
008100     05 MES-SSRFBN-STATE PIC 99.
008200     05 FILLER           PIC XX.
008300     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
008400     05 FILLER           PIC XX.
008500     05 MES-SSRFBN-CODE2 PIC 99.
008600     05 FILLER           PIC XX.
008700     05 MES-SSRFBN-STNAM PIC X(20).
008800     05 MES-SSRFBN-REST  PIC X(22).
008900
009000 01 WK-HLDDRG-DATA.
009100     05  HLDDRG-DATA.
009200         10  HLDDRG-DRGX               PIC X(03).
009300         10  FILLER1                   PIC X(01).
009400         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
009500         10  FILLER2                   PIC X(01).
009600         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
009700         10  FILLER3                   PIC X(05).
009800         10  HLDDRG-LOW                PIC X(01).
009900         10  FILLER5                   PIC X(01).
010000         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
010100         10  FILLER6                   PIC X(02).
010200         10  HLDDRG-PAC                PIC X(01).
010300         10  FILLER7                   PIC X(01).
010400         10  HLDDRG-SPPAC              PIC X(01).
010500         10  FILLER8                   PIC X(02).
010600         10  HLDDRG-DESC               PIC X(26).
010700
010800 01 WK-HLDDRG-DATA2.
010900     05  HLDDRG-DATA2.
011000         10  HLDDRG-DRGX2               PIC X(03).
011100         10  FILLER21                   PIC X(01).
011200         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
011300         10  FILLER22                   PIC X(01).
011400         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
011500         10  FILLER23                   PIC X(05).
011600         10  HLDDRG-LOW2                PIC X(01).
011700         10  FILLER25                   PIC X(01).
011800         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
011900         10  FILLER26                   PIC X(02).
012000         10  HLDDRG-TRANS-FLAGS.
012100                   88  D-DRG-POSTACUTE-50-50
012200                   VALUE 'Y Y'.
012300                   88  D-DRG-POSTACUTE-PERDIEM
012400                   VALUE 'Y  '.
012500             15  HLDDRG-PAC2            PIC X(01).
012600             15  FILLER27               PIC X(01).
012700             15  HLDDRG-SPPAC2          PIC X(01).
012800         10  FILLER28                   PIC X(02).
012900         10  HLDDRG-DESC2               PIC X(26).
013000         10  HLDDRG-VALID               PIC X(01).
013100
013200 01  MES-LOWVOL.
013300     05  MES-LOWVOL-PROV             PIC X(6).
013400     05  FILLER                      PIC XXX.
013500     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
013600
013700 01  WK-UNCOMP-CARE.
013800     05  WK-UNCOMP-CARE-PROV         PIC X(6).
013900     05  FILLER                      PIC X.
014000     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
014100
014200 01 WK-HLD-MID-DATA.
014300     05  HLD-MID-DATA.
014400         10  HLD-MID-MSAX              PIC X(04).
014500         10  FILLER1                   PIC X(01).
014600         10  HLD-MID-ADJ-FACT          PIC 9(02)V9(06).
014700
014800 01  HLD-PPS-DATA.
014900         10  HLD-PPS-RTC                PIC 9(02).
015000         10  HLD-PPS-WAGE-INDX          PIC 9(02)V9(04).
015100         10  HLD-PPS-OUTLIER-DAYS       PIC 9(03).
015200         10  HLD-PPS-AVG-LOS            PIC 9(02)V9(01).
015300         10  HLD-PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
015400         10  HLD-PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
015500         10  HLD-PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
015600         10  HLD-PPS-OPER-HSP-PART      PIC 9(06)V9(02).
015700         10  HLD-PPS-OPER-FSP-PART      PIC 9(06)V9(02).
015800         10  HLD-PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
015900         10  HLD-PPS-REG-DAYS-USED      PIC 9(03).
016000         10  HLD-PPS-LTR-DAYS-USED      PIC 9(02).
016100         10  HLD-PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
016200         10  HLD-PPS-CALC-VERS          PIC X(05).
016300
016400 01  WK-NEW-TECH-VARIABLES.
016500     05  WK-PROC-NEW-TECH       PIC X(07).
016600             88  PROC-ARGUS
016700                   VALUE '08H005Z' '08H105Z'.
016800             88  PROC-BLINATU
016900                   VALUE 'XW03351' 'XW04351'.
017000             88  PROC-CARDIO
017100                   VALUE '02HQ30Z' '02HR30Z'.
017200             88  PROC-DEFITELIO
017300                   VALUE 'XW03392' 'XW04392'.
017400             88  PROC-EDWARDS
017500                   VALUE 'X2RF032'.
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
020600             88  PROC-STELARA
020700                   VALUE 'XW033F3'.
020800             88  PROC-VISTOGARD
020900                   VALUE 'XW0DX82'.
021000             88  PROC-VORAXAZE
021100                   VALUE '3E033GQ' '3E043GQ'.
021200             88  PROC-ZENITH
021300                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
021400             88  PROC-ZILVER
021500                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
021600                         '047L34Z' '047L44Z'.
021700             88  PROC-ZINPLAVA
021800                   VALUE 'XW033A3' 'XW043A3'.
021900
022000     05  WK-DIAG-NEW-TECH       PIC X(07).
022100             88  DIAG-AUTOLITT
022200                   VALUE '1910   ' '1911   ' '1912   ' '1913  '
022300                         '1914   ' '1915   ' '1916   ' '1917  '
022400                         '1918   ' '1919   ' 'C710   ' 'C711  '
022500                         'C712   ' 'C713   ' 'C714   ' 'C715  '
022600                         'C716   ' 'C717   ' 'C718   ' 'C719  '.
022700             88  DIAG-ISLET
022800                   VALUE 'Z006   '.
022900             88  DIAG-KCENTRA
023000                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
023100                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
023200                         'D6832  ' 'D684   '.
023300             88  DIAG-VISTOGARD
023400                   VALUE 'T451X1A' 'T451X1D' 'T451X1S' 'T451X5A'
023500                         'T451X5D' 'T451X5S'.
023600
023700     05  WK-NEW-TECH-FLAGS.
023800         10  PROC-ARGUS-FLAG         PIC X(01).
023900         10  PROC-BLINATU-FLAG       PIC X(01).
024000         10  PROC-CARDIO-FLAG        PIC X(01).
024100         10  PROC-DEFITELIO-FLAG     PIC X(01).
024200         10  PROC-EDWARDS-FLAG       PIC X(01).
024300         10  PROC-GORE-FLAG          PIC X(01).
024400         10  PROC-IDARUCIZ-FLAG      PIC X(01).
024500         10  PROC-ISLET-FLAG         PIC X(01).
024600         10  PROC-KCENTRA-FLAG       PIC X(01).
024700         10  PROC-LUTONIX-FLAG       PIC X(01).
024800         10  PROC-MAGEC-FLAG         PIC X(01).
024900         10  PROC-MITRACLP-FLAG      PIC X(01).
025000         10  PROC-RNSSYS1-FLAG       PIC X(01).
025100         10  PROC-RNSSYS2-FLAG       PIC X(01).
025200         10  PROC-STELARA-FLAG       PIC X(01).
025300         10  PROC-VISTOGARD-FLAG     PIC X(01).
025400         10  PROC-VORAXAZE-FLAG      PIC X(01).
025500         10  PROC-ZENITH-FLAG        PIC X(01).
025600         10  PROC-ZILVER-FLAG        PIC X(01).
025700         10  PROC-ZINPLAVA-FLAG      PIC X(01).
025800         10  DIAG-AUTOLITT-FLAG      PIC X(01).
025900         10  DIAG-ISLET-FLAG         PIC X(01).
026000         10  DIAG-KCENTRA-FLAG       PIC X(01).
026100         10  DIAG-VISTOGARD-FLAG     PIC X(01).
026200
026300 LINKAGE SECTION.
026400***************************************************************
026500*                 * * * * * * * * *                           *
026600*    REVIEW CODES ARE USED TO DIRECT THE PPCAL  SUBROUTINE    *
026700*    IN HOW TO PAY THE BILL.                                  *
026800*                         *****                               *
026900*    COMMENTS  ** CLAIMS RECEIVED WITH CONDITION CODE 66      *
027000*                 SHOULD BE PROCESSED UNDER REVIEW CODE 06,   *
027100*                 07 OR 11 AS APPROPRIATE TO EXCLUDE ANY      *
027200*                 OUTLIER COMPUTATION.                        *
027300*                         *****                               *
027400*         REVIEW-CODE:                                        *
027500*            00 = PAY-WITH-OUTLIER.                           *
027600*                 WILL CALCULATE THE STANDARD PAYMENT.        *
027700*                 WILL ALSO ATTEMPT TO PAY ONLY COST          *
027800*                 OUTLIERS, DAY OUTLIERS EXPIRED 10/01/97     *
027900*            03 = PAY-PERDIEM-DAYS.                           *
028000*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
028100*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
028200*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
028300*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
028400*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
028500*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
028600*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
028700*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
028800*                 BILL EXCEED THE COST THRESHOLD.             *
028900*            06 = PAY-XFER-NO-COST                            *
029000*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
029100*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
029200*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
029300*                 FOR THE DRG.  IF COVERED DAYS EQUAL OR      *
029400*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
029500*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
029600*                 CALCULATE ANY COST OUTLIER PORTION          *
029700*                 OF THE PAYMENT.                             *
029800*            07 = PAY-WITHOUT-COST.                           *
029900*                 WILL CALCULATE THE STANDARD PAYMENT         *
030000*                 WITHOUT COST PORTION.                       *
030100*            09 = PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS    *
030200*                 50-50> NOW USES Y INDICATORS ON DRGS
030300*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
030400*                        RULE
030500* =======================================================
030600* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
030700* =======================================================
030800*
030900*
031000*     FULL PERDIEM >   NOW USES Y INDICATORS ON DRGS
031100*                      SEE TABLE 5 FROM ANNUAL IPPS FINAL
031200*                      RULE
031300*
031400*                               POST-ACUTE TRANSFERS          *
031500*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
031600*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
031700*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
031800*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
031900*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
032000*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
032100*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
032200*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
032300*                 BILL EXCEED THE COST THRESHOLD.             *
032400*            11 = PAY-XFER-SPEC-DRG-NO-COST                   *
032500*                 POST-ACUTE TRANSFERS                        *
032600*                 50-50> NOW USES Y INDICATORS ON DRGS
032700*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
032800*                        RULE
032900* =======================================================
033000* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
033100* =======================================================
033200*
033300*     FULL PERDIEM >  NOW USES Y INDICATORS ON DRGS
033400*                     SEE TABLE 5
033500*
033600*
033700*                               POST-ACUTE TRANSFERS          *
033800*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
033900*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
034000*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
034100*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
034200*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
034300*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
034400*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
034500*                 PAYMENT.                                    *
034600***************************************************************
034700
034800**************************************************************
034900*      MILLINNIUM COMPATIBLE                                 *
035000*      THIS IS THE BILL-RECORD THAT WILL BE PASSED BACK FROM *
035100*      THE PPCAL001 PROGRAM AND AFTER FOR PROCESSING         *
035200*      IN THE NEW FORMAT                                     *
035300**************************************************************
035400 01  BILL-NEW-DATA.
035500         10  B-NPI10.
035600             15  B-NPI8             PIC X(08).
035700             15  B-NPI-FILLER       PIC X(02).
035800         10  B-PROVIDER-NO          PIC X(06).
035900             88  B-FORMER-MDH-PROVIDERS
036000                                      VALUE '080006' '140184'
036100                                            '390072' '420019'
036200                                            '440031' '450451'
036300                                            '490019' '510062'.
036400         10  B-REVIEW-CODE          PIC 9(02).
036500             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
036600             88  PAY-WITH-OUTLIER     VALUE 00 07.
036700             88  PAY-PERDIEM-DAYS     VALUE 03.
036800             88  PAY-XFER-NO-COST     VALUE 06.
036900             88  PAY-WITHOUT-COST     VALUE 07.
037000             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
037100             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
037200         10  B-DRG                  PIC 9(03).
037300             88  B-DRG-SPIRATN-DRG
037400                   VALUE 163 164 165.
037500             88  B-DRG-SPIRATN-DRG11
037600                   VALUE 199 200 201.
037700             88  B-DRG-AUTOLITT-DRG
037800                   VALUE 25 26 27.
037900
038000* =======================================================
038100* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
038200* =======================================================
038300*
038400*            88  B-DRG-POSTACUTE-PERDIEM
038500*                         VALUE  NOW USES Y INDICATORS ON DRGS
038600*                         SEE TABLE 5
038700*                         D-DRG-POSTACUTE-PERDIEM
038800
038900         10  B-LOS                  PIC 9(03).
039000         10  B-COVERED-DAYS         PIC 9(03).
039100         10  B-LTR-DAYS             PIC 9(02).
039200         10  B-DISCHARGE-DATE.
039300             15  B-DISCHG-CC        PIC 9(02).
039400             15  B-DISCHG-YY        PIC 9(02).
039500             15  B-DISCHG-MM        PIC 9(02).
039600             15  B-DISCHG-DD        PIC 9(02).
039700         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
039800         10  B-PROCEDURE-CODE-TABLE.
039900             15  B-PROCEDURE-CODE    PIC X(07) OCCURS 25 TIMES
040000                 INDEXED BY IDX-DIAG.
040100         10  B-DIAGNOSIS-CODE-TABLE.
040200             15  B-DIAGNOSIS-CODE    PIC X(07) OCCURS 25 TIMES
040300                 INDEXED BY IDX-PROC.
040400         10  B-DEMO-DATA.
040500             15  B-DEMO-CODE1           PIC X(02).
040600             15  B-DEMO-CODE2           PIC X(02).
040700             15  B-DEMO-CODE3           PIC X(02).
040800             15  B-DEMO-CODE4           PIC X(02).
040900         10  B-NDC-DATA.
041000             15  B-NDC-NUMBER           PIC X(11).
041100               88  B-NDC-DIFICID-NDC
041200                   VALUE '52015008001'.
041300         10  FILLER                     PIC X(73).
041400
041500
041600***************************************************************
041700*    THIS DATA IS CALCULATED BY THIS PPCAL  SUBROUTINE        *
041800*    AND PASSED BACK TO THE CALLING PROGRAM                   *
041900*            RETURN CODE VALUES (PPS-RTC)                     *
042000*                                                             *
042100*            PPS-RTC 00-49 = HOW THE BILL WAS PAID            *
042200*                                                             *
042300*      PPS-RTC 30,33,40,42,44  = OUTLIER RECONCILIATION       *
042400*                                                             *
042500*           30,00 = PAID NORMAL DRG PAYMENT                   *
042600*                                                             *
042700*              01 = PAID AS A DAY-OUTLIER.                    *
042800*                   NOTE:                                     *
042900*                     DAY-OUTLIER NO LONGER BEING PAID        *
043000*                         AS OF 10/01/97                      *
043100*                                                             *
043200*              02 = PAID AS A COST-OUTLIER.                   *
043300*                                                             *
043400*           33,03 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
043500*                   AND INCLUDING THE FULL DRG.               *
043600*              05 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
043700*                   AND INCLUDING THE FULL DRG WHICH ALSO     *
043800*                   QUALIFIED FOR A COST OUTLIER PAYMENT.     *
043900*              06 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
044000*                   AND INCLUDING THE FULL DRG. PROVIDER      *
044100*                   REFUSED COST OUTLIER.                     *
044200*           40,10 = POST-ACUTE TRANSFER                       *
044300*                   SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
044400*
044500* =======================================================
044600* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
044700* =======================================================
044800*
044900*           42,12 = POST-ACAUTE TRANSFER WITH SPECIFIC DRGS   *
045000*                       THE FOLLOWING DRG'S                   *
045100*                   DRG =  VALUE  NOW USES Y INDICATORS ON DRGS
045200*                       SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
045300*                          D-DRG-POSTACUTE-PERDIEM
045400*
045500*           44,14 = PAID NORMAL DRG PAYMENT WITH              *
045600*                    PERDIEM DAYS = OR > GM  ALOS             *
045700*              16 = PAID AS A COST-OUTLIER WITH               *
045800*                    PERDIEM DAYS = OR > GM  ALOS             *
045900*                                                             *
046000*            PPS-RTC 50-99 = WHY THE BILL WAS NOT PAID        *
046100*              51 = NO PROVIDER SPECIFIC INFO FOUND           *
046200*              52 = INVALID CBSA# IN PROVIDER FILE            *
046300*                   OR INVALID WAGE INDEX                     *
046400*                                      OR                     *
046500*                   INVALID PROVIDER TYPES ON PROVIDER FILE   *
046600*              53 = WAIVER STATE - NOT CALCULATED BY PPS OR   *
046700*                   OR                                         *
046800*                   INVALID STATE CODE IN COMBINATION WITH     *
046900*                   HAC FLAG                                  *
047000*              54 = INVALID DRG                               *
047100*              55 = DISCHARGE DATE < PROVIDER EFF START DATE  *
047200*                                      OR                     *
047300*                   DISCHARGE DATE < CBSA EFF START DATE      *
047400*                   FOR PPS                                   *
047500*                                      OR                     *
047600*                   PROVIDER HAS BEEN TERMINATED ON OR BEFORE *
047700*                   DISCHARGE DATE                            *
047800*              56 = INVALID LENGTH OF STAY                    *
047900*              57 = REVIEW CODE INVALID (NOT 00 03 06 07 09   *
048000*                                        NOT 11)              *
048100*              58 = TOTAL CHARGES NOT NUMERIC                 *
048200*              61 = LIFETIME RESERVE DAYS NOT NUMERIC         *
048300*                   OR BILL-LTR-DAYS > 60                     *
048400*              62 = INVALID NUMBER OF COVERED DAYS            *
048500*              65 = PAY-CODE NOT = A,B OR C ON PROVIDER        *
048600*                   SPECIFIC FILE FOR CAPITAL                  *
048700*                   OR                                         *
048800*                   INVALID READMISSION FLAG IN PSF FILE       *
048900*                   OR                                         *
049000*                   BLANK READMISSION FLAG IN PSF FILE         *
049100*                   OR                                         *
049200*                   READMISSION ADJUSTMENT IS INVALID OR       *
049300*                   OUT OF RANGE IN PSF FILE                   *
049400*                   OR                                         *
049500*                   BLANK READMISSION ADJUSTMENT IN PSF FILE   *
049600*                   OR                                         *
049700*                   INVALID STATE CODE IN COMBINATION WITH     *
049800*                   READMISSION FLAG IN PSF FILE               *
049900*                   OR                                         *
050000*                   INVALID EHR FLAG IN PSF FILE               *
050100*                   (MUST BE A "Y" OR BLANK)                   *
050200*              67 = COST OUTLIER WITH LOS > COVERED DAYS      **
050300*                   OR COST OUTLIER THRESHOLD CALUCULATION    **
050400*              68 = INVALID VALUE BASED PURCHASE FLAG IN PSF   *
050500*                   FILE                                       *
050600*                   OR                                         *
050700*                   BLANK VALUE BASED PURCHASE FLAG IN PSF FILE*
050800*                   OR                                         *
050900*                   VALUE BASED PURCHASE ADJUSTMEMT IS INVALID *
051000*                   OR OUT OF RANGE IN PSF FILE                *
051100*                   INDICATOR                                  *
051200*                   OR                                         *
051300*                   BLANK VALUE BASED PURCHASE ADJUSTMEMT IN   *
051400*                   PSF FILE                                   *
051500*                   OR                                         *
051600*                   INVALID COMBINATION OF HOSPITAL QUALITY    *
051700*                   INDICATOR                                  *
051800*                   AND VALUE BASED PURCHASE FLAG IN PSF FILE  *
051900*                   OR                                         *
052000*                   INVALID STATE CODE IN COMBINATION WITH VALUE
052100*                   BASED PURCHASE FLAG IN PSF FILE            *
052200*              98 = CANNOT PROCESS BILL OLDER THAN 5 YEARS    *
052300***************************************************************
052400 01  PPS-DATA.
052500         10  PPS-RTC                PIC 9(02).
052600         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
052700         10  PPS-OUTLIER-DAYS       PIC 9(03).
052800         10  PPS-AVG-LOS            PIC 9(02)V9(01).
052900         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
053000         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
053100         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
053200         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
053300         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
053400         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
053500         10  PPS-REG-DAYS-USED      PIC 9(03).
053600         10  PPS-LTR-DAYS-USED      PIC 9(02).
053700         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
053800         10  PPS-CALC-VERS          PIC X(05).
053900
054000*****************************************************************
054100*            THESE ARE THE VERSIONS OF THE PPCAL
054200*           PROGRAMS THAT WILL BE PASSED BACK----
054300*          ASSOCIATED WITH THE BILL BEING PROCESSED
054400*****************************************************************
054500 01  PRICER-OPT-VERS-SW.
054600     02  PRICER-OPTION-SW          PIC X(01).
054700         88  ALL-TABLES-PASSED          VALUE 'A'.
054800         88  PROV-RECORD-PASSED         VALUE 'P'.
054900         88  ADDITIONAL-VARIABLES       VALUE 'M'.
055000         88  PC-PRICER                  VALUE 'C'.
055100     02  PPS-VERSIONS.
055200         10  PPDRV-VERSION         PIC X(05).
055300
055400*****************************************************************
055500*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
055600*          ASSOCIATED WITH THE BILL BEING PROCESSED
055700*****************************************************************
055800 01  PPS-ADDITIONAL-VARIABLES.
055900     05  PPS-HSP-PCT                PIC 9(01)V9(02).
056000     05  PPS-FSP-PCT                PIC 9(01)V9(02).
056100     05  PPS-NAT-PCT                PIC 9(01)V9(02).
056200     05  PPS-REG-PCT                PIC 9(01)V9(02).
056300     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
056400     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
056500     05  PPS-DRG-WT                 PIC 9(02)V9(04).
056600     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
056700     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
056800     05  PPS-REG-LABOR              PIC 9(05)V9(02).
056900     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
057000     05  PPS-OPER-COLA              PIC 9(01)V9(03).
057100     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
057200     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
057300     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
057400     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
057500     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
057600     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
057700     05  PPS-CAPITAL-VARIABLES.
057800         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
057900         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
058000         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
058100         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
058200         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
058300         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
058400         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
058500         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
058600     05  PPS-CAPITAL2-VARIABLES.
058700         10  PPS-CAPI2-PAY-CODE             PIC X(1).
058800         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
058900         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
059000     05  PPS-OTHER-VARIABLES.
059100         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
059200         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
059300         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
059400         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
059500         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
059600         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
059700         10  PPS-HOSP-READMISSION-REDU      PIC X.
059800         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
059900         10  PPS-OPERATNG-DATA.
060000             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
060100             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
060200             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
060300     05  PPS-PC-OTH-VARIABLES.
060400         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
060500         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
060600         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
060700         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
060800         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
060900         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
061000         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
061100         10  PPS-PC-HMO-FLAG                PIC X(01).
061200         10  PPS-PC-COT-FLAG                PIC X(01).
061300         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
061400         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
061500     05  PPS-ADDITIONAL-PAY-INFO-DATA.
061600         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
061700         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
061800         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
061900         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
062000     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
062100         10  PPS-HAC-PROG-REDUC-IND      PIC X.
062200         10  PPS-EHR-PROG-REDUC-IND      PIC X.
062300         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
062400         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
062500         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
062600         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
062700     05 PPS-FILLER                       PIC X(0897).
062800
062900 01  PROV-NEW-HOLD.
063000     02  PROV-NEWREC-HOLD1.
063100         05  P-NEW-NPI10.
063200             10  P-NEW-NPI8             PIC X(08).
063300             10  P-NEW-NPI-FILLER       PIC X(02).
063400         05  P-NEW-PROVIDER-NO.
063500             88  P-NEW-DSH-ADJ-PROVIDERS
063600                             VALUE '180049' '190044' '190144'
063700                                   '190191' '330047' '340085'
063800                                   '370016' '370149' '420043'.
063900             10  P-NEW-STATE            PIC 9(02).
064000                 88  P-VBP-INVALID-STATE
064100                             VALUE 21 80 40 84.
064200                 88  P-READ-INVALID-STATE
064300                             VALUE 40 84.
064400                 88  P-HAC-INVALID-STATE
064500                             VALUE 40 84.
064600                 88  P-PR-NEW-STATE
064700                             VALUE 40 84.
064800             10  FILLER                 PIC X(04).
064900         05  P-NEW-DATE-DATA.
065000             10  P-NEW-EFF-DATE.
065100                 15  P-NEW-EFF-DT-CC    PIC 9(02).
065200                 15  P-NEW-EFF-DT-YY    PIC 9(02).
065300                 15  P-NEW-EFF-DT-MM    PIC 9(02).
065400                 15  P-NEW-EFF-DT-DD    PIC 9(02).
065500             10  P-NEW-FY-BEGIN-DATE.
065600                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
065700                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
065800                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
065900                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
066000             10  P-NEW-REPORT-DATE.
066100                 15  P-NEW-REPORT-DT-CC PIC 9(02).
066200                 15  P-NEW-REPORT-DT-YY PIC 9(02).
066300                 15  P-NEW-REPORT-DT-MM PIC 9(02).
066400                 15  P-NEW-REPORT-DT-DD PIC 9(02).
066500             10  P-NEW-TERMINATION-DATE.
066600                 15  P-NEW-TERM-DT-CC   PIC 9(02).
066700                 15  P-NEW-TERM-DT-YY   PIC 9(02).
066800                 15  P-NEW-TERM-DT-MM   PIC 9(02).
066900                 15  P-NEW-TERM-DT-DD   PIC 9(02).
067000         05  P-NEW-WAIVER-CODE          PIC X(01).
067100             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
067200         05  P-NEW-INTER-NO             PIC 9(05).
067300         05  P-NEW-PROVIDER-TYPE        PIC X(02).
067400             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
067500             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
067600                                                  '15' '17'
067700                                                  '22'.
067800             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
067900             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
068000             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
068100             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
068200             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
068300             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
068400             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
068500             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
068600             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
068700             88  P-N-EACH                   VALUE '21' '22'.
068800             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
068900             88  P-N-NHCMQ-II-SNF           VALUE '32'.
069000             88  P-N-NHCMQ-III-SNF          VALUE '33'.
069100             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
069200         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
069300             88  P-N-NEW-ENGLAND            VALUE  1.
069400             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
069500             88  P-N-SOUTH-ATLANTIC         VALUE  3.
069600             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
069700             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
069800             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
069900             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
070000             88  P-N-MOUNTAIN               VALUE  8.
070100             88  P-N-PACIFIC                VALUE  9.
070200         05  P-NEW-CURRENT-DIV   REDEFINES
070300                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
070400             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
070500         05  P-NEW-MSA-DATA.
070600             10  P-NEW-CHG-CODE-INDEX       PIC X.
070700             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
070800             10  P-NEW-GEO-LOC-MSA9   REDEFINES
070900                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
071000             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
071100             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
071200             10  P-NEW-STAND-AMT-LOC-MSA9
071300       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
071400                 15  P-NEW-RURAL-1ST.
071500                     20  P-NEW-STAND-RURAL  PIC XX.
071600                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
071700                 15  P-NEW-RURAL-2ND        PIC XX.
071800         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
071900                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
072000                 88  P-NEW-SCH-YR82       VALUE   '82'.
072100                 88  P-NEW-SCH-YR87       VALUE   '87'.
072200         05  P-NEW-LUGAR                    PIC X.
072300         05  P-NEW-TEMP-RELIEF-IND          PIC X.
072400         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
072500         05  P-NEW-STATE-CODE               PIC 9(02).
072600         05  P-NEW-STATE-CODE-X REDEFINES
072700             P-NEW-STATE-CODE               PIC X(02).
072800         05  FILLER                         PIC X(03).
072900     02  PROV-NEWREC-HOLD2.
073000         05  P-NEW-VARIABLES.
073100             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
073200             10  P-NEW-COLA              PIC  9(01)V9(03).
073300             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
073400             10  P-NEW-BED-SIZE          PIC  9(05).
073500             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
073600             10  P-NEW-CMI               PIC  9(01)V9(04).
073700             10  P-NEW-SSI-RATIO         PIC  V9(04).
073800             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
073900             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
074000             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
074100             10  P-NEW-DSH-PERCENT       PIC  V9(04).
074200             10  P-NEW-FYE-DATE          PIC  X(08).
074300         05  P-NEW-CBSA-DATA.
074400             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
074500             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
074600             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
074700             10  P-NEW-CBSA-GEO-RURAL REDEFINES
074800                 P-NEW-CBSA-GEO-LOC.
074900                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
075000                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
075100                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
075200
075300             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
075400             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
075500             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
075600     02  PROV-NEWREC-HOLD3.
075700         05  P-NEW-PASS-AMT-DATA.
075800             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
075900             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
076000             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
076100             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
076200         05  P-NEW-CAPI-DATA.
076300             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
076400             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
076500             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
076600             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
076700             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
076800             15  P-NEW-CAPI-NEW-HOSP       PIC X.
076900             15  P-NEW-CAPI-IME            PIC 9V9999.
077000             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
077100         05  P-HVBP-HRR-DATA.
077200             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
077300             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
077400             15  P-HOSP-READMISSION-REDU    PIC X.
077500             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
077600         05  P-MODEL1-BUNDLE-DATA.
077700             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
077800             15  P-HAC-REDUC-IND            PIC X.
077900             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
078000             15  P-EHR-REDUC-IND            PIC X.
078100             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
078200         05  P-NEW-COUNTY-CODE              PIC 9(05).
078300         05  P-NEW-COUNTY-CODE-X REDEFINES
078400             P-NEW-COUNTY-CODE              PIC X(05).
078500         05  FILLER                         PIC X(47).
078600
078700*****************************************************************
078800 01  WAGE-NEW-CBSA-INDEX-RECORD.
078900     05  W-CBSA                        PIC X(5).
079000     05  W-CBSA-SIZE                   PIC X.
079100         88  LARGE-URBAN       VALUE 'L'.
079200         88  OTHER-URBAN       VALUE 'O'.
079300         88  ALL-RURAL         VALUE 'R'.
079400     05  W-CBSA-EFF-DATE               PIC X(8).
079500     05  FILLER                        PIC X.
079600     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
079700     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
079800
079900*******************************************************
080000*    HOLD VARIABLES POPULATED IN PPCAL___***          *
080100*******************************************************
080200 COPY PPHOLDAR.
080300
080400******************************************************************
080500 PROCEDURE DIVISION  USING BILL-NEW-DATA
080600                           PPS-DATA
080700                           PRICER-OPT-VERS-SW
080800                           PPS-ADDITIONAL-VARIABLES
080900                           PROV-NEW-HOLD
081000                           WAGE-NEW-CBSA-INDEX-RECORD
081100                           PPHOLDAR-HOLD-AREA.
081200
081300***************************************************************
081400*    PROCESSING:                                              *
081500*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE
081600*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
081700*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
081800*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
081900*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
082000*           GOBACK.                                           *
082100*        D. ASSEMBLE PRICING COMPONENTS.                      *
082200*        E. CALCULATE THE PRICE.                              *
082300***************************************************************
082400     INITIALIZE WK-HLDDRG-DATA
082500                WK-HLDDRG-DATA2
082600                WK-HLD-MID-DATA
082700                WK-NEW-TECH-VARIABLES.
082800
082900     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
083000     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
083100     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
083200     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
083300     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
083400     MOVE 'N' TO TEMP-RELIEF-FLAG.
083500     MOVE 'N' TO OUTLIER-RECON-FLAG.
083600     MOVE ZEROES TO WK-HAC-AMOUNT.
083700     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
083800     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
083900     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
084000     MOVE ZEROES TO PPS-ISLET-ISOL-PAY-ADD-ON.
084100
084200     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
084300
084400     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
084500     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
084600     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
084700     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
084800     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
084900     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
085000     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
085100     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
085200     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
085300                            PPS-ADDITIONAL-PAY-INFO-DATA.
085400     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
085500                            PPS-ADDITIONAL-PAY-INFO-DATA2.
085600
085700     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
085800     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
085900     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
086000     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
086100                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
086200     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
086300     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
086400                               PPS-MODEL1-BUNDLE-DISPRCNT.
086500
086600     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
086700     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
086800     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
086900*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
087000     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
087100     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
087200     MOVE 0     TO    PPS-FLX7-PAYMENT.
087300
087400     IF (PPS-RTC = '00' OR '03' OR '10' OR
087500                   '12' OR '14')
087600        MOVE 'Y' TO OUTLIER-RECON-FLAG
087700        MOVE PPS-DATA TO HLD-PPS-DATA
087800        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
087900        MOVE HLD-PPS-DATA TO PPS-DATA.
088000
088100     IF  PPS-RTC < 50
088200         IF  P-NEW-WAIVER-STATE
088300             MOVE 53 TO PPS-RTC
088400             MOVE ALL '0' TO PPS-OPER-HSP-PART
088500                             PPS-OPER-FSP-PART
088600                             PPS-OPER-OUTLIER-PART
088700                             PPS-OUTLIER-DAYS
088800                             PPS-REG-DAYS-USED
088900                             PPS-LTR-DAYS-USED
089000                             PPS-TOTAL-PAYMENT
089100                             WK-HAC-TOTAL-PAYMENT
089200                             PPS-OPER-DSH-ADJ
089300                             PPS-OPER-IME-ADJ
089400                             H-DSCHG-FRCTN
089500                             H-DRG-WT-FRCTN
089600                             HOLD-ADDITIONAL-VARIABLES
089700                             HOLD-CAPITAL-VARIABLES
089800                             HOLD-CAPITAL2-VARIABLES
089900                             HOLD-OTHER-VARIABLES
090000                             HOLD-PC-OTH-VARIABLES
090100                             H-ADDITIONAL-PAY-INFO-DATA
090200                             H-ADDITIONAL-PAY-INFO-DATA2.
090210
090220     DISPLAY "FINAL WAGE INDEX: " H-WAGE-INDEX.
090230
090300     GOBACK.
090400
090500 0200-MAINLINE-CONTROL.
090600
090700     MOVE 'N' TO HMO-TAG.
090800
090900     IF PPS-PC-HMO-FLAG = 'Y' OR
091000               HMO-FLAG = 'Y'
091100        MOVE 'Y' TO HMO-TAG.
091200
091300     MOVE ALL '0' TO PPS-DATA
091400                     H-OPER-DSH-SCH
091500                     H-OPER-DSH-RRC
091600                     HOLD-PPS-COMPONENTS
091700                     HOLD-PPS-COMPONENTS
091800                     HOLD-ADDITIONAL-VARIABLES
091900                     HOLD-CAPITAL-VARIABLES
092000                     HOLD-CAPITAL2-VARIABLES
092100                     HOLD-OTHER-VARIABLES
092200                     HOLD-PC-OTH-VARIABLES
092300                     H-ADDITIONAL-PAY-INFO-DATA
092400                     H-ADDITIONAL-PAY-INFO-DATA2
092500                     H-EHR-SUBSAV-QUANT
092600                     H-EHR-SUBSAV-LV
092700                     H-EHR-SUBSAV-QUANT-INCLV
092800                     H-EHR-RESTORE-FULL-QUANT
092900                     H-OPER-BILL-STDZ-COSTS
093000                     H-CAPI-BILL-STDZ-COSTS
093100                     H-OPER-STDZ-COST-OUTLIER
093200                     H-CAPI-STDZ-COST-OUTLIER
093300                     H-OPER-STDZ-DOLLAR-THRESHOLD
093400                     H-CAPI-STDZ-DOLLAR-THRESHOLD
093500                     WK-LOW-VOL-ADDON
093600                     WK-HAC-AMOUNT
093700                     WK-HAC-TOTAL-PAYMENT.
093800
093900     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
094000        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
094100
094200     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
094300        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
094400
094500     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
094600        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
094700
094800     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
094900        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
095000
095100     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
095200        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
095300
095400     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
095500        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
095600
095700     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
095800        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
095900
096000     PERFORM 1000-EDIT-THE-BILL-INFO.
096100
096200     IF  PPS-RTC = 00
096300         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
096400
096500     IF  PPS-RTC = 00
096600         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
096700
096800     IF OUTLIER-RECON-FLAG = 'Y'
096900        MOVE 'N' TO OUTLIER-RECON-FLAG
097000        GO TO 0200-EXIT.
097100
097200     IF PPS-RTC = 00
097300        IF H-PERDIEM-DAYS = H-ALOS OR
097400           H-PERDIEM-DAYS > H-ALOS
097500           MOVE 14 TO PPS-RTC.
097600
097700     IF PPS-RTC = 02
097800        IF H-PERDIEM-DAYS = H-ALOS OR
097900           H-PERDIEM-DAYS > H-ALOS
098000           MOVE 16 TO PPS-RTC.
098100
098200 0200-EXIT.   EXIT.
098300
098400 1000-EDIT-THE-BILL-INFO.
098500
098600     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
098700     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
098800
098900**   IF  PPS-RTC = 00
099000*        IF  P-NEW-WAIVER-STATE
099100*            MOVE 53 TO PPS-RTC.
099200
099300     IF  PPS-RTC = 00
099400         IF   HLDDRG-VALID = 'I'
099500             MOVE 54 TO PPS-RTC.
099600
099700     IF  PPS-RTC = 00
099800            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
099900                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
100000                MOVE 55 TO PPS-RTC.
100100
100200     IF  PPS-RTC = 00
100300         IF P-NEW-TERMINATION-DATE > 00000000
100400            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
100500                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
100600                  MOVE 55 TO PPS-RTC.
100700
100800     IF  PPS-RTC = 00
100900         IF  B-LOS NOT NUMERIC
101000             MOVE 56 TO PPS-RTC
101100         ELSE
101200         IF  B-LOS = 0
101300             IF B-REVIEW-CODE NOT = 00 AND
101400                              NOT = 03 AND
101500                              NOT = 06 AND
101600                              NOT = 07 AND
101700                              NOT = 09 AND
101800                              NOT = 11
101900             MOVE 56 TO PPS-RTC.
102000
102100     IF  PPS-RTC = 00
102200         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
102300             MOVE 61 TO PPS-RTC
102400         ELSE
102500             MOVE B-LTR-DAYS TO H-LTR-DAYS.
102600
102700     IF  PPS-RTC = 00
102800         IF  B-COVERED-DAYS NOT NUMERIC
102900             MOVE 62 TO PPS-RTC
103000         ELSE
103100         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
103200             MOVE 62 TO PPS-RTC
103300         ELSE
103400             MOVE B-COVERED-DAYS TO H-COV-DAYS.
103500
103600     IF  PPS-RTC = 00
103700         IF  H-LTR-DAYS  > H-COV-DAYS
103800             MOVE 62 TO PPS-RTC
103900         ELSE
104000             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
104100
104200     IF  PPS-RTC = 00
104300         IF  NOT VALID-REVIEW-CODE
104400             MOVE 57 TO PPS-RTC.
104500
104600     IF  PPS-RTC = 00
104700         IF  B-CHARGES-CLAIMED NOT NUMERIC
104800             MOVE 58 TO PPS-RTC.
104900
105000     IF PPS-RTC = 00
105100           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
105200                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
105300                                            NOT = 'C'
105400                 MOVE 65 TO PPS-RTC.
105500
105600***  MDH PROVISION ENDS 9/30/2018
105700***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
105800
105900     IF PPS-RTC = 00 AND
106000        B-DISCHARGE-DATE > 20180930 AND
106100        P-N-INVALID-PROV-TYPES
106200                 MOVE 52 TO PPS-RTC.
106300
106400 2000-ASSEMBLE-PPS-VARIABLES.
106500***  GET THE PROVIDER SPECIFIC VARIABLES.
106600
106700     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
106800     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
106900
107000     IF (P-NEW-STATE = 02 OR 12)
107100        MOVE P-NEW-COLA TO H-OPER-COLA
107200     ELSE
107300        MOVE 1.000 TO H-OPER-COLA.
107400
107500***************************************************************
107600***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
107700
107800     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
107900
108000     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
108100
108200     MOVE P-NEW-STATE            TO MES-PPS-STATE.
108300
108400*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
108500** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
108600** FOR FY 2011
108700***************************************************************
108800*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
108900*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
109000***************************************************************
109100***  GET THE WAGE-INDEX
109200
109300     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
109400     MOVE P-NEW-STATE            TO MES-PPS-STATE.
109500
109600***************************************************************
109700* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
109800* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
109900***************************************************************
110000
110100     PERFORM 2050-RATES-TB THRU 2050-EXIT.
110200
110300     IF P-NEW-GEO-LOC-MSA9 >= 9400 AND
110400        P-NEW-GEO-LOC-MSA9 <= 9900
110500        PERFORM 2100-MIDNIGHT-FACTORS THRU 2100-EXIT
110600     ELSE
110700        MOVE 1 TO HLD-MID-ADJ-FACT
110800        GO TO 2000-EXIT.
110900
111000 2000-EXIT.  EXIT.
111100
111200 2050-RATES-TB.
111300     MOVE 1 TO R2
111400     MOVE 1 TO R4.
111500
111600     IF LARGE-URBAN
111700         MOVE 1 TO R3
111800     ELSE
111900         MOVE 2 TO R3.
112000
112100     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
112200        (P-EHR-REDUC-IND = ' ')           AND
112300        (H-WAGE-INDEX > 01.0000))
112400        PERFORM 2300-GET-LAB-NONLAB-TB1-RATES
112500           THRU 2300-GET-LAB-NONLAB-TB1-EXIT
112600             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
112700
112800     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
112900        (P-EHR-REDUC-IND = ' ')               AND
113000         (H-WAGE-INDEX > 01.0000))
113100        PERFORM 2300-GET-LAB-NONLAB-TB2-RATES
113200           THRU 2300-GET-LAB-NONLAB-TB2-EXIT
113300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
113400
113500     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
113600        (P-EHR-REDUC-IND = ' ')            AND
113700         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
113800        PERFORM 2300-GET-LAB-NONLAB-TB3-RATES
113900           THRU 2300-GET-LAB-NONLAB-TB3-EXIT
114000             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
114100
114200     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
114300        (P-EHR-REDUC-IND = ' ')               AND
114400         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
114500        PERFORM 2300-GET-LAB-NONLAB-TB4-RATES
114600           THRU 2300-GET-LAB-NONLAB-TB4-EXIT
114700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
114800
114900     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
115000        (P-EHR-REDUC-IND = 'Y')           AND
115100        (H-WAGE-INDEX > 01.0000))
115200        PERFORM 2300-GET-LAB-NONLAB-TB5-RATES
115300           THRU 2300-GET-LAB-NONLAB-TB5-EXIT
115400             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
115500
115600     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
115700        (P-EHR-REDUC-IND = 'Y')               AND
115800         (H-WAGE-INDEX > 01.0000))
115900        PERFORM 2300-GET-LAB-NONLAB-TB6-RATES
116000           THRU 2300-GET-LAB-NONLAB-TB6-EXIT
116100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
116200
116300     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
116400        (P-EHR-REDUC-IND = 'Y')            AND
116500         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
116600        PERFORM 2300-GET-LAB-NONLAB-TB7-RATES
116700           THRU 2300-GET-LAB-NONLAB-TB7-EXIT
116800             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
116900
117000     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
117100        (P-EHR-REDUC-IND = 'Y')               AND
117200         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
117300        PERFORM 2300-GET-LAB-NONLAB-TB8-RATES
117400           THRU 2300-GET-LAB-NONLAB-TB8-EXIT
117500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
117600
117700***************************************************************
117800* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
117900***************************************************************
118000
118100     MOVE 0.00  TO H-OPER-HSP-PCT.
118200     MOVE 1.00  TO H-OPER-FSP-PCT.
118300
118400***************************************************************
118500*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
118600***************************************************************
118700
118800      MOVE 1.00 TO H-NAT-PCT.
118900      MOVE 0.00 TO H-REG-PCT.
119000
119100     IF  P-N-SCH-REBASED-FY90 OR
119200         P-N-EACH OR
119300         P-N-MDH-REBASED-FY90
119400         MOVE 1.00 TO H-OPER-HSP-PCT.
119500
119600 2050-EXIT.   EXIT.
119700
119800***************************************************************
119900*  APPLY THE TWO MIDNIGHT POLICY ADJUSTMENT FACTORS           *
120000***************************************************************
120100 2100-MIDNIGHT-FACTORS.
120200
120300     INITIALIZE HLD-MID-ADJ-FACT.
120400
120500     SET MID-IDX TO 1.
120600
120700     SEARCH MID-TAB VARYING MID-IDX
120800     WHEN WK-MID-MSAX(MID-IDX) = P-NEW-GEO-LOC-MSA9
120900       MOVE MID-DATA-TAB(MID-IDX) TO HLD-MID-DATA.
121000
121100 2100-EXIT.   EXIT.
121200
121300***************************************************************
121400* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
121500* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
121600***************************************************************
121700 2300-GET-LAB-NONLAB-TB1-RATES.
121800
121900     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
122000         MOVE TB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
122100         MOVE TB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
122200         MOVE TB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
122300         MOVE TB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
122400
122500 2300-GET-LAB-NONLAB-TB1-EXIT.   EXIT.
122600
122700 2300-GET-LAB-NONLAB-TB2-RATES.
122800
122900     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
123000         MOVE TB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
123100         MOVE TB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
123200         MOVE TB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
123300         MOVE TB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
123400
123500 2300-GET-LAB-NONLAB-TB2-EXIT.   EXIT.
123600
123700 2300-GET-LAB-NONLAB-TB3-RATES.
123800
123900     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
124000         MOVE TB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
124100         MOVE TB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
124200         MOVE TB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
124300         MOVE TB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
124400
124500 2300-GET-LAB-NONLAB-TB3-EXIT.   EXIT.
124600
124700 2300-GET-LAB-NONLAB-TB4-RATES.
124800
124900     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
125000         MOVE TB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
125100         MOVE TB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
125200         MOVE TB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
125300         MOVE TB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
125400
125500 2300-GET-LAB-NONLAB-TB4-EXIT.   EXIT.
125600
125700 2300-GET-LAB-NONLAB-TB5-RATES.
125800
125900     IF  B-DISCHARGE-DATE NOT < TB1-RATE-EFF-DATE (R1)
126000         MOVE TB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
126100         MOVE TB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
126200         MOVE TB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
126300         MOVE TB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
126400
126500 2300-GET-LAB-NONLAB-TB5-EXIT.   EXIT.
126600
126700 2300-GET-LAB-NONLAB-TB6-RATES.
126800
126900     IF  B-DISCHARGE-DATE NOT < TB2-RATE-EFF-DATE (R1)
127000         MOVE TB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
127100         MOVE TB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
127200         MOVE TB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
127300         MOVE TB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
127400
127500 2300-GET-LAB-NONLAB-TB6-EXIT.   EXIT.
127600
127700 2300-GET-LAB-NONLAB-TB7-RATES.
127800
127900     IF  B-DISCHARGE-DATE NOT < TB3-RATE-EFF-DATE (R1)
128000         MOVE TB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
128100         MOVE TB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
128200         MOVE TB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
128300         MOVE TB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
128400
128500 2300-GET-LAB-NONLAB-TB7-EXIT.   EXIT.
128600
128700 2300-GET-LAB-NONLAB-TB8-RATES.
128800
128900     IF  B-DISCHARGE-DATE NOT < TB4-RATE-EFF-DATE (R1)
129000         MOVE TB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
129100         MOVE TB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
129200         MOVE TB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
129300         MOVE TB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
129400
129500 2300-GET-LAB-NONLAB-TB8-EXIT.   EXIT.
129600
129700***************************************************************
129800* OBTAIN THE APPLICABLE DRG WEIGHTS                           *
129900***************************************************************
130000 2600-GET-DRG-WEIGHT.
130100
130200     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
130300     SET DRG-IDX TO 1
130400     SEARCH DRG-TAB VARYING DRG-IDX
130500         AT END
130600           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
130700           MOVE 'I' TO  HLDDRG-VALID
130800           MOVE 0 TO HLDDRG-WEIGHT
130900           MOVE 54 TO PPS-RTC
131000           GO TO 2600-EXIT
131100       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
131200         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
131300
131400
131500     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
131600     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
131700     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
131800                                  H-DRG-WT.
131900     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
132000                                  H-ALOS.
132100     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
132200     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
132300                                  H-ARITH-ALOS.
132400     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
132500     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
132600     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
132700     MOVE  'V'                 TO HLDDRG-VALID.
132800     MOVE ZEROES               TO H-DAYS-CUTOFF.
132900
133000 2600-EXIT.   EXIT.
133100
133200*
133300 3000-CALC-PAYMENT.
133400***************************************************************
133500
133600     PERFORM 3100-CALC-STAY-UTILIZATION.
133700     PERFORM 3300-CALC-OPER-FSP-AMT.
133800     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
133900
134000***********************************************************
134100***  OPERATING IME CALCULATION
134200
134300     COMPUTE H-OPER-IME-TEACH ROUNDED =
134400            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
134500
134600***********************************************************
134700
134800     MOVE 00                 TO  PPS-RTC.
134900     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
135000     MOVE H-ALOS             TO  PPS-AVG-LOS.
135100     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
135200
135300     MOVE B-LOS TO H-PERDIEM-DAYS.
135400     IF H-PERDIEM-DAYS < 1
135500         MOVE 1 TO H-PERDIEM-DAYS.
135600     ADD 1 TO H-PERDIEM-DAYS.
135700
135800     MOVE 1 TO H-DSCHG-FRCTN.
135900
136000     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
136100
136200     IF (PAY-PERDIEM-DAYS  OR
136300         PAY-XFER-NO-COST) OR
136400        (PAY-XFER-SPEC-DRG AND
136500         D-DRG-POSTACUTE-PERDIEM)
136600       IF H-ALOS > 0
136700         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
136800         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
136900         IF H-DSCHG-FRCTN > 1
137000              MOVE 1 TO H-DSCHG-FRCTN
137100              MOVE 1 TO H-TRANSFER-ADJ
137200         ELSE
137300              COMPUTE H-DRG-WT-FRCTN ROUNDED =
137400                  H-TRANSFER-ADJ * H-DRG-WT
137500         END-IF
137600        END-IF
137700     END-IF.
137800
137900
138000     IF (PAY-XFER-SPEC-DRG AND
138100         D-DRG-POSTACUTE-50-50) AND
138200         H-ALOS > 0
138300         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
138400         COMPUTE H-DSCHG-FRCTN  ROUNDED =
138500                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
138600         IF H-DSCHG-FRCTN > 1
138700              MOVE 1 TO H-DSCHG-FRCTN
138800              MOVE 1 TO H-TRANSFER-ADJ
138900         ELSE
139000              COMPUTE H-DRG-WT-FRCTN ROUNDED =
139100            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
139200
139300
139400***********************************************************
139500***  CAPITAL DSH CALCULATION
139600
139700     MOVE 0 TO H-CAPI-DSH.
139800
139900     IF P-NEW-BED-SIZE NOT NUMERIC
140000         MOVE 0 TO P-NEW-BED-SIZE.
140100
140200     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
140300         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
140400                  (.2025 * (P-NEW-SSI-RATIO
140500                          + P-NEW-MEDICAID-RATIO)) - 1.
140600
140700***********************************************************
140800***  CAPITAL IME TEACH CALCULATION
140900
141000     MOVE 0 TO H-WK-CAPI-IME-TEACH.
141100
141200     IF P-NEW-CAPI-IME NUMERIC
141300        IF P-NEW-CAPI-IME > 1.5000
141400           MOVE 1.5000 TO P-NEW-CAPI-IME.
141500
141600*****YEARCHANGE 2009.5 ****************************************
141700***
141800***  PER POLICY, WE REMOVED THE .5 MULTIPLER
141900***
142000***********************************************************
142100     IF P-NEW-CAPI-IME NUMERIC
142200        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
142300         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
142400
142500*****YEARCHANGE 2009.5 ****************************************
142600***********************************************************
142700     MOVE 0.00 TO H-DAYOUT-PCT.
142800     MOVE 0.80 TO H-CSTOUT-PCT.
142900
143000*****************************************************************
143100**
143200** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
143300**
143400*****************************************************************
143500
143600     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
143700             MOVE 0.90 TO H-CSTOUT-PCT.
143800
143900*****YEARCHANGE 2018.0 *******************************************
144000* NATIONAL PERCENTAGE                                            *
144100******************************************************************
144200
144300       MOVE 0.6830 TO H-LABOR-PCT.
144400       MOVE 0.3170 TO H-NONLABOR-PCT.
144500
144600     IF (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000)
144700       MOVE 0.6200 TO H-LABOR-PCT
144800       MOVE 0.3800 TO H-NONLABOR-PCT.
144900
145000     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
145100             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
145200     ELSE
145300             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
145400
145500     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
145600             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
145700     ELSE
145800             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
145900
146000***********************************************************
146100*****YEARCHANGE 2010.0 ************************************
146200***  CAPITAL PAYMENT METHOD B - YEARCHNG
146300***  CAPITAL PAYMENT METHOD B
146400
146500     IF W-CBSA-SIZE = 'L'
146600        MOVE 1.00 TO H-CAPI-LARG-URBAN
146700     ELSE
146800        MOVE 1.00 TO H-CAPI-LARG-URBAN.
146900
147000     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
147100
147200*****YEARCHANGE 2018.0 ************************************
147300
147400     COMPUTE H-FEDERAL-RATE ROUNDED =
147500                              (0453.95 * H-CAPI-GAF).
147600
147700*****YEARCHANGE 2015.1 ************************************
147800
147900     COMPUTE H-CAPI-COLA ROUNDED =
148000                     (.3152 * (H-OPER-COLA - 1) + 1).
148100
148200     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
148300
148400***********************************************************
148500* CAPITAL FSP CALCULATION                                 *
148600***********************************************************
148700
148800     COMPUTE H-CAPI-FSP-PART ROUNDED =
148900                               H-DRG-WT       *
149000                               H-CAPI-FED-RATE *
149100                               H-CAPI-COLA *
149200                               H-CAPI-LARG-URBAN *
149300                               HLD-MID-ADJ-FACT.
149400
149500***********************************************************
149600***  CAPITAL PAYMENT METHOD A
149700***  CAPITAL PAYMENT METHOD A
149800
149900     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
150000        MOVE 1.00 TO H-CAPI-SCH
150100     ELSE
150200        MOVE 0.85 TO H-CAPI-SCH.
150300
150400***********************************************************
150500***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
150600***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
150700
150800     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
150900                    (P-NEW-CAPI-OLD-HARM-RATE *
151000                    H-CAPI-SCH).
151100
151200***********************************************************
151300        IF PAY-PERDIEM-DAYS
151400            IF  H-PERDIEM-DAYS < H-ALOS
151500                IF  NOT (B-DRG = 789)
151600                    PERFORM 3500-CALC-PERDIEM-AMT
151700                    MOVE 03 TO PPS-RTC.
151800
151900        IF PAY-XFER-SPEC-DRG
152000            IF  H-PERDIEM-DAYS < H-ALOS
152100                IF  NOT (B-DRG = 789)
152200                    PERFORM 3550-CALC-PERDIEM-AMT.
152300
152400        IF  PAY-XFER-NO-COST
152500            MOVE 00 TO PPS-RTC
152600            IF H-PERDIEM-DAYS < H-ALOS
152700               IF  NOT (B-DRG = 789)
152800                   PERFORM 3500-CALC-PERDIEM-AMT
152900                   MOVE 06 TO PPS-RTC.
153000
153100     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
153200
153300     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
153400
153500     IF PPS-RTC = 65 OR 67 OR 68
153600               GO TO 3000-CONTINUE.
153700
153800     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
153900
154000     IF PPS-RTC = 65 OR 67 OR 68
154100               GO TO 3000-CONTINUE.
154200
154300     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
154400
154500     IF PPS-RTC = 65 OR 67 OR 68
154600               GO TO 3000-CONTINUE.
154700
154800     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
154900
155000     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
155100
155200     IF PPS-RTC = 65 OR 67 OR 68
155300               GO TO 3000-CONTINUE.
155400
155500        IF PAY-XFER-SPEC-DRG
155600            IF  H-PERDIEM-DAYS < H-ALOS
155700                IF  NOT (B-DRG = 789)
155800                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
155900
156000
156100        IF  PAY-PERDIEM-DAYS
156200            IF  H-OPER-OUTCST-PART > 0
156300                MOVE H-OPER-OUTCST-PART TO
156400                     H-OPER-OUTLIER-PART
156500                MOVE 05 TO PPS-RTC
156600            ELSE
156700            IF  PPS-RTC NOT = 03
156800                MOVE 00 TO PPS-RTC
156900                MOVE 0  TO H-OPER-OUTLIER-PART.
157000
157100        IF  PAY-PERDIEM-DAYS
157200            IF  H-CAPI-OUTCST-PART > 0
157300                MOVE H-CAPI-OUTCST-PART TO
157400                     H-CAPI-OUTLIER-PART
157500                MOVE 05 TO PPS-RTC
157600            ELSE
157700            IF  PPS-RTC NOT = 03
157800                MOVE 0  TO H-CAPI-OUTLIER-PART.
157900
158000
158100     IF P-N-SCH-REBASED-FY90 OR
158200        P-N-EACH OR
158300        P-N-MDH-REBASED-FY90
158400         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
158500
158600 3000-CONTINUE.
158700
158800***********************************************************
158900***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
159000***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
159100
159200     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
159300
159400***********************************************************
159500
159600     IF  PPS-RTC = 67
159700         MOVE H-OPER-DOLLAR-THRESHOLD TO
159800              WK-H-OPER-DOLLAR-THRESHOLD.
159900
160000     IF  PPS-RTC < 50
160100         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
160200
160300     IF  PPS-RTC < 50
160400         NEXT SENTENCE
160500     ELSE
160600         MOVE ALL '0' TO PPS-OPER-HSP-PART
160700                         PPS-OPER-FSP-PART
160800                         PPS-OPER-OUTLIER-PART
160900                         PPS-OUTLIER-DAYS
161000                         PPS-REG-DAYS-USED
161100                         PPS-LTR-DAYS-USED
161200                         PPS-TOTAL-PAYMENT
161300                         WK-HAC-TOTAL-PAYMENT
161400                         PPS-OPER-DSH-ADJ
161500                         PPS-OPER-IME-ADJ
161600                         H-DSCHG-FRCTN
161700                         H-DRG-WT-FRCTN
161800                         HOLD-ADDITIONAL-VARIABLES
161900                         HOLD-CAPITAL-VARIABLES
162000                         HOLD-CAPITAL2-VARIABLES
162100                         HOLD-OTHER-VARIABLES
162200                         HOLD-PC-OTH-VARIABLES
162300                        H-ADDITIONAL-PAY-INFO-DATA
162400                        H-ADDITIONAL-PAY-INFO-DATA2.
162500
162600     IF  PPS-RTC = 67
162700         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
162800                 H-OPER-DOLLAR-THRESHOLD.
162900
163000 3000-EXIT.  EXIT.
163100
163200 3100-CALC-STAY-UTILIZATION.
163300
163400     MOVE 0 TO PPS-REG-DAYS-USED.
163500     MOVE 0 TO PPS-LTR-DAYS-USED.
163600
163700     IF H-REG-DAYS > 0
163800        IF H-REG-DAYS > B-LOS
163900           MOVE B-LOS TO PPS-REG-DAYS-USED
164000        ELSE
164100           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
164200     ELSE
164300        IF H-LTR-DAYS > B-LOS
164400           MOVE B-LOS TO PPS-LTR-DAYS-USED
164500        ELSE
164600           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
164700
164800
164900
165000 3300-CALC-OPER-FSP-AMT.
165100***********************************************************
165200*  OPERATING FSP CALCULATION                              *
165300***********************************************************
165400
165500     COMPUTE H-OPER-FSP-PART ROUNDED =
165600       (H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
165700        H-NAT-NONLABOR * H-OPER-COLA) * H-DRG-WT *
165800        HLD-MID-ADJ-FACT)
165900           ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
166000
166100 3500-CALC-PERDIEM-AMT.
166200***********************************************************
166300***  REVIEW CODE = 03 OR 06
166400***  OPERATING PERDIEM-AMT CALCULATION
166500***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
166600
166700        COMPUTE H-OPER-FSP-PART ROUNDED =
166800        H-OPER-FSP-PART * H-TRANSFER-ADJ
166900        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
167000
167100***********************************************************
167200***********************************************************
167300***  REVIEW CODE = 03 OR 06
167400***  CAPITAL   PERDIEM-AMT CALCULATION
167500***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
167600
167700        COMPUTE H-CAPI-FSP-PART ROUNDED =
167800        H-CAPI-FSP-PART * H-TRANSFER-ADJ
167900        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
168000
168100***********************************************************
168200***  REVIEW CODE = 03 OR 06
168300***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
168400***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
168500
168600        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
168700        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
168800        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
168900
169000 3550-CALC-PERDIEM-AMT.
169100***********************************************************
169200***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
169300***  OPERATING PERDIEM-AMT CALCULATION
169400***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
169500
169600     IF (D-DRG-POSTACUTE-50-50)
169700        MOVE 10 TO PPS-RTC
169800        COMPUTE H-OPER-FSP-PART ROUNDED =
169900        H-OPER-FSP-PART * H-DSCHG-FRCTN
170000        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
170100
170200     IF (D-DRG-POSTACUTE-PERDIEM)
170300        MOVE 12 TO PPS-RTC
170400        COMPUTE H-OPER-FSP-PART ROUNDED =
170500        H-OPER-FSP-PART *  H-TRANSFER-ADJ
170600        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
170700
170800***********************************************************
170900***  CAPITAL PERDIEM-AMT CALCULATION
171000***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
171100
171200     IF (D-DRG-POSTACUTE-50-50)
171300        MOVE 10 TO PPS-RTC
171400        COMPUTE H-CAPI-FSP-PART ROUNDED =
171500        H-CAPI-FSP-PART * H-DSCHG-FRCTN
171600        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
171700
171800     IF (D-DRG-POSTACUTE-PERDIEM)
171900        MOVE 12 TO PPS-RTC
172000        COMPUTE H-CAPI-FSP-PART ROUNDED =
172100        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
172200        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
172300
172400***********************************************************
172500***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
172600***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
172700
172800     IF (D-DRG-POSTACUTE-50-50)
172900        MOVE 10 TO PPS-RTC
173000        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
173100        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
173200        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
173300
173400     IF (D-DRG-POSTACUTE-PERDIEM)
173500        MOVE 12 TO PPS-RTC
173600        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
173700        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
173800        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
173900
174000 3560-CHECK-RTN-CODE.
174100
174200     IF (D-DRG-POSTACUTE-50-50)
174300        MOVE 10 TO PPS-RTC.
174400     IF (D-DRG-POSTACUTE-PERDIEM)
174500        MOVE 12 TO PPS-RTC.
174600
174700 3560-EXIT.    EXIT.
174800
174900***********************************************************
175000 3600-CALC-OUTLIER.
175100***********************************************************
175200*---------------------------------------------------------*
175300* (YEARCHANGE 2016.0)
175400* COST OUTLIER OPERATING AND CAPITAL CALCULATION
175500*---------------------------------------------------------*
175600
175700     IF OUTLIER-RECON-FLAG = 'Y'
175800        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
175900               (H-OPER-CSTCHG-RATIO + .2).
176000
176100     IF H-CAPI-CSTCHG-RATIO > 0 OR
176200        H-OPER-CSTCHG-RATIO > 0
176300        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
176400                H-OPER-CSTCHG-RATIO /
176500               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
176600        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
176700                H-CAPI-CSTCHG-RATIO /
176800               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
176900     ELSE
177000        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
177100                  H-CAPI-SHARE-DOLL-THRESHOLD.
177200
177300*-----------------------------*
177400* (YEARCHANGE 2018.0)         *
177500* OUTLIER THRESHOLD AMOUNTS   *
177600*-----------------------------*
177700
177800     MOVE 26537.00 TO H-CST-THRESH.
177900
178000     IF (B-REVIEW-CODE = '03') AND
178100         H-PERDIEM-DAYS < H-ALOS
178200        COMPUTE H-CST-THRESH ROUNDED =
178300                      (H-CST-THRESH * H-TRANSFER-ADJ)
178400                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
178500
178600     IF ((B-REVIEW-CODE = '09') AND
178700         (H-PERDIEM-DAYS < H-ALOS))
178800         IF (D-DRG-POSTACUTE-PERDIEM)
178900            COMPUTE H-CST-THRESH ROUNDED =
179000                      (H-CST-THRESH * H-TRANSFER-ADJ)
179100                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
179200
179300     IF ((B-REVIEW-CODE = '09') AND
179400         (H-PERDIEM-DAYS < H-ALOS))
179500         IF (D-DRG-POSTACUTE-50-50)
179600           COMPUTE H-CST-THRESH ROUNDED =
179700          H-CST-THRESH * H-DSCHG-FRCTN
179800                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
179900
180000     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
180100        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
180200         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
180300          H-OPER-SHARE-DOLL-THRESHOLD.
180400
180500***********************************************************
180600
180700     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
180800          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
180900          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
181000
181100***********************************************************
181200******NOW INCLUDES UNCOMPENSATED CARE**********************
181300
181400     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
181500         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
181600                       +
181700           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
181800                       +
181900             H-OPER-DOLLAR-THRESHOLD
182000                       +
182100                WK-UNCOMP-CARE-AMOUNT
182200                       +
182300                 H-NEW-TECH-PAY-ADD-ON.
182400
182500     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
182600      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
182700                       +
182800             H-CAPI-DOLLAR-THRESHOLD.
182900
183000     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
183100         MOVE 0 TO H-CAPI-COST-OUTLIER.
183200
183300
183400***********************************************************
183500***  OPERATING COST CALCULATION
183600
183700     COMPUTE H-OPER-BILL-COSTS ROUNDED =
183800         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
183900         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
184000
184100
184200     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
184300         COMPUTE H-OPER-OUTCST-PART ROUNDED =
184400         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
184500                         H-OPER-COST-OUTLIER).
184600
184700     IF PAY-WITHOUT-COST OR
184800        PAY-XFER-NO-COST OR
184900        PAY-XFER-SPEC-DRG-NO-COST
185000         MOVE 0 TO H-OPER-OUTCST-PART.
185100
185200***********************************************************
185300***  CAPITAL COST CALCULATION
185400
185500     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
185600             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
185700         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
185800
185900     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
186000         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
186100         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
186200                         H-CAPI-COST-OUTLIER).
186300
186400***********************************************************
186500***  'A' NOT VALID FY 2015 ON
186600
186700*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
186800*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
186900*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
187000
187100     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
187200        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
187300               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
187400
187500     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
187600        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
187700        MOVE 0 TO H-CAPI-OUTCST-PART
187800                  H-OPER-OUTCST-PART.
187900
188000     IF PAY-WITHOUT-COST OR
188100        PAY-XFER-NO-COST OR
188200        PAY-XFER-SPEC-DRG-NO-COST
188300         MOVE 0 TO H-CAPI-OUTCST-PART.
188400
188500***********************************************************
188600***  DETERMINES THE BILL TO BE COST  OUTLIER
188700
188800     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
188900         MOVE 0 TO H-CAPI-OUTDAY-PART
189000                   H-CAPI-OUTCST-PART.
189100
189200     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
189300                 MOVE H-OPER-OUTCST-PART TO
189400                      H-OPER-OUTLIER-PART
189500                 MOVE H-CAPI-OUTCST-PART TO
189600                      H-CAPI-OUTLIER-PART
189700                 MOVE 02 TO PPS-RTC.
189800
189900     IF OUTLIER-RECON-FLAG = 'Y'
190000        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
190100           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
190200           GO TO 3600-EXIT
190300        ELSE
190400           GO TO 3600-EXIT
190500     ELSE
190600        NEXT SENTENCE.
190700
190800
190900***********************************************************
191000***  DETERMINES IF COST OUTLIER
191100***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
191200***         RETURN CODE OF 02
191300
191400     MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
191500
191600     IF PPS-RTC = 02
191700       IF H-CAPI-CSTCHG-RATIO > 0 OR
191800          H-OPER-CSTCHG-RATIO > 0
191900             COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
192000                     (H-CAPI-COST-OUTLIER  +
192100                      H-OPER-COST-OUTLIER)
192200                             /
192300                    (H-CAPI-CSTCHG-RATIO  +
192400                     H-OPER-CSTCHG-RATIO)
192500             ON SIZE ERROR MOVE 0 TO H-OPER-DOLLAR-THRESHOLD
192600       ELSE MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
192700
192800***********************************************************
192900***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
193000***         RETURN CODE OF 67
193100
193200     IF PPS-RTC = 02
193300         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
193400            PPS-PC-COT-FLAG = 'Y'
193500             MOVE 67 TO PPS-RTC.
193600***********************************************************
193700
193800***********************************************************
193900***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
194000***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
194100***********************************************************
194200*
194300***********************************************************
194400***  'A' NOT VALID FY 2015 ON
194500*
194600*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
194700*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
194800*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
194900*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
195000
195100     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
195200        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
195300                H-CAPI-OUTLIER-PART.
195400
195500     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
195600        H-CAPI-PAYCDE-PCT1 > 0
195700        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
195800                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
195900         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
196000     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
196100
196200 3600-EXIT.   EXIT.
196300
196400***********************************************************
196500 3450-CALC-ADDITIONAL-HSP.
196600***********************************************************
196700*---------------------------------------------------------*
196800* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
196900* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
197000* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
197100*---------------------------------------------------------*
197200***  GET THE RBN UPDATING FACTOR
197300
197400*****YEARCHANGE 2013.0 ****************************************
197500     MOVE 0.998431 TO H-BUDG-NUTR130.
197600
197700*****YEARCHANGE 2014.0 ****************************************
197800     MOVE 0.997989 TO H-BUDG-NUTR140.
197900
198000*****YEARCHANGE 2015.1 ****************************************
198100     MOVE 0.998761 TO H-BUDG-NUTR150.
198200
198300*****YEARCHANGE 2016.0 ****************************************
198400     IF B-DISCHARGE-DATE < 20160101
198500        MOVE 0.998405 TO H-BUDG-NUTR160
198600     ELSE
198700        MOVE 0.998404 TO H-BUDG-NUTR160
198800     END-IF.
198900
199000*****YEARCHANGE 2017.0 ****************************************
199100     MOVE 0.999078 TO H-BUDG-NUTR170.
199200
199300*****YEARCHANGE 2018.0 ****************************************
199400     MOVE 0.997439 TO H-BUDG-NUTR180.
199500
199600
199700***  GET THE MARKET BASKET UPDATE FACTOR
199800*****YEARCHANGE 2013.0 ****************************************
199900        MOVE 1.0180 TO H-UPDATE-130.
200000
200100*****YEARCHANGE 2014.0 ****************************************
200200        MOVE 1.0170 TO H-UPDATE-140.
200300
200400*****YEARCHANGE 2015.0 ****************************************
200500        MOVE 1.02200 TO H-UPDATE-150.
200600
200700*****YEARCHANGE 2016.0 ****************************************
200800        MOVE 1.01700 TO H-UPDATE-160.
200900
201000*****YEARCHANGE 2017.0 ****************************************
201100        MOVE 1.01650 TO H-UPDATE-170.
201200
201300
201400*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
201500*****YEARCHANGE 2018.0 ****************************************
201600     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
201700        P-EHR-REDUC-IND = ' '
201800        MOVE 1.01350 TO H-UPDATE-180.
201900
202000*****YEARCHANGE 2018.0 ****************************************
202100     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
202200        P-EHR-REDUC-IND = 'Y'
202300        MOVE 0.99325 TO H-UPDATE-180.
202400
202500*****YEARCHANGE 2018.0 ****************************************
202600     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
202700        P-EHR-REDUC-IND = ' '
202800        MOVE 1.00675 TO H-UPDATE-180.
202900
203000*****YEARCHANGE 2018.0 ****************************************
203100     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
203200        P-EHR-REDUC-IND = 'Y'
203300        MOVE 0.98650 TO H-UPDATE-180.
203400
203500
203600********YEARCHANGE 2018.0 *************************************
203700*** CASE MIX ADJUSTMENT AS OF FY 2015
203800*** SHORT STAY FIX OF 1.006 FOR FY 2017 (1.0 FOR FY 2018)
203900
204000     MOVE 0.9480 TO H-CASE-MIX-ADJ.
204100     MOVE 1.0000 TO H-SHORT-STAY-ADJ.
204200
204300     COMPUTE H-UPDATE-FACTOR ROUNDED =
204400                       (H-UPDATE-130 *
204500                        H-UPDATE-140 *
204600                        H-UPDATE-150 *
204700                        H-UPDATE-160 *
204800                        H-UPDATE-170 *
204900                        H-UPDATE-180 *
205000                        H-BUDG-NUTR130 *
205100                        H-BUDG-NUTR140 *
205200                        H-BUDG-NUTR150 *
205300                        H-BUDG-NUTR160 *
205400                        H-BUDG-NUTR170 *
205500                        H-BUDG-NUTR180 *
205600                        HLD-MID-ADJ-FACT *
205700                        H-CASE-MIX-ADJ * H-SHORT-STAY-ADJ).
205800
205900     COMPUTE H-HSP-RATE ROUNDED =
206000         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT.
206100***************************************************************
206200*
206300*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
206400*       COMPUTE H-HSP-RATE ROUNDED =
206500*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
206600*    ELSE
206700*       COMPUTE H-HSP-RATE ROUNDED =
206800*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
206900*
207000***************************************************************
207100********YEARCHANGE 2011.0 *************************************
207200***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
207300***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
207400***     IN THE HSP COMPARRISON
207500
207600********YEARCHANGE 2014.0 *XXXXXX******************************
207700*      THE HSP BUCKET FOR SCH                      ************
207800*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
207900***************************************************************
208000     COMPUTE H-FSP-RATE ROUNDED =
208100        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
208200         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN *
208300         HLD-MID-ADJ-FACT) *
208400             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
208500                               +
208600                         H-OPER-OUTLIER-PART
208700                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
208800
208900****************************************************************
209000****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
209100*****        CHOICE
209200
209300     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
209400           COMPUTE H-OPER-HSP-PART ROUNDED =
209500             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
209600                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
209700     ELSE
209800         MOVE 0 TO H-OPER-HSP-PART.
209900
210000***************************************************************
210100***  YEARCHANGE TURNING MDH BACK ON ***************************
210200***************************************************************
210300***  GET THE MDH REBASE
210400
210500     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
210600         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
210700           COMPUTE H-OPER-HSP-PART ROUNDED =
210800         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
210900                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
211000
211100***************************************************************
211200***  TRANSITIONAL PAYMENT FOR FORMER MDHS                     *
211300***************************************************************
211400
211500***  HSP PAYMENT FOR CLAIMS BETWEEN 10/01/2016 - 09/30/2017
211600
211700*    IF  B-FORMER-MDH-PROVIDERS       AND
211800*       (B-DISCHARGE-DATE > 20160930  AND
211900*        B-DISCHARGE-DATE < 20171001)
212000*      IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
212100*        COMPUTE H-OPER-HSP-PART ROUNDED =
212200*          ((H-HSP-RATE - (H-FSP-RATE +
212300*              WK-UNCOMP-CARE-AMOUNT))* 0.75)*(1 / 3)
212400*            ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
212500*      END-IF
212600*    END-IF.
212700
212800 3450-EXIT.   EXIT.
212900
213000***********************************************************
213100 3800-CALC-TOT-AMT.
213200***********************************************************
213300***  CALCULATE TOTALS FOR CAPITAL
213400
213500     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
213600
213700***********************************************************
213800***  'A' NOT VALID FY 2015 ON
213900*
214000*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
214100*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
214200*       MOVE 0.00 TO H-CAPI-HSP-PCT.
214300
214400     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
214500        MOVE 0    TO H-CAPI-OLD-HARMLESS
214600        MOVE 1.00 TO H-CAPI-FSP-PCT
214700        MOVE 0.00 TO H-CAPI-HSP-PCT.
214800
214900     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
215000        MOVE 0    TO H-CAPI-OLD-HARMLESS
215100        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
215200        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
215300
215400     COMPUTE H-CAPI-HSP ROUNDED =
215500         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
215600
215700     COMPUTE H-CAPI-FSP ROUNDED =
215800         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
215900
216000     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
216100
216200     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
216300
216400     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
216500             H-CAPI-FSP
216600              * H-CAPI-DSH.
216700
216800     COMPUTE H-CAPI-IME-ADJ ROUNDED =
216900          H-CAPI-FSP *
217000                 H-WK-CAPI-IME-TEACH.
217100
217200     COMPUTE H-CAPI-OUTLIER ROUNDED =
217300             1.00 * H-CAPI-OUTLIER-PART.
217400
217500     COMPUTE H-CAPI2-B-FSP ROUNDED =
217600             1.00 * H-CAPI2-B-FSP-PART.
217700
217800     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
217900             1.00 * H-CAPI2-B-OUTLIER-PART.
218000***********************************************************
218100***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
218200***        THIS ZEROES OUT ALL CAPITAL DATA
218300
218400     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
218500        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
218600***********************************************************
218700
218800***********************************************************
218900***  CALCULATE FINAL TOTALS FOR OPERATING
219000
219100     IF (H-CAPI-OUTLIER > 0 AND
219200         PPS-OPER-OUTLIER-PART = 0)
219300            COMPUTE PPS-OPER-OUTLIER-PART =
219400                    PPS-OPER-OUTLIER-PART + .01.
219500
219600***********************************************************
219700*LOW VOLUME CALCULATIONS
219800***********************************************************
219900*---------------------------------------------------------*
220000* (YEARCHANGE 2016.0)
220100* LOW VOLUME PAYMENT ADD-ON PERCENT
220200*---------------------------------------------------------*
220300
220400     MOVE ZERO TO PPS-OPER-DSH-ADJ.
220500************************************************
220600* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
220700* EMPERICAL DSH
220800************************************************
220900     IF  H-OPER-DSH NUMERIC
221000         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
221100                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
221200
221300     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
221400                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
221500
221600
221700     COMPUTE PPS-OPER-FSP-PART ROUNDED =
221800                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
221900
222000     COMPUTE PPS-OPER-HSP-PART ROUNDED =
222100                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
222200
222300     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
222400                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
222500
222600     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
222700                                H-NEW-TECH-PAY-ADD-ON.
222800
222900     COMPUTE PPS-ISLET-ISOL-PAY-ADD-ON ROUNDED =
223000                                H-NEW-TECH-ADDON-ISLET.
223100
223200     IF P-NEW-TEMP-RELIEF-IND = 'Y'
223300        AND P-LV-ADJ-FACTOR > 0.00
223400        AND P-LV-ADJ-FACTOR <= 0.25
223500     COMPUTE WK-LOW-VOL-ADDON ROUNDED =
223600       (PPS-OPER-HSP-PART +
223700        PPS-OPER-FSP-PART +
223800        PPS-OPER-IME-ADJ +
223900        PPS-OPER-DSH-ADJ +
224000        PPS-OPER-OUTLIER-PART +
224100        H-CAPI-FSP +
224200        H-CAPI-IME-ADJ +
224300        H-CAPI-DSH-ADJ +
224400        H-CAPI-OUTLIER +
224500        WK-UNCOMP-CARE-AMOUNT +
224600        PPS-NEW-TECH-PAY-ADD-ON) * P-LV-ADJ-FACTOR
224700     ELSE
224800     COMPUTE WK-LOW-VOL-ADDON ROUNDED = 0.
224900
225000     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
225100     IF HMO-TAG  = 'Y'
225200        PERFORM 3850-HMO-IME-ADJ.
225300
225400***********************************************************
225500***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
225600
225700     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
225800             H-CAPI-FSP + H-CAPI-IME-ADJ +
225900             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
226000
226100         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
226200         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
226300
226400***********************************************************
226500* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
226600***********************************************************
226700*---------------------------------------------------------*
226800* (YEARCHANGE 2016.0)
226900* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
227000*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
227100*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
227200*   + BASED ON INDICATOR FROM THE PPS FILE
227300*   + NOT VALID IN PUERTO RICO
227400*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
227500*---------------------------------------------------------*
227600
227700     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
227800        PPS-OPER-HSP-PART +
227900        PPS-OPER-FSP-PART +
228000        PPS-OPER-IME-ADJ +
228100        PPS-OPER-DSH-ADJ +
228200        PPS-OPER-OUTLIER-PART +
228300        H-CAPI-TOTAL-PAY +
228400        WK-UNCOMP-CARE-AMOUNT +
228500        PPS-NEW-TECH-PAY-ADD-ON +
228600        WK-LOW-VOL-ADDON +
228700        H-READMIS-ADJUST-AMT +
228800        H-VAL-BASED-PURCH-ADJUST-AMT.
228900
229000     MOVE ZERO TO WK-HAC-AMOUNT.
229100
229200     IF P-PR-NEW-STATE AND
229300        P-HAC-REDUC-IND = 'Y'
229400           MOVE 53 TO PPS-RTC
229500           GO TO 3800-EXIT.
229600
229700     IF  P-HAC-REDUC-IND = 'Y'
229800         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
229900                   WK-HAC-TOTAL-PAYMENT * -0.01
230000     ELSE
230100         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
230200
230300***********************************************************
230400***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
230500************************************************
230600     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
230700                 WK-HAC-TOTAL-PAYMENT
230800                           +
230900                 H-WK-PASS-AMT-PLUS-MISC
231000                           +
231100                 H-BUNDLE-ADJUST-AMT
231200                           +
231300                 WK-HAC-AMOUNT
231400                           +
231500                 H-NEW-TECH-ADDON-ISLET.
231600
231700     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
231800              H-VAL-BASED-PURCH-PARTIPNT.
231900
232000     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
232100              H-VAL-BASED-PURCH-ADJUST.
232200
232300     MOVE     P-HOSP-READMISSION-REDU    TO
232400              H-HOSP-READMISSION-REDU.
232500
232600     MOVE     P-HOSP-HRR-ADJUSTMT        TO
232700              H-HOSP-HRR-ADJUSTMT.
232800
232900 3800-EXIT.   EXIT.
233000
233100 3850-HMO-IME-ADJ.
233200***********************************************************
233300***  HMO CALC FOR PASS-THRU ADDON
233400
233500     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
233600          (P-NEW-PASS-AMT-PLUS-MISC -
233700          (P-NEW-PASS-AMT-ORGAN-ACQ +
233800           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
233900
234000***********************************************************
234100***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
234200
234300     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
234400                   PPS-OPER-IME-ADJ * .0.
234500
234600***********************************************************
234700
234800
234900 3900A-CALC-OPER-DSH.
235000
235100***  OPERATING DSH CALCULATION
235200
235300      MOVE 0.0000 TO H-OPER-DSH.
235400
235500      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
235600                                     + P-NEW-MEDICAID-RATIO).
235700
235800***********************************************************
235900**1**    0-99 BEDS
236000***  NOT TO EXCEED 12%
236100
236200      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
236300                               AND H-WK-OPER-DSH > .1499
236400                               AND H-WK-OPER-DSH < .2020
236500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
236600                                      * .65 + .025
236700        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
236800
236900      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
237000                               AND H-WK-OPER-DSH > .2019
237100        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
237200                                      * .825 + .0588
237300        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
237400
237500***********************************************************
237600**2**   100 + BEDS
237700***  NO CAP >> CAN EXCEED 12%
237800
237900      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
238000                               AND H-WK-OPER-DSH > .1499
238100                               AND H-WK-OPER-DSH < .2020
238200        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
238300                                      * .65 + .025.
238400
238500      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
238600                               AND H-WK-OPER-DSH > .2019
238700        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
238800                                      * .825 + .0588.
238900
239000***********************************************************
239100**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
239200***  NOT TO EXCEED 12%
239300
239400      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
239500                               AND H-WK-OPER-DSH > .1499
239600                               AND H-WK-OPER-DSH < .2020
239700        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
239800                                 * .65 + .025
239900        IF H-OPER-DSH > .1200
240000              MOVE .1200 TO H-OPER-DSH.
240100
240200      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
240300                               AND H-WK-OPER-DSH > .2019
240400        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
240500                                 * .825 + .0588
240600        IF H-OPER-DSH > .1200
240700                 MOVE .1200 TO H-OPER-DSH.
240800***********************************************************
240900**4**   OTHER RURAL HOSPITALS 500 BEDS +
241000***  NO CAP >> CAN EXCEED 12%
241100
241200      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
241300                               AND H-WK-OPER-DSH > .1499
241400                               AND H-WK-OPER-DSH < .2020
241500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
241600                                 * .65 + .025.
241700
241800      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
241900                               AND H-WK-OPER-DSH > .2019
242000        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
242100                                 * .825 + .0588.
242200
242300***********************************************************
242400**7**   RURAL HOSPITALS SCH
242500***  NOT TO EXCEED 12%
242600
242700      IF W-CBSA-SIZE = 'R'
242800         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
242900                               AND H-WK-OPER-DSH > .1499
243000                               AND H-WK-OPER-DSH < .2020
243100         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
243200                                 * .65 + .025
243300        IF H-OPER-DSH > .1200
243400                 MOVE .1200 TO H-OPER-DSH.
243500
243600      IF W-CBSA-SIZE = 'R'
243700         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
243800                               AND H-WK-OPER-DSH > .2019
243900         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
244000                                 * .825 + .0588
244100        IF H-OPER-DSH > .1200
244200                 MOVE .1200 TO H-OPER-DSH.
244300
244400***********************************************************
244500**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
244600***  RRC OVERRIDES SCH CAP
244700***  NO CAP >> CAN EXCEED 12%
244800
244900         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
245000                                   '17' OR '22')
245100                               AND H-WK-OPER-DSH > .1499
245200                               AND H-WK-OPER-DSH < .2020
245300         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
245400                                 * .65 + .025.
245500
245600         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
245700                                   '17' OR '22')
245800                               AND H-WK-OPER-DSH > .2019
245900         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
246000                                 * .825 + .0588.
246100
246200      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
246300
246400 3900A-EXIT.   EXIT.
246500
246600 4000-CALC-TECH-ADDON.
246700
246800***********************************************************
246900***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
247000
247100     COMPUTE PPS-OPER-HSP-PART ROUNDED =
247200         H-OPER-HSP-PCT * H-OPER-HSP-PART.
247300
247400     COMPUTE PPS-OPER-FSP-PART ROUNDED =
247500         H-OPER-FSP-PCT * H-OPER-FSP-PART.
247600
247700     MOVE ZERO TO PPS-OPER-DSH-ADJ.
247800
247900     IF  H-OPER-DSH NUMERIC
248000             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
248100             (PPS-OPER-FSP-PART
248200              * H-OPER-DSH) * .25.
248300
248400     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
248500             PPS-OPER-FSP-PART *
248600             H-OPER-IME-TEACH.
248700
248800     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
248900             PPS-OPER-FSP-PART +
249000             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
249100             WK-UNCOMP-CARE-AMOUNT.
249200
249300***********************************************************
249400***********************************************************
249500* PUT NEW CHECK HERE IF H-NEW-TECH ZERO PERFORM
249600
249700*    IF   B-DIAG-AUTOLITT-DIAG AND
249800*         B-DRG-AUTOLITT-DRG
249900*       PERFORM 4500-AUTOLIT-TECH-ADD-ON THRU 4500-EXIT.
250000
250100***********************************************************
250200*  DIFICID DISCONTINUED FOR FY 2015
250300*    IF   B-NDC-DIFICID-NDC
250400*      PERFORM 4600-DIFICID-TECH-ADD-ON THRU 4600-EXIT.
250500
250600***********************************************************
250700* NEW TECH ADD ON CODE *
250800***********************************************************
250900     MOVE 1 TO IDX-TECH.
251000     INITIALIZE H-TECH-ADDON-ISLET-CNTR.
251100
251200     PERFORM 4010-NEW-TECH-ADD-ON THRU 4010-EXIT
251300      VARYING IDX-TECH FROM 1 BY 1 UNTIL IDX-TECH > 25.
251400
251500*    IF PROC-ARGUS-FLAG = 'Y'
251600*      PERFORM 4810-ARGUS-TECH-ADD-ON THRU 4810-EXIT
251700*    ELSE
251800*      MOVE ZEROES TO H-NEW-TECH-ADDON-ARGUS.
251900
252000*    IF PROC-BLINATU-FLAG = 'Y'
252100*      PERFORM 4900-BLINATU-TECH-ADD-ON THRU 4900-EXIT
252200*    ELSE
252300*      MOVE ZEROES TO H-NEW-TECH-ADDON-BLINATU.
252400
252500*    IF PROC-CARDIO-FLAG = 'Y'
252600*      PERFORM 5010-CARDIO-MEMES-ADD-ON THRU 5010-EXIT
252700*    ELSE
252800*      MOVE ZEROES TO H-NEW-TECH-ADDON-CARDIO.
252900
253000     IF PROC-DEFITELIO-FLAG = 'Y'
253100       PERFORM 5040-DEFITELIO-TECH-ADD-ON THRU 5040-EXIT
253200     ELSE
253300       MOVE ZEROES TO H-NEW-TECH-ADDON-DEFITELIO.
253400
253500     IF PROC-EDWARDS-FLAG = 'Y'
253600       PERFORM 5110-EDWARDS-TECH-ADD-ON THRU 5110-EXIT
253700     ELSE
253800       MOVE ZEROES TO H-NEW-TECH-ADDON-EDWARDS.
253900
254000     IF PROC-GORE-FLAG = 'Y'
254100       PERFORM 5050-GORE-TECH-ADD-ON THRU 5050-EXIT
254200     ELSE
254300       MOVE ZEROES TO H-NEW-TECH-ADDON-GORE.
254400
254500     IF PROC-IDARUCIZ-FLAG = 'Y'
254600       PERFORM 5060-IDARUCIZ-TECH-ADD-ON THRU 5060-EXIT
254700     ELSE
254800       MOVE ZEROES TO H-NEW-TECH-ADDON-IDARUCIZ.
254900
255000     IF DIAG-ISLET-FLAG = 'Y' AND PROC-ISLET-FLAG = 'Y'
255100       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
255200     ELSE
255300       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
255400
255500*    IF DIAG-KCENTRA-FLAG = 'Y' AND PROC-KCENTRA-FLAG = 'Y'
255600*      MOVE ZEROES TO H-NEW-TECH-ADDON-KCENTRA
255700*    ELSE
255800*      PERFORM 4820-KCENTRA-TECH-ADD-ON THRU 4820-EXIT.
255900
256000*    IF PROC-LUTONIX-FLAG = 'Y'
256100*      PERFORM 4910-LUTONIX-TECH-ADD-ON THRU 4910-EXIT
256200*    ELSE
256300*      MOVE ZEROES TO H-NEW-TECH-ADDON-LUTONIX.
256400
256500*    IF PROC-MAGEC-FLAG = 'Y'
256600*      PERFORM 5070-MAGEC-TECH-ADD-ON THRU 5070-EXIT
256700*    ELSE
256800*      MOVE ZEROES TO H-NEW-TECH-ADDON-MAGEC.
256900
257000*    IF PROC-MITRACLP-FLAG = 'Y'
257100*      PERFORM 5020-MITRA-CLIP-ADD-ON THRU 5020-EXIT
257200*    ELSE
257300*      MOVE ZEROES TO H-NEW-TECH-ADDON-MITRACLP.
257400
257500*    IF PROC-RNSSYS1-FLAG = 'Y' AND PROC-RNSSYS2-FLAG = 'Y'
257600*      PERFORM 5030-RNS-SYS-ADD-ON THRU 5030-EXIT
257700*    ELSE
257800*      MOVE ZEROES TO H-NEW-TECH-ADDON-RNSSYS.
257900
258000     IF PROC-STELARA-FLAG = 'Y'
258100       PERFORM 5090-STELARA-TECH-ADD-ON THRU 5090-EXIT
258200     ELSE
258300       MOVE ZEROES TO H-NEW-TECH-ADDON-STELARA.
258400
258500     IF DIAG-VISTOGARD-FLAG = 'Y' AND PROC-VISTOGARD-FLAG = 'Y'
258600       PERFORM 5080-VISTOGARD-TECH-ADD-ON THRU 5080-EXIT
258700     ELSE
258800       MOVE ZEROES TO H-NEW-TECH-ADDON-VISTOGARD.
258900
259000*    IF PROC-VORAXAZE-FLAG = 'Y'
259100*      PERFORM 4800-VORAXAZE-TECH-ADD-ON THRU 4800-EXIT
259200*    ELSE
259300*      MOVE ZEROES TO H-NEW-TECH-ADDON-VORAXAZE.
259400
259500*    IF PROC-ZENITH-FLAG = 'Y'
259600*      PERFORM 4700-ZENITH-TECH-ADD-ON THRU 4700-EXIT
259700*    ELSE
259800*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZENITH.
259900
260000*    IF PROC-ZILVER-FLAG = 'Y'
260100*      PERFORM 4830-ZILVER-TECH-ADD-ON THRU 4830-EXIT
260200*    ELSE
260300*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZILVER.
260400
260500     IF PROC-ZINPLAVA-FLAG = 'Y'
260600       PERFORM 5100-ZINPLAVA-TECH-ADD-ON THRU 5100-EXIT
260700     ELSE
260800       MOVE ZEROES TO H-NEW-TECH-ADDON-ZINPLAVA.
260900
261000***********************************************************
261100*  ALL NEW TECH MUST BE CALCULATED BEFORE
261200*  5500-CAP-CALC-TECH-ADD-ON
261300***********************************************************
261400     PERFORM 5500-CAP-CALC-TECH-ADD-ON THRU 5500-EXIT.
261500
261600     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
261700             H-OPER-FSP-PART +
261800             H-NEW-TECH-PAY-ADD-ON.
261900
262000*
262100 4000-EXIT.    EXIT.
262200***********************************************************
262300
262400 4010-NEW-TECH-ADD-ON.
262500
262600     MOVE B-PROCEDURE-CODE(IDX-TECH) TO WK-PROC-NEW-TECH.
262700     MOVE B-DIAGNOSIS-CODE(IDX-TECH) TO WK-DIAG-NEW-TECH.
262800
262900*    IF PROC-ARGUS
263000*      MOVE 'Y' TO PROC-ARGUS-FLAG.
263100
263200*    IF PROC-BLINATU
263300*      MOVE 'Y' TO PROC-BLINATU-FLAG.
263400
263500*    IF PROC-CARDIO
263600*      MOVE 'Y' TO PROC-CARDIO-FLAG.
263700
263800     IF PROC-DEFITELIO
263900       MOVE 'Y' TO PROC-DEFITELIO-FLAG.
264000
264100     IF PROC-EDWARDS
264200       MOVE 'Y' TO PROC-EDWARDS-FLAG.
264300
264400     IF PROC-GORE
264500       MOVE 'Y' TO PROC-GORE-FLAG.
264600
264700     IF PROC-IDARUCIZ
264800       MOVE 'Y' TO PROC-IDARUCIZ-FLAG.
264900
265000     IF PROC-ISLET
265100       MOVE 'Y' TO PROC-ISLET-FLAG
265200       COMPUTE H-TECH-ADDON-ISLET-CNTR =
265300          H-TECH-ADDON-ISLET-CNTR + 1.
265400
265500*    IF PROC-KCENTRA
265600*      MOVE 'Y' TO PROC-KCENTRA-FLAG.
265700
265800*    IF PROC-LUTONIX
265900*      MOVE 'Y' TO PROC-LUTONIX-FLAG.
266000
266100*    IF PROC-MAGEC
266200*      MOVE 'Y' TO PROC-MAGEC-FLAG.
266300
266400*    IF PROC-MITRACLP
266500*      MOVE 'Y' TO PROC-MITRACLP-FLAG.
266600
266700*    IF PROC-RNSSYS1
266800*      MOVE 'Y' TO PROC-RNSSYS1-FLAG.
266900
267000*    IF PROC-RNSSYS2
267100*      MOVE 'Y' TO PROC-RNSSYS2-FLAG.
267200
267300     IF PROC-STELARA
267400       MOVE 'Y' TO PROC-STELARA-FLAG.
267500
267600     IF PROC-VISTOGARD
267700       MOVE 'Y' TO PROC-VISTOGARD-FLAG.
267800
267900*    IF PROC-VORAXAZE
268000*      MOVE 'Y' TO PROC-VORAXAZE-FLAG.
268100
268200*    IF PROC-ZENITH
268300*      MOVE 'Y' TO PROC-ZENITH-FLAG.
268400
268500*    IF PROC-ZILVER
268600*      MOVE 'Y' TO PROC-ZILVER-FLAG.
268700
268800     IF PROC-ZINPLAVA
268900       MOVE 'Y' TO PROC-ZINPLAVA-FLAG.
269000
269100     IF DIAG-ISLET
269200       MOVE 'Y' TO DIAG-ISLET-FLAG.
269300
269400*    IF DIAG-KCENTRA
269500*      MOVE 'Y' TO DIAG-KCENTRA-FLAG.
269600
269700     IF DIAG-VISTOGARD
269800       MOVE 'Y' TO DIAG-VISTOGARD-FLAG.
269900
270000 4010-EXIT.   EXIT.
270100
270200***********************************************************
270300* TECHNICAL TRANSPLANTATION OF CELLS                      *
270400***********************************************************
270500 4100-ISLET-ISOLATION-ADD-ON.
270600
270700     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
270800
270900     IF  H-TECH-ADDON-ISLET-CNTR = 1
271000     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
271100           GO TO 4100-EXIT.
271200
271300     IF  H-TECH-ADDON-ISLET-CNTR > 1
271400     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
271500           GO TO 4100-EXIT.
271600
271700 4100-EXIT.    EXIT.
271800
271900***********************************************************
272000* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
272100* DISCHARGE COUNTS.
272200***********************************************************
272300*4400-LOWVOL-CODE-RTN.
272400*
272500*    SET LOWVOL-IDX TO 1.
272600*    SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
272700*        AT END
272800*          MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
272900*          MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
273000*      WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
273100*        MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
273200*                           TO MESWK-LOWVOL-PROV-DISCHG.
273300*
273400*4400-EXIT.   EXIT.
273500
273600*****************************************************************
273700* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR DISCHARGE *
273800* COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER FILE       *
273900*****************************************************************
274000 4410-UNCOMP-CARE-CODE-RTN.
274100
274200*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
274300*
274400*    SET UNCOMP-CARE-IDX TO 1.
274500*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
274600*        AT END
274700*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
274800*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
274900*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
275000*                           TO WK-UNCOMP-CARE-AMOUNT.
275100*
275200        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
275300
275400        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
275500
275600 4410-EXIT.   EXIT.
275700
275800***********************************************************
275900* CASES INVOLVING AUTOLITT                                *
276000***********************************************************
276100*4500-AUTOLIT-TECH-ADD-ON.
276200*
276300*    MOVE 0 TO H-NEW-TECH-ADDON-AUTOLITT
276400*              H-LESSER-AUTOLITT-STOP-1
276500*              H-LESSER-AUTOLITT-STOP-2
276600*              H-CSTMED-AUTOLITT-STOP.
276700*
276800*    IF '1761   ' =  B-PRIN-PROC-CODE     OR
276900*                    B-OTHER-PROC-CODE1   OR
277000*                    B-OTHER-PROC-CODE2   OR
277100*                    B-OTHER-PROC-CODE3   OR
277200*                    B-OTHER-PROC-CODE4   OR
277300*                    B-OTHER-PROC-CODE5   OR
277400*                    B-OTHER-PROC-CODE6   OR
277500*                    B-OTHER-PROC-CODE7   OR
277600*                    B-OTHER-PROC-CODE8   OR
277700*                    B-OTHER-PROC-CODE9   OR
277800*                    B-OTHER-PROC-CODE10  OR
277900*                    B-OTHER-PROC-CODE11  OR
278000*                    B-OTHER-PROC-CODE12  OR
278100*                    B-OTHER-PROC-CODE13  OR
278200*                    B-OTHER-PROC-CODE14  OR
278300*                    B-OTHER-PROC-CODE15  OR
278400*                    B-OTHER-PROC-CODE16  OR
278500*                    B-OTHER-PROC-CODE17  OR
278600*                    B-OTHER-PROC-CODE18  OR
278700*                    B-OTHER-PROC-CODE19  OR
278800*                    B-OTHER-PROC-CODE20  OR
278900*                    B-OTHER-PROC-CODE21  OR
279000*                    B-OTHER-PROC-CODE22  OR
279100*                    B-OTHER-PROC-CODE23  OR
279200*                    B-OTHER-PROC-CODE24
279300*          GO TO 4500-COMPUTE-AUTOLITT
279400*    ELSE
279500*          NEXT SENTENCE.
279600*
279700*          MOVE ZEROES TO H-NEW-TECH-ADDON-AUTOLITT.
279800*          GO TO 4500-ADD-TECH-CASES.
279900*
280000*4500-COMPUTE-AUTOLITT.
280100*
280200*    MOVE  5300.00 TO H-CSTMED-AUTOLITT-STOP.
280300*
280400*    COMPUTE H-LESSER-AUTOLITT-STOP-1 ROUNDED =
280500*                 H-CSTMED-AUTOLITT-STOP.
280600*
280700*    COMPUTE H-LESSER-AUTOLITT-STOP-2 ROUNDED =
280800*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
280900*                    H-BASE-DRG-PAYMENT)) * .5.
281000*
281100*    IF H-LESSER-AUTOLITT-STOP-2 > 0
281200*       IF H-LESSER-AUTOLITT-STOP-1 < H-LESSER-AUTOLITT-STOP-2
281300*        MOVE H-LESSER-AUTOLITT-STOP-1 TO
281400*                               H-NEW-TECH-ADDON-AUTOLITT
281500*       ELSE
281600*        MOVE H-LESSER-AUTOLITT-STOP-2 TO
281700*                               H-NEW-TECH-ADDON-AUTOLITT
281800*    ELSE
281900*       MOVE ZEROES          TO H-NEW-TECH-ADDON-AUTOLITT.
282000*
282100*
282200*4500-ADD-TECH-CASES.
282300*
282400*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
282500*            H-NEW-TECH-PAY-ADD-ON +
282600*            H-NEW-TECH-ADDON-AUTOLITT.
282700*
282800*4500-EXIT.    EXIT.
282900*
283000***********************************************************
283100* CASES INVOLVING DIFICID                                 *
283200***********************************************************
283300*4600-DIFICID-TECH-ADD-ON.
283400*
283500*    MOVE 0 TO H-NEW-TECH-ADDON-DIFICID
283600*              H-LESSER-DIFICID-STOP-1
283700*              H-LESSER-DIFICID-STOP-2
283800*              H-CSTMED-DIFICID-STOP.
283900*
284000*    IF '00845  ' =  B-OTHER-DIAG-CODE1   OR
284100*                    B-OTHER-DIAG-CODE2   OR
284200*                    B-OTHER-DIAG-CODE3   OR
284300*                    B-OTHER-DIAG-CODE4   OR
284400*                    B-OTHER-DIAG-CODE5   OR
284500*                    B-OTHER-DIAG-CODE6   OR
284600*                    B-OTHER-DIAG-CODE7   OR
284700*                    B-OTHER-DIAG-CODE8   OR
284800*                    B-OTHER-DIAG-CODE9   OR
284900*                    B-OTHER-DIAG-CODE10  OR
285000*                    B-OTHER-DIAG-CODE11  OR
285100*                    B-OTHER-DIAG-CODE12  OR
285200*                    B-OTHER-DIAG-CODE13  OR
285300*                    B-OTHER-DIAG-CODE14  OR
285400*                    B-OTHER-DIAG-CODE15  OR
285500*                    B-OTHER-DIAG-CODE16  OR
285600*                    B-OTHER-DIAG-CODE17  OR
285700*                    B-OTHER-DIAG-CODE18  OR
285800*                    B-OTHER-DIAG-CODE19  OR
285900*                    B-OTHER-DIAG-CODE20  OR
286000*                    B-OTHER-DIAG-CODE21  OR
286100*                    B-OTHER-DIAG-CODE22  OR
286200*                    B-OTHER-DIAG-CODE23  OR
286300*                    B-OTHER-DIAG-CODE24  OR
286400*                    B-OTHER-DIAG-CODE25
286500*          GO TO 4600-COMPUTE-DIFICID
286600*    ELSE
286700*          NEXT SENTENCE.
286800*
286900*          MOVE ZEROES TO H-NEW-TECH-ADDON-DIFICID.
287000*          GO TO 4600-ADD-TECH-CASES.
287100*
287200*4600-COMPUTE-DIFICID.
287300*
287400*    MOVE  868.00 TO H-CSTMED-DIFICID-STOP.
287500*
287600*    COMPUTE H-LESSER-DIFICID-STOP-1 ROUNDED =
287700*                 H-CSTMED-DIFICID-STOP.
287800*
287900*    COMPUTE H-LESSER-DIFICID-STOP-2 ROUNDED =
288000*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
288100*                    H-BASE-DRG-PAYMENT)) * .5.
288200*
288300*    IF H-LESSER-DIFICID-STOP-2 > 0
288400*       IF H-LESSER-DIFICID-STOP-1 < H-LESSER-DIFICID-STOP-2
288500*        MOVE H-LESSER-DIFICID-STOP-1 TO
288600*                               H-NEW-TECH-ADDON-DIFICID
288700*       ELSE
288800*        MOVE H-LESSER-DIFICID-STOP-2 TO
288900*                               H-NEW-TECH-ADDON-DIFICID
289000*    ELSE
289100*       MOVE ZEROES          TO H-NEW-TECH-ADDON-DIFICID.
289200*
289300*
289400*
289500*4600-ADD-TECH-CASES.
289600*
289700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
289800*            H-NEW-TECH-PAY-ADD-ON +
289900*            H-NEW-TECH-ADDON-DIFICID.
290000*
290100*4600-EXIT.    EXIT.
290200
290300***********************************************************
290400* CASES INVOLVING ZENITH                                  *
290500***********************************************************
290600*4700-ZENITH-TECH-ADD-ON.
290700*
290800*    MOVE 0 TO H-NEW-TECH-ADDON-ZENITH
290900*              H-LESSER-ZENITH-STOP-1
291000*              H-LESSER-ZENITH-STOP-2
291100*              H-CSTMED-ZENITH-STOP.
291200*
291300*4700-COMPUTE-ZENITH.
291400*
291500*    MOVE  8171.50 TO H-CSTMED-ZENITH-STOP.
291600*
291700*    COMPUTE H-LESSER-ZENITH-STOP-1 ROUNDED =
291800*                 H-CSTMED-ZENITH-STOP.
291900*
292000*    COMPUTE H-LESSER-ZENITH-STOP-2 ROUNDED =
292100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
292200*                    H-BASE-DRG-PAYMENT)) * .5.
292300*
292400*    IF H-LESSER-ZENITH-STOP-2 > 0
292500*       IF H-LESSER-ZENITH-STOP-1 < H-LESSER-ZENITH-STOP-2
292600*        MOVE H-LESSER-ZENITH-STOP-1 TO
292700*                               H-NEW-TECH-ADDON-ZENITH
292800*       ELSE
292900*        MOVE H-LESSER-ZENITH-STOP-2 TO
293000*                               H-NEW-TECH-ADDON-ZENITH
293100*    ELSE
293200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZENITH.
293300*
293400*
293500*4700-ADD-TECH-CASES.
293600*
293700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
293800*            H-NEW-TECH-PAY-ADD-ON +
293900*            H-NEW-TECH-ADDON-ZENITH.
294000*
294100*4700-EXIT.    EXIT.
294200
294300***********************************************************
294400* CASES INVOLVING VORAXAZE                                *
294500***********************************************************
294600*4800-VORAXAZE-TECH-ADD-ON.
294700*
294800*    MOVE 0 TO H-NEW-TECH-ADDON-VORAXAZE
294900*              H-LESSER-VORAXAZE-STOP-1
295000*              H-LESSER-VORAXAZE-STOP-2
295100*              H-CSTMED-VORAXAZE-STOP.
295200*
295300*4800-COMPUTE-VORAXAZE.
295400*
295500*    MOVE  47250.00 TO H-CSTMED-VORAXAZE-STOP.
295600*
295700*    COMPUTE H-LESSER-VORAXAZE-STOP-1 ROUNDED =
295800*                 H-CSTMED-VORAXAZE-STOP.
295900*
296000*    COMPUTE H-LESSER-VORAXAZE-STOP-2 ROUNDED =
296100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
296200*                    H-BASE-DRG-PAYMENT)) * .5.
296300*
296400*    IF H-LESSER-VORAXAZE-STOP-2 > 0
296500*       IF H-LESSER-VORAXAZE-STOP-1 < H-LESSER-VORAXAZE-STOP-2
296600*        MOVE H-LESSER-VORAXAZE-STOP-1 TO
296700*                               H-NEW-TECH-ADDON-VORAXAZE
296800*       ELSE
296900*        MOVE H-LESSER-VORAXAZE-STOP-2 TO
297000*                               H-NEW-TECH-ADDON-VORAXAZE
297100*    ELSE
297200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-VORAXAZE.
297300*
297400*
297500*4800-ADD-TECH-CASES.
297600*
297700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
297800*            H-NEW-TECH-PAY-ADD-ON +
297900*            H-NEW-TECH-ADDON-VORAXAZE.
298000*
298100*4800-EXIT.    EXIT.
298200
298300***********************************************************
298400* CASES INVOLVING ARGUS                                   *
298500***********************************************************
298600*4810-ARGUS-TECH-ADD-ON.
298700*
298800*    MOVE 0 TO H-NEW-TECH-ADDON-ARGUS
298900*              H-LESSER-ARGUS-STOP-1
299000*              H-LESSER-ARGUS-STOP-2
299100*              H-CSTMED-ARGUS-STOP.
299200*
299300*4810-COMPUTE-ARGUS.
299400*
299500*    MOVE  72028.75 TO H-CSTMED-ARGUS-STOP.
299600*
299700*    COMPUTE H-LESSER-ARGUS-STOP-1 ROUNDED =
299800*                 H-CSTMED-ARGUS-STOP.
299900*
300000*    COMPUTE H-LESSER-ARGUS-STOP-2 ROUNDED =
300100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
300200*                    H-BASE-DRG-PAYMENT)) * .5.
300300*
300400*    IF H-LESSER-ARGUS-STOP-2 > 0
300500*       IF H-LESSER-ARGUS-STOP-1 < H-LESSER-ARGUS-STOP-2
300600*        MOVE H-LESSER-ARGUS-STOP-1 TO
300700*                               H-NEW-TECH-ADDON-ARGUS
300800*       ELSE
300900*        MOVE H-LESSER-ARGUS-STOP-2 TO
301000*                               H-NEW-TECH-ADDON-ARGUS
301100*    ELSE
301200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ARGUS.
301300*
301400*
301500*4810-ADD-TECH-CASES.
301600*
301700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
301800*            H-NEW-TECH-PAY-ADD-ON +
301900*            H-NEW-TECH-ADDON-ARGUS.
302000*
302100*4810-EXIT.    EXIT.
302200*
302300***********************************************************
302400* CASES INVOLVING KCENTRA                                 *
302500***********************************************************
302600*4820-KCENTRA-TECH-ADD-ON.
302700*
302800*    MOVE 0 TO H-NEW-TECH-ADDON-KCENTRA
302900*              H-LESSER-KCENTRA-STOP-1
303000*              H-LESSER-KCENTRA-STOP-2
303100*              H-CSTMED-KCENTRA-STOP.
303200*
303300*4820-COMPUTE-KCENTRA.
303400*
303500*    MOVE  01587.50 TO H-CSTMED-KCENTRA-STOP.
303600*
303700*    COMPUTE H-LESSER-KCENTRA-STOP-1 ROUNDED =
303800*                 H-CSTMED-KCENTRA-STOP.
303900*
304000*    COMPUTE H-LESSER-KCENTRA-STOP-2 ROUNDED =
304100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
304200*                    H-BASE-DRG-PAYMENT)) * .5.
304300*
304400*    IF H-LESSER-KCENTRA-STOP-2 > 0
304500*       IF H-LESSER-KCENTRA-STOP-1 < H-LESSER-KCENTRA-STOP-2
304600*        MOVE H-LESSER-KCENTRA-STOP-1 TO
304700*                               H-NEW-TECH-ADDON-KCENTRA
304800*       ELSE
304900*        MOVE H-LESSER-KCENTRA-STOP-2 TO
305000*                               H-NEW-TECH-ADDON-KCENTRA
305100*    ELSE
305200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-KCENTRA.
305300*
305400*
305500*4820-ADD-TECH-CASES.
305600*
305700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
305800*            H-NEW-TECH-PAY-ADD-ON +
305900*            H-NEW-TECH-ADDON-KCENTRA.
306000*
306100*4820-EXIT.    EXIT.
306200
306300***********************************************************
306400* CASES INVOLVING ZILVER                                  *
306500***********************************************************
306600*4830-ZILVER-TECH-ADD-ON.
306700*
306800*    MOVE 0 TO H-NEW-TECH-ADDON-ZILVER
306900*              H-LESSER-ZILVER-STOP-1
307000*              H-LESSER-ZILVER-STOP-2
307100*              H-CSTMED-ZILVER-STOP.
307200*
307300*4830-COMPUTE-ZILVER.
307400*
307500*    MOVE  01705.25 TO H-CSTMED-ZILVER-STOP.
307600*
307700*    COMPUTE H-LESSER-ZILVER-STOP-1 ROUNDED =
307800*                 H-CSTMED-ZILVER-STOP.
307900*
308000*    COMPUTE H-LESSER-ZILVER-STOP-2 ROUNDED =
308100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
308200*                    H-BASE-DRG-PAYMENT)) * .5.
308300*
308400*    IF H-LESSER-ZILVER-STOP-2 > 0
308500*       IF H-LESSER-ZILVER-STOP-1 < H-LESSER-ZILVER-STOP-2
308600*        MOVE H-LESSER-ZILVER-STOP-1 TO
308700*                               H-NEW-TECH-ADDON-ZILVER
308800*       ELSE
308900*        MOVE H-LESSER-ZILVER-STOP-2 TO
309000*                               H-NEW-TECH-ADDON-ZILVER
309100*    ELSE
309200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZILVER.
309300*
309400*
309500*4830-ADD-TECH-CASES.
309600*
309700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
309800*            H-NEW-TECH-PAY-ADD-ON +
309900*            H-NEW-TECH-ADDON-ZILVER.
310000*
310100*4830-EXIT.    EXIT.
310200
310300***********************************************************
310400* CASES INVOLVING BLINATUMOMAB                            *
310500***********************************************************
310600*4900-BLINATU-TECH-ADD-ON.
310700*
310800*    MOVE 0 TO H-NEW-TECH-ADDON-BLINATU
310900*              H-LESSER-BLINATU-STOP-1
311000*              H-LESSER-BLINATU-STOP-2
311100*              H-CSTMED-BLINATU-STOP.
311200*
311300*4900-COMPUTE-BLINATU.
311400*
311500*    MOVE  27017.85 TO H-CSTMED-BLINATU-STOP.
311600*
311700*    COMPUTE H-LESSER-BLINATU-STOP-1 ROUNDED =
311800*                 H-CSTMED-BLINATU-STOP.
311900*
312000*    COMPUTE H-LESSER-BLINATU-STOP-2 ROUNDED =
312100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
312200*                    H-BASE-DRG-PAYMENT)) * .5.
312300*
312400*    IF H-LESSER-BLINATU-STOP-2 > 0
312500*       IF H-LESSER-BLINATU-STOP-1 < H-LESSER-BLINATU-STOP-2
312600*        MOVE H-LESSER-BLINATU-STOP-1 TO
312700*                               H-NEW-TECH-ADDON-BLINATU
312800*       ELSE
312900*        MOVE H-LESSER-BLINATU-STOP-2 TO
313000*                               H-NEW-TECH-ADDON-BLINATU
313100*    ELSE
313200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-BLINATU.
313300*
313400*4900-ADD-TECH-CASES.
313500*
313600*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
313700*            H-NEW-TECH-PAY-ADD-ON +
313800*            H-NEW-TECH-ADDON-BLINATU.
313900*
314000*4900-EXIT.    EXIT.
314100
314200***********************************************************
314300* CASES INVOLVING LUTONIX DRUG COATED BALLOON (DCB)       *
314400* PERCUTANEOUS TRANSLUMINAL ANGIOPLASTY (PTA) AND IN.PACT *
314500* ADMIRAL PACLIAXEL COATED PERCUTANEOUS TRANSLUMINAL      *
314600* ANGIOPLASTY (PTA) BALLOON CATHETER                      *
314700***********************************************************
314800*4910-LUTONIX-TECH-ADD-ON.
314900*
315000*    MOVE 0 TO H-NEW-TECH-ADDON-LUTONIX
315100*              H-LESSER-LUTONIX-STOP-1
315200*              H-LESSER-LUTONIX-STOP-2
315300*              H-CSTMED-LUTONIX-STOP.
315400*
315500*4910-COMPUTE-LUTONIX.
315600*
315700*    MOVE  01035.72 TO H-CSTMED-LUTONIX-STOP.
315800*
315900*    COMPUTE H-LESSER-LUTONIX-STOP-1 ROUNDED =
316000*                 H-CSTMED-LUTONIX-STOP.
316100*
316200*    COMPUTE H-LESSER-LUTONIX-STOP-2 ROUNDED =
316300*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
316400*                    H-BASE-DRG-PAYMENT)) * .5.
316500*
316600*    IF H-LESSER-LUTONIX-STOP-2 > 0
316700*       IF H-LESSER-LUTONIX-STOP-1 < H-LESSER-LUTONIX-STOP-2
316800*        MOVE H-LESSER-LUTONIX-STOP-1 TO
316900*                               H-NEW-TECH-ADDON-LUTONIX
317000*       ELSE
317100*        MOVE H-LESSER-LUTONIX-STOP-2 TO
317200*                               H-NEW-TECH-ADDON-LUTONIX
317300*    ELSE
317400*       MOVE ZEROES          TO H-NEW-TECH-ADDON-LUTONIX.
317500*
317600*4910-ADD-TECH-CASES.
317700*
317800*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
317900*            H-NEW-TECH-PAY-ADD-ON +
318000*            H-NEW-TECH-ADDON-LUTONIX.
318100*
318200*4910-EXIT.    EXIT.
318300
318400**************************************************************
318500* CASES INVOLVING CARDIO MEMES                               *
318600**************************************************************
318700*5010-CARDIO-MEMES-ADD-ON.
318800*
318900*    MOVE 0 TO H-NEW-TECH-ADDON-CARDIO
319000*              H-LESSER-CARDIO-STOP-1
319100*              H-LESSER-CARDIO-STOP-2
319200*              H-CSTMED-CARDIO-STOP.
319300*
319400*5010-COMPUTE-CARDIO.
319500*
319600*    MOVE  08875.00 TO H-CSTMED-CARDIO-STOP.
319700*
319800*    COMPUTE H-LESSER-CARDIO-STOP-1 ROUNDED =
319900*                 H-CSTMED-CARDIO-STOP.
320000*
320100*    COMPUTE H-LESSER-CARDIO-STOP-2 ROUNDED =
320200*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
320300*                    H-BASE-DRG-PAYMENT)) * .5.
320400*
320500*    IF H-LESSER-CARDIO-STOP-2 > 0
320600*       IF H-LESSER-CARDIO-STOP-1 < H-LESSER-CARDIO-STOP-2
320700*        MOVE H-LESSER-CARDIO-STOP-1 TO
320800*                               H-NEW-TECH-ADDON-CARDIO
320900*       ELSE
321000*        MOVE H-LESSER-CARDIO-STOP-2 TO
321100*                               H-NEW-TECH-ADDON-CARDIO
321200*    ELSE
321300*       MOVE ZEROES          TO H-NEW-TECH-ADDON-CARDIO.
321400*
321500*5010-ADD-TECH-CASES.
321600*
321700*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
321800*            H-NEW-TECH-PAY-ADD-ON +
321900*            H-NEW-TECH-ADDON-CARDIO.
322000*
322100*5010-EXIT.    EXIT.
322200
322300***********************************************************
322400* CASES INVOLVING MITRACLIP                               *
322500***********************************************************
322600*5020-MITRA-CLIP-ADD-ON.
322700*
322800*    MOVE 0 TO H-NEW-TECH-ADDON-MITRACLP
322900*              H-LESSER-MITRACLP-STOP-1
323000*              H-LESSER-MITRACLP-STOP-2
323100*              H-CSTMED-MITRACLP-STOP.
323200*
323300*5020-COMPUTE-MITRACLP.
323400*
323500*    MOVE  15000.00 TO H-CSTMED-MITRACLP-STOP.
323600*
323700*    COMPUTE H-LESSER-MITRACLP-STOP-1 ROUNDED =
323800*                 H-CSTMED-MITRACLP-STOP.
323900*
324000*    COMPUTE H-LESSER-MITRACLP-STOP-2 ROUNDED =
324100*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
324200*                    H-BASE-DRG-PAYMENT)) * .5.
324300*
324400*    IF H-LESSER-MITRACLP-STOP-2 > 0
324500*       IF H-LESSER-MITRACLP-STOP-1 < H-LESSER-MITRACLP-STOP-2
324600*        MOVE H-LESSER-MITRACLP-STOP-1 TO
324700*                               H-NEW-TECH-ADDON-MITRACLP
324800*       ELSE
324900*        MOVE H-LESSER-MITRACLP-STOP-2 TO
325000*                               H-NEW-TECH-ADDON-MITRACLP
325100*    ELSE
325200*       MOVE ZEROES          TO H-NEW-TECH-ADDON-MITRACLP.
325300*
325400*5020-ADD-TECH-CASES.
325500*
325600*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
325700*            H-NEW-TECH-PAY-ADD-ON +
325800*            H-NEW-TECH-ADDON-MITRACLP.
325900*
326000*5020-EXIT.    EXIT.
326100
326200***********************************************************
326300* CASES INVOLVING RNS                                     *
326400***********************************************************
326500*5030-RNS-SYS-ADD-ON.
326600*
326700*    MOVE 0 TO H-NEW-TECH-ADDON-RNSSYS
326800*              H-LESSER-RNSSYS-STOP-1
326900*              H-LESSER-RNSSYS-STOP-2
327000*              H-CSTMED-RNSSYS-STOP.
327100*
327200*5030-COMPUTE-RNSSYS.
327300*
327400*    MOVE  18475.00 TO H-CSTMED-RNSSYS-STOP.
327500*
327600*    COMPUTE H-LESSER-RNSSYS-STOP-1 ROUNDED =
327700*                 H-CSTMED-RNSSYS-STOP.
327800*
327900*    COMPUTE H-LESSER-RNSSYS-STOP-2 ROUNDED =
328000*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
328100*                    H-BASE-DRG-PAYMENT)) * .5.
328200*
328300*    IF H-LESSER-RNSSYS-STOP-2 > 0
328400*       IF H-LESSER-RNSSYS-STOP-1 < H-LESSER-RNSSYS-STOP-2
328500*        MOVE H-LESSER-RNSSYS-STOP-1 TO
328600*                               H-NEW-TECH-ADDON-RNSSYS
328700*       ELSE
328800*        MOVE H-LESSER-RNSSYS-STOP-2 TO
328900*                               H-NEW-TECH-ADDON-RNSSYS
329000*    ELSE
329100*       MOVE ZEROES          TO H-NEW-TECH-ADDON-RNSSYS.
329200*
329300*5030-ADD-TECH-CASES.
329400*
329500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
329600*            H-NEW-TECH-PAY-ADD-ON +
329700*            H-NEW-TECH-ADDON-RNSSYS.
329800*
329900*5030-EXIT.    EXIT.
330000
330100***********************************************************
330200* CASES INVOLVING DEFITELIO                               *
330300***********************************************************
330400 5040-DEFITELIO-TECH-ADD-ON.
330500
330600     MOVE 0 TO H-NEW-TECH-ADDON-DEFITELIO
330700               H-LESSER-DEFITELIO-STOP-1
330800               H-LESSER-DEFITELIO-STOP-2
330900               H-CSTMED-DEFITELIO-STOP.
331000
331100 5040-COMPUTE-DEFITELIO.
331200
331300     MOVE  75900.00 TO H-CSTMED-DEFITELIO-STOP.
331400
331500     COMPUTE H-LESSER-DEFITELIO-STOP-1 ROUNDED =
331600                  H-CSTMED-DEFITELIO-STOP.
331700
331800     COMPUTE H-LESSER-DEFITELIO-STOP-2 ROUNDED =
331900          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
332000                     H-BASE-DRG-PAYMENT)) * .5.
332100
332200     IF H-LESSER-DEFITELIO-STOP-2 > 0
332300        IF H-LESSER-DEFITELIO-STOP-1 < H-LESSER-DEFITELIO-STOP-2
332400         MOVE H-LESSER-DEFITELIO-STOP-1 TO
332500                                H-NEW-TECH-ADDON-DEFITELIO
332600        ELSE
332700         MOVE H-LESSER-DEFITELIO-STOP-2 TO
332800                                H-NEW-TECH-ADDON-DEFITELIO
332900     ELSE
333000        MOVE ZEROES          TO H-NEW-TECH-ADDON-DEFITELIO.
333100
333200 5040-ADD-TECH-CASES.
333300
333400     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
333500             H-NEW-TECH-PAY-ADD-ON +
333600             H-NEW-TECH-ADDON-DEFITELIO.
333700
333800 5040-EXIT.    EXIT.
333900
334000***********************************************************
334100* CASES INVOLVING GORE EXCLUDER                           *
334200***********************************************************
334300 5050-GORE-TECH-ADD-ON.
334400
334500     MOVE 0 TO H-NEW-TECH-ADDON-GORE
334600               H-LESSER-GORE-STOP-1
334700               H-LESSER-GORE-STOP-2
334800               H-CSTMED-GORE-STOP.
334900
335000 5050-COMPUTE-GORE.
335100
335200     MOVE  05250.00 TO H-CSTMED-GORE-STOP.
335300
335400     COMPUTE H-LESSER-GORE-STOP-1 ROUNDED =
335500                  H-CSTMED-GORE-STOP.
335600
335700     COMPUTE H-LESSER-GORE-STOP-2 ROUNDED =
335800          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
335900                     H-BASE-DRG-PAYMENT)) * .5.
336000
336100     IF H-LESSER-GORE-STOP-2 > 0
336200        IF H-LESSER-GORE-STOP-1 < H-LESSER-GORE-STOP-2
336300         MOVE H-LESSER-GORE-STOP-1 TO
336400                                H-NEW-TECH-ADDON-GORE
336500        ELSE
336600         MOVE H-LESSER-GORE-STOP-2 TO
336700                                H-NEW-TECH-ADDON-GORE
336800     ELSE
336900        MOVE ZEROES          TO H-NEW-TECH-ADDON-GORE.
337000
337100 5050-ADD-TECH-CASES.
337200
337300     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
337400             H-NEW-TECH-PAY-ADD-ON +
337500             H-NEW-TECH-ADDON-GORE.
337600
337700 5050-EXIT.    EXIT.
337800
337900***********************************************************
338000* CASES INVOLVING IDARUCIZUMAB                            *
338100***********************************************************
338200 5060-IDARUCIZ-TECH-ADD-ON.
338300
338400     MOVE 0 TO H-NEW-TECH-ADDON-IDARUCIZ
338500               H-LESSER-IDARUCIZ-STOP-1
338600               H-LESSER-IDARUCIZ-STOP-2
338700               H-CSTMED-IDARUCIZ-STOP.
338800
338900 5060-COMPUTE-IDARUCIZ.
339000
339100     MOVE  01750.00 TO H-CSTMED-IDARUCIZ-STOP.
339200
339300     COMPUTE H-LESSER-IDARUCIZ-STOP-1 ROUNDED =
339400                  H-CSTMED-IDARUCIZ-STOP.
339500
339600     COMPUTE H-LESSER-IDARUCIZ-STOP-2 ROUNDED =
339700          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
339800                     H-BASE-DRG-PAYMENT)) * .5.
339900
340000     IF H-LESSER-IDARUCIZ-STOP-2 > 0
340100        IF H-LESSER-IDARUCIZ-STOP-1 < H-LESSER-IDARUCIZ-STOP-2
340200         MOVE H-LESSER-IDARUCIZ-STOP-1 TO
340300                                H-NEW-TECH-ADDON-IDARUCIZ
340400        ELSE
340500         MOVE H-LESSER-IDARUCIZ-STOP-2 TO
340600                                H-NEW-TECH-ADDON-IDARUCIZ
340700     ELSE
340800        MOVE ZEROES          TO H-NEW-TECH-ADDON-IDARUCIZ.
340900
341000 5060-ADD-TECH-CASES.
341100
341200     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
341300             H-NEW-TECH-PAY-ADD-ON +
341400             H-NEW-TECH-ADDON-IDARUCIZ.
341500
341600 5060-EXIT.    EXIT.
341700
341800***********************************************************
341900* CASES INVOLVING MAGEC SPINE                             *
342000***********************************************************
342100*5070-MAGEC-TECH-ADD-ON.
342200*
342300*    MOVE 0 TO H-NEW-TECH-ADDON-MAGEC
342400*              H-LESSER-MAGEC-STOP-1
342500*              H-LESSER-MAGEC-STOP-2
342600*              H-CSTMED-MAGEC-STOP.
342700*
342800*5070-COMPUTE-MAGEC.
342900*
343000*    MOVE  15750.00 TO H-CSTMED-MAGEC-STOP.
343100*
343200*    COMPUTE H-LESSER-MAGEC-STOP-1 ROUNDED =
343300*                 H-CSTMED-MAGEC-STOP.
343400*
343500*    COMPUTE H-LESSER-MAGEC-STOP-2 ROUNDED =
343600*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
343700*                    H-BASE-DRG-PAYMENT)) * .5.
343800*
343900*    IF H-LESSER-MAGEC-STOP-2 > 0
344000*       IF H-LESSER-MAGEC-STOP-1 < H-LESSER-MAGEC-STOP-2
344100*        MOVE H-LESSER-MAGEC-STOP-1 TO
344200*                               H-NEW-TECH-ADDON-MAGEC
344300*       ELSE
344400*        MOVE H-LESSER-MAGEC-STOP-2 TO
344500*                               H-NEW-TECH-ADDON-MAGEC
344600*    ELSE
344700*       MOVE ZEROES          TO H-NEW-TECH-ADDON-MAGEC.
344800*
344900*5070-ADD-TECH-CASES.
345000*
345100*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
345200*            H-NEW-TECH-PAY-ADD-ON +
345300*            H-NEW-TECH-ADDON-MAGEC.
345400*
345500*5070-EXIT.    EXIT.
345600
345700***********************************************************
345800* CASES INVOLVING VISTOGARD                               *
345900***********************************************************
346000 5080-VISTOGARD-TECH-ADD-ON.
346100
346200     MOVE 0 TO H-NEW-TECH-ADDON-VISTOGARD
346300               H-LESSER-VISTOGARD-STOP-1
346400               H-LESSER-VISTOGARD-STOP-2
346500               H-CSTMED-VISTOGARD-STOP.
346600
346700 5080-COMPUTE-VISTOGARD.
346800
346900     MOVE 40130.00 TO H-CSTMED-VISTOGARD-STOP.
347000
347100     COMPUTE H-LESSER-VISTOGARD-STOP-1 ROUNDED =
347200                  H-CSTMED-VISTOGARD-STOP.
347300
347400     COMPUTE H-LESSER-VISTOGARD-STOP-2 ROUNDED =
347500          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
347600                     H-BASE-DRG-PAYMENT)) * .5.
347700
347800     IF H-LESSER-VISTOGARD-STOP-2 > 0
347900        IF H-LESSER-VISTOGARD-STOP-1 < H-LESSER-VISTOGARD-STOP-2
348000         MOVE H-LESSER-VISTOGARD-STOP-1 TO
348100                                H-NEW-TECH-ADDON-VISTOGARD
348200        ELSE
348300         MOVE H-LESSER-VISTOGARD-STOP-2 TO
348400                                H-NEW-TECH-ADDON-VISTOGARD
348500     ELSE
348600        MOVE ZEROES          TO H-NEW-TECH-ADDON-VISTOGARD.
348700
348800 5080-ADD-TECH-CASES.
348900
349000     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
349100             H-NEW-TECH-PAY-ADD-ON +
349200             H-NEW-TECH-ADDON-VISTOGARD.
349300
349400 5080-EXIT.    EXIT.
349500
349600***********************************************************
349700* CASES INVOLVING STELARA                                 *
349800***********************************************************
349900 5090-STELARA-TECH-ADD-ON.
350000
350100     MOVE 0 TO H-NEW-TECH-ADDON-STELARA
350200               H-LESSER-STELARA-STOP-1
350300               H-LESSER-STELARA-STOP-2
350400               H-CSTMED-STELARA-STOP.
350500
350600 5090-COMPUTE-STELARA.
350700
350800     MOVE 2400.00 TO H-CSTMED-STELARA-STOP.
350900
351000     COMPUTE H-LESSER-STELARA-STOP-1 ROUNDED =
351100                  H-CSTMED-STELARA-STOP.
351200
351300     COMPUTE H-LESSER-STELARA-STOP-2 ROUNDED =
351400          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
351500                     H-BASE-DRG-PAYMENT)) * .5.
351600
351700     IF H-LESSER-STELARA-STOP-2 > 0
351800        IF H-LESSER-STELARA-STOP-1 < H-LESSER-STELARA-STOP-2
351900         MOVE H-LESSER-STELARA-STOP-1 TO
352000                                H-NEW-TECH-ADDON-STELARA
352100        ELSE
352200         MOVE H-LESSER-STELARA-STOP-2 TO
352300                                H-NEW-TECH-ADDON-STELARA
352400     ELSE
352500        MOVE ZEROES TO H-NEW-TECH-ADDON-STELARA.
352600
352700 5090-ADD-TECH-CASES.
352800
352900     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
353000             H-NEW-TECH-PAY-ADD-ON +
353100             H-NEW-TECH-ADDON-STELARA.
353200
353300 5090-EXIT.    EXIT.
353400
353500***********************************************************
353600* CASES INVOLVING ZINPLAVA                                *
353700***********************************************************
353800 5100-ZINPLAVA-TECH-ADD-ON.
353900
354000     MOVE 0 TO H-NEW-TECH-ADDON-ZINPLAVA
354100               H-LESSER-ZINPLAVA-STOP-1
354200               H-LESSER-ZINPLAVA-STOP-2
354300               H-CSTMED-ZINPLAVA-STOP.
354400
354500 5100-COMPUTE-ZINPLAVA.
354600
354700     MOVE 1900.00 TO H-CSTMED-ZINPLAVA-STOP.
354800
354900     COMPUTE H-LESSER-ZINPLAVA-STOP-1 ROUNDED =
355000                  H-CSTMED-ZINPLAVA-STOP.
355100
355200     COMPUTE H-LESSER-ZINPLAVA-STOP-2 ROUNDED =
355300          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
355400                     H-BASE-DRG-PAYMENT)) * .5.
355500
355600     IF H-LESSER-ZINPLAVA-STOP-2 > 0
355700        IF H-LESSER-ZINPLAVA-STOP-1 < H-LESSER-ZINPLAVA-STOP-2
355800         MOVE H-LESSER-ZINPLAVA-STOP-1 TO
355900                                H-NEW-TECH-ADDON-ZINPLAVA
356000        ELSE
356100         MOVE H-LESSER-ZINPLAVA-STOP-2 TO
356200                                H-NEW-TECH-ADDON-ZINPLAVA
356300     ELSE
356400        MOVE ZEROES TO H-NEW-TECH-ADDON-ZINPLAVA.
356500
356600 5100-ADD-TECH-CASES.
356700
356800     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
356900             H-NEW-TECH-PAY-ADD-ON +
357000             H-NEW-TECH-ADDON-ZINPLAVA.
357100
357200 5100-EXIT.    EXIT.
357300
357400***********************************************************
357500* CASES INVOLVING EDWARDS/PERCEVAL SUTURELESS VALVES      *
357600***********************************************************
357700 5110-EDWARDS-TECH-ADD-ON.
357800
357900     MOVE 0 TO H-NEW-TECH-ADDON-EDWARDS
358000               H-LESSER-EDWARDS-STOP-1
358100               H-LESSER-EDWARDS-STOP-2
358200               H-CSTMED-EDWARDS-STOP.
358300
358400 5110-COMPUTE-EDWARDS.
358500
358600     MOVE 6110.23 TO H-CSTMED-EDWARDS-STOP.
358700
358800     COMPUTE H-LESSER-EDWARDS-STOP-1 ROUNDED =
358900                  H-CSTMED-EDWARDS-STOP.
359000
359100     COMPUTE H-LESSER-EDWARDS-STOP-2 ROUNDED =
359200          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
359300                     H-BASE-DRG-PAYMENT)) * .5.
359400
359500     IF H-LESSER-EDWARDS-STOP-2 > 0
359600        IF H-LESSER-EDWARDS-STOP-1 < H-LESSER-EDWARDS-STOP-2
359700         MOVE H-LESSER-EDWARDS-STOP-1 TO
359800                                H-NEW-TECH-ADDON-EDWARDS
359900        ELSE
360000         MOVE H-LESSER-EDWARDS-STOP-2 TO
360100                                H-NEW-TECH-ADDON-EDWARDS
360200     ELSE
360300        MOVE ZEROES TO H-NEW-TECH-ADDON-EDWARDS.
360400
360500 5110-ADD-TECH-CASES.
360600
360700     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
360800             H-NEW-TECH-PAY-ADD-ON +
360900             H-NEW-TECH-ADDON-EDWARDS.
361000
361100 5110-EXIT.    EXIT.
361200
361300**************************************************************
361400* CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM *
361500**************************************************************
361600 5500-CAP-CALC-TECH-ADD-ON.
361700
361800     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
361900     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
362000
362100     COMPUTE H-OPER-BILL-COSTS ROUNDED =
362200         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
362300         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
362400
362500     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
362600                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
362700
362800     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
362900                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
363000
363100     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
363200         H-NEW-TECH-ADDON-CAPDIF  > 0
363300        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
363400             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
363500
363600 5500-EXIT.    EXIT.
363700
363800***********************************************************
363900 6000-CALC-READMIS-REDU.
364000***********************************************************
364100*---------------------------------------------------------*
364200* (YEARCHANGE 2016.0)
364300* READMISSIONS PROCESS ADJUSTMENTS
364400*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
364500*---------------------------------------------------------*
364600
364700     MOVE 0 TO H-READMIS-ADJUST-AMT.
364800
364900     IF P-HOSP-READMISSION-REDU = '1'
365000           GO TO 6000-EDIT-READMISN
365100     ELSE
365200           NEXT SENTENCE.
365300
365400     IF P-HOSP-READMISSION-REDU = '0' AND
365500        P-HOSP-HRR-ADJUSTMT = 0.0000
365600           MOVE ZEROES TO H-READMIS-ADJUST-AMT
365700           GO TO 6000-EXIT.
365800
365900     IF P-HOSP-READMISSION-REDU = '0' AND
366000        P-HOSP-HRR-ADJUSTMT > 0.0000
366100           MOVE 65 TO PPS-RTC
366200           MOVE ZEROES TO H-READMIS-ADJUST-AMT
366300           GO TO 6000-EXIT.
366400
366500     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
366600                                  '6' OR '7' OR '8' OR
366700                                  '9' OR ' '
366800           MOVE 65 TO PPS-RTC
366900           MOVE ZEROES TO H-READMIS-ADJUST-AMT
367000           GO TO 6000-EXIT.
367100
367200 6000-EDIT-READMISN.
367300
367400     IF P-HOSP-HRR-ADJUSTMT < 0.9700
367500           MOVE 65 TO PPS-RTC
367600           MOVE ZEROES TO H-READMIS-ADJUST-AMT
367700           GO TO 6000-EXIT.
367800
367900     IF P-HOSP-HRR-ADJUSTMT > 1.0000
368000           MOVE 65 TO PPS-RTC
368100           MOVE ZEROES TO H-READMIS-ADJUST-AMT
368200           GO TO 6000-EXIT.
368300
368400     IF P-READ-INVALID-STATE
368500           MOVE 65 TO PPS-RTC
368600           MOVE ZEROES TO H-READMIS-ADJUST-AMT
368700           GO TO 6000-EXIT.
368800
368900 6000-COMPUTE-READMISN.
369000
369100        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
369200              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
369300                H-OPER-BASE-DRG-PAY).
369400
369500 6000-EXIT.    EXIT.
369600
369700***********************************************************
369800 7000-CALC-VALUE-BASED-PURCH.
369900***********************************************************
370000*---------------------------------------------------------*
370100* (YEARCHANGE 2016.0)
370200* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
370300*   + FY17: RANGE OF ALLOWABLE FACTORS (< 0.98 OR > 2.0)
370400*---------------------------------------------------------*
370500
370600     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
370700
370800     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
370900           NEXT SENTENCE
371000     ELSE
371100           MOVE 68 TO PPS-RTC
371200           GO TO 7000-EXIT.
371300
371400     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
371500           GO TO 7000-EXIT.
371600
371700     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
371800         P-NEW-CBSA-HOSP-QUAL-IND = '1'
371900           NEXT SENTENCE
372000     ELSE
372100           MOVE 68 TO PPS-RTC
372200           GO TO 7000-EXIT.
372300
372400     IF  P-VBP-INVALID-STATE
372500           MOVE 68 TO PPS-RTC
372600           GO TO 7000-EXIT
372700     ELSE
372800           NEXT SENTENCE.
372900
373000     IF P-VAL-BASED-PURCH-ADJUST < 0.9800000000 OR
373100        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
373200           MOVE 68 TO PPS-RTC
373300           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
373400           GO TO 7000-EXIT
373500     ELSE
373600           GO TO 7000-COMPUTE-VAL-BASED-PUR.
373700
373800 7000-COMPUTE-VAL-BASED-PUR.
373900
374000     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
374100              ((P-VAL-BASED-PURCH-ADJUST *
374200                  H-OPER-BASE-DRG-PAY) -
374300                  H-OPER-BASE-DRG-PAY).
374400
374500 7000-EXIT.    EXIT.
374600
374700***********************************************************
374800 8000-CALC-BUNDLE-REDU.
374900***********************************************************
375000***** CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
375100***********************************************************
375200
375300     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
375400     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
375500
375600     IF '61' =  B-DEMO-CODE1  OR
375700                B-DEMO-CODE2  OR
375800                B-DEMO-CODE3  OR
375900                B-DEMO-CODE4
376000         NEXT SENTENCE
376100     ELSE
376200         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
376300           GO TO 8000-EXIT.
376400
376500     IF P-MODEL1-BUNDLE-DISPRCNT > .00
376600           GO TO 8000-COMPUTE-BUNDLE
376700     ELSE
376800           NEXT SENTENCE.
376900
377000     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
377100           GO TO 8000-EXIT.
377200
377300 8000-COMPUTE-BUNDLE.
377400
377500     IF  B-DISCHARGE-DATE < 20140401 AND
377600         P-MODEL1-BUNDLE-DISPRCNT = .01
377700         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
377800          (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
377900     ELSE
378000         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
378100          (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
378200
378300        COMPUTE H-BUNDLE-ADJUST-AMT      ROUNDED =
378400              ((WK-MODEL1-BUNDLE-DISPRCNT *
378500                                     H-OPER-BASE-DRG-PAY) -
378600                H-OPER-BASE-DRG-PAY).
378700
378800        COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
378900
379000 8000-EXIT.    EXIT.
379100
379200***********************************************************
379300 9000-CALC-EHR-SAVING.
379400***********************************************************
379500*---------------------------------------------------------*
379600* (YEARCHANGE 2017.0)
379700* CASES INVOLVING EHR SAVINGS
379800*   + FY17: ANNUAL UPDATE TO BELOW VALUES
379900*   + EHR-FULL = FULL MB / NO EHR MB
380000*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
380100*---------------------------------------------------------*
380200
380300     MOVE 1.020387616 TO H-MB-RATIO-EHR-FULL.
380400     MOVE 1.020527116 TO H-MB-RATIO-EHR-QUAL-FULL.
380500     MOVE 0 TO H-EHR-SUBSAV-QUANT.
380600     MOVE 0 TO H-EHR-SUBSAV-LV.
380700     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
380800     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
380900
381000     IF P-EHR-REDUC-IND = 'Y'
381100         NEXT SENTENCE
381200     ELSE
381300         GO TO 9000-EXIT.
381400
381500 9000-COMPUTE-EHR.
381600
381700* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
381800* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
381900* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
382000* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
382100* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
382200* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
382300* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
382400* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
382500* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
382600* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
382700* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
382800* OUT.
382900*
383000* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
383100* SAVINGS CALCULATION SO CALCULATE AN OPERATING
383200* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
383300* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
383400* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
383500
383600      COMPUTE H-EHR-SUBSAV-QUANT =
383700           (PPS-OPER-HSP-PART +
383800            PPS-OPER-FSP-PART +
383900            PPS-OPER-DSH-ADJ +
384000            PPS-OPER-IME-ADJ +
384100            H-READMIS-ADJUST-AMT +
384200            H-VAL-BASED-PURCH-ADJUST-AMT +
384300            H-BUNDLE-ADJUST-AMT).
384400
384500* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
384600* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
384700* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
384800* READM VBP AND BUNDLE
384900* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
385000* FOR EHR SAVINGS FIELD;
385100
385200      MOVE 0 TO H-EHR-SUBSAV-LV.
385300
385400      IF P-NEW-TEMP-RELIEF-IND = 'Y'
385500         AND P-LV-ADJ-FACTOR > 0.00
385600         AND P-LV-ADJ-FACTOR <= 0.25
385700      COMPUTE H-EHR-SUBSAV-LV =
385800          (PPS-OPER-HSP-PART +
385900           PPS-OPER-FSP-PART +
386000           PPS-OPER-DSH-ADJ +
386100           PPS-OPER-IME-ADJ ) * P-LV-ADJ-FACTOR.
386200
386300      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
386400           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
386500
386600* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
386700* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
386800* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
386900* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
387000* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
387100* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
387200* OF .625 ONLY.
387300
387400       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
387500       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
387600
387700     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
387800        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
387900          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
388000
388100        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
388200          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
388300
388400 9000-EXIT.    EXIT.
388500
388600*---------------------------------------------------------*
388700* (YEARCHANGE 2016.0)
388800*---------------------------------------------------------*
388900 9010-CALC-STANDARD-CHG.
389000
389100***********************************************************
389200***ÝCM-P3¨ STANDARDIZED OPERATING COST CALCULATION
389300
389400     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
389500               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
389600        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
389700        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
389800        ((H-LABOR-PCT * H-WAGE-INDEX) +
389900               (H-NONLABOR-PCT * H-OPER-COLA))
390000     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
390100
390200***********************************************************
390300***ÝCM-P3¨ STANDARDIZED CAPITAL COST CALCULATION
390400
390500     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
390600       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
390700        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
390800               (H-CAPI-GAF * H-CAPI-COLA)
390900     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
391000
391100***********************************************************
391200***ÝCM-P3¨ STANDARDIZED OPERATING TRESHOLD
391300
391400     MOVE 5572.53 TO H-OPER-BASE.
391500
391600     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
391700      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
391800                        +
391900           (H-OPER-BASE * H-DRG-WT-FRCTN)
392000                        +
392100              H-NEW-TECH-PAY-ADD-ON.
392200
392300******************************************************
392400***ÝCM-P3¨ STANDARDIZED CAPITAL TRESHOLD
392500
392600     MOVE 453.95 TO H-CAPI-BASE.
392700
392800     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
392900     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
393000                     +
393100     (H-CAPI-BASE * H-DRG-WT-FRCTN).
393200
393300******************************************************
393400***ÝCM-P3¨ STANDARDIZED OPERATING OUTLIER CALCULATION
393500
393600     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
393700        (H-OPER-STDZ-DOLLAR-THRESHOLD +
393800                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
393900                          AND
394000         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
394100
394200       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
394300        (H-CSTOUT-PCT  *
394400        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
394500
394600     ELSE
394700       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
394800
394900******************************************************
395000***ÝCM-P3¨ STANDARDIZED CAPITAL OUTLIER CALCULATION
395100
395200     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
395300        (H-OPER-STDZ-DOLLAR-THRESHOLD +
395400                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
395500                          AND
395600         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
395700
395800      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
395900      (H-CSTOUT-PCT  *
396000      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
396100     ELSE
396200      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
396300
396400*******************************************************
396500***ÝCM-P3¨ STANDARDIZED ALLOWED AMOUNT CALCULATION
396600
396700      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
396800       (H-OPER-BASE + H-CAPI-BASE)
396900                 *
397000       H-DRG-WT-FRCTN
397100                 +
397200       H-OPER-STDZ-COST-OUTLIER
397300                 +
397400       H-CAPI-STDZ-COST-OUTLIER
397500                 +
397600       H-NEW-TECH-PAY-ADD-ON.
397700
397800 9010-EXIT.    EXIT.
397900
398000***********************************************************
398100******        L A S T   S O U R C E   S T A T E M E N T   *****
