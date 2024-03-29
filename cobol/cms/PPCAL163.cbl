000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.                PPCAL163.
000300*REVISED.                   12-29-2015.
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
002000     'PPCAL163      - W O R K I N G   S T O R A G E'.
002100 01  CAL-VERSION                    PIC X(05)  VALUE 'C16.3'.
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
004500 01  H-C-NAT-PCT                    PIC 9(01)V9(02).
004600 01  H-C-REG-PCT                    PIC 9(01)V9(02).
004700
004800*---------------------------------------------------------*
004900* (YEARCHANGE 2016.0)
005000* LABOR & NON-LABOR RATES TABLE
005100*---------------------------------------------------------*
005200
005300 COPY RATEX160.
005400
005500 COPY RATEX162.
005600
005700*---------------------------------------------------------*
005800* (YEARCHANGE 2016.0)
005900* DIAGNOSIS RELATED GROUP (DRG) WEIGHT TABLE
006000*   + TABLE 5 FROM ANNUAL IPPS FINAL RULE
006100*---------------------------------------------------------*
006200
006300 COPY DRGSX160.
006400
006500*---------------------------------------------------------*
006600* (YEARCHANGE 2016.0)
006700* LOW VOLUME TABLE
006800*---------------------------------------------------------*
006900
007000 COPY LVOLX160.
007100
007200***********************************************************
007300*****YEARCHANGE 2015.0 ************************************
007400***********************************************************
007500***  PROVIDER ADJUSTMENT TABLE FOR UNCOMPENSATED CARE UCC
007600***  WAS CHANGED TO DATA COMING FROM THE PROVIDER FILE
007700***********************************************************
007800
007900 01  MES-ADD-PROV                   PIC X(53) VALUE SPACES.
008000 01  MES-CHG-PROV                   PIC X(53) VALUE SPACES.
008100 01  MES-PPS-PROV                   PIC X(06).
008200 01  MES-PPS-STATE                  PIC X(02).
008300 01  MES-INTRO                      PIC X(53) VALUE SPACES.
008400 01  MES-TOT-PAY                    PIC 9(07)V9(02) VALUE 0.
008500 01  MES-SSRFBN.
008600     05 MES-SSRFBN-STATE PIC 99.
008700     05 FILLER           PIC XX.
008800     05 MES-SSRFBN-RATE  PIC 9(1)V9(5).
008900     05 FILLER           PIC XX.
009000     05 MES-SSRFBN-CODE2 PIC 99.
009100     05 FILLER           PIC XX.
009200     05 MES-SSRFBN-STNAM PIC X(20).
009300     05 MES-SSRFBN-REST  PIC X(22).
009400
009500 01 WK-HLDDRG-DATA.
009600     05  HLDDRG-DATA.
009700         10  HLDDRG-DRGX               PIC X(03).
009800         10  FILLER1                   PIC X(01).
009900         10  HLDDRG-WEIGHT             PIC 9(02)V9(04).
010000         10  FILLER2                   PIC X(01).
010100         10  HLDDRG-GMALOS             PIC 9(02)V9(01).
010200         10  FILLER3                   PIC X(05).
010300         10  HLDDRG-LOW                PIC X(01).
010400         10  FILLER5                   PIC X(01).
010500         10  HLDDRG-ARITH-ALOS         PIC 9(02)V9(01).
010600         10  FILLER6                   PIC X(02).
010700         10  HLDDRG-PAC                PIC X(01).
010800         10  FILLER7                   PIC X(01).
010900         10  HLDDRG-SPPAC              PIC X(01).
011000         10  FILLER8                   PIC X(02).
011100         10  HLDDRG-DESC               PIC X(26).
011200
011300
011400 01 WK-HLDDRG-DATA2.
011500     05  HLDDRG-DATA2.
011600         10  HLDDRG-DRGX2               PIC X(03).
011700         10  FILLER21                   PIC X(01).
011800         10  HLDDRG-WEIGHT2             PIC 9(02)V9(04).
011900         10  FILLER22                   PIC X(01).
012000         10  HLDDRG-GMALOS2             PIC 9(02)V9(01).
012100         10  FILLER23                   PIC X(05).
012200         10  HLDDRG-LOW2                PIC X(01).
012300         10  FILLER25                   PIC X(01).
012400         10  HLDDRG-ARITH-ALOS2         PIC 9(02)V9(01).
012500         10  FILLER26                   PIC X(02).
012600         10  HLDDRG-TRANS-FLAGS.
012700                   88  D-DRG-POSTACUTE-50-50
012800                   VALUE 'Y Y'.
012900                   88  D-DRG-POSTACUTE-PERDIEM
013000                   VALUE 'Y  '.
013100             15  HLDDRG-PAC2            PIC X(01).
013200             15  FILLER27               PIC X(01).
013300             15  HLDDRG-SPPAC2          PIC X(01).
013400         10  FILLER28                   PIC X(02).
013500         10  HLDDRG-DESC2               PIC X(26).
013600         10  HLDDRG-VALID               PIC X(01).
013700
013800
013900 01  MES-LOWVOL.
014000     05  MES-LOWVOL-PROV             PIC X(6).
014100     05  FILLER                      PIC XXX.
014200     05  MESWK-LOWVOL-PROV-DISCHG    PIC 9999.
014300
014400
014500 01  WK-UNCOMP-CARE.
014600     05  WK-UNCOMP-CARE-PROV         PIC X(6).
014700     05  FILLER                      PIC X.
014800     05  WK-UNCOMP-CARE-AMOUNT       PIC 9(06)V9(02).
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
016600 LINKAGE SECTION.
016700***************************************************************
016800*                 * * * * * * * * *                           *
016900*    REVIEW CODES ARE USED TO DIRECT THE PPCAL  SUBROUTINE    *
017000*    IN HOW TO PAY THE BILL.                                  *
017100*                         *****                               *
017200*    COMMENTS  ** CLAIMS RECEIVED WITH CONDITION CODE 66      *
017300*                 SHOULD BE PROCESSED UNDER REVIEW CODE 06,   *
017400*                 07 OR 11 AS APPROPRIATE TO EXCLUDE ANY      *
017500*                 OUTLIER COMPUTATION.                        *
017600*                         *****                               *
017700*         REVIEW-CODE:                                        *
017800*            00 = PAY-WITH-OUTLIER.                           *
017900*                 WILL CALCULATE THE STANDARD PAYMENT.        *
018000*                 WILL ALSO ATTEMPT TO PAY ONLY COST          *
018100*                 OUTLIERS, DAY OUTLIERS EXPIRED 10/01/97     *
018200*            03 = PAY-PERDIEM-DAYS.                           *
018300*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
018400*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
018500*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
018600*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
018700*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
018800*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
018900*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
019000*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
019100*                 BILL EXCEED THE COST THRESHOLD.             *
019200*            06 = PAY-XFER-NO-COST                            *
019300*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
019400*                 THE STANDARD PAYMENT IF THE COVERED DAYS    *
019500*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
019600*                 FOR THE DRG.  IF COVERED DAYS EQUAL OR      *
019700*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
019800*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
019900*                 CALCULATE ANY COST OUTLIER PORTION          *
020000*                 OF THE PAYMENT.                             *
020100*            07 = PAY-WITHOUT-COST.                           *
020200*                 WILL CALCULATE THE STANDARD PAYMENT         *
020300*                 WITHOUT COST PORTION.                       *
020400*            09 = PAY-XFER-SPEC-DRG - POST-ACUTE TRANSFERS    *
020500*                 50-50> NOW USES Y INDICATORS ON DRGS
020600*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
020700*                        RULE
020800* =======================================================
020900* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
021000* =======================================================
021100*
021200*
021300*     FULL PERDIEM >   NOW USES Y INDICATORS ON DRGS
021400*                      SEE TABLE 5 FROM ANNUAL IPPS FINAL
021500*                      RULE
021600*
021700*                               POST-ACUTE TRANSFERS          *
021800*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
021900*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
022000*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
022100*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
022200*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
022300*                 STANDARD PAYMENT IS CALCULATED. WILL ALSO   *
022400*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
022500*                 PAYMENT IF THE ADJUSTED CHARGES ON THE      *
022600*                 BILL EXCEED THE COST THRESHOLD.             *
022700*            11 = PAY-XFER-SPEC-DRG-NO-COST                   *
022800*                 POST-ACUTE TRANSFERS                        *
022900*                 50-50> NOW USES Y INDICATORS ON DRGS
023000*                        SEE TABLE 5 FROM ANNUAL IPPS FINAL
023100*                        RULE
023200* =======================================================
023300* THE 50/50 DRG'S DO NOT REPEAT WITH THE FULL PERDIEM DRG'S
023400* =======================================================
023500*
023600*     FULL PERDIEM >  NOW USES Y INDICATORS ON DRGS
023700*                     SEE TABLE 5
023800*
023900*
024000*                               POST-ACUTE TRANSFERS          *
024100*                 WILL CALCULATE A PERDIEM PAYMENT BASED ON   *
024200*                 THE STANDARD DRG PAYMENT IF THE COVERED DAYS*
024300*                 ARE LESS THAN THE AVERAGE LENGTH OF STAY    *
024400*                 FOR THE DRG. IF COVERED DAYS EQUAL OR       *
024500*                 EXCEED THE AVERAGE LENGTH OF STAY, THE      *
024600*                 STANDARD PAYMENT IS CALCULATED. WILL NOT    *
024700*                 CALCULATE THE COST OUTLIER PORTION OF THE   *
024800*                 PAYMENT.                                    *
024900***************************************************************
025000
025100**************************************************************
025200*      MILLINNIUM COMPATIBLE                                 *
025300*      THIS IS THE BILL-RECORD THAT WILL BE PASSED BACK FROM *
025400*      THE PPCAL001 PROGRAM AND AFTER FOR PROCESSING         *
025500*      IN THE NEW FORMAT                                     *
025600**************************************************************
025700 01  BILL-NEW-DATA.
025800         10  B-NPI10.
025900             15  B-NPI8             PIC X(08).
026000             15  B-NPI-FILLER       PIC X(02).
026100         10  B-PROVIDER-NO          PIC X(06).
026200             88  B-FORMER-MDH-PROVIDERS
026300                                      VALUE '080006' '140184'
026400                                            '390072' '420019'
026500                                            '440031' '450451'
026600                                            '490019' '510062'.
026700         10  B-REVIEW-CODE          PIC 9(02).
026800             88  VALID-REVIEW-CODE    VALUE 00 03 06 07 09 11.
026900             88  PAY-WITH-OUTLIER     VALUE 00 07.
027000             88  PAY-PERDIEM-DAYS     VALUE 03.
027100             88  PAY-XFER-NO-COST     VALUE 06.
027200             88  PAY-WITHOUT-COST     VALUE 07.
027300             88  PAY-XFER-SPEC-DRG    VALUE 09 11.
027400             88  PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
027500         10  B-DRG                  PIC 9(03).
027600
027700             88  B-DRG-SPIRATN-DRG
027800                   VALUE 163 164 165.
027900
028000             88  B-DRG-SPIRATN-DRG11
028100                   VALUE 199 200 201.
028200
028300
028400             88  B-DRG-AUTOLITT-DRG
028500                   VALUE 25 26 27.
028600
028700* =======================================================
028800* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
028900* =======================================================
029000*
029100*            88  B-DRG-POSTACUTE-PERDIEM
029200*                         VALUE  NOW USES Y INDICATORS ON DRGS
029300*                         SEE TABLE 5
029400*                         D-DRG-POSTACUTE-PERDIEM
029500
029600         10  B-LOS                  PIC 9(03).
029700         10  B-COVERED-DAYS         PIC 9(03).
029800         10  B-LTR-DAYS             PIC 9(02).
029900         10  B-DISCHARGE-DATE.
030000             15  B-DISCHG-CC        PIC 9(02).
030100             15  B-DISCHG-YY        PIC 9(02).
030200             15  B-DISCHG-MM        PIC 9(02).
030300             15  B-DISCHG-DD        PIC 9(02).
030400         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
030500         10  B-PRIN-PROC-CODE       PIC X(07).
030600             88  B-PROC-ISLET-PRIN
030700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
030800                         '3E0J8U1'.
030900             88  B-PROC-ZENITH-PRIN
031000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
031100             88  B-PROC-VORAXAZE-PRIN
031200                   VALUE '3E033GQ' '3E043GQ'.
031300             88  B-PROC-ARGUS-PRIN
031400                   VALUE '08H005Z' '08H105Z'.
031500             88  B-PROC-KCENTRA-PRIN
031600                   VALUE '30283B1'.
031700             88  B-PROC-ZILVER-PRIN
031800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
031900                         '047L34Z' '047L44Z'.
032000             88  B-PROC-CARDIO-PRIN
032100                   VALUE '02HQ30Z' '02HR30Z'.
032200             88  B-PROC-MITRACLP-PRIN
032300                   VALUE '02UG3JZ'.
032400             88  B-PROC-RNSSYS1-PRIN
032500                   VALUE '0NH00NZ'.
032600             88  B-PROC-RNSSYS2-PRIN
032700                   VALUE '00H00MZ'.
032800             88  B-PROC-BLINATU-PRIN
032900                   VALUE 'XW03351' 'XW04351'.
033000             88  B-PROC-LUTONIX-PRIN
033100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
033200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
033300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
033400                         '047L341' '047L3D1' '047L3Z1' '047L441'
033500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
033600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
033700                         '047M441' '047M4D1' '047M4Z1' '047N041'
033800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
033900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
034000         10  B-OTHER-PROC-CODE1     PIC X(07).
034100             88  B-PROC-ISLET-PROC1
034200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
034300                         '3E0J8U1'.
034400             88  B-PROC-ZENITH-PROC1
034500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
034600             88  B-PROC-VORAXAZE-PROC1
034700                   VALUE '3E033GQ' '3E043GQ'.
034800             88  B-PROC-ARGUS-PROC1
034900                   VALUE '08H005Z' '08H105Z'.
035000             88  B-PROC-KCENTRA-PROC1
035100                   VALUE '30283B1'.
035200             88  B-PROC-ZILVER-PROC1
035300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
035400                         '047L34Z' '047L44Z'.
035500             88  B-PROC-CARDIO-PROC1
035600                   VALUE '02HQ30Z' '02HR30Z'.
035700             88  B-PROC-MITRACLP-PROC1
035800                   VALUE '02UG3JZ'.
035900             88  B-PROC-RNSSYS1-PROC1
036000                   VALUE '0NH00NZ'.
036100             88  B-PROC-RNSSYS2-PROC1
036200                   VALUE '00H00MZ'.
036300             88  B-PROC-BLINATU-PROC1
036400                   VALUE 'XW03351' 'XW04351'.
036500             88  B-PROC-LUTONIX-PROC1
036600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
036700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
036800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
036900                         '047L341' '047L3D1' '047L3Z1' '047L441'
037000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
037100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
037200                         '047M441' '047M4D1' '047M4Z1' '047N041'
037300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
037400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
037500         10  B-OTHER-PROC-CODE2     PIC X(07).
037600             88  B-PROC-ISLET-PROC2
037700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
037800                         '3E0J8U1'.
037900             88  B-PROC-ZENITH-PROC2
038000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
038100             88  B-PROC-VORAXAZE-PROC2
038200                   VALUE '3E033GQ' '3E043GQ'.
038300             88  B-PROC-ARGUS-PROC2
038400                   VALUE '08H005Z' '08H105Z'.
038500             88  B-PROC-KCENTRA-PROC2
038600                   VALUE '30283B1'.
038700             88  B-PROC-ZILVER-PROC2
038800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
038900                         '047L34Z' '047L44Z'.
039000             88  B-PROC-CARDIO-PROC2
039100                   VALUE '02HQ30Z' '02HR30Z'.
039200             88  B-PROC-MITRACLP-PROC2
039300                   VALUE '02UG3JZ'.
039400             88  B-PROC-RNSSYS1-PROC2
039500                   VALUE '0NH00NZ'.
039600             88  B-PROC-RNSSYS2-PROC2
039700                   VALUE '00H00MZ'.
039800             88  B-PROC-BLINATU-PROC2
039900                   VALUE 'XW03351' 'XW04351'.
040000             88  B-PROC-LUTONIX-PROC2
040100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
040200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
040300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
040400                         '047L341' '047L3D1' '047L3Z1' '047L441'
040500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
040600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
040700                         '047M441' '047M4D1' '047M4Z1' '047N041'
040800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
040900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
041000         10  B-OTHER-PROC-CODE3     PIC X(07).
041100             88  B-PROC-ISLET-PROC3
041200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
041300                         '3E0J8U1'.
041400             88  B-PROC-ZENITH-PROC3
041500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
041600             88  B-PROC-VORAXAZE-PROC3
041700                   VALUE '3E033GQ' '3E043GQ'.
041800             88  B-PROC-ARGUS-PROC3
041900                   VALUE '08H005Z' '08H105Z'.
042000             88  B-PROC-KCENTRA-PROC3
042100                   VALUE '30283B1'.
042200             88  B-PROC-ZILVER-PROC3
042300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
042400                         '047L34Z' '047L44Z'.
042500             88  B-PROC-CARDIO-PROC3
042600                   VALUE '02HQ30Z' '02HR30Z'.
042700             88  B-PROC-MITRACLP-PROC3
042800                   VALUE '02UG3JZ'.
042900             88  B-PROC-RNSSYS1-PROC3
043000                   VALUE '0NH00NZ'.
043100             88  B-PROC-RNSSYS2-PROC3
043200                   VALUE '00H00MZ'.
043300             88  B-PROC-BLINATU-PROC3
043400                   VALUE 'XW03351' 'XW04351'.
043500             88  B-PROC-LUTONIX-PROC3
043600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
043700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
043800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
043900                         '047L341' '047L3D1' '047L3Z1' '047L441'
044000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
044100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
044200                         '047M441' '047M4D1' '047M4Z1' '047N041'
044300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
044400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
044500         10  B-OTHER-PROC-CODE4     PIC X(07).
044600             88  B-PROC-ISLET-PROC4
044700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
044800                         '3E0J8U1'.
044900             88  B-PROC-ZENITH-PROC4
045000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
045100             88  B-PROC-VORAXAZE-PROC4
045200                   VALUE '3E033GQ' '3E043GQ'.
045300             88  B-PROC-ARGUS-PROC4
045400                   VALUE '08H005Z' '08H105Z'.
045500             88  B-PROC-KCENTRA-PROC4
045600                   VALUE '30283B1'.
045700             88  B-PROC-ZILVER-PROC4
045800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
045900                         '047L34Z' '047L44Z'.
046000             88  B-PROC-CARDIO-PROC4
046100                   VALUE '02HQ30Z' '02HR30Z'.
046200             88  B-PROC-MITRACLP-PROC4
046300                   VALUE '02UG3JZ'.
046400             88  B-PROC-RNSSYS1-PROC4
046500                   VALUE '0NH00NZ'.
046600             88  B-PROC-RNSSYS2-PROC4
046700                   VALUE '00H00MZ'.
046800             88  B-PROC-BLINATU-PROC4
046900                   VALUE 'XW03351' 'XW04351'.
047000             88  B-PROC-LUTONIX-PROC4
047100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
047200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
047300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
047400                         '047L341' '047L3D1' '047L3Z1' '047L441'
047500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
047600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
047700                         '047M441' '047M4D1' '047M4Z1' '047N041'
047800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
047900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
048000         10  B-OTHER-PROC-CODE5     PIC X(07).
048100             88  B-PROC-ISLET-PROC5
048200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
048300                         '3E0J8U1'.
048400             88  B-PROC-ZENITH-PROC5
048500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
048600             88  B-PROC-VORAXAZE-PROC5
048700                   VALUE '3E033GQ' '3E043GQ'.
048800             88  B-PROC-ARGUS-PROC5
048900                   VALUE '08H005Z' '08H105Z'.
049000             88  B-PROC-KCENTRA-PROC5
049100                   VALUE '30283B1'.
049200             88  B-PROC-ZILVER-PROC5
049300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
049400                         '047L34Z' '047L44Z'.
049500             88  B-PROC-CARDIO-PROC5
049600                   VALUE '02HQ30Z' '02HR30Z'.
049700             88  B-PROC-MITRACLP-PROC5
049800                   VALUE '02UG3JZ'.
049900             88  B-PROC-RNSSYS1-PROC5
050000                   VALUE '0NH00NZ'.
050100             88  B-PROC-RNSSYS2-PROC5
050200                   VALUE '00H00MZ'.
050300             88  B-PROC-BLINATU-PROC5
050400                   VALUE 'XW03351' 'XW04351'.
050500             88  B-PROC-LUTONIX-PROC5
050600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
050700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
050800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
050900                         '047L341' '047L3D1' '047L3Z1' '047L441'
051000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
051100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
051200                         '047M441' '047M4D1' '047M4Z1' '047N041'
051300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
051400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
051500         10  B-OTHER-PROC-CODE6     PIC X(07).
051600             88  B-PROC-ISLET-PROC6
051700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
051800                         '3E0J8U1'.
051900             88  B-PROC-ZENITH-PROC6
052000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
052100             88  B-PROC-VORAXAZE-PROC6
052200                   VALUE '3E033GQ' '3E043GQ'.
052300             88  B-PROC-ARGUS-PROC6
052400                   VALUE '08H005Z' '08H105Z'.
052500             88  B-PROC-KCENTRA-PROC6
052600                   VALUE '30283B1'.
052700             88  B-PROC-ZILVER-PROC6
052800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
052900                         '047L34Z' '047L44Z'.
053000             88  B-PROC-CARDIO-PROC6
053100                   VALUE '02HQ30Z' '02HR30Z'.
053200             88  B-PROC-MITRACLP-PROC6
053300                   VALUE '02UG3JZ'.
053400             88  B-PROC-RNSSYS1-PROC6
053500                   VALUE '0NH00NZ'.
053600             88  B-PROC-RNSSYS2-PROC6
053700                   VALUE '00H00MZ'.
053800             88  B-PROC-BLINATU-PROC6
053900                   VALUE 'XW03351' 'XW04351'.
054000             88  B-PROC-LUTONIX-PROC6
054100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
054200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
054300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
054400                         '047L341' '047L3D1' '047L3Z1' '047L441'
054500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
054600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
054700                         '047M441' '047M4D1' '047M4Z1' '047N041'
054800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
054900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
055000         10  B-OTHER-PROC-CODE7     PIC X(07).
055100             88  B-PROC-ISLET-PROC7
055200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
055300                         '3E0J8U1'.
055400             88  B-PROC-ZENITH-PROC7
055500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
055600             88  B-PROC-VORAXAZE-PROC7
055700                   VALUE '3E033GQ' '3E043GQ'.
055800             88  B-PROC-ARGUS-PROC7
055900                   VALUE '08H005Z' '08H105Z'.
056000             88  B-PROC-KCENTRA-PROC7
056100                   VALUE '30283B1'.
056200             88  B-PROC-ZILVER-PROC7
056300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
056400                         '047L34Z' '047L44Z'.
056500             88  B-PROC-CARDIO-PROC7
056600                   VALUE '02HQ30Z' '02HR30Z'.
056700             88  B-PROC-MITRACLP-PROC7
056800                   VALUE '02UG3JZ'.
056900             88  B-PROC-RNSSYS1-PROC7
057000                   VALUE '0NH00NZ'.
057100             88  B-PROC-RNSSYS2-PROC7
057200                   VALUE '00H00MZ'.
057300             88  B-PROC-BLINATU-PROC7
057400                   VALUE 'XW03351' 'XW04351'.
057500             88  B-PROC-LUTONIX-PROC7
057600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
057700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
057800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
057900                         '047L341' '047L3D1' '047L3Z1' '047L441'
058000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
058100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
058200                         '047M441' '047M4D1' '047M4Z1' '047N041'
058300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
058400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
058500         10  B-OTHER-PROC-CODE8     PIC X(07).
058600             88  B-PROC-ISLET-PROC8
058700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
058800                         '3E0J8U1'.
058900             88  B-PROC-ZENITH-PROC8
059000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
059100             88  B-PROC-VORAXAZE-PROC8
059200                   VALUE '3E033GQ' '3E043GQ'.
059300             88  B-PROC-ARGUS-PROC8
059400                   VALUE '08H005Z' '08H105Z'.
059500             88  B-PROC-KCENTRA-PROC8
059600                   VALUE '30283B1'.
059700             88  B-PROC-ZILVER-PROC8
059800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
059900                         '047L34Z' '047L44Z'.
060000             88  B-PROC-CARDIO-PROC8
060100                   VALUE '02HQ30Z' '02HR30Z'.
060200             88  B-PROC-MITRACLP-PROC8
060300                   VALUE '02UG3JZ'.
060400             88  B-PROC-RNSSYS1-PROC8
060500                   VALUE '0NH00NZ'.
060600             88  B-PROC-RNSSYS2-PROC8
060700                   VALUE '00H00MZ'.
060800             88  B-PROC-BLINATU-PROC8
060900                   VALUE 'XW03351' 'XW04351'.
061000             88  B-PROC-LUTONIX-PROC8
061100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
061200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
061300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
061400                         '047L341' '047L3D1' '047L3Z1' '047L441'
061500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
061600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
061700                         '047M441' '047M4D1' '047M4Z1' '047N041'
061800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
061900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
062000         10  B-OTHER-PROC-CODE9     PIC X(07).
062100             88  B-PROC-ISLET-PROC9
062200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
062300                         '3E0J8U1'.
062400             88  B-PROC-ZENITH-PROC9
062500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
062600             88  B-PROC-VORAXAZE-PROC9
062700                   VALUE '3E033GQ' '3E043GQ'.
062800             88  B-PROC-ARGUS-PROC9
062900                   VALUE '08H005Z' '08H105Z'.
063000             88  B-PROC-KCENTRA-PROC9
063100                   VALUE '30283B1'.
063200             88  B-PROC-ZILVER-PROC9
063300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
063400                         '047L34Z' '047L44Z'.
063500             88  B-PROC-CARDIO-PROC9
063600                   VALUE '02HQ30Z' '02HR30Z'.
063700             88  B-PROC-MITRACLP-PROC9
063800                   VALUE '02UG3JZ'.
063900             88  B-PROC-RNSSYS1-PROC9
064000                   VALUE '0NH00NZ'.
064100             88  B-PROC-RNSSYS2-PROC9
064200                   VALUE '00H00MZ'.
064300             88  B-PROC-BLINATU-PROC9
064400                   VALUE 'XW03351' 'XW04351'.
064500             88  B-PROC-LUTONIX-PROC9
064600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
064700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
064800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
064900                         '047L341' '047L3D1' '047L3Z1' '047L441'
065000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
065100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
065200                         '047M441' '047M4D1' '047M4Z1' '047N041'
065300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
065400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
065500         10  B-OTHER-PROC-CODE10    PIC X(07).
065600             88  B-PROC-ISLET-PROC10
065700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
065800                         '3E0J8U1'.
065900             88  B-PROC-ZENITH-PROC10
066000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
066100             88  B-PROC-VORAXAZE-PROC10
066200                   VALUE '3E033GQ' '3E043GQ'.
066300             88  B-PROC-ARGUS-PROC10
066400                   VALUE '08H005Z' '08H105Z'.
066500             88  B-PROC-KCENTRA-PROC10
066600                   VALUE '30283B1'.
066700             88  B-PROC-ZILVER-PROC10
066800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
066900                         '047L34Z' '047L44Z'.
067000             88  B-PROC-CARDIO-PROC10
067100                   VALUE '02HQ30Z' '02HR30Z'.
067200             88  B-PROC-MITRACLP-PROC10
067300                   VALUE '02UG3JZ'.
067400             88  B-PROC-RNSSYS1-PROC10
067500                   VALUE '0NH00NZ'.
067600             88  B-PROC-RNSSYS2-PROC10
067700                   VALUE '00H00MZ'.
067800             88  B-PROC-BLINATU-PROC10
067900                   VALUE 'XW03351' 'XW04351'.
068000             88  B-PROC-LUTONIX-PROC10
068100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
068200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
068300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
068400                         '047L341' '047L3D1' '047L3Z1' '047L441'
068500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
068600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
068700                         '047M441' '047M4D1' '047M4Z1' '047N041'
068800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
068900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
069000         10  B-OTHER-PROC-CODE11    PIC X(07).
069100             88  B-PROC-ISLET-PROC11
069200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
069300                         '3E0J8U1'.
069400             88  B-PROC-ZENITH-PROC11
069500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
069600             88  B-PROC-VORAXAZE-PROC11
069700                   VALUE '3E033GQ' '3E043GQ'.
069800             88  B-PROC-ARGUS-PROC11
069900                   VALUE '08H005Z' '08H105Z'.
070000             88  B-PROC-KCENTRA-PROC11
070100                   VALUE '30283B1'.
070200             88  B-PROC-ZILVER-PROC11
070300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
070400                         '047L34Z' '047L44Z'.
070500             88  B-PROC-CARDIO-PROC11
070600                   VALUE '02HQ30Z' '02HR30Z'.
070700             88  B-PROC-MITRACLP-PROC11
070800                   VALUE '02UG3JZ'.
070900             88  B-PROC-RNSSYS1-PROC11
071000                   VALUE '0NH00NZ'.
071100             88  B-PROC-RNSSYS2-PROC11
071200                   VALUE '00H00MZ'.
071300             88  B-PROC-BLINATU-PROC11
071400                   VALUE 'XW03351' 'XW04351'.
071500             88  B-PROC-LUTONIX-PROC11
071600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
071700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
071800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
071900                         '047L341' '047L3D1' '047L3Z1' '047L441'
072000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
072100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
072200                         '047M441' '047M4D1' '047M4Z1' '047N041'
072300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
072400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
072500         10  B-OTHER-PROC-CODE12    PIC X(07).
072600             88  B-PROC-ISLET-PROC12
072700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
072800                         '3E0J8U1'.
072900             88  B-PROC-ZENITH-PROC12
073000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
073100             88  B-PROC-VORAXAZE-PROC12
073200                   VALUE '3E033GQ' '3E043GQ'.
073300             88  B-PROC-ARGUS-PROC12
073400                   VALUE '08H005Z' '08H105Z'.
073500             88  B-PROC-KCENTRA-PROC12
073600                   VALUE '30283B1'.
073700             88  B-PROC-ZILVER-PROC12
073800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
073900                         '047L34Z' '047L44Z'.
074000             88  B-PROC-CARDIO-PROC12
074100                   VALUE '02HQ30Z' '02HR30Z'.
074200             88  B-PROC-MITRACLP-PROC12
074300                   VALUE '02UG3JZ'.
074400             88  B-PROC-RNSSYS1-PROC12
074500                   VALUE '0NH00NZ'.
074600             88  B-PROC-RNSSYS2-PROC12
074700                   VALUE '00H00MZ'.
074800             88  B-PROC-BLINATU-PROC12
074900                   VALUE 'XW03351' 'XW04351'.
075000             88  B-PROC-LUTONIX-PROC12
075100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
075200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
075300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
075400                         '047L341' '047L3D1' '047L3Z1' '047L441'
075500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
075600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
075700                         '047M441' '047M4D1' '047M4Z1' '047N041'
075800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
075900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
076000         10  B-OTHER-PROC-CODE13    PIC X(07).
076100             88  B-PROC-ISLET-PROC13
076200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
076300                         '3E0J8U1'.
076400             88  B-PROC-ZENITH-PROC13
076500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
076600             88  B-PROC-VORAXAZE-PROC13
076700                   VALUE '3E033GQ' '3E043GQ'.
076800             88  B-PROC-ARGUS-PROC13
076900                   VALUE '08H005Z' '08H105Z'.
077000             88  B-PROC-KCENTRA-PROC13
077100                   VALUE '30283B1'.
077200             88  B-PROC-ZILVER-PROC13
077300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
077400                         '047L34Z' '047L44Z'.
077500             88  B-PROC-CARDIO-PROC13
077600                   VALUE '02HQ30Z' '02HR30Z'.
077700             88  B-PROC-MITRACLP-PROC13
077800                   VALUE '02UG3JZ'.
077900             88  B-PROC-RNSSYS1-PROC13
078000                   VALUE '0NH00NZ'.
078100             88  B-PROC-RNSSYS2-PROC13
078200                   VALUE '00H00MZ'.
078300             88  B-PROC-BLINATU-PROC13
078400                   VALUE 'XW03351' 'XW04351'.
078500             88  B-PROC-LUTONIX-PROC13
078600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
078700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
078800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
078900                         '047L341' '047L3D1' '047L3Z1' '047L441'
079000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
079100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
079200                         '047M441' '047M4D1' '047M4Z1' '047N041'
079300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
079400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
079500         10  B-OTHER-PROC-CODE14    PIC X(07).
079600             88  B-PROC-ISLET-PROC14
079700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
079800                         '3E0J8U1'.
079900             88  B-PROC-ZENITH-PROC14
080000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
080100             88  B-PROC-VORAXAZE-PROC14
080200                   VALUE '3E033GQ' '3E043GQ'.
080300             88  B-PROC-ARGUS-PROC14
080400                   VALUE '08H005Z' '08H105Z'.
080500             88  B-PROC-KCENTRA-PROC14
080600                   VALUE '30283B1'.
080700             88  B-PROC-ZILVER-PROC14
080800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
080900                         '047L34Z' '047L44Z'.
081000             88  B-PROC-CARDIO-PROC14
081100                   VALUE '02HQ30Z' '02HR30Z'.
081200             88  B-PROC-MITRACLP-PROC14
081300                   VALUE '02UG3JZ'.
081400             88  B-PROC-RNSSYS1-PROC14
081500                   VALUE '0NH00NZ'.
081600             88  B-PROC-RNSSYS2-PROC14
081700                   VALUE '00H00MZ'.
081800             88  B-PROC-BLINATU-PROC14
081900                   VALUE 'XW03351' 'XW04351'.
082000             88  B-PROC-LUTONIX-PROC14
082100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
082200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
082300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
082400                         '047L341' '047L3D1' '047L3Z1' '047L441'
082500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
082600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
082700                         '047M441' '047M4D1' '047M4Z1' '047N041'
082800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
082900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
083000         10  B-OTHER-PROC-CODE15    PIC X(07).
083100             88  B-PROC-ISLET-PROC15
083200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
083300                         '3E0J8U1'.
083400             88  B-PROC-ZENITH-PROC15
083500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
083600             88  B-PROC-VORAXAZE-PROC15
083700                   VALUE '3E033GQ' '3E043GQ'.
083800             88  B-PROC-ARGUS-PROC15
083900                   VALUE '08H005Z' '08H105Z'.
084000             88  B-PROC-KCENTRA-PROC15
084100                   VALUE '30283B1'.
084200             88  B-PROC-ZILVER-PROC15
084300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
084400                         '047L34Z' '047L44Z'.
084500             88  B-PROC-CARDIO-PROC15
084600                   VALUE '02HQ30Z' '02HR30Z'.
084700             88  B-PROC-MITRACLP-PROC15
084800                   VALUE '02UG3JZ'.
084900             88  B-PROC-RNSSYS1-PROC15
085000                   VALUE '0NH00NZ'.
085100             88  B-PROC-RNSSYS2-PROC15
085200                   VALUE '00H00MZ'.
085300             88  B-PROC-BLINATU-PROC15
085400                   VALUE 'XW03351' 'XW04351'.
085500             88  B-PROC-LUTONIX-PROC15
085600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
085700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
085800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
085900                         '047L341' '047L3D1' '047L3Z1' '047L441'
086000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
086100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
086200                         '047M441' '047M4D1' '047M4Z1' '047N041'
086300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
086400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
086500         10  B-OTHER-PROC-CODE16    PIC X(07).
086600             88  B-PROC-ISLET-PROC16
086700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
086800                         '3E0J8U1'.
086900             88  B-PROC-ZENITH-PROC16
087000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
087100             88  B-PROC-VORAXAZE-PROC16
087200                   VALUE '3E033GQ' '3E043GQ'.
087300             88  B-PROC-ARGUS-PROC16
087400                   VALUE '08H005Z' '08H105Z'.
087500             88  B-PROC-KCENTRA-PROC16
087600                   VALUE '30283B1'.
087700             88  B-PROC-ZILVER-PROC16
087800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
087900                         '047L34Z' '047L44Z'.
088000             88  B-PROC-CARDIO-PROC16
088100                   VALUE '02HQ30Z' '02HR30Z'.
088200             88  B-PROC-MITRACLP-PROC16
088300                   VALUE '02UG3JZ'.
088400             88  B-PROC-RNSSYS1-PROC16
088500                   VALUE '0NH00NZ'.
088600             88  B-PROC-RNSSYS2-PROC16
088700                   VALUE '00H00MZ'.
088800             88  B-PROC-BLINATU-PROC16
088900                   VALUE 'XW03351' 'XW04351'.
089000             88  B-PROC-LUTONIX-PROC16
089100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
089200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
089300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
089400                         '047L341' '047L3D1' '047L3Z1' '047L441'
089500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
089600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
089700                         '047M441' '047M4D1' '047M4Z1' '047N041'
089800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
089900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
090000         10  B-OTHER-PROC-CODE17    PIC X(07).
090100             88  B-PROC-ISLET-PROC17
090200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
090300                         '3E0J8U1'.
090400             88  B-PROC-ZENITH-PROC17
090500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
090600             88  B-PROC-VORAXAZE-PROC17
090700                   VALUE '3E033GQ' '3E043GQ'.
090800             88  B-PROC-ARGUS-PROC17
090900                   VALUE '08H005Z' '08H105Z'.
091000             88  B-PROC-KCENTRA-PROC17
091100                   VALUE '30283B1'.
091200             88  B-PROC-ZILVER-PROC17
091300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
091400                         '047L34Z' '047L44Z'.
091500             88  B-PROC-CARDIO-PROC17
091600                   VALUE '02HQ30Z' '02HR30Z'.
091700             88  B-PROC-MITRACLP-PROC17
091800                   VALUE '02UG3JZ'.
091900             88  B-PROC-RNSSYS1-PROC17
092000                   VALUE '0NH00NZ'.
092100             88  B-PROC-RNSSYS2-PROC17
092200                   VALUE '00H00MZ'.
092300             88  B-PROC-BLINATU-PROC17
092400                   VALUE 'XW03351' 'XW04351'.
092500             88  B-PROC-LUTONIX-PROC17
092600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
092700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
092800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
092900                         '047L341' '047L3D1' '047L3Z1' '047L441'
093000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
093100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
093200                         '047M441' '047M4D1' '047M4Z1' '047N041'
093300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
093400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
093500         10  B-OTHER-PROC-CODE18    PIC X(07).
093600             88  B-PROC-ISLET-PROC18
093700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
093800                         '3E0J8U1'.
093900             88  B-PROC-ZENITH-PROC18
094000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
094100             88  B-PROC-VORAXAZE-PROC18
094200                   VALUE '3E033GQ' '3E043GQ'.
094300             88  B-PROC-ARGUS-PROC18
094400                   VALUE '08H005Z' '08H105Z'.
094500             88  B-PROC-KCENTRA-PROC18
094600                   VALUE '30283B1'.
094700             88  B-PROC-ZILVER-PROC18
094800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
094900                         '047L34Z' '047L44Z'.
095000             88  B-PROC-CARDIO-PROC18
095100                   VALUE '02HQ30Z' '02HR30Z'.
095200             88  B-PROC-MITRACLP-PROC18
095300                   VALUE '02UG3JZ'.
095400             88  B-PROC-RNSSYS1-PROC18
095500                   VALUE '0NH00NZ'.
095600             88  B-PROC-RNSSYS2-PROC18
095700                   VALUE '00H00MZ'.
095800             88  B-PROC-BLINATU-PROC18
095900                   VALUE 'XW03351' 'XW04351'.
096000             88  B-PROC-LUTONIX-PROC18
096100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
096200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
096300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
096400                         '047L341' '047L3D1' '047L3Z1' '047L441'
096500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
096600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
096700                         '047M441' '047M4D1' '047M4Z1' '047N041'
096800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
096900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
097000         10  B-OTHER-PROC-CODE19    PIC X(07).
097100             88  B-PROC-ISLET-PROC19
097200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
097300                         '3E0J8U1'.
097400             88  B-PROC-ZENITH-PROC19
097500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
097600             88  B-PROC-VORAXAZE-PROC19
097700                   VALUE '3E033GQ' '3E043GQ'.
097800             88  B-PROC-ARGUS-PROC19
097900                   VALUE '08H005Z' '08H105Z'.
098000             88  B-PROC-KCENTRA-PROC19
098100                   VALUE '30283B1'.
098200             88  B-PROC-ZILVER-PROC19
098300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
098400                         '047L34Z' '047L44Z'.
098500             88  B-PROC-CARDIO-PROC19
098600                   VALUE '02HQ30Z' '02HR30Z'.
098700             88  B-PROC-MITRACLP-PROC19
098800                   VALUE '02UG3JZ'.
098900             88  B-PROC-RNSSYS1-PROC19
099000                   VALUE '0NH00NZ'.
099100             88  B-PROC-RNSSYS2-PROC19
099200                   VALUE '00H00MZ'.
099300             88  B-PROC-BLINATU-PROC19
099400                   VALUE 'XW03351' 'XW04351'.
099500             88  B-PROC-LUTONIX-PROC19
099600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
099700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
099800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
099900                         '047L341' '047L3D1' '047L3Z1' '047L441'
100000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
100100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
100200                         '047M441' '047M4D1' '047M4Z1' '047N041'
100300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
100400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
100500         10  B-OTHER-PROC-CODE20    PIC X(07).
100600             88  B-PROC-ISLET-PROC20
100700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
100800                         '3E0J8U1'.
100900             88  B-PROC-ZENITH-PROC20
101000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
101100             88  B-PROC-VORAXAZE-PROC20
101200                   VALUE '3E033GQ' '3E043GQ'.
101300             88  B-PROC-ARGUS-PROC20
101400                   VALUE '08H005Z' '08H105Z'.
101500             88  B-PROC-KCENTRA-PROC20
101600                   VALUE '30283B1'.
101700             88  B-PROC-ZILVER-PROC20
101800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
101900                         '047L34Z' '047L44Z'.
102000             88  B-PROC-CARDIO-PROC20
102100                   VALUE '02HQ30Z' '02HR30Z'.
102200             88  B-PROC-MITRACLP-PROC20
102300                   VALUE '02UG3JZ'.
102400             88  B-PROC-RNSSYS1-PROC20
102500                   VALUE '0NH00NZ'.
102600             88  B-PROC-RNSSYS2-PROC20
102700                   VALUE '00H00MZ'.
102800             88  B-PROC-BLINATU-PROC20
102900                   VALUE 'XW03351' 'XW04351'.
103000             88  B-PROC-LUTONIX-PROC20
103100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
103200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
103300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
103400                         '047L341' '047L3D1' '047L3Z1' '047L441'
103500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
103600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
103700                         '047M441' '047M4D1' '047M4Z1' '047N041'
103800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
103900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
104000         10  B-OTHER-PROC-CODE21    PIC X(07).
104100             88  B-PROC-ISLET-PROC21
104200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
104300                         '3E0J8U1'.
104400             88  B-PROC-ZENITH-PROC21
104500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
104600             88  B-PROC-VORAXAZE-PROC21
104700                   VALUE '3E033GQ' '3E043GQ'.
104800             88  B-PROC-ARGUS-PROC21
104900                   VALUE '08H005Z' '08H105Z'.
105000             88  B-PROC-KCENTRA-PROC21
105100                   VALUE '30283B1'.
105200             88  B-PROC-ZILVER-PROC21
105300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
105400                         '047L34Z' '047L44Z'.
105500             88  B-PROC-CARDIO-PROC21
105600                   VALUE '02HQ30Z' '02HR30Z'.
105700             88  B-PROC-MITRACLP-PROC21
105800                   VALUE '02UG3JZ'.
105900             88  B-PROC-RNSSYS1-PROC21
106000                   VALUE '0NH00NZ'.
106100             88  B-PROC-RNSSYS2-PROC21
106200                   VALUE '00H00MZ'.
106300             88  B-PROC-BLINATU-PROC21
106400                   VALUE 'XW03351' 'XW04351'.
106500             88  B-PROC-LUTONIX-PROC21
106600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
106700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
106800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
106900                         '047L341' '047L3D1' '047L3Z1' '047L441'
107000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
107100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
107200                         '047M441' '047M4D1' '047M4Z1' '047N041'
107300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
107400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
107500         10  B-OTHER-PROC-CODE22    PIC X(07).
107600             88  B-PROC-ISLET-PROC22
107700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
107800                         '3E0J8U1'.
107900             88  B-PROC-ZENITH-PROC22
108000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
108100             88  B-PROC-VORAXAZE-PROC22
108200                   VALUE '3E033GQ' '3E043GQ'.
108300             88  B-PROC-ARGUS-PROC22
108400                   VALUE '08H005Z' '08H105Z'.
108500             88  B-PROC-KCENTRA-PROC22
108600                   VALUE '30283B1'.
108700             88  B-PROC-ZILVER-PROC22
108800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
108900                         '047L34Z' '047L44Z'.
109000             88  B-PROC-CARDIO-PROC22
109100                   VALUE '02HQ30Z' '02HR30Z'.
109200             88  B-PROC-MITRACLP-PROC22
109300                   VALUE '02UG3JZ'.
109400             88  B-PROC-RNSSYS1-PROC22
109500                   VALUE '0NH00NZ'.
109600             88  B-PROC-RNSSYS2-PROC22
109700                   VALUE '00H00MZ'.
109800             88  B-PROC-BLINATU-PROC22
109900                   VALUE 'XW03351' 'XW04351'.
110000             88  B-PROC-LUTONIX-PROC22
110100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
110200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
110300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
110400                         '047L341' '047L3D1' '047L3Z1' '047L441'
110500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
110600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
110700                         '047M441' '047M4D1' '047M4Z1' '047N041'
110800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
110900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
111000         10  B-OTHER-PROC-CODE23    PIC X(07).
111100             88  B-PROC-ISLET-PROC23
111200                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
111300                         '3E0J8U1'.
111400             88  B-PROC-ZENITH-PROC23
111500                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
111600             88  B-PROC-VORAXAZE-PROC23
111700                   VALUE '3E033GQ' '3E043GQ'.
111800             88  B-PROC-ARGUS-PROC23
111900                   VALUE '08H005Z' '08H105Z'.
112000             88  B-PROC-KCENTRA-PROC23
112100                   VALUE '30283B1'.
112200             88  B-PROC-ZILVER-PROC23
112300                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
112400                         '047L34Z' '047L44Z'.
112500             88  B-PROC-CARDIO-PROC23
112600                   VALUE '02HQ30Z' '02HR30Z'.
112700             88  B-PROC-MITRACLP-PROC23
112800                   VALUE '02UG3JZ'.
112900             88  B-PROC-RNSSYS1-PROC23
113000                   VALUE '0NH00NZ'.
113100             88  B-PROC-RNSSYS2-PROC23
113200                   VALUE '00H00MZ'.
113300             88  B-PROC-BLINATU-PROC23
113400                   VALUE 'XW03351' 'XW04351'.
113500             88  B-PROC-LUTONIX-PROC23
113600                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
113700                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
113800                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
113900                         '047L341' '047L3D1' '047L3Z1' '047L441'
114000                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
114100                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
114200                         '047M441' '047M4D1' '047M4Z1' '047N041'
114300                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
114400                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
114500         10  B-OTHER-PROC-CODE24    PIC X(07).
114600             88  B-PROC-ISLET-PROC24
114700                   VALUE '3E030U1' '3E033U1' '3E0J3U1' '3E0J7U1'
114800                         '3E0J8U1'.
114900             88  B-PROC-ZENITH-PROC24
115000                   VALUE '04U03JZ' '04U04JZ' '04V03DZ' '04V04DZ'.
115100             88  B-PROC-VORAXAZE-PROC24
115200                   VALUE '3E033GQ' '3E043GQ'.
115300             88  B-PROC-ARGUS-PROC24
115400                   VALUE '08H005Z' '08H105Z'.
115500             88  B-PROC-KCENTRA-PROC24
115600                   VALUE '30283B1'.
115700             88  B-PROC-ZILVER-PROC24
115800                   VALUE '047K04Z' '047K34Z' '047K44Z' '047L04Z'
115900                         '047L34Z' '047L44Z'.
116000             88  B-PROC-CARDIO-PROC24
116100                   VALUE '02HQ30Z' '02HR30Z'.
116200             88  B-PROC-MITRACLP-PROC24
116300                   VALUE '02UG3JZ'.
116400             88  B-PROC-RNSSYS1-PROC24
116500                   VALUE '0NH00NZ'.
116600             88  B-PROC-RNSSYS2-PROC24
116700                   VALUE '00H00MZ'.
116800             88  B-PROC-BLINATU-PROC24
116900                   VALUE 'XW03351' 'XW04351'.
117000             88  B-PROC-LUTONIX-PROC24
117100                   VALUE '047K041' '047K0D1' '047K0Z1' '047K341'
117200                         '047K3D1' '047K3Z1' '047K441' '047K4D1'
117300                         '047K4Z1' '047L041' '047L0D1' '047L0Z1'
117400                         '047L341' '047L3D1' '047L3Z1' '047L441'
117500                         '047L4D1' '047L4Z1' '047M041' '047M0D1'
117600                         '047M0Z1' '047M341' '047M3D1' '047M3Z1'
117700                         '047M441' '047M4D1' '047M4Z1' '047N041'
117800                         '047N0D1' '047N0Z1' '047N341' '047N3D1'
117900                         '047N3Z1' '047N441' '047N4D1' '047N4Z1'.
118000         10  B-OTHER-DIAG-CODE1     PIC X(07).
118100             88  B-DIAG-ISLET-DIAG1
118200                   VALUE 'Z006   '.
118300             88  B-DIAG-AUTOLITT-DIAG
118400                   VALUE '1910   ' '1911   ' '1912   ' '1913  '
118500                         '1914   ' '1915   ' '1916   ' '1917  '
118600                         '1918   ' '1919   ' 'C710   ' 'C711  '
118700                         'C712   ' 'C713   ' 'C714   ' 'C715  '
118800                         'C716   ' 'C717   ' 'C718   ' 'C719  '.
118900             88  B-DIAG-KCENTRA-DIAG1
119000                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
119100                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
119200                         'D6832  ' 'D684   '.
119300         10  B-OTHER-DIAG-CODE2     PIC X(07).
119400             88  B-DIAG-ISLET-DIAG2
119500                   VALUE 'Z006   '.
119600             88  B-DIAG-KCENTRA-DIAG2
119700                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
119800                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
119900                         'D6832  ' 'D684   '.
120000         10  B-OTHER-DIAG-CODE3     PIC X(07).
120100             88  B-DIAG-ISLET-DIAG3
120200                   VALUE 'Z006   '.
120300             88  B-DIAG-KCENTRA-DIAG3
120400                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
120500                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
120600                         'D6832  ' 'D684   '.
120700         10  B-OTHER-DIAG-CODE4     PIC X(07).
120800             88  B-DIAG-ISLET-DIAG4
120900                   VALUE 'Z006   '.
121000             88  B-DIAG-KCENTRA-DIAG4
121100                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
121200                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
121300                         'D6832  ' 'D684   '.
121400         10  B-OTHER-DIAG-CODE5     PIC X(07).
121500             88  B-DIAG-ISLET-DIAG5
121600                   VALUE 'Z006   '.
121700             88  B-DIAG-KCENTRA-DIAG5
121800                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
121900                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
122000                         'D6832  ' 'D684   '.
122100         10  B-OTHER-DIAG-CODE6     PIC X(07).
122200             88  B-DIAG-ISLET-DIAG6
122300                   VALUE 'Z006   '.
122400             88  B-DIAG-KCENTRA-DIAG6
122500                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
122600                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
122700                         'D6832  ' 'D684   '.
122800         10  B-OTHER-DIAG-CODE7     PIC X(07).
122900             88  B-DIAG-ISLET-DIAG7
123000                   VALUE 'Z006   '.
123100             88  B-DIAG-KCENTRA-DIAG7
123200                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
123300                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
123400                         'D6832  ' 'D684   '.
123500         10  B-OTHER-DIAG-CODE8     PIC X(07).
123600             88  B-DIAG-ISLET-DIAG8
123700                   VALUE 'Z006   '.
123800             88  B-DIAG-KCENTRA-DIAG8
123900                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
124000                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
124100                         'D6832  ' 'D684   '.
124200         10  B-OTHER-DIAG-CODE9     PIC X(07).
124300             88  B-DIAG-ISLET-DIAG9
124400                   VALUE 'Z006   '.
124500             88  B-DIAG-KCENTRA-DIAG9
124600                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
124700                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
124800                         'D6832  ' 'D684   '.
124900         10  B-OTHER-DIAG-CODE10    PIC X(07).
125000             88  B-DIAG-ISLET-DIAG10
125100                   VALUE 'Z006   '.
125200             88  B-DIAG-KCENTRA-DIAG10
125300                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
125400                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
125500                         'D6832  ' 'D684   '.
125600         10  B-OTHER-DIAG-CODE11    PIC X(07).
125700             88  B-DIAG-ISLET-DIAG11
125800                   VALUE 'Z006   '.
125900             88  B-DIAG-KCENTRA-DIAG11
126000                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
126100                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
126200                         'D6832  ' 'D684   '.
126300         10  B-OTHER-DIAG-CODE12    PIC X(07).
126400             88  B-DIAG-ISLET-DIAG12
126500                   VALUE 'Z006   '.
126600             88  B-DIAG-KCENTRA-DIAG12
126700                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
126800                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
126900                         'D6832  ' 'D684   '.
127000         10  B-OTHER-DIAG-CODE13    PIC X(07).
127100             88  B-DIAG-ISLET-DIAG13
127200                   VALUE 'Z006   '.
127300             88  B-DIAG-KCENTRA-DIAG13
127400                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
127500                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
127600                         'D6832  ' 'D684   '.
127700         10  B-OTHER-DIAG-CODE14    PIC X(07).
127800             88  B-DIAG-ISLET-DIAG14
127900                   VALUE 'Z006   '.
128000             88  B-DIAG-KCENTRA-DIAG14
128100                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
128200                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
128300                         'D6832  ' 'D684   '.
128400         10  B-OTHER-DIAG-CODE15    PIC X(07).
128500             88  B-DIAG-ISLET-DIAG15
128600                   VALUE 'Z006   '.
128700             88  B-DIAG-KCENTRA-DIAG15
128800                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
128900                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
129000                         'D6832  ' 'D684   '.
129100         10  B-OTHER-DIAG-CODE16    PIC X(07).
129200             88  B-DIAG-ISLET-DIAG16
129300                   VALUE 'Z006   '.
129400             88  B-DIAG-KCENTRA-DIAG16
129500                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
129600                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
129700                         'D6832  ' 'D684   '.
129800         10  B-OTHER-DIAG-CODE17    PIC X(07).
129900             88  B-DIAG-ISLET-DIAG17
130000                   VALUE 'Z006   '.
130100             88  B-DIAG-KCENTRA-DIAG17
130200                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
130300                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
130400                         'D6832  ' 'D684   '.
130500         10  B-OTHER-DIAG-CODE18    PIC X(07).
130600             88  B-DIAG-ISLET-DIAG18
130700                   VALUE 'Z006   '.
130800             88  B-DIAG-KCENTRA-DIAG18
130900                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
131000                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
131100                         'D6832  ' 'D684   '.
131200         10  B-OTHER-DIAG-CODE19    PIC X(07).
131300             88  B-DIAG-ISLET-DIAG19
131400                   VALUE 'Z006   '.
131500             88  B-DIAG-KCENTRA-DIAG19
131600                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
131700                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
131800                         'D6832  ' 'D684   '.
131900         10  B-OTHER-DIAG-CODE20    PIC X(07).
132000             88  B-DIAG-ISLET-DIAG20
132100                   VALUE 'Z006   '.
132200             88  B-DIAG-KCENTRA-DIAG20
132300                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
132400                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
132500                         'D6832  ' 'D684   '.
132600         10  B-OTHER-DIAG-CODE21    PIC X(07).
132700             88  B-DIAG-ISLET-DIAG21
132800                   VALUE 'Z006   '.
132900             88  B-DIAG-KCENTRA-DIAG21
133000                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
133100                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
133200                         'D6832  ' 'D684   '.
133300         10  B-OTHER-DIAG-CODE22    PIC X(07).
133400             88  B-DIAG-ISLET-DIAG22
133500                   VALUE 'Z006   '.
133600             88  B-DIAG-KCENTRA-DIAG22
133700                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
133800                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
133900                         'D6832  ' 'D684   '.
134000         10  B-OTHER-DIAG-CODE23    PIC X(07).
134100             88  B-DIAG-ISLET-DIAG23
134200                   VALUE 'Z006   '.
134300             88  B-DIAG-KCENTRA-DIAG23
134400                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
134500                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
134600                         'D6832  ' 'D684   '.
134700         10  B-OTHER-DIAG-CODE24    PIC X(07).
134800             88  B-DIAG-ISLET-DIAG24
134900                   VALUE 'Z006   '.
135000             88  B-DIAG-KCENTRA-DIAG24
135100                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
135200                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
135300                         'D6832  ' 'D684   '.
135400         10  B-OTHER-DIAG-CODE25    PIC X(07).
135500             88  B-DIAG-ISLET-DIAG25
135600                   VALUE 'Z006   '.
135700             88  B-DIAG-KCENTRA-DIAG25
135800                   VALUE 'D66    ' 'D67    ' 'D681   ' 'D682   '
135900                         'D680   ' 'D68311 ' 'D68312 ' 'D68318 '
136000                         'D6832  ' 'D684   '.
136100         10  B-DEMO-DATA.
136200             15  B-DEMO-CODE1           PIC X(02).
136300             15  B-DEMO-CODE2           PIC X(02).
136400             15  B-DEMO-CODE3           PIC X(02).
136500             15  B-DEMO-CODE4           PIC X(02).
136600         10  B-NDC-DATA.
136700             15  B-NDC-NUMBER           PIC X(11).
136800               88  B-NDC-DIFICID-NDC
136900                   VALUE '52015008001'.
137000         10  FILLER                     PIC X(73).
137100
137200
137300***************************************************************
137400*    THIS DATA IS CALCULATED BY THIS PPCAL  SUBROUTINE        *
137500*    AND PASSED BACK TO THE CALLING PROGRAM                   *
137600*            RETURN CODE VALUES (PPS-RTC)                     *
137700*                                                             *
137800*            PPS-RTC 00-49 = HOW THE BILL WAS PAID            *
137900*                                                             *
138000*      PPS-RTC 30,33,40,42,44  = OUTLIER RECONCILIATION       *
138100*                                                             *
138200*           30,00 = PAID NORMAL DRG PAYMENT                   *
138300*                                                             *
138400*              01 = PAID AS A DAY-OUTLIER.                    *
138500*                   NOTE:                                     *
138600*                     DAY-OUTLIER NO LONGER BEING PAID        *
138700*                         AS OF 10/01/97                      *
138800*                                                             *
138900*              02 = PAID AS A COST-OUTLIER.                   *
139000*                                                             *
139100*           33,03 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
139200*                   AND INCLUDING THE FULL DRG.               *
139300*              05 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
139400*                   AND INCLUDING THE FULL DRG WHICH ALSO     *
139500*                   QUALIFIED FOR A COST OUTLIER PAYMENT.     *
139600*              06 = TRANSFER PAID ON A PERDIEM BASIS UP TO    *
139700*                   AND INCLUDING THE FULL DRG. PROVIDER      *
139800*                   REFUSED COST OUTLIER.                     *
139900*           40,10 = POST-ACUTE TRANSFER                       *
140000*                   SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
140100*
140200* =======================================================
140300* THE 50/50 DRG'S DO NOT REPEAT WITH THE POSTACUTE  DRG'S
140400* =======================================================
140500*
140600*           42,12 = POST-ACAUTE TRANSFER WITH SPECIFIC DRGS   *
140700*                       THE FOLLOWING DRG'S                   *
140800*                   DRG =  VALUE  NOW USES Y INDICATORS ON DRGS
140900*                       SEE TABLE 5 FROM ANNUAL IPPS FINAL RULE
141000*                          D-DRG-POSTACUTE-PERDIEM
141100*
141200*           44,14 = PAID NORMAL DRG PAYMENT WITH              *
141300*                    PERDIEM DAYS = OR > GM  ALOS             *
141400*              16 = PAID AS A COST-OUTLIER WITH               *
141500*                    PERDIEM DAYS = OR > GM  ALOS             *
141600*                                                             *
141700*            PPS-RTC 50-99 = WHY THE BILL WAS NOT PAID        *
141800*              51 = NO PROVIDER SPECIFIC INFO FOUND           *
141900*              52 = INVALID CBSA# IN PROVIDER FILE            *
142000*                   OR INVALID WAGE INDEX                     *
142100*                                      OR                     *
142200*                   INVALID PROVIDER TYPES ON PROVIDER FILE   *
142300*              53 = WAIVER STATE - NOT CALCULATED BY PPS OR   *
142400*                   OR                                         *
142500*                   INVALID STATE CODE IN COMBINATION WITH     *
142600*                   HAC FLAG                                  *
142700*              54 = INVALID DRG                               *
142800*              55 = DISCHARGE DATE < PROVIDER EFF START DATE  *
142900*                                      OR                     *
143000*                   DISCHARGE DATE < CBSA EFF START DATE      *
143100*                   FOR PPS                                   *
143200*                                      OR                     *
143300*                   PROVIDER HAS BEEN TERMINATED ON OR BEFORE *
143400*                   DISCHARGE DATE                            *
143500*              56 = INVALID LENGTH OF STAY                    *
143600*              57 = REVIEW CODE INVALID (NOT 00 03 06 07 09   *
143700*                                        NOT 11)              *
143800*              58 = TOTAL CHARGES NOT NUMERIC                 *
143900*              61 = LIFETIME RESERVE DAYS NOT NUMERIC         *
144000*                   OR BILL-LTR-DAYS > 60                     *
144100*              62 = INVALID NUMBER OF COVERED DAYS            *
144200*              65 = PAY-CODE NOT = A,B OR C ON PROVIDER        *
144300*                   SPECIFIC FILE FOR CAPITAL                  *
144400*                   OR                                         *
144500*                   INVALID READMISSION FLAG IN PSF FILE       *
144600*                   OR                                         *
144700*                   BLANK READMISSION FLAG IN PSF FILE         *
144800*                   OR                                         *
144900*                   READMISSION ADJUSTMENT IS INVALID OR       *
145000*                   OUT OF RANGE IN PSF FILE                   *
145100*                   OR                                         *
145200*                   BLANK READMISSION ADJUSTMENT IN PSF FILE   *
145300*                   OR                                         *
145400*                   INVALID STATE CODE IN COMBINATION WITH     *
145500*                   READMISSION FLAG IN PSF FILE               *
145600*                   OR                                         *
145700*                   INVALID EHR FLAG IN PSF FILE               *
145800*                   (MUST BE A "Y" OR BLANK)                   *
145900*              67 = COST OUTLIER WITH LOS > COVERED DAYS      **
146000*                   OR COST OUTLIER THRESHOLD CALUCULATION    **
146100*              68 = INVALID VALUE BASED PURCHASE FLAG IN PSF   *
146200*                   FILE                                       *
146300*                   OR                                         *
146400*                   BLANK VALUE BASED PURCHASE FLAG IN PSF FILE*
146500*                   OR                                         *
146600*                   VALUE BASED PURCHASE ADJUSTMEMT IS INVALID *
146700*                   OR OUT OF RANGE IN PSF FILE                *
146800*                   INDICATOR                                  *
146900*                   OR                                         *
147000*                   BLANK VALUE BASED PURCHASE ADJUSTMEMT IN   *
147100*                   PSF FILE                                   *
147200*                   OR                                         *
147300*                   INVALID COMBINATION OF HOSPITAL QUALITY    *
147400*                   INDICATOR                                  *
147500*                   AND VALUE BASED PURCHASE FLAG IN PSF FILE  *
147600*                   OR                                         *
147700*                   INVALID STATE CODE IN COMBINATION WITH VALUE
147800*                   BASED PURCHASE FLAG IN PSF FILE            *
147900*              98 = CANNOT PROCESS BILL OLDER THAN 5 YEARS    *
148000***************************************************************
148100 01  PPS-DATA.
148200         10  PPS-RTC                PIC 9(02).
148300         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
148400         10  PPS-OUTLIER-DAYS       PIC 9(03).
148500         10  PPS-AVG-LOS            PIC 9(02)V9(01).
148600         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
148700         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
148800         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
148900         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
149000         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
149100         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
149200         10  PPS-REG-DAYS-USED      PIC 9(03).
149300         10  PPS-LTR-DAYS-USED      PIC 9(02).
149400         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
149500         10  PPS-CALC-VERS          PIC X(05).
149600
149700*****************************************************************
149800*            THESE ARE THE VERSIONS OF THE PPCAL
149900*           PROGRAMS THAT WILL BE PASSED BACK----
150000*          ASSOCIATED WITH THE BILL BEING PROCESSED
150100*****************************************************************
150200 01  PRICER-OPT-VERS-SW.
150300     02  PRICER-OPTION-SW          PIC X(01).
150400         88  ALL-TABLES-PASSED          VALUE 'A'.
150500         88  PROV-RECORD-PASSED         VALUE 'P'.
150600         88  ADDITIONAL-VARIABLES       VALUE 'M'.
150700         88  PC-PRICER                  VALUE 'C'.
150800     02  PPS-VERSIONS.
150900         10  PPDRV-VERSION         PIC X(05).
151000
151100*****************************************************************
151200*        THIS IS THE VARIABLES THAT WILL BE PASSED BACK
151300*          ASSOCIATED WITH THE BILL BEING PROCESSED
151400*****************************************************************
151500 01  PPS-ADDITIONAL-VARIABLES.
151600     05  PPS-HSP-PCT                PIC 9(01)V9(02).
151700     05  PPS-FSP-PCT                PIC 9(01)V9(02).
151800     05  PPS-NAT-PCT                PIC 9(01)V9(02).
151900     05  PPS-REG-PCT                PIC 9(01)V9(02).
152000     05  PPS-FAC-SPEC-RATE          PIC 9(05)V9(02).
152100     05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
152200     05  PPS-DRG-WT                 PIC 9(02)V9(04).
152300     05  PPS-NAT-LABOR              PIC 9(05)V9(02).
152400     05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
152500     05  PPS-REG-LABOR              PIC 9(05)V9(02).
152600     05  PPS-REG-NLABOR             PIC 9(05)V9(02).
152700     05  PPS-OPER-COLA              PIC 9(01)V9(03).
152800     05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
152900     05  PPS-COST-OUTLIER           PIC 9(07)V9(09).
153000     05  PPS-BILL-COSTS             PIC 9(07)V9(09).
153100     05  PPS-DOLLAR-THRESHOLD       PIC 9(07)V9(09).
153200     05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
153300     05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
153400     05  PPS-CAPITAL-VARIABLES.
153500         10  PPS-CAPI-TOTAL-PAY           PIC 9(07)V9(02).
153600         10  PPS-CAPI-HSP                 PIC 9(07)V9(02).
153700         10  PPS-CAPI-FSP                 PIC 9(07)V9(02).
153800         10  PPS-CAPI-OUTLIER             PIC 9(07)V9(02).
153900         10  PPS-CAPI-OLD-HARM            PIC 9(07)V9(02).
154000         10  PPS-CAPI-DSH-ADJ             PIC 9(07)V9(02).
154100         10  PPS-CAPI-IME-ADJ             PIC 9(07)V9(02).
154200         10  PPS-CAPI-EXCEPTIONS          PIC 9(07)V9(02).
154300     05  PPS-CAPITAL2-VARIABLES.
154400         10  PPS-CAPI2-PAY-CODE             PIC X(1).
154500         10  PPS-CAPI2-B-FSP                PIC 9(07)V9(02).
154600         10  PPS-CAPI2-B-OUTLIER            PIC 9(07)V9(02).
154700
154800     05  PPS-OTHER-VARIABLES.
154900         10  PPS-NON-TEMP-RELIEF-PAYMENT    PIC 9(07)V9(02).
155000         10  PPS-NEW-TECH-PAY-ADD-ON        PIC 9(07)V9(02).
155100         10  PPS-ISLET-ISOL-PAY-ADD-ON      PIC 9(07)V9(02).
155200         10  PPS-LOW-VOL-PAYMENT            PIC 9(07)V9(02).
155300         10  PPS-VAL-BASED-PURCH-PARTIPNT   PIC X.
155400         10  PPS-VAL-BASED-PURCH-ADJUST     PIC 9V9(11).
155500         10  PPS-HOSP-READMISSION-REDU      PIC X.
155600         10  PPS-HOSP-HRR-ADJUSTMT          PIC 9V9(4).
155700         10  PPS-OPERATNG-DATA.
155800             15  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
155900             15  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
156000             15  PPS-OPER-HSP-AMT            PIC 9(08)V99.
156100
156200     05  PPS-PC-OTH-VARIABLES.
156300         10  PPS-OPER-DSH                   PIC 9(01)V9(04).
156400         10  PPS-CAPI-DSH                   PIC 9(01)V9(04).
156500         10  PPS-CAPI-HSP-PCT               PIC 9(01)V9(02).
156600         10  PPS-CAPI-FSP-PCT               PIC 9(01)V9(04).
156700         10  PPS-ARITH-ALOS                 PIC 9(02)V9(01).
156800         10  PPS-PR-WAGE-INDEX              PIC 9(02)V9(04).
156900         10  PPS-TRANSFER-ADJ               PIC 9(01)V9(04).
157000         10  PPS-PC-HMO-FLAG                PIC X(01).
157100         10  PPS-PC-COT-FLAG                PIC X(01).
157200         10  PPS-OPER-HSP-PART2             PIC 9(07)V9(02).
157300         10  PPS-BUNDLE-ADJUST-PAY          PIC S9(07)V99.
157400
157500     05  PPS-ADDITIONAL-PAY-INFO-DATA.
157600         10 PPS-UNCOMP-CARE-AMOUNT          PIC S9(07)V9(02).
157700         10 PPS-BUNDLE-ADJUST-AMT           PIC S9(07)V9(02).
157800         10 PPS-VAL-BASED-PURCH-ADJUST-AMT  PIC S9(07)V9(02).
157900         10 PPS-READMIS-ADJUST-AMT          PIC S9(07)V9(02).
158000     05  PPS-ADDITIONAL-PAY-INFO-DATA2.
158100         10  PPS-HAC-PROG-REDUC-IND      PIC X.
158200         10  PPS-EHR-PROG-REDUC-IND      PIC X.
158300         10  PPS-EHR-ADJUST-AMT          PIC S9(07)V9(02).
158400         10  PPS-STNDRD-VALUE            PIC S9(07)V9(02).
158500         10  PPS-HAC-PAYMENT-AMT         PIC S9(07)V9(02).
158600         10  PPS-FLX7-PAYMENT            PIC S9(07)V9(02).
158700     05 PPS-FILLER                       PIC X(0906).
158800
158900 01  PROV-NEW-HOLD.
159000     02  PROV-NEWREC-HOLD1.
159100         05  P-NEW-NPI10.
159200             10  P-NEW-NPI8             PIC X(08).
159300             10  P-NEW-NPI-FILLER       PIC X(02).
159400         05  P-NEW-PROVIDER-NO.
159500             88  P-NEW-DSH-ADJ-PROVIDERS
159600                             VALUE '180049' '190044' '190144'
159700                                   '190191' '330047' '340085'
159800                                   '370016' '370149' '420043'.
159900             10  P-NEW-STATE            PIC 9(02).
160000                 88  P-VBP-INVALID-STATE
160100                             VALUE 21 80 40 84.
160200                 88  P-READ-INVALID-STATE
160300                             VALUE 40 84.
160400                 88  P-HAC-INVALID-STATE
160500                             VALUE 40 84.
160600                 88  P-PR-NEW-STATE
160700                             VALUE 40 84.
160800             10  FILLER                 PIC X(04).
160900         05  P-NEW-DATE-DATA.
161000             10  P-NEW-EFF-DATE.
161100                 15  P-NEW-EFF-DT-CC    PIC 9(02).
161200                 15  P-NEW-EFF-DT-YY    PIC 9(02).
161300                 15  P-NEW-EFF-DT-MM    PIC 9(02).
161400                 15  P-NEW-EFF-DT-DD    PIC 9(02).
161500             10  P-NEW-FY-BEGIN-DATE.
161600                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
161700                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
161800                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
161900                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
162000             10  P-NEW-REPORT-DATE.
162100                 15  P-NEW-REPORT-DT-CC PIC 9(02).
162200                 15  P-NEW-REPORT-DT-YY PIC 9(02).
162300                 15  P-NEW-REPORT-DT-MM PIC 9(02).
162400                 15  P-NEW-REPORT-DT-DD PIC 9(02).
162500             10  P-NEW-TERMINATION-DATE.
162600                 15  P-NEW-TERM-DT-CC   PIC 9(02).
162700                 15  P-NEW-TERM-DT-YY   PIC 9(02).
162800                 15  P-NEW-TERM-DT-MM   PIC 9(02).
162900                 15  P-NEW-TERM-DT-DD   PIC 9(02).
163000         05  P-NEW-WAIVER-CODE          PIC X(01).
163100             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
163200         05  P-NEW-INTER-NO             PIC 9(05).
163300         05  P-NEW-PROVIDER-TYPE        PIC X(02).
163400             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
163500             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
163600                                                  '15' '17'
163700                                                  '22'.
163800             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
163900             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
164000             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
164100             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
164200             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
164300             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
164400             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
164500             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
164600             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
164700             88  P-N-EACH                   VALUE '21' '22'.
164800             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
164900             88  P-N-NHCMQ-II-SNF           VALUE '32'.
165000             88  P-N-NHCMQ-III-SNF          VALUE '33'.
165100             88  P-N-INVALID-PROV-TYPES     VALUE '14' '15'.
165200         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
165300             88  P-N-NEW-ENGLAND            VALUE  1.
165400             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
165500             88  P-N-SOUTH-ATLANTIC         VALUE  3.
165600             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
165700             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
165800             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
165900             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
166000             88  P-N-MOUNTAIN               VALUE  8.
166100             88  P-N-PACIFIC                VALUE  9.
166200         05  P-NEW-CURRENT-DIV   REDEFINES
166300                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
166400             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
166500         05  P-NEW-MSA-DATA.
166600             10  P-NEW-CHG-CODE-INDEX       PIC X.
166700             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
166800             10  P-NEW-GEO-LOC-MSA9   REDEFINES
166900                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
167000             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
167100             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
167200             10  P-NEW-STAND-AMT-LOC-MSA9
167300       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
167400                 15  P-NEW-RURAL-1ST.
167500                     20  P-NEW-STAND-RURAL  PIC XX.
167600                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
167700                 15  P-NEW-RURAL-2ND        PIC XX.
167800         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
167900                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
168000                 88  P-NEW-SCH-YR82       VALUE   '82'.
168100                 88  P-NEW-SCH-YR87       VALUE   '87'.
168200         05  P-NEW-LUGAR                    PIC X.
168300         05  P-NEW-TEMP-RELIEF-IND          PIC X.
168400         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
168500         05  FILLER                         PIC X(05).
168600     02  PROV-NEWREC-HOLD2.
168700         05  P-NEW-VARIABLES.
168800             10  P-NEW-FAC-SPEC-RATE     PIC  9(05)V9(02).
168900             10  P-NEW-COLA              PIC  9(01)V9(03).
169000             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
169100             10  P-NEW-BED-SIZE          PIC  9(05).
169200             10  P-NEW-OPER-CSTCHG-RATIO PIC  9(01)V9(03).
169300             10  P-NEW-CMI               PIC  9(01)V9(04).
169400             10  P-NEW-SSI-RATIO         PIC  V9(04).
169500             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
169600             10  P-NEW-PPS-BLEND-YR-IND  PIC  9(01).
169700             10  P-NEW-PRUF-UPDTE-FACTOR PIC  9(01)V9(05).
169800             10  P-NEW-DSH-PERCENT       PIC  V9(04).
169900             10  P-NEW-FYE-DATE          PIC  X(08).
170000         05  P-NEW-CBSA-DATA.
170100             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
170200             10  P-NEW-CBSA-HOSP-QUAL-IND   PIC X.
170300             10  P-NEW-CBSA-GEO-LOC         PIC X(05) JUST RIGHT.
170400             10  P-NEW-CBSA-GEO-RURAL REDEFINES
170500                 P-NEW-CBSA-GEO-LOC.
170600                 15  P-NEW-CBSA-GEO-RURAL1ST PIC XXX.
170700                     88  P-NEW-CBSA-GEO-RURAL1    VALUE '   '.
170800                 15  P-NEW-CBSA-GEO-RURAL2ND PIC XX.
170900
171000             10  P-NEW-CBSA-RECLASS-LOC     PIC X(05) JUST RIGHT.
171100             10  P-NEW-CBSA-STAND-AMT-LOC   PIC X(05) JUST RIGHT.
171200             10  P-NEW-CBSA-SPEC-WAGE-INDEX    PIC 9(02)V9(04).
171300     02  PROV-NEWREC-HOLD3.
171400         05  P-NEW-PASS-AMT-DATA.
171500             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
171600             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
171700             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
171800             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
171900         05  P-NEW-CAPI-DATA.
172000             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
172100             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
172200             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
172300             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
172400             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
172500             15  P-NEW-CAPI-NEW-HOSP       PIC X.
172600             15  P-NEW-CAPI-IME            PIC 9V9999.
172700             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
172800         05  P-HVBP-HRR-DATA.
172900             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
173000             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
173100             15  P-HOSP-READMISSION-REDU    PIC X.
173200             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
173300         05  P-MODEL1-BUNDLE-DATA.
173400             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
173500             15  P-HAC-REDUC-IND            PIC X.
173600             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
173700             15  P-EHR-REDUC-IND            PIC X.
173800         05  FILLER                         PIC X(09).
173900
174000*****************************************************************
174100 01  WAGE-NEW-CBSA-INDEX-RECORD.
174200     05  W-CBSA                        PIC X(5).
174300     05  W-CBSA-SIZE                   PIC X.
174400         88  LARGE-URBAN       VALUE 'L'.
174500         88  OTHER-URBAN       VALUE 'O'.
174600         88  ALL-RURAL         VALUE 'R'.
174700     05  W-CBSA-EFF-DATE               PIC X(8).
174800     05  FILLER                        PIC X.
174900     05  W-CBSA-INDEX-RECORD           PIC S9(02)V9(04).
175000     05  W-CBSA-PR-INDEX-RECORD        PIC S9(02)V9(04).
175100
175200*******************************************************
175300*    HOLD VARIABLES POPULATED IN PPCAL___***          *
175400*******************************************************
175500 COPY PPHOLDAR.
175600
175700******************************************************************
175800 PROCEDURE DIVISION  USING BILL-NEW-DATA
175900                           PPS-DATA
176000                           PRICER-OPT-VERS-SW
176100                           PPS-ADDITIONAL-VARIABLES
176200                           PROV-NEW-HOLD
176300                           WAGE-NEW-CBSA-INDEX-RECORD
176400                           PPHOLDAR-HOLD-AREA.
176500
176600***************************************************************
176700*    PROCESSING:                                              *
176800*        A. WILL PROCESS CASES BASED ON DISCHARGE DATE
176900*        B. INITIALIZE PPCAL  HOLD VARIABLES.                 *
177000*        C. EDIT THE DATA PASSED FROM THE BILL BEFORE         *
177100*           ATTEMPTING TO CALCULATE PPS. IF THIS BILL         *
177200*           CANNOT BE PROCESSED, SET A RETURN CODE AND        *
177300*           GOBACK.                                           *
177400*        D. ASSEMBLE PRICING COMPONENTS.                      *
177500*        E. CALCULATE THE PRICE.                              *
177600***************************************************************
177700     INITIALIZE WK-HLDDRG-DATA
177800                WK-HLDDRG-DATA2.
177900
178000     MOVE ZEROES TO NON-TEMP-RELIEF-PAYMENT.
178100     MOVE ZEROES TO WK-UNCOMP-CARE-AMOUNT.
178200     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT.
178300     MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT.
178400     MOVE ZEROES TO H-READMIS-ADJUST-AMT.
178500     MOVE 'N' TO TEMP-RELIEF-FLAG.
178600     MOVE 'N' TO OUTLIER-RECON-FLAG.
178700     MOVE ZEROES TO WK-HAC-AMOUNT.
178800     MOVE ZEROES TO WK-HAC-TOTAL-PAYMENT.
178900     MOVE ZEROES TO H-NEW-TECH-PAY-ADD-ON.
179000     MOVE ZEROES TO PPS-NEW-TECH-PAY-ADD-ON.
179100
179200     PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT.
179300
179400     MOVE HOLD-ADDITIONAL-VARIABLES TO  PPS-ADDITIONAL-VARIABLES.
179500     MOVE H-DSCHG-FRCTN             TO  PPS-DSCHG-FRCTN.
179600     MOVE H-DRG-WT-FRCTN            TO  PPS-DRG-WT-FRCTN.
179700     MOVE HOLD-CAPITAL-VARIABLES    TO  PPS-CAPITAL-VARIABLES.
179800     MOVE HOLD-CAPITAL2-VARIABLES   TO  PPS-CAPITAL2-VARIABLES.
179900     MOVE CAL-VERSION               TO  PPS-CALC-VERS.
180000     MOVE HOLD-OTHER-VARIABLES      TO  PPS-OTHER-VARIABLES.
180100     MOVE HOLD-PC-OTH-VARIABLES     TO  PPS-PC-OTH-VARIABLES.
180200     MOVE H-ADDITIONAL-PAY-INFO-DATA TO
180300                            PPS-ADDITIONAL-PAY-INFO-DATA.
180400     MOVE H-ADDITIONAL-PAY-INFO-DATA2 TO
180500                            PPS-ADDITIONAL-PAY-INFO-DATA2.
180600
180700
180800     COMPUTE PPS-OPER-HSP-PART2 ROUNDED =  1 *  H-HSP-RATE.
180900     MOVE    WK-UNCOMP-CARE-AMOUNT TO PPS-UNCOMP-CARE-AMOUNT.
181000     MOVE    H-BUNDLE-ADJUST-AMT TO PPS-BUNDLE-ADJUST-AMT.
181100     MOVE    H-VAL-BASED-PURCH-ADJUST-AMT TO
181200                           PPS-VAL-BASED-PURCH-ADJUST-AMT.
181300     MOVE    H-READMIS-ADJUST-AMT TO PPS-READMIS-ADJUST-AMT.
181400     MOVE    P-MODEL1-BUNDLE-DISPRCNT TO
181500                               PPS-MODEL1-BUNDLE-DISPRCNT.
181600
181700     MOVE P-HAC-REDUC-IND  TO  PPS-HAC-PROG-REDUC-IND.
181800     MOVE P-EHR-REDUC-IND  TO  PPS-EHR-PROG-REDUC-IND.
181900     MOVE H-EHR-ADJUST-AMT TO  PPS-EHR-ADJUST-AMT.
182000*    MOVE H-STNDRD-VALUE   TO  PPS-STNDRD-VALUE.
182100     MOVE H-STANDARD-ALLOWED-AMOUNT  TO  PPS-STNDRD-VALUE.
182200     MOVE WK-HAC-AMOUNT  TO   PPS-HAC-PAYMENT-AMT.
182300     MOVE 0     TO    PPS-FLX7-PAYMENT.
182400
182500     IF (PPS-RTC = '00' OR '03' OR '10' OR
182600                   '12' OR '14')
182700        MOVE 'Y' TO OUTLIER-RECON-FLAG
182800        MOVE PPS-DATA TO HLD-PPS-DATA
182900        PERFORM 0200-MAINLINE-CONTROL THRU 0200-EXIT
183000        MOVE HLD-PPS-DATA TO PPS-DATA.
183100
183200     IF  PPS-RTC < 50
183300         IF  P-NEW-WAIVER-STATE
183400             MOVE 53 TO PPS-RTC
183500             MOVE ALL '0' TO PPS-OPER-HSP-PART
183600                             PPS-OPER-FSP-PART
183700                             PPS-OPER-OUTLIER-PART
183800                             PPS-OUTLIER-DAYS
183900                             PPS-REG-DAYS-USED
184000                             PPS-LTR-DAYS-USED
184100                             PPS-TOTAL-PAYMENT
184200                             WK-HAC-TOTAL-PAYMENT
184300                             PPS-OPER-DSH-ADJ
184400                             PPS-OPER-IME-ADJ
184500                             H-DSCHG-FRCTN
184600                             H-DRG-WT-FRCTN
184700                             HOLD-ADDITIONAL-VARIABLES
184800                             HOLD-CAPITAL-VARIABLES
184900                             HOLD-CAPITAL2-VARIABLES
185000                             HOLD-OTHER-VARIABLES
185100                             HOLD-PC-OTH-VARIABLES
185200                             H-ADDITIONAL-PAY-INFO-DATA
185300                             H-ADDITIONAL-PAY-INFO-DATA2.
185400
185500     GOBACK.
185600
185700 0200-MAINLINE-CONTROL.
185800
185900     MOVE 'N' TO HMO-TAG.
186000
186100     IF PPS-PC-HMO-FLAG = 'Y' OR
186200               HMO-FLAG = 'Y'
186300        MOVE 'Y' TO HMO-TAG.
186400
186500     IF NOT P-PR-NEW-STATE
186600        MOVE ZEROES TO W-CBSA-PR-INDEX-RECORD.
186700
186800     MOVE ALL '0' TO PPS-DATA
186900                     H-OPER-DSH-SCH
187000                     H-OPER-DSH-RRC
187100                     HOLD-PPS-COMPONENTS
187200                     HOLD-PPS-COMPONENTS
187300                     HOLD-ADDITIONAL-VARIABLES
187400                     HOLD-CAPITAL-VARIABLES
187500                     HOLD-CAPITAL2-VARIABLES
187600                     HOLD-OTHER-VARIABLES
187700                     HOLD-PC-OTH-VARIABLES
187800                     H-ADDITIONAL-PAY-INFO-DATA
187900                     H-ADDITIONAL-PAY-INFO-DATA2
188000                     H-EHR-SUBSAV-QUANT
188100                     H-EHR-SUBSAV-LV
188200                     H-EHR-SUBSAV-QUANT-INCLV
188300                     H-EHR-RESTORE-FULL-QUANT
188400                     H-OPER-BILL-STDZ-COSTS
188500                     H-CAPI-BILL-STDZ-COSTS
188600                     H-OPER-STDZ-COST-OUTLIER
188700                     H-CAPI-STDZ-COST-OUTLIER
188800                     H-OPER-STDZ-DOLLAR-THRESHOLD
188900                     H-CAPI-STDZ-DOLLAR-THRESHOLD
189000                     WK-LOW-VOL-ADDON
189100                     WK-HAC-AMOUNT
189200                     WK-HAC-TOTAL-PAYMENT.
189300
189400     IF P-NEW-CAPI-HOSP-SPEC-RATE NOT NUMERIC
189500        MOVE 0 TO P-NEW-CAPI-HOSP-SPEC-RATE.
189600
189700     IF P-NEW-CAPI-OLD-HARM-RATE  NOT NUMERIC
189800        MOVE 0 TO P-NEW-CAPI-OLD-HARM-RATE.
189900
190000     IF P-NEW-CAPI-NEW-HARM-RATIO NOT NUMERIC
190100        MOVE 0 TO P-NEW-CAPI-NEW-HARM-RATIO.
190200
190300     IF P-NEW-CAPI-CSTCHG-RATIO NOT NUMERIC
190400        MOVE 0 TO P-NEW-CAPI-CSTCHG-RATIO.
190500
190600     IF P-HOSP-HRR-ADJUSTMT     NOT NUMERIC
190700        MOVE 0 TO P-HOSP-HRR-ADJUSTMT.
190800
190900     IF P-VAL-BASED-PURCH-ADJUST NOT NUMERIC
191000        MOVE 0 TO P-VAL-BASED-PURCH-ADJUST.
191100
191200     IF P-MODEL1-BUNDLE-DISPRCNT NOT NUMERIC
191300        MOVE 0 TO P-MODEL1-BUNDLE-DISPRCNT.
191400
191500
191600     PERFORM 1000-EDIT-THE-BILL-INFO.
191700
191800     IF  PPS-RTC = 00
191900         PERFORM 2000-ASSEMBLE-PPS-VARIABLES THRU 2000-EXIT.
192000
192100     IF  PPS-RTC = 00
192200         PERFORM 3000-CALC-PAYMENT THRU 3000-EXIT.
192300
192400     IF OUTLIER-RECON-FLAG = 'Y'
192500        MOVE 'N' TO OUTLIER-RECON-FLAG
192600        GO TO 0200-EXIT.
192700
192800     IF PPS-RTC = 00
192900        IF H-PERDIEM-DAYS = H-ALOS OR
193000           H-PERDIEM-DAYS > H-ALOS
193100           MOVE 14 TO PPS-RTC.
193200
193300     IF PPS-RTC = 02
193400        IF H-PERDIEM-DAYS = H-ALOS OR
193500           H-PERDIEM-DAYS > H-ALOS
193600           MOVE 16 TO PPS-RTC.
193700
193800 0200-EXIT.   EXIT.
193900
194000 1000-EDIT-THE-BILL-INFO.
194100
194200     MOVE 1.00 TO H-CAPI-PAYCDE-PCT1.
194300     MOVE 0.00 TO H-CAPI-PAYCDE-PCT2.
194400
194500**   IF  PPS-RTC = 00
194600*        IF  P-NEW-WAIVER-STATE
194700*            MOVE 53 TO PPS-RTC.
194800
194900     IF  PPS-RTC = 00
195000         IF   HLDDRG-VALID = 'I'
195100             MOVE 54 TO PPS-RTC.
195200
195300     IF  PPS-RTC = 00
195400            IF  ((B-DISCHARGE-DATE < P-NEW-EFF-DATE) OR
195500                 (B-DISCHARGE-DATE < W-CBSA-EFF-DATE))
195600                MOVE 55 TO PPS-RTC.
195700
195800     IF  PPS-RTC = 00
195900         IF P-NEW-TERMINATION-DATE > 00000000
196000            IF  ((B-DISCHARGE-DATE = P-NEW-TERMINATION-DATE) OR
196100                 (B-DISCHARGE-DATE > P-NEW-TERMINATION-DATE))
196200                  MOVE 55 TO PPS-RTC.
196300
196400     IF  PPS-RTC = 00
196500         IF  B-LOS NOT NUMERIC
196600             MOVE 56 TO PPS-RTC
196700         ELSE
196800         IF  B-LOS = 0
196900             IF B-REVIEW-CODE NOT = 00 AND
197000                              NOT = 03 AND
197100                              NOT = 06 AND
197200                              NOT = 07 AND
197300                              NOT = 09 AND
197400                              NOT = 11
197500             MOVE 56 TO PPS-RTC.
197600
197700     IF  PPS-RTC = 00
197800         IF  B-LTR-DAYS NOT NUMERIC OR B-LTR-DAYS > 60
197900             MOVE 61 TO PPS-RTC
198000         ELSE
198100             MOVE B-LTR-DAYS TO H-LTR-DAYS.
198200
198300     IF  PPS-RTC = 00
198400         IF  B-COVERED-DAYS NOT NUMERIC
198500             MOVE 62 TO PPS-RTC
198600         ELSE
198700         IF  B-COVERED-DAYS = 0 AND B-LOS > 0
198800             MOVE 62 TO PPS-RTC
198900         ELSE
199000             MOVE B-COVERED-DAYS TO H-COV-DAYS.
199100
199200     IF  PPS-RTC = 00
199300         IF  H-LTR-DAYS  > H-COV-DAYS
199400             MOVE 62 TO PPS-RTC
199500         ELSE
199600             COMPUTE H-REG-DAYS = H-COV-DAYS - H-LTR-DAYS.
199700
199800     IF  PPS-RTC = 00
199900         IF  NOT VALID-REVIEW-CODE
200000             MOVE 57 TO PPS-RTC.
200100
200200     IF  PPS-RTC = 00
200300         IF  B-CHARGES-CLAIMED NOT NUMERIC
200400             MOVE 58 TO PPS-RTC.
200500
200600     IF PPS-RTC = 00
200700           IF P-NEW-CAPI-NEW-HOSP NOT = 'Y'
200800                 IF P-NEW-CAPI-PPS-PAY-CODE NOT = 'B' AND
200900                                            NOT = 'C'
201000                 MOVE 65 TO PPS-RTC.
201100
201200***  MDH PROVISION ENDS 3/31/2015
201300***  CODE COMMENTED OUT IN ORDER TO EXTEND EXPIRING PROVISON
201400
201500*    IF PPS-RTC = 00 AND
201600*       B-DISCHARGE-DATE > 20150331 AND
201700*       P-N-INVALID-PROV-TYPES
201800*                MOVE 52 TO PPS-RTC.
201900
202000 2000-ASSEMBLE-PPS-VARIABLES.
202100***  GET THE PROVIDER SPECIFIC VARIABLES.
202200
202300     MOVE P-NEW-FAC-SPEC-RATE TO H-FAC-SPEC-RATE.
202400     MOVE P-NEW-INTERN-RATIO TO H-INTERN-RATIO.
202500
202600     IF  (P-NEW-STATE = 02 OR 12)
202700         MOVE P-NEW-COLA TO H-OPER-COLA
202800     ELSE
202900         MOVE 1.000  TO H-OPER-COLA.
203000
203100***************************************************************
203200***  GET THE DRG RELATIVE WEIGHTS, ALOS, DAYS CUTOFF
203300
203400     PERFORM 2600-GET-DRG-WEIGHT THRU 2600-EXIT.
203500
203600     PERFORM 4410-UNCOMP-CARE-CODE-RTN THRU 4410-EXIT.
203700
203800     MOVE P-NEW-STATE            TO MES-PPS-STATE.
203900*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
204000** USING THE STATE FACTORS TO ALTER THE WAGE INDEX WAS STOPPED*
204100** FOR FY 2011
204200***************************************************************
204300*    PERFORM 4200-SSRFBN-CODE-RTN THRU 4200-EXIT.
204400*****YEARCHANGE 2011.0 ** NOT USED 2012 *******************
204500***************************************************************
204600***  GET THE WAGE-INDEX
204700
204800     MOVE W-CBSA-INDEX-RECORD TO H-WAGE-INDEX.
204900     MOVE W-CBSA-PR-INDEX-RECORD TO H-PR-WAGE-INDEX.
205000     MOVE P-NEW-STATE            TO MES-PPS-STATE.
205100
205200*****YEARCHANGE 2011.0 ** NOT USED 2013 *******************
205300
205400***************************************************************
205500* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
205600* WITH DISCHARGE DATES PRIOR TO 01/01/2016                    *
205700***************************************************************
205800
205900     IF B-DISCHARGE-DATE < 20160101
206000        PERFORM 2050-RATES-OTB
206100     ELSE
206200        PERFORM 2051-RATES-NTB.
206300
206400 2000-EXIT.  EXIT.
206500
206600 2050-RATES-OTB.
206700     IF  P-PR-NEW-STATE
206800         MOVE 2 TO R2
206900         MOVE 3 TO R4
207000     ELSE
207100         MOVE 1 TO R2
207200         MOVE 1 TO R4.
207300
207400     IF  LARGE-URBAN
207500         MOVE 1 TO R3
207600     ELSE
207700         MOVE 2 TO R3.
207800
207900     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
208000        (P-EHR-REDUC-IND = ' ')           AND
208100        (H-WAGE-INDEX > 01.0000))
208200        PERFORM 2300-GET-LAB-NONLAB-OTB1-RATES
208300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
208400
208500     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
208600        (P-EHR-REDUC-IND = ' ')               AND
208700         (H-WAGE-INDEX > 01.0000))
208800        PERFORM 2300-GET-LAB-NONLAB-OTB2-RATES
208900             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
209000
209100     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
209200        (P-EHR-REDUC-IND = ' ')            AND
209300         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
209400        PERFORM 2300-GET-LAB-NONLAB-OTB3-RATES
209500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
209600
209700     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
209800        (P-EHR-REDUC-IND = ' ')               AND
209900         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
210000        PERFORM 2300-GET-LAB-NONLAB-OTB4-RATES
210100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
210200
210300     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
210400        (P-EHR-REDUC-IND = 'Y')           AND
210500        (H-WAGE-INDEX > 01.0000))
210600        PERFORM 2300-GET-LAB-NONLAB-OTB5-RATES
210700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
210800
210900     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
211000        (P-EHR-REDUC-IND = 'Y')               AND
211100         (H-WAGE-INDEX > 01.0000))
211200        PERFORM 2300-GET-LAB-NONLAB-OTB6-RATES
211300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
211400
211500     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
211600        (P-EHR-REDUC-IND = 'Y')            AND
211700         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
211800        PERFORM 2300-GET-LAB-NONLAB-OTB7-RATES
211900             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
212000
212100     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
212200        (P-EHR-REDUC-IND = 'Y')               AND
212300         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
212400        PERFORM 2300-GET-LAB-NONLAB-OTB8-RATES
212500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
212600
212700     IF P-PR-NEW-STATE
212800        IF (H-PR-WAGE-INDEX > 01.0000)
212900            PERFORM 2300-GET-PR-LAB-OTB1-RATES
213000            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
213100
213200     IF P-PR-NEW-STATE
213300        IF (H-PR-WAGE-INDEX < 01.0000 OR
213400            H-PR-WAGE-INDEX = 01.0000)
213500            PERFORM 2300-GET-PR-LAB-OTB3-RATES
213600            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
213700*
213800*    IF P-PR-NEW-STATE
213900*       IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
214000*           (H-PR-WAGE-INDEX > 01.0000))
214100*            PERFORM 2300-GET-PR-LAB-OTB1-RATES
214200*            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
214300*
214400*    IF P-PR-NEW-STATE
214500*       IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
214600*            (H-PR-WAGE-INDEX > 01.0000))
214700*             PERFORM 2300-GET-PR-LAB-OTB2-RATES
214800*                 VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
214900*
215000*    IF P-PR-NEW-STATE
215100*       IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
215200*       (H-PR-WAGE-INDEX < 01.0000 OR H-PR-WAGE-INDEX = 01.0000))
215300*         PERFORM 2300-GET-PR-LAB-OTB3-RATES
215400*             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
215500*
215600*    IF P-PR-NEW-STATE
215700*       IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
215800*       (H-PR-WAGE-INDEX < 01.0000 OR H-PR-WAGE-INDEX = 01.0000))
215900*         PERFORM 2300-GET-PR-LAB-OTB4-RATES
216000*             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
216100*
216200***************************************************************
216300* GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL              *
216400***************************************************************
216500
216600     MOVE 0.00  TO H-OPER-HSP-PCT.
216700     MOVE 1.00  TO H-OPER-FSP-PCT.
216800
216900***************************************************************
217000*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
217100***************************************************************
217200
217300      MOVE 1.00 TO H-NAT-PCT.
217400      MOVE 0.00 TO H-REG-PCT.
217500
217600      MOVE 1.00 TO H-C-NAT-PCT.
217700      MOVE 0.00 TO H-C-REG-PCT.
217800
217900     IF  (P-PR-NEW-STATE AND
218000         B-DISCHARGE-DATE < 20160101)
218100         MOVE 0.75 TO H-NAT-PCT
218200         MOVE 0.25 TO H-REG-PCT.
218300
218400     IF  P-PR-NEW-STATE
218500         MOVE 0.75 TO H-C-NAT-PCT
218600         MOVE 0.25 TO H-C-REG-PCT.
218700
218800     IF  P-N-SCH-REBASED-FY90 OR
218900         P-N-EACH OR
219000         P-N-MDH-REBASED-FY90 OR
219100         B-FORMER-MDH-PROVIDERS
219200         MOVE 1.00 TO H-OPER-HSP-PCT.
219300
219400***************************************************************
219500* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
219600* WITH DISCHARGE DATES BEFORE 01/01/2016                      *
219700***************************************************************
219800
219900 2300-GET-LAB-NONLAB-OTB1-RATES.
220000
220100     IF  B-DISCHARGE-DATE NOT < OTB1-RATE-EFF-DATE (R1)
220200         MOVE OTB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
220300         MOVE OTB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
220400         MOVE OTB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
220500         MOVE OTB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
220600
220700 2300-GET-LAB-NONLAB-OTB2-RATES.
220800
220900     IF  B-DISCHARGE-DATE NOT < OTB2-RATE-EFF-DATE (R1)
221000         MOVE OTB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
221100         MOVE OTB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
221200         MOVE OTB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
221300         MOVE OTB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
221400
221500 2300-GET-LAB-NONLAB-OTB3-RATES.
221600
221700     IF  B-DISCHARGE-DATE NOT < OTB3-RATE-EFF-DATE (R1)
221800         MOVE OTB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
221900         MOVE OTB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
222000         MOVE OTB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
222100         MOVE OTB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
222200
222300 2300-GET-LAB-NONLAB-OTB4-RATES.
222400
222500     IF  B-DISCHARGE-DATE NOT < OTB4-RATE-EFF-DATE (R1)
222600         MOVE OTB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
222700         MOVE OTB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
222800         MOVE OTB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
222900         MOVE OTB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
223000
223100 2300-GET-LAB-NONLAB-OTB5-RATES.
223200
223300     IF  B-DISCHARGE-DATE NOT < OTB1-RATE-EFF-DATE (R1)
223400         MOVE OTB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
223500         MOVE OTB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
223600         MOVE OTB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
223700         MOVE OTB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
223800
223900 2300-GET-LAB-NONLAB-OTB6-RATES.
224000
224100     IF  B-DISCHARGE-DATE NOT < OTB2-RATE-EFF-DATE (R1)
224200         MOVE OTB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
224300         MOVE OTB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
224400         MOVE OTB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
224500         MOVE OTB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
224600
224700 2300-GET-LAB-NONLAB-OTB7-RATES.
224800
224900     IF  B-DISCHARGE-DATE NOT < OTB3-RATE-EFF-DATE (R1)
225000         MOVE OTB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
225100         MOVE OTB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
225200         MOVE OTB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
225300         MOVE OTB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
225400
225500 2300-GET-LAB-NONLAB-OTB8-RATES.
225600
225700     IF  B-DISCHARGE-DATE NOT < OTB4-RATE-EFF-DATE (R1)
225800         MOVE OTB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
225900         MOVE OTB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
226000         MOVE OTB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
226100         MOVE OTB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
226200
226300 2300-GET-PR-LAB-OTB1-RATES.
226400
226500     IF  B-DISCHARGE-DATE NOT < OTB1-RATE-EFF-DATE (R1)
226600         MOVE OTB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
226700         MOVE OTB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
226800
226900*2300-GET-PR-LAB-OTB2-RATES.
227000*
227100*    IF  B-DISCHARGE-DATE NOT < OTB2-RATE-EFF-DATE (R1)
227200*        MOVE OTB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
227300*        MOVE OTB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
227400
227500 2300-GET-PR-LAB-OTB3-RATES.
227600
227700     IF  B-DISCHARGE-DATE NOT < OTB3-RATE-EFF-DATE (R1)
227800         MOVE OTB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
227900         MOVE OTB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
228000
228100*2300-GET-PR-LAB-OTB4-RATES.
228200*
228300*    IF  B-DISCHARGE-DATE NOT < OTB4-RATE-EFF-DATE (R1)
228400*        MOVE OTB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
228500*        MOVE OTB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
228600
228700
228800 2051-RATES-NTB.
228900     IF  P-PR-NEW-STATE
229000         MOVE 2 TO R2
229100         MOVE 3 TO R4
229200     ELSE
229300         MOVE 1 TO R2
229400         MOVE 1 TO R4.
229500
229600     IF  LARGE-URBAN
229700         MOVE 1 TO R3
229800     ELSE
229900         MOVE 2 TO R3.
230000
230100     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
230200        (P-EHR-REDUC-IND = ' ')           AND
230300        (H-WAGE-INDEX > 01.0000))
230400        PERFORM 2300-GET-LAB-NONLAB-NTB1-RATES
230500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
230600
230700     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
230800        (P-EHR-REDUC-IND = ' ')               AND
230900         (H-WAGE-INDEX > 01.0000))
231000        PERFORM 2300-GET-LAB-NONLAB-NTB2-RATES
231100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
231200
231300     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
231400        (P-EHR-REDUC-IND = ' ')            AND
231500         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
231600        PERFORM 2300-GET-LAB-NONLAB-NTB3-RATES
231700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
231800
231900     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
232000        (P-EHR-REDUC-IND = ' ')               AND
232100         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
232200        PERFORM 2300-GET-LAB-NONLAB-NTB4-RATES
232300             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
232400
232500     IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
232600        (P-EHR-REDUC-IND = 'Y')           AND
232700        (H-WAGE-INDEX > 01.0000))
232800        PERFORM 2300-GET-LAB-NONLAB-NTB5-RATES
232900             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
233000
233100     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
233200        (P-EHR-REDUC-IND = 'Y')               AND
233300         (H-WAGE-INDEX > 01.0000))
233400        PERFORM 2300-GET-LAB-NONLAB-NTB6-RATES
233500             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
233600
233700     IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
233800        (P-EHR-REDUC-IND = 'Y')            AND
233900         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
234000        PERFORM 2300-GET-LAB-NONLAB-NTB7-RATES
234100             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
234200
234300     IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
234400        (P-EHR-REDUC-IND = 'Y')               AND
234500         (H-WAGE-INDEX < 01.0000 OR H-WAGE-INDEX = 01.0000))
234600        PERFORM 2300-GET-LAB-NONLAB-NTB8-RATES
234700             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
234800
234900     IF P-PR-NEW-STATE
235000        IF (H-PR-WAGE-INDEX > 01.0000)
235100            PERFORM 2300-GET-PR-LAB-NTB1-RATES
235200            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
235300
235400     IF P-PR-NEW-STATE
235500        IF (H-PR-WAGE-INDEX < 01.0000 OR
235600            H-PR-WAGE-INDEX = 01.0000)
235700            PERFORM 2300-GET-PR-LAB-NTB3-RATES
235800            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
235900*
236000*    IF P-PR-NEW-STATE
236100*       IF ((P-NEW-CBSA-HOSP-QUAL-IND = '1') AND
236200*           (H-PR-WAGE-INDEX > 01.0000))
236300*            PERFORM 2300-GET-PR-LAB-NTB1-RATES
236400*            VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
236500*
236600*    IF P-PR-NEW-STATE
236700*       IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
236800*            (H-PR-WAGE-INDEX > 01.0000))
236900*             PERFORM 2300-GET-PR-LAB-NTB2-RATES
237000*                 VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
237100*
237200*    IF P-PR-NEW-STATE
237300*       IF ((P-NEW-CBSA-HOSP-QUAL-IND  = '1') AND
237400*       (H-PR-WAGE-INDEX < 01.0000 OR H-PR-WAGE-INDEX = 01.0000))
237500*         PERFORM 2300-GET-PR-LAB-NTB3-RATES
237600*             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
237700*
237800*    IF P-PR-NEW-STATE
237900*       IF ((P-NEW-CBSA-HOSP-QUAL-IND NOT = '1') AND
238000*       (H-PR-WAGE-INDEX < 01.0000 OR H-PR-WAGE-INDEX = 01.0000))
238100*         PERFORM 2300-GET-PR-LAB-NTB4-RATES
238200*             VARYING R1 FROM 1 BY 1 UNTIL R1 > 1.
238300*
238400***************************************************************
238500***  GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL
238600***  GET THE HSP & FSP BLEND PERCENTS FOR THIS BILL
238700
238800     MOVE 0.00  TO H-OPER-HSP-PCT.
238900     MOVE 1.00  TO H-OPER-FSP-PCT.
239000
239100***************************************************************
239200*  GET THE NATIONAL & REGIONAL BLEND PERCENTS FOR THIS BILL   *
239300***************************************************************
239400
239500      MOVE 1.00 TO H-NAT-PCT.
239600      MOVE 0.00 TO H-REG-PCT.
239700
239800      MOVE 1.00 TO H-C-NAT-PCT.
239900      MOVE 0.00 TO H-C-REG-PCT.
240000
240100     IF  (P-PR-NEW-STATE AND
240200         B-DISCHARGE-DATE < 20160101)
240300         MOVE 0.75 TO H-NAT-PCT
240400         MOVE 0.25 TO H-REG-PCT.
240500
240600     IF  P-PR-NEW-STATE
240700         MOVE 0.75 TO H-C-NAT-PCT
240800         MOVE 0.25 TO H-C-REG-PCT.
240900
241000     IF  P-N-SCH-REBASED-FY90 OR
241100         P-N-EACH OR
241200         P-N-MDH-REBASED-FY90 OR
241300         B-FORMER-MDH-PROVIDERS
241400         MOVE 1.00 TO H-OPER-HSP-PCT.
241500
241600***************************************************************
241700* GET THE LABOR, NON-LABOR STANDARD RATES FOR CLAIMS          *
241800* WITH DISCHARGE DATES ON OR AFTER 01/01/2016                 *
241900***************************************************************
242000
242100 2300-GET-LAB-NONLAB-NTB1-RATES.
242200
242300     IF  B-DISCHARGE-DATE NOT < NTB1-RATE-EFF-DATE (R1)
242400         MOVE NTB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
242500         MOVE NTB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
242600         MOVE NTB1-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
242700         MOVE NTB1-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
242800
242900 2300-GET-LAB-NONLAB-NTB2-RATES.
243000
243100     IF  B-DISCHARGE-DATE NOT < NTB2-RATE-EFF-DATE (R1)
243200         MOVE NTB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
243300         MOVE NTB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
243400         MOVE NTB2-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
243500         MOVE NTB2-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
243600
243700 2300-GET-LAB-NONLAB-NTB3-RATES.
243800
243900     IF  B-DISCHARGE-DATE NOT < NTB3-RATE-EFF-DATE (R1)
244000         MOVE NTB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
244100         MOVE NTB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
244200         MOVE NTB3-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
244300         MOVE NTB3-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
244400
244500 2300-GET-LAB-NONLAB-NTB4-RATES.
244600
244700     IF  B-DISCHARGE-DATE NOT < NTB4-RATE-EFF-DATE (R1)
244800         MOVE NTB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
244900         MOVE NTB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
245000         MOVE NTB4-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
245100         MOVE NTB4-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
245200
245300 2300-GET-LAB-NONLAB-NTB5-RATES.
245400
245500     IF  B-DISCHARGE-DATE NOT < NTB1-RATE-EFF-DATE (R1)
245600         MOVE NTB5-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
245700         MOVE NTB5-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
245800         MOVE NTB5-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
245900         MOVE NTB5-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
246000
246100 2300-GET-LAB-NONLAB-NTB6-RATES.
246200
246300     IF  B-DISCHARGE-DATE NOT < NTB2-RATE-EFF-DATE (R1)
246400         MOVE NTB6-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
246500         MOVE NTB6-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
246600         MOVE NTB6-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
246700         MOVE NTB6-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
246800
246900 2300-GET-LAB-NONLAB-NTB7-RATES.
247000
247100     IF  B-DISCHARGE-DATE NOT < NTB3-RATE-EFF-DATE (R1)
247200         MOVE NTB7-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
247300         MOVE NTB7-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
247400         MOVE NTB7-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
247500         MOVE NTB7-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
247600
247700 2300-GET-LAB-NONLAB-NTB8-RATES.
247800
247900     IF  B-DISCHARGE-DATE NOT < NTB4-RATE-EFF-DATE (R1)
248000         MOVE NTB8-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
248100         MOVE NTB8-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR
248200         MOVE NTB8-REG-LABOR (R1 R4 R3) TO H-NAT-LABOR
248300         MOVE NTB8-REG-NLABOR (R1 R4 R3) TO H-NAT-NONLABOR.
248400
248500 2300-GET-PR-LAB-NTB1-RATES.
248600
248700     IF  B-DISCHARGE-DATE NOT < NTB1-RATE-EFF-DATE (R1)
248800         MOVE NTB1-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
248900         MOVE NTB1-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
249000
249100*2300-GET-PR-LAB-NTB2-RATES.
249200*
249300*    IF  B-DISCHARGE-DATE NOT < NTB2-RATE-EFF-DATE (R1)
249400*        MOVE NTB2-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
249500*        MOVE NTB2-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
249600
249700 2300-GET-PR-LAB-NTB3-RATES.
249800
249900     IF  B-DISCHARGE-DATE NOT < NTB3-RATE-EFF-DATE (R1)
250000         MOVE NTB3-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
250100         MOVE NTB3-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
250200
250300*2300-GET-PR-LAB-NTB4-RATES.
250400*
250500*    IF  B-DISCHARGE-DATE NOT < NTB4-RATE-EFF-DATE (R1)
250600*        MOVE NTB4-REG-LABOR (R1 R2 R3) TO H-REG-LABOR
250700*        MOVE NTB4-REG-NLABOR (R1 R2 R3) TO H-REG-NONLABOR.
250800
250900*
251000 2600-GET-DRG-WEIGHT.
251100
251200     IF  B-DISCHARGE-DATE NOT < WK-DRGX-EFF-DATE
251300     SET DRG-IDX TO 1
251400     SEARCH DRG-TAB VARYING DRG-IDX
251500         AT END
251600           MOVE ' NO DRG CODE    FOUND' TO HLDDRG-DESC
251700           MOVE 'I' TO  HLDDRG-VALID
251800           MOVE 0 TO HLDDRG-WEIGHT
251900           MOVE 54 TO PPS-RTC
252000           GO TO 2600-EXIT
252100       WHEN WK-DRG-DRGX(DRG-IDX) = B-DRG
252200         MOVE DRG-DATA-TAB(DRG-IDX) TO HLDDRG-DATA.
252300
252400
252500     MOVE HLDDRG-DATA TO WK-HLDDRG-DATA2.
252600     MOVE  HLDDRG-DRGX         TO HLDDRG-DRGX2.
252700     MOVE  HLDDRG-WEIGHT       TO HLDDRG-WEIGHT2
252800                                  H-DRG-WT.
252900     MOVE  HLDDRG-GMALOS       TO HLDDRG-GMALOS2
253000                                  H-ALOS.
253100     MOVE  HLDDRG-LOW          TO HLDDRG-LOW2.
253200     MOVE  HLDDRG-ARITH-ALOS   TO HLDDRG-ARITH-ALOS2
253300                                  H-ARITH-ALOS.
253400     MOVE  HLDDRG-PAC          TO HLDDRG-PAC2.
253500     MOVE  HLDDRG-SPPAC        TO HLDDRG-SPPAC2.
253600     MOVE  HLDDRG-DESC         TO HLDDRG-DESC2.
253700     MOVE  'V'                 TO HLDDRG-VALID.
253800     MOVE ZEROES               TO H-DAYS-CUTOFF.
253900
254000 2600-EXIT.   EXIT.
254100*
254200 3000-CALC-PAYMENT.
254300***************************************************************
254400
254500     PERFORM 3100-CALC-STAY-UTILIZATION.
254600     PERFORM 3300-CALC-OPER-FSP-AMT.
254700     PERFORM 3900A-CALC-OPER-DSH THRU 3900A-EXIT.
254800
254900***********************************************************
255000***  OPERATING IME CALCULATION
255100
255200     COMPUTE H-OPER-IME-TEACH ROUNDED =
255300            1.35 * ((1 + H-INTERN-RATIO) ** .405  - 1).
255400
255500***********************************************************
255600
255700     MOVE 00                 TO  PPS-RTC.
255800     MOVE H-WAGE-INDEX       TO  PPS-WAGE-INDX.
255900     MOVE H-ALOS             TO  PPS-AVG-LOS.
256000     MOVE H-DAYS-CUTOFF      TO  PPS-DAYS-CUTOFF.
256100
256200     MOVE B-LOS TO H-PERDIEM-DAYS.
256300     IF H-PERDIEM-DAYS < 1
256400         MOVE 1 TO H-PERDIEM-DAYS.
256500     ADD 1 TO H-PERDIEM-DAYS.
256600
256700     MOVE 1 TO H-DSCHG-FRCTN.
256800
256900     COMPUTE H-DRG-WT-FRCTN ROUNDED = H-DSCHG-FRCTN * H-DRG-WT.
257000
257100     IF (PAY-PERDIEM-DAYS  OR
257200         PAY-XFER-NO-COST) OR
257300        (PAY-XFER-SPEC-DRG AND
257400         D-DRG-POSTACUTE-PERDIEM)
257500       IF H-ALOS > 0
257600         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
257700         COMPUTE H-DSCHG-FRCTN  ROUNDED = H-PERDIEM-DAYS / H-ALOS
257800         IF H-DSCHG-FRCTN > 1
257900              MOVE 1 TO H-DSCHG-FRCTN
258000              MOVE 1 TO H-TRANSFER-ADJ
258100         ELSE
258200              COMPUTE H-DRG-WT-FRCTN ROUNDED =
258300                  H-TRANSFER-ADJ * H-DRG-WT
258400         END-IF
258500        END-IF
258600     END-IF.
258700
258800
258900     IF (PAY-XFER-SPEC-DRG AND
259000         D-DRG-POSTACUTE-50-50) AND
259100         H-ALOS > 0
259200         COMPUTE H-TRANSFER-ADJ ROUNDED = H-PERDIEM-DAYS / H-ALOS
259300         COMPUTE H-DSCHG-FRCTN  ROUNDED =
259400                        .5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)
259500         IF H-DSCHG-FRCTN > 1
259600              MOVE 1 TO H-DSCHG-FRCTN
259700              MOVE 1 TO H-TRANSFER-ADJ
259800         ELSE
259900              COMPUTE H-DRG-WT-FRCTN ROUNDED =
260000            (.5 + ((.5 * H-PERDIEM-DAYS) / H-ALOS)) * H-DRG-WT.
260100
260200
260300***********************************************************
260400***  CAPITAL DSH CALCULATION
260500
260600     MOVE 0 TO H-CAPI-DSH.
260700
260800     IF P-NEW-BED-SIZE NOT NUMERIC
260900         MOVE 0 TO P-NEW-BED-SIZE.
261000
261100     IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
261200         COMPUTE H-CAPI-DSH ROUNDED = 2.7183 **
261300                  (.2025 * (P-NEW-SSI-RATIO
261400                          + P-NEW-MEDICAID-RATIO)) - 1.
261500
261600***********************************************************
261700***  CAPITAL IME TEACH CALCULATION
261800
261900     MOVE 0 TO H-WK-CAPI-IME-TEACH.
262000
262100     IF P-NEW-CAPI-IME NUMERIC
262200        IF P-NEW-CAPI-IME > 1.5000
262300           MOVE 1.5000 TO P-NEW-CAPI-IME.
262400
262500*****YEARCHANGE 2009.5 ****************************************
262600***
262700***  PER POLICY, WE REMOVED THE .5 MULTIPLER
262800***
262900***********************************************************
263000     IF P-NEW-CAPI-IME NUMERIC
263100        COMPUTE H-WK-CAPI-IME-TEACH ROUNDED =
263200         ((2.7183 ** (.2822 * P-NEW-CAPI-IME)) - 1).
263300
263400*****YEARCHANGE 2009.5 ****************************************
263500***********************************************************
263600     MOVE 0.00 TO H-DAYOUT-PCT.
263700     MOVE 0.80 TO H-CSTOUT-PCT.
263800
263900*****************************************************************
264000**
264100** BURN DRGS FOR FY14 ARE 927, 928, 929, 933, 934 AND 935.
264200**
264300*****************************************************************
264400
264500     IF  B-DRG = 927 OR 928 OR 929 OR 933 OR 934 OR 935
264600             MOVE 0.90 TO H-CSTOUT-PCT.
264700
264800*****YEARCHANGE 2015.0 ****************************************
264900***     NATIONAL PERCENTAGE
265000     MOVE 0.6960   TO H-LABOR-PCT.
265100     MOVE 0.3040   TO H-NONLABOR-PCT.
265200
265300*****YEARCHANGE 2015.0 ****************************************
265400***     PUERTO RICO PERCENTAGE
265500     MOVE 0.6320   TO H-PR-LABOR-PCT.
265600     MOVE 0.3680   TO H-PR-NONLABOR-PCT.
265700*****YEARCHANGE 2015.0 ****************************************
265800
265900     IF (H-WAGE-INDEX < 01.0000 OR
266000         H-WAGE-INDEX = 01.0000)
266100        MOVE 0.6200 TO H-LABOR-PCT
266200        MOVE 0.3800 TO H-NONLABOR-PCT.
266300***
266400     IF P-PR-NEW-STATE
266500       IF (H-PR-WAGE-INDEX < 01.0000 OR
266600           H-PR-WAGE-INDEX = 01.0000)
266700          MOVE 0.6200 TO H-PR-LABOR-PCT
266800          MOVE 0.3800 TO H-PR-NONLABOR-PCT.
266900
267000     IF  P-NEW-OPER-CSTCHG-RATIO NUMERIC
267100             MOVE P-NEW-OPER-CSTCHG-RATIO TO H-OPER-CSTCHG-RATIO
267200     ELSE
267300             MOVE 0.000 TO H-OPER-CSTCHG-RATIO.
267400
267500     IF P-NEW-CAPI-CSTCHG-RATIO NUMERIC
267600             MOVE P-NEW-CAPI-CSTCHG-RATIO TO H-CAPI-CSTCHG-RATIO
267700     ELSE
267800             MOVE 0.000 TO H-CAPI-CSTCHG-RATIO.
267900
268000***********************************************************
268100*****YEARCHANGE 2010.0 ************************************
268200***  CAPITAL PAYMENT METHOD B - YEARCHNG
268300***  CAPITAL PAYMENT METHOD B
268400
268500     IF W-CBSA-SIZE = 'L'
268600        MOVE 1.00 TO H-CAPI-LARG-URBAN
268700     ELSE
268800        MOVE 1.00 TO H-CAPI-LARG-URBAN.
268900
269000     COMPUTE H-CAPI-GAF    ROUNDED = (H-WAGE-INDEX ** .6848).
269100     COMPUTE H-PR-CAPI-GAF ROUNDED = (H-PR-WAGE-INDEX ** .6848).
269200
269300*****YEARCHANGE 2016.0 ************************************
269400
269500     IF B-DISCHARGE-DATE < 20160101
269600        COMPUTE H-FEDERAL-RATE ROUNDED =
269700                                 (0438.75 * H-CAPI-GAF)
269800        COMPUTE H-PUERTO-RICO-RATE ROUNDED =
269900                                 (0212.55 * H-PR-CAPI-GAF).
270000
270100     IF B-DISCHARGE-DATE > 20151231
270200        COMPUTE H-FEDERAL-RATE ROUNDED =
270300                                 (0438.75 * H-CAPI-GAF)
270400        COMPUTE H-PUERTO-RICO-RATE ROUNDED =
270500                                 (0212.29 * H-PR-CAPI-GAF).
270600
270700*****YEARCHANGE 2015.1 ************************************
270800
270900     COMPUTE H-CAPI-COLA ROUNDED =
271000                     (.3152 * (H-OPER-COLA - 1) + 1).
271100
271200     MOVE H-FEDERAL-RATE TO H-CAPI-FED-RATE.
271300
271400     IF P-PR-NEW-STATE
271500        COMPUTE  H-CAPI-FED-RATE ROUNDED =
271600                 (H-C-NAT-PCT * H-FEDERAL-RATE) +
271700                 (H-C-REG-PCT * H-PUERTO-RICO-RATE).
271800***********************************************************
271900* CAPITAL FSP CALCULATION                                 *
272000***********************************************************
272100
272200     COMPUTE H-CAPI-FSP-PART ROUNDED =
272300                               H-DRG-WT       *
272400                               H-CAPI-FED-RATE *
272500                               H-CAPI-COLA *
272600                               H-CAPI-LARG-URBAN.
272700
272800***********************************************************
272900***  CAPITAL PAYMENT METHOD A
273000***  CAPITAL PAYMENT METHOD A
273100
273200     IF P-N-SCH-REBASED-FY90 OR P-N-EACH
273300        MOVE 1.00 TO H-CAPI-SCH
273400     ELSE
273500        MOVE 0.85 TO H-CAPI-SCH.
273600
273700***********************************************************
273800***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
273900***********  CAPITAL OLD-HOLD-HARMLESS CALCULATION ***********
274000
274100     COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
274200                    (P-NEW-CAPI-OLD-HARM-RATE *
274300                    H-CAPI-SCH).
274400
274500***********************************************************
274600        IF PAY-PERDIEM-DAYS
274700            IF  H-PERDIEM-DAYS < H-ALOS
274800                IF  NOT (B-DRG = 789)
274900                    PERFORM 3500-CALC-PERDIEM-AMT
275000                    MOVE 03 TO PPS-RTC.
275100
275200        IF PAY-XFER-SPEC-DRG
275300            IF  H-PERDIEM-DAYS < H-ALOS
275400                IF  NOT (B-DRG = 789)
275500                    PERFORM 3550-CALC-PERDIEM-AMT.
275600
275700        IF  PAY-XFER-NO-COST
275800            MOVE 00 TO PPS-RTC
275900            IF H-PERDIEM-DAYS < H-ALOS
276000               IF  NOT (B-DRG = 789)
276100                   PERFORM 3500-CALC-PERDIEM-AMT
276200                   MOVE 06 TO PPS-RTC.
276300
276400     PERFORM 4000-CALC-TECH-ADDON THRU 4000-EXIT.
276500
276600     PERFORM 6000-CALC-READMIS-REDU THRU 6000-EXIT.
276700
276800     IF PPS-RTC = 65 OR 67 OR 68
276900               GO TO 3000-CONTINUE.
277000
277100     PERFORM 7000-CALC-VALUE-BASED-PURCH THRU 7000-EXIT.
277200
277300     IF PPS-RTC = 65 OR 67 OR 68
277400               GO TO 3000-CONTINUE.
277500
277600     PERFORM 8000-CALC-BUNDLE-REDU  THRU 8000-EXIT.
277700
277800     IF PPS-RTC = 65 OR 67 OR 68
277900               GO TO 3000-CONTINUE.
278000
278100     PERFORM 3600-CALC-OUTLIER THRU 3600-EXIT.
278200
278300     IF OUTLIER-RECON-FLAG = 'Y' GO TO 3000-EXIT.
278400
278500     IF PPS-RTC = 65 OR 67 OR 68
278600               GO TO 3000-CONTINUE.
278700
278800        IF PAY-XFER-SPEC-DRG
278900            IF  H-PERDIEM-DAYS < H-ALOS
279000                IF  NOT (B-DRG = 789)
279100                    PERFORM 3560-CHECK-RTN-CODE THRU 3560-EXIT.
279200
279300
279400        IF  PAY-PERDIEM-DAYS
279500            IF  H-OPER-OUTCST-PART > 0
279600                MOVE H-OPER-OUTCST-PART TO
279700                     H-OPER-OUTLIER-PART
279800                MOVE 05 TO PPS-RTC
279900            ELSE
280000            IF  PPS-RTC NOT = 03
280100                MOVE 00 TO PPS-RTC
280200                MOVE 0  TO H-OPER-OUTLIER-PART.
280300
280400        IF  PAY-PERDIEM-DAYS
280500            IF  H-CAPI-OUTCST-PART > 0
280600                MOVE H-CAPI-OUTCST-PART TO
280700                     H-CAPI-OUTLIER-PART
280800                MOVE 05 TO PPS-RTC
280900            ELSE
281000            IF  PPS-RTC NOT = 03
281100                MOVE 0  TO H-CAPI-OUTLIER-PART.
281200
281300
281400     IF P-N-SCH-REBASED-FY90 OR
281500        P-N-EACH OR
281600        P-N-MDH-REBASED-FY90 OR
281700        B-FORMER-MDH-PROVIDERS
281800         PERFORM 3450-CALC-ADDITIONAL-HSP THRU 3450-EXIT.
281900
282000
282100 3000-CONTINUE.
282200
282300***********************************************************
282400***  DETERMINES THE FEDERAL AMOUNT THAT WOULD BE PAID IF
282500***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
282600
282700     COMPUTE H-CAPI2-B-FSP-PART ROUNDED = H-CAPI-FSP-PART.
282800
282900***********************************************************
283000
283100     IF  PPS-RTC = 67
283200         MOVE H-OPER-DOLLAR-THRESHOLD TO
283300              WK-H-OPER-DOLLAR-THRESHOLD.
283400
283500     IF  PPS-RTC < 50
283600         PERFORM 3800-CALC-TOT-AMT THRU 3800-EXIT.
283700
283800     IF  PPS-RTC < 50
283900         NEXT SENTENCE
284000     ELSE
284100         MOVE ALL '0' TO PPS-OPER-HSP-PART
284200                         PPS-OPER-FSP-PART
284300                         PPS-OPER-OUTLIER-PART
284400                         PPS-OUTLIER-DAYS
284500                         PPS-REG-DAYS-USED
284600                         PPS-LTR-DAYS-USED
284700                         PPS-TOTAL-PAYMENT
284800                         WK-HAC-TOTAL-PAYMENT
284900                         PPS-OPER-DSH-ADJ
285000                         PPS-OPER-IME-ADJ
285100                         H-DSCHG-FRCTN
285200                         H-DRG-WT-FRCTN
285300                         HOLD-ADDITIONAL-VARIABLES
285400                         HOLD-CAPITAL-VARIABLES
285500                         HOLD-CAPITAL2-VARIABLES
285600                         HOLD-OTHER-VARIABLES
285700                         HOLD-PC-OTH-VARIABLES
285800                        H-ADDITIONAL-PAY-INFO-DATA
285900                        H-ADDITIONAL-PAY-INFO-DATA2.
286000
286100     IF  PPS-RTC = 67
286200         MOVE WK-H-OPER-DOLLAR-THRESHOLD TO
286300                 H-OPER-DOLLAR-THRESHOLD.
286400
286500 3000-EXIT.  EXIT.
286600
286700 3100-CALC-STAY-UTILIZATION.
286800
286900     MOVE 0 TO PPS-REG-DAYS-USED.
287000     MOVE 0 TO PPS-LTR-DAYS-USED.
287100
287200     IF H-REG-DAYS > 0
287300        IF H-REG-DAYS > B-LOS
287400           MOVE B-LOS TO PPS-REG-DAYS-USED
287500        ELSE
287600           MOVE H-REG-DAYS TO PPS-REG-DAYS-USED
287700     ELSE
287800        IF H-LTR-DAYS > B-LOS
287900           MOVE B-LOS TO PPS-LTR-DAYS-USED
288000        ELSE
288100           MOVE H-LTR-DAYS TO PPS-LTR-DAYS-USED.
288200
288300
288400
288500 3300-CALC-OPER-FSP-AMT.
288600***********************************************************
288700*  OPERATING FSP CALCULATION                              *
288800***********************************************************
288900
289000     COMPUTE H-OPER-FSP-PART ROUNDED =
289100           (H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
289200            H-NAT-NONLABOR * H-OPER-COLA) * H-DRG-WT)
289300                   ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
289400
289500     IF P-PR-NEW-STATE
289600       COMPUTE H-OPER-FSP-PART ROUNDED =
289700           (H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
289800            H-NAT-NONLABOR * H-OPER-COLA) * H-DRG-WT)
289900                           +
290000           (H-REG-PCT * (H-REG-LABOR * H-PR-WAGE-INDEX +
290100            H-REG-NONLABOR * H-OPER-COLA) * H-DRG-WT)
290200                   ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
290300
290400
290500 3500-CALC-PERDIEM-AMT.
290600***********************************************************
290700***  REVIEW CODE = 03 OR 06
290800***  OPERATING PERDIEM-AMT CALCULATION
290900***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
291000
291100        COMPUTE H-OPER-FSP-PART ROUNDED =
291200        H-OPER-FSP-PART * H-TRANSFER-ADJ
291300        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
291400
291500***********************************************************
291600***********************************************************
291700***  REVIEW CODE = 03 OR 06
291800***  CAPITAL   PERDIEM-AMT CALCULATION
291900***  CAPITAL   HSP AND FSP CALCULATION FOR TRANSFERS
292000
292100        COMPUTE H-CAPI-FSP-PART ROUNDED =
292200        H-CAPI-FSP-PART * H-TRANSFER-ADJ
292300        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
292400
292500***********************************************************
292600***  REVIEW CODE = 03 OR 06
292700***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
292800***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
292900
293000        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
293100        H-CAPI-OLD-HARMLESS * H-TRANSFER-ADJ
293200        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
293300
293400 3550-CALC-PERDIEM-AMT.
293500***********************************************************
293600***  REVIEW CODE = 09  OR 11 TRANSFER WITH SPECIAL DRG
293700***  OPERATING PERDIEM-AMT CALCULATION
293800***  OPERATING HSP AND FSP CALCULATION FOR TRANSFERS
293900
294000     IF (D-DRG-POSTACUTE-50-50)
294100        MOVE 10 TO PPS-RTC
294200        COMPUTE H-OPER-FSP-PART ROUNDED =
294300        H-OPER-FSP-PART * H-DSCHG-FRCTN
294400        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
294500
294600     IF (D-DRG-POSTACUTE-PERDIEM)
294700        MOVE 12 TO PPS-RTC
294800        COMPUTE H-OPER-FSP-PART ROUNDED =
294900        H-OPER-FSP-PART *  H-TRANSFER-ADJ
295000        ON SIZE ERROR MOVE 0 TO H-OPER-FSP-PART.
295100
295200***********************************************************
295300***  CAPITAL PERDIEM-AMT CALCULATION
295400***  CAPITAL HSP AND FSP CALCULATION FOR TRANSFERS
295500
295600     IF (D-DRG-POSTACUTE-50-50)
295700        MOVE 10 TO PPS-RTC
295800        COMPUTE H-CAPI-FSP-PART ROUNDED =
295900        H-CAPI-FSP-PART * H-DSCHG-FRCTN
296000        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
296100
296200     IF (D-DRG-POSTACUTE-PERDIEM)
296300        MOVE 12 TO PPS-RTC
296400        COMPUTE H-CAPI-FSP-PART ROUNDED =
296500        H-CAPI-FSP-PART *  H-TRANSFER-ADJ
296600        ON SIZE ERROR MOVE 0 TO H-CAPI-FSP-PART.
296700
296800***********************************************************
296900***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
297000***  CAPITAL PERDIEM-AMT, OLD-HARMLESS CALCULATION
297100
297200     IF (D-DRG-POSTACUTE-50-50)
297300        MOVE 10 TO PPS-RTC
297400        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
297500        H-CAPI-OLD-HARMLESS * H-DSCHG-FRCTN
297600        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
297700
297800     IF (D-DRG-POSTACUTE-PERDIEM)
297900        MOVE 12 TO PPS-RTC
298000        COMPUTE H-CAPI-OLD-HARMLESS ROUNDED =
298100        H-CAPI-OLD-HARMLESS *  H-TRANSFER-ADJ
298200        ON SIZE ERROR MOVE 0 TO H-CAPI-OLD-HARMLESS.
298300
298400 3560-CHECK-RTN-CODE.
298500
298600     IF (D-DRG-POSTACUTE-50-50)
298700        MOVE 10 TO PPS-RTC.
298800     IF (D-DRG-POSTACUTE-PERDIEM)
298900        MOVE 12 TO PPS-RTC.
299000
299100 3560-EXIT.    EXIT.
299200
299300***********************************************************
299400 3600-CALC-OUTLIER.
299500***********************************************************
299600*---------------------------------------------------------*
299700* (YEARCHANGE 2016.0)
299800* COST OUTLIER OPERATING AND CAPITAL CALCULATION
299900*---------------------------------------------------------*
300000
300100     IF OUTLIER-RECON-FLAG = 'Y'
300200        COMPUTE H-OPER-CSTCHG-RATIO ROUNDED =
300300               (H-OPER-CSTCHG-RATIO + .2).
300400
300500     IF H-CAPI-CSTCHG-RATIO > 0 OR
300600        H-OPER-CSTCHG-RATIO > 0
300700        COMPUTE H-OPER-SHARE-DOLL-THRESHOLD ROUNDED =
300800                H-OPER-CSTCHG-RATIO /
300900               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
301000        COMPUTE H-CAPI-SHARE-DOLL-THRESHOLD ROUNDED =
301100                H-CAPI-CSTCHG-RATIO /
301200               (H-OPER-CSTCHG-RATIO + H-CAPI-CSTCHG-RATIO)
301300     ELSE
301400        MOVE 0 TO H-OPER-SHARE-DOLL-THRESHOLD
301500                  H-CAPI-SHARE-DOLL-THRESHOLD.
301600
301700*---------------------------------------------------------*
301800* (YEARCHANGE 2016.0)
301900* OUTLIER THRESHOLD AMOUNTS
302000*---------------------------------------------------------*
302100
302200     IF B-DISCHARGE-DATE < 20160101
302300        MOVE 22539.00 TO H-CST-THRESH
302400     ELSE
302500        MOVE 22538.00 TO H-CST-THRESH.
302600
302700     IF (B-REVIEW-CODE = '03') AND
302800         H-PERDIEM-DAYS < H-ALOS
302900        COMPUTE H-CST-THRESH ROUNDED =
303000                      (H-CST-THRESH * H-TRANSFER-ADJ)
303100                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
303200
303300     IF ((B-REVIEW-CODE = '09') AND
303400         (H-PERDIEM-DAYS < H-ALOS))
303500         IF (D-DRG-POSTACUTE-PERDIEM)
303600            COMPUTE H-CST-THRESH ROUNDED =
303700                      (H-CST-THRESH * H-TRANSFER-ADJ)
303800                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
303900
304000     IF ((B-REVIEW-CODE = '09') AND
304100         (H-PERDIEM-DAYS < H-ALOS))
304200         IF (D-DRG-POSTACUTE-50-50)
304300           COMPUTE H-CST-THRESH ROUNDED =
304400          H-CST-THRESH * H-DSCHG-FRCTN
304500                ON SIZE ERROR MOVE 0 TO H-CST-THRESH.
304600
304700     COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
304800        ((H-CST-THRESH * H-LABOR-PCT * H-WAGE-INDEX) +
304900         (H-CST-THRESH * H-NONLABOR-PCT * H-OPER-COLA)) *
305000          H-OPER-SHARE-DOLL-THRESHOLD.
305100
305200     IF P-PR-NEW-STATE
305300        COMPUTE H-OPER-PR-DOLLAR-THRESHOLD ROUNDED =
305400           ((H-CST-THRESH * H-PR-LABOR-PCT * H-PR-WAGE-INDEX) +
305500            (H-CST-THRESH * H-PR-NONLABOR-PCT * H-OPER-COLA)) *
305600             H-OPER-SHARE-DOLL-THRESHOLD
305700        COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
305800               (H-OPER-DOLLAR-THRESHOLD * H-NAT-PCT) +
305900               (H-OPER-PR-DOLLAR-THRESHOLD * H-REG-PCT).
306000
306100***********************************************************
306200
306300     COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
306400          H-CST-THRESH * H-CAPI-GAF * H-CAPI-LARG-URBAN *
306500          H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA.
306600
306700     IF P-PR-NEW-STATE
306800        COMPUTE H-CAPI-PR-DOLLAR-THRESHOLD ROUNDED =
306900           H-CST-THRESH * H-PR-CAPI-GAF * H-CAPI-LARG-URBAN *
307000           H-CAPI-SHARE-DOLL-THRESHOLD * H-CAPI-COLA
307100        COMPUTE H-CAPI-DOLLAR-THRESHOLD ROUNDED =
307200               (H-CAPI-DOLLAR-THRESHOLD * H-C-NAT-PCT) +
307300               (H-CAPI-PR-DOLLAR-THRESHOLD * H-C-REG-PCT).
307400
307500***********************************************************
307600******NOW INCLUDES UNCOMPENSATED CARE**********************
307700
307800     COMPUTE H-OPER-COST-OUTLIER ROUNDED =
307900         ((H-OPER-FSP-PART * (1 + H-OPER-IME-TEACH))
308000                       +
308100           ((H-OPER-FSP-PART * H-OPER-DSH) * .25))
308200                       +
308300             H-OPER-DOLLAR-THRESHOLD
308400                       +
308500                WK-UNCOMP-CARE-AMOUNT
308600                       +
308700                 H-NEW-TECH-PAY-ADD-ON
308800                       -
308900                 H-NEW-TECH-ADDON-ISLET.
309000
309100     COMPUTE H-CAPI-COST-OUTLIER ROUNDED =
309200      (H-CAPI-FSP-PART * (1 + H-WK-CAPI-IME-TEACH + H-CAPI-DSH))
309300                       +
309400             H-CAPI-DOLLAR-THRESHOLD.
309500
309600     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
309700         MOVE 0 TO H-CAPI-COST-OUTLIER.
309800
309900
310000***********************************************************
310100***  OPERATING COST CALCULATION
310200
310300     COMPUTE H-OPER-BILL-COSTS ROUNDED =
310400         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
310500         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
310600
310700
310800     IF  H-OPER-BILL-COSTS > H-OPER-COST-OUTLIER
310900         COMPUTE H-OPER-OUTCST-PART ROUNDED =
311000         H-CSTOUT-PCT * (H-OPER-BILL-COSTS -
311100                         H-OPER-COST-OUTLIER).
311200
311300     IF PAY-WITHOUT-COST OR
311400        PAY-XFER-NO-COST OR
311500        PAY-XFER-SPEC-DRG-NO-COST
311600         MOVE 0 TO H-OPER-OUTCST-PART.
311700
311800***********************************************************
311900***  CAPITAL COST CALCULATION
312000
312100     COMPUTE H-CAPI-BILL-COSTS ROUNDED =
312200             B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO
312300         ON SIZE ERROR MOVE 0 TO H-CAPI-BILL-COSTS.
312400
312500     IF  H-CAPI-BILL-COSTS > H-CAPI-COST-OUTLIER
312600         COMPUTE H-CAPI-OUTCST-PART ROUNDED =
312700         H-CSTOUT-PCT * (H-CAPI-BILL-COSTS -
312800                         H-CAPI-COST-OUTLIER).
312900
313000***********************************************************
313100***  'A' NOT VALID FY 2015 ON
313200
313300*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
313400*      COMPUTE H-CAPI-OUTCST-PART ROUNDED =
313500*             (H-CAPI-OUTCST-PART * P-NEW-CAPI-NEW-HARM-RATIO).
313600
313700     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
313800        COMPUTE H-CAPI-OUTCST-PART ROUNDED =
313900               (H-CAPI-OUTCST-PART * H-CAPI-PAYCDE-PCT1).
314000
314100     IF (H-CAPI-BILL-COSTS   + H-OPER-BILL-COSTS) <
314200        (H-CAPI-COST-OUTLIER + H-OPER-COST-OUTLIER)
314300        MOVE 0 TO H-CAPI-OUTCST-PART
314400                  H-OPER-OUTCST-PART.
314500
314600     IF PAY-WITHOUT-COST OR
314700        PAY-XFER-NO-COST OR
314800        PAY-XFER-SPEC-DRG-NO-COST
314900         MOVE 0 TO H-CAPI-OUTCST-PART.
315000
315100***********************************************************
315200***  DETERMINES THE BILL TO BE COST  OUTLIER
315300
315400     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
315500         MOVE 0 TO H-CAPI-OUTDAY-PART
315600                   H-CAPI-OUTCST-PART.
315700
315800     IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
315900                 MOVE H-OPER-OUTCST-PART TO
316000                      H-OPER-OUTLIER-PART
316100                 MOVE H-CAPI-OUTCST-PART TO
316200                      H-CAPI-OUTLIER-PART
316300                 MOVE 02 TO PPS-RTC.
316400
316500     IF OUTLIER-RECON-FLAG = 'Y'
316600        IF (H-OPER-OUTCST-PART + H-CAPI-OUTCST-PART) > 0
316700           COMPUTE HLD-PPS-RTC = HLD-PPS-RTC + 30
316800           GO TO 3600-EXIT
316900        ELSE
317000           GO TO 3600-EXIT
317100     ELSE
317200        NEXT SENTENCE.
317300
317400
317500***********************************************************
317600***  DETERMINES IF COST OUTLIER
317700***  RECOMPUTES DOLLAR THRESHOLD TO BE SENT BACK WITH
317800***         RETURN CODE OF 02
317900
318000     MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
318100
318200     IF PPS-RTC = 02
318300       IF H-CAPI-CSTCHG-RATIO > 0 OR
318400          H-OPER-CSTCHG-RATIO > 0
318500             COMPUTE H-OPER-DOLLAR-THRESHOLD ROUNDED =
318600                     (H-CAPI-COST-OUTLIER  +
318700                      H-OPER-COST-OUTLIER)
318800                             /
318900                    (H-CAPI-CSTCHG-RATIO  +
319000                     H-OPER-CSTCHG-RATIO)
319100             ON SIZE ERROR MOVE 0 TO H-OPER-DOLLAR-THRESHOLD
319200       ELSE MOVE 0 TO H-OPER-DOLLAR-THRESHOLD.
319300
319400***********************************************************
319500***  DETERMINES IF COST OUTLIER WITH LOS IS > COVERED  DAYS
319600***         RETURN CODE OF 67
319700
319800     IF PPS-RTC = 02
319900         IF ((H-REG-DAYS + H-LTR-DAYS) < B-LOS) OR
320000            PPS-PC-COT-FLAG = 'Y'
320100             MOVE 67 TO PPS-RTC.
320200***********************************************************
320300
320400***********************************************************
320500***  DETERMINES THE OUTLIER AMOUNT THAT WOULD BE PAID IF
320600***  THE PROVIDER WAS TYPE B-HOLD-HARMLESS 100% FED RATE
320700***********************************************************
320800*
320900***********************************************************
321000***  'A' NOT VALID FY 2015 ON
321100*
321200*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
321300*       COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
321400*               H-CAPI-OUTLIER-PART / P-NEW-CAPI-NEW-HARM-RATIO
321500*        ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
321600
321700     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
321800        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
321900                H-CAPI-OUTLIER-PART.
322000
322100     IF P-NEW-CAPI-PPS-PAY-CODE = 'C' AND
322200        H-CAPI-PAYCDE-PCT1 > 0
322300        COMPUTE H-CAPI2-B-OUTLIER-PART ROUNDED =
322400                H-CAPI-OUTLIER-PART / H-CAPI-PAYCDE-PCT1
322500         ON SIZE ERROR MOVE 0 TO H-CAPI2-B-OUTLIER-PART
322600     ELSE MOVE 0 TO H-CAPI2-B-OUTLIER-PART.
322700
322800 3600-EXIT.   EXIT.
322900
323000***********************************************************
323100 3450-CALC-ADDITIONAL-HSP.
323200***********************************************************
323300*---------------------------------------------------------*
323400* (YEARCHANGE 2016.0)
323500* OBRA 89 CALCULATE ADDITIONAL HSP PAYMENT FOR SOLE COMMUNITY
323600* AND ESSENTIAL ACCESS COMMUNITY HOSPITALS (EACH)
323700* NOW REIMBURSED WITH 100% NATIONAL FEDERAL RATES
323800*---------------------------------------------------------*
323900***  GET THE RBN UPDATING FACTOR
324000
324100*****YEARCHANGE 2013.0 ****************************************
324200     MOVE 0.998431 TO H-BUDG-NUTR130.
324300
324400*****YEARCHANGE 2014.0 ****************************************
324500     MOVE 0.997989 TO H-BUDG-NUTR140.
324600
324700*****YEARCHANGE 2015.1 ****************************************
324800     MOVE 0.998761 TO H-BUDG-NUTR150.
324900
325000*****YEARCHANGE 2016.0 ****************************************
325100
325200     IF B-DISCHARGE-DATE < 20160101
325300        MOVE 0.998405 TO H-BUDG-NUTR160
325400     ELSE
325500        MOVE 0.998404 TO H-BUDG-NUTR160
325600     END-IF.
325700
325800***  GET THE MARKET BASKET UPDATE FACTOR
325900*****YEARCHANGE 2013.0 ****************************************
326000        MOVE 1.0180 TO H-UPDATE-130.
326100
326200*****YEARCHANGE 2014.0 ****************************************
326300        MOVE 1.0170 TO H-UPDATE-140.
326400
326500*****YEARCHANGE 2015.0 ****************************************
326600        MOVE 1.02200 TO H-UPDATE-150.
326700
326800*****YEARCHANGE 2016.0 ****************************************
326900        MOVE 1.01700 TO H-UPDATE-160.
327000
327100
327200*** APPLY APPROPRIATE MARKET BASKET UPDATE FACTOR PER PSF FLAGS
327300*****YEARCHANGE 2016.0 ****************************************
327400     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
327500        P-EHR-REDUC-IND = ' '
327600        MOVE 1.01700 TO H-UPDATE-160.
327700
327800*****YEARCHANGE 2016.0 ****************************************
327900     IF P-NEW-CBSA-HOSP-QUAL-IND = '1' AND
328000        P-EHR-REDUC-IND = 'Y'
328100        MOVE 1.00500 TO H-UPDATE-160.
328200
328300*****YEARCHANGE 2016.0 ****************************************
328400     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
328500        P-EHR-REDUC-IND = ' '
328600        MOVE 1.01100 TO H-UPDATE-160.
328700
328800*****YEARCHANGE 2016.0 ****************************************
328900     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1' AND
329000        P-EHR-REDUC-IND = 'Y'
329100        MOVE 0.99900 TO H-UPDATE-160.
329200
329300
329400*****YEARCHANGE 2016.0 ****************************************
329500     IF P-PR-NEW-STATE
329600        MOVE 1.0170 TO H-UPDATE-160.
329700
329800
329900********YEARCHANGE 2016.0 *************************************
330000*** CASE MIX ADJUSTMENT AS OF FY 2015
330100*** SHORT STAY ADJUSTMENT AS OF FY 2014
330200     MOVE 0.9480 TO H-CASE-MIX-ADJ.
330300     MOVE 0.9980 TO H-SHORT-STAY-ADJ.
330400
330500     COMPUTE H-UPDATE-FACTOR ROUNDED =
330600                       (H-UPDATE-130 *
330700                        H-UPDATE-140 *
330800                        H-UPDATE-150 *
330900                        H-UPDATE-160 *
331000                        H-BUDG-NUTR130 *
331100                        H-BUDG-NUTR140 *
331200                        H-BUDG-NUTR150 *
331300                        H-BUDG-NUTR160 *
331400                        H-CASE-MIX-ADJ * H-SHORT-STAY-ADJ).
331500
331600     COMPUTE H-HSP-RATE ROUNDED =
331700         H-FAC-SPEC-RATE * H-UPDATE-FACTOR * H-DRG-WT.
331800***************************************************************
331900*
332000*    IF P-NEW-CBSA-HOSP-QUAL-IND = '1'
332100*       COMPUTE H-HSP-RATE ROUNDED =
332200*        (H-FAC-SPEC-RATE * 1) * H-UPDATE-FACTOR
332300*    ELSE
332400*       COMPUTE H-HSP-RATE ROUNDED =
332500*        ((H-FAC-SPEC-RATE / 1.036) * 1.016) * H-UPDATE-FACTOR.
332600*
332700***************************************************************
332800********YEARCHANGE 2011.0 *************************************
332900***     OUTLIER OFFSETS NO LONGER USED IN HSP COMPARISON
333000***     WE NOW USE THE ACTUAL OPERATING OUTLIER PAYMEMT
333100***     IN THE HSP COMPARRISON
333200
333300********YEARCHANGE 2014.0 *XXXXXX******************************
333400*      THE HSP BUCKET FOR SCH                      ************
333500*      ADDED UNCOMPENSATED CARE TO COMPARRISON FOR 2014 *******
333600***************************************************************
333700     COMPUTE H-FSP-RATE ROUNDED =
333800        ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
333900         H-NAT-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN)
334000                           *
334100             (1 + H-OPER-IME-TEACH + (H-OPER-DSH * .25))
334200                               +
334300                         H-OPER-OUTLIER-PART
334400                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
334500
334600     IF P-PR-NEW-STATE
334700       COMPUTE H-FSP-RATE ROUNDED =
334800         ((H-NAT-PCT * (H-NAT-LABOR * H-WAGE-INDEX +
334900         H-NAT-NONLABOR * H-OPER-COLA))  * H-DRG-WT-FRCTN)
335000                           *
335100         (1 + H-OPER-IME-TEACH + H-OPER-DSH) +
335200                            H-OPER-OUTLIER-PART
335300                               +
335400        ((H-REG-PCT * (H-REG-LABOR * H-PR-WAGE-INDEX +
335500         H-REG-NONLABOR * H-OPER-COLA)) * H-DRG-WT-FRCTN)
335600                           *
335700             (1 + H-OPER-IME-TEACH +(H-OPER-DSH * .25))
335800                   ON SIZE ERROR MOVE 0 TO H-FSP-RATE.
335900
336000****************************************************************
336100****         INCLUDE UNCOMPENSATED CARE PER CLAIM IN HSP
336200*****        CHOICE
336300
336400     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
336500           COMPUTE H-OPER-HSP-PART ROUNDED =
336600             (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT))
336700                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
336800     ELSE
336900         MOVE 0 TO H-OPER-HSP-PART.
337000
337100***************************************************************
337200***  YEARCHANGE TURNING MDH BACK ON ***************************
337300***************************************************************
337400***  GET THE MDH REBASE
337500
337600     IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
337700         IF P-NEW-PROVIDER-TYPE = '14' OR '15'
337800           COMPUTE H-OPER-HSP-PART ROUNDED =
337900         (H-HSP-RATE - (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)) * .75
338000                   ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART.
338100
338200***************************************************************
338300***  TRANSITIONAL PAYMENT FOR FORMER MDHS
338400
338500     IF  B-FORMER-MDH-PROVIDERS       AND
338600        (B-DISCHARGE-DATE > 20150930  AND
338700         B-DISCHARGE-DATE < 20160101)
338800         MOVE 0 TO H-OPER-HSP-PART
338900         GO TO 3450-EXIT
339000     END-IF.
339100
339200***  HSP PAYMENT FOR CLAIMS BETWEEN 01/01/2016 - 09/30/2016
339300
339400     IF  B-FORMER-MDH-PROVIDERS       AND
339500        (B-DISCHARGE-DATE > 20151231  AND
339600         B-DISCHARGE-DATE < 20161001)
339700       IF  H-HSP-RATE > (H-FSP-RATE + WK-UNCOMP-CARE-AMOUNT)
339800         COMPUTE H-OPER-HSP-PART ROUNDED =
339900           ((H-HSP-RATE - (H-FSP-RATE +
340000               WK-UNCOMP-CARE-AMOUNT))* 0.75)*(2 / 3)
340100             ON SIZE ERROR MOVE 0 TO H-OPER-HSP-PART
340200       END-IF
340300     END-IF.
340400
340500 3450-EXIT.   EXIT.
340600
340700***********************************************************
340800 3800-CALC-TOT-AMT.
340900***********************************************************
341000***  CALCULATE TOTALS FOR CAPITAL
341100
341200     MOVE P-NEW-CAPI-PPS-PAY-CODE  TO H-CAPI2-PAY-CODE.
341300
341400***********************************************************
341500***  'A' NOT VALID FY 2015 ON
341600*
341700*    IF P-NEW-CAPI-PPS-PAY-CODE = 'A'
341800*       MOVE P-NEW-CAPI-NEW-HARM-RATIO TO H-CAPI-FSP-PCT
341900*       MOVE 0.00 TO H-CAPI-HSP-PCT.
342000
342100     IF P-NEW-CAPI-PPS-PAY-CODE = 'B'
342200        MOVE 0    TO H-CAPI-OLD-HARMLESS
342300        MOVE 1.00 TO H-CAPI-FSP-PCT
342400        MOVE 0.00 TO H-CAPI-HSP-PCT.
342500
342600     IF P-NEW-CAPI-PPS-PAY-CODE = 'C'
342700        MOVE 0    TO H-CAPI-OLD-HARMLESS
342800        MOVE H-CAPI-PAYCDE-PCT1 TO H-CAPI-FSP-PCT
342900        MOVE H-CAPI-PAYCDE-PCT2 TO H-CAPI-HSP-PCT.
343000
343100     COMPUTE H-CAPI-HSP ROUNDED =
343200         H-CAPI-HSP-PCT * H-CAPI-HSP-PART.
343300
343400     COMPUTE H-CAPI-FSP ROUNDED =
343500         H-CAPI-FSP-PCT * H-CAPI-FSP-PART.
343600
343700     MOVE P-NEW-CAPI-EXCEPTIONS TO H-CAPI-EXCEPTIONS.
343800
343900     MOVE H-CAPI-OLD-HARMLESS TO H-CAPI-OLD-HARM.
344000
344100     COMPUTE H-CAPI-DSH-ADJ ROUNDED =
344200             H-CAPI-FSP
344300              * H-CAPI-DSH.
344400
344500     COMPUTE H-CAPI-IME-ADJ ROUNDED =
344600          H-CAPI-FSP *
344700                 H-WK-CAPI-IME-TEACH.
344800
344900     COMPUTE H-CAPI-OUTLIER ROUNDED =
345000             1.00 * H-CAPI-OUTLIER-PART.
345100
345200     COMPUTE H-CAPI2-B-FSP ROUNDED =
345300             1.00 * H-CAPI2-B-FSP-PART.
345400
345500     COMPUTE H-CAPI2-B-OUTLIER ROUNDED =
345600             1.00 * H-CAPI2-B-OUTLIER-PART.
345700***********************************************************
345800***  IF CAPITAL IS NOT IN EFFECT FOR GIVEN PROVIDER
345900***        THIS ZEROES OUT ALL CAPITAL DATA
346000
346100     IF (P-NEW-CAPI-NEW-HOSP = 'Y')
346200        MOVE ALL '0' TO HOLD-CAPITAL-VARIABLES.
346300***********************************************************
346400
346500***********************************************************
346600***  CALCULATE FINAL TOTALS FOR OPERATING
346700
346800     IF (H-CAPI-OUTLIER > 0 AND
346900         PPS-OPER-OUTLIER-PART = 0)
347000            COMPUTE PPS-OPER-OUTLIER-PART =
347100                    PPS-OPER-OUTLIER-PART + .01.
347200
347300***********************************************************
347400*LOW VOLUME CALCULATIONS
347500***********************************************************
347600*---------------------------------------------------------*
347700* (YEARCHANGE 2016.0)
347800* LOW VOLUME PAYMENT ADD-ON PERCENT
347900*---------------------------------------------------------*
348000
348100     MOVE 01.000 TO WK-LOW-VOL25PCT.
348200
348300     IF P-NEW-TEMP-RELIEF-IND = 'Y'
348400     MOVE P-NEW-PROVIDER-NO      TO MES-PPS-PROV
348500     PERFORM 4400-LOWVOL-CODE-RTN THRU 4400-EXIT.
348600
348700     IF P-NEW-TEMP-RELIEF-IND = 'Y'
348800     AND MESWK-LOWVOL-PROV-DISCHG <= 200
348900            MOVE 00.250 TO WK-LOW-VOL25PCT
349000            GO TO LOW-VOL-CALC.
349100
349200***  CALCULATE THE LOW VOLUME DISCHARGE PERCENT
349300***  SLIDING SCALE ADD ON FOR
349400
349500     IF P-NEW-TEMP-RELIEF-IND = 'Y'
349600        AND MESWK-LOWVOL-PROV-DISCHG > 200
349700        AND MESWK-LOWVOL-PROV-DISCHG < 1600
349800            COMPUTE  WK-LOW-VOL25PCT ROUNDED =
349900            ((4 / 14) - (MESWK-LOWVOL-PROV-DISCHG / 5600))
350000            GO TO LOW-VOL-CALC
350100     ELSE
350200           MOVE 01.000 TO WK-LOW-VOL25PCT.
350300
350400 LOW-VOL-CALC.
350500
350600     MOVE ZERO TO PPS-OPER-DSH-ADJ.
350700************************************************
350800* FOR FY 2014 WE APPLY AN ADJUSTMENT OF 0.25 TO CALCULATE
350900* EMPERICAL DSH
351000************************************************
351100     IF  H-OPER-DSH NUMERIC
351200         COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
351300                     (PPS-OPER-FSP-PART  * H-OPER-DSH) * .25.
351400
351500     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
351600                         PPS-OPER-FSP-PART * H-OPER-IME-TEACH.
351700
351800
351900     COMPUTE PPS-OPER-FSP-PART ROUNDED =
352000                           H-OPER-FSP-PART * H-OPER-FSP-PCT.
352100
352200     COMPUTE PPS-OPER-HSP-PART ROUNDED =
352300                           H-OPER-HSP-PART * H-OPER-HSP-PCT.
352400
352500     COMPUTE PPS-OPER-OUTLIER-PART ROUNDED =
352600                         H-OPER-OUTLIER-PART * H-OPER-FSP-PCT.
352700
352800     COMPUTE PPS-NEW-TECH-PAY-ADD-ON ROUNDED =
352900                                H-NEW-TECH-PAY-ADD-ON -
353000                                H-NEW-TECH-ADDON-ISLET.
353100
353200     IF  WK-LOW-VOL25PCT < 1.000000
353300     COMPUTE WK-LOW-VOL-ADDON  ROUNDED =
353400       (PPS-OPER-HSP-PART +
353500        PPS-OPER-FSP-PART +
353600        PPS-OPER-IME-ADJ +
353700        PPS-OPER-DSH-ADJ +
353800        PPS-OPER-OUTLIER-PART +
353900        H-CAPI-FSP +
354000        H-CAPI-IME-ADJ +
354100        H-CAPI-DSH-ADJ +
354200        H-CAPI-OUTLIER +
354300        WK-UNCOMP-CARE-AMOUNT +
354400        PPS-NEW-TECH-PAY-ADD-ON) * WK-LOW-VOL25PCT
354500     ELSE
354600     COMPUTE WK-LOW-VOL-ADDON  ROUNDED = 0.
354700
354800 LOW-VOL-END.
354900
355000     COMPUTE H-LOW-VOL-PAYMENT ROUNDED = WK-LOW-VOL-ADDON.
355100
355200     IF HMO-TAG  = 'Y'
355300        PERFORM 3850-HMO-IME-ADJ.
355400
355500***********************************************************
355600***  CALCULATE FINAL TOTALS FOR CAPITAL AND OPERATING
355700
355800     COMPUTE H-CAPI-TOTAL-PAY ROUNDED =
355900             H-CAPI-FSP + H-CAPI-IME-ADJ +
356000             H-CAPI-DSH-ADJ + H-CAPI-OUTLIER.
356100
356200         PERFORM 9000-CALC-EHR-SAVING   THRU 9000-EXIT.
356300         PERFORM 9010-CALC-STANDARD-CHG THRU 9010-EXIT.
356400
356500***********************************************************
356600* HOSPITAL ACQUIRED CONDITION (HAC) PENALTY & REDUCTION FACTOR
356700***********************************************************
356800*---------------------------------------------------------*
356900* (YEARCHANGE 2016.0)
357000* HOSPITAL ACQUIRED CONDITION (HAC) REDUCTION FACTOR
357100*   + FOR FY 2015 AN ADJUSTMENT OF 0.01 TO CALCULATE
357200*     HOSPITAL ACQUIRED CONDITION (HAC) PENALTY
357300*   + BASED ON INDICATOR FROM THE PPS FILE
357400*   + NOT VALID IN PUERTO RICO
357500*   + TOTAL PAYMENT NOW INCLUDES UNCOMPENSATED CARE AMOUNT
357600*---------------------------------------------------------*
357700
357800     COMPUTE WK-HAC-TOTAL-PAYMENT ROUNDED =
357900        PPS-OPER-HSP-PART +
358000        PPS-OPER-FSP-PART +
358100        PPS-OPER-IME-ADJ +
358200        PPS-OPER-DSH-ADJ +
358300        PPS-OPER-OUTLIER-PART +
358400        H-CAPI-TOTAL-PAY +
358500        WK-UNCOMP-CARE-AMOUNT +
358600        PPS-NEW-TECH-PAY-ADD-ON +
358700        WK-LOW-VOL-ADDON +
358800        H-READMIS-ADJUST-AMT +
358900        H-VAL-BASED-PURCH-ADJUST-AMT.
359000
359100     MOVE ZERO TO WK-HAC-AMOUNT.
359200
359300     IF P-PR-NEW-STATE AND
359400        P-HAC-REDUC-IND = 'Y'
359500           MOVE 53 TO PPS-RTC
359600           GO TO 3800-EXIT.
359700
359800     IF  P-HAC-REDUC-IND = 'Y'
359900         COMPUTE   WK-HAC-AMOUNT     ROUNDED =
360000                   WK-HAC-TOTAL-PAYMENT * -0.01
360100     ELSE
360200         COMPUTE   WK-HAC-AMOUNT     ROUNDED = 0.
360300
360400***********************************************************
360500***  TOTAL PAYMENT NOW INCLUDES HAC PENALTY AMOUNT
360600************************************************
360700     COMPUTE   PPS-TOTAL-PAYMENT ROUNDED =
360800                 WK-HAC-TOTAL-PAYMENT
360900                           +
361000                 H-WK-PASS-AMT-PLUS-MISC
361100                           +
361200                 H-BUNDLE-ADJUST-AMT
361300                           +
361400                 WK-HAC-AMOUNT
361500                           +
361600                 H-NEW-TECH-ADDON-ISLET.
361700
361800     MOVE     P-VAL-BASED-PURCH-PARTIPNT TO
361900              H-VAL-BASED-PURCH-PARTIPNT.
362000
362100     MOVE     P-VAL-BASED-PURCH-ADJUST   TO
362200              H-VAL-BASED-PURCH-ADJUST.
362300
362400     MOVE     P-HOSP-READMISSION-REDU    TO
362500              H-HOSP-READMISSION-REDU.
362600
362700     MOVE     P-HOSP-HRR-ADJUSTMT        TO
362800              H-HOSP-HRR-ADJUSTMT.
362900
363000 3800-EXIT.   EXIT.
363100
363200 3850-HMO-IME-ADJ.
363300***********************************************************
363400***  HMO CALC FOR PASS-THRU ADDON
363500
363600     COMPUTE H-WK-PASS-AMT-PLUS-MISC ROUNDED =
363700          (P-NEW-PASS-AMT-PLUS-MISC -
363800          (P-NEW-PASS-AMT-ORGAN-ACQ +
363900           P-NEW-PASS-AMT-DIR-MED-ED)) * B-LOS.
364000
364100***********************************************************
364200***  HMO IME ADJUSTMENT --- NO LONGER PAID AS OF 10/01/2002
364300
364400     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
364500                   PPS-OPER-IME-ADJ * .0.
364600
364700***********************************************************
364800
364900
365000 3900A-CALC-OPER-DSH.
365100
365200***  OPERATING DSH CALCULATION
365300
365400      MOVE 0.0000 TO H-OPER-DSH.
365500
365600      COMPUTE H-WK-OPER-DSH ROUNDED  = (P-NEW-SSI-RATIO
365700                                     + P-NEW-MEDICAID-RATIO).
365800
365900***********************************************************
366000**1**    0-99 BEDS
366100***  NOT TO EXCEED 12%
366200
366300      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
366400                               AND H-WK-OPER-DSH > .1499
366500                               AND H-WK-OPER-DSH < .2020
366600        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
366700                                      * .65 + .025
366800        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
366900
367000      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE < 100
367100                               AND H-WK-OPER-DSH > .2019
367200        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
367300                                      * .825 + .0588
367400        IF H-OPER-DSH > .1200  MOVE .1200 TO H-OPER-DSH.
367500
367600***********************************************************
367700**2**   100 + BEDS
367800***  NO CAP >> CAN EXCEED 12%
367900
368000      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
368100                               AND H-WK-OPER-DSH > .1499
368200                               AND H-WK-OPER-DSH < .2020
368300        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
368400                                      * .65 + .025.
368500
368600      IF (W-CBSA-SIZE = 'O' OR 'L') AND P-NEW-BED-SIZE > 99
368700                               AND H-WK-OPER-DSH > .2019
368800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
368900                                      * .825 + .0588.
369000
369100***********************************************************
369200**3**   OTHER RURAL HOSPITALS LESS THEN 500 BEDS
369300***  NOT TO EXCEED 12%
369400
369500      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
369600                               AND H-WK-OPER-DSH > .1499
369700                               AND H-WK-OPER-DSH < .2020
369800        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
369900                                 * .65 + .025
370000        IF H-OPER-DSH > .1200
370100              MOVE .1200 TO H-OPER-DSH.
370200
370300      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE < 500
370400                               AND H-WK-OPER-DSH > .2019
370500        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
370600                                 * .825 + .0588
370700        IF H-OPER-DSH > .1200
370800                 MOVE .1200 TO H-OPER-DSH.
370900***********************************************************
371000**4**   OTHER RURAL HOSPITALS 500 BEDS +
371100***  NO CAP >> CAN EXCEED 12%
371200
371300      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
371400                               AND H-WK-OPER-DSH > .1499
371500                               AND H-WK-OPER-DSH < .2020
371600        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
371700                                 * .65 + .025.
371800
371900      IF W-CBSA-SIZE = 'R'     AND P-NEW-BED-SIZE > 499
372000                               AND H-WK-OPER-DSH > .2019
372100        COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
372200                                 * .825 + .0588.
372300
372400***********************************************************
372500**7**   RURAL HOSPITALS SCH
372600***  NOT TO EXCEED 12%
372700
372800      IF W-CBSA-SIZE = 'R'
372900         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
373000                               AND H-WK-OPER-DSH > .1499
373100                               AND H-WK-OPER-DSH < .2020
373200         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
373300                                 * .65 + .025
373400        IF H-OPER-DSH > .1200
373500                 MOVE .1200 TO H-OPER-DSH.
373600
373700      IF W-CBSA-SIZE = 'R'
373800         IF (P-NEW-PROVIDER-TYPE = '16' OR '17' OR '21' OR '22')
373900                               AND H-WK-OPER-DSH > .2019
374000         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
374100                                 * .825 + .0588
374200        IF H-OPER-DSH > .1200
374300                 MOVE .1200 TO H-OPER-DSH.
374400
374500***********************************************************
374600**6**   RURAL HOSPITALS RRC   RULE 5 & 6 SAME
374700***  RRC OVERRIDES SCH CAP
374800***  NO CAP >> CAN EXCEED 12%
374900
375000         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
375100                                   '17' OR '22')
375200                               AND H-WK-OPER-DSH > .1499
375300                               AND H-WK-OPER-DSH < .2020
375400         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .15)
375500                                 * .65 + .025.
375600
375700         IF (P-NEW-PROVIDER-TYPE = '07' OR '14' OR '15' OR
375800                                   '17' OR '22')
375900                               AND H-WK-OPER-DSH > .2019
376000         COMPUTE H-OPER-DSH ROUNDED = (H-WK-OPER-DSH - .202)
376100                                 * .825 + .0588.
376200
376300      COMPUTE H-OPER-DSH ROUNDED = H-OPER-DSH * 1.0000.
376400
376500 3900A-EXIT.   EXIT.
376600
376700 4000-CALC-TECH-ADDON.
376800
376900***********************************************************
377000***  CALCULATE TOTALS FOR OPERATING  ADD ON FOR TECH
377100
377200     COMPUTE PPS-OPER-HSP-PART ROUNDED =
377300         H-OPER-HSP-PCT * H-OPER-HSP-PART.
377400
377500     COMPUTE PPS-OPER-FSP-PART ROUNDED =
377600         H-OPER-FSP-PCT * H-OPER-FSP-PART.
377700
377800     MOVE ZERO TO PPS-OPER-DSH-ADJ.
377900
378000     IF  H-OPER-DSH NUMERIC
378100             COMPUTE PPS-OPER-DSH-ADJ ROUNDED =
378200             (PPS-OPER-FSP-PART
378300              * H-OPER-DSH) * .25.
378400
378500     COMPUTE PPS-OPER-IME-ADJ ROUNDED =
378600             PPS-OPER-FSP-PART *
378700             H-OPER-IME-TEACH.
378800
378900     COMPUTE H-BASE-DRG-PAYMENT ROUNDED =
379000             PPS-OPER-FSP-PART +
379100             PPS-OPER-DSH-ADJ + PPS-OPER-IME-ADJ +
379200             WK-UNCOMP-CARE-AMOUNT.
379300
379400***********************************************************
379500***********************************************************
379600* PUT NEW CHECK HERE IF H-NEW-TECH ZERO PERFORM
379700
379800*    IF   B-DIAG-AUTOLITT-DIAG AND
379900*         B-DRG-AUTOLITT-DRG
380000*       PERFORM 4500-AUTOLIT-TECH-ADD-ON THRU 4500-EXIT.
380100
380200***********************************************************
380300*  DIFICID DISCONTINUED FOR FY 2015
380400*    IF   B-NDC-DIFICID-NDC
380500*      PERFORM 4600-DIFICID-TECH-ADD-ON THRU 4600-EXIT.
380600
380700     IF   B-DIAG-ISLET-DIAG1     OR
380800          B-DIAG-ISLET-DIAG2     OR
380900          B-DIAG-ISLET-DIAG3     OR
381000          B-DIAG-ISLET-DIAG4     OR
381100          B-DIAG-ISLET-DIAG5     OR
381200          B-DIAG-ISLET-DIAG6     OR
381300          B-DIAG-ISLET-DIAG7     OR
381400          B-DIAG-ISLET-DIAG8     OR
381500          B-DIAG-ISLET-DIAG9     OR
381600          B-DIAG-ISLET-DIAG10    OR
381700          B-DIAG-ISLET-DIAG11    OR
381800          B-DIAG-ISLET-DIAG12    OR
381900          B-DIAG-ISLET-DIAG13    OR
382000          B-DIAG-ISLET-DIAG14    OR
382100          B-DIAG-ISLET-DIAG15    OR
382200          B-DIAG-ISLET-DIAG16    OR
382300          B-DIAG-ISLET-DIAG17    OR
382400          B-DIAG-ISLET-DIAG18    OR
382500          B-DIAG-ISLET-DIAG19    OR
382600          B-DIAG-ISLET-DIAG20    OR
382700          B-DIAG-ISLET-DIAG21    OR
382800          B-DIAG-ISLET-DIAG22    OR
382900          B-DIAG-ISLET-DIAG23    OR
383000          B-DIAG-ISLET-DIAG24    OR
383100          B-DIAG-ISLET-DIAG25
383200       PERFORM 4100-ISLET-ISOLATION-ADD-ON THRU 4100-EXIT
383300     ELSE
383400       MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET.
383500
383600*    IF   B-PROC-ZENITH-PRIN     OR
383700*         B-PROC-ZENITH-PROC1    OR
383800*         B-PROC-ZENITH-PROC2    OR
383900*         B-PROC-ZENITH-PROC3    OR
384000*         B-PROC-ZENITH-PROC4    OR
384100*         B-PROC-ZENITH-PROC5    OR
384200*         B-PROC-ZENITH-PROC6    OR
384300*         B-PROC-ZENITH-PROC7    OR
384400*         B-PROC-ZENITH-PROC8    OR
384500*         B-PROC-ZENITH-PROC9    OR
384600*         B-PROC-ZENITH-PROC10   OR
384700*         B-PROC-ZENITH-PROC11   OR
384800*         B-PROC-ZENITH-PROC12   OR
384900*         B-PROC-ZENITH-PROC13   OR
385000*         B-PROC-ZENITH-PROC14   OR
385100*         B-PROC-ZENITH-PROC15   OR
385200*         B-PROC-ZENITH-PROC16   OR
385300*         B-PROC-ZENITH-PROC17   OR
385400*         B-PROC-ZENITH-PROC18   OR
385500*         B-PROC-ZENITH-PROC19   OR
385600*         B-PROC-ZENITH-PROC20   OR
385700*         B-PROC-ZENITH-PROC21   OR
385800*         B-PROC-ZENITH-PROC22   OR
385900*         B-PROC-ZENITH-PROC23   OR
386000*         B-PROC-ZENITH-PROC24
386100*      PERFORM 4700-ZENITH-TECH-ADD-ON THRU 4700-EXIT
386200*    ELSE
386300*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZENITH.
386400
386500*    IF   B-PROC-VORAXAZE-PRIN   OR
386600*         B-PROC-VORAXAZE-PROC1  OR
386700*         B-PROC-VORAXAZE-PROC2  OR
386800*         B-PROC-VORAXAZE-PROC3  OR
386900*         B-PROC-VORAXAZE-PROC4  OR
387000*         B-PROC-VORAXAZE-PROC5  OR
387100*         B-PROC-VORAXAZE-PROC6  OR
387200*         B-PROC-VORAXAZE-PROC7  OR
387300*         B-PROC-VORAXAZE-PROC8  OR
387400*         B-PROC-VORAXAZE-PROC9  OR
387500*         B-PROC-VORAXAZE-PROC10 OR
387600*         B-PROC-VORAXAZE-PROC11 OR
387700*         B-PROC-VORAXAZE-PROC12 OR
387800*         B-PROC-VORAXAZE-PROC13 OR
387900*         B-PROC-VORAXAZE-PROC14 OR
388000*         B-PROC-VORAXAZE-PROC15 OR
388100*         B-PROC-VORAXAZE-PROC16 OR
388200*         B-PROC-VORAXAZE-PROC17 OR
388300*         B-PROC-VORAXAZE-PROC18 OR
388400*         B-PROC-VORAXAZE-PROC19 OR
388500*         B-PROC-VORAXAZE-PROC20 OR
388600*         B-PROC-VORAXAZE-PROC21 OR
388700*         B-PROC-VORAXAZE-PROC22 OR
388800*         B-PROC-VORAXAZE-PROC23 OR
388900*         B-PROC-VORAXAZE-PROC24
389000*      PERFORM 4800-VORAXAZE-TECH-ADD-ON THRU 4800-EXIT
389100*    ELSE
389200*      MOVE ZEROES TO H-NEW-TECH-ADDON-VORAXAZE.
389300
389400     IF   B-PROC-ARGUS-PRIN      OR
389500          B-PROC-ARGUS-PROC1     OR
389600          B-PROC-ARGUS-PROC2     OR
389700          B-PROC-ARGUS-PROC3     OR
389800          B-PROC-ARGUS-PROC4     OR
389900          B-PROC-ARGUS-PROC5     OR
390000          B-PROC-ARGUS-PROC6     OR
390100          B-PROC-ARGUS-PROC7     OR
390200          B-PROC-ARGUS-PROC8     OR
390300          B-PROC-ARGUS-PROC9     OR
390400          B-PROC-ARGUS-PROC10    OR
390500          B-PROC-ARGUS-PROC11    OR
390600          B-PROC-ARGUS-PROC12    OR
390700          B-PROC-ARGUS-PROC13    OR
390800          B-PROC-ARGUS-PROC14    OR
390900          B-PROC-ARGUS-PROC15    OR
391000          B-PROC-ARGUS-PROC16    OR
391100          B-PROC-ARGUS-PROC17    OR
391200          B-PROC-ARGUS-PROC18    OR
391300          B-PROC-ARGUS-PROC19    OR
391400          B-PROC-ARGUS-PROC20    OR
391500          B-PROC-ARGUS-PROC21    OR
391600          B-PROC-ARGUS-PROC22    OR
391700          B-PROC-ARGUS-PROC23    OR
391800          B-PROC-ARGUS-PROC24
391900       PERFORM 4810-ARGUS-TECH-ADD-ON THRU 4810-EXIT
392000     ELSE
392100       MOVE ZEROES TO H-NEW-TECH-ADDON-ARGUS.
392200
392300     IF   B-DIAG-KCENTRA-DIAG1   OR
392400          B-DIAG-KCENTRA-DIAG2   OR
392500          B-DIAG-KCENTRA-DIAG3   OR
392600          B-DIAG-KCENTRA-DIAG4   OR
392700          B-DIAG-KCENTRA-DIAG5   OR
392800          B-DIAG-KCENTRA-DIAG6   OR
392900          B-DIAG-KCENTRA-DIAG7   OR
393000          B-DIAG-KCENTRA-DIAG8   OR
393100          B-DIAG-KCENTRA-DIAG9   OR
393200          B-DIAG-KCENTRA-DIAG10  OR
393300          B-DIAG-KCENTRA-DIAG11  OR
393400          B-DIAG-KCENTRA-DIAG12  OR
393500          B-DIAG-KCENTRA-DIAG13  OR
393600          B-DIAG-KCENTRA-DIAG14  OR
393700          B-DIAG-KCENTRA-DIAG15  OR
393800          B-DIAG-KCENTRA-DIAG16  OR
393900          B-DIAG-KCENTRA-DIAG17  OR
394000          B-DIAG-KCENTRA-DIAG18  OR
394100          B-DIAG-KCENTRA-DIAG19  OR
394200          B-DIAG-KCENTRA-DIAG20  OR
394300          B-DIAG-KCENTRA-DIAG21  OR
394400          B-DIAG-KCENTRA-DIAG22  OR
394500          B-DIAG-KCENTRA-DIAG23  OR
394600          B-DIAG-KCENTRA-DIAG24  OR
394700          B-DIAG-KCENTRA-DIAG25
394800       MOVE ZEROES TO H-NEW-TECH-ADDON-KCENTRA
394900     ELSE
395000       PERFORM 4820-KCENTRA-TECH-ADD-ON THRU 4820-EXIT.
395100
395200*    IF   B-PROC-ZILVER-PRIN     OR
395300*         B-PROC-ZILVER-PROC1    OR
395400*         B-PROC-ZILVER-PROC2    OR
395500*         B-PROC-ZILVER-PROC3    OR
395600*         B-PROC-ZILVER-PROC4    OR
395700*         B-PROC-ZILVER-PROC5    OR
395800*         B-PROC-ZILVER-PROC6    OR
395900*         B-PROC-ZILVER-PROC7    OR
396000*         B-PROC-ZILVER-PROC8    OR
396100*         B-PROC-ZILVER-PROC9    OR
396200*         B-PROC-ZILVER-PROC10   OR
396300*         B-PROC-ZILVER-PROC11   OR
396400*         B-PROC-ZILVER-PROC12   OR
396500*         B-PROC-ZILVER-PROC13   OR
396600*         B-PROC-ZILVER-PROC14   OR
396700*         B-PROC-ZILVER-PROC15   OR
396800*         B-PROC-ZILVER-PROC16   OR
396900*         B-PROC-ZILVER-PROC17   OR
397000*         B-PROC-ZILVER-PROC18   OR
397100*         B-PROC-ZILVER-PROC19   OR
397200*         B-PROC-ZILVER-PROC20   OR
397300*         B-PROC-ZILVER-PROC21   OR
397400*         B-PROC-ZILVER-PROC22   OR
397500*         B-PROC-ZILVER-PROC23   OR
397600*         B-PROC-ZILVER-PROC24
397700*      PERFORM 4830-ZILVER-TECH-ADD-ON THRU 4830-EXIT
397800*    ELSE
397900*      MOVE ZEROES TO H-NEW-TECH-ADDON-ZILVER.
398000
398100     IF   B-PROC-CARDIO-PRIN     OR
398200          B-PROC-CARDIO-PROC1    OR
398300          B-PROC-CARDIO-PROC2    OR
398400          B-PROC-CARDIO-PROC3    OR
398500          B-PROC-CARDIO-PROC4    OR
398600          B-PROC-CARDIO-PROC5    OR
398700          B-PROC-CARDIO-PROC6    OR
398800          B-PROC-CARDIO-PROC7    OR
398900          B-PROC-CARDIO-PROC8    OR
399000          B-PROC-CARDIO-PROC9    OR
399100          B-PROC-CARDIO-PROC10   OR
399200          B-PROC-CARDIO-PROC11   OR
399300          B-PROC-CARDIO-PROC12   OR
399400          B-PROC-CARDIO-PROC13   OR
399500          B-PROC-CARDIO-PROC14   OR
399600          B-PROC-CARDIO-PROC15   OR
399700          B-PROC-CARDIO-PROC16   OR
399800          B-PROC-CARDIO-PROC17   OR
399900          B-PROC-CARDIO-PROC18   OR
400000          B-PROC-CARDIO-PROC19   OR
400100          B-PROC-CARDIO-PROC20   OR
400200          B-PROC-CARDIO-PROC21   OR
400300          B-PROC-CARDIO-PROC22   OR
400400          B-PROC-CARDIO-PROC23   OR
400500          B-PROC-CARDIO-PROC24
400600       PERFORM 5010-CARDIO-MEMES-ADD-ON THRU 5010-EXIT
400700     ELSE
400800       MOVE ZEROES TO H-NEW-TECH-ADDON-CARDIO.
400900
401000     IF   B-PROC-MITRACLP-PRIN   OR
401100          B-PROC-MITRACLP-PROC1  OR
401200          B-PROC-MITRACLP-PROC2  OR
401300          B-PROC-MITRACLP-PROC3  OR
401400          B-PROC-MITRACLP-PROC4  OR
401500          B-PROC-MITRACLP-PROC5  OR
401600          B-PROC-MITRACLP-PROC6  OR
401700          B-PROC-MITRACLP-PROC7  OR
401800          B-PROC-MITRACLP-PROC8  OR
401900          B-PROC-MITRACLP-PROC9  OR
402000          B-PROC-MITRACLP-PROC10 OR
402100          B-PROC-MITRACLP-PROC11 OR
402200          B-PROC-MITRACLP-PROC12 OR
402300          B-PROC-MITRACLP-PROC13 OR
402400          B-PROC-MITRACLP-PROC14 OR
402500          B-PROC-MITRACLP-PROC15 OR
402600          B-PROC-MITRACLP-PROC16 OR
402700          B-PROC-MITRACLP-PROC17 OR
402800          B-PROC-MITRACLP-PROC18 OR
402900          B-PROC-MITRACLP-PROC19 OR
403000          B-PROC-MITRACLP-PROC20 OR
403100          B-PROC-MITRACLP-PROC21 OR
403200          B-PROC-MITRACLP-PROC22 OR
403300          B-PROC-MITRACLP-PROC23 OR
403400          B-PROC-MITRACLP-PROC24
403500       PERFORM 5020-MITRA-CLIP-ADD-ON THRU 5020-EXIT
403600     ELSE
403700       MOVE ZEROES TO H-NEW-TECH-ADDON-MITRACLP.
403800
403900     IF   B-PROC-RNSSYS1-PRIN    OR
404000          B-PROC-RNSSYS1-PROC1   OR
404100          B-PROC-RNSSYS1-PROC2   OR
404200          B-PROC-RNSSYS1-PROC3   OR
404300          B-PROC-RNSSYS1-PROC4   OR
404400          B-PROC-RNSSYS1-PROC5   OR
404500          B-PROC-RNSSYS1-PROC6   OR
404600          B-PROC-RNSSYS1-PROC7   OR
404700          B-PROC-RNSSYS1-PROC8   OR
404800          B-PROC-RNSSYS1-PROC9   OR
404900          B-PROC-RNSSYS1-PROC10  OR
405000          B-PROC-RNSSYS1-PROC11  OR
405100          B-PROC-RNSSYS1-PROC12  OR
405200          B-PROC-RNSSYS1-PROC13  OR
405300          B-PROC-RNSSYS1-PROC14  OR
405400          B-PROC-RNSSYS1-PROC15  OR
405500          B-PROC-RNSSYS1-PROC16  OR
405600          B-PROC-RNSSYS1-PROC17  OR
405700          B-PROC-RNSSYS1-PROC18  OR
405800          B-PROC-RNSSYS1-PROC19  OR
405900          B-PROC-RNSSYS1-PROC20  OR
406000          B-PROC-RNSSYS1-PROC21  OR
406100          B-PROC-RNSSYS1-PROC22  OR
406200          B-PROC-RNSSYS1-PROC23  OR
406300          B-PROC-RNSSYS1-PROC24
406400       PERFORM 5030-RNS-SYS-ADD-ON THRU 5030-EXIT
406500     ELSE
406600       MOVE ZEROES TO H-NEW-TECH-ADDON-RNSSYS.
406700
406800     IF   B-PROC-BLINATU-PRIN    OR
406900          B-PROC-BLINATU-PROC1   OR
407000          B-PROC-BLINATU-PROC2   OR
407100          B-PROC-BLINATU-PROC3   OR
407200          B-PROC-BLINATU-PROC4   OR
407300          B-PROC-BLINATU-PROC5   OR
407400          B-PROC-BLINATU-PROC6   OR
407500          B-PROC-BLINATU-PROC7   OR
407600          B-PROC-BLINATU-PROC8   OR
407700          B-PROC-BLINATU-PROC9   OR
407800          B-PROC-BLINATU-PROC10  OR
407900          B-PROC-BLINATU-PROC11  OR
408000          B-PROC-BLINATU-PROC12  OR
408100          B-PROC-BLINATU-PROC13  OR
408200          B-PROC-BLINATU-PROC14  OR
408300          B-PROC-BLINATU-PROC15  OR
408400          B-PROC-BLINATU-PROC16  OR
408500          B-PROC-BLINATU-PROC17  OR
408600          B-PROC-BLINATU-PROC18  OR
408700          B-PROC-BLINATU-PROC19  OR
408800          B-PROC-BLINATU-PROC20  OR
408900          B-PROC-BLINATU-PROC21  OR
409000          B-PROC-BLINATU-PROC22  OR
409100          B-PROC-BLINATU-PROC23  OR
409200          B-PROC-BLINATU-PROC24
409300       PERFORM 4900-BLINATU-TECH-ADD-ON THRU 4900-EXIT
409400     ELSE
409500       MOVE ZEROES TO H-NEW-TECH-ADDON-BLINATU.
409600
409700     IF   B-PROC-LUTONIX-PRIN    OR
409800          B-PROC-LUTONIX-PROC1   OR
409900          B-PROC-LUTONIX-PROC2   OR
410000          B-PROC-LUTONIX-PROC3   OR
410100          B-PROC-LUTONIX-PROC4   OR
410200          B-PROC-LUTONIX-PROC5   OR
410300          B-PROC-LUTONIX-PROC6   OR
410400          B-PROC-LUTONIX-PROC7   OR
410500          B-PROC-LUTONIX-PROC8   OR
410600          B-PROC-LUTONIX-PROC9   OR
410700          B-PROC-LUTONIX-PROC10  OR
410800          B-PROC-LUTONIX-PROC11  OR
410900          B-PROC-LUTONIX-PROC12  OR
411000          B-PROC-LUTONIX-PROC13  OR
411100          B-PROC-LUTONIX-PROC14  OR
411200          B-PROC-LUTONIX-PROC15  OR
411300          B-PROC-LUTONIX-PROC16  OR
411400          B-PROC-LUTONIX-PROC17  OR
411500          B-PROC-LUTONIX-PROC18  OR
411600          B-PROC-LUTONIX-PROC19  OR
411700          B-PROC-LUTONIX-PROC20  OR
411800          B-PROC-LUTONIX-PROC21  OR
411900          B-PROC-LUTONIX-PROC22  OR
412000          B-PROC-LUTONIX-PROC23  OR
412100          B-PROC-LUTONIX-PROC24
412200       PERFORM 4910-LUTONIX-TECH-ADD-ON THRU 4910-EXIT
412300     ELSE
412400       MOVE ZEROES TO H-NEW-TECH-ADDON-LUTONIX.
412500
412600***********************************************************
412700*  ALL NEW TECH MUST BE CALCULATED BEFORE
412800*  5000-CAP-CALC-TECH-ADD-ON
412900***********************************************************
413000     PERFORM 5000-CAP-CALC-TECH-ADD-ON THRU 5000-EXIT.
413100
413200     COMPUTE H-OPER-BASE-DRG-PAY ROUNDED =
413300             H-OPER-FSP-PART +
413400             H-NEW-TECH-PAY-ADD-ON -
413500             H-NEW-TECH-ADDON-ISLET.
413600
413700*
413800 4000-EXIT.    EXIT.
413900***********************************************************
414000
414100 4100-ISLET-ISOLATION-ADD-ON.
414200***********************************************************
414300***  TECHNICAL TRANSPLANTATION OF CELLS
414400***
414500*** CODE 52.85 (ALLTRANSPLANTATION OF CELLS OF ISLETS OF
414600*** ISLETS OF LANGERHAUS) AND
414700*** V70.7 (EXAMINATION OF PARTICIPANT IN CLINICAL TRIAL).
414800*** V70.7 ONE OR MORE TIMES IN PROC-CODES AND 52.85 ONE OR MORE
414900*** TIMES IN ANY OTHER PROC-CODE
415000***********************************************************
415100*** IT WAS DECIDED ON 3/12/07 TO ONLY CHECK FOR 52.85 AND NOT
415200*** V70.7
415300***********************************************************
415400
415500     MOVE 0 TO H-TECH-ADDON-ISLET-CNTR
415600               H-TECH-ADDON-ISLET-CNTR2
415700               H-NEW-TECH-ADDON-ISLET.
415800
415900     IF   B-PROC-ISLET-PRIN      OR
416000          B-PROC-ISLET-PROC1     OR
416100          B-PROC-ISLET-PROC2     OR
416200          B-PROC-ISLET-PROC3     OR
416300          B-PROC-ISLET-PROC4     OR
416400          B-PROC-ISLET-PROC5     OR
416500          B-PROC-ISLET-PROC6     OR
416600          B-PROC-ISLET-PROC7     OR
416700          B-PROC-ISLET-PROC8     OR
416800          B-PROC-ISLET-PROC9     OR
416900          B-PROC-ISLET-PROC10    OR
417000          B-PROC-ISLET-PROC11    OR
417100          B-PROC-ISLET-PROC12    OR
417200          B-PROC-ISLET-PROC13    OR
417300          B-PROC-ISLET-PROC14    OR
417400          B-PROC-ISLET-PROC15    OR
417500          B-PROC-ISLET-PROC16    OR
417600          B-PROC-ISLET-PROC17    OR
417700          B-PROC-ISLET-PROC18    OR
417800          B-PROC-ISLET-PROC19    OR
417900          B-PROC-ISLET-PROC20    OR
418000          B-PROC-ISLET-PROC21    OR
418100          B-PROC-ISLET-PROC22    OR
418200          B-PROC-ISLET-PROC23    OR
418300          B-PROC-ISLET-PROC24
418400           NEXT SENTENCE
418500     ELSE
418600           MOVE ZEROES TO H-NEW-TECH-ADDON-ISLET
418700           GO TO 4100-ADD-TECH-CASES
418800     END-IF.
418900
419000     IF B-PROC-ISLET-PRIN
419100      COMPUTE H-TECH-ADDON-ISLET-CNTR
419200        = H-TECH-ADDON-ISLET-CNTR + 1.
419300
419400     IF B-PROC-ISLET-PROC1
419500      COMPUTE H-TECH-ADDON-ISLET-CNTR
419600        = H-TECH-ADDON-ISLET-CNTR + 1.
419700
419800     IF B-PROC-ISLET-PROC2
419900      COMPUTE H-TECH-ADDON-ISLET-CNTR
420000        = H-TECH-ADDON-ISLET-CNTR + 1.
420100
420200     IF B-PROC-ISLET-PROC3
420300      COMPUTE H-TECH-ADDON-ISLET-CNTR
420400        = H-TECH-ADDON-ISLET-CNTR + 1.
420500
420600     IF B-PROC-ISLET-PROC4
420700      COMPUTE H-TECH-ADDON-ISLET-CNTR
420800        = H-TECH-ADDON-ISLET-CNTR + 1.
420900
421000     IF B-PROC-ISLET-PROC5
421100      COMPUTE H-TECH-ADDON-ISLET-CNTR
421200        = H-TECH-ADDON-ISLET-CNTR + 1.
421300
421400     IF B-PROC-ISLET-PROC6
421500      COMPUTE H-TECH-ADDON-ISLET-CNTR
421600        = H-TECH-ADDON-ISLET-CNTR + 1.
421700
421800     IF B-PROC-ISLET-PROC7
421900      COMPUTE H-TECH-ADDON-ISLET-CNTR
422000        = H-TECH-ADDON-ISLET-CNTR + 1.
422100
422200     IF B-PROC-ISLET-PROC8
422300      COMPUTE H-TECH-ADDON-ISLET-CNTR
422400        = H-TECH-ADDON-ISLET-CNTR + 1.
422500
422600     IF B-PROC-ISLET-PROC9
422700      COMPUTE H-TECH-ADDON-ISLET-CNTR
422800        = H-TECH-ADDON-ISLET-CNTR + 1.
422900
423000     IF B-PROC-ISLET-PROC10
423100      COMPUTE H-TECH-ADDON-ISLET-CNTR
423200        = H-TECH-ADDON-ISLET-CNTR + 1.
423300
423400     IF B-PROC-ISLET-PROC11
423500      COMPUTE H-TECH-ADDON-ISLET-CNTR
423600        = H-TECH-ADDON-ISLET-CNTR + 1.
423700
423800     IF B-PROC-ISLET-PROC12
423900      COMPUTE H-TECH-ADDON-ISLET-CNTR
424000        = H-TECH-ADDON-ISLET-CNTR + 1.
424100
424200     IF B-PROC-ISLET-PROC13
424300      COMPUTE H-TECH-ADDON-ISLET-CNTR
424400        = H-TECH-ADDON-ISLET-CNTR + 1.
424500
424600     IF B-PROC-ISLET-PROC14
424700      COMPUTE H-TECH-ADDON-ISLET-CNTR
424800        = H-TECH-ADDON-ISLET-CNTR + 1.
424900
425000     IF B-PROC-ISLET-PROC15
425100      COMPUTE H-TECH-ADDON-ISLET-CNTR
425200        = H-TECH-ADDON-ISLET-CNTR + 1.
425300
425400     IF B-PROC-ISLET-PROC16
425500      COMPUTE H-TECH-ADDON-ISLET-CNTR
425600        = H-TECH-ADDON-ISLET-CNTR + 1.
425700
425800     IF B-PROC-ISLET-PROC17
425900      COMPUTE H-TECH-ADDON-ISLET-CNTR
426000        = H-TECH-ADDON-ISLET-CNTR + 1.
426100
426200     IF B-PROC-ISLET-PROC18
426300      COMPUTE H-TECH-ADDON-ISLET-CNTR
426400        = H-TECH-ADDON-ISLET-CNTR + 1.
426500
426600     IF B-PROC-ISLET-PROC19
426700      COMPUTE H-TECH-ADDON-ISLET-CNTR
426800        = H-TECH-ADDON-ISLET-CNTR + 1.
426900
427000     IF B-PROC-ISLET-PROC20
427100      COMPUTE H-TECH-ADDON-ISLET-CNTR
427200        = H-TECH-ADDON-ISLET-CNTR + 1.
427300
427400     IF B-PROC-ISLET-PROC21
427500      COMPUTE H-TECH-ADDON-ISLET-CNTR
427600        = H-TECH-ADDON-ISLET-CNTR + 1.
427700
427800     IF B-PROC-ISLET-PROC22
427900      COMPUTE H-TECH-ADDON-ISLET-CNTR
428000        = H-TECH-ADDON-ISLET-CNTR + 1.
428100
428200     IF B-PROC-ISLET-PROC23
428300      COMPUTE H-TECH-ADDON-ISLET-CNTR
428400        = H-TECH-ADDON-ISLET-CNTR + 1.
428500
428600     IF B-PROC-ISLET-PROC24
428700      COMPUTE H-TECH-ADDON-ISLET-CNTR
428800        = H-TECH-ADDON-ISLET-CNTR + 1.
428900
429000
429100     IF  H-TECH-ADDON-ISLET-CNTR = 1
429200     MOVE 18848.00 TO H-NEW-TECH-ADDON-ISLET
429300           GO TO 4100-ADD-TECH-CASES.
429400
429500     IF  H-TECH-ADDON-ISLET-CNTR > 1
429600     MOVE 37696.00 TO H-NEW-TECH-ADDON-ISLET
429700           GO TO 4100-ADD-TECH-CASES.
429800
429900     MOVE 0 TO H-NEW-TECH-ADDON-ISLET.
430000
430100
430200 4100-ADD-TECH-CASES.
430300
430400     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
430500             H-NEW-TECH-PAY-ADD-ON +
430600             H-NEW-TECH-ADDON-ISLET.
430700
430800 4100-EXIT.    EXIT.
430900
431000***********************************************************
431100* THIS IS A SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
431200* DISCHARGE COUNTS.
431300***********************************************************
431400 4400-LOWVOL-CODE-RTN.
431500
431600     SET LOWVOL-IDX TO 1.
431700     SEARCH LOWVOL-TAB VARYING LOWVOL-IDX
431800         AT END
431900           MOVE ' NO LOWVOL PROVIDER FOUND' TO MES-LOWVOL
432000           MOVE 1600 TO  MESWK-LOWVOL-PROV-DISCHG
432100       WHEN WK-LOWVOL-PROV (LOWVOL-IDX) = MES-PPS-PROV
432200         MOVE WK-LOWVOL-PROV-DISCHG(LOWVOL-IDX)
432300                            TO MESWK-LOWVOL-PROV-DISCHG.
432400
432500 4400-EXIT.   EXIT.
432600
432700***********************************************************
432800*
432900****YEARCHANGE 2015.0**************************************
433000* THIS SEARCH FOR LOW VOLUME PROVIDERS BASED ON THEIR
433100* DISCHARGE COUNTS WAS REPLACED BY A FIELD ON THE PSF PROVIDER
433200* FILE
433300***********************************************************
433400 4410-UNCOMP-CARE-CODE-RTN.
433500
433600*    MOVE P-NEW-PROVIDER-NO  TO MES-PPS-PROV.
433700*
433800*    SET UNCOMP-CARE-IDX TO 1.
433900*    SEARCH UNCOMP-CARE-TAB VARYING UNCOMP-CARE-IDX
434000*        AT END
434100*          MOVE 0 TO  WK-UNCOMP-CARE-AMOUNT
434200*      WHEN TB-UNCOMP-CARE-PROV (UNCOMP-CARE-IDX) = MES-PPS-PROV
434300*        MOVE TB-UNCOMP-CARE-AMOUNT (UNCOMP-CARE-IDX)
434400*                           TO WK-UNCOMP-CARE-AMOUNT.
434500*
434600        COMPUTE WK-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
434700
434800        COMPUTE H-UNCOMP-CARE-AMOUNT = P-UNCOMP-CARE-AMOUNT.
434900
435000 4410-EXIT.   EXIT.
435100
435200****YEARCHANGE 2015.0**************************************
435300***********************************************************
435400*
435500*
435600***********************************************************
435700*4500-AUTOLIT-TECH-ADD-ON.
435800***********************************************************
435900***** CASES INVOLVING AUTOLITT PROCESS DECOMPRESSION SYSTEM
436000***********************************************************
436100*
436200*    MOVE 0 TO H-NEW-TECH-ADDON-AUTOLITT
436300*              H-LESSER-AUTOLITT-STOP-1
436400*              H-LESSER-AUTOLITT-STOP-2
436500*              H-CSTMED-AUTOLITT-STOP.
436600*
436700*
436800*
436900*    IF '1761   ' =  B-PRIN-PROC-CODE     OR
437000*                    B-OTHER-PROC-CODE1   OR
437100*                    B-OTHER-PROC-CODE2   OR
437200*                    B-OTHER-PROC-CODE3   OR
437300*                    B-OTHER-PROC-CODE4   OR
437400*                    B-OTHER-PROC-CODE5   OR
437500*                    B-OTHER-PROC-CODE6   OR
437600*                    B-OTHER-PROC-CODE7   OR
437700*                    B-OTHER-PROC-CODE8   OR
437800*                    B-OTHER-PROC-CODE9   OR
437900*                    B-OTHER-PROC-CODE10  OR
438000*                    B-OTHER-PROC-CODE11  OR
438100*                    B-OTHER-PROC-CODE12  OR
438200*                    B-OTHER-PROC-CODE13  OR
438300*                    B-OTHER-PROC-CODE14  OR
438400*                    B-OTHER-PROC-CODE15  OR
438500*                    B-OTHER-PROC-CODE16  OR
438600*                    B-OTHER-PROC-CODE17  OR
438700*                    B-OTHER-PROC-CODE18  OR
438800*                    B-OTHER-PROC-CODE19  OR
438900*                    B-OTHER-PROC-CODE20  OR
439000*                    B-OTHER-PROC-CODE21  OR
439100*                    B-OTHER-PROC-CODE22  OR
439200*                    B-OTHER-PROC-CODE23  OR
439300*                    B-OTHER-PROC-CODE24
439400*          GO TO 4500-COMPUTE-AUTOLITT
439500*    ELSE
439600*          NEXT SENTENCE.
439700*
439800*          MOVE ZEROES TO H-NEW-TECH-ADDON-AUTOLITT-STOP.
439900*          GO TO 4500-ADD-TECH-CASES.
440000*
440100*4500-COMPUTE-AUTOLITT.
440200*
440300*    MOVE  5300.00 TO H-CSTMED-AUTOLITT-STOP.
440400*
440500*    COMPUTE H-LESSER-AUTOLITT-STOP-1 ROUNDED =
440600*                 H-CSTMED-AUTOLITT-STOP.
440700*
440800*    COMPUTE H-LESSER-AUTOLITT-STOP-2 ROUNDED =
440900*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
441000*                    H-BASE-DRG-PAYMENT)) * .5.
441100*
441200*    IF H-LESSER-AUTOLITT-STOP-2 > 0
441300*       IF H-LESSER-AUTOLITT-STOP-1 < H-LESSER-AUTOLITT-STOP-2
441400*        MOVE H-LESSER-AUTOLITT-STOP-1 TO
441500*                               H-NEW-TECH-ADDON-AUTOLITT-STOP
441600*       ELSE
441700*        MOVE H-LESSER-AUTOLITT-STOP-2 TO
441800*                               H-NEW-TECH-ADDON-AUTOLITT-STOP
441900*    ELSE
442000*       MOVE ZEROES          TO H-NEW-TECH-ADDON-AUTOLITT-STOP.
442100*
442200*
442300*4500-ADD-TECH-CASES.
442400*
442500*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
442600*            H-NEW-TECH-PAY-ADD-ON +
442700*            H-NEW-TECH-ADDON-AUTOLITT-STOP.
442800*
442900*4500-EXIT.    EXIT.
443000*
443100*
443200***********************************************************
443300*4600-DIFICID-TECH-ADD-ON.
443400***********************************************************
443500***** CASES INVOLVING DIFICID PROCESS DECOMPRESSION SYSTEM
443600***********************************************************
443700***** PER HPAR 8041H TEST WAS CHANGED FR 0845 TO 00845
443800***********************************************************
443900*
444000*    MOVE 0 TO H-NEW-TECH-ADDON-DIFICID
444100*              H-LESSER-DIFICID-STOP-1
444200*              H-LESSER-DIFICID-STOP-2
444300*              H-CSTMED-DIFICID-STOP.
444400*
444500*    IF '00845  ' =  B-OTHER-DIAG-CODE1   OR
444600*                    B-OTHER-DIAG-CODE2   OR
444700*                    B-OTHER-DIAG-CODE3   OR
444800*                    B-OTHER-DIAG-CODE4   OR
444900*                    B-OTHER-DIAG-CODE5   OR
445000*                    B-OTHER-DIAG-CODE6   OR
445100*                    B-OTHER-DIAG-CODE7   OR
445200*                    B-OTHER-DIAG-CODE8   OR
445300*                    B-OTHER-DIAG-CODE9   OR
445400*                    B-OTHER-DIAG-CODE10  OR
445500*                    B-OTHER-DIAG-CODE11  OR
445600*                    B-OTHER-DIAG-CODE12  OR
445700*                    B-OTHER-DIAG-CODE13  OR
445800*                    B-OTHER-DIAG-CODE14  OR
445900*                    B-OTHER-DIAG-CODE15  OR
446000*                    B-OTHER-DIAG-CODE16  OR
446100*                    B-OTHER-DIAG-CODE17  OR
446200*                    B-OTHER-DIAG-CODE18  OR
446300*                    B-OTHER-DIAG-CODE19  OR
446400*                    B-OTHER-DIAG-CODE20  OR
446500*                    B-OTHER-DIAG-CODE21  OR
446600*                    B-OTHER-DIAG-CODE22  OR
446700*                    B-OTHER-DIAG-CODE23  OR
446800*                    B-OTHER-DIAG-CODE24  OR
446900*                    B-OTHER-DIAG-CODE25
447000*          GO TO 4600-COMPUTE-DIFICID
447100*    ELSE
447200*          NEXT SENTENCE.
447300*
447400*          MOVE ZEROES TO H-NEW-TECH-ADDON-DIFICID.
447500*          GO TO 4600-ADD-TECH-CASES.
447600*
447700*4600-COMPUTE-DIFICID.
447800*
447900*    MOVE  868.00 TO H-CSTMED-DIFICID-STOP.
448000*
448100*    COMPUTE H-LESSER-DIFICID-STOP-1 ROUNDED =
448200*                 H-CSTMED-DIFICID-STOP.
448300*
448400*    COMPUTE H-LESSER-DIFICID-STOP-2 ROUNDED =
448500*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
448600*                    H-BASE-DRG-PAYMENT)) * .5.
448700*
448800*    IF H-LESSER-DIFICID-STOP-2 > 0
448900*       IF H-LESSER-DIFICID-STOP-1 < H-LESSER-DIFICID-STOP-2
449000*        MOVE H-LESSER-DIFICID-STOP-1 TO
449100*                               H-NEW-TECH-ADDON-DIFICID
449200*       ELSE
449300*        MOVE H-LESSER-DIFICID-STOP-2 TO
449400*                               H-NEW-TECH-ADDON-DIFICID
449500*    ELSE
449600*       MOVE ZEROES          TO H-NEW-TECH-ADDON-DIFICID.
449700*
449800*
449900*
450000*4600-ADD-TECH-CASES.
450100*
450200*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
450300*            H-NEW-TECH-PAY-ADD-ON +
450400*            H-NEW-TECH-ADDON-DIFICID.
450500*
450600*4600-EXIT.    EXIT.
450700
450800***********************************************************
450900*4700-ZENITH-TECH-ADD-ON.
451000***********************************************************
451100***** CASES INVOLVING ZENITH PROCESS DECOMPRESSION SYSTEM
451200***********************************************************
451300*
451400*    MOVE 0 TO H-NEW-TECH-ADDON-ZENITH
451500*              H-LESSER-ZENITH-STOP-1
451600*              H-LESSER-ZENITH-STOP-2
451700*              H-CSTMED-ZENITH-STOP.
451800*
451900*4700-COMPUTE-ZENITH.
452000*
452100*    MOVE  8171.50 TO H-CSTMED-ZENITH-STOP.
452200*
452300*    COMPUTE H-LESSER-ZENITH-STOP-1 ROUNDED =
452400*                 H-CSTMED-ZENITH-STOP.
452500*
452600*    COMPUTE H-LESSER-ZENITH-STOP-2 ROUNDED =
452700*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
452800*                    H-BASE-DRG-PAYMENT)) * .5.
452900*
453000*    IF H-LESSER-ZENITH-STOP-2 > 0
453100*       IF H-LESSER-ZENITH-STOP-1 < H-LESSER-ZENITH-STOP-2
453200*        MOVE H-LESSER-ZENITH-STOP-1 TO
453300*                               H-NEW-TECH-ADDON-ZENITH
453400*       ELSE
453500*        MOVE H-LESSER-ZENITH-STOP-2 TO
453600*                               H-NEW-TECH-ADDON-ZENITH
453700*    ELSE
453800*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZENITH.
453900*
454000*
454100*4700-ADD-TECH-CASES.
454200*
454300*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
454400*            H-NEW-TECH-PAY-ADD-ON +
454500*            H-NEW-TECH-ADDON-ZENITH.
454600*
454700*4700-EXIT.    EXIT.
454800
454900***********************************************************
455000*4800-VORAXAZE-TECH-ADD-ON.
455100***********************************************************
455200***** CASES INVOLVING VORAXAZE PROCESS DECOMPRESSION SYSTEM
455300***********************************************************
455400*
455500*    MOVE 0 TO H-NEW-TECH-ADDON-VORAXAZE
455600*              H-LESSER-VORAXAZE-STOP-1
455700*              H-LESSER-VORAXAZE-STOP-2
455800*              H-CSTMED-VORAXAZE-STOP.
455900*
456000*4800-COMPUTE-VORAXAZE.
456100*
456200*    MOVE  47250.00 TO H-CSTMED-VORAXAZE-STOP.
456300*
456400*    COMPUTE H-LESSER-VORAXAZE-STOP-1 ROUNDED =
456500*                 H-CSTMED-VORAXAZE-STOP.
456600*
456700*    COMPUTE H-LESSER-VORAXAZE-STOP-2 ROUNDED =
456800*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
456900*                    H-BASE-DRG-PAYMENT)) * .5.
457000*
457100*    IF H-LESSER-VORAXAZE-STOP-2 > 0
457200*       IF H-LESSER-VORAXAZE-STOP-1 < H-LESSER-VORAXAZE-STOP-2
457300*        MOVE H-LESSER-VORAXAZE-STOP-1 TO
457400*                               H-NEW-TECH-ADDON-VORAXAZE
457500*       ELSE
457600*        MOVE H-LESSER-VORAXAZE-STOP-2 TO
457700*                               H-NEW-TECH-ADDON-VORAXAZE
457800*    ELSE
457900*       MOVE ZEROES          TO H-NEW-TECH-ADDON-VORAXAZE.
458000*
458100*
458200*4800-ADD-TECH-CASES.
458300*
458400*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
458500*            H-NEW-TECH-PAY-ADD-ON +
458600*            H-NEW-TECH-ADDON-VORAXAZE.
458700*
458800*4800-EXIT.    EXIT.
458900
459000***********************************************************
459100 4810-ARGUS-TECH-ADD-ON.
459200***********************************************************
459300***** CASES INVOLVING VORAXAZE PROCESS DECOMPRESSION SYSTEM
459400***********************************************************
459500
459600     MOVE 0 TO H-NEW-TECH-ADDON-ARGUS
459700               H-LESSER-ARGUS-STOP-1
459800               H-LESSER-ARGUS-STOP-2
459900               H-CSTMED-ARGUS-STOP.
460000
460100 4810-COMPUTE-ARGUS.
460200
460300     MOVE  72028.75 TO H-CSTMED-ARGUS-STOP.
460400
460500     COMPUTE H-LESSER-ARGUS-STOP-1 ROUNDED =
460600                  H-CSTMED-ARGUS-STOP.
460700
460800     COMPUTE H-LESSER-ARGUS-STOP-2 ROUNDED =
460900          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
461000                     H-BASE-DRG-PAYMENT)) * .5.
461100
461200     IF H-LESSER-ARGUS-STOP-2 > 0
461300        IF H-LESSER-ARGUS-STOP-1 < H-LESSER-ARGUS-STOP-2
461400         MOVE H-LESSER-ARGUS-STOP-1 TO
461500                                H-NEW-TECH-ADDON-ARGUS
461600        ELSE
461700         MOVE H-LESSER-ARGUS-STOP-2 TO
461800                                H-NEW-TECH-ADDON-ARGUS
461900     ELSE
462000        MOVE ZEROES          TO H-NEW-TECH-ADDON-ARGUS.
462100
462200
462300 4810-ADD-TECH-CASES.
462400
462500     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
462600             H-NEW-TECH-PAY-ADD-ON +
462700             H-NEW-TECH-ADDON-ARGUS.
462800*
462900 4810-EXIT.    EXIT.
463000
463100
463200***********************************************************
463300 4820-KCENTRA-TECH-ADD-ON.
463400***********************************************************
463500***** CASES INVOLVING VORAXAZE PROCESS DECOMPRESSION SYSTEM
463600***********************************************************
463700
463800     MOVE 0 TO H-NEW-TECH-ADDON-KCENTRA
463900               H-LESSER-KCENTRA-STOP-1
464000               H-LESSER-KCENTRA-STOP-2
464100               H-CSTMED-KCENTRA-STOP.
464200
464300
464400     IF   B-PROC-KCENTRA-PRIN   OR
464500          B-PROC-KCENTRA-PROC1  OR
464600          B-PROC-KCENTRA-PROC2  OR
464700          B-PROC-KCENTRA-PROC3  OR
464800          B-PROC-KCENTRA-PROC4  OR
464900          B-PROC-KCENTRA-PROC5  OR
465000          B-PROC-KCENTRA-PROC6  OR
465100          B-PROC-KCENTRA-PROC7  OR
465200          B-PROC-KCENTRA-PROC8  OR
465300          B-PROC-KCENTRA-PROC9  OR
465400          B-PROC-KCENTRA-PROC10 OR
465500          B-PROC-KCENTRA-PROC11 OR
465600          B-PROC-KCENTRA-PROC12 OR
465700          B-PROC-KCENTRA-PROC13 OR
465800          B-PROC-KCENTRA-PROC14 OR
465900          B-PROC-KCENTRA-PROC15 OR
466000          B-PROC-KCENTRA-PROC16 OR
466100          B-PROC-KCENTRA-PROC17 OR
466200          B-PROC-KCENTRA-PROC18 OR
466300          B-PROC-KCENTRA-PROC19 OR
466400          B-PROC-KCENTRA-PROC20 OR
466500          B-PROC-KCENTRA-PROC21 OR
466600          B-PROC-KCENTRA-PROC22 OR
466700          B-PROC-KCENTRA-PROC23 OR
466800          B-PROC-KCENTRA-PROC24
466900           GO TO 4820-COMPUTE-KCENTRA
467000     ELSE
467100           NEXT SENTENCE.
467200
467300           MOVE ZEROES TO H-NEW-TECH-ADDON-KCENTRA.
467400           GO TO 4820-ADD-TECH-CASES.
467500
467600 4820-COMPUTE-KCENTRA.
467700
467800     MOVE  01587.50 TO H-CSTMED-KCENTRA-STOP.
467900
468000     COMPUTE H-LESSER-KCENTRA-STOP-1 ROUNDED =
468100                  H-CSTMED-KCENTRA-STOP.
468200
468300     COMPUTE H-LESSER-KCENTRA-STOP-2 ROUNDED =
468400          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
468500                     H-BASE-DRG-PAYMENT)) * .5.
468600
468700     IF H-LESSER-KCENTRA-STOP-2 > 0
468800        IF H-LESSER-KCENTRA-STOP-1 < H-LESSER-KCENTRA-STOP-2
468900         MOVE H-LESSER-KCENTRA-STOP-1 TO
469000                                H-NEW-TECH-ADDON-KCENTRA
469100        ELSE
469200         MOVE H-LESSER-KCENTRA-STOP-2 TO
469300                                H-NEW-TECH-ADDON-KCENTRA
469400     ELSE
469500        MOVE ZEROES          TO H-NEW-TECH-ADDON-KCENTRA.
469600
469700
469800 4820-ADD-TECH-CASES.
469900
470000     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
470100             H-NEW-TECH-PAY-ADD-ON +
470200             H-NEW-TECH-ADDON-KCENTRA.
470300*
470400 4820-EXIT.    EXIT.
470500
470600
470700***********************************************************
470800*4830-ZILVER-TECH-ADD-ON.
470900***********************************************************
471000***** CASES INVOLVING VORAXAZE PROCESS DECOMPRESSION SYSTEM
471100***********************************************************
471200*
471300*    MOVE 0 TO H-NEW-TECH-ADDON-ZILVER
471400*              H-LESSER-ZILVER-STOP-1
471500*              H-LESSER-ZILVER-STOP-2
471600*              H-CSTMED-ZILVER-STOP.
471700*
471800*4830-COMPUTE-ZILVER.
471900*
472000*    MOVE  01705.25 TO H-CSTMED-ZILVER-STOP.
472100*
472200*    COMPUTE H-LESSER-ZILVER-STOP-1 ROUNDED =
472300*                 H-CSTMED-ZILVER-STOP.
472400*
472500*    COMPUTE H-LESSER-ZILVER-STOP-2 ROUNDED =
472600*         (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
472700*                    H-BASE-DRG-PAYMENT)) * .5.
472800*
472900*    IF H-LESSER-ZILVER-STOP-2 > 0
473000*       IF H-LESSER-ZILVER-STOP-1 < H-LESSER-ZILVER-STOP-2
473100*        MOVE H-LESSER-ZILVER-STOP-1 TO
473200*                               H-NEW-TECH-ADDON-ZILVER
473300*       ELSE
473400*        MOVE H-LESSER-ZILVER-STOP-2 TO
473500*                               H-NEW-TECH-ADDON-ZILVER
473600*    ELSE
473700*       MOVE ZEROES          TO H-NEW-TECH-ADDON-ZILVER.
473800*
473900*
474000*4830-ADD-TECH-CASES.
474100*
474200*    COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
474300*            H-NEW-TECH-PAY-ADD-ON +
474400*            H-NEW-TECH-ADDON-ZILVER.
474500*
474600*4830-EXIT.    EXIT.
474700
474800
474900***********************************************************
475000 5000-CAP-CALC-TECH-ADD-ON.
475100***********************************************************
475200***** CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM
475300***********************************************************
475400
475500     MOVE 0 TO H-NEW-TECH-ADDON-CAP.
475600     MOVE 0 TO H-NEW-TECH-ADDON-CAPDIF.
475700
475800     COMPUTE H-OPER-BILL-COSTS ROUNDED =
475900         B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO
476000         ON SIZE ERROR MOVE 0 TO H-OPER-BILL-COSTS.
476100
476200     COMPUTE H-NEW-TECH-ADDON-CAP ROUNDED =
476300                 (H-BASE-DRG-PAYMENT + H-NEW-TECH-PAY-ADD-ON).
476400
476500     COMPUTE H-NEW-TECH-ADDON-CAPDIF ROUNDED =
476600                 (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
476700
476800     IF (H-NEW-TECH-ADDON-CAP > H-OPER-BILL-COSTS) AND
476900         H-NEW-TECH-ADDON-CAPDIF  > 0
477000        COMPUTE H-NEW-TECH-PAY-ADD-ON  ROUNDED =
477100             (H-OPER-BILL-COSTS - H-BASE-DRG-PAYMENT).
477200
477300*
477400 5000-EXIT.    EXIT.
477500
477600***********************************************************
477700 5010-CARDIO-MEMES-ADD-ON.
477800***********************************************************
477900***** CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM
478000***********************************************************
478100
478200     MOVE 0 TO H-NEW-TECH-ADDON-CARDIO
478300               H-LESSER-CARDIO-STOP-1
478400               H-LESSER-CARDIO-STOP-2
478500               H-CSTMED-CARDIO-STOP.
478600
478700 5010-COMPUTE-CARDIO.
478800
478900     MOVE  08875.00 TO H-CSTMED-CARDIO-STOP.
479000
479100     COMPUTE H-LESSER-CARDIO-STOP-1 ROUNDED =
479200                  H-CSTMED-CARDIO-STOP.
479300
479400     COMPUTE H-LESSER-CARDIO-STOP-2 ROUNDED =
479500          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
479600                     H-BASE-DRG-PAYMENT)) * .5.
479700
479800     IF H-LESSER-CARDIO-STOP-2 > 0
479900        IF H-LESSER-CARDIO-STOP-1 < H-LESSER-CARDIO-STOP-2
480000         MOVE H-LESSER-CARDIO-STOP-1 TO
480100                                H-NEW-TECH-ADDON-CARDIO
480200        ELSE
480300         MOVE H-LESSER-CARDIO-STOP-2 TO
480400                                H-NEW-TECH-ADDON-CARDIO
480500     ELSE
480600        MOVE ZEROES          TO H-NEW-TECH-ADDON-CARDIO.
480700
480800
480900 5010-ADD-TECH-CASES.
481000
481100     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
481200             H-NEW-TECH-PAY-ADD-ON +
481300             H-NEW-TECH-ADDON-CARDIO.
481400*
481500 5010-EXIT.    EXIT.
481600***********************************************************
481700***********************************************************
481800 5020-MITRA-CLIP-ADD-ON.
481900***********************************************************
482000***** CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM
482100***********************************************************
482200
482300     MOVE 0 TO H-NEW-TECH-ADDON-MITRACLP
482400               H-LESSER-MITRACLP-STOP-1
482500               H-LESSER-MITRACLP-STOP-2
482600               H-CSTMED-MITRACLP-STOP.
482700
482800 5020-COMPUTE-MITRACLP.
482900
483000     MOVE  15000.00 TO H-CSTMED-MITRACLP-STOP.
483100
483200     COMPUTE H-LESSER-MITRACLP-STOP-1 ROUNDED =
483300                  H-CSTMED-MITRACLP-STOP.
483400
483500     COMPUTE H-LESSER-MITRACLP-STOP-2 ROUNDED =
483600          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
483700                     H-BASE-DRG-PAYMENT)) * .5.
483800
483900     IF H-LESSER-MITRACLP-STOP-2 > 0
484000        IF H-LESSER-MITRACLP-STOP-1 < H-LESSER-MITRACLP-STOP-2
484100         MOVE H-LESSER-MITRACLP-STOP-1 TO
484200                                H-NEW-TECH-ADDON-MITRACLP
484300        ELSE
484400         MOVE H-LESSER-MITRACLP-STOP-2 TO
484500                                H-NEW-TECH-ADDON-MITRACLP
484600     ELSE
484700        MOVE ZEROES          TO H-NEW-TECH-ADDON-MITRACLP.
484800
484900
485000 5020-ADD-TECH-CASES.
485100
485200     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
485300             H-NEW-TECH-PAY-ADD-ON +
485400             H-NEW-TECH-ADDON-MITRACLP.
485500*
485600 5020-EXIT.    EXIT.
485700
485800***********************************************************
485900***********************************************************
486000 5030-RNS-SYS-ADD-ON.
486100***********************************************************
486200***** CASES INVOLVING TECH-CAP-CALC PROCESS DECOMPRESSION SYSTEM
486300***********************************************************
486400
486500     MOVE 0 TO H-NEW-TECH-ADDON-RNSSYS
486600               H-LESSER-RNSSYS-STOP-1
486700               H-LESSER-RNSSYS-STOP-2
486800               H-CSTMED-RNSSYS-STOP.
486900
487000     IF   B-PROC-RNSSYS2-PRIN    OR
487100          B-PROC-RNSSYS2-PROC1 OR
487200          B-PROC-RNSSYS2-PROC2 OR
487300          B-PROC-RNSSYS2-PROC3 OR
487400          B-PROC-RNSSYS2-PROC4 OR
487500          B-PROC-RNSSYS2-PROC5 OR
487600          B-PROC-RNSSYS2-PROC6 OR
487700          B-PROC-RNSSYS2-PROC7 OR
487800          B-PROC-RNSSYS2-PROC8 OR
487900          B-PROC-RNSSYS2-PROC9 OR
488000          B-PROC-RNSSYS2-PROC10 OR
488100          B-PROC-RNSSYS2-PROC11 OR
488200          B-PROC-RNSSYS2-PROC12 OR
488300          B-PROC-RNSSYS2-PROC13 OR
488400          B-PROC-RNSSYS2-PROC14 OR
488500          B-PROC-RNSSYS2-PROC15 OR
488600          B-PROC-RNSSYS2-PROC16 OR
488700          B-PROC-RNSSYS2-PROC17 OR
488800          B-PROC-RNSSYS2-PROC18 OR
488900          B-PROC-RNSSYS2-PROC19 OR
489000          B-PROC-RNSSYS2-PROC20 OR
489100          B-PROC-RNSSYS2-PROC21 OR
489200          B-PROC-RNSSYS2-PROC22 OR
489300          B-PROC-RNSSYS2-PROC23 OR
489400          B-PROC-RNSSYS2-PROC24
489500           GO TO 5030-COMPUTE-RNSSYS
489600     ELSE
489700           NEXT SENTENCE.
489800
489900           MOVE ZEROES TO H-NEW-TECH-ADDON-RNSSYS.
490000           GO TO 5030-ADD-TECH-CASES.
490100
490200 5030-COMPUTE-RNSSYS.
490300
490400     MOVE  18475.00 TO H-CSTMED-RNSSYS-STOP.
490500
490600     COMPUTE H-LESSER-RNSSYS-STOP-1 ROUNDED =
490700                  H-CSTMED-RNSSYS-STOP.
490800
490900     COMPUTE H-LESSER-RNSSYS-STOP-2 ROUNDED =
491000          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
491100                     H-BASE-DRG-PAYMENT)) * .5.
491200
491300     IF H-LESSER-RNSSYS-STOP-2 > 0
491400        IF H-LESSER-RNSSYS-STOP-1 < H-LESSER-RNSSYS-STOP-2
491500         MOVE H-LESSER-RNSSYS-STOP-1 TO
491600                                H-NEW-TECH-ADDON-RNSSYS
491700        ELSE
491800         MOVE H-LESSER-RNSSYS-STOP-2 TO
491900                                H-NEW-TECH-ADDON-RNSSYS
492000     ELSE
492100        MOVE ZEROES          TO H-NEW-TECH-ADDON-RNSSYS.
492200
492300
492400 5030-ADD-TECH-CASES.
492500
492600     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
492700             H-NEW-TECH-PAY-ADD-ON +
492800             H-NEW-TECH-ADDON-RNSSYS.
492900*
493000*
493100 5030-EXIT.    EXIT.
493200
493300***********************************************************
493400 4900-BLINATU-TECH-ADD-ON.
493500***********************************************************
493600***** CASES INVOLVING BLINATUMOMAB
493700***********************************************************
493800
493900     MOVE 0 TO H-NEW-TECH-ADDON-BLINATU
494000               H-LESSER-BLINATU-STOP-1
494100               H-LESSER-BLINATU-STOP-2
494200               H-CSTMED-BLINATU-STOP.
494300
494400 4900-COMPUTE-BLINATU.
494500
494600     MOVE  27017.85 TO H-CSTMED-BLINATU-STOP.
494700
494800     COMPUTE H-LESSER-BLINATU-STOP-1 ROUNDED =
494900                  H-CSTMED-BLINATU-STOP.
495000
495100     COMPUTE H-LESSER-BLINATU-STOP-2 ROUNDED =
495200          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
495300                     H-BASE-DRG-PAYMENT)) * .5.
495400
495500     IF H-LESSER-BLINATU-STOP-2 > 0
495600        IF H-LESSER-BLINATU-STOP-1 < H-LESSER-BLINATU-STOP-2
495700         MOVE H-LESSER-BLINATU-STOP-1 TO
495800                                H-NEW-TECH-ADDON-BLINATU
495900        ELSE
496000         MOVE H-LESSER-BLINATU-STOP-2 TO
496100                                H-NEW-TECH-ADDON-BLINATU
496200     ELSE
496300        MOVE ZEROES          TO H-NEW-TECH-ADDON-BLINATU.
496400
496500
496600 4900-ADD-TECH-CASES.
496700
496800     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
496900             H-NEW-TECH-PAY-ADD-ON +
497000             H-NEW-TECH-ADDON-BLINATU.
497100*
497200 4900-EXIT.    EXIT.
497300
497400
497500***********************************************************
497600 4910-LUTONIX-TECH-ADD-ON.
497700***********************************************************
497800***** CASES INVOLVING VORAXAZE PROCESS DECOMPRESSION SYSTEM
497900***********************************************************
498000
498100     MOVE 0 TO H-NEW-TECH-ADDON-LUTONIX
498200               H-LESSER-LUTONIX-STOP-1
498300               H-LESSER-LUTONIX-STOP-2
498400               H-CSTMED-LUTONIX-STOP.
498500
498600 4910-COMPUTE-LUTONIX.
498700
498800     MOVE  01035.72 TO H-CSTMED-LUTONIX-STOP.
498900
499000     COMPUTE H-LESSER-LUTONIX-STOP-1 ROUNDED =
499100                  H-CSTMED-LUTONIX-STOP.
499200
499300     COMPUTE H-LESSER-LUTONIX-STOP-2 ROUNDED =
499400          (((B-CHARGES-CLAIMED * P-NEW-OPER-CSTCHG-RATIO) -
499500                     H-BASE-DRG-PAYMENT)) * .5.
499600
499700     IF H-LESSER-LUTONIX-STOP-2 > 0
499800        IF H-LESSER-LUTONIX-STOP-1 < H-LESSER-LUTONIX-STOP-2
499900         MOVE H-LESSER-LUTONIX-STOP-1 TO
500000                                H-NEW-TECH-ADDON-LUTONIX
500100        ELSE
500200         MOVE H-LESSER-LUTONIX-STOP-2 TO
500300                                H-NEW-TECH-ADDON-LUTONIX
500400     ELSE
500500        MOVE ZEROES          TO H-NEW-TECH-ADDON-LUTONIX.
500600
500700
500800 4910-ADD-TECH-CASES.
500900
501000     COMPUTE H-NEW-TECH-PAY-ADD-ON ROUNDED =
501100             H-NEW-TECH-PAY-ADD-ON +
501200             H-NEW-TECH-ADDON-LUTONIX.
501300*
501400 4910-EXIT.    EXIT.
501500
501600***********************************************************
501700 6000-CALC-READMIS-REDU.
501800***********************************************************
501900*---------------------------------------------------------*
502000* (YEARCHANGE 2016.0)
502100* READMISSIONS PROCESS ADJUSTMENTS
502200*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.97 OR > 1.0)
502300*---------------------------------------------------------*
502400
502500     MOVE 0 TO H-READMIS-ADJUST-AMT.
502600
502700     IF P-HOSP-READMISSION-REDU = '1'
502800           GO TO 6000-EDIT-READMISN
502900     ELSE
503000           NEXT SENTENCE.
503100
503200     IF P-HOSP-READMISSION-REDU = '0' AND
503300        P-HOSP-HRR-ADJUSTMT = 0.0000
503400           MOVE ZEROES TO H-READMIS-ADJUST-AMT
503500           GO TO 6000-EXIT.
503600
503700     IF P-HOSP-READMISSION-REDU = '0' AND
503800        P-HOSP-HRR-ADJUSTMT > 0.0000
503900           MOVE 65 TO PPS-RTC
504000           MOVE ZEROES TO H-READMIS-ADJUST-AMT
504100           GO TO 6000-EXIT.
504200
504300
504400     IF P-HOSP-READMISSION-REDU = '2' OR '3' OR '4' OR '5' OR
504500                                  '6' OR '7' OR '8' OR
504600                                  '9' OR ' '
504700           MOVE 65 TO PPS-RTC
504800           MOVE ZEROES TO H-READMIS-ADJUST-AMT
504900           GO TO 6000-EXIT.
505000
505100 6000-EDIT-READMISN.
505200
505300     IF P-HOSP-HRR-ADJUSTMT < 0.9700
505400           MOVE 65 TO PPS-RTC
505500           MOVE ZEROES TO H-READMIS-ADJUST-AMT
505600           GO TO 6000-EXIT.
505700
505800
505900     IF P-HOSP-HRR-ADJUSTMT > 1.0000
506000           MOVE 65 TO PPS-RTC
506100           MOVE ZEROES TO H-READMIS-ADJUST-AMT
506200           GO TO 6000-EXIT.
506300
506400     IF P-READ-INVALID-STATE
506500           MOVE 65 TO PPS-RTC
506600           MOVE ZEROES TO H-READMIS-ADJUST-AMT
506700           GO TO 6000-EXIT.
506800
506900
507000 6000-COMPUTE-READMISN.
507100
507200
507300        COMPUTE H-READMIS-ADJUST-AMT         ROUNDED =
507400              ((P-HOSP-HRR-ADJUSTMT * H-OPER-BASE-DRG-PAY) -
507500                H-OPER-BASE-DRG-PAY).
507600
507700
507800 6000-EXIT.    EXIT.
507900
508000***********************************************************
508100 7000-CALC-VALUE-BASED-PURCH.
508200***********************************************************
508300*---------------------------------------------------------*
508400* (YEARCHANGE 2016.0)
508500* VALUE BASED PURCHASING (VBP) ADJUSTMENTS
508600*   + FY16: RANGE OF ALLOWABLE FACTORS (< 0.9825 OR > 2.0)
508700*---------------------------------------------------------*
508800
508900     MOVE 0 TO H-VAL-BASED-PURCH-ADJUST-AMT.
509000
509100     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N' OR 'Y'
509200           NEXT SENTENCE
509300     ELSE
509400           MOVE 68 TO PPS-RTC
509500           GO TO 7000-EXIT.
509600
509700     IF  P-VAL-BASED-PURCH-PARTIPNT = 'N'
509800           GO TO 7000-EXIT.
509900
510000     IF  P-VAL-BASED-PURCH-PARTIPNT = 'Y' AND
510100         P-NEW-CBSA-HOSP-QUAL-IND = '1'
510200           NEXT SENTENCE
510300     ELSE
510400           MOVE 68 TO PPS-RTC
510500           GO TO 7000-EXIT.
510600
510700     IF  P-VBP-INVALID-STATE
510800           MOVE 68 TO PPS-RTC
510900           GO TO 7000-EXIT
511000     ELSE
511100           NEXT SENTENCE.
511200
511300     IF P-VAL-BASED-PURCH-ADJUST < 0.9825000000 OR
511400        P-VAL-BASED-PURCH-ADJUST > 2.0000000000
511500           MOVE 68 TO PPS-RTC
511600           MOVE ZEROES TO H-VAL-BASED-PURCH-ADJUST-AMT
511700           GO TO 7000-EXIT
511800     ELSE
511900           GO TO 7000-COMPUTE-VAL-BASED-PUR.
512000
512100 7000-COMPUTE-VAL-BASED-PUR.
512200
512300     COMPUTE H-VAL-BASED-PURCH-ADJUST-AMT  ROUNDED =
512400              ((P-VAL-BASED-PURCH-ADJUST *
512500                  H-OPER-BASE-DRG-PAY) -
512600                  H-OPER-BASE-DRG-PAY).
512700*
512800 7000-EXIT.    EXIT.
512900
513000***********************************************************
513100 8000-CALC-BUNDLE-REDU.
513200***********************************************************
513300***** CASES INVOLVING BUNDLE PROCESS ADJUSTMENTS
513400***********************************************************
513500
513600
513700     MOVE 0 TO H-BUNDLE-ADJUST-AMT.
513800     MOVE 0 TO WK-MODEL1-BUNDLE-DISPRCNT.
513900
514000     IF '61' =  B-DEMO-CODE1  OR
514100                B-DEMO-CODE2  OR
514200                B-DEMO-CODE3  OR
514300                B-DEMO-CODE4
514400         NEXT SENTENCE
514500     ELSE
514600         MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
514700           GO TO 8000-EXIT.
514800
514900     IF P-MODEL1-BUNDLE-DISPRCNT > .00
515000           GO TO 8000-COMPUTE-BUNDLE
515100     ELSE
515200           NEXT SENTENCE.
515300
515400     MOVE ZEROES TO H-BUNDLE-ADJUST-AMT
515500           GO TO 8000-EXIT.
515600
515700 8000-COMPUTE-BUNDLE.
515800
515900     IF  B-DISCHARGE-DATE < 20140401 AND
516000         P-MODEL1-BUNDLE-DISPRCNT = .01
516100         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
516200          (1 - (P-MODEL1-BUNDLE-DISPRCNT * .5))
516300     ELSE
516400         COMPUTE WK-MODEL1-BUNDLE-DISPRCNT =
516500          (1 - (P-MODEL1-BUNDLE-DISPRCNT * 1)).
516600
516700        COMPUTE H-BUNDLE-ADJUST-AMT      ROUNDED =
516800              ((WK-MODEL1-BUNDLE-DISPRCNT *
516900                                     H-OPER-BASE-DRG-PAY) -
517000                H-OPER-BASE-DRG-PAY).
517100
517200        COMPUTE H-BUNDLE-ADJUST-AMT ROUNDED = H-BUNDLE-ADJUST-AMT.
517300
517400 8000-EXIT.    EXIT.
517500
517600***********************************************************
517700 9000-CALC-EHR-SAVING.
517800***********************************************************
517900*---------------------------------------------------------*
518000* (YEARCHANGE 2016.0)
518100* CASES INVOLVING EHR SAVINGS
518200*   + FY16: ANNUAL UPDATE TO BELOW VALUES
518300*   + EHR-FULL = FULL MB / NO EHR MB
518400*   + EHR-QUAL-FULL = NO QUAL MB / NO QUAL & NO EHR MB
518500*---------------------------------------------------------*
518600
518700     MOVE 1.011940299 TO H-MB-RATIO-EHR-FULL.
518800     MOVE 1.012012012 TO H-MB-RATIO-EHR-QUAL-FULL.
518900     MOVE 0 TO H-EHR-SUBSAV-QUANT.
519000     MOVE 0 TO H-EHR-SUBSAV-LV.
519100     MOVE 0 TO H-EHR-SUBSAV-QUANT-INCLV.
519200     MOVE 0 TO H-EHR-RESTORE-FULL-QUANT.
519300
519400     IF P-EHR-REDUC-IND = 'Y'
519500         NEXT SENTENCE
519600     ELSE
519700         GO TO 9000-EXIT.
519800
519900 9000-COMPUTE-EHR.
520000
520100*
520200* LOGIC TO IMPLEMENT EHR SAVINGS CALCULATION -
520300* ACTUAL EHR REDUCTIONS WILL BE BUILT INTO NEW RATE
520400* TABLES (5,6,7,&8) UP FRONT BUT OESS WANTS TO HAVE THE
520500* AMOUNT OF MONEY THE EHR POLICY 'SAVED' IN ITS OWN FIELD
520600* WHICH INVOLVES RESTORING THE FULL MARKET  BASKET
520700* TO THE PAYMENT TO GET THE 'WOULD'VE PAID' AND THEN
520800* TAKING THE DIFFERENCE BETWEEN ACTUAL PAID AND
520900* WOULD'VE PAID FOR THE SAVINGS.  OUTLIERS ARE TO BE
521000* LEFT OUT AT MOMENT SINCE OUTLIER SHOULD BE LOWER
521100* ON THE FULL RATE THAN IT WINDS UP BEING ON THE
521200* REDUCED RATE - LIKEWISE NEW TECH IS BEING LEFT
521300* OUT.
521400*
521500*
521600* FOR EHR NEED TO EXCLUDE NEW TECH AND OUTLIERS FROM
521700* SAVINGS CALCULATION SO CALCULATE AN OPERATING
521800* PAYMENT SUBTOTAL ON SO CALCULATE AN OPERATING
521900* PAYMENT SUBTOTAL ON EHR PAYMENTS THAT EXCLUDES
522000* OUTLIERS AND NEW TECH FOR CLAIMS WITH AN EHR FLAG
522100*
522200*
522300
522400      COMPUTE H-EHR-SUBSAV-QUANT =
522500           (PPS-OPER-HSP-PART +
522600            PPS-OPER-FSP-PART +
522700            PPS-OPER-DSH-ADJ +
522800            PPS-OPER-IME-ADJ +
522900            H-READMIS-ADJUST-AMT +
523000            H-VAL-BASED-PURCH-ADJUST-AMT +
523100            H-BUNDLE-ADJUST-AMT).
523200
523300*
523400*
523500* NEED TO ENSURE THAT LOW VOLUME, IF APPLICABLE IS
523600* INCLUDED - CAN'T USE PRICER'S LOW VOLUME PAYMENT
523700* AS THAT INCLUDES NEW TECH OUTLIERS AND CAPITAL -
523800* READM VBP AND BUNDLE
523900* DON'T MULTIPLY BY LV ADJUSTMENT SO MAKE A NEW LV AMT
524000* FOR ERH SAVINGS FIELD;
524100*
524200*
524300*
524400
524500      MOVE 0 TO H-EHR-SUBSAV-LV.
524600
524700      IF  WK-LOW-VOL25PCT < 1.000000
524800
524900      COMPUTE H-EHR-SUBSAV-LV =
525000          (PPS-OPER-HSP-PART +
525100           PPS-OPER-FSP-PART +
525200           PPS-OPER-DSH-ADJ +
525300           PPS-OPER-IME-ADJ )  * WK-LOW-VOL25PCT.
525400
525500      COMPUTE H-EHR-SUBSAV-QUANT-INCLV =
525600           H-EHR-SUBSAV-QUANT + H-EHR-SUBSAV-LV.
525700
525800*
525900* H-MB-RATIO-EHR-FULL IS THE RATIO OF THE FULL MARKET
526000* BASKET TO THE REDUCED EHR MB - NEED TO CARRY 2 RATIOS
526100* FOR PROVIDERS FAILING EHR AND FOR PROVIDERS FAILING EHR
526200* AND QUALITY IN COMBINATION.  EHR SAVINGS REQUIRES
526300* BACKING OFF THE LOW UPDATE AND MULTIPLYING ON THE
526400* FULL UPDATE SO USING RATIO OF LOW/FULL AND LOW/QUALHIT
526500* OF .625 ONLY.
526600*
526700
526800       COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
526900       H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-FULL.
527000
527100     IF P-NEW-CBSA-HOSP-QUAL-IND NOT = '1'
527200        COMPUTE  H-EHR-RESTORE-FULL-QUANT ROUNDED =
527300          H-EHR-SUBSAV-QUANT-INCLV * H-MB-RATIO-EHR-QUAL-FULL.
527400
527500        COMPUTE  H-EHR-ADJUST-AMT ROUNDED =
527600          H-EHR-RESTORE-FULL-QUANT - H-EHR-SUBSAV-QUANT-INCLV.
527700
527800
527900
528000 9000-EXIT.    EXIT.
528100
528200
528300*---------------------------------------------------------*
528400* (YEARCHANGE 2016.0)
528500*---------------------------------------------------------*
528600 9010-CALC-STANDARD-CHG.
528700
528800***********************************************************
528900***�CM-P3� STANDARDIZED OPERATING COST CALCULATION
529000
529100     IF ((H-LABOR-PCT * H-WAGE-INDEX) +
529200               (H-NONLABOR-PCT * H-OPER-COLA)) > 0
529300        COMPUTE  H-OPER-BILL-STDZ-COSTS ROUNDED =
529400        (B-CHARGES-CLAIMED * H-OPER-CSTCHG-RATIO) /
529500        ((H-LABOR-PCT * H-WAGE-INDEX) +
529600               (H-NONLABOR-PCT * H-OPER-COLA))
529700     ELSE MOVE 0 TO H-OPER-BILL-STDZ-COSTS.
529800
529900***********************************************************
530000***�CM-P3� STANDARDIZED CAPITAL COST CALCULATION
530100
530200     IF (H-CAPI-GAF * H-CAPI-COLA) > 0
530300       COMPUTE  H-CAPI-BILL-STDZ-COSTS ROUNDED =
530400        (B-CHARGES-CLAIMED * H-CAPI-CSTCHG-RATIO) /
530500               (H-CAPI-GAF * H-CAPI-COLA)
530600     ELSE MOVE 0 TO H-CAPI-BILL-STDZ-COSTS.
530700
530800***********************************************************
530900***�CM-P3� STANDARDIZED OPERATING TRESHOLD
531000
531100     IF B-DISCHARGE-DATE < 20160101
531200        MOVE 5467.39 TO H-OPER-BASE
531300     ELSE
531400        MOVE 5467.53 TO H-OPER-BASE.
531500
531600     COMPUTE   H-OPER-STDZ-DOLLAR-THRESHOLD ROUNDED =
531700      (H-CST-THRESH * H-OPER-SHARE-DOLL-THRESHOLD)  +
531800                        +
531900           (H-OPER-BASE * H-DRG-WT-FRCTN)
532000                        +
532100              H-NEW-TECH-PAY-ADD-ON.
532200
532300******************************************************
532400***�CM-P3� STANDARDIZED CAPITAL TRESHOLD
532500
532600     MOVE 438.75 TO H-CAPI-BASE.
532700
532800     COMPUTE   H-CAPI-STDZ-DOLLAR-THRESHOLD ROUNDED =
532900     (H-CST-THRESH * H-CAPI-SHARE-DOLL-THRESHOLD)
533000                     +
533100     (H-CAPI-BASE * H-DRG-WT-FRCTN).
533200
533300
533400******************************************************
533500***�CM-P3� STANDARDIZED OPERATING OUTLIER CALCULATION
533600
533700     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
533800        (H-OPER-STDZ-DOLLAR-THRESHOLD +
533900                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
534000                          AND
534100         H-OPER-BILL-STDZ-COSTS > H-OPER-STDZ-DOLLAR-THRESHOLD
534200
534300       COMPUTE  H-OPER-STDZ-COST-OUTLIER ROUNDED =
534400        (H-CSTOUT-PCT  *
534500        (H-OPER-BILL-STDZ-COSTS - H-OPER-STDZ-DOLLAR-THRESHOLD))
534600
534700     ELSE
534800       MOVE 0 TO H-OPER-STDZ-COST-OUTLIER.
534900
535000******************************************************
535100***�CM-P3� STANDARDIZED CAPITAL OUTLIER CALCULATION
535200
535300     IF (H-OPER-BILL-STDZ-COSTS + H-CAPI-BILL-STDZ-COSTS) >
535400        (H-OPER-STDZ-DOLLAR-THRESHOLD +
535500                           H-CAPI-STDZ-DOLLAR-THRESHOLD)
535600                          AND
535700         H-CAPI-BILL-STDZ-COSTS > H-CAPI-STDZ-DOLLAR-THRESHOLD
535800
535900      COMPUTE  H-CAPI-STDZ-COST-OUTLIER ROUNDED =
536000      (H-CSTOUT-PCT  *
536100      (H-CAPI-BILL-STDZ-COSTS - H-CAPI-STDZ-DOLLAR-THRESHOLD))
536200     ELSE
536300      MOVE 0 TO H-CAPI-STDZ-COST-OUTLIER.
536400
536500*******************************************************
536600***�CM-P3� STANDARDIZED ALLOWED AMOUNT CALCULATION
536700
536800      COMPUTE H-STANDARD-ALLOWED-AMOUNT ROUNDED =
536900       (H-OPER-BASE + H-CAPI-BASE)
537000                 *
537100       H-DRG-WT-FRCTN
537200                 +
537300       H-OPER-STDZ-COST-OUTLIER
537400                 +
537500       H-CAPI-STDZ-COST-OUTLIER
537600                 +
537700       H-NEW-TECH-PAY-ADD-ON.
537800
537900 9010-EXIT.    EXIT.
538000
538100***********************************************************
538200******        L A S T   S O U R C E   S T A T E M E N T   *****
