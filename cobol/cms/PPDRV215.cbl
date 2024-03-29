000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID.           PPDRV215.
000300*AUTHOR.   DDS TEAM.
000400*UPDATE.   FY 2021 PRODUCTION.
000500*
000600*REMARKS.  - CALLS THE PPCAL__ MODULES
000700*          - FINDS  WAGE-INDEX RECORD FOR
000800*            GIVEN BILL TO BE PASSED TO PPCAL__ MODULES.
000900 DATE-COMPILED.
001000****************************************************************
001100*   THE RESPONSIBILITY FOR INSTALLING, MODIFYING, TESTING,     *
001200*   MAINTAINING, AND VERIFYING THE ACCURACY OF THIS PROGRAM    *
001300*   IS THAT OF THE USER.                                       *
001400*                  *  *  *  *  *  *  *  *                      *
001500*   ONCE GROUPED THE PROSPECTIVE PAYMENT SUBROUTINE IS CALLED  *
001600*   TO CALCULATE THE TOTAL PAYMENT PRIOR TO DEDUCTIBLE,        *
001700*   CO-INSURANCE, AND CASES WHERE MEDICARE IS SECONDARY PAYOR. *
001800*   THE PROGRAM WILL:                                          *
001900*       1. FIND THE WAGE INDEX  TO CALCULATE PPS.              *
002000*       2. EDIT THE BILL INFORMATION PASSED TO IT.             *
002100*       3. PASS BACK RETURN CODES.                             *
002200*       4. CALCULATE WHEN APPLICABLE                           *
002300*          A. THE COVERED DAYS UTILIZATION.                    *
002400*          B. THE HOSPITAL SPECIFIC PART OF PAYMENT.           *
002500*          C. THE FEDERAL  SPECIFIC PART OF PAYMENT            *
002600*          D. THE OUTLIER PORTION (COST).                      *
002700*          E. THE NUMBER OF OUTLIER DAYS.                      *
002800*          F. TOTAL PAYMENT (B + C + D  ABOVE).                *
002900*          G. DISPROPORTIONATE SHARE ADJUSTMENT                *
003000*          H. INDIRECT TEACH ADJUSTMENT.                       *
003100*                  *  *  *  *  *  *  *  *                      *
003200*   THIS SUBROUTINE CALCULATES THE PROVIDER SPECIFIC           *
003300*   ELEMENTS ON A PROVIDER BREAK, THEREFORE IT WILL RUN FASTER *
003400*   WHEN BILLS ARE BATCHED BY PROVIDER.                        *
003500*                                                              *
003600****************************************************************
003700 ENVIRONMENT DIVISION.
003800 CONFIGURATION SECTION.
003900 SOURCE-COMPUTER.            IBM-370.
004000 OBJECT-COMPUTER.            IBM-370.
004100 INPUT-OUTPUT SECTION.
004200 FILE-CONTROL.
004300 DATA DIVISION.
004400 FILE SECTION.
004500
004600 WORKING-STORAGE SECTION.
004700 77  W-STORAGE-REF                  PIC X(48)  VALUE
004800     'P P D R I V E R - W O R K I N G   S T O R A G E'.
004900
005000 01  DRV-VERSION                    PIC X(05) VALUE 'D21.5'.
005100 01  PPCAL884                       PIC X(08) VALUE 'PPCAL884'.
005200 01  PPCAL894                       PIC X(08) VALUE 'PPCAL894'.
005300 01  PPCAL905                       PIC X(08) VALUE 'PPCAL905'.
005400 01  PPCAL915                       PIC X(08) VALUE 'PPCAL915'.
005500 01  PPCAL926                       PIC X(08) VALUE 'PPCAL926'.
005600 01  PPCAL935                       PIC X(08) VALUE 'PPCAL935'.
005700 01  PPCAL944                       PIC X(08) VALUE 'PPCAL944'.
005800 01  PPCAL954                       PIC X(08) VALUE 'PPCAL954'.
005900 01  PPCAL964                       PIC X(08) VALUE 'PPCAL964'.
006000 01  PPCAL974                       PIC X(08) VALUE 'PPCAL974'.
006100 01  PPCAL987                       PIC X(08) VALUE 'PPCAL987'.
006200 01  PPCAL998                       PIC X(08) VALUE 'PPCAL998'.
006300 01  PPCAL006                       PIC X(08) VALUE 'PPCAL006'.
006400 01  PPCAL017                       PIC X(08) VALUE 'PPCAL017'.
006500 01  PPCAL026                       PIC X(08) VALUE 'PPCAL026'.
006600 01  PPCAL038                       PIC X(08) VALUE 'PPCAL038'.
006700 01  PPCAL04D                       PIC X(08) VALUE 'PPCAL04D'.
006800 01  PPCAL059                       PIC X(08) VALUE 'PPCAL059'.
006900 01  PPCAL069                       PIC X(08) VALUE 'PPCAL069'.
007000 01  PPCAL07B                       PIC X(08) VALUE 'PPCAL07B'.
007100 01  PPCAL08D                       PIC X(08) VALUE 'PPCAL08D'.
007200 01  PPCAL09D                       PIC X(08) VALUE 'PPCAL09D'.
007300 01  PPCAL10O                       PIC X(08) VALUE 'PPCAL10O'.
007400 01  PPCAL10P                       PIC X(08) VALUE 'PPCAL10P'.
007500 01  PPCAL119                       PIC X(08) VALUE 'PPCAL119'.
007600 01  PPCAL125                       PIC X(08) VALUE 'PPCAL125'.
007700 01  PPCAL135                       PIC X(08) VALUE 'PPCAL135'.
007800 01  PPCAL14B                       PIC X(08) VALUE 'PPCAL14B'.
007900 01  PPCAL156                       PIC X(08) VALUE 'PPCAL156'.
008000 01  PPCAL163                       PIC X(08) VALUE 'PPCAL163'.
008100 01  PPCAL171                       PIC X(08) VALUE 'PPCAL171'.
008200 01  PPCAL182                       PIC X(08) VALUE 'PPCAL182'.
008300 01  PPCAL192                       PIC X(08) VALUE 'PPCAL192'.
008400 01  PPCAL204                       PIC X(08) VALUE 'PPCAL204'.
008500 01  PPCAL215                       PIC X(08) VALUE 'PPCAL215'.
008600
008700 01  TABLES-LOADED-SW               PIC 9(01) VALUE 0.
008800 01  EOF-SW                         PIC 9(01) VALUE 0.
008900 01  WS-9S                          PIC X(08) VALUE '99999999'.
009000
009100 01  WI_QUARTILE_FY2020       PIC 9(02)V9(04)  VALUE 0.8457.
009200 01  WI_QUARTILE_FY2021       PIC 9(02)V9(04)  VALUE 0.8469.
009300 01  WI_PCT_REDUC_FY2020      PIC S9(01)V9(02) VALUE -0.05.
009400 01  WI_PCT_ADJ_FY2020        PIC 9(01)V9(02)  VALUE 0.95.
009500
009600*---------------------------------------------------------*
009700* OUTMIGRATION ADJUSTMENT FACTOR TABLE                    *
009800*---------------------------------------------------------*
009900 COPY HOUTI212.
010000
010100 01  HOLD-OUTM-DATA.
010200     05  OUTM-IND                       PIC 9(01) VALUE 0.
010300     05  OUTM-IDX2                      PIC 9(04) VALUE 0.
010400     05  HLD-OUTM-ADJ                   PIC S9(01)V9(04).
010500
010600*---------------------------------------------------------*
010700* RURAL FLOOR FACTOR TABLE                                *
010800*---------------------------------------------------------*
010900 COPY RUFL200.
011000
011100 01  HOLD-RUFL-DATA.
011200     05  RUFL-IDX2                      PIC 9(03) VALUE 0.
011300
011400*---------------------------------------------------------*
011500* PREVIOUS FY WAGE INDEX TABLE                            *
011600*---------------------------------------------------------*
011700 COPY PREV200.
011800
011900 01  HOLD-PREV-DATA.
012000     05  HLD-PREV-WI                    PIC S9(02)V9(04).
012100
012200 01  HOLD-PROV-MSAX.
012300         10  H-MSAX-PROV-BLANK   PIC X(2).
012400         10  H-MSAX-PROV-STATE.
012500             15  FILLER          PIC X.
012600             15  H-MSAX-LAST-POS PIC X.
012700
012800 01  HOLD-PROV-CBSA.
012900         10  H-CBSA-PROV-BLANK      PIC X(3).
013000         10  H-CBSA-PROV-STATE.
013100             15  FILLER             PIC X.
013200             15  H-CBSA-LAST-POS    PIC X.
013300
013400 01  HOLD-RURAL-CBSA.
013500         10  H-CBSA-RURAL-BLANK     PIC X(3).
013600         10  H-CBSA-RURAL-STATE.
013700             15  FILLER                   PIC X.
013800             15  H-CBSA-RURAL-LAST-POS    PIC X.
013900
014000*-------------------------------------------------------------*
014100* VARIABLES TO HOLD THE BILL'S FY BEGIN AND END DATES
014200*-----------------------------------------------------------
014300 01  W-FY-BEGIN-DATE.
014400         05  W-FY-BEGIN-CC              PIC 9(02).
014500         05  W-FY-BEGIN-YY              PIC 9(02).
014600         05  W-FY-BEGIN-MM              PIC 9(02) VALUE 10.
014700         05  W-FY-BEGIN-DD              PIC 9(02) VALUE 01.
014800
014900 01  W-FY-END-DATE.
015000         05  W-FY-END-CC                PIC 9(02).
015100         05  W-FY-END-YY                PIC 9(02).
015200         05  W-FY-END-MM                PIC 9(02) VALUE 09.
015300         05  W-FY-END-DD                PIC 9(02) VALUE 30.
015400
015500**************YEARCHANGE 2015.1 ******************************
015600* USED FOR FY15 BLENDED TRANSITION WI FOR CERTAIN PR PROVIDERS
015700**************************************************************
015800 01  PRSPC-PROV-TABLE.
015900     05 PRSPC-PROV-BLEND-DATA.
016000         10  FILLER      PIC X(14) VALUE '400001  010206'.
016100         10  FILLER      PIC X(14) VALUE '400003  009532'.
016200         10  FILLER      PIC X(14) VALUE '400004  010206'.
016300         10  FILLER      PIC X(14) VALUE '400005  010206'.
016400         10  FILLER      PIC X(14) VALUE '400006  010206'.
016500         10  FILLER      PIC X(14) VALUE '400007  010206'.
016600         10  FILLER      PIC X(14) VALUE '400011  010206'.
016700         10  FILLER      PIC X(14) VALUE '400012  010206'.
016800         10  FILLER      PIC X(14) VALUE '400013  010206'.
016900         10  FILLER      PIC X(14) VALUE '400015  010206'.
017000         10  FILLER      PIC X(14) VALUE '400016  010206'.
017100         10  FILLER      PIC X(14) VALUE '400018  010206'.
017200         10  FILLER      PIC X(14) VALUE '400019  010206'.
017300         10  FILLER      PIC X(14) VALUE '400021  010588'.
017400         10  FILLER      PIC X(14) VALUE '400022  009532'.
017500         10  FILLER      PIC X(14) VALUE '400032  010206'.
017600         10  FILLER      PIC X(14) VALUE '400044  009532'.
017700         10  FILLER      PIC X(14) VALUE '400061  010206'.
017800         10  FILLER      PIC X(14) VALUE '400087  009942'.
017900         10  FILLER      PIC X(14) VALUE '400098  010206'.
018000         10  FILLER      PIC X(14) VALUE '400102  010206'.
018100         10  FILLER      PIC X(14) VALUE '400104  010206'.
018200         10  FILLER      PIC X(14) VALUE '400105  010206'.
018300         10  FILLER      PIC X(14) VALUE '400106  010206'.
018400         10  FILLER      PIC X(14) VALUE '400109  010206'.
018500         10  FILLER      PIC X(14) VALUE '400112  010206'.
018600         10  FILLER      PIC X(14) VALUE '400113  009532'.
018700         10  FILLER      PIC X(14) VALUE '400114  010206'.
018800         10  FILLER      PIC X(14) VALUE '400115  010206'.
018900         10  FILLER      PIC X(14) VALUE '400117  009942'.
019000         10  FILLER      PIC X(14) VALUE '400118  010206'.
019100         10  FILLER      PIC X(14) VALUE '400120  010206'.
019200         10  FILLER      PIC X(14) VALUE '400121  010206'.
019300         10  FILLER      PIC X(14) VALUE '400122  010206'.
019400         10  FILLER      PIC X(14) VALUE '400124  010206'.
019500         10  FILLER      PIC X(14) VALUE '400126  010588'.
019600         10  FILLER      PIC X(14) VALUE '400127  010206'.
019700         10  FILLER      PIC X(14) VALUE '400128  010206'.
019800     05  WK-PRSPC-DATA2 REDEFINES PRSPC-PROV-BLEND-DATA.
019900        10  PRSPC-TAB OCCURS 38
020000                        ASCENDING KEY IS WK-PRSPC-PROV
020100                        INDEXED BY PRSPC-IDX.
020200           15  WK-PRSPC-COUNT-ALL.
020300              20  WK-PRSPC-PROV            PIC X(6).
020400              20  FILLER                   PIC XX.
020500              20  WK-PRSPC-WAGEIN-BLEND    PIC S9(02)V9(04).
020600
020700**************YEARCHANGE 2015.1 ******************************
020800* USED FOR FY15 BLENDED TRANSITION WI FOR CERTAIN PR PROVIDERS
020900**************************************************************
021000 01  MES-PRSPC.
021100     05  MES-PRSPC-PROV                PIC X(6).
021200     05  FILLER                        PIC XXX.
021300     05  MESWK-PRSPC-WAGEIN-BLEND      PIC S9(02)V9(04).
021400
021500*
021600************************************************************
021700**    OLD PROVIDER RECORD FORMAT
021800************************************************************
021900 01  W-PROV-OLD-HOLD.
022000     02  W-PROV-OLDREC-HOLD1.
022100         05  W-P-PROVIDER-NO.
022200             10  W-P-STATE                PIC X(02).
022300             10  FILLER                   PIC X(04).
022400         05  W-P-EFF-DATE.
022500             10  W-P-EFF-YY               PIC 9(02).
022600             10  W-P-EFF-MM               PIC 9(02).
022700             10  W-P-EFF-DD               PIC 9(02).
022800         05  W-P-WAIVER-CODE              PIC X(01).
022900         05  W-P-PROVIDER-TYPE            PIC X(02).
023000         05  W-P-CURRENT-CENSUS-DIV       PIC X(01).
023100         05  W-P-PPS-BLEND-YR-IND         PIC X(01).
023200         05  W-P-MSA-X.
023300             10  W-P-RURAL                PIC X(04).
023400         05  W-P-MSA-9 REDEFINES W-P-MSA-X  PIC 9(04).
023500         05  W-P-FISCAL-YEAR-END.
023600             10  W-P-MM                   PIC 9(02).
023700             10  W-P-DD                   PIC 9(02).
023800             10  W-P-YY                   PIC 9(02).
023900         05  W-P-VARIABLES.
024000             10  W-P-FAC-SPEC-RATE        PIC  X(07).
024100             10  W-P-COLA                 PIC  X(04).
024200             10  W-P-INTERN-RATIO         PIC  X(05).
024300             10  W-PRUP-UPDT-FACTOR       PIC  X(06).
024400             10  W-P-BED-SIZE             PIC  X(05).
024500             10  W-P-DSH-PERCENT          PIC  V9(04).
024600             10  W-P-CCR                  PIC  X(04).
024700             10  W-P-CMI                  PIC  X(05).
024800             10  FILLER                 PIC  X(01).
024900             10  W-P-REPORT-DATE.
025000                 15  W-P-REPORT-DT-MM     PIC 99.
025100                 15  W-P-REPORT-DT-DD     PIC 99.
025200                 15  W-P-REPORT-DT-YY     PIC 99.
025300             10  FILLER                 PIC  X(01).
025400             10  W-P-INTER-NO             PIC  X(05).
025500     02  W-PROV-OLDREC-HOLD2.
025600         05  W-P-FY-BEGIN-DATE.
025700             10  W-P-FY-BEG-DT-MM         PIC 99.
025800             10  W-P-FY-BEG-DT-DD         PIC 99.
025900             10  W-P-FY-BEG-DT-YY         PIC 99.
026000         05  W-P-PASS-AMT-CAPITAL         PIC X(6).
026100         05  W-P-PASS-AMT-DIR-MED-ED      PIC X(6).
026200         05  W-P-PASS-AMT-ORGAN-ACQ       PIC X(6).
026300         05  W-P-PASS-AMT-PLUS-MISC       PIC X(6).
026400         05  W-P-SSI-RATIO                PIC X(4).
026500         05  W-P-MEDICAID-RATIO           PIC X(4).
026600         05  W-P-TERMINATION-DATE.
026700             10  W-P-TERM-DT-YY           PIC 99.
026800             10  W-P-TERM-DT-MM           PIC 99.
026900             10  W-P-TERM-DT-DD           PIC 99.
027000         05  W-P-WAGE-INDEX-LOC-MSA       PIC X(4).
027100         05  W-P-CHG-CODE-INDEX           PIC X.
027200         05  W-P-STAND-AMT-LOC-MSA.
027300             10  W-P-RURAL-1ST.
027400                 15  W-P-STAND-RURAL      PIC XX.
027500             10  W-P-RURAL-2ND            PIC XX.
027600         05  W-P-SOL-COM-DEP-HOSP-YR      PIC XX.
027700         05  W-P-LUGAR                    PIC X.
027800         05  W-P-TEMP-RELIEF-IND          PIC X.
027900         05  FILLER                       PIC X(23).
028000     02  W-PROV-OLDREC-HOLD3.
028100         05  W-P-CAPI-PPS-PAY-CODE        PIC X.
028200         05  W-P-CAPI-HOSP-SPEC-RATE      PIC X(6).
028300         05  W-P-CAPI-OLD-HARM-RATE       PIC X(6).
028400         05  W-P-CAPI-NEW-HARM-RATIO      PIC X(5).
028500         05  W-P-CAPI-CSTCHG-RATIO        PIC X(04).
028600         05  W-P-CAPI-NEW-HOSP            PIC X.
028700         05  W-P-CAPI-IME                 PIC X(05).
028800         05  W-P-CAPI-EXCEPTIONS          PIC X(6).
028900         05  W-P-HVBP-HRR-DATA.
029000             15  W-P-VAL-BASED-PURCH-PARTIPNT PIC X.
029100             15  W-P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
029200             15  W-P-HOSP-READMISSION-REDUCTN PIC X.
029300             15  W-P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
029400         05  W-P-MODEL1-BUNDLE-DATA.
029500             15  W-P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
029600             15  W-P-HAC-REDUC-IND            PIC X.
029700             15  W-P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
029800             15  W-P-EHR-REDUC-IND            PIC X.
029900         05  FILLER                           PIC X(09).
030000
030100***************************************************************
030200**    NEW PROVIDER RECORD FORMAT
030300***************************************************************
030400 01  W-PROV-NEW-HOLD.
030500     02  W-PROV-NEWREC-HOLD1.
030600         05  W-P-NEW-NPI10.
030700             10  W-P-NEW-NPI8           PIC X(08).
030800             10  W-P-NEW-NPI-FILLER     PIC X(02).
030900         05  W-P-NEW-PROVIDER-OSCAR-NO.
031000             10  W-P-NEW-STATE            PIC X(02).
031100             10  FILLER                   PIC X(04).
031200         05  W-P-NEW-DATE-DATA.
031300             10  W-P-NEW-EFF-DATE.
031400                 15  W-P-NEW-EFF-DT-CC    PIC 9(02).
031500                 15  W-P-NEW-EFF-DT-YY    PIC 9(02).
031600                 15  W-P-NEW-EFF-DT-MM    PIC 9(02).
031700                 15  W-P-NEW-EFF-DT-DD    PIC 9(02).
031800             10  W-P-NEW-FY-BEGIN-DATE.
031900                 15  W-P-NEW-FY-BEG-DT-CC PIC 9(02).
032000                 15  W-P-NEW-FY-BEG-DT-YY PIC 9(02).
032100                 15  W-P-NEW-FY-BEG-DT-MM PIC 9(02).
032200                 15  W-P-NEW-FY-BEG-DT-DD PIC 9(02).
032300             10  W-P-NEW-REPORT-DATE.
032400                 15  W-P-NEW-REPORT-DT-CC PIC 9(02).
032500                 15  W-P-NEW-REPORT-DT-YY PIC 9(02).
032600                 15  W-P-NEW-REPORT-DT-MM PIC 9(02).
032700                 15  W-P-NEW-REPORT-DT-DD PIC 9(02).
032800             10  W-P-NEW-TERMINATION-DATE.
032900                 15  W-P-NEW-TERM-DT-CC   PIC 9(02).
033000                 15  W-P-NEW-TERM-DT-YY   PIC 9(02).
033100                 15  W-P-NEW-TERM-DT-MM   PIC 9(02).
033200                 15  W-P-NEW-TERM-DT-DD   PIC 9(02).
033300         05  W-P-NEW-WAIVER-CODE          PIC X(01).
033400             88  W-P-NEW-WAIVER-STATE       VALUE 'Y'.
033500         05  W-P-NEW-INTER-NO             PIC X(05).
033600         05  W-P-NEW-PROVIDER-TYPE        PIC X(02).
033700         05  W-P-NEW-CURRENT-CENSUS-DIV   PIC X(01).
033800         05  W-P-NEW-MSA-DATA.
033900             10  W-P-NEW-CHG-CODE-INDEX    PIC X.
034000             10  W-P-NEW-GEO-LOC-MSA        PIC X(04) JUST RIGHT.
034100             10  W-P-NEW-WAGE-INDEX-LOC-MSA PIC X(04) JUST RIGHT.
034200             10  W-P-NEW-STAND-AMT-LOC-MSA  PIC X(04) JUST RIGHT.
034300             10  W-P-NEW-STAND-AMT-LOC-MSA9
034400       REDEFINES W-P-NEW-STAND-AMT-LOC-MSA.
034500                 15  W-P-NEW-RURAL-1ST.
034600                     20  W-P-NEW-STAND-RURAL  PIC XX.
034700                 15  W-P-NEW-RURAL-2ND        PIC XX.
034800         05  W-P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
034900         05  W-P-NEW-LUGAR               PIC X.
035000         05  W-P-NEW-TEMP-RELIEF-IND     PIC X.
035100         05  W-P-NEW-FED-PPS-BLEND-IND   PIC X.
035200         05  W-P-NEW-STATE-CODE          PIC 9(02).
035300         05  W-P-NEW-STATE-CODE-X REDEFINES
035400             W-P-NEW-STATE-CODE          PIC X(02).
035500         05  FILLER                      PIC X(03).
035600     02  W-PROV-NEWREC-HOLD2.
035700         05  W-P-NEW-VARIABLES.
035800             10  W-P-NEW-FAC-SPEC-RATE     PIC  X(07).
035900             10  W-P-NEW-COLA              PIC  X(04).
036000             10  W-P-NEW-INTERN-RATIO      PIC  X(05).
036100             10  W-P-NEW-BED-SIZE          PIC  X(05).
036200             10  W-P-NEW-CCR               PIC  X(04).
036300             10  W-P-NEW-CMI               PIC  X(05).
036400             10  W-P-NEW-SSI-RATIO         PIC  X(04).
036500             10  W-P-NEW-MEDICAID-RATIO    PIC  X(04).
036600             10  W-P-NEW-PPS-BLEND-YR-IND  PIC  X(01).
036700             10  W-P-NEW-PRUP-UPDTE-FACTOR PIC  9(01)V9(05).
036800             10  W-P-NEW-DSH-PERCENT       PIC  V9(04).
036900             10  W-P-NEW-FYE-DATE.
037000                 15  W-P-NEW-FYE-CC        PIC 99.
037100                 15  W-P-NEW-FYE-YY        PIC 99.
037200                 15  W-P-NEW-FYE-MM        PIC 99.
037300                 15  W-P-NEW-FYE-DD        PIC 99.
037400         05  W-P-NEW-CBSA-DATA.
037500             10  W-P-NEW-CBSA-SPEC-PAY-IND   PIC X.
037600             10  W-P-NEW-CBSA-HOSP-QUAL-IND  PIC X.
037700             10  W-P-NEW-CBSA-GEO-LOC        PIC X(05) JUST RIGHT.
037800             10  W-P-NEW-CBSA-RECLASS-LOC    PIC X(05) JUST RIGHT.
037900             10  W-P-NEW-CBSA-STAND-AMT-LOC  PIC X(05) JUST RIGHT.
038000             10  W-P-NEW-CBSA-STAND-AMT-LOC9
038100       REDEFINES W-P-NEW-CBSA-STAND-AMT-LOC.
038200                 15  W-P-NEW-CBSA-RURAL-1ST.
038300                     20  W-P-NEW-CBSA-STAND-RURAL PIC 999.
038400                 15  W-P-NEW-CBSA-RURAL-2ND       PIC 99.
038500             10  W-P-NEW-CBSA-SPEC-WAGE-INDEX     PIC 9(02)V9(04).
038600     02  W-PROV-NEWREC-HOLD3.
038700         05  W-P-NEW-PASS-AMT-DATA.
038800             10  W-P-NEW-PASS-AMT-CAPITAL    PIC X(06).
038900             10  W-P-NEW-PASS-AMT-DIR-MED-ED PIC X(06).
039000             10  W-P-NEW-PASS-AMT-ORGAN-ACQ  PIC X(06).
039100             10  W-P-NEW-PASS-AMT-PLUS-MISC  PIC X(06).
039200         05  W-P-NEW-CAPI-DATA.
039300             15  W-P-NEW-CAPI-PPS-PAY-CODE   PIC X.
039400             15  W-P-NEW-CAPI-HOSP-SPEC-RATE PIC X(6).
039500             15  W-P-NEW-CAPI-OLD-HARM-RATE  PIC X(6).
039600             15  W-P-NEW-CAPI-NEW-HARM-RATIO PIC X(5).
039700             15  W-P-NEW-CAPI-CSTCHG-RATIO   PIC X(04).
039800             15  W-P-NEW-CAPI-NEW-HOSP       PIC X.
039900             15  W-P-NEW-CAPI-IME            PIC X(05).
040000             15  W-P-NEW-CAPI-EXCEPTIONS     PIC X(6).
040100         05  W-P-HVBP-HRR-DATA.
040200             15  W-P-NEW-VAL-BASED-PURCH-PARTIP   PIC X.
040300             15  W-P-NEW-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
040400             15  W-P-NEW-HOSP-READMISSION-REDU    PIC X.
040500             15  W-P-NEW-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
040600         05  W-P-MODEL1-BUNDLE-DATA.
040700             15  W-P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
040800             15  W-P-HAC-REDUC-IND            PIC X.
040900             15  W-P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
041000             15  W-P-EHR-REDUC-IND            PIC X.
041100             15  W-P-LV-ADJ-FACTOR            PIC 9V9(6).
041200         05  W-P-NEW-COUNTY-CODE              PIC 9(05).
041300         05  W-P-NEW-COUNTY-CODE-X REDEFINES
041400             W-P-NEW-COUNTY-CODE              PIC X(05).
041500         05  W-P-NEW-SUPPLEMENTAL-WI.
041600             10  W-P-NEW-SUPP-WI-IND          PIC X.
041700                 88  W-P-NEW-IND-PRIOR-YEAR   VALUE '1'.
041800             10  W-P-NEW-SUPP-WI              PIC 9(02)V9(04).
041900         05  W-P-PASS-THRU-ALLO-STEM-CELL     PIC 9(07)V9(02).
042000         05  FILLER                           PIC X(31).
042100
042200***************************************************************
042300*      THIS IS THE WAGE-INDEX RECORD THAT WILL BE PASSED TO   *
042400*      THE PPCAL001 PROGRAM THRU PPCAL047 FOR PROCESSING
042500*      MSAX TABLE
042600***************************************************************
042700 01  WAGE-NEW-INDEX-RECORD.
042800     05  W-NEW-MSA               PIC 9(4).
042900     05  W-NEW-SIZE              PIC X(01).
043000         88  NEW-LARGE-URBAN       VALUE 'L'.
043100         88  NEW-OTHER-URBAN       VALUE 'O'.
043200         88  NEW-ALL-RURAL         VALUE 'R'.
043300     05  W-NEW-EFF-DATE.
043400          10  W-NEW-EFF-DATE-CC   PIC 9(2).
043500          10  W-NEW-EFF-DATE-YMD.
043600              15  W-NEW-EFF-DATE-YY   PIC 9(2).
043700              15  W-NEW-EFF-DATE-MM   PIC 9(2).
043800              15  W-NEW-EFF-DATE-DD   PIC 9(2).
043900     05  FILLER              PIC X.
044000     05  W-NEW-INDEX-RECORD      PIC S9(02)V9(04).
044100     05  W-NEW-PR-INDEX-RECORD   PIC S9(02)V9(04).
044200
044300***************************************************************
044400*      THIS IS THE WAGE-INDEX RECORD THAT WILL BE PASSED TO   *
044500*      THE PPCAL052 PROGRAM AND AFTER FOR PROCESSING
044600*      CBSA TABLE
044700***************************************************************
044800 01  WAGE-NEW-CBSA-INDEX-RECORD.
044900     05  W-NEW-CBSA               PIC 9(5).
045000     05  W-NEW-CBSA-X  REDEFINES W-NEW-CBSA     PIC X(05).
045100     05  W-NEW-CBSA-SIZE              PIC X(01).
045200         88  NEW-CBSA-LARGE-URBAN       VALUE 'L'.
045300         88  NEW-CBSA-OTHER-URBAN       VALUE 'O'.
045400         88  NEW-CBSA-ALL-RURAL         VALUE 'R'.
045500     05  W-NEW-CBSA-EFF-DATE.
045600          10  W-NEW-CBSA-EFF-DATE-CC   PIC 9(2).
045700          10  W-NEW-CBSA-EFF-DATE-YMD.
045800              15  W-NEW-CBSA-EFF-DATE-YY   PIC 9(2).
045900              15  W-NEW-CBSA-EFF-DATE-MM   PIC 9(2).
046000              15  W-NEW-CBSA-EFF-DATE-DD   PIC 9(2).
046100     05  FILLER                      PIC X.
046200     05  W-NEW-CBSA-WI               PIC S9(02)V9(04).
046300     05  W-NEW-CBSA-PR-WI            PIC S9(02)V9(04).
046400
046500***************************************************************
046600*      THIS IS THE RURAL WAGE-INDEX RECORD THAT WILL BE COMPARED
046700*      TO THE URBAN CBSA RECORD FOR RURAL FLOOR PROCESSING
046800*      CBSA TABLE
046900***************************************************************
047000 01  WAGE-RURAL-CBSA-INDEX-RECORD.
047100     05  W-RURAL-CBSA             PIC 9(5).
047200     05  W-RURAL-CBSA-X REDEFINES W-RURAL-CBSA  PIC X(05).
047300     05  W-RURAL-CBSA-SIZE            PIC X(01).
047400         88  RURAL-CBSA-LARGE-URBAN     VALUE 'L'.
047500         88  RURAL-CBSA-OTHER-URBAN     VALUE 'O'.
047600         88  RURAL-CBSA-ALL-RURAL       VALUE 'R'.
047700     05  W-RURAL-CBSA-EFF-DATE.
047800          10  W-RURAL-CBSA-EFF-DATE-CC PIC 9(2).
047900          10  W-RURAL-CBSA-EFF-DATE-YMD.
048000              15  W-RURAL-CBSA-EFF-DATE-YY PIC 9(2).
048100              15  W-RURAL-CBSA-EFF-DATE-MM PIC 9(2).
048200              15  W-RURAL-CBSA-EFF-DATE-DD PIC 9(2).
048300     05  FILLER                      PIC X.
048400     05  W-RURAL-CBSA-WI             PIC S9(02)V9(04).
048500     05  W-RURAL-CBSA-PR-WI          PIC S9(02)V9(04).
048600
048700***************************************************************
048800*      NON-MILLINNIUM                                         *
048900*      THIS IS THE WAGE-INDEX RECORD THAT WILL BE PASSED TO   *
049000*      PPCAL983 PROGRAM  AND PRIOR YEARS FOR PROCESSING       *
049100***************************************************************
049200 01  WAGE-INDEX-RECORD.
049300     05  W-MSA               PIC 9(4).
049400     05  W-SIZE              PIC X(01).
049500         88  LARGE-URBAN       VALUE 'L'.
049600         88  OTHER-URBAN       VALUE 'O'.
049700         88  ALL-RURAL         VALUE 'R'.
049800     05  W-EFF-DATE          PIC 9(6).
049900     05  FILLER              PIC X.
050000     05  W-INDEX-RECORD      PIC S9(02)V9(04).
050100     05  W-PR-INDEX-RECORD   PIC S9(02)V9(04).
050200
050300**************************************************************
050400*      MILLINNIUM COMPATIBLE                                 *
050500*      THIS IS THE PROV-RECORD THAT WILL BE PASSED TO        *
050600*      THE PPCAL001 PROGRAM AND AFTER FOR PROCESSING         *
050700*      IN THE NEW FORMAT                                     *
050800**************************************************************
050900 01  PROV-NEW-HOLD.
051000     02  PROV-NEWREC-HOLD1.
051100         05  P-NEW-NPI10.
051200             10  P-NEW-NPI8             PIC X(08).
051300             10  P-NEW-NPI-FILLER       PIC X(02).
051400         05  P-NEW-PROVIDER-NO.
051500             10  P-NEW-STATE            PIC X(02).
051600                 88  P-PR-NEW-STATE     VALUE '40' '84'.
051700             10  FILLER                 PIC X(04).
051800         05  P-NEW-DATE-DATA.
051900             10  P-NEW-EFF-DATE.
052000                 15  P-NEW-EFF-DT-CC    PIC 9(02).
052100                 15  P-NEW-EFF-DT-YY    PIC 9(02).
052200                 15  P-NEW-EFF-DT-MM    PIC 9(02).
052300                 15  P-NEW-EFF-DT-DD    PIC 9(02).
052400             10  P-NEW-FY-BEGIN-DATE.
052500                 15  P-NEW-FY-BEG-DT-CC PIC 9(02).
052600                 15  P-NEW-FY-BEG-DT-YY PIC 9(02).
052700                 15  P-NEW-FY-BEG-DT-MM PIC 9(02).
052800                 15  P-NEW-FY-BEG-DT-DD PIC 9(02).
052900             10  P-NEW-REPORT-DATE.
053000                 15  P-NEW-REPORT-DT-CC PIC 9(02).
053100                 15  P-NEW-REPORT-DT-YY PIC 9(02).
053200                 15  P-NEW-REPORT-DT-MM PIC 9(02).
053300                 15  P-NEW-REPORT-DT-DD PIC 9(02).
053400             10  P-NEW-TERMINATION-DATE.
053500                 15  P-NEW-TERM-DT-CC   PIC 9(02).
053600                 15  P-NEW-TERM-DT-YY   PIC 9(02).
053700                 15  P-NEW-TERM-DT-MM   PIC 9(02).
053800                 15  P-NEW-TERM-DT-DD   PIC 9(02).
053900         05  P-NEW-WAIVER-CODE          PIC X(01).
054000             88  P-NEW-WAIVER-STATE       VALUE 'Y'.
054100         05  P-NEW-INTER-NO             PIC 9(05).
054200         05  P-NEW-PROVIDER-TYPE        PIC X(02).
054300             88  P-N-SOLE-COMMUNITY-PROV    VALUE '01' '11'.
054400             88  P-N-REFERRAL-CENTER        VALUE '07' '11'
054500                                                  '15' '17'
054600                                                  '22'.
054700             88  P-N-INDIAN-HEALTH-SERVICE  VALUE '08'.
054800             88  P-N-REDESIGNATED-RURAL-YR1 VALUE '09'.
054900             88  P-N-REDESIGNATED-RURAL-YR2 VALUE '10'.
055000             88  P-N-SOLE-COM-REF-CENT      VALUE '11'.
055100             88  P-N-MDH-REBASED-FY90       VALUE '14' '15'.
055200             88  P-N-MDH-RRC-REBASED-FY90   VALUE '15'.
055300             88  P-N-SCH-REBASED-FY90       VALUE '16' '17'.
055400             88  P-N-SCH-RRC-REBASED-FY90   VALUE '17'.
055500             88  P-N-MEDICAL-ASSIST-FACIL   VALUE '18'.
055600             88  P-N-EACH                   VALUE '21' '22'.
055700             88  P-N-EACH-REFERRAL-CENTER   VALUE '22'.
055800             88  P-N-NHCMQ-II-SNF           VALUE '32'.
055900             88  P-N-NHCMQ-III-SNF          VALUE '33'.
056000         05  P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
056100             88  P-N-NEW-ENGLAND            VALUE  1.
056200             88  P-N-MIDDLE-ATLANTIC        VALUE  2.
056300             88  P-N-SOUTH-ATLANTIC         VALUE  3.
056400             88  P-N-EAST-NORTH-CENTRAL     VALUE  4.
056500             88  P-N-EAST-SOUTH-CENTRAL     VALUE  5.
056600             88  P-N-WEST-NORTH-CENTRAL     VALUE  6.
056700             88  P-N-WEST-SOUTH-CENTRAL     VALUE  7.
056800             88  P-N-MOUNTAIN               VALUE  8.
056900             88  P-N-PACIFIC                VALUE  9.
057000         05  P-NEW-CURRENT-DIV   REDEFINES
057100                    P-NEW-CURRENT-CENSUS-DIV   PIC 9(01).
057200             88  P-N-VALID-CENSUS-DIV    VALUE 1 THRU 9.
057300         05  P-NEW-MSA-DATA.
057400             10  P-NEW-CHG-CODE-INDEX       PIC X.
057500             10  P-NEW-GEO-LOC-MSAX         PIC X(04) JUST RIGHT.
057600             10  P-NEW-GEO-LOC-MSA9   REDEFINES
057700                             P-NEW-GEO-LOC-MSAX  PIC 9(04).
057800             10  P-NEW-GEO-LOC-MSA-AST REDEFINES
057900                             P-NEW-GEO-LOC-MSA9.
058000                 15  P-NEW-GEO-MSA-1ST    PIC X.
058100                 15  P-NEW-GEO-MSA-2ND    PIC X.
058200                 15  P-NEW-GEO-MSA-3RD    PIC X.
058300                 15  P-NEW-GEO-MSA-4TH    PIC X.
058400             10  P-NEW-WAGE-INDEX-LOC-MSA   PIC X(04) JUST RIGHT.
058500             10  P-NEW-STAND-AMT-LOC-MSA    PIC X(04) JUST RIGHT.
058600             10  P-NEW-STAND-AMT-LOC-MSA9
058700       REDEFINES P-NEW-STAND-AMT-LOC-MSA.
058800                 15  P-NEW-RURAL-1ST.
058900                     20  P-NEW-STAND-RURAL  PIC XX.
059000                         88  P-NEW-STD-RURAL-CHECK VALUE '  '.
059100                 15  P-NEW-RURAL-2ND        PIC XX.
059200         05  P-NEW-SOL-COM-DEP-HOSP-YR PIC XX.
059300                 88  P-NEW-SCH-YRBLANK    VALUE   '  '.
059400                 88  P-NEW-SCH-YR82       VALUE   '82'.
059500                 88  P-NEW-SCH-YR87       VALUE   '87'.
059600         05  P-NEW-LUGAR                    PIC X.
059700         05  P-NEW-TEMP-RELIEF-IND          PIC X.
059800             88  P-NEW-LOW-VOL25PCT     VALUE 'Y'.
059900***          Y = LOW VOLUME PERCENTAGE  25 % ADD ON
060000         05  P-NEW-FED-PPS-BLEND-IND        PIC X.
060100         05  P-NEW-STATE-CODE               PIC 9(02).
060200         05  P-NEW-STATE-CODE-X REDEFINES
060300             P-NEW-STATE-CODE               PIC X(02).
060400         05  FILLER                         PIC X(03).
060500     02  PROV-NEWREC-HOLD2.
060600         05  P-NEW-VARIABLES.
060700             10  P-NEW-CMI-ADJ-CPD       PIC  9(05)V9(02).
060800             10  P-NEW-COLA              PIC  9(01)V9(03).
060900             10  P-NEW-INTERN-RATIO      PIC  9(01)V9(04).
061000             10  P-NEW-BED-SIZE          PIC  9(05).
061100             10  P-NEW-CCR               PIC  9(01)V9(03).
061200             10  P-NEW-CMI               PIC  9(01)V9(04).
061300             10  P-NEW-SSI-RATIO         PIC  V9(04).
061400             10  P-NEW-MEDICAID-RATIO    PIC  V9(04).
061500             10  P-NEW-PPS-BLEND-YR-IND  PIC  X(01).
061600             10  P-NEW-PRUP-UPDTE-FACTOR PIC  9(01)V9(05).
061700             10  P-NEW-DSH-PERCENT       PIC  V9(04).
061800             10  P-NEW-FYE-DATE.
061900                 15  P-NEW-FYE-CC        PIC 99.
062000                 15  P-NEW-FYE-YY        PIC 99.
062100                 15  P-NEW-FYE-MM        PIC 99.
062200                 15  P-NEW-FYE-DD        PIC 99.
062300         05  P-NEW-CBSA-DATA.
062400             10  P-NEW-CBSA-SPEC-PAY-IND    PIC X.
062500                 88  P-NEW-CBSA-WI-GEO        VALUE 'N'.
062600                 88  P-NEW-CBSA-WI-RECLASS    VALUE 'Y'.
062700                 88  P-NEW-CBSA-WI-SPECIAL    VALUE '1' '2'.
062800***                  1 = ANYTHING OR HOLD HARMLESS WITH SPEC WI
062900***                  2 = RECLASS WITH SPEC WI
063000                 88  P-NEW-CBSA-WI-DUAL       VALUE 'D'.
063100                 88  P-NEW-CBSA-WI-BLANK      VALUE ' ' '0'.
063200             10  P-NEW-CBSA-HOSP-QUAL-IND  PIC X.
063300                 88  P-NEW-CBSA-HOSP-QUAL-MET   VALUE '1'.
063400                 88  P-NEW-CBSA-HOSP-QUAL-25PER VALUE '2'.
063500                 88  P-NEW-CBSA-HOSP-QUAL-BOTH  VALUE '3'.
063600             10  P-NEW-CBSA-GEO-LOC        PIC X(05) JUST RIGHT.
063700             10  P-NEW-CBSA-GEO-LOC9  REDEFINES
063800                             P-NEW-CBSA-GEO-LOC  PIC 9(05).
063900             10  P-NEW-CBSA-GEO-LOC-AST REDEFINES
064000                             P-NEW-CBSA-GEO-LOC9.
064100                 15  P-NEW-CBSA-GEO-1ST    PIC X.
064200                 15  P-NEW-CBSA-GEO-2ND    PIC X.
064300                 15  P-NEW-CBSA-GEO-3RD    PIC X.
064400                 15  P-NEW-CBSA-GEO-4TH    PIC X.
064500                 15  P-NEW-CBSA-GEO-5TH    PIC X.
064600             10  P-NEW-CBSA-RECLASS-LOC    PIC X(05) JUST RIGHT.
064700             10  P-NEW-CBSA-STAND-AMT-LOC  PIC X(05) JUST RIGHT.
064800             10  P-NEW-CBSA-STAND-AMT-LOC-MSA9
064900       REDEFINES P-NEW-CBSA-STAND-AMT-LOC.
065000               15  P-NEW-CBSA-RURAL-1ST.
065100                   20  P-NEW-CBSA-STAND-RURAL  PIC XXX.
065200                      88  P-NEW-CBSA-STD-RURAL-CHECK VALUE '   '.
065300               15  P-NEW-CBSA-RURAL-2ND    PIC XX.
065400             10  P-NEW-CBSA-SPEC-WI          PIC 9(02)V9(04).
065500             10  P-NEW-CBSA-SPEC-WI-N  REDEFINES
065600                 P-NEW-CBSA-SPEC-WI          PIC 9(06).
065700     02  PROV-NEWREC-HOLD3.
065800         05  P-NEW-PASS-AMT-DATA.
065900             10  P-NEW-PASS-AMT-CAPITAL    PIC 9(04)V99.
066000             10  P-NEW-PASS-AMT-DIR-MED-ED PIC 9(04)V99.
066100             10  P-NEW-PASS-AMT-ORGAN-ACQ  PIC 9(04)V99.
066200             10  P-NEW-PASS-AMT-PLUS-MISC  PIC 9(04)V99.
066300         05  P-NEW-CAPI-DATA.
066400             15  P-NEW-CAPI-PPS-PAY-CODE   PIC X.
066500             15  P-NEW-CAPI-HOSP-SPEC-RATE PIC 9(04)V99.
066600             15  P-NEW-CAPI-OLD-HARM-RATE  PIC 9(04)V99.
066700             15  P-NEW-CAPI-NEW-HARM-RATIO PIC 9(01)V9999.
066800             15  P-NEW-CAPI-CSTCHG-RATIO   PIC 9V999.
066900             15  P-NEW-CAPI-NEW-HOSP       PIC X.
067000             15  P-NEW-CAPI-IME            PIC 9V9999.
067100             15  P-NEW-CAPI-EXCEPTIONS     PIC 9(04)V99.
067200         05  P-NEW-HVBP-HRR-DATA.
067300             15  P-NEW-VAL-BASED-PURCH-PARTIPNT PIC X.
067400             15  P-NEW-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
067500             15  P-NEW-HOSP-READMISSION-REDU    PIC X.
067600             15  P-NEW-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
067700         05  P-MODEL1-BUNDLE-DATA.
067800             15  P-MODEL1-BUNDLE-DISPRCNT   PIC V999.
067900             15  P-HAC-REDUC-IND            PIC X.
068000             15  P-UNCOMP-CARE-AMOUNT       PIC 9(07)V99.
068100             15  P-NEW-EHR-REDUC-INDN           PIC X.
068200             15  P-LV-ADJ-FACTOR            PIC 9V9(6).
068300         05  P-NEW-COUNTY-CODE              PIC 9(05).
068400         05  P-NEW-COUNTY-CODE-X REDEFINES
068500             P-NEW-COUNTY-CODE              PIC X(05).
068600         05  P-NEW-SUPPLEMENTAL-WI.
068700             10  P-NEW-SUPP-WI-IND          PIC X.
068800                 88  P-NEW-IND-PRIOR-YEAR   VALUE '1'.
068900             10  P-NEW-SUPP-WI              PIC 9(02)V9(04).
069000         05  P-PASS-THRU-ALLO-STEM-CELL     PIC 9(07)V9(02).
069100         05  FILLER                         PIC X(31).
069200*
069300**************************************************************
069400*      THIS IS THE PROV-RECORD THAT WILL BE PASSED TO        *
069500*      THE PPCAL984 PROGRAM AND PRIOR PPCAL MODULES          *
069600*      FOR PROCESSING --- OLD PSF FORMAT                     *
069700**************************************************************
069800 01  PROV-HOLD.
069900     02  PROV-REC-HOLD1.
070000         05  P-PROVIDER-NO.
070100             10  P-STATE                PIC X(02).
070200             10  FILLER                 PIC X(04).
070300         05  P-EFF-DATE.
070400             10  P-EFF-YY               PIC 9(02).
070500             10  P-EFF-MM               PIC 9(02).
070600             10  P-EFF-DD               PIC 9(02).
070700         05  P-WAIVER-CODE              PIC X(01).
070800             88  WAIVER-STATE           VALUE 'Y'.
070900         05  P-PROVIDER-TYPE            PIC X(02).
071000             88  SOLE-COMMUNITY-PROV    VALUE '01' '11'.
071100             88  REFERRAL-CENTER        VALUE '07' '11' '15' '17'.
071200             88  INDIAN-HEALTH-SERVICE  VALUE '08'.
071300             88  REDESIGNATED-RURAL-YR1 VALUE '09'.
071400             88  REDESIGNATED-RURAL-YR2 VALUE '10'.
071500             88  SOLE-COM-REF-CENT      VALUE '11'.
071600             88  MDH-REBASED-FY90       VALUE '14' '15'.
071700             88  MDH-RRC-REBASED-FY90   VALUE '15'.
071800             88  SCH-REBASED-FY90       VALUE '16' '17'.
071900             88  SCH-RRC-REBASED-FY90   VALUE '17'.
072000             88  MEDICAL-ASSIST-FACIL   VALUE '18'.
072100             88  EACH                   VALUE '21'.
072200             88  EACH-REF-CTR           VALUE '22'.
072300         05  P-CURRENT-CENSUS-DIV       PIC 9(01).
072400             88  NEW-ENGLAND            VALUE  1.
072500             88  MIDDLE-ATLANTIC        VALUE  2.
072600             88  SOUTH-ATLANTIC         VALUE  3.
072700             88  EAST-NORTH-CENTRAL     VALUE  4.
072800             88  EAST-SOUTH-CENTRAL     VALUE  5.
072900             88  WEST-NORTH-CENTRAL     VALUE  6.
073000             88  WEST-SOUTH-CENTRAL     VALUE  7.
073100             88  MOUNTAIN               VALUE  8.
073200             88  PACIFIC                VALUE  9.
073300         05  P-PPS-BLEND-YEAR           PIC 9(01).
073400             88  VALID-PPS-BLEND-YEAR   VALUE 0 THRU 9.
073500         05  P-MSA-X.
073600             10  P-RURAL                PIC X(04).
073700                 88  RURAL              VALUE   '9999'.
073800         05  P-MSA-9 REDEFINES P-MSA-X  PIC 9(04).
073900         05  P-FISCAL-YEAR-END.
074000             10  P-MM                   PIC 9(02).
074100             10  P-DD                   PIC 9(02).
074200             10  P-YY                   PIC 9(02).
074300         05  P-VARIABLES.
074400             10  P-CMI-ADJ-CPD          PIC S9(05)V9(02).
074500             10  P-COLA                 PIC S9(01)V9(03).
074600             10  P-INTERN-RATIO         PIC S9(01)V9(04).
074700             10  PRUP-UPDT-FACTOR       PIC S9(01)V9(05).
074800             10  P-BED-SIZE             PIC  9(05).
074900             10  P-DSH-PERCENT          PIC V9(04).
075000             10  P-CCR                  PIC  9(01)V9(03).
075100             10  P-CMI                  PIC  9(01)V9(04).
075200             10  FILLER                 PIC  9(01).
075300             10  P-REPORT-DATE          PIC  9(06).
075400             10  FILLER                 PIC  9(01).
075500             10  P-INTER-NO             PIC  9(05).
075600     02  PROV-REC-HOLD2.
075700         05  P-FY-BEGIN-DATE            PIC 9(6).
075800         05  P-PASS-AMT-CAPITAL         PIC 9(4)V99.
075900         05  P-PASS-AMT-DIR-MED-ED      PIC 9(4)V99.
076000         05  P-PASS-AMT-ORGAN-ACQ       PIC 9(4)V99.
076100         05  P-PASS-AMT-PLUS-MISC       PIC 9(4)V99.
076200         05  P-SSI-RATIO                PIC V9(4).
076300         05  P-MEDICAID-RATIO           PIC V9(4).
076400         05  P-TERMINATION-DATE         PIC X(6).
076500         05  P-WAGE-INDEX-LOC-MSA       PIC X(4).
076600         05  P-CHG-CODE-INDEX           PIC X.
076700         05  P-STAND-AMT-LOC-MSA.
076800             10  P-RURAL-1ST.
076900                 88  P-RURAL-CHECK        VALUE '  '.
077000                 15  P-STAND-RURAL      PIC XX.
077100             10  P-RURAL-2ND            PIC XX.
077200         05  P-CAPI-SOL-HOSP-RATE       PIC XX.
077300         05  P-LUGAR                    PIC X.
077400         05  P-TEMP-RELIEF-IND          PIC X.
077500         05  FILLER                     PIC X(23).
077600     02  PROV-REC-HOLD3.
077700         05  P-CAPI-PPS-PAY-CODE        PIC X.
077800         05  P-CAPI-HOSP-SPEC-RATE      PIC 9(4)V99.
077900         05  P-CAPI-OLD-HARM-RATE       PIC 9(4)V99.
078000         05  P-CAPI-NEW-HARM-RATIO      PIC 9(1)V9999.
078100         05  P-CAPI-CSTCHG-RATIO        PIC 9V999.
078200         05  P-CAPI-NEW-HOSP            PIC X.
078300         05  P-CAPI-IME                 PIC 9V9999.
078400         05  P-CAPI-EXCEPTIONS          PIC 9(4)V99.
078500         05  P-HVBP-HRR-DATA.
078600             15  P-VAL-BASED-PURCH-PARTIPNT PIC X.
078700             15  P-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
078800             15  P-HOSP-READMISSION-REDU    PIC X.
078900             15  P-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
079000         05  FILLER                         PIC X(04).
079100
079200************************************************************************
079300* OLD BILL FORMAT                                                      *
079400*                                                                      *
079500* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL983 PROGRAM  *
079600* AND PRIOR PPCAL MODULES FOR PROCESSING.                              *
079700*                                                                      *
079800* B-CHARGES-CLAIMED = TOTAL COVERED CHARGES ON THE 0001 (TOTALS        *
079900* LINE) MINUS BLOOD CLOT COST, KIDNEY COSTS, ACQUISITION COSTS AND     *
080000* TECHNICAL PROVIDER CHARGES.                                          *
080100************************************************************************
080200 01  BILL-DATA.
080300         10  B-PROVIDER-NO          PIC X(06).
080400         10  B-REVIEW-CODE          PIC 9(02).
080500             88  VALID-REVIEW-CODE     VALUE 00 THRU 08.
080600             88  PAY-WITH-OUTLIER      VALUE 00 07.
080700             88  PAY-DAYS-OUTLIER      VALUE 01.
080800             88  PAY-COST-OUTLIER      VALUE 02.
080900             88  PAY-PERDIEM-DAYS      VALUE 03.
081000             88  PAY-AVG-STAY-ONLY     VALUE 04.
081100             88  PAY-XFER-WITH-COST    VALUE 05.
081200             88  PAY-XFER-NO-COST      VALUE 06.
081300             88  PAY-WITHOUT-COST      VALUE 07.
081400             88  PAY-DRG-480           VALUE 08.
081500         10  B-DRG                  PIC 9(03).
081600         10  B-LOS                  PIC 9(03).
081700         10  B-COVERED-DAYS         PIC 9(03).
081800         10  B-LTR-DAYS             PIC 9(02).
081900         10  B-DISCHARGE-DATE.
082000             15  B-DISCHG-MM        PIC 9(02).
082100             15  B-DISCHG-DD        PIC 9(02).
082200             15  B-DISCHG-YY        PIC 9(02).
082300         10  B-CHARGES-CLAIMED      PIC 9(07)V9(02).
082400
082500************************************************************************
082600* NEW BILL FORMAT (MILLINNIUM COMPATIBLE)                              *
082700*                                                                      *
082800* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL001 PROGRAM  *
082900* AND AFTER FOR PROCESSING IN THE NEW FORMAT.                          *
083000*                                                                      *
083100* B-N-CHARGES-CLAIMED = TOTAL COVERED CHARGES ON THE 0001 (TOTALS      *
083200* LINE) MINUS BLOOD CLOT COST, KIDNEY COSTS, ACQUISITION COSTS AND     *
083300* TECHNICAL PROVIDER CHARGES.                                          *
083400************************************************************************
083500 01  BILL-NEW-DATA.
083600         10  B-N-NPI10.
083700            15  B-N-NPI8              PIC X(08).
083800            15  B-N-NPI-FILLER        PIC X(02).
083900         10  B-N-PROVIDER-NO          PIC X(06).
084000         10  B-N-REVIEW-CODE          PIC 9(02).
084100             88  N-VALID-REVIEW-CODE    VALUE 00 THRU 09 11.
084200             88  N-PAY-WITH-OUTLIER     VALUE 00 07.
084300             88  N-PAY-DAYS-OUTLIER     VALUE 01.
084400             88  N-PAY-COST-OUTLIER     VALUE 02.
084500             88  N-PAY-PERDIEM-DAYS     VALUE 03.
084600             88  N-PAY-AVG-STAY-ONLY    VALUE 04.
084700             88  N-PAY-XFER-WITH-COST   VALUE 05.
084800             88  N-PAY-XFER-NO-COST     VALUE 06.
084900             88  N-PAY-WITHOUT-COST     VALUE 07.
085000             88  N-PAY-DRG-480          VALUE 08.
085100             88  N-PAY-XFER-SPEC-DRG    VALUE 09 11.
085200             88  N-PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
085300         10  B-N-DRG                  PIC 9(03).
085400         10  B-N-LOS                  PIC 9(03).
085500         10  B-N-COVERED-DAYS         PIC 9(03).
085600         10  B-N-LTR-DAYS             PIC 9(02).
085700         10  B-N-DISCHARGE-DATE.
085800             15  B-N-DISCHG-CC        PIC 9(02).
085900             15  B-N-DISCHG-YY        PIC 9(02).
086000             15  B-N-DISCHG-MM        PIC 9(02).
086100             15  B-N-DISCHG-DD        PIC 9(02).
086200         10  B-N-CHARGES-CLAIMED      PIC 9(07)V9(02).
086300         10  B-N-OTHER-PROC-CODES.
086400             15  B-N-PRIN-PROC-CODE       PIC X(07).
086500             15  B-N-OTHER-PROC-CODE1     PIC X(07).
086600             15  B-N-OTHER-PROC-CODE2     PIC X(07).
086700             15  B-N-OTHER-PROC-CODE3     PIC X(07).
086800             15  B-N-OTHER-PROC-CODE4     PIC X(07).
086900             15  B-N-OTHER-PROC-CODE5     PIC X(07).
087000             15  B-N-OTHER-PROC-CODE6     PIC X(07).
087100             15  B-N-OTHER-PROC-CODE7     PIC X(07).
087200             15  B-N-OTHER-PROC-CODE8     PIC X(07).
087300             15  B-N-OTHER-PROC-CODE9     PIC X(07).
087400             15  B-N-OTHER-PROC-CODE10    PIC X(07).
087500             15  B-N-OTHER-PROC-CODE11    PIC X(07).
087600             15  B-N-OTHER-PROC-CODE12    PIC X(07).
087700             15  B-N-OTHER-PROC-CODE13    PIC X(07).
087800             15  B-N-OTHER-PROC-CODE14    PIC X(07).
087900             15  B-N-OTHER-PROC-CODE15    PIC X(07).
088000             15  B-N-OTHER-PROC-CODE16    PIC X(07).
088100             15  B-N-OTHER-PROC-CODE17    PIC X(07).
088200             15  B-N-OTHER-PROC-CODE18    PIC X(07).
088300             15  B-N-OTHER-PROC-CODE19    PIC X(07).
088400             15  B-N-OTHER-PROC-CODE20    PIC X(07).
088500             15  B-N-OTHER-PROC-CODE21    PIC X(07).
088600             15  B-N-OTHER-PROC-CODE22    PIC X(07).
088700             15  B-N-OTHER-PROC-CODE23    PIC X(07).
088800             15  B-N-OTHER-PROC-CODE24    PIC X(07).
088900         10  B-N-OTHER-DIAG-CODES.
089000             15  B-N-OTHER-DIAG-CODE1     PIC X(07).
089100             15  B-N-OTHER-DIAG-CODE2     PIC X(07).
089200             15  B-N-OTHER-DIAG-CODE3     PIC X(07).
089300             15  B-N-OTHER-DIAG-CODE4     PIC X(07).
089400             15  B-N-OTHER-DIAG-CODE5     PIC X(07).
089500             15  B-N-OTHER-DIAG-CODE6     PIC X(07).
089600             15  B-N-OTHER-DIAG-CODE7     PIC X(07).
089700             15  B-N-OTHER-DIAG-CODE8     PIC X(07).
089800             15  B-N-OTHER-DIAG-CODE9     PIC X(07).
089900             15  B-N-OTHER-DIAG-CODE10    PIC X(07).
090000             15  B-N-OTHER-DIAG-CODE11    PIC X(07).
090100             15  B-N-OTHER-DIAG-CODE12    PIC X(07).
090200             15  B-N-OTHER-DIAG-CODE13    PIC X(07).
090300             15  B-N-OTHER-DIAG-CODE14    PIC X(07).
090400             15  B-N-OTHER-DIAG-CODE15    PIC X(07).
090500             15  B-N-OTHER-DIAG-CODE16    PIC X(07).
090600             15  B-N-OTHER-DIAG-CODE17    PIC X(07).
090700             15  B-N-OTHER-DIAG-CODE18    PIC X(07).
090800             15  B-N-OTHER-DIAG-CODE19    PIC X(07).
090900             15  B-N-OTHER-DIAG-CODE20    PIC X(07).
091000             15  B-N-OTHER-DIAG-CODE21    PIC X(07).
091100             15  B-N-OTHER-DIAG-CODE22    PIC X(07).
091200             15  B-N-OTHER-DIAG-CODE23    PIC X(07).
091300             15  B-N-OTHER-DIAG-CODE24    PIC X(07).
091400             15  B-N-OTHER-DIAG-CODE25    PIC X(07).
091500         10  B-N-DEMO-DATA.
091600             15  B-N-DEMO-CODE1        PIC X(02).
091700             15  B-N-DEMO-CODE2        PIC X(02).
091800             15  B-N-DEMO-CODE3        PIC X(02).
091900             15  B-N-DEMO-CODE4        PIC X(02).
092000         10  B-N-NDC-DATA.
092100             15  B-N-NDC-NUMBER        PIC X(11).
092200         10  B-N-COND-DATA.
092300             15  B-N-COND-CODE1        PIC X(02).
092400             15  B-N-COND-CODE2        PIC X(02).
092500             15  B-N-COND-CODE3        PIC X(02).
092600             15  B-N-COND-CODE4        PIC X(02).
092700             15  B-N-COND-CODE5        PIC X(02).
092800         10  FILLER                     PIC X(63).
092900
093000****************************************************************
093100* OLD PPS-ADDITIONAL-VARIABLES FORMAT - PRE FY 2013            *
093200*                                                              *
093300* THIS IS THE PPS-ADDITIONAL-VARIABLES FORMAT WHICH IS PASSED  *
093400* TO THE PPCAL04E THROUGH PPCAL125 MODULES FOR PROCESSING.     *
093500* RECORD LENGTH: 1320                                          *
093600* TRANSFER ADJ. IS 9(01)V9(05)                                 *
093700* THIS RECORD WILL FILL PPS-ADDITIONAL-VARIABLES AFTER         *
093800* RETURNING FROM PPCAL FOR RETURN TO FISS.                     *
093900****************************************************************
094000 01  PPS-ADDITIONAL-VARIABLES-PRE13.
094100     02  PPS-VARIABLES-SECTION1-PRE13     PIC X(218).
094200     02  PPS-VARIABLES-SECTION2-PRE13.
094300         05  PPS-OTHER-VARIABLES-PRE13.
094400             10  PPS-NON-TEMP-RELIEF-PMT-PRE13    PIC 9(07)V9(02).
094500             10  PPS-NEW-TECH-PAY-ADD-ON-PRE13    PIC 9(07)V9(02).
094600             10  PPS-LOW-VOL-PAYMENT-PRE13        PIC 9(07)V9(02).
094700     02  PPS-VARIABLES-SECTION3-PRE13     PIC X(1075).
094800
094900****************************************************************
095000* OLD PPS-ADDITIONAL-VARIABLES FORMAT - FY 2013 & FY 2014      *
095100*                                                              *
095200* THIS IS THE PPS-ADDITIONAL-VARIABLES FORMAT WHICH IS PASSED  *
095300* TO THE PPCAL135 AND PPCAL14B MODULES FOR PROCESSING.         *
095400* RECORD LENGTH: 1319                                          *
095500* TRANSFER ADJ. IS 9(01)V9(04)                                 *
095600* THIS RECORD WILL FILL PPS-ADDITIONAL-VARIABLES AFTER         *
095700* RETURNING FROM PPCAL FOR RETURN TO FISS.                     *
095800****************************************************************
095900 01  PPS-ADDITIONAL-VARIABLES-1314.
096000     02  PPS-VARIABLES-SECTION1-1314      PIC X(218).
096100     02  PPS-VARIABLES-SECTION2-1314.
096200         05  PPS-OTHER-VARIABLES-1314.
096300             10  PPS-NON-TEMP-RELIEF-PMT-1314     PIC 9(07)V9(02).
096400             10  PPS-NEW-TECH-PAY-ADD-ON-1314     PIC 9(07)V9(02).
096500             10  PPS-LOW-VOL-PAYMENT-1314         PIC 9(07)V9(02).
096600     02  PPS-VARIABLES-SECTION3-1314      PIC X(1074).
096700
096800 LINKAGE SECTION.
096900
097000************************************************************************
097100* NEW BILL FORMAT (MILLINNIUM COMPATIBLE)                              *
097200* CHANGE TO THE NDC CODES PER FY 2021 CR# #####                        *
097300
097400* THIS IS THE BILL-RECORD THAT WILL BE PASSED TO THE PPCAL215 PROGRAM  *
097500* AND AFTER FOR PROCESSING IN THE NEW FORMAT.                          *
097600************************************************************************
097700 01  BILL-DATA-2021.
097800         10  B-21-NPI10.
097900             15  B-21-NPI8             PIC X(08).
098000             15  B-21-NPI-FILLER       PIC X(02).
098100         10  B-21-PROVIDER-NO         PIC X(06).
098200         10  B-21-REVIEW-CODE         PIC 9(02).
098300             88  N-VALID-REVIEW-CODE    VALUE 00 THRU 09 11.
098400             88  N-PAY-WITH-OUTLIER     VALUE 00 07.
098500             88  N-PAY-DAYS-OUTLIER     VALUE 01.
098600             88  N-PAY-COST-OUTLIER     VALUE 02.
098700             88  N-PAY-PERDIEM-DAYS     VALUE 03.
098800             88  N-PAY-AVG-STAY-ONLY    VALUE 04.
098900             88  N-PAY-XFER-WITH-COST   VALUE 05.
099000             88  N-PAY-XFER-NO-COST     VALUE 06.
099100             88  N-PAY-WITHOUT-COST     VALUE 07.
099200             88  N-PAY-DRG-480          VALUE 08.
099300             88  N-PAY-XFER-SPEC-DRG    VALUE 09 11.
099400             88  N-PAY-XFER-SPEC-DRG-NO-COST VALUE 11.
099500         10  B-21-DRG                 PIC 9(03).
099600         10  B-21-LOS                 PIC 9(03).
099700         10  B-21-COVERED-DAYS        PIC 9(03).
099800         10  B-21-LTR-DAYS            PIC 9(02).
099900         10  B-21-DISCHARGE-DATE.
100000             15  B-21-DISCHG-CC       PIC 9(02).
100100             15  B-21-DISCHG-YY       PIC 9(02).
100200             15  B-21-DISCHG-MM       PIC 9(02).
100300             15  B-21-DISCHG-DD       PIC 9(02).
100400         10  B-21-CHARGES-CLAIMED     PIC 9(07)V9(02).
100500         10  B-21-OTHER-PROC-CODES.
100600             15  B-21-PRIN-PROC-CODE      PIC X(07).
100700             15  B-21-OTHER-PROC-CODE1    PIC X(07).
100800             15  B-21-OTHER-PROC-CODE2    PIC X(07).
100900             15  B-21-OTHER-PROC-CODE3    PIC X(07).
101000             15  B-21-OTHER-PROC-CODE4    PIC X(07).
101100             15  B-21-OTHER-PROC-CODE5    PIC X(07).
101200             15  B-21-OTHER-PROC-CODE6    PIC X(07).
101300             15  B-21-OTHER-PROC-CODE7    PIC X(07).
101400             15  B-21-OTHER-PROC-CODE8    PIC X(07).
101500             15  B-21-OTHER-PROC-CODE9    PIC X(07).
101600             15  B-21-OTHER-PROC-CODE10   PIC X(07).
101700             15  B-21-OTHER-PROC-CODE11   PIC X(07).
101800             15  B-21-OTHER-PROC-CODE12   PIC X(07).
101900             15  B-21-OTHER-PROC-CODE13   PIC X(07).
102000             15  B-21-OTHER-PROC-CODE14   PIC X(07).
102100             15  B-21-OTHER-PROC-CODE15   PIC X(07).
102200             15  B-21-OTHER-PROC-CODE16   PIC X(07).
102300             15  B-21-OTHER-PROC-CODE17   PIC X(07).
102400             15  B-21-OTHER-PROC-CODE18   PIC X(07).
102500             15  B-21-OTHER-PROC-CODE19   PIC X(07).
102600             15  B-21-OTHER-PROC-CODE20   PIC X(07).
102700             15  B-21-OTHER-PROC-CODE21   PIC X(07).
102800             15  B-21-OTHER-PROC-CODE22   PIC X(07).
102900             15  B-21-OTHER-PROC-CODE23   PIC X(07).
103000             15  B-21-OTHER-PROC-CODE24   PIC X(07).
103100         10  B-21-OTHER-DIAG-CODES.
103200             15  B-21-OTHER-DIAG-CODE1    PIC X(07).
103300             15  B-21-OTHER-DIAG-CODE2    PIC X(07).
103400             15  B-21-OTHER-DIAG-CODE3    PIC X(07).
103500             15  B-21-OTHER-DIAG-CODE4    PIC X(07).
103600             15  B-21-OTHER-DIAG-CODE5    PIC X(07).
103700             15  B-21-OTHER-DIAG-CODE6    PIC X(07).
103800             15  B-21-OTHER-DIAG-CODE7    PIC X(07).
103900             15  B-21-OTHER-DIAG-CODE8    PIC X(07).
104000             15  B-21-OTHER-DIAG-CODE9    PIC X(07).
104100             15  B-21-OTHER-DIAG-CODE10   PIC X(07).
104200             15  B-21-OTHER-DIAG-CODE11   PIC X(07).
104300             15  B-21-OTHER-DIAG-CODE12   PIC X(07).
104400             15  B-21-OTHER-DIAG-CODE13   PIC X(07).
104500             15  B-21-OTHER-DIAG-CODE14   PIC X(07).
104600             15  B-21-OTHER-DIAG-CODE15   PIC X(07).
104700             15  B-21-OTHER-DIAG-CODE16   PIC X(07).
104800             15  B-21-OTHER-DIAG-CODE17   PIC X(07).
104900             15  B-21-OTHER-DIAG-CODE18   PIC X(07).
105000             15  B-21-OTHER-DIAG-CODE19   PIC X(07).
105100             15  B-21-OTHER-DIAG-CODE20   PIC X(07).
105200             15  B-21-OTHER-DIAG-CODE21   PIC X(07).
105300             15  B-21-OTHER-DIAG-CODE22   PIC X(07).
105400             15  B-21-OTHER-DIAG-CODE23   PIC X(07).
105500             15  B-21-OTHER-DIAG-CODE24   PIC X(07).
105600             15  B-21-OTHER-DIAG-CODE25   PIC X(07).
105700         10  B-21-DEMO-DATA.
105800             15  B-21-DEMO-CODE1       PIC X(02).
105900             15  B-21-DEMO-CODE2       PIC X(02).
106000             15  B-21-DEMO-CODE3       PIC X(02).
106100             15  B-21-DEMO-CODE4       PIC X(02).
106200         10  B-21-NDC-DATA.
106300             15  B-21-NDC-NUMBER1      PIC X(11).
106400             15  B-21-NDC-NUMBER2      PIC X(11).
106500             15  B-21-NDC-NUMBER3      PIC X(11).
106600             15  B-21-NDC-NUMBER4      PIC X(11).
106700             15  B-21-NDC-NUMBER5      PIC X(11).
106800             15  B-21-NDC-NUMBER6      PIC X(11).
106900             15  B-21-NDC-NUMBER7      PIC X(11).
107000             15  B-21-NDC-NUMBER8      PIC X(11).
107100             15  B-21-NDC-NUMBER9      PIC X(11).
107200             15  B-21-NDC-NUMBER10     PIC X(11).
107300         10  B-21-COND-DATA.
107400             15  B-21-COND-CODE1       PIC X(02).
107500             15  B-21-COND-CODE2       PIC X(02).
107600             15  B-21-COND-CODE3       PIC X(02).
107700             15  B-21-COND-CODE4       PIC X(02).
107800             15  B-21-COND-CODE5       PIC X(02).
107900
108000 01  PPS-DATA.
108100         10  PPS-RTC                PIC 9(02).
108200         10  PPS-WAGE-INDX          PIC 9(02)V9(04).
108300         10  PPS-OUTLIER-DAYS       PIC 9(03).
108400         10  PPS-AVG-LOS            PIC 9(02)V9(01).
108500         10  PPS-DAYS-CUTOFF        PIC 9(02)V9(01).
108600         10  PPS-OPER-IME-ADJ       PIC 9(06)V9(02).
108700         10  PPS-TOTAL-PAYMENT      PIC 9(07)V9(02).
108800         10  PPS-OPER-HSP-PART      PIC 9(06)V9(02).
108900         10  PPS-OPER-FSP-PART      PIC 9(06)V9(02).
109000         10  PPS-OPER-OUTLIER-PART  PIC 9(07)V9(02).
109100         10  PPS-REG-DAYS-USED      PIC 9(03).
109200         10  PPS-LTR-DAYS-USED      PIC 9(02).
109300         10  PPS-OPER-DSH-ADJ       PIC 9(06)V9(02).
109400         10  PPS-CALC-VERS          PIC 9(05).
109500
109600 01  PRICER-OPT-VERS-SW.
109700     02  PRICER-OPTION-SW               PIC X(01).
109800         88  ALL-TABLES-PASSED          VALUE 'A'.
109900         88  PROV-RECORD-PASSED         VALUE 'P'.
110000         88  ADDITIONAL-VARIABLES       VALUE 'M'.
110100     02  PPS-VERSIONS.
110200         10  PPDRV-VERSION              PIC X(05).
110300
110400 01  PPS-ADDITIONAL-VARIABLES.
110500     02  PPS-VARIABLES-SECTION1.
110600         05  PPS-HSP-PCT                PIC 9(01)V9(02).
110700         05  PPS-FSP-PCT                PIC 9(01)V9(02).
110800         05  PPS-NAT-PCT                PIC 9(01)V9(02).
110900         05  PPS-REG-PCT                PIC 9(01)V9(02).
111000         05  PPS-CMI-ADJ-CPD            PIC 9(05)V9(02).
111100         05  PPS-UPDATE-FACTOR          PIC 9(01)V9(05).
111200         05  PPS-DRG-WT                 PIC 9(02)V9(04).
111300         05  PPS-NAT-LABOR              PIC 9(05)V9(02).
111400         05  PPS-NAT-NLABOR             PIC 9(05)V9(02).
111500         05  PPS-REG-LABOR              PIC 9(05)V9(02).
111600         05  PPS-REG-NLABOR             PIC 9(05)V9(02).
111700         05  PPS-OPER-COLA              PIC 9(01)V9(03).
111800         05  PPS-INTERN-RATIO           PIC 9(01)V9(04).
111900         05  PPS-OPER-OUTLIER           PIC 9(07)V9(09).
112000         05  PPS-OPER-BILL-COSTS        PIC 9(07)V9(09).
112100         05  PPS-OPER-DOLLAR-THRESHOLD  PIC 9(07)V9(09).
112200         05  PPS-DSCHG-FRCTN            PIC 9(1)V9999.
112300         05  PPS-DRG-WT-FRCTN           PIC 9(2)V9999.
112400         05  PPS-CAPITAL-VARIABLES.
112500             10  PPS-CAPI-TOTAL-PAY         PIC S9(07)V9(02).
112600             10  PPS-CAPI-HSP               PIC S9(07)V9(02).
112700             10  PPS-CAPI-FSP               PIC S9(07)V9(02).
112800             10  PPS-CAPI-OUTLIER           PIC S9(07)V9(02).
112900             10  PPS-CAPI-OLD-HARM          PIC S9(07)V9(02).
113000             10  PPS-CAPI-DSH-ADJ           PIC S9(07)V9(02).
113100             10  PPS-CAPI-IME-ADJ           PIC S9(07)V9(02).
113200             10  PPS-CAPI-EXCEPTIONS        PIC S9(07)V9(02).
113300         05  PPS-CAPITAL2-VARIABLES.
113400             10  PPS-CAPI2-PAY-CODE          PIC X(1).
113500             10  PPS-CAPI2-B-FSP             PIC S9(07)V9(02).
113600             10  PPS-CAPI2-B-OUTLIER         PIC S9(07)V9(02).
113700     02  PPS-VARIABLES-SECTION2.
113800         05  PPS-OTHER-VARIABLES.
113900             10  PPS-NON-TEMP-RELIEF-PAYMENT PIC 9(07)V9(02).
114000             10  PPS-NEW-TECH-PAY-ADD-ON     PIC 9(07)V9(02).
114100             10  PPS-ISLET-ISOL-PAY-ADD-ON   PIC 9(07)V9(02).
114200             10  PPS-LOW-VOL-PAYMENT         PIC 9(07)V9(02).
114300     02  PPS-VARIABLES-SECTION3.
114400         05  PPS-HVBP-HRR-DATA.
114500             10  PPS-VAL-BASED-PURCH-PARTIPNT PIC X.
114600             10  PPS-VAL-BASED-PURCH-ADJUST   PIC 9V9(11).
114700             10  PPS-HOSP-READMISS-REDUCTN    PIC X.
114800             10  PPS-HOSP-HRR-ADJUSTMT        PIC 9V9(4).
114900         05  PPS-OPERATNG-DATA.
115000             10  PPS-MODEL1-BUNDLE-DISPRCNT  PIC V999.
115100             10  PPS-OPER-BASE-DRG-PAY       PIC 9(08)V99.
115200             10  PPS-OPER-HSP-AMT            PIC 9(08)V99.
115300        10  PPS-PC-VARIABLES.
115400             15  PPS-OPER-DSH                PIC 9(01)V9(04).
115500             15  PPS-CAPI-DSH                PIC 9(01)V9(04).
115600             15  PPS-CAPI-HSP-PCT            PIC 9(01)V9(02).
115700             15  PPS-CAPI-FSP-PCT            PIC 9(01)V9(04).
115800             15  PPS-ARITH-ALOS              PIC 9(02)V9(01).
115900             15  PPS-PR-WAGE-INDEX           PIC 9(02)V9(04).
116000             15  PPS-TRANSFER-ADJ            PIC 9(01)V9(04).
116100             15  PPS-PC-HMO-FLAG             PIC X(01).
116200             15  PPS-PC-COT-FLAG             PIC X(01).
116300             15  PPS-OPER-HSP-PART2          PIC 9(07)V9(02).
116400             15  PPS-BUNDLE-ADJUST-AMT       PIC S9(07)V99.
116500        10  PPS-ADDITIONAL-PAY-INFO-DATA.
116600             15  PPS-UNCOMP-CARE-AMOUNT         PIC S9(07)V9(02).
116700             15  PPS-BUNDLE-ADJUST-AMT          PIC S9(07)V9(02).
116800             15  PPS-VAL-BASED-PURCH-ADJUST-AMT PIC S9(07)V9(02).
116900             15  PPS-READMIS-ADJUST-AMT         PIC S9(07)V9(02).
117000        10  PPS-ADDITIONAL-PAY-INFO-DATA2.
117100             15  PPS-HAC-PROG-REDUC-IND         PIC X.
117200             15  PPS-EHR-PROG-REDUC-IND         PIC X.
117300             15  PPS-EHR-ADJUST-AMT             PIC S9(07)V9(02).
117400             15  PPS-STNDRD-VALUE               PIC S9(07)V9(02).
117500             15  PPS-HAC-PAYMENT-AMT            PIC S9(07)V9(02).
117600             15  PPS-FLX7-PAYMENT               PIC S9(07)V9(02).
117700        10  PPS-FILLER                          PIC X(0897).
117800*******************************************************
117900
118000 01  PROV-RECORD.
118100     05  PROV-RECORD1               PIC X(80).
118200     05  PROV-RECORD2               PIC X(80).
118300     05  PROV-RECORD3               PIC X(150).
118400
118500 01  MSAX-WI-TABLE.
118600     05  M-MSAX-DATA                OCCURS 9000
118700                                    INDEXED BY MU1 MU2 MU3.
118800         10  M-MSAX-MSA             PIC X(4).
118900         10  M-MSAX-SIZE            PIC X(01).
119000         10  M-MSAX-EFF-DATE        PIC X(08).
119100         10  M-MSAX-WAGE-INDX1      PIC S9(02)V9(04).
119200         10  M-MSAX-WAGE-INDX2      PIC S9(02)V9(04).
119300
119400 01  CBSA-WI-TABLE.
119500     05  T-CBSA-DATA                  OCCURS 8000
119600                                    INDEXED BY MA1 MA2 MA3.
119700         10  T-CBSA                   PIC X(5).
119800         10  T-CBSA-SIZE              PIC X(01).
119900         10  T-CBSA-EFF-DATE          PIC X(08).
120000         10  T-CBSA-WAGE-INDX1        PIC S9(02)V9(04).
120100         10  T-CBSA-WAGE-INDX2        PIC S9(02)V9(04).
120200         10  T-CBSA-WAGE-INDX3        PIC S9(02)V9(04).
120300
120400*******************************************************
120500*    HOLD VARIABLES POPULATED IN PPCAL___***          *
120600*******************************************************
120700 COPY PPHOLDAR.
120800
120900**YEARCHANGE 2015.2             **********************
121000*****************************************************************
121100 PROCEDURE DIVISION  USING BILL-DATA-2021
121200                           PPS-DATA
121300                           PRICER-OPT-VERS-SW
121400                           PPS-ADDITIONAL-VARIABLES
121500                           PROV-RECORD
121600                           MSAX-WI-TABLE
121700                           CBSA-WI-TABLE
121800                           PPHOLDAR-HOLD-AREA.
121900
122000*****************************************************************
122100*    PROCESSING:
122200*        A. THIS MODULE WILL CALL THE PPCAL MODULES.
122300*        B. THE PROV-RECORD AND WAGE-INDEX-RECORD ASSOCIATED WITH
122400*           EACH BILL WILL BE PASSED TO THE PPCAL PROGRAMS.
122500*****************************************************************
122600
122700     MOVE DRV-VERSION TO PPDRV-VERSION.
122800
122900     MOVE ALL '0' TO PPS-DATA.
123000     MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES-PRE13.
123100     MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES-1314.
123200     MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES.
123300
123400*    MOVE ZEROES  TO W-PR-INDEX-RECORD
123500*                    W-NEW-INDEX-RECORD
123600*                    W-NEW-PR-INDEX-RECORD
123700*                    W-NEW-CBSA-PR-WI
123800*                    W-NEW-CBSA-WI
123900*                    W-RURAL-CBSA-WI
124000*                    W-RURAL-CBSA-PR-WI
124100
124200*****************************************************************
124300
124400     INITIALIZE WAGE-NEW-INDEX-RECORD
124500                WAGE-NEW-CBSA-INDEX-RECORD
124600                WAGE-RURAL-CBSA-INDEX-RECORD
124700                WAGE-INDEX-RECORD
124800                MESWK-PRSPC-WAGEIN-BLEND
124900                HOLD-OUTM-DATA.
125000
125100     INITIALIZE PPHOLDAR-HOLD-AREA.
125200
125300     INITIALIZE W-FY-BEGIN-CC
125400                W-FY-BEGIN-YY
125500                W-FY-END-CC
125600                W-FY-END-YY.
125700
125800     MOVE PROV-RECORD TO PROV-NEW-HOLD.
125900
126000*----------------------------------------------------------*
126100* SET FY BEGIN AND END DATES USING BILL DISCHARGE DATE     *
126200*----------------------------------------------------------*
126300           MOVE B-21-DISCHG-CC TO W-FY-BEGIN-CC.
126400           MOVE B-21-DISCHG-CC TO W-FY-END-CC.
126500
126600*----------------------------------*
126700* FOR CLAIMS DISCHARGED JAN - SEPT *
126800*----------------------------------*
126900     IF B-21-DISCHG-MM >= 01 AND
127000        B-21-DISCHG-MM <= 09
127100        COMPUTE W-FY-BEGIN-YY = B-21-DISCHG-YY - 1
127200        MOVE B-21-DISCHG-YY TO W-FY-END-YY
127300
127400*----------------------------------*
127500* FOR CLAIMS DISCHARGED OCT - DEC  *
127600*----------------------------------*
127700     ELSE
127800        MOVE B-21-DISCHG-YY TO W-FY-BEGIN-YY
127900        COMPUTE W-FY-END-YY = B-21-DISCHG-YY + 1
128000        END-IF.
128100
128200*****************************************************************
128300
128400***     RTC = 98 >> A BILL OLDER THEN 20021001
128500***
128600***   THIS NEXT STATEMENT WILL ONLY ALLOW
128700***      THE LATEST 5 YEARS TO PROCESS
128800***
128900
129000***  IF B-21-DISCHARGE-DATE < 20021001
129100***          MOVE ALL '0' TO  PPS-ADDITIONAL-VARIABLES
129200***          MOVE 98 TO PPS-RTC
129300***          GOBACK.
129400***
129500*****************************************************************
129600
129700*EHR INDICATOR CHECK FOR 2015 FORWARD
129800
129900 CHECK-EHR-IND.
130000     IF B-21-DISCHARGE-DATE > 20140930 AND
130100        (P-NEW-EHR-REDUC-INDN NOT = 'Y' AND
130200         P-NEW-EHR-REDUC-INDN NOT = ' ')
130300           MOVE 65 TO PPS-RTC
130400             GOBACK
130500     END-IF.
130600
130700 0030-GET-WAGE-INDEX.
130800***  GET THE WAGE-INDEX
130900
131000     IF B-21-DISCHARGE-DATE > 19990930 AND
131100        B-21-DISCHARGE-DATE < 20001001
131200      IF (P-NEW-CHG-CODE-INDEX = 'Y' AND
131300          P-NEW-GEO-LOC-MSAX = P-NEW-WAGE-INDEX-LOC-MSA)
131400          AND (P-NEW-GEO-LOC-MSAX NOT = '1600' AND NOT = '3285'
131500                            AND   NOT = '5600' AND NOT = '1520'
131600                            AND   NOT = '1640' AND NOT = '0240'
131700                            AND   NOT = '3360' AND NOT = '1123')
131800          MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
131900          MOVE 52 TO PPS-RTC
132000          GOBACK.
132100
132200     IF P-NEW-EFF-DATE < 20041001
132300        IF '*' = P-NEW-GEO-MSA-1ST OR
132400                 P-NEW-GEO-MSA-2ND OR
132500                 P-NEW-GEO-MSA-3RD OR
132600                 P-NEW-GEO-MSA-4TH
132700         MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
132800         MOVE 52 TO PPS-RTC
132900         GOBACK.
133000
133100     IF P-NEW-EFF-DATE < 20041001 AND
133200        B-21-DISCHARGE-DATE > 20040930
133300        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
133400        MOVE 52 TO PPS-RTC
133500        GOBACK.
133600
133700     IF P-NEW-EFF-DATE > 20040930
133800        IF '*' = P-NEW-CBSA-GEO-1ST OR
133900                 P-NEW-CBSA-GEO-2ND OR
134000                 P-NEW-CBSA-GEO-3RD OR
134100                 P-NEW-CBSA-GEO-4TH OR
134200                 P-NEW-CBSA-GEO-5TH
134300         MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
134400         MOVE 52 TO PPS-RTC
134500         GOBACK.
134600
134700     IF P-NEW-EFF-DATE < 20041001
134800        IF (P-NEW-WAGE-INDEX-LOC-MSA = '    ' OR
134900            P-NEW-WAGE-INDEX-LOC-MSA = '0000')
135000            MOVE P-NEW-GEO-LOC-MSA9 TO P-NEW-WAGE-INDEX-LOC-MSA.
135100     IF P-NEW-EFF-DATE < 20041001
135200        IF (P-NEW-STAND-AMT-LOC-MSA = '    ' OR
135300            P-NEW-STAND-AMT-LOC-MSA = '0000')
135400            MOVE P-NEW-GEO-LOC-MSA9 TO P-NEW-STAND-AMT-LOC-MSA.
135500
135600****************************
135700* BEGIN OUTMIGRATION CHECK *
135800****************************
135900
136000     IF (P-NEW-CBSA-RECLASS-LOC = '     ' OR
136100         P-NEW-CBSA-RECLASS-LOC = '00000') AND
136200        (P-NEW-CBSA-STAND-AMT-LOC = '     ' OR
136300         P-NEW-CBSA-STAND-AMT-LOC = '00000') AND
136400         P-NEW-CBSA-WI-BLANK
136500       PERFORM 0900-GET-COUNTY-CODE THRU 0900-EXIT
136600     END-IF.
136700
136800     IF OUTM-IND = 1
136900       PERFORM 0950-GET-OUTM-ADJ THRU 0950-EXIT
137000         VARYING OUTM-IDX2 FROM OUTM-IDX BY 1 UNTIL
137100         OUTM-CNTY(OUTM-IDX2) NOT = P-NEW-COUNTY-CODE-X
137200     END-IF.
137300
137400**************************
137500* END OUTMIGRATION CHECK *
137600**************************
137700
137800     IF P-NEW-EFF-DATE > 20040930
137900        IF (P-NEW-CBSA-RECLASS-LOC = '     ' OR
138000            P-NEW-CBSA-RECLASS-LOC = '00000')
138100            MOVE P-NEW-CBSA-GEO-LOC9 TO P-NEW-CBSA-RECLASS-LOC.
138200     IF P-NEW-EFF-DATE > 20040930
138300        IF (P-NEW-CBSA-STAND-AMT-LOC = '     ' OR
138400            P-NEW-CBSA-STAND-AMT-LOC = '00000')
138500            MOVE P-NEW-CBSA-GEO-LOC9 TO P-NEW-CBSA-STAND-AMT-LOC.
138600
138700     IF P-NEW-EFF-DATE < 20041001
138800        PERFORM 0500-GET-MSA THRU 0500-EXIT
138900     ELSE
139000        PERFORM 0550-GET-CBSA THRU 0550-EXIT.
139100
139200***     RTC = 52  --  WAGE-INDEX NOT FOUND
139300     IF PPS-RTC = 52
139400          MOVE ALL '0' TO  PPS-ADDITIONAL-VARIABLES
139500          GOBACK.
139600
139700*****************************************************************
139800**            FY 2021 "            "                        *****
139900**          THIS NEXT CALL WILL PROCESS 2021 BILLS  WITH
140000**              A DISCHARGE DATE ON OR AFTER 20201001
140100*****************************************************************
140200     IF B-21-DISCHARGE-DATE
140300              > 20200930
140400         CALL  PPCAL215 USING BILL-DATA-2021
140500                              PPS-DATA
140600                              PRICER-OPT-VERS-SW
140700                              PPS-ADDITIONAL-VARIABLES
140800                              PROV-NEW-HOLD
140900                              WAGE-NEW-CBSA-INDEX-RECORD
141000                              PPHOLDAR-HOLD-AREA
141100         GOBACK.
141200
141300*****************************************************************
141400**   MOVE NEW DATA FORMAT INTO OLD DATA FORMAT
141500**   TO PROCESS BEFORE 19981001 NON-MILLENNNIUM STANDARD
141600*****************************************************************
141700
141800     MOVE B-21-PROVIDER-NO      TO B-N-PROVIDER-NO.
141900     MOVE B-21-REVIEW-CODE      TO B-N-REVIEW-CODE.
142000     MOVE B-21-DRG              TO B-N-DRG.
142100     MOVE B-21-LOS              TO B-N-LOS.
142200     MOVE B-21-COVERED-DAYS     TO B-N-COVERED-DAYS.
142300     MOVE B-21-LTR-DAYS         TO B-N-LTR-DAYS.
142400     MOVE B-21-DISCHG-CC        TO B-N-DISCHG-CC.
142500     MOVE B-21-DISCHG-YY        TO B-N-DISCHG-YY.
142600     MOVE B-21-DISCHG-MM        TO B-N-DISCHG-MM.
142700     MOVE B-21-DISCHG-DD        TO B-N-DISCHG-DD.
142800     MOVE B-21-CHARGES-CLAIMED  TO B-N-CHARGES-CLAIMED.
142900     MOVE B-21-OTHER-PROC-CODES TO B-N-OTHER-PROC-CODES.
143000     MOVE B-21-OTHER-DIAG-CODES TO B-N-OTHER-DIAG-CODES.
143100     MOVE B-21-DEMO-DATA        TO B-N-DEMO-DATA.
143200     MOVE B-21-NDC-NUMBER1      TO B-N-NDC-NUMBER.
143300     MOVE B-21-COND-DATA        TO B-N-COND-DATA.
143400
143500*****************************************************************
143600**            FY 2020 "            "                        *****
143700**          THIS NEXT CALL WILL PROCESS 2020 BILLS  WITH
143800**              A DISCHARGE DATE ON OR AFTER 20191001
143900*****************************************************************
144000     IF B-21-DISCHARGE-DATE
144100              > 20190930
144200         CALL  PPCAL204 USING BILL-NEW-DATA
144300                              PPS-DATA
144400                              PRICER-OPT-VERS-SW
144500                              PPS-ADDITIONAL-VARIABLES
144600                              PROV-NEW-HOLD
144700                              WAGE-NEW-CBSA-INDEX-RECORD
144800                              PPHOLDAR-HOLD-AREA
144900         GOBACK.
145000*****************************************************************
145100**            FY 2019 "            "                        *****
145200**          THIS NEXT CALL WILL PROCESS 2019 BILLS  WITH
145300**              A DISCHARGE DATE ON OR AFTER 20181001
145400*****************************************************************
145500     IF B-21-DISCHARGE-DATE
145600              > 20180930
145700         CALL  PPCAL192 USING BILL-NEW-DATA
145800                              PPS-DATA
145900                              PRICER-OPT-VERS-SW
146000                              PPS-ADDITIONAL-VARIABLES
146100                              PROV-NEW-HOLD
146200                              WAGE-NEW-CBSA-INDEX-RECORD
146300                              PPHOLDAR-HOLD-AREA
146400         GOBACK.
146500*****************************************************************
146600**            FY 2018 "            "                        *****
146700**          THIS NEXT CALL WILL PROCESS 2018 BILLS  WITH
146800**              A DISCHARGE DATE ON OR AFTER 20171001
146900*****************************************************************
147000     IF B-21-DISCHARGE-DATE
147100              > 20170930
147200         CALL  PPCAL182 USING BILL-NEW-DATA
147300                              PPS-DATA
147400                              PRICER-OPT-VERS-SW
147500                              PPS-ADDITIONAL-VARIABLES
147600                              PROV-NEW-HOLD
147700                              WAGE-NEW-CBSA-INDEX-RECORD
147800                              PPHOLDAR-HOLD-AREA
147900         GOBACK.
148000*****************************************************************
148100**            FY 2017 "            "                        *****
148200**          THIS NEXT CALL WILL PROCESS 2017 BILLS  WITH
148300**              A DISCHARGE DATE ON OR AFTER 20161001
148400*****************************************************************
148500     IF B-21-DISCHARGE-DATE
148600              > 20160930
148700         CALL  PPCAL171 USING BILL-NEW-DATA
148800                              PPS-DATA
148900                              PRICER-OPT-VERS-SW
149000                              PPS-ADDITIONAL-VARIABLES
149100                              PROV-NEW-HOLD
149200                              WAGE-NEW-CBSA-INDEX-RECORD
149300                              PPHOLDAR-HOLD-AREA
149400         GOBACK.
149500*****************************************************************
149600**            FY 2016 "ICD10 REFORM"                        *****
149700**          THIS NEXT CALL WILL PROCESS 2016 BILLS  WITH
149800**              A DISCHARGE DATE ON OR AFTER 20151001
149900*****************************************************************
150000     IF B-21-DISCHARGE-DATE
150100              > 20150930
150200         CALL  PPCAL163 USING BILL-NEW-DATA
150300                              PPS-DATA
150400                              PRICER-OPT-VERS-SW
150500                              PPS-ADDITIONAL-VARIABLES
150600                              PROV-NEW-HOLD
150700                              WAGE-NEW-CBSA-INDEX-RECORD
150800                              PPHOLDAR-HOLD-AREA
150900           MOVE 0 TO PPS-ISLET-ISOL-PAY-ADD-ON
151000         GOBACK.
151100*****************************************************************
151200**            FY 2015 "HEALTH REFORM"                       *****
151300**          THIS NEXT CALL WILL PROCESS 2015 BILLS  WITH
151400**              A DISCHARGE DATE ON OR AFTER 20141001
151500*****************************************************************
151600     IF B-21-DISCHARGE-DATE
151700              > 20140930
151800         CALL  PPCAL156 USING BILL-NEW-DATA
151900                              PPS-DATA
152000                              PRICER-OPT-VERS-SW
152100                              PPS-ADDITIONAL-VARIABLES
152200                              PROV-NEW-HOLD
152300                              WAGE-NEW-CBSA-INDEX-RECORD
152400                              PPHOLDAR-HOLD-AREA
152500           MOVE 0 TO PPS-ISLET-ISOL-PAY-ADD-ON
152600         GOBACK.
152700*****************************************************************
152800*****************************************************************
152900**            FY 2014 "HEALTH REFORM"                       *****
153000**          THIS NEXT CALL WILL PROCESS 2014 BILLS  WITH
153100**              A DISCHARGE DATE ON OR AFTER 20131001
153200*****************************************************************
153300     IF B-21-DISCHARGE-DATE
153400              > 20130930
153500         CALL  PPCAL14B USING BILL-NEW-DATA
153600                              PPS-DATA
153700                              PRICER-OPT-VERS-SW
153800                              PPS-ADDITIONAL-VARIABLES-1314
153900                              PROV-NEW-HOLD
154000                              WAGE-NEW-CBSA-INDEX-RECORD
154100
154200         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
154300         GOBACK.
154400*****************************************************************
154500*****************************************************************
154600*****************************************************************
154700**            FY 2013 "HEALTH REFORM"                       *****
154800**          THIS NEXT CALL WILL PROCESS 2011 BILLS  WITH
154900**              A DISCHARGE DATE ON OR AFTER 20121001
155000*****************************************************************
155100     IF B-21-DISCHARGE-DATE
155200              > 20120930
155300         CALL  PPCAL135 USING BILL-NEW-DATA
155400                              PPS-DATA
155500                              PRICER-OPT-VERS-SW
155600                              PPS-ADDITIONAL-VARIABLES-1314
155700                              PROV-NEW-HOLD
155800                              WAGE-NEW-CBSA-INDEX-RECORD
155900
156000         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
156100         GOBACK.
156200*****************************************************************
156300*****************************************************************
156400*****************************************************************
156500**            FY 2012 "HEALTH REFORM"                       *****
156600**          THIS NEXT CALL WILL PROCESS 2011 BILLS  WITH
156700**              A DISCHARGE DATE ON OR AFTER 20111001
156800*****************************************************************
156900     IF B-21-DISCHARGE-DATE
157000              > 20110930
157100         CALL  PPCAL125 USING BILL-NEW-DATA
157200                              PPS-DATA
157300                              PRICER-OPT-VERS-SW
157400                              PPS-ADDITIONAL-VARIABLES-PRE13
157500                              PROV-NEW-HOLD
157600                              WAGE-NEW-CBSA-INDEX-RECORD
157700
157800         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
157900         GOBACK.
158000*****************************************************************
158100*****************************************************************
158200**            FY 2011 "HEALTH REFORM"                       *****
158300**          THIS NEXT CALL WILL PROCESS 2011 BILLS  WITH
158400**              A DISCHARGE DATE ON OR AFTER 20101001
158500*****************************************************************
158600     IF B-21-DISCHARGE-DATE
158700              > 20100930
158800         CALL  PPCAL119 USING BILL-NEW-DATA
158900                              PPS-DATA
159000                              PRICER-OPT-VERS-SW
159100                              PPS-ADDITIONAL-VARIABLES-PRE13
159200                              PROV-NEW-HOLD
159300                              WAGE-NEW-CBSA-INDEX-RECORD
159400
159500         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
159600         GOBACK.
159700*****************************************************************
159800*****************************************************************
159900**            FY 2010 "HEALTH REFORM"                       *****
160000**          THIS NEXT CALL WILL PROCESS 2010 BILLS  WITH
160100**              A DISCHARGE DATE ON OR AFTER 20100401
160200*****************************************************************
160300     IF B-21-DISCHARGE-DATE
160400              > 20100331
160500         CALL  PPCAL10P USING BILL-NEW-DATA
160600                              PPS-DATA
160700                              PRICER-OPT-VERS-SW
160800                              PPS-ADDITIONAL-VARIABLES-PRE13
160900                              PROV-NEW-HOLD
161000                              WAGE-NEW-CBSA-INDEX-RECORD
161100
161200         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
161300         GOBACK.
161400*****************************************************************
161500*****************************************************************
161600**          THIS NEXT CALL WILL PROCESS 2010 BILLS  WITH
161700**        A DISCHARGE DATE ON OR AFTER 20091001 TO 4/01/2010
161800*****************************************************************
161900     IF B-21-DISCHARGE-DATE
162000              > 20090930
162100         CALL  PPCAL10O USING BILL-NEW-DATA
162200                              PPS-DATA
162300                              PRICER-OPT-VERS-SW
162400                              PPS-ADDITIONAL-VARIABLES-PRE13
162500                              PROV-NEW-HOLD
162600                              WAGE-NEW-CBSA-INDEX-RECORD
162700
162800         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
162900         GOBACK.
163000*****************************************************************
163100*****************************************************************
163200**          THIS NEXT CALL WILL PROCESS 2009 BILLS  WITH
163300**              A DISCHARGE DATE ON OR AFTER 20081001
163400*****************************************************************
163500     IF B-21-DISCHARGE-DATE
163600              > 20080930
163700         CALL  PPCAL09D USING BILL-NEW-DATA
163800                              PPS-DATA
163900                              PRICER-OPT-VERS-SW
164000                              PPS-ADDITIONAL-VARIABLES-PRE13
164100                              PROV-NEW-HOLD
164200                              WAGE-NEW-CBSA-INDEX-RECORD
164300
164400         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
164500         GOBACK.
164600*****************************************************************
164700*****************************************************************
164800**          THIS NEXT CALL WILL PROCESS 2008 BILLS  WITH
164900**              A DISCHARGE DATE ON OR AFTER 20071001
165000*****************************************************************
165100     IF B-21-DISCHARGE-DATE
165200              > 20070930
165300         CALL  PPCAL08D USING BILL-NEW-DATA
165400                              PPS-DATA
165500                              PRICER-OPT-VERS-SW
165600                              PPS-ADDITIONAL-VARIABLES-PRE13
165700                              PROV-NEW-HOLD
165800                              WAGE-NEW-CBSA-INDEX-RECORD
165900
166000         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
166100         GOBACK.
166200*****************************************************************
166300*****************************************************************
166400**          THIS NEXT CALL WILL PROCESS 2007 BILLS  WITH
166500**              A DISCHARGE DATE ON OR AFTER 20061001
166600*****************************************************************
166700     IF B-21-DISCHARGE-DATE
166800              > 20060930
166900         CALL  PPCAL07B USING BILL-NEW-DATA
167000                              PPS-DATA
167100                              PRICER-OPT-VERS-SW
167200                              PPS-ADDITIONAL-VARIABLES-PRE13
167300                              PROV-NEW-HOLD
167400                              WAGE-NEW-CBSA-INDEX-RECORD
167500
167600         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
167700         GOBACK.
167800*****************************************************************
167900*****************************************************************
168000**          THIS NEXT CALL WILL PROCESS 2006 BILLS  WITH
168100**              A DISCHARGE DATE ON OR AFTER 20051001
168200*****************************************************************
168300     IF B-21-DISCHARGE-DATE
168400              > 20050930
168500         CALL  PPCAL069 USING BILL-NEW-DATA
168600                              PPS-DATA
168700                              PRICER-OPT-VERS-SW
168800                              PPS-ADDITIONAL-VARIABLES-PRE13
168900                              PROV-NEW-HOLD
169000                              WAGE-NEW-CBSA-INDEX-RECORD
169100
169200         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
169300         GOBACK.
169400*****************************************************************
169500*****************************************************************
169600*****************************************************************
169700*****************************************************************
169800**          THIS NEXT CALL WILL PROCESS 2005 BILLS  WITH
169900**              A DISCHARGE DATE ON OR AFTER 20041001
170000*****************************************************************
170100     IF B-21-DISCHARGE-DATE
170200              > 20040930
170300         CALL  PPCAL059 USING BILL-NEW-DATA
170400                              PPS-DATA
170500                              PRICER-OPT-VERS-SW
170600                              PPS-ADDITIONAL-VARIABLES-PRE13
170700                              PROV-NEW-HOLD
170800                              WAGE-NEW-CBSA-INDEX-RECORD
170900
171000         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
171100         GOBACK.
171200*****************************************************************
171300*****************************************************************
171400*****************************************************************
171500**          THIS NEXT CALL WILL PROCESS 2004 BILLS  WITH
171600**              A DISCHARGE DATE ON OR AFTER 20031001
171700*****************************************************************
171800     IF B-21-DISCHARGE-DATE
171900              > 20030930
172000         CALL  PPCAL04D USING BILL-NEW-DATA
172100                              PPS-DATA
172200                              PRICER-OPT-VERS-SW
172300                              PPS-ADDITIONAL-VARIABLES-PRE13
172400                              PROV-NEW-HOLD
172500                              WAGE-NEW-INDEX-RECORD
172600
172700         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
172800         GOBACK.
172900*****************************************************************
173000*****************************************************************
173100*****************************************************************
173200**          THIS NEXT CALL WILL PROCESS 2003 BILLS  WITH
173300**              A DISCHARGE DATE ON OR AFTER 20021001
173400*****************************************************************
173500     IF B-21-DISCHARGE-DATE
173600              > 20020930
173700         CALL  PPCAL038 USING BILL-NEW-DATA
173800                              PPS-DATA
173900                              PRICER-OPT-VERS-SW
174000                              PPS-ADDITIONAL-VARIABLES-PRE13
174100                              PROV-NEW-HOLD
174200                              WAGE-NEW-INDEX-RECORD
174300
174400         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
174500         GOBACK.
174600*****************************************************************
174700*****************************************************************
174800*****************************************************************
174900**          THIS NEXT CALL WILL PROCESS 2002 BILLS  WITH
175000**              A DISCHARGE DATE ON OR AFTER 20011001
175100*****************************************************************
175200     IF B-21-DISCHARGE-DATE
175300              > 20010930
175400         CALL  PPCAL026 USING BILL-NEW-DATA
175500                              PPS-DATA
175600                              PRICER-OPT-VERS-SW
175700                              PPS-ADDITIONAL-VARIABLES-PRE13
175800                              PROV-NEW-HOLD
175900                              WAGE-NEW-INDEX-RECORD
176000
176100         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
176200         GOBACK.
176300*****************************************************************
176400*****************************************************************
176500*****************************************************************
176600**          THIS NEXT CALL WILL PROCESS 2001 BILLS  WITH
176700**              A DISCHARGE DATE ON OR AFTER 20001001
176800*****************************************************************
176900     IF B-21-DISCHARGE-DATE
177000              > 20000930
177100         CALL  PPCAL017 USING BILL-NEW-DATA
177200                              PPS-DATA
177300                              PRICER-OPT-VERS-SW
177400                              PPS-ADDITIONAL-VARIABLES-PRE13
177500                              PROV-NEW-HOLD
177600                              WAGE-NEW-INDEX-RECORD
177700
177800         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
177900         GOBACK.
178000*****************************************************************
178100*****************************************************************
178200*****************************************************************
178300**          THIS NEXT CALL WILL PROCESS 2000 BILLS  WITH
178400**              A DISCHARGE DATE ON OR AFTER 19991001
178500*****************************************************************
178600     IF B-21-DISCHARGE-DATE
178700              > 19990930
178800         CALL  PPCAL006 USING BILL-NEW-DATA
178900                              PPS-DATA
179000                              PRICER-OPT-VERS-SW
179100                              PPS-ADDITIONAL-VARIABLES-PRE13
179200                              PROV-NEW-HOLD
179300                              WAGE-NEW-INDEX-RECORD
179400
179500         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
179600         GOBACK.
179700*****************************************************************
179800*****************************************************************
179900**          THIS NEXT CALL WILL PROCESS 1999 BILLS  WITH
180000**              A DISCHARGE DATE ON OR AFTER 19981001
180100*****************************************************************
180200     IF B-21-DISCHARGE-DATE
180300              > 19980930
180400         CALL  PPCAL998 USING BILL-NEW-DATA
180500                              PPS-DATA
180600                              PRICER-OPT-VERS-SW
180700                              PPS-ADDITIONAL-VARIABLES-PRE13
180800                              PROV-NEW-HOLD
180900                              WAGE-NEW-INDEX-RECORD
181000
181100         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
181200         GOBACK.
181300*****************************************************************
181400*****************************************************************
181500**   MOVE NEW DATA FORMAT INTO OLD DATA FORMAT
181600**   TO PROCESS BEFORE 19981001 NON-MILLENNNIUM STANDARD
181700*****************************************************************
181800     MOVE ALL '9' TO W-PROV-NEW-HOLD.
181900     MOVE PROV-NEW-HOLD TO W-PROV-NEW-HOLD.
182000     PERFORM 2400-CONVERT-PSF.
182100     MOVE W-PROV-OLD-HOLD TO PROV-HOLD.
182200
182300     MOVE W-NEW-MSA             TO W-MSA.
182400     MOVE W-NEW-SIZE            TO W-SIZE.
182500     MOVE W-NEW-EFF-DATE-YMD    TO W-EFF-DATE.
182600     MOVE W-NEW-INDEX-RECORD    TO W-INDEX-RECORD.
182700     MOVE W-NEW-PR-INDEX-RECORD TO W-PR-INDEX-RECORD.
182800
182900     MOVE B-N-PROVIDER-NO  TO B-PROVIDER-NO.
183000     MOVE B-N-REVIEW-CODE  TO B-REVIEW-CODE.
183100     MOVE B-N-DRG          TO B-DRG.
183200     MOVE B-N-LOS          TO B-LOS.
183300     MOVE B-N-COVERED-DAYS TO B-COVERED-DAYS.
183400     MOVE B-N-LTR-DAYS     TO B-LTR-DAYS.
183500     MOVE B-N-DISCHG-YY    TO B-DISCHG-YY.
183600     MOVE B-N-DISCHG-MM    TO B-DISCHG-MM.
183700     MOVE B-N-DISCHG-DD    TO B-DISCHG-DD.
183800     MOVE B-N-CHARGES-CLAIMED TO B-CHARGES-CLAIMED.
183900
184000*****************************************************************
184100**          THIS NEXT CALL WILL PROCESS 1998 BILLS  WITH
184200**              A DISCHARGE DATE ON OR AFTER 19971001
184300*****************************************************************
184400     IF B-21-DISCHARGE-DATE
184500              > 19970930
184600         CALL  PPCAL987 USING BILL-DATA
184700                              PPS-DATA
184800                              PRICER-OPT-VERS-SW
184900                              PPS-ADDITIONAL-VARIABLES-PRE13
185000                              PROV-HOLD
185100                              WAGE-INDEX-RECORD
185200
185300         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
185400         GOBACK.
185500*****************************************************************
185600*****************************************************************
185700**          THIS NEXT CALL WILL PROCESS 1997 BILLS  WITH
185800**              A DISCHARGE DATE ON OR AFTER 19961001
185900*****************************************************************
186000     IF B-21-DISCHARGE-DATE
186100              > 19960930
186200         CALL  PPCAL974 USING BILL-DATA
186300                              PPS-DATA
186400                              PRICER-OPT-VERS-SW
186500                              PPS-ADDITIONAL-VARIABLES-PRE13
186600                              PROV-HOLD
186700                              WAGE-INDEX-RECORD
186800
186900         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
187000         GOBACK.
187100*****************************************************************
187200*****************************************************************
187300**          THIS NEXT CALL WILL PROCESS 1996 BILLS  WITH
187400**              A DISCHARGE DATE ON OR AFTER 19951001
187500*****************************************************************
187600     IF B-21-DISCHARGE-DATE
187700              > 19950930
187800         CALL  PPCAL964 USING BILL-DATA
187900                              PPS-DATA
188000                              PRICER-OPT-VERS-SW
188100                              PPS-ADDITIONAL-VARIABLES-PRE13
188200                              PROV-HOLD
188300                              WAGE-INDEX-RECORD
188400
188500         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
188600         GOBACK.
188700*****************************************************************
188800*****************************************************************
188900**          THIS NEXT CALL WILL PROCESS 1995 BILLS  WITH
189000**              A DISCHARGE DATE ON OR AFTER 19941001
189100*****************************************************************
189200     IF B-21-DISCHARGE-DATE
189300              > 19940930
189400         CALL  PPCAL954 USING BILL-DATA
189500                              PPS-DATA
189600                              PRICER-OPT-VERS-SW
189700                              PPS-ADDITIONAL-VARIABLES-PRE13
189800                              PROV-HOLD
189900                              WAGE-INDEX-RECORD
190000
190100         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
190200         GOBACK.
190300*****************************************************************
190400*****************************************************************
190500**          THIS NEXT CALL WILL PROCESS 1994 BILLS  WITH
190600**              A DISCHARGE DATE ON OR AFTER 19931001
190700*****************************************************************
190800     IF B-21-DISCHARGE-DATE
190900              > 19930930
191000         CALL  PPCAL944 USING BILL-DATA
191100                              PPS-DATA
191200                              PRICER-OPT-VERS-SW
191300                              PPS-ADDITIONAL-VARIABLES-PRE13
191400                              PROV-HOLD
191500                              WAGE-INDEX-RECORD
191600
191700         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
191800         GOBACK.
191900*****************************************************************
192000*****************************************************************
192100**          THIS NEXT CALL WILL PROCESS 1993 BILLS  WITH
192200**              A DISCHARGE DATE ON OR AFTER 19921001
192300*****************************************************************
192400     IF B-21-DISCHARGE-DATE
192500              > 19920930
192600         CALL  PPCAL935 USING BILL-DATA
192700                              PPS-DATA
192800                              PRICER-OPT-VERS-SW
192900                              PPS-ADDITIONAL-VARIABLES-PRE13
193000                              PROV-HOLD
193100                              WAGE-INDEX-RECORD
193200
193300         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
193400         GOBACK.
193500*****************************************************************
193600*****************************************************************
193700**          THIS NEXT CALL WILL PROCESS 1992 BILLS  WITH
193800**              A DISCHARGE DATE ON OR AFTER 19911001
193900*****************************************************************
194000     IF B-21-DISCHARGE-DATE
194100              > 19910930
194200         CALL  PPCAL926 USING BILL-DATA
194300                              PPS-DATA
194400                              PRICER-OPT-VERS-SW
194500                              PPS-ADDITIONAL-VARIABLES-PRE13
194600                              PROV-HOLD
194700                              WAGE-INDEX-RECORD
194800
194900         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
195000         GOBACK.
195100*****************************************************************
195200*****************************************************************
195300**          THIS NEXT CALL WILL PROCESS 1991 BILLS  WITH
195400**              A DISCHARGE DATE ON OR AFTER 19901001
195500*****************************************************************
195600     IF B-21-DISCHARGE-DATE
195700              > 19900930
195800         CALL  PPCAL915 USING BILL-DATA
195900                              PPS-DATA
196000                              PRICER-OPT-VERS-SW
196100                              PPS-ADDITIONAL-VARIABLES-PRE13
196200                              PROV-HOLD
196300                              WAGE-INDEX-RECORD
196400
196500         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
196600         GOBACK.
196700*****************************************************************
196800*****************************************************************
196900**          THIS NEXT CALL WILL PROCESS 1990 BILLS  WITH
197000**              A DISCHARGE DATE ON OR AFTER 19891001
197100*****************************************************************
197200     IF B-21-DISCHARGE-DATE
197300              > 19890930
197400         CALL  PPCAL905 USING BILL-DATA
197500                              PPS-DATA
197600                              PRICER-OPT-VERS-SW
197700                              PPS-ADDITIONAL-VARIABLES-PRE13
197800                              PROV-HOLD
197900                              WAGE-INDEX-RECORD
198000
198100         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
198200         GOBACK.
198300*****************************************************************
198400*****************************************************************
198500**          THIS NEXT CALL WILL PROCESS 1989 BILLS  WITH
198600**              A DISCHARGE DATE ON OR AFTER 19881001
198700*****************************************************************
198800     IF B-21-DISCHARGE-DATE
198900              > 19880930
199000         CALL  PPCAL894 USING BILL-DATA
199100                              PPS-DATA
199200                              PRICER-OPT-VERS-SW
199300                              PPS-ADDITIONAL-VARIABLES-PRE13
199400                              PROV-HOLD
199500                              WAGE-INDEX-RECORD
199600
199700         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
199800         GOBACK.
199900*****************************************************************
200000*****************************************************************
200100**          THIS NEXT CALL WILL PROCESS 1988 BILLS  WITH
200200**              A DISCHARGE DATE ON OR AFTER 19871001
200300*****************************************************************
200400     IF B-21-DISCHARGE-DATE
200500              > 19870930
200600         CALL  PPCAL884 USING BILL-DATA
200700                              PPS-DATA
200800                              PRICER-OPT-VERS-SW
200900                              PPS-ADDITIONAL-VARIABLES-PRE13
201000                              PROV-HOLD
201100                              WAGE-INDEX-RECORD
201200
201300         PERFORM 2900-MOVE-PPS-ADDITIONAL-VARS THRU 2900-EXIT
201400         GOBACK.
201500*****************************************************************
201600*****************************************************************
201700     MOVE 98 TO PPS-RTC.
201800     GOBACK.
201900
202000 0100-GET-MSA.
202100     SET MU1 TO 1.
202200
202300     SEARCH M-MSAX-DATA VARYING MU1
202400     AT END
202500          MOVE 999999 TO P-PROVIDER-NO
202600          MOVE 52     TO PPS-RTC
202700          GO TO 0100-EXIT
202800     WHEN M-MSAX-MSA (MU1) = HOLD-PROV-MSAX
202900          SET MU2 TO MU1.
203000
203100 0100-EXIT.  EXIT.
203200
203300 0150-GET-CBSA.
203400     SET MA1 TO 1.
203500
203600     SEARCH T-CBSA-DATA VARYING MA1
203700     AT END
203800          MOVE 999999 TO P-PROVIDER-NO
203900          MOVE 52     TO PPS-RTC
204000          GO TO 0150-EXIT
204100     WHEN T-CBSA  (MA1) = HOLD-PROV-CBSA
204200          SET MA2 TO MA1.
204300
204400 0150-EXIT.  EXIT.
204500
204600 0175-GET-RURAL-CBSA.
204700
204800     IF B-21-DISCHARGE-DATE > 20190930 AND
204900        B-21-DISCHARGE-DATE < 20201001
205000       IF H-CBSA-PROV-BLANK = '   '
205100         GO TO 0175-EXIT
205200       ELSE
205300         PERFORM 0190-GET-RURAL-FLOOR-2020 THRU 0190-EXIT
205400         GO TO 0175-EXIT
205500       END-IF
205600     END-IF.
205700
205800     SET MA1 TO 1.
205900
206000     SEARCH T-CBSA-DATA VARYING MA1
206100     AT END
206200       MOVE '   00'              TO W-RURAL-CBSA-X
206300       MOVE 99999999             TO W-RURAL-CBSA-EFF-DATE
206400       MOVE 0                    TO W-RURAL-CBSA-WI
206500       GO TO 0175-EXIT
206600     WHEN T-CBSA  (MA1) = HOLD-RURAL-CBSA
206700          SET MA2 TO MA1.
206800
206900 0175-EXIT.  EXIT.
207000
207100 0190-GET-RURAL-FLOOR-2020.
207200
207300     SET RUFL-IDX TO 1.
207400
207500     SEARCH RUFL-TAB VARYING RUFL-IDX
207600     AT END
207700       MOVE '   00'              TO W-RURAL-CBSA-X
207800       MOVE 99999999             TO W-RURAL-CBSA-EFF-DATE
207900       MOVE 0                    TO W-RURAL-CBSA-WI
208000       GO TO 0190-EXIT
208100     WHEN RUFL-CBSA(RUFL-IDX) = HOLD-RURAL-CBSA
208200          SET RUFL-IDX2 TO RUFL-IDX.
208300
208400 0190-EXIT.  EXIT.
208500
208600 0200-N-GET-MSAPR.
208700
208800     IF P-NEW-CHG-CODE-INDEX = 'Y'
208900        MOVE P-NEW-WAGE-INDEX-LOC-MSA TO HOLD-PROV-MSAX
209000     ELSE
209100        MOVE P-NEW-GEO-LOC-MSAX TO HOLD-PROV-MSAX.
209200
209300     MOVE '*' TO H-MSAX-LAST-POS.
209400     PERFORM 0100-GET-MSA THRU 0100-EXIT.
209500
209600     IF PPS-RTC = 00
209700      PERFORM 0300-N-GET-PR-WAGE-INDX
209800           THRU 0300-N-EXIT VARYING MU2
209900           FROM MU1 BY 1 UNTIL
210000           M-MSAX-MSA (MU2) NOT = HOLD-PROV-MSAX.
210100
210200 0200-N-EXIT.  EXIT.
210300
210400 0250-N-GET-CBSA-PR.
210500
210600     MOVE ZERO TO MESWK-PRSPC-WAGEIN-BLEND.
210700
210800     IF B-21-DISCHARGE-DATE > 20140930 AND
210900        B-21-DISCHARGE-DATE < 20151001
211000        PERFORM 0370-PRSPC-CODE-RTN THRU 0370-EXIT
211100
211200        IF  MESWK-PRSPC-WAGEIN-BLEND > 00.0000
211300            MOVE MESWK-PRSPC-WAGEIN-BLEND
211400                              TO W-NEW-CBSA-PR-WI
211500        GO TO 0250-EXIT.
211600
211700     MOVE P-NEW-CBSA-STAND-AMT-LOC TO HOLD-PROV-CBSA.
211800
211900     IF P-NEW-CBSA-WI-RECLASS OR P-NEW-CBSA-WI-DUAL
212000        MOVE P-NEW-CBSA-RECLASS-LOC TO HOLD-PROV-CBSA.
212100
212200     MOVE '*' TO H-CBSA-LAST-POS.
212300     PERFORM 0150-GET-CBSA THRU 0150-EXIT.
212400
212500     IF PPS-RTC = 00
212600      PERFORM 0350-N-GET-PR-WAGE-INDX
212700           THRU 0350-EXIT VARYING MA2
212800           FROM MA1 BY 1 UNTIL
212900           T-CBSA (MA2) NOT = HOLD-PROV-CBSA.
213000
213100 0250-EXIT.  EXIT.
213200
213300 0260-N-GET-RURAL-CBSA-PR.
213400
213500     MOVE '*' TO H-CBSA-RURAL-LAST-POS.
213600     PERFORM 0175-GET-RURAL-CBSA THRU 0175-EXIT.
213700
213800     IF PPS-RTC = 00
213900      PERFORM 0360-N-GET-PR-RURAL-WAGE-INDX
214000           THRU 0360-EXIT VARYING MA2
214100           FROM MA1 BY 1 UNTIL
214200           T-CBSA (MA2) NOT = HOLD-RURAL-CBSA.
214300
214400 0260-EXIT.  EXIT.
214500
214600 0300-N-GET-PR-WAGE-INDX.
214700
214800     IF  B-21-DISCHARGE-DATE NOT < M-MSAX-EFF-DATE (MU2)
214900         MOVE M-MSAX-WAGE-INDX1 (MU2) TO W-NEW-PR-INDEX-RECORD
215000         IF P-NEW-CHG-CODE-INDEX  = 'Y'
215100            MOVE M-MSAX-WAGE-INDX2 (MU2)
215200                                     TO W-NEW-PR-INDEX-RECORD.
215300
215400 0300-N-EXIT.  EXIT.
215500
215600 0350-N-GET-PR-WAGE-INDX.
215700
215800     IF  B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2) AND
215900         (T-CBSA-EFF-DATE (MA2) >= W-FY-BEGIN-DATE AND
216000          T-CBSA-EFF-DATE (MA2) <= W-FY-END-DATE)
216100         MOVE T-CBSA-WAGE-INDX1 (MA2)
216200                              TO W-NEW-CBSA-PR-WI
216300         IF P-NEW-CBSA-SPEC-PAY-IND =  'Y'
216400             MOVE T-CBSA-WAGE-INDX2 (MA2)
216500                              TO W-NEW-CBSA-PR-WI.
216600
216700 0350-EXIT.  EXIT.
216800
216900 0360-N-GET-PR-RURAL-WAGE-INDX.
217000
217100     IF  B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2) AND
217200         (T-CBSA-EFF-DATE (MA2) >= W-FY-BEGIN-DATE AND
217300          T-CBSA-EFF-DATE (MA2) <= W-FY-END-DATE)
217400         MOVE T-CBSA-WAGE-INDX1 (MA2)
217500                              TO W-RURAL-CBSA-PR-WI
217600         IF P-NEW-CBSA-SPEC-PAY-IND = 'Y'
217700             MOVE T-CBSA-WAGE-INDX2 (MA2)
217800                              TO W-RURAL-CBSA-PR-WI.
217900
218000 0360-EXIT.  EXIT.
218100
218200*
218300***************************************************************
218400* USE IPPS COMPARABLE BLENDED WAGE INDEX FROM TABLE IF        *
218500* PROVIDER FOUND IN TABLE - FOR FY 2015                       *
218600***************************************************************
218700***********************************************************
218800 0370-PRSPC-CODE-RTN.
218900*
219000     SET PRSPC-IDX TO 1.
219100     SEARCH PRSPC-TAB VARYING PRSPC-IDX
219200         AT END
219300           MOVE ZERO TO MESWK-PRSPC-WAGEIN-BLEND
219400       WHEN WK-PRSPC-PROV (PRSPC-IDX) = P-NEW-PROVIDER-NO
219500         MOVE WK-PRSPC-WAGEIN-BLEND(PRSPC-IDX)
219600                            TO MESWK-PRSPC-WAGEIN-BLEND.
219700
219800
219900 0370-EXIT.   EXIT.
220000
220100***********************************************************
220200 0500-GET-MSA.
220300        IF P-NEW-CHG-CODE-INDEX = 'Y'
220400           MOVE P-NEW-WAGE-INDEX-LOC-MSA TO HOLD-PROV-MSAX
220500        ELSE
220600           MOVE P-NEW-GEO-LOC-MSA9 TO HOLD-PROV-MSAX.
220700
220800**1998***********************************************************
220900     IF B-21-DISCHARGE-DATE > 19970930 AND
221000        B-21-DISCHARGE-DATE < 19981001
221100        PERFORM 2300-1998-FLOOR-MSA THRU 2300-1998-EXIT.
221200
221300**1999***********************************************************
221400     IF B-21-DISCHARGE-DATE > 19980930 AND
221500        B-21-DISCHARGE-DATE < 19991001
221600        PERFORM 2300-1999-FLOOR-MSA THRU 2300-1999-EXIT.
221700
221800**2000***********************************************************
221900     IF B-21-DISCHARGE-DATE > 19990930 AND
222000        B-21-DISCHARGE-DATE < 20001001
222100        PERFORM 2300-2000-FLOOR-MSA THRU 2300-2000-EXIT.
222200
222300**2001***********************************************************
222400     IF B-21-DISCHARGE-DATE > 20000930 AND
222500        B-21-DISCHARGE-DATE < 20011001
222600        PERFORM 2300-2001-FLOOR-MSA THRU 2300-2001-EXIT.
222700
222800**2002***********************************************************
222900     IF B-21-DISCHARGE-DATE > 20010930 AND
223000        B-21-DISCHARGE-DATE < 20021001
223100        PERFORM 2300-2002-FLOOR-MSA THRU 2300-2002-EXIT.
223200
223300     IF B-21-DISCHARGE-DATE > 20010930 AND
223400        B-21-DISCHARGE-DATE < 20021001
223500          PERFORM 2700-2002-WI-401-HOSPITAL THRU 2700-2002-EXIT.
223600
223700**2003***********************************************************
223800     IF B-21-DISCHARGE-DATE > 20020930 AND
223900        B-21-DISCHARGE-DATE < 20031001
224000          PERFORM 2700-2003-WI-401-HOSPITAL THRU 2700-2003-EXIT.
224100
224200     IF B-21-DISCHARGE-DATE > 20020930 AND
224300        B-21-DISCHARGE-DATE < 20031001
224400        PERFORM 2300-2003-FLOOR-MSA THRU 2300-2003-EXIT.
224500
224600**2004***********************************************************
224700     IF B-21-DISCHARGE-DATE > 20030930 AND
224800        B-21-DISCHARGE-DATE < 20041001
224900          PERFORM 2700-2004-WI-401-HOSPITAL THRU 2700-2004-EXIT.
225000
225100     IF B-21-DISCHARGE-DATE > 20030930 AND
225200        B-21-DISCHARGE-DATE < 20041001
225300        PERFORM 2300-2004-FLOOR-MSA THRU 2300-2004-EXIT.
225400
225500*****************************************************************
225600     PERFORM 0100-GET-MSA THRU 0100-EXIT.
225700
225800***     RTC = 52  --  MSA NOT FOUND
225900     IF PPS-RTC = 52    GOBACK.
226000
226100     IF PPS-RTC = 00
226200        PERFORM 0600-N-GET-WAGE-INDX
226300           THRU 0600-N-EXIT VARYING MU2
226400           FROM MU1 BY 1 UNTIL
226500           M-MSAX-MSA (MU2) NOT = HOLD-PROV-MSAX.
226600
226700     IF P-N-INDIAN-HEALTH-SERVICE
226800             MOVE 00 TO PPS-RTC
226900             PERFORM 0800-N-GET-INDIAN-WI THRU 0800-N-EXIT.
227000
227100***     RTC = 52  --  WAGE-INDEX NOT FOUND
227200     IF PPS-RTC = 52    GOBACK.
227300
227400**2000***********************************************************
227500     IF B-21-DISCHARGE-DATE > 19990930 AND
227600        B-21-DISCHARGE-DATE < 20001001
227700          PERFORM 2500-2000-WI-LUGAR THRU 2500-2000-EXIT.
227800
227900**2001***********************************************************
228000     IF B-21-DISCHARGE-DATE > 20000930 AND
228100        B-21-DISCHARGE-DATE < 20011001
228200          PERFORM 2500-2001-WI-LUGAR THRU 2500-2001-EXIT.
228300
228400**2003***********************************************************
228500     IF B-21-DISCHARGE-DATE > 20020930 AND
228600        B-21-DISCHARGE-DATE < 20031001
228700          PERFORM 2500-2003-WI-LUGAR THRU 2500-2003-EXIT.
228800
228900**2004***********************************************************
229000     IF B-21-DISCHARGE-DATE > 20031231 AND
229100        B-21-DISCHARGE-DATE < 20041001
229200        PERFORM 2300-2004-RECLASS152 THRU 2300-2004-RECLASS-EXIT.
229300
229400*****************************************************************
229500     IF W-NEW-INDEX-RECORD = 00.0000
229600        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
229700        MOVE 52 TO PPS-RTC.
229800
229900***  GET THE WAGE-SIZE
230000
230100     MOVE P-NEW-STAND-AMT-LOC-MSA TO HOLD-PROV-MSAX.
230200
230300     PERFORM 0100-GET-MSA THRU 0100-EXIT.
230400
230500     IF PPS-RTC = 00
230600         PERFORM 0700-N-GET-WAGE-SIZE
230700           THRU 0700-N-EXIT VARYING MU2
230800           FROM MU1 BY 1 UNTIL
230900           M-MSAX-MSA (MU2) NOT = HOLD-PROV-MSAX.
231000
231100     IF P-PR-NEW-STATE
231200        IF B-21-DISCHARGE-DATE > 19970930
231300              PERFORM 0200-N-GET-MSAPR THRU 0200-N-EXIT.
231400
231500***     RTC = 52  --  PR-WAGE-INDEX NOT FOUND
231600     IF PPS-RTC = 52
231700          MOVE ALL '0' TO  PPS-ADDITIONAL-VARIABLES
231800          GOBACK.
231900
232000 0500-EXIT.  EXIT.
232100
232200 0550-GET-CBSA.
232300**2005*  OCT 1, 2004 CBSA REPLACED MSA
232400
232500**----------------------------------------------------------------
232600** CBSA DOESN'T APPLY TO CLAIMS DISCHARGED BEFORE 10/01/2004
232700**----------------------------------------------------------------
232800     IF P-NEW-EFF-DATE < 20041001
232900        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
233000        MOVE 52 TO PPS-RTC
233100        GOBACK.
233200
233300**----------------------------------------------------------------
233400** HOLD THE PROVIDER'S CBSA FROM PSF
233500** (EQUIVALENT TO GEO LOCATION CBSA IF NO STAND AMT LOC CBSA)
233600**----------------------------------------------------------------
233700     MOVE P-NEW-CBSA-STAND-AMT-LOC TO HOLD-PROV-CBSA.
233800
233900**----------------------------------------------------------------
234000** HOLD THE PROVIDER'S RECLASS CBSA IF RECLASS STATUS INDICATED
234100**----------------------------------------------------------------
234200     IF P-NEW-CBSA-WI-RECLASS OR P-NEW-CBSA-WI-DUAL
234300        MOVE P-NEW-CBSA-RECLASS-LOC TO HOLD-PROV-CBSA.
234400
234500**----------------------------------------------------------------
234600** IF THE PSF INDICATES THE SPECIAL WAGE INDEX SHOULD BE USED,
234700** VALIDATE THE SPECIAL WAGE INDEX VALUE AND EFFECTIVE DATE. IF
234800** VALID, USE THE SPECIAL WAGE INDEX AND SKIP THE NON-PR SPECIFIC
234900** CBSA WAGE INDEX SEARCH.
235000**----------------------------------------------------------------
235100     IF (P-NEW-CBSA-WI-SPECIAL AND
235200         P-NEW-CBSA-SPEC-WI-N NOT NUMERIC)
235300        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
235400        MOVE 52 TO PPS-RTC
235500        GOBACK.
235600
235700     IF (P-NEW-CBSA-WI-SPECIAL AND
235800         P-NEW-CBSA-SPEC-WI-N = ZEROES)
235900        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
236000        MOVE 52 TO PPS-RTC
236100        GOBACK.
236200
236300     IF (P-NEW-CBSA-WI-SPECIAL AND
236400        (P-NEW-EFF-DATE < W-FY-BEGIN-DATE OR
236500         P-NEW-EFF-DATE > W-FY-END-DATE))
236600        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
236700        MOVE 52 TO PPS-RTC
236800        GOBACK.
236900
237000     IF P-NEW-CBSA-WI-SPECIAL
237100        MOVE 'SPEC*'            TO W-NEW-CBSA-X
237200        MOVE P-NEW-EFF-DATE     TO W-NEW-CBSA-EFF-DATE
237300        MOVE P-NEW-CBSA-SPEC-WI TO W-NEW-CBSA-WI
237400        GO TO 0550-BYPASS.
237500
237600**----------------------------------------------------------------
237700** FOR FYS 2005 - 2014, CHANGE THE HOLD CBSA TO APPLY THE SECTION
237800** 401 HOSPITAL AND RURAL FLOOR POLICIES FOR CERTAIN PROVIDERS
237900**----------------------------------------------------------------
238000
238100**2005***********************************************************
238200     IF B-21-DISCHARGE-DATE > 20040930 AND
238300        B-21-DISCHARGE-DATE < 20050401
238400          PERFORM 2700-2005-WI-401-HOSPITAL THRU 2700-2005-EXIT.
238500
238600     IF B-21-DISCHARGE-DATE > 20050331 AND
238700        B-21-DISCHARGE-DATE < 20051001
238800          PERFORM 2750-2005-WI-401-HOSPITAL THRU 2750-2005-EXIT.
238900
239000     IF B-21-DISCHARGE-DATE > 20040930 AND
239100        B-21-DISCHARGE-DATE < 20051001
239200        PERFORM 2300-2005-FLOOR-CBSA THRU 2300-2005-EXIT.
239300
239400**2006***********************************************************
239500
239600     IF B-21-DISCHARGE-DATE > 20050930 AND
239700        B-21-DISCHARGE-DATE < 20061001
239800          PERFORM 2800-2006-WI-401-HOSPITAL THRU 2800-2006-EXIT.
239900
240000     IF B-21-DISCHARGE-DATE > 20050930 AND
240100        B-21-DISCHARGE-DATE < 20061001
240200        PERFORM 2300-2006-FLOOR-CBSA THRU 2300-2006-EXIT.
240300
240400**2007***********************************************************
240500
240600     IF B-21-DISCHARGE-DATE > 20060930 AND
240700        B-21-DISCHARGE-DATE < 20071001
240800          PERFORM 2800-2007-WI-401-HOSPITAL THRU 2800-2007-EXIT.
240900
241000     IF B-21-DISCHARGE-DATE > 20060930 AND
241100        B-21-DISCHARGE-DATE < 20071001
241200        PERFORM 2300-2007-FLOOR-CBSA THRU 2300-2007-EXIT.
241300
241400*****************************************************************
241500**2008***********************************************************
241600
241700     IF B-21-DISCHARGE-DATE > 20070930 AND
241800        B-21-DISCHARGE-DATE < 20081001
241900          PERFORM 2800-2008-WI-401-HOSPITAL THRU 2800-2008-EXIT.
242000
242100     IF B-21-DISCHARGE-DATE > 20070930 AND
242200        B-21-DISCHARGE-DATE < 20081001
242300        PERFORM 2300-2008-FLOOR-CBSA THRU 2300-2008-EXIT.
242400
242500*****************************************************************
242600**2009***********************************************************
242700
242800     IF B-21-DISCHARGE-DATE > 20080930 AND
242900        B-21-DISCHARGE-DATE < 20091001
243000          PERFORM 2800-2009-WI-401-HOSPITAL THRU 2800-2009-EXIT.
243100
243200     IF B-21-DISCHARGE-DATE > 20080930 AND
243300        B-21-DISCHARGE-DATE < 20091001
243400        PERFORM 2300-2009-FLOOR-CBSA THRU 2300-2009-EXIT.
243500
243600*****************************************************************
243700**2010***********************************************************
243800
243900     IF B-21-DISCHARGE-DATE > 20090930 AND
244000        B-21-DISCHARGE-DATE < 20101001
244100          PERFORM 2800-2010-WI-401-HOSPITAL THRU 2800-2010-EXIT.
244200
244300     IF B-21-DISCHARGE-DATE > 20090930 AND
244400        B-21-DISCHARGE-DATE < 20101001
244500        PERFORM 2300-2010-FLOOR-CBSA THRU 2300-2010-EXIT.
244600
244700*****************************************************************
244800**2011***********************************************************
244900
245000     IF B-21-DISCHARGE-DATE > 20100930 AND
245100        B-21-DISCHARGE-DATE < 20111001
245200          PERFORM 2800-2011-WI-401-HOSPITAL THRU 2800-2011-EXIT.
245300
245400     IF B-21-DISCHARGE-DATE > 20100930 AND
245500        B-21-DISCHARGE-DATE < 20111001
245600        PERFORM 2300-2011-FLOOR-CBSA THRU 2300-2011-EXIT.
245700
245800**2012***********************************************************
245900
246000     IF B-21-DISCHARGE-DATE > 20110930 AND
246100        B-21-DISCHARGE-DATE < 20121001
246200          PERFORM 2800-2012-WI-401-HOSPITAL THRU 2800-2012-EXIT.
246300
246400     IF B-21-DISCHARGE-DATE > 20110930 AND
246500        B-21-DISCHARGE-DATE < 20121001
246600        PERFORM 2300-2012-FLOOR-CBSA THRU 2300-2012-EXIT.
246700
246800*****************************************************************
246900**2013***********************************************************
247000
247100     IF B-21-DISCHARGE-DATE > 20120930 AND
247200        B-21-DISCHARGE-DATE < 20131001
247300          PERFORM 2800-2013-WI-401-HOSPITAL THRU 2800-2013-EXIT.
247400
247500     IF B-21-DISCHARGE-DATE > 20120930 AND
247600        B-21-DISCHARGE-DATE < 20131001
247700        PERFORM 2300-2013-FLOOR-CBSA THRU 2300-2013-EXIT.
247800
247900**2014***********************************************************
248000
248100     IF B-21-DISCHARGE-DATE > 20130930 AND
248200        B-21-DISCHARGE-DATE < 20141001
248300          PERFORM 2800-2014-WI-401-HOSPITAL THRU 2800-2014-EXIT.
248400
248500     IF B-21-DISCHARGE-DATE > 20130930 AND
248600        B-21-DISCHARGE-DATE < 20141001
248700        PERFORM 2300-2014-FLOOR-CBSA THRU 2300-2014-EXIT.
248800
248900**2015***********************************************************
249000* 401 HOSPITAL PROCESS STOPPED FOR FY2015
249100**2015***********************************************************
249200*
249300*    IF B-21-DISCHARGE-DATE > 20140930
249400*         PERFORM 2800-2015-WI-401-HOSPITAL THRU 2800-2015-EXIT.
249500*
249600*****************************************************************
249700
249800**----------------------------------------------------------------
249900** SEARCH FOR THE HOLD CBSA IN THE CBSA WAGE INDEX TABLE, AND
250000** ID THE FIRST RECORD FOR THE HOLD CBSA IN THE TABLE
250100**----------------------------------------------------------------
250200     PERFORM 0150-GET-CBSA THRU 0150-EXIT.
250300
250400***  RTC = 52  --  CBSA NOT FOUND
250500     IF PPS-RTC = 52 GOBACK.
250600
250700**----------------------------------------------------------------
250800** GET CBSA WAGE INDEX - BASED ON DISCHARGE DATE
250900**----------------------------------------------------------------
251000     IF PPS-RTC = 00
251100        PERFORM 0650-N-GET-CBSA-WAGE-INDX
251200         THRU   0650-N-EXIT VARYING MA2
251300                FROM MA1 BY 1 UNTIL
251400                T-CBSA (MA2) NOT = HOLD-PROV-CBSA.
251500
251600**----------------------------------------------------------------
251700** FOR FYS 2015 AND AFTER, APPLY THE RURAL FLOOR POLICY
251800**----------------------------------------------------------------
251900     IF B-21-DISCHARGE-DATE > 20140930
252000          PERFORM 2300-2015-FWD-FLOOR-CBSA
252100             THRU 2300-2015-EXIT.
252200
252300**----------------------------------------------------------------
252400** FOR FYS 2018 AND AFTER, APPLY THE OUTMIGRATION ADJUSTMENT
252500**----------------------------------------------------------------
252600
252700     IF OUTM-IND = 1
252800        COMPUTE W-NEW-CBSA-WI = W-NEW-CBSA-WI + HLD-OUTM-ADJ.
252900
253000**----------------------------------------------------------------
253100** FOR FYS 2020 THROUGH 2023, APPLY THE WAGE INDEX BOOST
253200**----------------------------------------------------------------
253300
253400     IF B-21-DISCHARGE-DATE > 20200930
253500        IF W-NEW-CBSA-WI < WI_QUARTILE_FY2021
253600           COMPUTE W-NEW-CBSA-WI ROUNDED =
253700             ((WI_QUARTILE_FY2021 - W-NEW-CBSA-WI) / 2)
253800             + W-NEW-CBSA-WI.
253900
254000     IF B-21-DISCHARGE-DATE > 20190930 AND
254100        B-21-DISCHARGE-DATE < 20201001
254200        IF W-NEW-CBSA-WI < WI_QUARTILE_FY2020
254300           COMPUTE W-NEW-CBSA-WI ROUNDED =
254400             ((WI_QUARTILE_FY2020 - W-NEW-CBSA-WI) / 2)
254500             + W-NEW-CBSA-WI.
254600
254700**----------------------------------------------------------------
254800** APPLY RURAL FLOOR POLICY TRANSITION ADJUSTMENT
254900**----------------------------------------------------------------
255000
255100     IF B-21-DISCHARGE-DATE > 20200930
255200        IF P-NEW-IND-PRIOR-YEAR AND P-NEW-SUPP-WI > 0
255300           MOVE P-NEW-SUPP-WI TO HLD-PREV-WI
255400        ELSE
255500           MOVE 52 TO PPS-RTC
255600           GOBACK.
255700
255800     IF B-21-DISCHARGE-DATE > 20190930 AND
255900        B-21-DISCHARGE-DATE < 20201001
256000        PERFORM 1000-GET-PREVYR-WI THRU 1000-EXIT.
256100
256200     IF B-21-DISCHARGE-DATE > 20190930
256300        IF (((W-NEW-CBSA-WI - HLD-PREV-WI) / HLD-PREV-WI)
256400              < WI_PCT_REDUC_FY2020)
256500           COMPUTE W-NEW-CBSA-WI ROUNDED =
256600              HLD-PREV-WI * WI_PCT_ADJ_FY2020.
256700
256800 0550-BYPASS.
256900
257000**----------------------------------------------------------------
257100** GET WAGE INDEX FOR INDIAN HEALTH SERVICE PROVIDERS
257200**----------------------------------------------------------------
257300     IF P-N-INDIAN-HEALTH-SERVICE
257400        MOVE 00 TO PPS-RTC
257500        PERFORM 0850-N-GET-CBSA-INDIAN-WI THRU 0850-EXIT.
257600
257700***  RTC = 52  --  WAGE-INDEX NOT FOUND
257800     IF PPS-RTC = 52 GOBACK.
257900
258000     IF W-NEW-CBSA-WI NOT NUMERIC
258100        MOVE 0 TO W-NEW-CBSA-WI.
258200
258300     IF W-NEW-CBSA-WI = 00.0000
258400        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
258500        MOVE 52 TO PPS-RTC
258600        GOBACK.
258700
258800**----------------------------------------------------------------
258900** GET CBSA WAGE SIZE  - BASED ON DISCHARGE DATE
259000**----------------------------------------------------------------
259100     MOVE P-NEW-CBSA-STAND-AMT-LOC TO HOLD-PROV-CBSA.
259200
259300     PERFORM 0150-GET-CBSA THRU 0150-EXIT.
259400
259500***  RTC = 52  --  WAGE-INDEX NOT FOUND
259600     IF PPS-RTC = 52 GOBACK.
259700
259800     IF PPS-RTC = 00
259900        PERFORM 0750-GET-CBSA-SIZE
260000           THRU 0750-EXIT VARYING MA2
260100           FROM MA1 BY 1 UNTIL
260200           T-CBSA (MA2) NOT = HOLD-PROV-CBSA.
260300
260400**----------------------------------------------------------------
260500** FOR PUERTO RICO PROVIDERS, GET THE PUERTO RICO SPECIFIC
260600** WAGE INDEX
260700**----------------------------------------------------------------
260800     IF P-PR-NEW-STATE AND B-21-DISCHARGE-DATE < 20161001
260900        PERFORM 0250-N-GET-CBSA-PR THRU 0250-EXIT.
261000
261100**----------------------------------------------------------------
261200** FOR FYS 2015 AND AFTER, APPLY THE RURAL FLOOR POLICY TO THE
261300** PUERTO RICO SPECIFIC WAGE INDEX
261400**----------------------------------------------------------------
261500     IF P-PR-NEW-STATE AND B-21-DISCHARGE-DATE > 20140930
261600                       AND B-21-DISCHARGE-DATE < 20161001
261700        PERFORM 2350-2015-FWD-FLOOR-CBSA-PR
261800           THRU 2350-2015-EXIT.
261900
262000***  RTC = 52  --  WAGE-INDEX NOT FOUND
262100     IF PPS-RTC = 52 GOBACK.
262200
262300     IF P-PR-NEW-STATE AND B-21-DISCHARGE-DATE > 20160930
262400        GO TO 0550-EXIT.
262500
262600     IF P-PR-NEW-STATE AND
262700        W-NEW-CBSA-PR-WI NOT NUMERIC
262800        MOVE 0 TO W-NEW-CBSA-PR-WI.
262900
263000     IF P-PR-NEW-STATE AND
263100        W-NEW-CBSA-PR-WI = 00.0000
263200        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
263300        MOVE 52 TO PPS-RTC
263400        GOBACK.
263500
263600 0550-EXIT.  EXIT.
263700
263800 0600-N-GET-WAGE-INDX.
263900
264000     IF  B-21-DISCHARGE-DATE NOT < M-MSAX-EFF-DATE (MU2)
264100         MOVE M-MSAX-MSA        (MU2) TO W-NEW-MSA
264200         MOVE M-MSAX-EFF-DATE   (MU2) TO W-NEW-EFF-DATE
264300         MOVE M-MSAX-WAGE-INDX1 (MU2) TO W-NEW-INDEX-RECORD
264400         IF P-NEW-CHG-CODE-INDEX  = 'Y'
264500            MOVE M-MSAX-WAGE-INDX2 (MU2) TO W-NEW-INDEX-RECORD.
264600
264700 0600-N-EXIT.  EXIT.
264800
264900 0650-N-GET-CBSA-WAGE-INDX.
265000
265100     IF  B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2)
265200       IF (HOLD-PROV-CBSA = '   98'  OR
265300           HOLD-PROV-CBSA = '   99') OR
265400          (T-CBSA-EFF-DATE (MA2) >= W-FY-BEGIN-DATE AND
265500           T-CBSA-EFF-DATE (MA2) <= W-FY-END-DATE)
265600         MOVE T-CBSA            (MA2) TO W-NEW-CBSA-X
265700         MOVE T-CBSA-EFF-DATE   (MA2) TO W-NEW-CBSA-EFF-DATE
265800         MOVE T-CBSA-WAGE-INDX1 (MA2) TO W-NEW-CBSA-WI
265900         IF P-NEW-CBSA-WI-RECLASS
266000            MOVE T-CBSA-WAGE-INDX2 (MA2) TO W-NEW-CBSA-WI.
266100
266200 0650-N-EXIT.  EXIT.
266300
266400 0660-GET-RURAL-CBSA-WAGE-INDX.
266500
266600     IF B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2) AND
266700        T-CBSA-EFF-DATE (MA2) >= W-FY-BEGIN-DATE AND
266800        T-CBSA-EFF-DATE (MA2) <= W-FY-END-DATE
266900        MOVE T-CBSA            (MA2) TO W-RURAL-CBSA-X
267000        MOVE T-CBSA-EFF-DATE   (MA2) TO W-RURAL-CBSA-EFF-DATE
267100        MOVE T-CBSA-WAGE-INDX1 (MA2) TO W-RURAL-CBSA-WI.
267200
267300 0660-EXIT.  EXIT.
267400
267500 0670-GET-RURAL-CBSA-WAGE-INDX.
267600
267700     IF B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2) AND
267800        T-CBSA-EFF-DATE (MA2) >= W-FY-BEGIN-DATE AND
267900        T-CBSA-EFF-DATE (MA2) <= W-FY-END-DATE
268000        MOVE T-CBSA            (MA2) TO W-RURAL-CBSA-X
268100        MOVE T-CBSA-EFF-DATE   (MA2) TO W-RURAL-CBSA-EFF-DATE
268200        MOVE T-CBSA-WAGE-INDX3 (MA2) TO W-RURAL-CBSA-WI.
268300
268400 0670-EXIT.  EXIT.
268500
268600 0690-GET-RURAL-FLOOR-WAGE-INDX.
268700
268800     IF  B-21-DISCHARGE-DATE NOT < RUFL-EFF-DATE (RUFL-IDX2) AND
268900         RUFL-EFF-DATE (RUFL-IDX2) >= W-FY-BEGIN-DATE AND
269000         RUFL-EFF-DATE (RUFL-IDX2) <= W-FY-END-DATE
269100         MOVE RUFL-CBSA      (RUFL-IDX2)  TO W-RURAL-CBSA-X
269200         MOVE RUFL-EFF-DATE  (RUFL-IDX2)  TO W-RURAL-CBSA-EFF-DATE
269300         MOVE RUFL-WI3       (RUFL-IDX2)  TO W-RURAL-CBSA-WI.
269400
269500 0690-EXIT.  EXIT.
269600
269700 0700-N-GET-WAGE-SIZE.
269800
269900     IF  B-21-DISCHARGE-DATE NOT < M-MSAX-EFF-DATE (MU2)
270000         IF  P-NEW-STD-RURAL-CHECK
270100             MOVE 'R' TO W-NEW-SIZE
270200         ELSE
270300         IF  M-MSAX-SIZE (MU2) = 'L'
270400             MOVE 'L' TO W-NEW-SIZE
270500         ELSE
270600             MOVE 'O' TO W-NEW-SIZE.
270700 0700-N-EXIT.  EXIT.
270800
270900 0750-GET-CBSA-SIZE.
271000
271100     IF  B-21-DISCHARGE-DATE NOT < T-CBSA-EFF-DATE (MA2)
271200         IF  P-NEW-CBSA-STD-RURAL-CHECK
271300             MOVE 'R' TO W-NEW-CBSA-SIZE
271400         ELSE
271500         IF  T-CBSA-SIZE (MA2) = 'L'
271600             MOVE 'L' TO W-NEW-CBSA-SIZE
271700         ELSE
271800             MOVE 'O' TO W-NEW-CBSA-SIZE.
271900 0750-EXIT.  EXIT.
272000
272100 0800-N-GET-INDIAN-WI.
272200     IF  P-NEW-STATE = 02
272300             MOVE 98 TO H-MSAX-PROV-STATE
272400     ELSE
272500             MOVE 99 TO H-MSAX-PROV-STATE.
272600
272700     MOVE   '  '  TO H-MSAX-PROV-BLANK.
272800
272900     PERFORM 0100-GET-MSA THRU 0100-EXIT.
273000
273100     IF PPS-RTC = 00
273200        PERFORM 0600-N-GET-WAGE-INDX
273300            THRU 0600-N-EXIT VARYING MU2
273400            FROM MU1 BY 1 UNTIL
273500            M-MSAX-MSA (MU2) NOT = HOLD-PROV-MSAX.
273600
273700 0800-N-EXIT.  EXIT.
273800
273900 0850-N-GET-CBSA-INDIAN-WI.
274000     IF  P-NEW-STATE-CODE-X = 02
274100             MOVE 98 TO H-CBSA-PROV-STATE
274200     ELSE
274300             MOVE 99 TO H-CBSA-PROV-STATE.
274400
274500     MOVE  '   '  TO H-CBSA-PROV-BLANK.
274600
274700     PERFORM 0150-GET-CBSA THRU 0150-EXIT.
274800
274900     IF PPS-RTC = 00
275000        PERFORM 0650-N-GET-CBSA-WAGE-INDX
275100            THRU 0650-N-EXIT VARYING MA2
275200            FROM MA1 BY 1 UNTIL
275300            T-CBSA (MA2) NOT = HOLD-PROV-CBSA.
275400 0850-EXIT.  EXIT.
275500
275600 0900-GET-COUNTY-CODE.
275700
275800     SET OUTM-IDX TO 1.
275900
276000     SEARCH OUTM-TAB VARYING OUTM-IDX
276100     AT END
276200          GO TO 0900-EXIT
276300     WHEN OUTM-CNTY(OUTM-IDX) = P-NEW-COUNTY-CODE-X
276400        SET OUTM-IDX2 TO OUTM-IDX
276500        MOVE 1 TO OUTM-IND.
276600
276700 0900-EXIT.  EXIT.
276800
276900 0950-GET-OUTM-ADJ.
277000
277100     IF OUTM-EFF-DATE(OUTM-IDX2) <= B-21-DISCHARGE-DATE AND
277200        OUTM-EFF-DATE(OUTM-IDX2) >= W-FY-BEGIN-DATE AND
277300        OUTM-EFF-DATE(OUTM-IDX2) <= W-FY-END-DATE
277400          MOVE OUTM-ADJ-FACT(OUTM-IDX2) TO HLD-OUTM-ADJ.
277500
277600 0950-EXIT.  EXIT.
277700
277800 1000-GET-PREVYR-WI.
277900
278000     SET PREV-IDX TO 1.
278100
278200     SEARCH PREV-TAB VARYING PREV-IDX
278300     AT END
278400       MOVE 52 TO PPS-RTC
278500       GOBACK
278600     WHEN PREV-PROV(PREV-IDX) = P-NEW-PROVIDER-NO
278700       MOVE PREV-WI(PREV-IDX) TO HLD-PREV-WI.
278800
278900 1000-EXIT.  EXIT.
279000
279100*-----------------------------------------------------------------
279200
279300 2300-1998-FLOOR-MSA.
279400
279500        IF HOLD-PROV-MSAX = '6020'
279600           AND P-NEW-STATE = 36
279700               MOVE '  36' TO HOLD-PROV-MSAX.
279800
279900        IF HOLD-PROV-MSAX = '9000'
280000           AND P-NEW-STATE = 36
280100                MOVE '  36' TO HOLD-PROV-MSAX.
280200
280300 2300-1998-EXIT.  EXIT.
280400
280500 2300-1999-FLOOR-MSA.
280600
280700        IF HOLD-PROV-MSAX = '6020'
280800           AND P-NEW-STATE = 36
280900               MOVE '  36' TO HOLD-PROV-MSAX.
281000
281100        IF HOLD-PROV-MSAX = '9000'
281200           AND P-NEW-STATE = 36
281300                MOVE '  36' TO HOLD-PROV-MSAX.
281400
281500        IF HOLD-PROV-MSAX = '8080'
281600           AND P-NEW-STATE = 36
281700               MOVE '  36' TO HOLD-PROV-MSAX.
281800
281900        IF HOLD-PROV-MSAX = '1900'
282000           AND P-NEW-STATE = 21
282100               MOVE '  21' TO HOLD-PROV-MSAX.
282200
282300        IF HOLD-PROV-MSAX = '6340'
282400           AND P-NEW-CHG-CODE-INDEX = 'Y'
282500           AND P-NEW-STATE = 53
282600               MOVE '  53' TO HOLD-PROV-MSAX.
282700
282800 2300-1999-EXIT. EXIT.
282900
283000 2300-2000-FLOOR-MSA.
283100
283200        IF HOLD-PROV-MSAX = '6020'
283300           AND P-NEW-STATE = 36
283400               MOVE '  36' TO HOLD-PROV-MSAX.
283500
283600        IF HOLD-PROV-MSAX = '9000'
283700           AND P-NEW-STATE = 36
283800                MOVE '  36' TO HOLD-PROV-MSAX.
283900
284000        IF HOLD-PROV-MSAX = '8080'
284100           AND P-NEW-STATE = 36
284200               MOVE '  36' TO HOLD-PROV-MSAX.
284300
284400        IF HOLD-PROV-MSAX = '2440'
284500           AND P-NEW-STATE = 15
284600               MOVE '  15' TO HOLD-PROV-MSAX.
284700
284800        IF HOLD-PROV-MSAX = '2520'
284900           AND P-NEW-CHG-CODE-INDEX = 'Y'
285000           AND P-NEW-STATE = 24
285100               MOVE '  24' TO HOLD-PROV-MSAX.
285200
285300        IF HOLD-PROV-MSAX = '1123'
285400           AND P-NEW-STATE = 22
285500               MOVE '  22' TO HOLD-PROV-MSAX.
285600
285700 2300-2000-EXIT. EXIT.
285800
285900 2300-2001-FLOOR-MSA.
286000
286100        IF HOLD-PROV-MSAX = '1900'
286200           AND P-NEW-STATE = 21
286300               MOVE '  21' TO HOLD-PROV-MSAX.
286400
286500        IF HOLD-PROV-MSAX = '6020'
286600           AND P-NEW-STATE = 36
286700               MOVE '  36' TO HOLD-PROV-MSAX.
286800
286900        IF HOLD-PROV-MSAX = '8080'
287000           AND P-NEW-STATE = 36
287100               MOVE '  36' TO HOLD-PROV-MSAX.
287200
287300        IF HOLD-PROV-MSAX = '1123'
287400           AND P-NEW-STATE = 22
287500               MOVE '  22' TO HOLD-PROV-MSAX.
287600
287700        IF HOLD-PROV-MSAX = '2440'
287800           AND P-NEW-STATE = 15
287900               MOVE '  15' TO HOLD-PROV-MSAX.
288000
288100        IF HOLD-PROV-MSAX = '9000'
288200           AND P-NEW-STATE = 36
288300                MOVE '  36' TO HOLD-PROV-MSAX.
288400
288500        IF HOLD-PROV-MSAX = '9000'
288600           AND P-NEW-STATE = 51
288700                MOVE '  51' TO HOLD-PROV-MSAX.
288800
288900 2300-2001-EXIT. EXIT.
289000
289100 2300-2002-FLOOR-MSA.
289200
289300        IF HOLD-PROV-MSAX = '1123'
289400           AND P-NEW-STATE = 22
289500               MOVE '  22' TO HOLD-PROV-MSAX.
289600
289700        IF HOLD-PROV-MSAX = '1900'
289800           AND P-NEW-STATE = 21
289900               MOVE '  21' TO HOLD-PROV-MSAX.
290000
290100        IF HOLD-PROV-MSAX = '2440'
290200           AND P-NEW-STATE = 15
290300               MOVE '  15' TO HOLD-PROV-MSAX.
290400
290500        IF HOLD-PROV-MSAX = '6020'
290600           AND P-NEW-STATE = 36
290700               MOVE '  36' TO HOLD-PROV-MSAX.
290800
290900        IF HOLD-PROV-MSAX = '8080'
291000           AND P-NEW-STATE = 36
291100               MOVE '  36' TO HOLD-PROV-MSAX.
291200
291300        IF HOLD-PROV-MSAX = '9000'
291400           AND P-NEW-STATE = 36
291500                MOVE '  36' TO HOLD-PROV-MSAX.
291600
291700        IF HOLD-PROV-MSAX = '1303'
291800           AND P-NEW-CHG-CODE-INDEX = 'Y'
291900           AND P-NEW-STATE = 47
292000               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
292100               MOVE '  47' TO HOLD-PROV-MSAX.
292200
292300        IF HOLD-PROV-MSAX = '  14'
292400           AND P-NEW-CHG-CODE-INDEX = 'Y'
292500           AND P-NEW-STATE = 16
292600               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
292700               MOVE '  16' TO HOLD-PROV-MSAX.
292800
292900 2300-2002-EXIT. EXIT.
293000
293100 2300-2003-FLOOR-MSA.
293200
293300        IF HOLD-PROV-MSAX = '  14'
293400           AND P-NEW-CHG-CODE-INDEX = 'Y'
293500           AND P-NEW-STATE = 16
293600               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
293700               MOVE '  16' TO HOLD-PROV-MSAX.
293800
293900        IF HOLD-PROV-MSAX = '1123'
294000           AND P-NEW-STATE = 22
294100               MOVE '  22' TO HOLD-PROV-MSAX.
294200
294300        IF HOLD-PROV-MSAX = '1800'
294400           AND P-NEW-CHG-CODE-INDEX = 'Y'
294500           AND P-NEW-STATE = 11
294600               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
294700               MOVE '  11' TO HOLD-PROV-MSAX.
294800
294900        IF HOLD-PROV-MSAX = '1900'
295000           AND P-NEW-STATE = 21
295100               MOVE '  21' TO HOLD-PROV-MSAX.
295200
295300        IF HOLD-PROV-MSAX = '2440'
295400           AND P-NEW-STATE = 15
295500               MOVE '  15' TO HOLD-PROV-MSAX.
295600
295700        IF HOLD-PROV-MSAX = '3660'
295800           AND P-NEW-CHG-CODE-INDEX = 'Y'
295900           AND P-NEW-STATE = 49
296000               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
296100               MOVE '  49' TO HOLD-PROV-MSAX.
296200
296300        IF HOLD-PROV-MSAX = '3660'
296400           AND P-NEW-STATE = 49
296500               MOVE '  49' TO HOLD-PROV-MSAX.
296600
296700        IF HOLD-PROV-MSAX = '3700'
296800           AND P-NEW-CHG-CODE-INDEX = 'Y'
296900           AND P-NEW-STATE = 26
297000               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
297100               MOVE '  26' TO HOLD-PROV-MSAX.
297200
297300        IF HOLD-PROV-MSAX = '6020'
297400           AND P-NEW-STATE = 36
297500               MOVE '  36' TO HOLD-PROV-MSAX.
297600
297700        IF HOLD-PROV-MSAX = '9000'
297800           AND P-NEW-STATE = 36
297900                MOVE '  36' TO HOLD-PROV-MSAX.
298000
298100 2300-2003-EXIT. EXIT.
298200
298300 2300-2004-FLOOR-MSA.
298400
298500        IF HOLD-PROV-MSAX = '  14'
298600           AND P-NEW-CHG-CODE-INDEX = 'Y'
298700           AND P-NEW-STATE = 16
298800               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
298900               MOVE '  16' TO HOLD-PROV-MSAX.
299000
299100        IF HOLD-PROV-MSAX = '0200'
299200           AND P-NEW-CHG-CODE-INDEX = 'Y'
299300           AND P-NEW-STATE = 06
299400               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
299500               MOVE '  06' TO HOLD-PROV-MSAX.
299600
299700        IF HOLD-PROV-MSAX = '1480'
299800           AND P-NEW-CHG-CODE-INDEX = 'Y'
299900           AND P-NEW-STATE = 36
300000               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
300100               MOVE '  36' TO HOLD-PROV-MSAX.
300200
300300        IF HOLD-PROV-MSAX = '1900'
300400           AND P-NEW-STATE = 21
300500               MOVE '  21' TO HOLD-PROV-MSAX.
300600
300700        IF HOLD-PROV-MSAX = '2440'
300800           AND P-NEW-STATE = 15
300900               MOVE '  15' TO HOLD-PROV-MSAX.
301000
301100        IF HOLD-PROV-MSAX = '2985'
301200           AND P-NEW-STATE = 24
301300               MOVE '  24' TO HOLD-PROV-MSAX.
301400
301500        IF HOLD-PROV-MSAX = '3660'
301600           AND P-NEW-CHG-CODE-INDEX = 'Y'
301700           AND P-NEW-STATE = 49
301800               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
301900               MOVE '  49' TO HOLD-PROV-MSAX.
302000
302100        IF HOLD-PROV-MSAX = '3660'
302200           AND P-NEW-STATE = 49
302300               MOVE '  49' TO HOLD-PROV-MSAX.
302400
302500        IF HOLD-PROV-MSAX = '3700'
302600           AND P-NEW-CHG-CODE-INDEX = 'Y'
302700           AND P-NEW-STATE = 26
302800               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
302900               MOVE '  26' TO HOLD-PROV-MSAX.
303000
303100        IF HOLD-PROV-MSAX = '6020'
303200           AND P-NEW-STATE = 36
303300               MOVE '  36' TO HOLD-PROV-MSAX.
303400
303500        IF HOLD-PROV-MSAX = '6740'
303600           AND P-NEW-CHG-CODE-INDEX = 'Y'
303700           AND P-NEW-STATE = 50
303800               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
303900               MOVE '  50' TO HOLD-PROV-MSAX.
304000
304100        IF HOLD-PROV-MSAX = '7720'
304200           AND P-NEW-CHG-CODE-INDEX = 'Y'
304300           AND P-NEW-STATE = 28
304400               MOVE 'N' TO P-NEW-CHG-CODE-INDEX
304500               MOVE '  28' TO HOLD-PROV-MSAX.
304600
304700        IF HOLD-PROV-MSAX = '8080'
304800           AND P-NEW-STATE = 36
304900                MOVE '  36' TO HOLD-PROV-MSAX.
305000
305100        IF HOLD-PROV-MSAX = '9000'
305200           AND P-NEW-STATE = 36
305300                MOVE '  36' TO HOLD-PROV-MSAX.
305400
305500 2300-2004-EXIT. EXIT.
305600 2300-2004-RECLASS152.
305700
305800       IF (P-NEW-PROVIDER-NO = '330001' OR '330126' OR
305900                               '330135' OR '330205' OR
306000                               '330209' OR '330264')
306100          AND (P-NEW-WAGE-INDEX-LOC-MSA = '5600'
306200          AND  P-NEW-CHG-CODE-INDEX = 'Y')
306300               MOVE 1.3892 TO W-NEW-INDEX-RECORD.
306400
306500       IF (P-NEW-PROVIDER-NO = '470003')
306600          AND (P-NEW-WAGE-INDEX-LOC-MSA = '1123'
306700          AND  P-NEW-CHG-CODE-INDEX = 'Y')
306800               MOVE 1.1120 TO W-NEW-INDEX-RECORD.
306900
307000 2300-2004-RECLASS-EXIT. EXIT.
307100
307200 2300-2005-FLOOR-CBSA.
307300
307400        IF HOLD-PROV-CBSA = '10900'
307500           AND P-NEW-STATE = 31
307600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
307700               MOVE '   31' TO HOLD-PROV-CBSA.
307800
307900        IF HOLD-PROV-CBSA = '16620'
308000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
308100           AND P-NEW-STATE = 36
308200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
308300               MOVE '   36' TO HOLD-PROV-CBSA.
308400
308500        IF HOLD-PROV-CBSA = '19060'
308600           AND P-NEW-STATE = 21
308700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
308800               MOVE '   21' TO HOLD-PROV-CBSA.
308900
309000        IF HOLD-PROV-CBSA = '21780'
309100           AND P-NEW-STATE = 15
309200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
309300               MOVE '   15' TO HOLD-PROV-CBSA.
309400
309500        IF HOLD-PROV-CBSA = '22020'
309600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
309700           AND P-NEW-STATE = 24
309800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
309900               MOVE '   24' TO HOLD-PROV-CBSA.
310000
310100        IF HOLD-PROV-CBSA = '22020'
310200           AND P-NEW-STATE = 24
310300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
310400               MOVE '   24' TO HOLD-PROV-CBSA.
310500
310600        IF HOLD-PROV-CBSA = '24220'
310700           AND P-NEW-STATE = 24
310800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
310900               MOVE '   24' TO HOLD-PROV-CBSA.
311000
311100        IF HOLD-PROV-CBSA = '25540'
311200           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
311300           AND P-NEW-STATE = 07
311400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
311500               MOVE '   07' TO HOLD-PROV-CBSA.
311600
311700        IF HOLD-PROV-CBSA = '29100'
311800           AND P-NEW-STATE = 52
311900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
312000               MOVE '   52' TO HOLD-PROV-CBSA.
312100
312200        IF HOLD-PROV-CBSA = '30300'
312300           AND P-NEW-STATE = 50
312400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
312500               MOVE '   50' TO HOLD-PROV-CBSA.
312600
312700        IF HOLD-PROV-CBSA = '37620'
312800           AND P-NEW-STATE = 36
312900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
313000               MOVE '   36' TO HOLD-PROV-CBSA.
313100
313200        IF HOLD-PROV-CBSA = '48260'
313300           AND P-NEW-STATE = 36
313400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
313500               MOVE '   36' TO HOLD-PROV-CBSA.
313600
313700        IF HOLD-PROV-CBSA = '48540'
313800           AND P-NEW-STATE = 36
313900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
314000               MOVE '   36' TO HOLD-PROV-CBSA.
314100
314200        IF HOLD-PROV-CBSA = '48864'
314300           AND P-NEW-STATE = 31
314400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
314500               MOVE '   31' TO HOLD-PROV-CBSA.
314600
314700        IF HOLD-PROV-CBSA = '48864'
314800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
314900           AND P-NEW-STATE = 31
315000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
315100               MOVE '   31' TO HOLD-PROV-CBSA.
315200
315300        IF B-21-DISCHARGE-DATE > 20041231
315400           IF HOLD-PROV-CBSA = '39900'
315500              AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
315600              AND P-NEW-STATE = 05
315700                  MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
315800                  MOVE '   05' TO HOLD-PROV-CBSA.
315900
316000        IF B-21-DISCHARGE-DATE < 20050101
316100           IF (HOLD-PROV-CBSA = '28420'
316200               AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
316300               AND P-NEW-STATE = 50)
316400                  MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
316500                  MOVE '   50' TO HOLD-PROV-CBSA.
316600
316700 2300-2005-EXIT. EXIT.
316800
316900
317000 2300-2006-FLOOR-CBSA.
317100
317200        IF HOLD-PROV-CBSA = '   10'
317300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
317400           AND P-NEW-STATE = 10
317500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
317600               MOVE '   10' TO HOLD-PROV-CBSA.
317700
317800        IF HOLD-PROV-CBSA = '   50'
317900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
318000           AND P-NEW-STATE = 50
318100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
318200               MOVE '   50' TO HOLD-PROV-CBSA.
318300
318400        IF HOLD-PROV-CBSA = '10900'
318500           AND P-NEW-STATE = 31
318600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
318700               MOVE '   31' TO HOLD-PROV-CBSA.
318800
318900        IF HOLD-PROV-CBSA = '15764'
319000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
319100           AND P-NEW-STATE = 30
319200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
319300               MOVE '   30' TO HOLD-PROV-CBSA.
319400
319500        IF HOLD-PROV-CBSA = '16620'
319600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
319700           AND P-NEW-STATE = 36
319800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
319900               MOVE '   36' TO HOLD-PROV-CBSA.
320000
320100        IF HOLD-PROV-CBSA = '19060'
320200           AND P-NEW-STATE = 21
320300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
320400               MOVE '   21' TO HOLD-PROV-CBSA.
320500
320600        IF HOLD-PROV-CBSA = '22020'
320700           AND P-NEW-STATE = 24
320800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
320900               MOVE '   24' TO HOLD-PROV-CBSA.
321000
321100        IF HOLD-PROV-CBSA = '24220'
321200           AND P-NEW-STATE = 24
321300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
321400               MOVE '   24' TO HOLD-PROV-CBSA.
321500
321600        IF HOLD-PROV-CBSA = '24580'
321700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
321800           AND P-NEW-STATE = 52
321900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
322000               MOVE '   52' TO HOLD-PROV-CBSA.
322100
322200        IF HOLD-PROV-CBSA = '25540'
322300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
322400           AND P-NEW-STATE = 07
322500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
322600               MOVE '   07' TO HOLD-PROV-CBSA.
322700
322800        IF HOLD-PROV-CBSA = '30300'
322900           AND P-NEW-STATE = 50
323000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
323100               MOVE '   50' TO HOLD-PROV-CBSA.
323200
323300        IF HOLD-PROV-CBSA = '37620'
323400           AND P-NEW-STATE = 36
323500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
323600               MOVE '   36' TO HOLD-PROV-CBSA.
323700
323800        IF HOLD-PROV-CBSA = '39900'
323900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
324000           AND P-NEW-STATE = 05
324100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
324200               MOVE '   05' TO HOLD-PROV-CBSA.
324300
324400        IF HOLD-PROV-CBSA = '48260'
324500           AND P-NEW-STATE = 36
324600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
324700               MOVE '   36' TO HOLD-PROV-CBSA.
324800
324900        IF HOLD-PROV-CBSA = '48540'
325000           AND P-NEW-STATE = 36
325100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
325200               MOVE '   36' TO HOLD-PROV-CBSA.
325300
325400        IF HOLD-PROV-CBSA = '48540'
325500           AND P-NEW-STATE = 51
325600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
325700               MOVE '   51' TO HOLD-PROV-CBSA.
325800
325900        IF HOLD-PROV-CBSA = '48864'
326000           AND P-NEW-STATE = 31
326100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
326200               MOVE '   31' TO HOLD-PROV-CBSA.
326300
326400        IF HOLD-PROV-CBSA = '49660'
326500           AND P-NEW-STATE = 36
326600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
326700               MOVE '   36' TO HOLD-PROV-CBSA.
326800
326900 2300-2006-EXIT. EXIT.
327000
327100 2300-2007-FLOOR-CBSA.
327200
327300        IF HOLD-PROV-CBSA = '   10'
327400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
327500           AND P-NEW-STATE = 10
327600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
327700               MOVE '   10' TO HOLD-PROV-CBSA.
327800
327900        IF HOLD-PROV-CBSA = '   14'
328000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
328100           AND P-NEW-STATE = 14
328200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
328300               MOVE '   14' TO HOLD-PROV-CBSA.
328400
328500        IF HOLD-PROV-CBSA = '   26'
328600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
328700           AND P-NEW-STATE = 26
328800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
328900               MOVE '   26' TO HOLD-PROV-CBSA.
329000
329100        IF HOLD-PROV-CBSA = '   50'
329200           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
329300           AND P-NEW-STATE = 50
329400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
329500               MOVE '   50' TO HOLD-PROV-CBSA.
329600
329700        IF HOLD-PROV-CBSA = '10900'
329800           AND P-NEW-STATE = 31
329900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
330000               MOVE '   31' TO HOLD-PROV-CBSA.
330100
330200        IF HOLD-PROV-CBSA = '19060'
330300           AND P-NEW-STATE = 21
330400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
330500               MOVE '   21' TO HOLD-PROV-CBSA.
330600
330700        IF HOLD-PROV-CBSA = '22020'
330800           AND P-NEW-STATE = 24
330900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
331000               MOVE '   24' TO HOLD-PROV-CBSA.
331100
331200        IF HOLD-PROV-CBSA = '24220'
331300           AND P-NEW-STATE = 24
331400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
331500               MOVE '   24' TO HOLD-PROV-CBSA.
331600
331700        IF HOLD-PROV-CBSA = '24580'
331800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
331900           AND P-NEW-STATE = 52
332000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
332100               MOVE '   52' TO HOLD-PROV-CBSA.
332200
332300        IF HOLD-PROV-CBSA = '25540'
332400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
332500           AND P-NEW-STATE = 07
332600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
332700               MOVE '   07' TO HOLD-PROV-CBSA.
332800
332900        IF HOLD-PROV-CBSA = '26580'
333000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
333100           AND P-NEW-STATE = 36
333200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
333300               MOVE '   36' TO HOLD-PROV-CBSA.
333400
333500        IF B-21-DISCHARGE-DATE < 20061103
333600           IF (HOLD-PROV-CBSA = '27860'
333700               AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
333800               AND P-NEW-STATE = 26)
333900                   MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
334000                   MOVE '   26' TO HOLD-PROV-CBSA.
334100
334200        IF HOLD-PROV-CBSA = '29100'
334300           AND P-NEW-STATE = 52
334400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
334500               MOVE '   52' TO HOLD-PROV-CBSA.
334600
334700        IF HOLD-PROV-CBSA = '30300'
334800           AND P-NEW-STATE = 50
334900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
335000               MOVE '   50' TO HOLD-PROV-CBSA.
335100
335200        IF HOLD-PROV-CBSA = '37620'
335300           AND P-NEW-STATE = 36
335400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
335500               MOVE '   36' TO HOLD-PROV-CBSA.
335600
335700        IF HOLD-PROV-CBSA = '37964'
335800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
335900           AND P-NEW-STATE = 31
336000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
336100               MOVE '   31' TO HOLD-PROV-CBSA.
336200
336300        IF HOLD-PROV-CBSA = '38300'
336400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
336500           AND P-NEW-STATE = 36
336600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
336700               MOVE '   36' TO HOLD-PROV-CBSA.
336800
336900        IF HOLD-PROV-CBSA = '39300'
337000           AND P-NEW-STATE = 22
337100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
337200               MOVE '   22' TO HOLD-PROV-CBSA.
337300
337400        IF HOLD-PROV-CBSA = '39300'
337500           AND P-NEW-STATE = 41
337600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
337700               MOVE '   41' TO HOLD-PROV-CBSA.
337800
337900        IF HOLD-PROV-CBSA = '45500'
338000           AND P-NEW-STATE = 45
338100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
338200               MOVE '   45' TO HOLD-PROV-CBSA.
338300
338400        IF HOLD-PROV-CBSA = '48260'
338500           AND P-NEW-STATE = 36
338600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
338700               MOVE '   36' TO HOLD-PROV-CBSA.
338800
338900        IF HOLD-PROV-CBSA = '48540'
339000           AND P-NEW-STATE = 36
339100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
339200               MOVE '   36' TO HOLD-PROV-CBSA.
339300
339400        IF HOLD-PROV-CBSA = '48540'
339500           AND P-NEW-STATE = 51
339600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
339700               MOVE '   51' TO HOLD-PROV-CBSA.
339800
339900        IF HOLD-PROV-CBSA = '48864'
340000           AND P-NEW-STATE = 31
340100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
340200               MOVE '   31' TO HOLD-PROV-CBSA.
340300
340400
340500 2300-2007-EXIT. EXIT.
340600
340700 2300-2008-FLOOR-CBSA.
340800
340900        IF HOLD-PROV-CBSA = '   39'
341000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
341100           AND P-NEW-STATE = 33
341200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
341300               MOVE '   33' TO HOLD-PROV-CBSA.
341400
341500        IF HOLD-PROV-CBSA = '   39'
341600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
341700           AND P-NEW-STATE = 39
341800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
341900               MOVE '   39' TO HOLD-PROV-CBSA.
342000
342100        IF HOLD-PROV-CBSA = '10900'
342200           AND P-NEW-STATE = 31
342300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
342400               MOVE '   31' TO HOLD-PROV-CBSA.
342500
342600        IF HOLD-PROV-CBSA = '19060'
342700           AND P-NEW-STATE = 21
342800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
342900               MOVE '   21' TO HOLD-PROV-CBSA.
343000
343100        IF HOLD-PROV-CBSA = '21780'
343200           AND P-NEW-STATE = 15
343300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
343400               MOVE '   15' TO HOLD-PROV-CBSA.
343500
343600        IF HOLD-PROV-CBSA = '21780'
343700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
343800           AND P-NEW-STATE = 15
343900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
344000               MOVE '   15' TO HOLD-PROV-CBSA.
344100
344200        IF HOLD-PROV-CBSA = '22020'
344300           AND P-NEW-STATE = 24
344400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
344500               MOVE '   24' TO HOLD-PROV-CBSA.
344600
344700        IF HOLD-PROV-CBSA = '24220'
344800           AND P-NEW-STATE = 24
344900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
345000               MOVE '   24' TO HOLD-PROV-CBSA.
345100
345200        IF HOLD-PROV-CBSA = '24580'
345300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
345400           AND P-NEW-STATE = 52
345500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
345600               MOVE '   52' TO HOLD-PROV-CBSA.
345700
345800        IF HOLD-PROV-CBSA = '25540'
345900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
346000           AND P-NEW-STATE = 07
346100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
346200               MOVE '   07' TO HOLD-PROV-CBSA.
346300
346400        IF HOLD-PROV-CBSA = '28420'
346500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
346600           AND P-NEW-STATE = 50
346700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
346800               MOVE '   50' TO HOLD-PROV-CBSA.
346900
347000        IF HOLD-PROV-CBSA = '28700'
347100           AND P-NEW-STATE = 44
347200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
347300               MOVE '   44' TO HOLD-PROV-CBSA.
347400
347500        IF HOLD-PROV-CBSA = '28700'
347600           AND P-NEW-STATE = 49
347700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
347800               MOVE '   49' TO HOLD-PROV-CBSA.
347900
348000        IF HOLD-PROV-CBSA = '30300'
348100           AND P-NEW-STATE = 50
348200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
348300               MOVE '   50' TO HOLD-PROV-CBSA.
348400
348500        IF HOLD-PROV-CBSA = '35084'
348600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
348700           AND P-NEW-STATE = 31
348800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
348900               MOVE '   31' TO HOLD-PROV-CBSA.
349000
349100        IF HOLD-PROV-CBSA = '37620'
349200           AND P-NEW-STATE = 36
349300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
349400               MOVE '   36' TO HOLD-PROV-CBSA.
349500
349600        IF HOLD-PROV-CBSA = '37964'
349700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
349800           AND P-NEW-STATE = 31
349900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
350000               MOVE '   31' TO HOLD-PROV-CBSA.
350100
350200        IF HOLD-PROV-CBSA = '38300'
350300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
350400           AND P-NEW-STATE = 36
350500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
350600               MOVE '   36' TO HOLD-PROV-CBSA.
350700
350800        IF HOLD-PROV-CBSA = '45500'
350900           AND P-NEW-STATE = 45
351000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
351100               MOVE '   45' TO HOLD-PROV-CBSA.
351200
351300        IF HOLD-PROV-CBSA = '48260'
351400           AND P-NEW-STATE = 36
351500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
351600               MOVE '   36' TO HOLD-PROV-CBSA.
351700
351800        IF HOLD-PROV-CBSA = '48540'
351900           AND P-NEW-STATE = 36
352000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
352100               MOVE '   36' TO HOLD-PROV-CBSA.
352200
352300        IF HOLD-PROV-CBSA = '48540'
352400           AND P-NEW-STATE = 51
352500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
352600               MOVE '   51' TO HOLD-PROV-CBSA.
352700
352800        IF HOLD-PROV-CBSA = '48864'
352900           AND P-NEW-STATE = 31
353000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
353100               MOVE '   31' TO HOLD-PROV-CBSA.
353200
353300        IF HOLD-PROV-CBSA = '48864'
353400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
353500           AND P-NEW-STATE = 31
353600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
353700               MOVE '   31' TO HOLD-PROV-CBSA.
353800
353900
354000 2300-2008-EXIT. EXIT.
354100
354200**************YEARCHANGE 2009.3 **********************      *****
354300
354400 2300-2009-FLOOR-CBSA.
354500
354600        IF HOLD-PROV-CBSA = '   04'
354700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
354800           AND P-NEW-STATE = 04
354900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
355000               MOVE '   04' TO HOLD-PROV-CBSA.
355100
355200        IF HOLD-PROV-CBSA = '   04'
355300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
355400           AND P-NEW-STATE = 19
355500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
355600               MOVE '   19' TO HOLD-PROV-CBSA.
355700
355800        IF HOLD-PROV-CBSA = '   14'
355900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
356000           AND P-NEW-STATE = 14
356100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
356200               MOVE '   14' TO HOLD-PROV-CBSA.
356300
356400        IF HOLD-PROV-CBSA = '   14'
356500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
356600           AND P-NEW-STATE = 26
356700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
356800               MOVE '   26' TO HOLD-PROV-CBSA.
356900
357000        IF HOLD-PROV-CBSA = '10900'
357100           AND P-NEW-STATE = 31
357200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
357300               MOVE '   31' TO HOLD-PROV-CBSA.
357400
357500        IF HOLD-PROV-CBSA = '19340'
357600           AND P-NEW-STATE = 16
357700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
357800               MOVE '   16' TO HOLD-PROV-CBSA.
357900
358000        IF HOLD-PROV-CBSA = '21780'
358100           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
358200           AND P-NEW-STATE = 15
358300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
358400               MOVE '   15' TO HOLD-PROV-CBSA.
358500
358600        IF HOLD-PROV-CBSA = '22020'
358700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
358800           AND P-NEW-STATE = 43
358900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
359000               MOVE '   43' TO HOLD-PROV-CBSA.
359100
359200        IF HOLD-PROV-CBSA = '22900'
359300           AND P-NEW-STATE = 37
359400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
359500               MOVE '   37' TO HOLD-PROV-CBSA.
359600
359700        IF HOLD-PROV-CBSA = '24580'
359800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
359900           AND P-NEW-STATE = 52
360000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
360100               MOVE '   52' TO HOLD-PROV-CBSA.
360200
360300        IF HOLD-PROV-CBSA = '25540'
360400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
360500           AND P-NEW-STATE = 07
360600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
360700               MOVE '   07' TO HOLD-PROV-CBSA.
360800
360900        IF HOLD-PROV-CBSA = '28420'
361000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
361100           AND P-NEW-STATE = 50
361200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
361300               MOVE '   50' TO HOLD-PROV-CBSA.
361400
361500        IF HOLD-PROV-CBSA = '28700'
361600           AND P-NEW-STATE = 44
361700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
361800               MOVE '   44' TO HOLD-PROV-CBSA.
361900
362000        IF HOLD-PROV-CBSA = '28700'
362100           AND P-NEW-STATE = 49
362200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
362300               MOVE '   49' TO HOLD-PROV-CBSA.
362400
362500        IF HOLD-PROV-CBSA = '28700'
362600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
362700           AND P-NEW-STATE = 18
362800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
362900               MOVE '   18' TO HOLD-PROV-CBSA.
363000
363100        IF HOLD-PROV-CBSA = '28700'
363200           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
363300           AND P-NEW-STATE = 44
363400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
363500               MOVE '   44' TO HOLD-PROV-CBSA.
363600
363700        IF HOLD-PROV-CBSA = '28940'
363800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
363900           AND P-NEW-STATE = 18
364000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
364100               MOVE '   18' TO HOLD-PROV-CBSA.
364200
364300        IF HOLD-PROV-CBSA = '28940'
364400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
364500           AND P-NEW-STATE = 44
364600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
364700               MOVE '   44' TO HOLD-PROV-CBSA.
364800
364900        IF HOLD-PROV-CBSA = '34820'
365000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
365100           AND P-NEW-STATE = 34
365200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
365300               MOVE '   34' TO HOLD-PROV-CBSA.
365400
365500        IF HOLD-PROV-CBSA = '34820'
365600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
365700           AND P-NEW-STATE = 42
365800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
365900               MOVE '   42' TO HOLD-PROV-CBSA.
366000
366100        IF HOLD-PROV-CBSA = '37620'
366200           AND P-NEW-STATE = 36
366300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
366400               MOVE '   36' TO HOLD-PROV-CBSA.
366500
366600        IF HOLD-PROV-CBSA = '37964'
366700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
366800           AND P-NEW-STATE = 31
366900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
367000               MOVE '   31' TO HOLD-PROV-CBSA.
367100
367200        IF HOLD-PROV-CBSA = '38340'
367300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
367400           AND P-NEW-STATE = 47
367500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
367600               MOVE '   47' TO HOLD-PROV-CBSA.
367700
367800        IF HOLD-PROV-CBSA = '41620'
367900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
368000           AND P-NEW-STATE = 29
368100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
368200               MOVE '   29' TO HOLD-PROV-CBSA.
368300
368400        IF HOLD-PROV-CBSA = '43580'
368500           AND P-NEW-STATE = 16
368600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
368700               MOVE '   16' TO HOLD-PROV-CBSA.
368800
368900        IF HOLD-PROV-CBSA = '48540'
369000           AND P-NEW-STATE = 36
369100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
369200               MOVE '   36' TO HOLD-PROV-CBSA.
369300
369400        IF HOLD-PROV-CBSA = '48540'
369500           AND P-NEW-STATE = 51
369600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
369700               MOVE '   51' TO HOLD-PROV-CBSA.
369800
369900        IF HOLD-PROV-CBSA = '48864'
370000           AND P-NEW-STATE = 31
370100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
370200               MOVE '   31' TO HOLD-PROV-CBSA.
370300
370400        IF HOLD-PROV-CBSA = '48864'
370500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
370600           AND P-NEW-STATE = 31
370700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
370800               MOVE '   31' TO HOLD-PROV-CBSA.
370900
371000        IF HOLD-PROV-CBSA = '19060'
371100           AND P-NEW-STATE = 21
371200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
371300               MOVE '   21' TO HOLD-PROV-CBSA.
371400
371500        IF HOLD-PROV-CBSA = '19060'
371600           AND P-NEW-STATE = 51
371700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
371800               MOVE '   51' TO HOLD-PROV-CBSA.
371900
372000        IF HOLD-PROV-CBSA = '22020'
372100           AND P-NEW-STATE = 24
372200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
372300               MOVE '   24' TO HOLD-PROV-CBSA.
372400
372500        IF HOLD-PROV-CBSA = '24220'
372600           AND P-NEW-STATE = 24
372700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
372800               MOVE '   24' TO HOLD-PROV-CBSA.
372900
373000        IF HOLD-PROV-CBSA = '30300'
373100           AND P-NEW-STATE = 50
373200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
373300               MOVE '   50' TO HOLD-PROV-CBSA.
373400
373500        IF HOLD-PROV-CBSA = '48260'
373600           AND P-NEW-STATE = 36
373700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
373800               MOVE '   36' TO HOLD-PROV-CBSA.
373900
374000
374100
374200**************YEARCHANGE 2009.3 **********************      *****
374300
374400 2300-2009-EXIT. EXIT.
374500
374600**************YEARCHANGE 2010.0 **********************      *****
374700
374800 2300-2010-FLOOR-CBSA.
374900
375000        IF HOLD-PROV-CBSA = '   33'
375100          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
375200          AND P-NEW-STATE = 33
375300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
375400               MOVE '   33' TO HOLD-PROV-CBSA.
375500
375600        IF HOLD-PROV-CBSA = '   30'
375700          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
375800          AND P-NEW-STATE = 30
375900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
376000               MOVE '   30' TO HOLD-PROV-CBSA.
376100
376200        IF HOLD-PROV-CBSA = '   33'
376300          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
376400          AND P-NEW-STATE = 30
376500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
376600               MOVE '   30' TO HOLD-PROV-CBSA.
376700
376800        IF HOLD-PROV-CBSA = '10900'
376900           AND P-NEW-STATE = 31
377000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
377100               MOVE '   31' TO HOLD-PROV-CBSA.
377200
377300        IF HOLD-PROV-CBSA = '19340'
377400           AND P-NEW-STATE = 16
377500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
377600               MOVE '   16' TO HOLD-PROV-CBSA.
377700
377800        IF HOLD-PROV-CBSA = '19340'
377900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
378000           AND P-NEW-STATE = 16
378100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
378200               MOVE '   16' TO HOLD-PROV-CBSA.
378300
378400        IF HOLD-PROV-CBSA = '21780'
378500           AND P-NEW-STATE = 15
378600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
378700               MOVE '   15' TO HOLD-PROV-CBSA.
378800
378900        IF HOLD-PROV-CBSA = '21780'
379000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
379100           AND P-NEW-STATE = 15
379200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
379300               MOVE '   15' TO HOLD-PROV-CBSA.
379400
379500        IF HOLD-PROV-CBSA = '25180'
379600           AND P-NEW-STATE = 21
379700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
379800               MOVE '   21' TO HOLD-PROV-CBSA.
379900
380000        IF HOLD-PROV-CBSA = '25540'
380100           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
380200           AND P-NEW-STATE = 07
380300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
380400               MOVE '   07' TO HOLD-PROV-CBSA.
380500
380600        IF HOLD-PROV-CBSA = '28420'
380700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
380800           AND P-NEW-STATE = 50
380900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
381000               MOVE '   50' TO HOLD-PROV-CBSA.
381100
381200        IF HOLD-PROV-CBSA = '28940'
381300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
381400           AND P-NEW-STATE = 18
381500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
381600               MOVE '   18' TO HOLD-PROV-CBSA.
381700
381800        IF HOLD-PROV-CBSA = '28940'
381900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
382000           AND P-NEW-STATE = 44
382100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
382200               MOVE '   44' TO HOLD-PROV-CBSA.
382300
382400        IF HOLD-PROV-CBSA = '35084'
382500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
382600           AND P-NEW-STATE = 31
382700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
382800               MOVE '   31' TO HOLD-PROV-CBSA.
382900
383000        IF HOLD-PROV-CBSA = '37620'
383100           AND P-NEW-STATE = 36
383200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
383300               MOVE '   36' TO HOLD-PROV-CBSA.
383400
383500        IF HOLD-PROV-CBSA = '37964'
383600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
383700           AND P-NEW-STATE = 31
383800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
383900               MOVE '   31' TO HOLD-PROV-CBSA.
384000
384100        IF HOLD-PROV-CBSA = '48540'
384200           AND P-NEW-STATE = 36
384300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
384400               MOVE '   36' TO HOLD-PROV-CBSA.
384500
384600        IF HOLD-PROV-CBSA = '48540'
384700           AND P-NEW-STATE = 51
384800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
384900               MOVE '   51' TO HOLD-PROV-CBSA.
385000
385100        IF HOLD-PROV-CBSA = '48864'
385200           AND P-NEW-STATE = 31
385300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
385400               MOVE '   31' TO HOLD-PROV-CBSA.
385500
385600        IF HOLD-PROV-CBSA = '48864'
385700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
385800           AND P-NEW-STATE = 31
385900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
386000               MOVE '   31' TO HOLD-PROV-CBSA.
386100
386200        IF HOLD-PROV-CBSA = '49660'
386300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
386400           AND P-NEW-STATE = 36
386500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
386600               MOVE '   36' TO HOLD-PROV-CBSA.
386700
386800        IF HOLD-PROV-CBSA = '19060'
386900           AND P-NEW-STATE = 21
387000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
387100               MOVE '   21' TO HOLD-PROV-CBSA.
387200
387300        IF HOLD-PROV-CBSA = '22020'
387400           AND P-NEW-STATE = 24
387500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
387600               MOVE '   24' TO HOLD-PROV-CBSA.
387700
387800        IF HOLD-PROV-CBSA = '24220'
387900           AND P-NEW-STATE = 24
388000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
388100               MOVE '   24' TO HOLD-PROV-CBSA.
388200
388300        IF HOLD-PROV-CBSA = '30300'
388400           AND P-NEW-STATE = 50
388500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
388600               MOVE '   50' TO HOLD-PROV-CBSA.
388700
388800        IF HOLD-PROV-CBSA = '35084'
388900           AND P-NEW-STATE = 31
389000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
389100               MOVE '   31' TO HOLD-PROV-CBSA.
389200
389300        IF HOLD-PROV-CBSA = '48260'
389400           AND P-NEW-STATE = 36
389500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
389600               MOVE '   36' TO HOLD-PROV-CBSA.
389700
389800        IF HOLD-PROV-CBSA = '48260'
389900           AND P-NEW-STATE = 51
390000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
390100               MOVE '   51' TO HOLD-PROV-CBSA.
390200
390300
390400
390500**************YEARCHANGE 2010.0 **********************      *****
390600
390700 2300-2010-EXIT. EXIT.
390800
390900
391000**************YEARCHANGE 2011.0 **********************      *****
391100
391200 2300-2011-FLOOR-CBSA.
391300
391400        IF HOLD-PROV-CBSA = '   45'
391500          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
391600          AND P-NEW-STATE = 45
391700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
391800               MOVE '   45' TO HOLD-PROV-CBSA.
391900
392000        IF HOLD-PROV-CBSA = '   37'
392100          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
392200          AND P-NEW-STATE = 37
392300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
392400               MOVE '   37' TO HOLD-PROV-CBSA.
392500
392600        IF HOLD-PROV-CBSA = '10900'
392700           AND P-NEW-STATE = 31
392800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
392900               MOVE '   31' TO HOLD-PROV-CBSA.
393000
393100        IF HOLD-PROV-CBSA = '21500'
393200          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
393300           AND P-NEW-STATE = 33
393400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
393500               MOVE '   33' TO HOLD-PROV-CBSA.
393600
393700        IF HOLD-PROV-CBSA = '21500'
393800          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
393900           AND P-NEW-STATE = 39
394000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
394100               MOVE '   39' TO HOLD-PROV-CBSA.
394200
394300        IF HOLD-PROV-CBSA = '21780'
394400           AND P-NEW-STATE = 15
394500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
394600               MOVE '   15' TO HOLD-PROV-CBSA.
394700
394800        IF HOLD-PROV-CBSA = '22900'
394900           AND P-NEW-STATE = 37
395000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
395100               MOVE '   37' TO HOLD-PROV-CBSA.
395200
395300        IF HOLD-PROV-CBSA = '24540'
395400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
395500           AND P-NEW-STATE = 53
395600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
395700               MOVE '   53' TO HOLD-PROV-CBSA.
395800
395900        IF HOLD-PROV-CBSA = '25540'
396000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
396100           AND P-NEW-STATE = 07
396200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
396300               MOVE '   07' TO HOLD-PROV-CBSA.
396400
396500        IF HOLD-PROV-CBSA = '28700'
396600           AND P-NEW-STATE = 44
396700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
396800               MOVE '   44' TO HOLD-PROV-CBSA.
396900
397000        IF HOLD-PROV-CBSA = '28700'
397100           AND P-NEW-STATE = 49
397200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
397300               MOVE '   49' TO HOLD-PROV-CBSA.
397400
397500        IF HOLD-PROV-CBSA = '28940'
397600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
397700           AND P-NEW-STATE = 18
397800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
397900               MOVE '   18' TO HOLD-PROV-CBSA.
398000
398100        IF HOLD-PROV-CBSA = '28940'
398200           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
398300           AND P-NEW-STATE = 44
398400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
398500               MOVE '   44' TO HOLD-PROV-CBSA.
398600
398700        IF HOLD-PROV-CBSA = '37620'
398800           AND P-NEW-STATE = 36
398900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
399000               MOVE '   36' TO HOLD-PROV-CBSA.
399100
399200        IF HOLD-PROV-CBSA = '37620'
399300           AND P-NEW-STATE = 51
399400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
399500               MOVE '   51' TO HOLD-PROV-CBSA.
399600
399700        IF HOLD-PROV-CBSA = '37964'
399800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
399900           AND P-NEW-STATE = 31
400000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
400100               MOVE '   31' TO HOLD-PROV-CBSA.
400200
400300        IF HOLD-PROV-CBSA = '38300'
400400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
400500           AND P-NEW-STATE = 36
400600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
400700               MOVE '   36' TO HOLD-PROV-CBSA.
400800
400900        IF HOLD-PROV-CBSA = '38300'
401000           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
401100           AND P-NEW-STATE = 39
401200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
401300               MOVE '   39' TO HOLD-PROV-CBSA.
401400
401500        IF HOLD-PROV-CBSA = '43580'
401600           AND P-NEW-STATE = 43
401700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
401800               MOVE '   43' TO HOLD-PROV-CBSA.
401900
402000        IF HOLD-PROV-CBSA = '48540'
402100           AND P-NEW-STATE = 36
402200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
402300               MOVE '   36' TO HOLD-PROV-CBSA.
402400
402500        IF HOLD-PROV-CBSA = '48540'
402600           AND P-NEW-STATE = 51
402700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
402800               MOVE '   51' TO HOLD-PROV-CBSA.
402900
403000        IF HOLD-PROV-CBSA = '48864'
403100           AND P-NEW-STATE = 31
403200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
403300               MOVE '   31' TO HOLD-PROV-CBSA.
403400
403500        IF HOLD-PROV-CBSA = '17300'
403600           AND P-NEW-STATE = 18
403700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
403800               MOVE '   18' TO HOLD-PROV-CBSA.
403900
404000        IF HOLD-PROV-CBSA = '17300'
404100           AND P-NEW-STATE = 44
404200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
404300               MOVE '   44' TO HOLD-PROV-CBSA.
404400
404500        IF HOLD-PROV-CBSA = '19060'
404600           AND P-NEW-STATE = 21
404700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
404800               MOVE '   21' TO HOLD-PROV-CBSA.
404900
405000        IF HOLD-PROV-CBSA = '22020'
405100           AND P-NEW-STATE = 24
405200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
405300               MOVE '   24' TO HOLD-PROV-CBSA.
405400
405500        IF HOLD-PROV-CBSA = '22020'
405600           AND P-NEW-STATE = 35
405700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
405800               MOVE '   35' TO HOLD-PROV-CBSA.
405900
406000        IF HOLD-PROV-CBSA = '24220'
406100           AND P-NEW-STATE = 24
406200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
406300               MOVE '   24' TO HOLD-PROV-CBSA.
406400
406500        IF HOLD-PROV-CBSA = '24220'
406600           AND P-NEW-STATE = 35
406700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
406800               MOVE '   35' TO HOLD-PROV-CBSA.
406900
407000        IF HOLD-PROV-CBSA = '30300'
407100           AND P-NEW-STATE = 50
407200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
407300               MOVE '   50' TO HOLD-PROV-CBSA.
407400
407500        IF HOLD-PROV-CBSA = '44600'
407600           AND P-NEW-STATE = 36
407700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
407800               MOVE '   36' TO HOLD-PROV-CBSA.
407900
408000        IF HOLD-PROV-CBSA = '44600'
408100           AND P-NEW-STATE = 51
408200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
408300               MOVE '   51' TO HOLD-PROV-CBSA.
408400
408500        IF HOLD-PROV-CBSA = '45500'
408600           AND P-NEW-STATE = 45
408700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
408800               MOVE '   45' TO HOLD-PROV-CBSA.
408900
409000
409100**************YEARCHANGE 2011.0 **********************      *****
409200
409300 2300-2011-EXIT. EXIT.
409400
409500**************YEARCHANGE 2012.0 **********************      *****
409600
409700 2300-2012-FLOOR-CBSA.
409800
409900**************YEARCHANGE 2012.0 ******************************
410000
410100        IF HOLD-PROV-CBSA = '   30'
410200          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
410300          AND P-NEW-STATE = 30
410400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
410500               MOVE '   30' TO HOLD-PROV-CBSA.
410600
410700        IF HOLD-PROV-CBSA = '   39'
410800          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
410900          AND P-NEW-STATE = 39
411000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
411100               MOVE '   39' TO HOLD-PROV-CBSA.
411200
411300        IF HOLD-PROV-CBSA = '   39'
411400          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
411500          AND P-NEW-STATE = 33
411600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
411700               MOVE '   33' TO HOLD-PROV-CBSA.
411800
411900        IF HOLD-PROV-CBSA = '10900'
412000           AND P-NEW-STATE = 31
412100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
412200               MOVE '   31' TO HOLD-PROV-CBSA.
412300
412400        IF HOLD-PROV-CBSA = '14484'
412500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
412600           AND P-NEW-STATE = 22
412700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
412800               MOVE '   22' TO HOLD-PROV-CBSA.
412900
413000        IF HOLD-PROV-CBSA = '16020'
413100           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
413200           AND P-NEW-STATE = 14
413300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
413400               MOVE '   14' TO HOLD-PROV-CBSA.
413500
413600        IF HOLD-PROV-CBSA = '21500'
413700          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
413800           AND P-NEW-STATE = 33
413900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
414000               MOVE '   33' TO HOLD-PROV-CBSA.
414100
414200        IF HOLD-PROV-CBSA = '21500'
414300          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
414400           AND P-NEW-STATE = 39
414500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
414600               MOVE '   39' TO HOLD-PROV-CBSA.
414700
414800        IF HOLD-PROV-CBSA = '22900'
414900           AND P-NEW-STATE = 37
415000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
415100               MOVE '   37' TO HOLD-PROV-CBSA.
415200
415300        IF HOLD-PROV-CBSA = '25180'
415400           AND P-NEW-STATE = 21
415500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
415600               MOVE '   21' TO HOLD-PROV-CBSA.
415700
415800        IF HOLD-PROV-CBSA = '25540'
415900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
416000           AND P-NEW-STATE = 07
416100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
416200               MOVE '   07' TO HOLD-PROV-CBSA.
416300
416400        IF HOLD-PROV-CBSA = '25540'
416500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
416600           AND P-NEW-STATE = 22
416700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
416800               MOVE '   22' TO HOLD-PROV-CBSA.
416900
417000        IF HOLD-PROV-CBSA = '26820'
417100           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
417200           AND P-NEW-STATE = 53
417300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
417400               MOVE '   53' TO HOLD-PROV-CBSA.
417500
417600        IF HOLD-PROV-CBSA = '28700'
417700           AND P-NEW-STATE = 44
417800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
417900               MOVE '   44' TO HOLD-PROV-CBSA.
418000
418100        IF HOLD-PROV-CBSA = '28700'
418200           AND P-NEW-STATE = 49
418300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
418400               MOVE '   49' TO HOLD-PROV-CBSA.
418500
418600        IF HOLD-PROV-CBSA = '28700'
418700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
418800           AND P-NEW-STATE = 18
418900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
419000               MOVE '   18' TO HOLD-PROV-CBSA.
419100
419200        IF HOLD-PROV-CBSA = '28700'
419300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
419400           AND P-NEW-STATE = 44
419500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
419600               MOVE '   44' TO HOLD-PROV-CBSA.
419700
419800        IF HOLD-PROV-CBSA = '28940'
419900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
420000           AND P-NEW-STATE = 18
420100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
420200               MOVE '   18' TO HOLD-PROV-CBSA.
420300
420400        IF HOLD-PROV-CBSA = '35084'
420500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
420600           AND P-NEW-STATE = 31
420700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
420800               MOVE '   31' TO HOLD-PROV-CBSA.
420900
421000        IF HOLD-PROV-CBSA = '37620'
421100           AND P-NEW-STATE = 36
421200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
421300               MOVE '   36' TO HOLD-PROV-CBSA.
421400
421500        IF HOLD-PROV-CBSA = '37964'
421600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
421700           AND P-NEW-STATE = 31
421800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
421900               MOVE '   31' TO HOLD-PROV-CBSA.
422000
422100        IF HOLD-PROV-CBSA = '43580'
422200           AND P-NEW-STATE = 43
422300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
422400               MOVE '   43' TO HOLD-PROV-CBSA.
422500
422600        IF HOLD-PROV-CBSA = '44600'
422700           AND P-NEW-STATE = 36
422800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
422900               MOVE '   36' TO HOLD-PROV-CBSA.
423000
423100        IF HOLD-PROV-CBSA = '44600'
423200           AND P-NEW-STATE = 51
423300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
423400               MOVE '   51' TO HOLD-PROV-CBSA.
423500
423600        IF HOLD-PROV-CBSA = '48540'
423700           AND P-NEW-STATE = 36
423800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
423900               MOVE '   36' TO HOLD-PROV-CBSA.
424000
424100        IF HOLD-PROV-CBSA = '48540'
424200           AND P-NEW-STATE = 51
424300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
424400               MOVE '   51' TO HOLD-PROV-CBSA.
424500
424600        IF HOLD-PROV-CBSA = '48864'
424700           AND P-NEW-STATE = 31
424800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
424900               MOVE '   31' TO HOLD-PROV-CBSA.
425000
425100        IF HOLD-PROV-CBSA = '49660'
425200           AND P-NEW-STATE = 36
425300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
425400               MOVE '   36' TO HOLD-PROV-CBSA.
425500
425600        IF HOLD-PROV-CBSA = '49660'
425700           AND P-NEW-STATE = 39
425800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
425900               MOVE '   39' TO HOLD-PROV-CBSA.
426000
426100        IF HOLD-PROV-CBSA = '19060'
426200           AND P-NEW-STATE = 21
426300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
426400               MOVE '   21' TO HOLD-PROV-CBSA.
426500
426600        IF HOLD-PROV-CBSA = '22020'
426700           AND P-NEW-STATE = 24
426800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
426900               MOVE '   24' TO HOLD-PROV-CBSA.
427000
427100        IF HOLD-PROV-CBSA = '22020'
427200           AND P-NEW-STATE = 35
427300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
427400               MOVE '   35' TO HOLD-PROV-CBSA.
427500
427600        IF HOLD-PROV-CBSA = '24220'
427700           AND P-NEW-STATE = 24
427800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
427900               MOVE '   24' TO HOLD-PROV-CBSA.
428000
428100        IF HOLD-PROV-CBSA = '24220'
428200           AND P-NEW-STATE = 35
428300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
428400               MOVE '   35' TO HOLD-PROV-CBSA.
428500
428600        IF HOLD-PROV-CBSA = '30300'
428700           AND P-NEW-STATE = 50
428800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
428900               MOVE '   50' TO HOLD-PROV-CBSA.
429000
429100        IF HOLD-PROV-CBSA = '30860'
429200           AND P-NEW-STATE = 46
429300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
429400               MOVE '   46' TO HOLD-PROV-CBSA.
429500
429600        IF HOLD-PROV-CBSA = '35084'
429700           AND P-NEW-STATE = 31
429800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
429900               MOVE '   31' TO HOLD-PROV-CBSA.
430000
430100        IF HOLD-PROV-CBSA = '39300'
430200           AND P-NEW-STATE = 22
430300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
430400               MOVE '   22' TO HOLD-PROV-CBSA.
430500
430600        IF HOLD-PROV-CBSA = '45500'
430700           AND P-NEW-STATE = 45
430800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
430900               MOVE '   45' TO HOLD-PROV-CBSA.
431000
431100**************YEARCHANGE 2012.0 ******************************
431200
431300 2300-2012-EXIT. EXIT.
431400
431500**************YEARCHANGE 2013.0 **********************      *****
431600
431700 2300-2013-FLOOR-CBSA.
431800
431900**************YEARCHANGE 2013.0 ******************************
432000
432100        IF HOLD-PROV-CBSA = '10900'
432200           AND P-NEW-STATE = 31
432300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
432400               MOVE '   31' TO HOLD-PROV-CBSA.
432500
432600        IF HOLD-PROV-CBSA = '14484'
432700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
432800           AND P-NEW-STATE = 22
432900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
433000               MOVE '   22' TO HOLD-PROV-CBSA.
433100
433200        IF HOLD-PROV-CBSA = '16020'
433300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
433400           AND P-NEW-STATE = 14
433500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
433600               MOVE '   14' TO HOLD-PROV-CBSA.
433700
433800        IF HOLD-PROV-CBSA = '21500'
433900          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
434000           AND P-NEW-STATE = 33
434100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
434200               MOVE '   33' TO HOLD-PROV-CBSA.
434300
434400        IF HOLD-PROV-CBSA = '21500'
434500          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
434600           AND P-NEW-STATE = 39
434700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
434800               MOVE '   39' TO HOLD-PROV-CBSA.
434900
435000        IF HOLD-PROV-CBSA = '21780'
435100          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
435200           AND P-NEW-STATE = 15
435300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
435400               MOVE '   15' TO HOLD-PROV-CBSA.
435500
435600        IF HOLD-PROV-CBSA = '24580'
435700          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
435800           AND P-NEW-STATE = 52
435900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
436000               MOVE '   52' TO HOLD-PROV-CBSA.
436100
436200        IF HOLD-PROV-CBSA = '25540'
436300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
436400           AND P-NEW-STATE = 07
436500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
436600               MOVE '   07' TO HOLD-PROV-CBSA.
436700
436800        IF HOLD-PROV-CBSA = '25540'
436900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
437000           AND P-NEW-STATE = 22
437100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
437200               MOVE '   22' TO HOLD-PROV-CBSA.
437300
437400        IF HOLD-PROV-CBSA = '26820'
437500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
437600           AND P-NEW-STATE = 53
437700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
437800               MOVE '   53' TO HOLD-PROV-CBSA.
437900
438000        IF HOLD-PROV-CBSA = '27900'
438100           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
438200           AND P-NEW-STATE = 17
438300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
438400               MOVE '   17' TO HOLD-PROV-CBSA.
438500
438600        IF HOLD-PROV-CBSA = '28700'
438700           AND P-NEW-STATE = 44
438800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
438900               MOVE '   44' TO HOLD-PROV-CBSA.
439000
439100        IF HOLD-PROV-CBSA = '28700'
439200           AND P-NEW-STATE = 49
439300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
439400               MOVE '   49' TO HOLD-PROV-CBSA.
439500
439600        IF HOLD-PROV-CBSA = '28700'
439700           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
439800           AND P-NEW-STATE = 18
439900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
440000               MOVE '   18' TO HOLD-PROV-CBSA.
440100
440200        IF HOLD-PROV-CBSA = '28700'
440300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
440400           AND P-NEW-STATE = 44
440500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
440600               MOVE '   44' TO HOLD-PROV-CBSA.
440700
440800        IF HOLD-PROV-CBSA = '28940'
440900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
441000           AND P-NEW-STATE = 18
441100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
441200               MOVE '   18' TO HOLD-PROV-CBSA.
441300
441400        IF HOLD-PROV-CBSA = '35084'
441500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
441600           AND P-NEW-STATE = 31
441700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
441800               MOVE '   31' TO HOLD-PROV-CBSA.
441900
442000        IF HOLD-PROV-CBSA = '37620'
442100           AND P-NEW-STATE = 36
442200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
442300               MOVE '   36' TO HOLD-PROV-CBSA.
442400
442500        IF HOLD-PROV-CBSA = '37964'
442600           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
442700           AND P-NEW-STATE = 31
442800               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
442900               MOVE '   31' TO HOLD-PROV-CBSA.
443000
443100        IF HOLD-PROV-CBSA = '38300'
443200           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
443300           AND P-NEW-STATE = 36
443400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
443500               MOVE '   36' TO HOLD-PROV-CBSA.
443600
443700        IF HOLD-PROV-CBSA = '43580'
443800           AND P-NEW-STATE = 43
443900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
444000               MOVE '   43' TO HOLD-PROV-CBSA.
444100
444200        IF HOLD-PROV-CBSA = '48540'
444300           AND P-NEW-STATE = 36
444400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
444500               MOVE '   36' TO HOLD-PROV-CBSA.
444600
444700        IF HOLD-PROV-CBSA = '48540'
444800           AND P-NEW-STATE = 51
444900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
445000               MOVE '   51' TO HOLD-PROV-CBSA.
445100
445200        IF HOLD-PROV-CBSA = '48864'
445300           AND P-NEW-STATE = 31
445400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
445500               MOVE '   31' TO HOLD-PROV-CBSA.
445600
445700        IF HOLD-PROV-CBSA = '49660'
445800           AND P-NEW-STATE = 36
445900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
446000               MOVE '   36' TO HOLD-PROV-CBSA.
446100
446200        IF HOLD-PROV-CBSA = '49660'
446300           AND P-NEW-STATE = 39
446400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
446500               MOVE '   39' TO HOLD-PROV-CBSA.
446600
446700        IF HOLD-PROV-CBSA = '22020'
446800           AND P-NEW-STATE = 24
446900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
447000               MOVE '   24' TO HOLD-PROV-CBSA.
447100
447200        IF HOLD-PROV-CBSA = '22020'
447300           AND P-NEW-STATE = 35
447400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
447500               MOVE '   35' TO HOLD-PROV-CBSA.
447600
447700        IF HOLD-PROV-CBSA = '24220'
447800           AND P-NEW-STATE = 24
447900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
448000               MOVE '   24' TO HOLD-PROV-CBSA.
448100
448200        IF HOLD-PROV-CBSA = '24220'
448300           AND P-NEW-STATE = 35
448400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
448500               MOVE '   35' TO HOLD-PROV-CBSA.
448600
448700        IF HOLD-PROV-CBSA = '30300'
448800           AND P-NEW-STATE = 50
448900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
449000               MOVE '   50' TO HOLD-PROV-CBSA.
449100
449200        IF HOLD-PROV-CBSA = '39300'
449300           AND P-NEW-STATE = 22
449400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
449500               MOVE '   22' TO HOLD-PROV-CBSA.
449600
449700        IF HOLD-PROV-CBSA = '39300'
449800           AND P-NEW-STATE = 41
449900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
450000               MOVE '   41' TO HOLD-PROV-CBSA.
450100
450200        IF HOLD-PROV-CBSA = '44600'
450300           AND P-NEW-STATE = 36
450400               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
450500               MOVE '   36' TO HOLD-PROV-CBSA.
450600
450700
450800**************YEARCHANGE 2013.0 ******************************
450900
451000 2300-2013-EXIT. EXIT.
451100
451200
451300 2300-2014-FLOOR-CBSA.
451400
451500**************YEARCHANGE 2014.0 ******************************
451600
451700        IF HOLD-PROV-CBSA = '   07'
451800           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
451900           AND P-NEW-STATE = 07
452000               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
452100               MOVE '   07' TO HOLD-PROV-CBSA.
452200
452300        IF HOLD-PROV-CBSA = '   36'
452400           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
452500           AND P-NEW-STATE = 36
452600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
452700               MOVE '   36' TO HOLD-PROV-CBSA.
452800
452900        IF HOLD-PROV-CBSA = '10900'
453000           AND P-NEW-STATE = 31
453100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
453200               MOVE '   31' TO HOLD-PROV-CBSA.
453300
453400        IF HOLD-PROV-CBSA = '14484'
453500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
453600           AND P-NEW-STATE = 22
453700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
453800               MOVE '   22' TO HOLD-PROV-CBSA.
453900
454000        IF HOLD-PROV-CBSA = '17300'
454100           AND P-NEW-STATE = 18
454200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
454300               MOVE '   18' TO HOLD-PROV-CBSA.
454400
454500        IF HOLD-PROV-CBSA = '22900'
454600           AND P-NEW-STATE = 37
454700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
454800               MOVE '   37' TO HOLD-PROV-CBSA.
454900
455000        IF HOLD-PROV-CBSA = '25540'
455100          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
455200           AND P-NEW-STATE = 07
455300               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
455400               MOVE '   07' TO HOLD-PROV-CBSA.
455500
455600        IF HOLD-PROV-CBSA = '25540'
455700          AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
455800           AND P-NEW-STATE = 22
455900               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
456000               MOVE '   22' TO HOLD-PROV-CBSA.
456100
456200        IF HOLD-PROV-CBSA = '26820'
456300           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
456400           AND P-NEW-STATE = 53
456500               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
456600               MOVE '   53' TO HOLD-PROV-CBSA.
456700
456800        IF HOLD-PROV-CBSA = '27180'
456900           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
457000           AND P-NEW-STATE = 25
457100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
457200               MOVE '   25' TO HOLD-PROV-CBSA.
457300
457400        IF HOLD-PROV-CBSA = '28700'
457500           AND P-NEW-STATE = 44
457600               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
457700               MOVE '   44' TO HOLD-PROV-CBSA.
457800
457900        IF HOLD-PROV-CBSA = '28700'
458000           AND P-NEW-STATE = 49
458100               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
458200               MOVE '   49' TO HOLD-PROV-CBSA.
458300
458400        IF HOLD-PROV-CBSA = '35644'
458500           AND P-NEW-CBSA-SPEC-PAY-IND  = 'Y'
458600           AND P-NEW-STATE = 07
458700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
458800               MOVE '   07' TO HOLD-PROV-CBSA.
458900
459000        IF HOLD-PROV-CBSA = '37620'
459100           AND P-NEW-STATE = 36
459200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
459300               MOVE '   36' TO HOLD-PROV-CBSA.
459400
459500        IF HOLD-PROV-CBSA = '43580'
459600           AND P-NEW-STATE = 43
459700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
459800               MOVE '   43' TO HOLD-PROV-CBSA.
459900
460000        IF HOLD-PROV-CBSA = '48540'
460100           AND P-NEW-STATE = 36
460200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
460300               MOVE '   36' TO HOLD-PROV-CBSA.
460400
460500        IF HOLD-PROV-CBSA = '48540'
460600           AND P-NEW-STATE = 51
460700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
460800               MOVE '   51' TO HOLD-PROV-CBSA.
460900
461000        IF HOLD-PROV-CBSA = '48864'
461100           AND P-NEW-STATE = 31
461200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
461300               MOVE '   31' TO HOLD-PROV-CBSA.
461400
461500        IF HOLD-PROV-CBSA = '49660'
461600           AND P-NEW-STATE = 36
461700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
461800               MOVE '   36' TO HOLD-PROV-CBSA.
461900
462000        IF HOLD-PROV-CBSA = '49660'
462100           AND P-NEW-STATE = 39
462200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
462300               MOVE '   39' TO HOLD-PROV-CBSA.
462400
462500        IF HOLD-PROV-CBSA = '19060'
462600           AND P-NEW-STATE = 21
462700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
462800               MOVE '   21' TO HOLD-PROV-CBSA.
462900
463000        IF HOLD-PROV-CBSA = '22020'
463100           AND P-NEW-STATE = 24
463200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
463300               MOVE '   24' TO HOLD-PROV-CBSA.
463400
463500        IF HOLD-PROV-CBSA = '22020'
463600           AND P-NEW-STATE = 35
463700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
463800               MOVE '   35' TO HOLD-PROV-CBSA.
463900
464000        IF HOLD-PROV-CBSA = '24220'
464100           AND P-NEW-STATE = 24
464200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
464300               MOVE '   24' TO HOLD-PROV-CBSA.
464400
464500        IF HOLD-PROV-CBSA = '24220'
464600           AND P-NEW-STATE = 35
464700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
464800               MOVE '   35' TO HOLD-PROV-CBSA.
464900
465000        IF HOLD-PROV-CBSA = '30300'
465100           AND P-NEW-STATE = 50
465200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
465300               MOVE '   50' TO HOLD-PROV-CBSA.
465400
465500        IF HOLD-PROV-CBSA = '39300'
465600           AND P-NEW-STATE = 22
465700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
465800               MOVE '   22' TO HOLD-PROV-CBSA.
465900
466000        IF HOLD-PROV-CBSA = '39300'
466100           AND P-NEW-STATE = 41
466200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
466300               MOVE '   41' TO HOLD-PROV-CBSA.
466400
466500        IF HOLD-PROV-CBSA = '44600'
466600           AND P-NEW-STATE = 36
466700               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
466800               MOVE '   36' TO HOLD-PROV-CBSA.
466900
467000        IF HOLD-PROV-CBSA = '45500'
467100           AND P-NEW-STATE = 45
467200               MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
467300               MOVE '   45' TO HOLD-PROV-CBSA.
467400
467500
467600**************YEARCHANGE 2014.0 ******************************
467700
467800 2300-2014-EXIT. EXIT.
467900
468000**************YEARCHANGE 2015.0 ******************************
468100 2300-2015-FWD-FLOOR-CBSA.
468200
468300**----------------------------------------------------------------
468400** ENSURE THE CBSA WAGE INDEX IS A VALID VALUE, ELSE SET ERROR RTC
468500**----------------------------------------------------------------
468600     IF W-NEW-CBSA-WI NOT NUMERIC
468700        MOVE 0 TO W-NEW-CBSA-WI.
468800
468900     IF W-NEW-CBSA-WI = 00.0000
469000        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
469100        MOVE 52 TO PPS-RTC
469200        GO TO 2300-2015-EXIT.
469300
469400**----------------------------------------------------------------
469500** SET THE PROVIDER'S STATE RURAL CBSA
469600**----------------------------------------------------------------
469700     MOVE '   ' TO  H-CBSA-RURAL-BLANK.
469800     MOVE P-NEW-STATE-CODE-X TO H-CBSA-RURAL-STATE.
469900
470000     IF H-CBSA-RURAL-STATE = '00'
470100        MOVE '03' TO H-CBSA-RURAL-STATE.
470200
470300*------------------------------------------------------------*
470400* SEARCH TABLE FOR RURAL IPPS CBSA & GET WAGE INDEX (FLOOR)  *
470500*------------------------------------------------------------*
470600     PERFORM 0175-GET-RURAL-CBSA THRU 0175-EXIT.
470700
470800     IF PPS-RTC = 00
470900      IF W-RURAL-CBSA-EFF-DATE NOT = WS-9S
471000       IF B-21-DISCHARGE-DATE > 20200930
471100        IF H-CBSA-PROV-BLANK = '   '
471200          GO TO 0690-BYPASS
471300        ELSE
471400         PERFORM 0670-GET-RURAL-CBSA-WAGE-INDX
471500          THRU   0670-EXIT VARYING MA2
471600                 FROM MA1 BY 1 UNTIL
471700                 T-CBSA (MA2) NOT = HOLD-RURAL-CBSA
471800         GO TO 0690-BYPASS
471900        END-IF
472000       END-IF
472100      END-IF
472200     END-IF.
472300
472400     IF PPS-RTC = 00
472500      IF W-RURAL-CBSA-EFF-DATE NOT = WS-9S
472600       IF B-21-DISCHARGE-DATE > 20190930 AND
472700          B-21-DISCHARGE-DATE < 20201001
472800        IF H-CBSA-PROV-BLANK = '   '
472900          GO TO 0690-BYPASS
473000        ELSE
473100          PERFORM 0690-GET-RURAL-FLOOR-WAGE-INDX THRU 0690-EXIT
473200          GO TO 0690-BYPASS
473300        END-IF
473400       END-IF
473500      END-IF
473600     END-IF.
473700
473800     IF PPS-RTC = 00
473900       IF W-RURAL-CBSA-EFF-DATE NOT = WS-9S
474000         PERFORM 0660-GET-RURAL-CBSA-WAGE-INDX
474100          THRU   0660-EXIT VARYING MA2
474200                 FROM MA1 BY 1 UNTIL
474300                 T-CBSA (MA2) NOT = HOLD-RURAL-CBSA
474400       END-IF
474500     END-IF.
474600
474700 0690-BYPASS.
474800
474900*    IF W-NEW-CBSA-WI NOT NUMERIC
475000*       MOVE 0 TO W-NEW-CBSA-WI.
475100
475200*------------------------------------------------------------*
475300* IF NO RURAL WAGE INDEX FOUND, SET TO ZERO (VALID BECAUSE   *
475400* SOME STATES DO NOT HAVE A RURAL AREA)                      *
475500*------------------------------------------------------------*
475600     IF W-RURAL-CBSA-WI NOT NUMERIC
475700        MOVE 0 TO W-RURAL-CBSA-WI.
475800
475900*------------------------------------------------------------*
476000* IF THE STATE'S RURAL FLOOR WAGE INDEX IS HIGHER THAN THE   *
476100* PROVIDER'S CBSA WAGE INDEX, REPLACE THE CBSA AND WAGE      *
476200* INDEX WITH STATE CODE AND RURAL FLOOR WAGE INDEX           *
476300*------------------------------------------------------------*
476400     IF W-RURAL-CBSA-WI > W-NEW-CBSA-WI
476500        MOVE WAGE-RURAL-CBSA-INDEX-RECORD TO
476600                   WAGE-NEW-CBSA-INDEX-RECORD
476700        MOVE 'N' TO P-NEW-CBSA-SPEC-PAY-IND
476800        MOVE HOLD-RURAL-CBSA TO HOLD-PROV-CBSA.
476900
477000 2300-2015-EXIT. EXIT.
477100
477200**************YEARCHANGE 2015.0 ******************************
477300 2350-2015-FWD-FLOOR-CBSA-PR.
477400
477500**----------------------------------------------------------------
477600** ENSURE PR CBSA WAGE INDEX IS A VALID VALUE, ELSE SET ERROR RTC
477700**----------------------------------------------------------------
477800     IF W-NEW-CBSA-PR-WI NOT NUMERIC
477900        MOVE 0 TO W-NEW-CBSA-WI.
478000
478100     IF W-NEW-CBSA-PR-WI = 00.0000
478200        MOVE ALL '0' TO PPS-ADDITIONAL-VARIABLES
478300        MOVE 52 TO PPS-RTC
478400        GO TO 2350-2015-EXIT.
478500
478600**----------------------------------------------------------------
478700** SET THE PROVIDER'S STATE RURAL CBSA TO 40 (PUERTO RICO)
478800**----------------------------------------------------------------
478900     MOVE '   ' TO H-CBSA-RURAL-BLANK.
479000     MOVE '40'  TO H-CBSA-RURAL-STATE.
479100
479200*------------------------------------------------------------*
479300* SEARCH TABLE FOR RURAL PR CBSA & GET WAGE INDEX (FLOOR)    *
479400*------------------------------------------------------------*
479500     PERFORM 0260-N-GET-RURAL-CBSA-PR THRU 0260-EXIT.
479600
479700*------------------------------------------------------------*
479800* IF NO PR RURAL WAGE INDEX FOUND, SET TO ZERO (VALID FOR    *
479900* ANY YEAR WITH NO PUERTO RICO RURAL AREA)                   *
480000*------------------------------------------------------------*
480100     IF W-RURAL-CBSA-PR-WI NOT NUMERIC
480200        MOVE 0 TO W-RURAL-CBSA-PR-WI.
480300
480400*------------------------------------------------------------*
480500* IF THE STATE'S RURAL FLOOR WAGE INDEX IS HIGHER THAN THE   *
480600* PROVIDER'S CBSA WAGE INDEX, REPLACE THE CBSA AND WAGE      *
480700* INDEX WITH STATE CODE AND RURAL FLOOR WAGE INDEX           *
480800*------------------------------------------------------------*
480900     IF W-RURAL-CBSA-PR-WI > W-NEW-CBSA-PR-WI
481000        MOVE W-RURAL-CBSA-PR-WI TO W-NEW-CBSA-PR-WI.
481100
481200 2350-2015-EXIT. EXIT.
481300
481400**************YEARCHANGE 2015.0 ******************************
481500 2400-CONVERT-PSF.
481600***************************************************************
481700*              CONVERSION FOR MILLENNIUM                      *
481800*    CONVERTS TO PSF FROM NEW FORMAT TO OLD FORMAT            *
481900*    THIS OLD PSF FORMAT IS PROCESSED IN NON-MILLENNIUN       *
482000*    PPS PROGRAMS                                             *
482100***************************************************************
482200     MOVE SPACES TO W-PROV-OLD-HOLD.
482300     MOVE W-P-NEW-PROVIDER-OSCAR-NO TO W-P-PROVIDER-NO.
482400
482500     IF W-P-NEW-EFF-DATE < 0 OR
482600        W-P-NEW-EFF-DATE = '00000000'
482700        MOVE ZEROES TO W-P-EFF-DATE
482800     ELSE
482900        MOVE  W-P-NEW-EFF-DT-YY        TO W-P-EFF-YY
483000        MOVE  W-P-NEW-EFF-DT-MM        TO W-P-EFF-MM
483100        MOVE  W-P-NEW-EFF-DT-DD        TO W-P-EFF-DD.
483200     MOVE  W-P-NEW-WAIVER-CODE   TO W-P-WAIVER-CODE.
483300     MOVE  W-P-NEW-PROVIDER-TYPE TO W-P-PROVIDER-TYPE.
483400     MOVE  W-P-NEW-CURRENT-CENSUS-DIV
483500                               TO W-P-CURRENT-CENSUS-DIV.
483600     MOVE  W-P-NEW-GEO-LOC-MSA   TO W-P-MSA-X.
483700     MOVE  W-P-NEW-FAC-SPEC-RATE TO W-P-FAC-SPEC-RATE.
483800     MOVE  W-P-NEW-COLA          TO W-P-COLA.
483900     MOVE  W-P-NEW-INTERN-RATIO  TO W-P-INTERN-RATIO.
484000     MOVE  W-P-NEW-BED-SIZE      TO W-P-BED-SIZE.
484100     MOVE  W-P-NEW-CCR           TO W-P-CCR.
484200     MOVE  W-P-NEW-CMI           TO W-P-CMI.
484300
484400     IF W-P-NEW-REPORT-DATE < 0 OR
484500        W-P-NEW-REPORT-DATE = '00000000'
484600        MOVE ZEROES TO W-P-REPORT-DATE
484700     ELSE
484800        MOVE  W-P-NEW-REPORT-DT-YY  TO W-P-REPORT-DT-YY
484900        MOVE  W-P-NEW-REPORT-DT-MM  TO W-P-REPORT-DT-MM
485000        MOVE  W-P-NEW-REPORT-DT-DD  TO W-P-REPORT-DT-DD.
485100
485200     MOVE  W-P-NEW-INTER-NO      TO W-P-INTER-NO.
485300
485400     IF W-P-NEW-FY-BEGIN-DATE < 0  OR
485500        W-P-NEW-FY-BEGIN-DATE = '00000000'
485600        MOVE ZEROES TO W-P-FY-BEGIN-DATE
485700     ELSE
485800        MOVE  W-P-NEW-FY-BEG-DT-YY  TO W-P-FY-BEG-DT-YY
485900        MOVE  W-P-NEW-FY-BEG-DT-MM  TO W-P-FY-BEG-DT-MM
486000        MOVE  W-P-NEW-FY-BEG-DT-DD  TO W-P-FY-BEG-DT-DD.
486100
486200     MOVE W-P-NEW-PASS-AMT-CAPITAL    TO W-P-PASS-AMT-CAPITAL.
486300     MOVE W-P-NEW-PASS-AMT-DIR-MED-ED TO W-P-PASS-AMT-DIR-MED-ED.
486400     MOVE W-P-NEW-PASS-AMT-ORGAN-ACQ  TO W-P-PASS-AMT-ORGAN-ACQ.
486500     MOVE W-P-NEW-PASS-AMT-PLUS-MISC  TO W-P-PASS-AMT-PLUS-MISC.
486600     MOVE W-P-NEW-SSI-RATIO           TO W-P-SSI-RATIO.
486700     MOVE W-P-NEW-MEDICAID-RATIO      TO W-P-MEDICAID-RATIO.
486800
486900     IF W-P-NEW-TERMINATION-DATE < 0  OR
487000        W-P-NEW-TERMINATION-DATE = '00000000'
487100        MOVE ZEROES TO W-P-TERMINATION-DATE
487200     ELSE
487300        MOVE  W-P-NEW-TERM-DT-YY    TO W-P-TERM-DT-YY
487400        MOVE  W-P-NEW-TERM-DT-MM    TO W-P-TERM-DT-MM
487500        MOVE  W-P-NEW-TERM-DT-DD    TO W-P-TERM-DT-DD.
487600
487700     IF P-NEW-FYE-DATE  NOT NUMERIC
487800        MOVE ZEROES TO W-P-FISCAL-YEAR-END.
487900
488000     IF P-NEW-FYE-DATE               < 0  OR
488100        P-NEW-FYE-DATE               = '00000000'
488200        MOVE ZEROES TO W-P-FISCAL-YEAR-END
488300     ELSE
488400        MOVE  W-P-NEW-FYE-YY       TO W-P-YY
488500        MOVE  W-P-NEW-FYE-MM       TO W-P-MM
488600        MOVE  W-P-NEW-FYE-DD       TO W-P-DD.
488700
488800     MOVE W-P-NEW-WAGE-INDEX-LOC-MSA  TO W-P-WAGE-INDEX-LOC-MSA.
488900     MOVE W-P-NEW-CHG-CODE-INDEX      TO W-P-CHG-CODE-INDEX.
489000     MOVE W-P-NEW-STAND-AMT-LOC-MSA   TO W-P-STAND-AMT-LOC-MSA.
489100     MOVE W-P-NEW-SOL-COM-DEP-HOSP-YR TO W-P-SOL-COM-DEP-HOSP-YR.
489200     MOVE W-P-NEW-LUGAR               TO W-P-LUGAR.
489300     MOVE W-P-NEW-TEMP-RELIEF-IND     TO W-P-TEMP-RELIEF-IND.
489400     MOVE W-P-NEW-CAPI-PPS-PAY-CODE   TO W-P-CAPI-PPS-PAY-CODE.
489500     MOVE W-P-NEW-CAPI-HOSP-SPEC-RATE TO W-P-CAPI-HOSP-SPEC-RATE.
489600     MOVE W-P-NEW-CAPI-OLD-HARM-RATE  TO W-P-CAPI-OLD-HARM-RATE.
489700     MOVE W-P-NEW-CAPI-NEW-HARM-RATIO TO W-P-CAPI-NEW-HARM-RATIO.
489800     MOVE W-P-NEW-CAPI-CSTCHG-RATIO   TO W-P-CAPI-CSTCHG-RATIO.
489900     MOVE W-P-NEW-CAPI-NEW-HOSP       TO W-P-CAPI-NEW-HOSP.
490000     MOVE W-P-NEW-CAPI-IME            TO W-P-CAPI-IME.
490100     MOVE W-P-NEW-CAPI-EXCEPTIONS     TO W-P-CAPI-EXCEPTIONS.
490200 2400-EXIT.   EXIT.
490300
490400 2500-2000-WI-LUGAR.
490500***************************************************************
490600****    FOR FY 2000 NEW LUGAR HOSPITALS ONLY                  *
490700***************************************************************
490800
490900     IF (P-NEW-PROVIDER-NO = '140012' OR '150002' OR '150004' OR
491000                             '150008' OR '150034' OR '150090' OR
491100                             '150125' OR '150126' OR '150132')
491200            AND (P-NEW-STAND-AMT-LOC-MSA = '1600'
491300            AND P-NEW-WAGE-INDEX-LOC-MSA = '1600'
491400            AND P-NEW-CHG-CODE-INDEX  = 'Y')
491500          MOVE 01.0750 TO W-NEW-INDEX-RECORD.
491600
491700     IF (P-NEW-PROVIDER-NO = '250078')
491800            AND (P-NEW-STAND-AMT-LOC-MSA = '3285'
491900            AND P-NEW-WAGE-INDEX-LOC-MSA = '3285'
492000            AND P-NEW-CHG-CODE-INDEX  = 'Y')
492100          MOVE 00.7634 TO W-NEW-INDEX-RECORD.
492200
492300     IF (P-NEW-PROVIDER-NO = '330001' OR '330126' OR '330135' OR
492400                             '330205' OR '330209' OR '330264')
492500            AND (P-NEW-STAND-AMT-LOC-MSA = '5600'
492600            AND P-NEW-WAGE-INDEX-LOC-MSA = '5600'
492700            AND P-NEW-CHG-CODE-INDEX  = 'Y')
492800          MOVE 01.4342 TO W-NEW-INDEX-RECORD.
492900
493000     IF (P-NEW-PROVIDER-NO = '340039' OR '340129' OR '340144')
493100            AND (P-NEW-STAND-AMT-LOC-MSA = '1520'
493200            AND P-NEW-WAGE-INDEX-LOC-MSA = '1520'
493300            AND P-NEW-CHG-CODE-INDEX  = 'Y')
493400          MOVE 00.9434 TO W-NEW-INDEX-RECORD.
493500
493600     IF (P-NEW-PROVIDER-NO = '360046' OR '360056' OR '360076' OR
493700                             '360132')
493800            AND (P-NEW-STAND-AMT-LOC-MSA = '1640'
493900            AND P-NEW-WAGE-INDEX-LOC-MSA = '1640'
494000            AND P-NEW-CHG-CODE-INDEX  = 'Y')
494100          MOVE 00.9419 TO W-NEW-INDEX-RECORD.
494200
494300     IF (P-NEW-PROVIDER-NO = '390019' OR '390049' OR '390162' OR
494400                             '390194' OR '390197' OR '390263')
494500            AND (P-NEW-STAND-AMT-LOC-MSA = '0240'
494600            AND P-NEW-WAGE-INDEX-LOC-MSA = '0240'
494700            AND P-NEW-CHG-CODE-INDEX  = 'Y')
494800          MOVE 01.0228 TO W-NEW-INDEX-RECORD.
494900
495000     IF (P-NEW-PROVIDER-NO = '450065' OR '450072' OR '450591')
495100            AND (P-NEW-STAND-AMT-LOC-MSA = '3360'
495200            AND P-NEW-WAGE-INDEX-LOC-MSA = '3360'
495300            AND P-NEW-CHG-CODE-INDEX  = 'Y')
495400          MOVE 00.9388 TO W-NEW-INDEX-RECORD.
495500
495600     IF (P-NEW-PROVIDER-NO = '470003')
495700            AND (P-NEW-STAND-AMT-LOC-MSA = '1123'
495800            AND P-NEW-WAGE-INDEX-LOC-MSA = '1123'
495900            AND P-NEW-CHG-CODE-INDEX  = 'Y')
496000          MOVE 01.1359 TO W-NEW-INDEX-RECORD.
496100
496200 2500-2000-EXIT.   EXIT.
496300
496400 2500-2001-WI-LUGAR.
496500***************************************************************
496600****    FOR FY 2001 NEW LUGAR HOSPITALS ONLY                  *
496700***************************************************************
496800     IF (P-NEW-PROVIDER-NO = '010043')
496900            AND (P-NEW-STAND-AMT-LOC-MSA = '1000'
497000            AND P-NEW-WAGE-INDEX-LOC-MSA = '1000'
497100            AND P-NEW-CHG-CODE-INDEX  = 'Y')
497200          MOVE 00.8490 TO W-NEW-INDEX-RECORD.
497300
497400     IF (P-NEW-PROVIDER-NO = '010072' OR '010101')
497500            AND (P-NEW-STAND-AMT-LOC-MSA = '0450'
497600            AND P-NEW-WAGE-INDEX-LOC-MSA = '0450'
497700            AND P-NEW-CHG-CODE-INDEX  = 'Y')
497800          MOVE 00.7871 TO W-NEW-INDEX-RECORD.
497900
498000     IF (P-NEW-PROVIDER-NO = '100098')
498100            AND (P-NEW-STAND-AMT-LOC-MSA = '8960'
498200            AND P-NEW-WAGE-INDEX-LOC-MSA = '8960'
498300            AND P-NEW-CHG-CODE-INDEX  = 'Y')
498400          MOVE 00.9615 TO W-NEW-INDEX-RECORD.
498500
498600     IF (P-NEW-PROVIDER-NO = '100232')
498700            AND (P-NEW-STAND-AMT-LOC-MSA = '2900'
498800            AND P-NEW-WAGE-INDEX-LOC-MSA = '2900'
498900            AND P-NEW-CHG-CODE-INDEX  = 'Y')
499000          MOVE 01.0074 TO W-NEW-INDEX-RECORD.
499100
499200     IF (P-NEW-PROVIDER-NO = '110130')
499300            AND (P-NEW-STAND-AMT-LOC-MSA = '0500'
499400            AND P-NEW-WAGE-INDEX-LOC-MSA = '0500'
499500            AND P-NEW-CHG-CODE-INDEX  = 'Y')
499600          MOVE 00.9739 TO W-NEW-INDEX-RECORD.
499700
499800     IF (P-NEW-PROVIDER-NO = '140230')
499900            AND (P-NEW-STAND-AMT-LOC-MSA = '1400'
500000            AND P-NEW-WAGE-INDEX-LOC-MSA = '1400'
500100            AND P-NEW-CHG-CODE-INDEX  = 'Y')
500200          MOVE 00.9069 TO W-NEW-INDEX-RECORD.
500300
500400     IF (P-NEW-PROVIDER-NO = '230027')
500500            AND (P-NEW-STAND-AMT-LOC-MSA = '3000'
500600            AND P-NEW-WAGE-INDEX-LOC-MSA = '3000'
500700            AND P-NEW-CHG-CODE-INDEX  = 'Y')
500800          MOVE 01.0119 TO W-NEW-INDEX-RECORD.
500900
501000     IF (P-NEW-PROVIDER-NO = '340071' OR '340124')
501100            AND (P-NEW-STAND-AMT-LOC-MSA = '6640'
501200            AND P-NEW-WAGE-INDEX-LOC-MSA = '6640'
501300            AND P-NEW-CHG-CODE-INDEX  = 'Y')
501400          MOVE 00.9506 TO W-NEW-INDEX-RECORD.
501500
501600     IF (P-NEW-PROVIDER-NO = '390030' OR '390181' OR '390183')
501700            AND (P-NEW-STAND-AMT-LOC-MSA = '6680'
501800            AND P-NEW-WAGE-INDEX-LOC-MSA = '6680'
501900            AND P-NEW-CHG-CODE-INDEX  = 'Y')
502000          MOVE 00.8992 TO W-NEW-INDEX-RECORD.
502100
502200     IF (P-NEW-PROVIDER-NO = '390201')
502300            AND (P-NEW-STAND-AMT-LOC-MSA = '5640'
502400            AND P-NEW-WAGE-INDEX-LOC-MSA = '5640'
502500            AND P-NEW-CHG-CODE-INDEX  = 'Y')
502600          MOVE 01.0890 TO W-NEW-INDEX-RECORD.
502700
502800 2500-2001-EXIT.   EXIT.
502900
503000 2500-2003-WI-LUGAR.
503100***************************************************************
503200****    FOR FY 2003 NEW LUGAR HOSPITALS ONLY                  *
503300***************************************************************
503400     IF (P-NEW-PROVIDER-NO = '110130')
503500            AND (P-NEW-STAND-AMT-LOC-MSA = '  11'
503600            AND P-NEW-WAGE-INDEX-LOC-MSA = '  11'
503700            AND P-NEW-CHG-CODE-INDEX  = 'Y')
503800          MOVE 00.8230 TO W-NEW-INDEX-RECORD.
503900
504000
504100 2500-2003-EXIT.   EXIT.
504200
504300 2700-2002-WI-401-HOSPITAL.
504400***************************************************************
504500****    FOR FY 2002 SECTION 401 HOSPITALS                     *
504600***************************************************************
504700     IF (P-NEW-PROVIDER-NO = '050192' OR '050286' OR
504800                             '050446' OR '050469' OR
504900                             '050528' OR '050542')
505000         MOVE '  05' TO HOLD-PROV-MSAX
505100                        P-NEW-STAND-AMT-LOC-MSA.
505200
505300     IF (P-NEW-PROVIDER-NO = '100048' OR '100118')
505400         MOVE '  10' TO HOLD-PROV-MSAX
505500                        P-NEW-STAND-AMT-LOC-MSA.
505600
505700     IF (P-NEW-PROVIDER-NO = '170137')
505800         MOVE '  17' TO HOLD-PROV-MSAX
505900                        P-NEW-STAND-AMT-LOC-MSA.
506000
506100     IF (P-NEW-PROVIDER-NO = '190048' OR '190110')
506200         MOVE '  19' TO HOLD-PROV-MSAX
506300                        P-NEW-STAND-AMT-LOC-MSA.
506400
506500     IF (P-NEW-PROVIDER-NO = '230078')
506600         MOVE '  23' TO HOLD-PROV-MSAX
506700                        P-NEW-STAND-AMT-LOC-MSA.
506800
506900     IF (P-NEW-PROVIDER-NO = '260006')
507000         MOVE '  26' TO HOLD-PROV-MSAX
507100                        P-NEW-STAND-AMT-LOC-MSA.
507200
507300     IF (P-NEW-PROVIDER-NO = '290038')
507400         MOVE '  29' TO HOLD-PROV-MSAX
507500                        P-NEW-STAND-AMT-LOC-MSA.
507600
507700     IF (P-NEW-PROVIDER-NO = '300009')
507800         MOVE '  30' TO HOLD-PROV-MSAX
507900                        P-NEW-STAND-AMT-LOC-MSA.
508000
508100     IF (P-NEW-PROVIDER-NO = '390106')
508200         MOVE '  39' TO HOLD-PROV-MSAX
508300                        P-NEW-STAND-AMT-LOC-MSA.
508400
508500     IF (P-NEW-PROVIDER-NO = '520007' OR '520153')
508600         MOVE '  52' TO HOLD-PROV-MSAX
508700                        P-NEW-STAND-AMT-LOC-MSA.
508800
508900 2700-2002-EXIT.   EXIT.
509000
509100 2700-2003-WI-401-HOSPITAL.
509200***************************************************************
509300****    FOR FY 2003 SECTION 401 HOSPITALS                     *
509400***************************************************************
509500     IF (P-NEW-PROVIDER-NO = '050192' OR '050286' OR
509600                             '050446' OR '050469' OR
509700                             '050528')
509800         MOVE '  05' TO HOLD-PROV-MSAX
509900                        P-NEW-STAND-AMT-LOC-MSA.
510000
510100     IF (P-NEW-PROVIDER-NO = '100048' OR '100118')
510200         MOVE '  10' TO HOLD-PROV-MSAX
510300                        P-NEW-STAND-AMT-LOC-MSA.
510400
510500     IF (P-NEW-PROVIDER-NO = '170137')
510600         MOVE '  17' TO HOLD-PROV-MSAX
510700                        P-NEW-STAND-AMT-LOC-MSA.
510800
510900     IF (P-NEW-PROVIDER-NO = '190048' OR '190110')
511000         MOVE '  19' TO HOLD-PROV-MSAX
511100                        P-NEW-STAND-AMT-LOC-MSA.
511200
511300     IF (P-NEW-PROVIDER-NO = '230078')
511400         MOVE '  23' TO HOLD-PROV-MSAX
511500                        P-NEW-STAND-AMT-LOC-MSA.
511600
511700     IF (P-NEW-PROVIDER-NO = '260006')
511800         MOVE '  26' TO HOLD-PROV-MSAX
511900                        P-NEW-STAND-AMT-LOC-MSA.
512000
512100     IF (P-NEW-PROVIDER-NO = '300009')
512200         MOVE '  30' TO HOLD-PROV-MSAX
512300                        P-NEW-STAND-AMT-LOC-MSA.
512400
512500     IF (P-NEW-PROVIDER-NO = '380084')
512600         MOVE '  38' TO HOLD-PROV-MSAX
512700                        P-NEW-STAND-AMT-LOC-MSA.
512800
512900 2700-2003-EXIT.   EXIT.
513000
513100 2700-2004-WI-401-HOSPITAL.
513200***************************************************************
513300****    FOR FY 2004 SECTION 401 HOSPITALS                     *
513400***************************************************************
513500     IF (P-NEW-PROVIDER-NO = '050192' OR '050286' OR
513600                             '050469' OR '050528' OR
513700                             '050618')
513800         MOVE '  05' TO HOLD-PROV-MSAX
513900                        P-NEW-STAND-AMT-LOC-MSA.
514000
514100     IF (P-NEW-PROVIDER-NO = '100048' OR '100118')
514200         MOVE '  10' TO HOLD-PROV-MSAX
514300                        P-NEW-STAND-AMT-LOC-MSA.
514400
514500     IF (P-NEW-PROVIDER-NO = '170137')
514600         MOVE '  17' TO HOLD-PROV-MSAX
514700                        P-NEW-STAND-AMT-LOC-MSA.
514800
514900     IF (P-NEW-PROVIDER-NO = '190048' OR '190110')
515000         MOVE '  19' TO HOLD-PROV-MSAX
515100                        P-NEW-STAND-AMT-LOC-MSA.
515200
515300     IF (P-NEW-PROVIDER-NO = '230078')
515400         MOVE '  23' TO HOLD-PROV-MSAX
515500                        P-NEW-STAND-AMT-LOC-MSA.
515600
515700     IF (P-NEW-PROVIDER-NO = '260006')
515800         MOVE '  26' TO HOLD-PROV-MSAX
515900                        P-NEW-STAND-AMT-LOC-MSA.
516000
516100     IF (P-NEW-PROVIDER-NO = '300009')
516200         MOVE '  30' TO HOLD-PROV-MSAX
516300                        P-NEW-STAND-AMT-LOC-MSA.
516400
516500     IF (P-NEW-PROVIDER-NO = '380084')
516600         MOVE '  38' TO HOLD-PROV-MSAX
516700                        P-NEW-STAND-AMT-LOC-MSA.
516800
516900     IF (P-NEW-PROVIDER-NO = '390106')
517000         MOVE '  39' TO HOLD-PROV-MSAX
517100                        P-NEW-STAND-AMT-LOC-MSA.
517200
517300 2700-2004-EXIT.   EXIT.
517400
517500 2700-2005-WI-401-HOSPITAL.
517600***************************************************************
517700****    FOR FY 2005 SECTION 401 HOSPITALS  OCT THRU MAR 2005  *
517800***************************************************************
517900     IF (P-NEW-PROVIDER-NO = '050192' OR '050286' OR
518000                             '050446' OR '050469' OR
518100                             '050528' OR '050618' OR
518200                             '051301')
518300         MOVE '   05' TO HOLD-PROV-CBSA
518400                        P-NEW-CBSA-STAND-AMT-LOC.
518500
518600     IF (P-NEW-PROVIDER-NO = '070004')
518700         MOVE '   07' TO HOLD-PROV-CBSA
518800                        P-NEW-CBSA-STAND-AMT-LOC.
518900
519000     IF (P-NEW-PROVIDER-NO = '100048' OR '100118')
519100         MOVE '   10' TO HOLD-PROV-CBSA
519200                        P-NEW-CBSA-STAND-AMT-LOC.
519300
519400     IF (P-NEW-PROVIDER-NO = '170137')
519500         MOVE '   17' TO HOLD-PROV-CBSA
519600                        P-NEW-CBSA-STAND-AMT-LOC.
519700
519800     IF (P-NEW-PROVIDER-NO = '190048' OR '190110')
519900         MOVE '   19' TO HOLD-PROV-CBSA
520000                        P-NEW-CBSA-STAND-AMT-LOC.
520100
520200     IF (P-NEW-PROVIDER-NO = '230078')
520300         MOVE '   23' TO HOLD-PROV-CBSA
520400                        P-NEW-CBSA-STAND-AMT-LOC.
520500
520600     IF (P-NEW-PROVIDER-NO = '260006')
520700         MOVE '   26' TO HOLD-PROV-CBSA
520800                        P-NEW-CBSA-STAND-AMT-LOC.
520900
521000     IF (P-NEW-PROVIDER-NO = '290038' OR '291301')
521100         MOVE '   29' TO HOLD-PROV-CBSA
521200                        P-NEW-CBSA-STAND-AMT-LOC.
521300
521400     IF (P-NEW-PROVIDER-NO = '300009')
521500         MOVE '   30' TO HOLD-PROV-CBSA
521600                        P-NEW-CBSA-STAND-AMT-LOC.
521700
521800     IF (P-NEW-PROVIDER-NO = '380084')
521900         MOVE '   38' TO HOLD-PROV-CBSA
522000                        P-NEW-CBSA-STAND-AMT-LOC.
522100
522200     IF (P-NEW-PROVIDER-NO = '390106' OR '390181')
522300         MOVE '   39' TO HOLD-PROV-CBSA
522400                        P-NEW-CBSA-STAND-AMT-LOC.
522500
522600 2700-2005-EXIT.   EXIT.
522700
522800 2750-2005-WI-401-HOSPITAL.
522900***************************************************************
523000****  FOR FY 2005 SECTION 401 HOSPITALS EFFECTIVE APR 1,2005  *
523100***************************************************************
523200     IF (P-NEW-PROVIDER-NO = '030007')
523300         MOVE '   03' TO HOLD-PROV-CBSA
523400                        P-NEW-CBSA-STAND-AMT-LOC.
523500
523600     IF (P-NEW-PROVIDER-NO = '040075')
523700         MOVE '   04' TO HOLD-PROV-CBSA
523800                        P-NEW-CBSA-STAND-AMT-LOC.
523900
524000     IF (P-NEW-PROVIDER-NO = '050192' OR '050469' OR
524100                             '050528' OR '050618')
524200         MOVE '   05' TO HOLD-PROV-CBSA
524300                        P-NEW-CBSA-STAND-AMT-LOC.
524400
524500     IF (P-NEW-PROVIDER-NO = '070004')
524600         MOVE '   07' TO HOLD-PROV-CBSA
524700                        P-NEW-CBSA-STAND-AMT-LOC.
524800
524900     IF (P-NEW-PROVIDER-NO = '100048' OR '100134')
525000         MOVE '   10' TO HOLD-PROV-CBSA
525100                        P-NEW-CBSA-STAND-AMT-LOC.
525200
525300     IF (P-NEW-PROVIDER-NO = '130018')
525400         MOVE '   13' TO HOLD-PROV-CBSA
525500                        P-NEW-CBSA-STAND-AMT-LOC.
525600
525700     IF (P-NEW-PROVIDER-NO = '140167')
525800         MOVE '   14' TO HOLD-PROV-CBSA
525900                        P-NEW-CBSA-STAND-AMT-LOC.
526000
526100     IF (P-NEW-PROVIDER-NO = '150051' OR '150078')
526200         MOVE '   15' TO HOLD-PROV-CBSA
526300                        P-NEW-CBSA-STAND-AMT-LOC.
526400
526500     IF (P-NEW-PROVIDER-NO = '170137')
526600         MOVE '   17' TO HOLD-PROV-CBSA
526700                        P-NEW-CBSA-STAND-AMT-LOC.
526800
526900     IF (P-NEW-PROVIDER-NO = '190048')
527000         MOVE '   19' TO HOLD-PROV-CBSA
527100                        P-NEW-CBSA-STAND-AMT-LOC.
527200
527300     IF (P-NEW-PROVIDER-NO = '230078')
527400         MOVE '   23' TO HOLD-PROV-CBSA
527500                        P-NEW-CBSA-STAND-AMT-LOC.
527600
527700     IF (P-NEW-PROVIDER-NO = '240037')
527800         MOVE '   24' TO HOLD-PROV-CBSA
527900                        P-NEW-CBSA-STAND-AMT-LOC.
528000
528100     IF (P-NEW-PROVIDER-NO = '260006' OR '260122')
528200         MOVE '   26' TO HOLD-PROV-CBSA
528300                        P-NEW-CBSA-STAND-AMT-LOC.
528400
528500     IF (P-NEW-PROVIDER-NO = '300009')
528600         MOVE '   30' TO HOLD-PROV-CBSA
528700                        P-NEW-CBSA-STAND-AMT-LOC.
528800
528900     IF (P-NEW-PROVIDER-NO = '370054')
529000         MOVE '   37' TO HOLD-PROV-CBSA
529100                        P-NEW-CBSA-STAND-AMT-LOC.
529200
529300     IF (P-NEW-PROVIDER-NO = '380040' OR '380084')
529400         MOVE '   38' TO HOLD-PROV-CBSA
529500                        P-NEW-CBSA-STAND-AMT-LOC.
529600
529700     IF (P-NEW-PROVIDER-NO = '390181' OR '390183' OR
529800                             '390201')
529900         MOVE '   39' TO HOLD-PROV-CBSA
530000                        P-NEW-CBSA-STAND-AMT-LOC.
530100
530200     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
530300                             '450243' OR '450276' OR
530400                             '450348')
530500         MOVE '   45' TO HOLD-PROV-CBSA
530600                        P-NEW-CBSA-STAND-AMT-LOC.
530700
530800     IF (P-NEW-PROVIDER-NO = '500023' OR '500037' OR
530900                             '500122' OR '500147' OR
531000                             '500148')
531100         MOVE '   50' TO HOLD-PROV-CBSA
531200                        P-NEW-CBSA-STAND-AMT-LOC.
531300
531400 2750-2005-EXIT.   EXIT.
531500
531600 2800-2006-WI-401-HOSPITAL.
531700***************************************************************
531800****  FOR FY 2006 SECTION 401 HOSPITALS EFFECTIVE OCT 1,2005  *
531900***************************************************************
532000     IF (P-NEW-PROVIDER-NO = '030007')
532100         MOVE '   03' TO HOLD-PROV-CBSA
532200                        P-NEW-CBSA-STAND-AMT-LOC.
532300
532400     IF (P-NEW-PROVIDER-NO = '040075')
532500         MOVE '   04' TO HOLD-PROV-CBSA
532600                        P-NEW-CBSA-STAND-AMT-LOC.
532700
532800     IF (P-NEW-PROVIDER-NO = '050192' OR '050469' OR
532900                             '050528' OR '050618')
533000         MOVE '   05' TO HOLD-PROV-CBSA
533100                        P-NEW-CBSA-STAND-AMT-LOC.
533200
533300     IF (P-NEW-PROVIDER-NO = '070004')
533400         MOVE '   07' TO HOLD-PROV-CBSA
533500                        P-NEW-CBSA-STAND-AMT-LOC.
533600
533700     IF (P-NEW-PROVIDER-NO = '100048' OR '100134')
533800         MOVE '   10' TO HOLD-PROV-CBSA
533900                        P-NEW-CBSA-STAND-AMT-LOC.
534000
534100     IF (P-NEW-PROVIDER-NO = '130018')
534200         MOVE '   13' TO HOLD-PROV-CBSA
534300                        P-NEW-CBSA-STAND-AMT-LOC.
534400
534500     IF (P-NEW-PROVIDER-NO = '140167')
534600         MOVE '   14' TO HOLD-PROV-CBSA
534700                        P-NEW-CBSA-STAND-AMT-LOC.
534800
534900     IF B-21-DISCHARGE-DATE > 20051231
535000        IF (P-NEW-PROVIDER-NO = '150078')
535100           MOVE '   15' TO HOLD-PROV-CBSA
535200                          P-NEW-CBSA-STAND-AMT-LOC.
535300
535400     IF B-21-DISCHARGE-DATE < 20060101
535500        IF (P-NEW-PROVIDER-NO = '150051' OR '150078')
535600           MOVE '   15' TO HOLD-PROV-CBSA
535700                          P-NEW-CBSA-STAND-AMT-LOC.
535800
535900     IF (P-NEW-PROVIDER-NO = '170137')
536000         MOVE '   17' TO HOLD-PROV-CBSA
536100                        P-NEW-CBSA-STAND-AMT-LOC.
536200
536300     IF (P-NEW-PROVIDER-NO = '190048' OR '190110')
536400         MOVE '   19' TO HOLD-PROV-CBSA
536500                        P-NEW-CBSA-STAND-AMT-LOC.
536600
536700     IF (P-NEW-PROVIDER-NO = '230042' OR '230078')
536800         MOVE '   23' TO HOLD-PROV-CBSA
536900                        P-NEW-CBSA-STAND-AMT-LOC.
537000
537100     IF (P-NEW-PROVIDER-NO = '240037' OR '240122')
537200         MOVE '   24' TO HOLD-PROV-CBSA
537300                        P-NEW-CBSA-STAND-AMT-LOC.
537400
537500     IF (P-NEW-PROVIDER-NO = '260006')
537600         MOVE '   26' TO HOLD-PROV-CBSA
537700                        P-NEW-CBSA-STAND-AMT-LOC.
537800
537900     IF (P-NEW-PROVIDER-NO = '300009')
538000         MOVE '   30' TO HOLD-PROV-CBSA
538100                        P-NEW-CBSA-STAND-AMT-LOC.
538200
538300     IF (P-NEW-PROVIDER-NO = '330268')
538400         MOVE '   33' TO HOLD-PROV-CBSA
538500                        P-NEW-CBSA-STAND-AMT-LOC.
538600
538700     IF (P-NEW-PROVIDER-NO = '370054')
538800         MOVE '   37' TO HOLD-PROV-CBSA
538900                        P-NEW-CBSA-STAND-AMT-LOC.
539000
539100     IF (P-NEW-PROVIDER-NO = '380040' OR '380084')
539200         MOVE '   38' TO HOLD-PROV-CBSA
539300                        P-NEW-CBSA-STAND-AMT-LOC.
539400
539500     IF (P-NEW-PROVIDER-NO = '390181' OR '390183' OR
539600                             '390201')
539700         MOVE '   39' TO HOLD-PROV-CBSA
539800                        P-NEW-CBSA-STAND-AMT-LOC.
539900
540000     IF (P-NEW-PROVIDER-NO = '440135')
540100         MOVE '   44' TO HOLD-PROV-CBSA
540200                        P-NEW-CBSA-STAND-AMT-LOC.
540300
540400     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
540500                             '450243' OR '450276' OR
540600                             '450348')
540700         MOVE '   45' TO HOLD-PROV-CBSA
540800                        P-NEW-CBSA-STAND-AMT-LOC.
540900
541000     IF (P-NEW-PROVIDER-NO = '500023' OR '500043' OR
541100                             '500086' OR '500103' OR
541200                             '500122' OR '500147' OR
541300                             '500148')
541400         MOVE '   50' TO HOLD-PROV-CBSA
541500                        P-NEW-CBSA-STAND-AMT-LOC.
541600
541700 2800-2006-EXIT.   EXIT.
541800
541900 2800-2007-WI-401-HOSPITAL.
542000***************************************************************
542100****  FOR FY 2007 SECTION 401 HOSPITALS EFFECTIVE OCT 1,2006  *
542200***************************************************************
542300
542400     IF (P-NEW-PROVIDER-NO = '050192' OR '050469' OR
542500                             '050528' OR '050618')
542600         MOVE '   05' TO HOLD-PROV-CBSA
542700                        P-NEW-CBSA-STAND-AMT-LOC.
542800
542900     IF (P-NEW-PROVIDER-NO = '070004')
543000         MOVE '   07' TO HOLD-PROV-CBSA
543100                        P-NEW-CBSA-STAND-AMT-LOC.
543200
543300     IF (P-NEW-PROVIDER-NO = '100048' OR '100134')
543400         MOVE '   10' TO HOLD-PROV-CBSA
543500                        P-NEW-CBSA-STAND-AMT-LOC.
543600
543700     IF (P-NEW-PROVIDER-NO = '140167')
543800         MOVE '   14' TO HOLD-PROV-CBSA
543900                        P-NEW-CBSA-STAND-AMT-LOC.
544000
544100     IF (P-NEW-PROVIDER-NO = '170137')
544200         MOVE '   17' TO HOLD-PROV-CBSA
544300                        P-NEW-CBSA-STAND-AMT-LOC.
544400
544500     IF (P-NEW-PROVIDER-NO = '230078')
544600         MOVE '   23' TO HOLD-PROV-CBSA
544700                        P-NEW-CBSA-STAND-AMT-LOC.
544800
544900     IF (P-NEW-PROVIDER-NO = '260006' OR '260047' OR '260195')
545000         MOVE '   26' TO HOLD-PROV-CBSA
545100                        P-NEW-CBSA-STAND-AMT-LOC.
545200
545300     IF (P-NEW-PROVIDER-NO = '330044' OR '330245' OR '330268')
545400         MOVE '   33' TO HOLD-PROV-CBSA
545500                        P-NEW-CBSA-STAND-AMT-LOC.
545600
545700     IF (P-NEW-PROVIDER-NO = '360125')
545800         MOVE '   36' TO HOLD-PROV-CBSA
545900                        P-NEW-CBSA-STAND-AMT-LOC.
546000
546100     IF (P-NEW-PROVIDER-NO = '370054')
546200         MOVE '   37' TO HOLD-PROV-CBSA
546300                        P-NEW-CBSA-STAND-AMT-LOC.
546400
546500     IF (P-NEW-PROVIDER-NO = '380040')
546600         MOVE '   38' TO HOLD-PROV-CBSA
546700                        P-NEW-CBSA-STAND-AMT-LOC.
546800
546900     IF (P-NEW-PROVIDER-NO = '440135' OR '440144')
547000         MOVE '   44' TO HOLD-PROV-CBSA
547100                        P-NEW-CBSA-STAND-AMT-LOC.
547200
547300     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
547400                             '450243' OR '450348')
547500         MOVE '   45' TO HOLD-PROV-CBSA
547600                        P-NEW-CBSA-STAND-AMT-LOC.
547700
547800     IF (P-NEW-PROVIDER-NO = '500148')
547900         MOVE '   50' TO HOLD-PROV-CBSA
548000                        P-NEW-CBSA-STAND-AMT-LOC.
548100
548200     IF (P-NEW-PROVIDER-NO = '520060')
548300         MOVE '   52' TO HOLD-PROV-CBSA
548400                        P-NEW-CBSA-STAND-AMT-LOC.
548500
548600 2800-2007-EXIT.   EXIT.
548700
548800 2800-2008-WI-401-HOSPITAL.
548900***************************************************************
549000****  FOR FY 2008 SECTION 401 HOSPITALS EFFECTIVE OCT 1,2007  *
549100***************************************************************
549200
549300     IF (P-NEW-PROVIDER-NO = '050192' OR
549400                             '050528' OR '050618')
549500         MOVE '   05' TO HOLD-PROV-CBSA
549600                        P-NEW-CBSA-STAND-AMT-LOC.
549700
549800     IF (P-NEW-PROVIDER-NO = '100134')
549900         MOVE '   10' TO HOLD-PROV-CBSA
550000                        P-NEW-CBSA-STAND-AMT-LOC.
550100
550200     IF (P-NEW-PROVIDER-NO = '170137')
550300         MOVE '   17' TO HOLD-PROV-CBSA
550400                        P-NEW-CBSA-STAND-AMT-LOC.
550500
550600     IF (P-NEW-PROVIDER-NO = '220051' OR '230078')
550700         MOVE '   23' TO HOLD-PROV-CBSA
550800                        P-NEW-CBSA-STAND-AMT-LOC.
550900
551000     IF (P-NEW-PROVIDER-NO = '250017')
551100         MOVE '   25' TO HOLD-PROV-CBSA
551200                        P-NEW-CBSA-STAND-AMT-LOC.
551300
551400     IF (P-NEW-PROVIDER-NO = '260006' OR '260195')
551500         MOVE '   26' TO HOLD-PROV-CBSA
551600                        P-NEW-CBSA-STAND-AMT-LOC.
551700
551800     IF (P-NEW-PROVIDER-NO = '330268')
551900         MOVE '   33' TO HOLD-PROV-CBSA
552000                        P-NEW-CBSA-STAND-AMT-LOC.
552100
552200     IF (P-NEW-PROVIDER-NO = '360125')
552300         MOVE '   36' TO HOLD-PROV-CBSA
552400                        P-NEW-CBSA-STAND-AMT-LOC.
552500
552600     IF (P-NEW-PROVIDER-NO = '370054')
552700         MOVE '   37' TO HOLD-PROV-CBSA
552800                        P-NEW-CBSA-STAND-AMT-LOC.
552900
553000     IF (P-NEW-PROVIDER-NO = '380040')
553100         MOVE '   38' TO HOLD-PROV-CBSA
553200                        P-NEW-CBSA-STAND-AMT-LOC.
553300
553400     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
553500                             '390185' OR '390201')
553600         MOVE '   39' TO HOLD-PROV-CBSA
553700                        P-NEW-CBSA-STAND-AMT-LOC.
553800
553900     IF (P-NEW-PROVIDER-NO = '440135')
554000         MOVE '   44' TO HOLD-PROV-CBSA
554100                        P-NEW-CBSA-STAND-AMT-LOC.
554200
554300     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
554400                             '450243' OR '450348')
554500         MOVE '   45' TO HOLD-PROV-CBSA
554600                        P-NEW-CBSA-STAND-AMT-LOC.
554700
554800     IF (P-NEW-PROVIDER-NO = '500148')
554900         MOVE '   50' TO HOLD-PROV-CBSA
555000                        P-NEW-CBSA-STAND-AMT-LOC.
555100
555200 2800-2008-EXIT.   EXIT.
555300
555400 2800-2009-WI-401-HOSPITAL.
555500***************************************************************
555600****  FOR FY 2009 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2009 *
555700***************************************************************
555800
555900     IF (P-NEW-PROVIDER-NO = '040118')
556000         MOVE '   04' TO HOLD-PROV-CBSA
556100                        P-NEW-CBSA-STAND-AMT-LOC.
556200
556300     IF (P-NEW-PROVIDER-NO = '234202' OR
556400                             '329008' OR '040140')
556500         MOVE '   05' TO HOLD-PROV-CBSA
556600                        P-NEW-CBSA-STAND-AMT-LOC.
556700
556800     IF (P-NEW-PROVIDER-NO = '070004' OR
556900                             '070036')
557000         MOVE '   07' TO HOLD-PROV-CBSA
557100                        P-NEW-CBSA-STAND-AMT-LOC.
557200
557300     IF (P-NEW-PROVIDER-NO = '100048' OR
557400                             '100118' OR '100134')
557500         MOVE '   10' TO HOLD-PROV-CBSA
557600                        P-NEW-CBSA-STAND-AMT-LOC.
557700
557800     IF (P-NEW-PROVIDER-NO = '140167')
557900         MOVE '   14' TO HOLD-PROV-CBSA
558000                        P-NEW-CBSA-STAND-AMT-LOC.
558100
558200     IF (P-NEW-PROVIDER-NO = '170137')
558300         MOVE '   17' TO HOLD-PROV-CBSA
558400                        P-NEW-CBSA-STAND-AMT-LOC.
558500
558600     IF (P-NEW-PROVIDER-NO = '180038')
558700         MOVE '   18' TO HOLD-PROV-CBSA
558800                        P-NEW-CBSA-STAND-AMT-LOC.
558900
559000     IF (P-NEW-PROVIDER-NO = '220051')
559100         MOVE '   22' TO HOLD-PROV-CBSA
559200                        P-NEW-CBSA-STAND-AMT-LOC.
559300
559400     IF (P-NEW-PROVIDER-NO = '230078')
559500         MOVE '   23' TO HOLD-PROV-CBSA
559600                        P-NEW-CBSA-STAND-AMT-LOC.
559700
559800     IF (P-NEW-PROVIDER-NO = '250017')
559900         MOVE '   25' TO HOLD-PROV-CBSA
560000                        P-NEW-CBSA-STAND-AMT-LOC.
560100
560200     IF (P-NEW-PROVIDER-NO = '260006' OR '260047' OR '260195')
560300         MOVE '   26' TO HOLD-PROV-CBSA
560400                        P-NEW-CBSA-STAND-AMT-LOC.
560500
560600     IF (P-NEW-PROVIDER-NO = '330235' OR '330268')
560700         MOVE '   33' TO HOLD-PROV-CBSA
560800                        P-NEW-CBSA-STAND-AMT-LOC.
560900
561000     IF (P-NEW-PROVIDER-NO = '360125')
561100         MOVE '   36' TO HOLD-PROV-CBSA
561200                        P-NEW-CBSA-STAND-AMT-LOC.
561300
561400     IF (P-NEW-PROVIDER-NO = '370054')
561500         MOVE '   37' TO HOLD-PROV-CBSA
561600                        P-NEW-CBSA-STAND-AMT-LOC.
561700
561800     IF (P-NEW-PROVIDER-NO = '380040')
561900         MOVE '   38' TO HOLD-PROV-CBSA
562000                        P-NEW-CBSA-STAND-AMT-LOC.
562100
562200     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
562300                             '390233')
562400         MOVE '   39' TO HOLD-PROV-CBSA
562500                        P-NEW-CBSA-STAND-AMT-LOC.
562600
562700     IF (P-NEW-PROVIDER-NO = '440135')
562800         MOVE '   44' TO HOLD-PROV-CBSA
562900                        P-NEW-CBSA-STAND-AMT-LOC.
563000
563100     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
563200                             '450243' OR '450348')
563300         MOVE '   45' TO HOLD-PROV-CBSA
563400                        P-NEW-CBSA-STAND-AMT-LOC.
563500
563600     IF (P-NEW-PROVIDER-NO = '490116')
563700         MOVE '   49' TO HOLD-PROV-CBSA
563800                        P-NEW-CBSA-STAND-AMT-LOC.
563900
564000     IF (P-NEW-PROVIDER-NO = '500148')
564100         MOVE '   50' TO HOLD-PROV-CBSA
564200                        P-NEW-CBSA-STAND-AMT-LOC.
564300
564400 2800-2009-EXIT.   EXIT.
564500
564600 2800-2010-WI-401-HOSPITAL.
564700***************************************************************
564800****  FOR FY 2010 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2009 *
564900***************************************************************
565000
565100     IF (P-NEW-PROVIDER-NO = '040118')
565200         MOVE '   04' TO HOLD-PROV-CBSA
565300                        P-NEW-CBSA-STAND-AMT-LOC.
565400
565500     IF (P-NEW-PROVIDER-NO = '050192' OR
565600                             '050528' OR '050618')
565700         MOVE '   05' TO HOLD-PROV-CBSA
565800                        P-NEW-CBSA-STAND-AMT-LOC.
565900
566000     IF (P-NEW-PROVIDER-NO = '070004')
566100         MOVE '   07' TO HOLD-PROV-CBSA
566200                        P-NEW-CBSA-STAND-AMT-LOC.
566300
566400     IF (P-NEW-PROVIDER-NO = '100048' OR
566500                             '100118' OR '100134')
566600         MOVE '   10' TO HOLD-PROV-CBSA
566700                        P-NEW-CBSA-STAND-AMT-LOC.
566800
566900     IF (P-NEW-PROVIDER-NO = '140167')
567000         MOVE '   14' TO HOLD-PROV-CBSA
567100                        P-NEW-CBSA-STAND-AMT-LOC.
567200
567300     IF (P-NEW-PROVIDER-NO = '170137')
567400         MOVE '   17' TO HOLD-PROV-CBSA
567500                        P-NEW-CBSA-STAND-AMT-LOC.
567600
567700     IF (P-NEW-PROVIDER-NO = '180038')
567800         MOVE '   18' TO HOLD-PROV-CBSA
567900                        P-NEW-CBSA-STAND-AMT-LOC.
568000
568100     IF (P-NEW-PROVIDER-NO = '220051')
568200         MOVE '   22' TO HOLD-PROV-CBSA
568300                        P-NEW-CBSA-STAND-AMT-LOC.
568400
568500     IF (P-NEW-PROVIDER-NO = '230078')
568600         MOVE '   23' TO HOLD-PROV-CBSA
568700                        P-NEW-CBSA-STAND-AMT-LOC.
568800
568900     IF (P-NEW-PROVIDER-NO = '250017')
569000         MOVE '   25' TO HOLD-PROV-CBSA
569100                        P-NEW-CBSA-STAND-AMT-LOC.
569200
569300     IF (P-NEW-PROVIDER-NO = '260006' OR '260034' OR
569400                             '260047' OR '260195')
569500         MOVE '   26' TO HOLD-PROV-CBSA
569600                        P-NEW-CBSA-STAND-AMT-LOC.
569700
569800     IF (P-NEW-PROVIDER-NO = '330235' OR '330268')
569900         MOVE '   33' TO HOLD-PROV-CBSA
570000                        P-NEW-CBSA-STAND-AMT-LOC.
570100
570200     IF (P-NEW-PROVIDER-NO = '300023')
570300         MOVE '   30' TO HOLD-PROV-CBSA
570400                        P-NEW-CBSA-STAND-AMT-LOC.
570500
570600     IF (P-NEW-PROVIDER-NO = '360125')
570700         MOVE '   36' TO HOLD-PROV-CBSA
570800                        P-NEW-CBSA-STAND-AMT-LOC.
570900
571000     IF (P-NEW-PROVIDER-NO = '370054')
571100         MOVE '   37' TO HOLD-PROV-CBSA
571200                        P-NEW-CBSA-STAND-AMT-LOC.
571300
571400     IF (P-NEW-PROVIDER-NO = '380040')
571500         MOVE '   38' TO HOLD-PROV-CBSA
571600                        P-NEW-CBSA-STAND-AMT-LOC.
571700
571800     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
571900                             '390233')
572000         MOVE '   39' TO HOLD-PROV-CBSA
572100                        P-NEW-CBSA-STAND-AMT-LOC.
572200
572300     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
572400                             '450243' OR '450348')
572500         MOVE '   45' TO HOLD-PROV-CBSA
572600                        P-NEW-CBSA-STAND-AMT-LOC.
572700
572800     IF (P-NEW-PROVIDER-NO = '490116')
572900         MOVE '   49' TO HOLD-PROV-CBSA
573000                        P-NEW-CBSA-STAND-AMT-LOC.
573100
573200     IF (P-NEW-PROVIDER-NO = '500148')
573300         MOVE '   50' TO HOLD-PROV-CBSA
573400                        P-NEW-CBSA-STAND-AMT-LOC.
573500
573600 2800-2010-EXIT.   EXIT.
573700
573800 2800-2011-WI-401-HOSPITAL.
573900***************************************************************
574000****  FOR FY 2011 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2010 *
574100***************************************************************
574200*
574300
574400     IF (P-NEW-PROVIDER-NO = '040118')
574500         MOVE '   04' TO HOLD-PROV-CBSA
574600                        P-NEW-CBSA-STAND-AMT-LOC.
574700
574800     IF (P-NEW-PROVIDER-NO = '050192' OR
574900                             '050528' OR '050618')
575000         MOVE '   05' TO HOLD-PROV-CBSA
575100                        P-NEW-CBSA-STAND-AMT-LOC.
575200
575300     IF (P-NEW-PROVIDER-NO = '070004')
575400         MOVE '   07' TO HOLD-PROV-CBSA
575500                        P-NEW-CBSA-STAND-AMT-LOC.
575600
575700     IF (P-NEW-PROVIDER-NO = '100048' OR
575800                             '100118' OR '100134')
575900         MOVE '   10' TO HOLD-PROV-CBSA
576000                        P-NEW-CBSA-STAND-AMT-LOC.
576100
576200     IF (P-NEW-PROVIDER-NO = '140167')
576300         MOVE '   14' TO HOLD-PROV-CBSA
576400                        P-NEW-CBSA-STAND-AMT-LOC.
576500
576600     IF (P-NEW-PROVIDER-NO = '170037' OR '170137')
576700         MOVE '   17' TO HOLD-PROV-CBSA
576800                        P-NEW-CBSA-STAND-AMT-LOC.
576900
577000     IF (P-NEW-PROVIDER-NO = '180016' OR '180038')
577100         MOVE '   18' TO HOLD-PROV-CBSA
577200                        P-NEW-CBSA-STAND-AMT-LOC.
577300
577400     IF (P-NEW-PROVIDER-NO = '220051')
577500         MOVE '   22' TO HOLD-PROV-CBSA
577600                        P-NEW-CBSA-STAND-AMT-LOC.
577700
577800     IF (P-NEW-PROVIDER-NO = '230040' OR '230078')
577900         MOVE '   23' TO HOLD-PROV-CBSA
578000                        P-NEW-CBSA-STAND-AMT-LOC.
578100
578200     IF (P-NEW-PROVIDER-NO = '260006' OR '260034' OR
578300                             '260047' OR '260195')
578400         MOVE '   26' TO HOLD-PROV-CBSA
578500                        P-NEW-CBSA-STAND-AMT-LOC.
578600
578700     IF (P-NEW-PROVIDER-NO = '300023')
578800         MOVE '   30' TO HOLD-PROV-CBSA
578900                        P-NEW-CBSA-STAND-AMT-LOC.
579000
579100     IF (P-NEW-PROVIDER-NO = '330215' OR '330235' OR
579200                             '330268')
579300         MOVE '   33' TO HOLD-PROV-CBSA
579400                        P-NEW-CBSA-STAND-AMT-LOC.
579500
579600     IF (P-NEW-PROVIDER-NO = '340010')
579700         MOVE '   34' TO HOLD-PROV-CBSA
579800                        P-NEW-CBSA-STAND-AMT-LOC.
579900
580000     IF (P-NEW-PROVIDER-NO = '360125')
580100         MOVE '   36' TO HOLD-PROV-CBSA
580200                        P-NEW-CBSA-STAND-AMT-LOC.
580300
580400     IF (P-NEW-PROVIDER-NO = '370054')
580500         MOVE '   37' TO HOLD-PROV-CBSA
580600                        P-NEW-CBSA-STAND-AMT-LOC.
580700
580800     IF (P-NEW-PROVIDER-NO = '380040')
580900         MOVE '   38' TO HOLD-PROV-CBSA
581000                        P-NEW-CBSA-STAND-AMT-LOC.
581100
581200     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
581300                             '390233')
581400         MOVE '   39' TO HOLD-PROV-CBSA
581500                        P-NEW-CBSA-STAND-AMT-LOC.
581600
581700     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
581800                             '450243' OR '450348')
581900         MOVE '   45' TO HOLD-PROV-CBSA
582000                        P-NEW-CBSA-STAND-AMT-LOC.
582100
582200     IF (P-NEW-PROVIDER-NO = '490116')
582300         MOVE '   49' TO HOLD-PROV-CBSA
582400                        P-NEW-CBSA-STAND-AMT-LOC.
582500
582600     IF (P-NEW-PROVIDER-NO = '500148')
582700         MOVE '   50' TO HOLD-PROV-CBSA
582800                        P-NEW-CBSA-STAND-AMT-LOC.
582900
583000
583100 2800-2011-EXIT.   EXIT.
583200
583300
583400 2800-2012-WI-401-HOSPITAL.
583500***************************************************************
583600****  FOR FY 2012 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2011 *
583700***************************************************************
583800*
583900
584000     IF (P-NEW-PROVIDER-NO = '040118')
584100         MOVE '   04' TO HOLD-PROV-CBSA
584200                        P-NEW-CBSA-STAND-AMT-LOC.
584300
584400     IF (P-NEW-PROVIDER-NO = '050192' OR
584500                             '050528' OR '050618')
584600         MOVE '   05' TO HOLD-PROV-CBSA
584700                        P-NEW-CBSA-STAND-AMT-LOC.
584800
584900     IF (P-NEW-PROVIDER-NO = '070004')
585000         MOVE '   07' TO HOLD-PROV-CBSA
585100                        P-NEW-CBSA-STAND-AMT-LOC.
585200
585300     IF (P-NEW-PROVIDER-NO = '100048' OR
585400                             '100118' OR '100134')
585500         MOVE '   10' TO HOLD-PROV-CBSA
585600                        P-NEW-CBSA-STAND-AMT-LOC.
585700
585800     IF (P-NEW-PROVIDER-NO = '140167')
585900         MOVE '   14' TO HOLD-PROV-CBSA
586000                        P-NEW-CBSA-STAND-AMT-LOC.
586100
586200     IF (P-NEW-PROVIDER-NO = '150003')
586300         MOVE '   15' TO HOLD-PROV-CBSA
586400                        P-NEW-CBSA-STAND-AMT-LOC.
586500
586600     IF (P-NEW-PROVIDER-NO = '170074' OR '170137')
586700         MOVE '   17' TO HOLD-PROV-CBSA
586800                        P-NEW-CBSA-STAND-AMT-LOC.
586900
587000     IF (P-NEW-PROVIDER-NO = '180016' OR '180038')
587100         MOVE '   18' TO HOLD-PROV-CBSA
587200                        P-NEW-CBSA-STAND-AMT-LOC.
587300
587400     IF (P-NEW-PROVIDER-NO = '220051')
587500         MOVE '   22' TO HOLD-PROV-CBSA
587600                        P-NEW-CBSA-STAND-AMT-LOC.
587700
587800     IF (P-NEW-PROVIDER-NO = '230040' OR '230078')
587900         MOVE '   23' TO HOLD-PROV-CBSA
588000                        P-NEW-CBSA-STAND-AMT-LOC.
588100
588200     IF (P-NEW-PROVIDER-NO = '260006' OR '260034' OR
588300                             '260047' OR '260195')
588400         MOVE '   26' TO HOLD-PROV-CBSA
588500                        P-NEW-CBSA-STAND-AMT-LOC.
588600
588700     IF (P-NEW-PROVIDER-NO = '300023')
588800         MOVE '   30' TO HOLD-PROV-CBSA
588900                        P-NEW-CBSA-STAND-AMT-LOC.
589000
589100     IF (P-NEW-PROVIDER-NO = '330013' OR '330057' OR
589200                             '330108' OR '330164' OR
589300                             '330215' OR '330235' OR
589400                             '330268' OR '330285')
589500         MOVE '   33' TO HOLD-PROV-CBSA
589600                        P-NEW-CBSA-STAND-AMT-LOC.
589700
589800     IF (P-NEW-PROVIDER-NO = '340010')
589900         MOVE '   34' TO HOLD-PROV-CBSA
590000                        P-NEW-CBSA-STAND-AMT-LOC.
590100
590200     IF (P-NEW-PROVIDER-NO = '360125')
590300         MOVE '   36' TO HOLD-PROV-CBSA
590400                        P-NEW-CBSA-STAND-AMT-LOC.
590500
590600     IF (P-NEW-PROVIDER-NO = '370054')
590700         MOVE '   37' TO HOLD-PROV-CBSA
590800                        P-NEW-CBSA-STAND-AMT-LOC.
590900
591000     IF (P-NEW-PROVIDER-NO = '380040')
591100         MOVE '   38' TO HOLD-PROV-CBSA
591200                        P-NEW-CBSA-STAND-AMT-LOC.
591300
591400     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
591500                             '390233')
591600         MOVE '   39' TO HOLD-PROV-CBSA
591700                        P-NEW-CBSA-STAND-AMT-LOC.
591800
591900     IF (P-NEW-PROVIDER-NO = '420038')
592000         MOVE '   42' TO HOLD-PROV-CBSA
592100                        P-NEW-CBSA-STAND-AMT-LOC.
592200
592300     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
592400                             '450243' OR '450348')
592500         MOVE '   45' TO HOLD-PROV-CBSA
592600                        P-NEW-CBSA-STAND-AMT-LOC.
592700
592800     IF (P-NEW-PROVIDER-NO = '490116' OR '490116')
592900         MOVE '   49' TO HOLD-PROV-CBSA
593000                        P-NEW-CBSA-STAND-AMT-LOC.
593100
593200     IF (P-NEW-PROVIDER-NO = '500148')
593300         MOVE '   50' TO HOLD-PROV-CBSA
593400                        P-NEW-CBSA-STAND-AMT-LOC.
593500
593600
593700 2800-2012-EXIT.   EXIT.
593800
593900 2800-2013-WI-401-HOSPITAL.
594000***************************************************************
594100****  FOR FY 2013 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2012 *
594200***************************************************************
594300*
594400
594500     IF (P-NEW-PROVIDER-NO = '290009')
594600         MOVE '   29' TO HOLD-PROV-CBSA
594700                        P-NEW-CBSA-STAND-AMT-LOC.
594800
594900     IF (P-NEW-PROVIDER-NO = '030024')
595000         MOVE '   03' TO HOLD-PROV-CBSA
595100                        P-NEW-CBSA-STAND-AMT-LOC.
595200
595300     IF (P-NEW-PROVIDER-NO = '050192' OR
595400                             '050528' OR '050618')
595500         MOVE '   05' TO HOLD-PROV-CBSA
595600                        P-NEW-CBSA-STAND-AMT-LOC.
595700
595800     IF (P-NEW-PROVIDER-NO = '070004')
595900         MOVE '   07' TO HOLD-PROV-CBSA
596000                        P-NEW-CBSA-STAND-AMT-LOC.
596100
596200     IF (P-NEW-PROVIDER-NO = '100048' OR '100090' OR
596300                             '100118' OR '100134')
596400         MOVE '   10' TO HOLD-PROV-CBSA
596500                        P-NEW-CBSA-STAND-AMT-LOC.
596600
596700     IF (P-NEW-PROVIDER-NO = '170074' OR '170137')
596800         MOVE '   17' TO HOLD-PROV-CBSA
596900                        P-NEW-CBSA-STAND-AMT-LOC.
597000
597100     IF (P-NEW-PROVIDER-NO = '180016' OR '180038')
597200         MOVE '   18' TO HOLD-PROV-CBSA
597300                        P-NEW-CBSA-STAND-AMT-LOC.
597400
597500     IF (P-NEW-PROVIDER-NO = '220051')
597600         MOVE '   22' TO HOLD-PROV-CBSA
597700                        P-NEW-CBSA-STAND-AMT-LOC.
597800
597900     IF (P-NEW-PROVIDER-NO = '230040' OR '230078')
598000         MOVE '   23' TO HOLD-PROV-CBSA
598100                        P-NEW-CBSA-STAND-AMT-LOC.
598200
598300     IF (P-NEW-PROVIDER-NO = '260006' OR '260034' OR
598400                             '260047' OR '260195')
598500         MOVE '   26' TO HOLD-PROV-CBSA
598600                        P-NEW-CBSA-STAND-AMT-LOC.
598700
598800     IF (P-NEW-PROVIDER-NO = '330108' OR
598900                             '330215' OR '330235' OR
599000                             '330268')
599100         MOVE '   33' TO HOLD-PROV-CBSA
599200                        P-NEW-CBSA-STAND-AMT-LOC.
599300
599400     IF (P-NEW-PROVIDER-NO = '340010')
599500         MOVE '   34' TO HOLD-PROV-CBSA
599600                        P-NEW-CBSA-STAND-AMT-LOC.
599700
599800     IF (P-NEW-PROVIDER-NO = '350011')
599900         MOVE '   35' TO HOLD-PROV-CBSA
600000                        P-NEW-CBSA-STAND-AMT-LOC.
600100
600200     IF (P-NEW-PROVIDER-NO = '360125' OR '360141')
600300         MOVE '   36' TO HOLD-PROV-CBSA
600400                        P-NEW-CBSA-STAND-AMT-LOC.
600500
600600     IF (P-NEW-PROVIDER-NO = '370054')
600700         MOVE '   37' TO HOLD-PROV-CBSA
600800                        P-NEW-CBSA-STAND-AMT-LOC.
600900
601000     IF (P-NEW-PROVIDER-NO = '380040')
601100         MOVE '   38' TO HOLD-PROV-CBSA
601200                        P-NEW-CBSA-STAND-AMT-LOC.
601300
601400     IF (P-NEW-PROVIDER-NO = '390130' OR '390183' OR
601500                             '390233')
601600         MOVE '   39' TO HOLD-PROV-CBSA
601700                        P-NEW-CBSA-STAND-AMT-LOC.
601800
601900     IF (P-NEW-PROVIDER-NO = '420009' OR '420038')
602000         MOVE '   42' TO HOLD-PROV-CBSA
602100                        P-NEW-CBSA-STAND-AMT-LOC.
602200
602300     IF (P-NEW-PROVIDER-NO = '440189')
602400         MOVE '   44' TO HOLD-PROV-CBSA
602500                        P-NEW-CBSA-STAND-AMT-LOC.
602600
602700     IF (P-NEW-PROVIDER-NO = '450052' OR '450078' OR
602800                             '450243' OR '450348')
602900         MOVE '   45' TO HOLD-PROV-CBSA
603000                        P-NEW-CBSA-STAND-AMT-LOC.
603100
603200     IF (P-NEW-PROVIDER-NO = '490004' OR '490005' OR
603300                             '490116')
603400         MOVE '   49' TO HOLD-PROV-CBSA
603500                        P-NEW-CBSA-STAND-AMT-LOC.
603600
603700     IF (P-NEW-PROVIDER-NO = '500030' OR '500148')
603800         MOVE '   50' TO HOLD-PROV-CBSA
603900                        P-NEW-CBSA-STAND-AMT-LOC.
604000
604100 2800-2013-EXIT.   EXIT.
604200
604300
604400 2800-2014-WI-401-HOSPITAL.
604500***************************************************************
604600****  FOR FY 2014 SECTION 401 HOSPITALS EFFECTIVE OCT 1, 2012 *
604700***************************************************************
604800*
604900
605000     IF (P-NEW-PROVIDER-NO = '050192' OR
605100                             '050225' OR
605200                             '050528' OR
605300                             '050618')
605400         MOVE '   05' TO HOLD-PROV-CBSA
605500                        P-NEW-CBSA-STAND-AMT-LOC.
605600
605700     IF (P-NEW-PROVIDER-NO = '060010')
605800         MOVE '   06' TO HOLD-PROV-CBSA
605900                        P-NEW-CBSA-STAND-AMT-LOC.
606000
606100     IF (P-NEW-PROVIDER-NO = '070004' OR
606200                             '070005')
606300         MOVE '   07' TO HOLD-PROV-CBSA
606400                        P-NEW-CBSA-STAND-AMT-LOC.
606500
606600     IF (P-NEW-PROVIDER-NO = '100048' OR
606700                             '100090' OR
606800                             '100118' OR
606900                             '100134')
607000         MOVE '   10' TO HOLD-PROV-CBSA
607100                        P-NEW-CBSA-STAND-AMT-LOC.
607200
607300     IF (P-NEW-PROVIDER-NO = '140059' OR
607400                             '140145')
607500         MOVE '   14' TO HOLD-PROV-CBSA
607600                        P-NEW-CBSA-STAND-AMT-LOC.
607700
607800     IF (P-NEW-PROVIDER-NO = '170074' OR
607900                             '170137')
608000         MOVE '   17' TO HOLD-PROV-CBSA
608100                        P-NEW-CBSA-STAND-AMT-LOC.
608200
608300     IF (P-NEW-PROVIDER-NO = '180016' OR
608400                             '180038')
608500         MOVE '   18' TO HOLD-PROV-CBSA
608600                        P-NEW-CBSA-STAND-AMT-LOC.
608700
608800     IF (P-NEW-PROVIDER-NO = '190008')
608900         MOVE '   19' TO HOLD-PROV-CBSA
609000                        P-NEW-CBSA-STAND-AMT-LOC.
609100
609200     IF (P-NEW-PROVIDER-NO = '220051')
609300         MOVE '   22' TO HOLD-PROV-CBSA
609400                        P-NEW-CBSA-STAND-AMT-LOC.
609500
609600     IF (P-NEW-PROVIDER-NO = '230040' OR
609700                             '230078')
609800         MOVE '   23' TO HOLD-PROV-CBSA
609900                        P-NEW-CBSA-STAND-AMT-LOC.
610000
610100     IF (P-NEW-PROVIDER-NO = '260006' OR
610200                             '260034' OR
610300                             '260047' OR
610400                             '260195')
610500         MOVE '   26' TO HOLD-PROV-CBSA
610600                        P-NEW-CBSA-STAND-AMT-LOC.
610700
610800     IF (P-NEW-PROVIDER-NO = '290009')
610900         MOVE '   29' TO HOLD-PROV-CBSA
611000                        P-NEW-CBSA-STAND-AMT-LOC.
611100
611200     IF (P-NEW-PROVIDER-NO = '330108' OR
611300                             '330235' OR
611400                             '330268')
611500         MOVE '   33' TO HOLD-PROV-CBSA
611600                        P-NEW-CBSA-STAND-AMT-LOC.
611700
611800     IF (P-NEW-PROVIDER-NO = '340010')
611900         MOVE '   34' TO HOLD-PROV-CBSA
612000                        P-NEW-CBSA-STAND-AMT-LOC.
612100
612200     IF (P-NEW-PROVIDER-NO = '350011' OR
612300                             '350015')
612400         MOVE '   35' TO HOLD-PROV-CBSA
612500                        P-NEW-CBSA-STAND-AMT-LOC.
612600
612700     IF (P-NEW-PROVIDER-NO = '360125')
612800         MOVE '   36' TO HOLD-PROV-CBSA
612900                        P-NEW-CBSA-STAND-AMT-LOC.
613000
613100     IF (P-NEW-PROVIDER-NO = '370054')
613200         MOVE '   37' TO HOLD-PROV-CBSA
613300                        P-NEW-CBSA-STAND-AMT-LOC.
613400
613500     IF (P-NEW-PROVIDER-NO = '380040')
613600         MOVE '   38' TO HOLD-PROV-CBSA
613700                        P-NEW-CBSA-STAND-AMT-LOC.
613800
613900     IF (P-NEW-PROVIDER-NO = '390130' OR
614000                             '390183' OR
614100                             '390211')
614200         MOVE '   39' TO HOLD-PROV-CBSA
614300                        P-NEW-CBSA-STAND-AMT-LOC.
614400
614500     IF (P-NEW-PROVIDER-NO = '420009' OR
614600                             '420038')
614700         MOVE '   42' TO HOLD-PROV-CBSA
614800                        P-NEW-CBSA-STAND-AMT-LOC.
614900
615000     IF (P-NEW-PROVIDER-NO = '450052' OR
615100                             '450078' OR
615200                             '450243' OR
615300                             '450348')
615400         MOVE '   45' TO HOLD-PROV-CBSA
615500                        P-NEW-CBSA-STAND-AMT-LOC.
615600
615700     IF (P-NEW-PROVIDER-NO = '490004' OR
615800                             '490116')
615900         MOVE '   49' TO HOLD-PROV-CBSA
616000                        P-NEW-CBSA-STAND-AMT-LOC.
616100
616200     IF (P-NEW-PROVIDER-NO = '500030' OR
616300                             '500148')
616400         MOVE '   50' TO HOLD-PROV-CBSA
616500                        P-NEW-CBSA-STAND-AMT-LOC.
616600
616700     IF (P-NEW-PROVIDER-NO = '510039')
616800         MOVE '   51' TO HOLD-PROV-CBSA
616900                        P-NEW-CBSA-STAND-AMT-LOC.
617000
617100 2800-2014-EXIT.   EXIT.
617200
617300 2900-MOVE-PPS-ADDITIONAL-VARS.
617400
617500***FYS 2013 AND 2014 - 1319 BYTES***
617600     IF B-21-DISCHARGE-DATE >= 20121001 AND
617700        B-21-DISCHARGE-DATE <= 20140930
617800        MOVE PPS-VARIABLES-SECTION1-1314
617900            TO PPS-VARIABLES-SECTION1
618000        MOVE PPS-NON-TEMP-RELIEF-PMT-1314
618100            TO PPS-NON-TEMP-RELIEF-PAYMENT
618200        MOVE PPS-NEW-TECH-PAY-ADD-ON-1314
618300            TO PPS-NEW-TECH-PAY-ADD-ON
618400        MOVE 0 TO PPS-ISLET-ISOL-PAY-ADD-ON
618500        MOVE PPS-LOW-VOL-PAYMENT-1314
618600            TO PPS-LOW-VOL-PAYMENT
618700        MOVE PPS-VARIABLES-SECTION3-1314
618800            TO PPS-VARIABLES-SECTION3
618900     END-IF.
619000
619100***FYS 2012 AND EARLIER - 1320 BYTES***
619200     IF B-21-DISCHARGE-DATE < 20121001
619300        MOVE PPS-VARIABLES-SECTION1-PRE13
619400            TO PPS-VARIABLES-SECTION1
619500        MOVE PPS-NON-TEMP-RELIEF-PMT-PRE13
619600            TO PPS-NON-TEMP-RELIEF-PAYMENT
619700        MOVE PPS-NEW-TECH-PAY-ADD-ON-PRE13
619800            TO PPS-NEW-TECH-PAY-ADD-ON
619900        MOVE 0 TO PPS-ISLET-ISOL-PAY-ADD-ON
620000        MOVE PPS-LOW-VOL-PAYMENT-PRE13
620100            TO PPS-LOW-VOL-PAYMENT
620200        MOVE PPS-VARIABLES-SECTION3-PRE13
620300            TO PPS-VARIABLES-SECTION3
620400     END-IF.
620500
620600 2900-EXIT.   EXIT.
620700
620800***************************************************************
620900******       L A S T   S O U R C E   S T A T E M E N T    *****
