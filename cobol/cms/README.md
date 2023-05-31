==============================================================
MU00.@BFN2699.INNDM215
========================================================================

NOTE TO: Shared System Coordinators, MEDICARE Coordinators

SUBJECT: Inpatient Prospective Payment System (IPPS) Pricer System
         Release, Version 2021.5
         CMS Change Request #'s: 12062
         ---------------------------------------------------------
         Pricer Release Date:      February 19, 2021
         Pricer Effective Date:    October 1, 2020

This IPPS Pricer release is the PRODUCTION version for the April 2021
release. The 21.5 IPPS Pricer release revises the latest IPPS Pricer
release which was the PRODUCTION (21.4) release for FY 2021.

It includes the following changes:

  - Revised member, "README", to describe the release.

  - Revised member, "MANIFEST", which lists members included and
    omitted from the release dataset.  CMS is only sending members
    that changed since the previous release.

  - PPDRV215 R  2021.5 REVISED FOR RELEASE
         ** Replaces PPDRV214 **
         --------------------------------------
         Revised Call PPCAL215 Using statement.

  - PPCAL215 R  2021.5 REVISED FOR RELEASE
         ** Replaces PPCAL214 **
         --------------------------------------
         ADJUSTED VALUE OF PPS-DOLLAR-THRESHOLD:

         + MOVE H-OPER-CHARGE-THRESHOLD   TO  PPS-DOLLAR-THRESHOLD.

========================================================================

A partitioned data set (PDS) of the Version 2021.5 code
(MU00.@BFN2699.INNDM215) is at the CMS Data Center for users
to obtain by Connect:Direct (Network DataMover).
    **
The 48 members listed below comprise the Pricer, but only the modules
that changed since the previous release are included in the PDS. The
changed members should be used in conjunction w/ MU00.@BFN2699.INNDM214.

CMS now only sends the program modules in the release. The supporting
files and unchanged or unused program modules are no longer included in
the release. New and/or revised program modules and files will have
--- 2021.5 --- in the description column below.

NAME        DESCRIPTION
-------     ------------------------------------------------------------
CBSA212     2021.2 FY 2021 CBSA & WAGE INDEX TABLE
DRGSX211    2021.2 FY 2021 DRG WEIGHTS TABLE
HOUTI212    2021.2 FY 2021 OUTMIGRATION ADJUSTMENT TABLE
MANIFEST   *2021.5 LIST OF RELEASE MEMBERS
MIDNIGHT    2017.0 TWO MIDNIGHT POLICY ADJUSTMENT FACTOR TABLE
MSAX045     2004.5 INPAT PRICING METROPOLITAN STATISTICAL AREA
NTECH211    2021.2 FY 2021 NEW TECHNOLOGY ADD-ON VARIABLE TABLE
--------    -----------------------------------------------------------
PPCAL006    2000.6 2000 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2000
PPCAL017    2001.7 2001 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2001
PPCAL026    2002.6 2002 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2002
PPCAL038    2003.8 2003 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2003
PPCAL04D    2004.D 2004 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2004
PPCAL059    2005.9 2005 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2005
PPCAL069    2006.9 2006 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2006
PPCAL07B    2007.B 2007 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2007
PPCAL08D    2008.D 2008 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2008
PPCAL09D    2009.D 2009 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2009
PPCAL10O    2010.O 2010 INPAT CALC MODULE - CLAIMS 10/01 TO 03/31/2010
PPCAL10P    2010.P 2010 INPAT CALC MODULE - CLAIMS 04/01 TO 09/30/2010
PPCAL119    2011.9 2011 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2011
PPCAL125    2012.5 2012 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2012
PPCAL135    2013.5 2013 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2013
PPCAL14B    2014.B 2014 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2014
PPCAL156    2015.6 2015 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2015
PPCAL163    2016.3 2016 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2016
PPCAL171    2017.1 2017 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2017
PPCAL182    2018.2 2018 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2018
PPCAL192    2019.2 2019 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2019
PPCAL204    2020.4 2020 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2020
PPCAL215   *2021.5 2021 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/2021
PPCAL884    1988.4 1988 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1988
PPCAL894    1989.4 1989 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1989
PPCAL905    1990.5 1990 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1990
PPCAL915    1991.5 1991 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1991
PPCAL926    1992.6 1992 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1992
PPCAL935    1993.5 1993 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1993
PPCAL944    1994.4 1994 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1994
PPCAL954    1995.4 1995 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1995
PPCAL964    1996.4 1996 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1996
PPCAL974    1997.4 1997 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1997
PPCAL987    1998.7 1998 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1998
PPCAL998    1999.8 1999 INPAT CALC MODULE - CLAIMS 10/01 TO 09/30/1999
--------    -----------------------------------------------------------
PPDRV215   *2021.5 2021 INPAT DRIVER MODULE
PPHOLDAR    2021.2 PPHOLDAR-HOLD-AREA
PREV200     2020.0 FY 2019 FINAL WAGE INDEX TABLE BY PROVIDER
RATEX213    2021.3 FY 2021 RATES TABLE
RUFL200     2020.0 FY 2020 CBSA & RURAL FLOOR WAGE INDEX TABLE
README     *2021.5 DESCRIPTION OF RELEASE UPDATES
