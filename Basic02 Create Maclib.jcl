//BASIC02 JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A ,TYPRUN=SCAN
//S1  EXEC PGM=IEBUPDTE,PARM=NEW
//SYSPRINT DD SYSOUT=C
//SYSUT2   DD DSN=HERCEL.BASIC.ASM,DISP=OLD
//SYSIN DD *
./  ADD NAME=BASALO
         TITLE '**** BASALO - SVC 99 SUPPORT FOR BASIC360 ****'
         PUNCH ' ALIAS BASALOP'
***********************************************************************
*                                                                     *
*   BASALO - BASIC ALLOCATE                                           *
*            ---   -- -                                               *
*   THIS SUBROUTINE IS CALLED BY BASIC360 TO DYNAMICLY ALLOCATE A     *
*   MEMBER OF A PDS TO A GIVEN DDNAME.                                *
*   IT IS EQUIVALENT TO JCL STATEMENT:                                *
*                                                                     *
*       //DDNAME DD DSN=DSNAME(MEMBER),DISP=(OLD,KEEP)                *
*                                                                     *
*   THE CALLER CAN THEN OPEN A FILE USING 'DDNAME' TO EITHER          *
*   READ 'MEMBER' OR CREATE/UPDATE PDS MEMBER 'MEMBER'.               *
*                                                                     *
*   A SINGLE PARM STRUCTURE IS PASSED FORMATTED AS FOLLOWS:           *
*      1)  BLOCK NAME        8 CHARACTERS.                            *
*      2)  FUNCTION CODE     4 CHARACTERS - ALLO OR FREE.             *
*      3)  SVC 99 RET CD     2 BYTES                                  *
*      4)  SVC 99 INFO CD    2 BYTES                                  *
*      5)  REQUESTED DDNAME  8 CHARACTERS                             *
*      6)  REQUESTED DSNAME  44 CHARACTERS                            *
*      7)  REQUESTED MEMBER  8 CHARACTERS                             *
*                                                                     *
*   RETURNS:                                                          *
*      1)  BLOCK NAME IS CHANGED TO HOLD CONTROL INFO.  DO NOT        *
*          AFTER FIRST CALL.                                          *
*      2)  SVC 99 RETURN CODES.  COMMON RETURN CODES (DECIMAL)        *
*          RET CD  INFO CD   MEANING                                  *
*          ------  -------                                            *
*              0      0      SUCCESSFUL                               *
*             33      ?      DATASET IN USE                           *
*           1056      ?      ATTEMPT TO FREE OPEN DATASET             *
*           1096      ?      DATASET ALREADY EXISTS                   *
*                                                                     *
*   REVISION HISTORY                                                  *
*   ------------------                                                *
*   V1.0.0  09/26/2017    INITIAL VERSION.                            *
*                                                                     *
*                                                                     *
***********************************************************************
**                                                                    *
**   EDYNAL - (C) COPYRIGHT 2017 BY EDWARD G LISS                     *
**                ALL RIGHTS RESERVED                                 *
**                                                                    *
**            THIS WORK MAY BE REDISTRUTED UNMODIFIED FOR             *
**            NON COMMERCIAL USE AS LONG AS THE COPYRIGHT             *
**            HOLD IS GIVEN CREDIT FOR THE ORIGINAL WORK.             *
**                                                                    *
***********************************************************************
BASALO   ESTART TYPE=START,DESC='BASIC ALLO',VER='1.0.0',BASE=R12,     *
               REGS=YES,PLIF=BASALOP
         BNE   NOTPLI
         L     R1,0(,R1)               GET PLI PARMLIST OF DDB
NOTPLI   EQU   *
         L     R11,0(,R1)              GET ADDR OF PARM.
COMM     EQU   *
         USING PARMS,R11
         USING DBLOCK,R2
         CLC   PFUNC,=C'ALLO'
         BE    FALLOC
         CLC   PCNTRLB(4),=4X'00'      IS BLOCK INITIALIZED?
         BE    INITOK                  IF SO, BRANCH
         WTO   'BASALO-BLOCK NOT ALLOC',ROUTCDE=(2,11)
         ABEND 1234
         B     ERREXIT
INITOK   EQU   *
         L     R2,PCNTRLB+4            GET BLOCK ADDR
         CLC   PFUNC,=C'FREE'
         BE    FFREE
         WTO   'BASALO-INVALID FUNCTION',ROUTCDE=(2,11)
         ABEND 1234
***********************************************************************
*                                                                     *
*    FALLO - ALLOCATE MEMBER                                          *
*                                                                     *
*    IF BLOCK IS ALLOCATED, ABEND                                     *
*    OTHERWISE CREATE THE BLOCK AND ALLOCATE THE MEMBER               *
*                                                                     *
***********************************************************************
FALLOC   EQU   *
         CLC   PCNTRLB(4),=4X'00'      IS BLOCK INITIALIZED?
         BNE   INITOK2
         WTO   'BASALO-ATTEMPT TO RE-ALLOCATE A BLOCK',ROUTCDE=(2,11)
         ABEND 1234
INITOK2  EQU   *
*    GETMAIN TO ALLOCATE BLOCK, MOVE MODEL BLOCK TO IT, MOVE THE      *
*    DDNAME FROM THE PARM TO THE DCB, SAVE THE BLOCK ADDR INTO        *
*    BYTES 5-8 OF DDNAME AND SET BYTES 1-4 TO ZEROS.                  *
         LA    R2,DBLOCKL
         GETMAIN R,LV=(R2)
         LR    R2,R1                  SET BR FOR WORK BLOCK
*
         USING DBLOCK,R2
         MVC   DHOLDNM,PCNTRLB        SAVE ORIGINAL DDNAME
         MVI   PCNTRLB,X'00'
         MVC   PCNTRLB+1(L'PCNTRLB-1),PCNTRLB
         ST    R1,PCNTRLB+4
         LA    R1,DBLOCKL
         ST    R1,DLEN
*
         EDYNAL INITRB,RBAREA=DBLOCK,VERB=S99VRBAL
         EDYNAL ADDTU,TUKEY=DALDDNAM,TXT=PDDN
         EDYNAL ADDTU,TUKEY=DALDSNAM,TXT=PDSNAME
         CLC    PNAME,=8C' '      SKIP IF MEMBER IS BLANK
         BE     SKIPMEM1
         EDYNAL ADDTU,TUKEY=DALMEMBR,TXT=PNAME
SKIPMEM1 EQU   *
         EDYNAL ADDTU,TUKEY=DALSTATS,TXT=DISPOLD
         EDYNAL ADDTU,TUKEY=DALNDISP,TXT=DISPKEEP
         EDYNAL EXEC,RC=PRC,RSN=PREASCD
         B     EXIT
DISPOLD  DC    X'01'
DISPKEEP DC    X'08'
         EJECT
***********************************************************************
*                                                                     *
*    FFREE - DEALLOCATE MEMBER                                        *
*                                                                     *
*    IF BLOCK IS NOT ALLOCATED, ABEND                                 *
*    A FREEMAIN WILL BE ISSUED TO RELEASE THE BLOCK STORAGE           *
*                                                                     *
***********************************************************************
FFREE    EQU   *
         CLC   PCNTRLB(4),=4X'00'      IS BLOCK INITIALIZED?
         BE    FREEOK
         WTO   'BASALO-ATTEMPT TO FREE UNALOCATED BLOCK',              *
               ROUTCDE=(2,11)
         ABEND 1234
FREEOK   EQU   *
         EDYNAL INITRB,RBAREA=DBLOCK,VERB=S99VRBUN
         EDYNAL ADDTU,TUKEY=DALDDNAM,TXT=PDDN
         EDYNAL ADDTU,TUKEY=DALDSNAM,TXT=PDSNAME
         CLC    PNAME,=8C' '      SKIP IF MEMBER IS BLANK
         BE     SKIPMEM2
         EDYNAL ADDTU,TUKEY=DALMEMBR,TXT=PNAME
SKIPMEM2 EQU   *
         EDYNAL EXEC,RC=PRC,RSN=PREASCD
         MVC   PCNTRLB,DHOLDNM      RESTORE ORIGINAL DDNAME
         B     EXIT
ERREXIT  EQU   *
EXIT     EQU   *
         ERETURN RC=0
         LTORG
*
*   PARM LIST
*
PARMS    DSECT
PCNTRLB  DS    CL8         DDNAME OF CONTROL BLOCK.  BYTES 1-4=ZERO
*                                                    ACTIVE BLOCK
*                                                    BYTES 5-8=ADDR
*                                                    OF CONTROL BLOCK
PFUNC    DS    CL4         FUNCTION CODE
PRC      DS    H           RETURN CODE
PREASCD  DS    H           REASON CODE
PDDN     DS    CL8         DDNAME
PDSNAME  DS    CL44        DSNAME
PNAME    DS    CL8         MEMBER NAME
*
*   WORKING CONTROL BLOCK
*
         EDYNAL DCLRB,DSECT=DCNTRLB,TUPTRS=6,TUSIZE=256
         END
./  ADD NAME=EDYNAL
         MACRO
&LAB     EDYNAL &REQ,&DSECT=,&TUPTRS=,&TUSIZE=,                        *
               &RBAREA=,&VERB=,&TUKEY=,&TXT=,&RC=,&RSN=
***********************************************************************
.**********************************************************************
.*                                                                    *
.*   EDYNAL - (C) COPYRIGHT 2017 BY EDWARD G LISS                     *
.*                ALL RIGHTS RESERVED                                 *
.*                                                                    *
.*            THIS WORK MAY BE REDISTRUTED UNMODIFIED FOR             *
.*            NON COMMERCIAL USE AS LONG AS THE COPYRIGHT             *
.*            HOLD IS GIVEN CREDIT FOR THE ORIGINAL WORK.             *
.*                                                                    *
.**********************************************************************
.*                                                                    *
.*   EDYNAL - DYNAMIC FILE ALLOCATION MACRO                           *
.*                                                                    *
.*   THIS MACRO CAN BE USED TO GENERATE THE REQUEST BLOCKS FOR USING  *
.*   THE DYNAMIC ALLOCATION FACILITIES (SVC 99) AS WELL AS EXECUTING  *
.*   THE REQUESTS.                                                    *
.*                                                                    *
.*   USAGE:    NOTE:ALL KEYWORDS ARE REQUIRED                         *
.*       EDYNAL DCLRB,DSECT=,TUPTRS=,TUSIZE=                          *
.*       EDYNAL INITRB,RBAREA=,VERB=                                  *
.*       EDYNAL ADDTU,&TUKEY=,&TXT=                                   *
.*       EDYNAL EXEC,RC=,RSN=                                         *
.*                                                                    *
.*   1)  CREATE THE DATA AREAS                                        *
.*       EDYNAL DCLRB,DSECT=,TUPTRS=,TUSIZE=                          *
.*          DSECT IS A SYMBOLIC NAME FOR THE STORAGE AQUIRED FOR      *
.*                CONTAINING THE RB.                                  *
.*          TUPTRS IS A NUMBER SPECIFING HOW MANY TEXT UNITS THAT     *
.*                SPACE SHOULD BE RESERVED FOR.                       *
.*          TUSIZE IS A NUMBER SPECIFING HOW MANY BYTES SHOULD BE     *
.*                RESERVED FOR THE TEXT UNITS.  TEXT UNITS ARE        *
.*                VARIABLE LENGTH DEPENDING ON WHAT IS REQUESTED.     *
.*       NOTE THAT DCLRB NEED ONLY BE REQUESTED 1 TIME AND CAN BE     *
.*            REUSED.                                                 *
.*                                                                    *
.*   2)  INITIALIZE THE REQUEST BLOCK.                                *
.*       EDYNAL INITRB,RBAREA=,VERB=                                  *
.*           RBAREA IS THE NAME OF THE DSECT FROM THE DCLRB           *
.*           VERB IS ONE OF THE ALLOCATION KEYS DEFINED IN            *
.*                MACRO IEFZB4D2                                      *
.*                                                                    *
.*   3)  ADD TEXT UNIT TO THE REQUEST BLOCK.                          *
.*       EDYNAL ADDTU,TUKEY=,TXT=                                     *
.*           TUKEY IS STATES A PART OF THE REQUEST.  SEE IBM          *
.*                WEBSITE OR MANUAL FOR ALL THE KEYS AND WHAT         *
.*                ARE VALUES FOR TXT.                                 *
.*                                                                    *
.*   4)  EXECUTE THE REQUEST                                          *
.*       EDYNAL EXEC,RC=,RSN=                                         *
.*            RC AND RSN ARE BOTH 2 BYTE FEEDBACK FIELDS.             *
.*            IF RC=X'0000', REQUEST IS SUCCESSFUL AND RSN IS         *
.*                NOT USED.                                           *
.*            RSN VALUES VARYING DEPENDING ON THE VALUE OF RC.  SEE   *
.*                WEBSITE OR MANUAL FOR ALL THE RC AND RSN MEANINGS.  *
.**********************************************************************
.*                                                                    *
.*   REVISION HISTORY                                                 *
.*   -------------------                                              *
.*   V1.0.0   09/29/2017    INITIAL VERSION                           *
.*                                                                    *
.**********************************************************************
         LCLC  &ERRSW
&ERRSW   SETC  'N'
         AIF   (T'&REQ EQ 'O').NOREQ
         AIF   ('&REQ' EQ 'DCLRB').DCLRB
         AIF   ('&REQ' EQ 'INITRB').INITRB
         AIF   ('&REQ' EQ 'ADDTU').ADDTU
         AIF   ('&REQ' EQ 'EXEC').EXEC
         MNOTE 16,'INVALID REQ CODE'
         MEXIT
.NOREQ   MNOTE 16,'REQ IS OMITTED'
         MEXIT
.DCLRB   ANOP
.****************************************************************
.*     DCLRB DETECTED
         AIF   (T'&DSECT NE 'O').NED001
         MNOTE 12,'DSECT MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED001  ANOP
         AIF   (T'&TUPTRS NE 'O').NED002
         MNOTE 12,'TUPTRS MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED002  ANOP
         AIF   (T'&TUSIZE NE 'O').NED003
         MNOTE 12,'TUSIZE MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED003  ANOP
         AIF   ('&ERRSW' EQ 'Y').EXIT
DBLOCK   DSECT
DLEN     DS    F              BLOCK LENGTH (ONLY USED IF GET/FREE MAIN)
DHOLDNM  DS    CL8            SAVE AREA FOR NAME
DBOUNDS  DS    0A
DTUPTRCU DS    A              CURRENT TUPTR
DTUPTRMA DS    A              MAX TUPRT
DTUNITCU DS    A              CURRENT TEXT UNIT
DTUNITMA DS    A              MAX TEXT UNIT
DBOUNDSL EQU   *-DBOUNDS
DRBP     DS    A              <- S99RBP  DSECT
DRB      DS    CL20           <- S99RB   DSECT
DTUPTR   DS    &TUPTRS.A      <- S99TUPL DSECT
DTXTUNIT DS    CL&TUSIZE      <- S99UNIT AREA
DBLOCKL  EQU   *-DBLOCK
DTUPTRL  EQU   &TUPTRS*L'DTUPTR
DTXTUNIL EQU   &TUSIZE
         IEFZB4D0
         IEFZB4D2
         AGO   .EXIT
.****************************************************************
.*     INITRB DETECTED
.INITRB  ANOP
         AIF   (T'&RBAREA NE 'O').NED010
         MNOTE 12,'RBAREA MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED010  ANOP
         AIF   (T'&VERB NE 'O').NED011
         MNOTE 12,'VERB MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED011  ANOP
         AIF   ('&ERRSW' EQ 'Y').EXIT
         LA    R1,&RBAREA      SET BR FOR &RBAREA
         USING DBLOCK,R1
         LA    R15,DTUPTR      GET START ADDR OF DTUPTR TABLE
         ST    R15,DTUPTRCU        AND SAVE IT
         LA    R15,DTUPTRL(,R15)   CALC END ADDR OF TABLE
         ST    R15,DTUPTRMA        AND SAVE IT
         LA    R15,DTXTUNIT    GET START ADDR OF DTXTUNIT TABLE
         ST    R15,DTUNITCU        AND SAVE IT
         LA    R15,DTXTUNIL(,R15)   CALC END ADDR OF TABLE
         ST    R15,DTUNITMA            AND SAVE IT
*
         LA    R15,DRB          GET ADDR OF DRB
*
         LA    R1,DRBP
         USING S99RBP,R1
         ST    R15,S99RBPTR      SET RB POINTER TO DRB ADDR
         OI    S99RBPTR,X'80'      AND TURN ON HIGH ORDER BIT
*
         L     R1,S99RBPTR         GET ADDR OF RB AND SET BR
         USING S99RB,R1
*
         MVI   S99RBLN,X'14'       SET RB LENGTH TO 20
         MVI   S99VERB,&VERB       SET UP ALLOCATE
         MVI   S99FLG11,X'00'
         MVI   S99FLG12,X'00'
         MVI   S99RSC,X'00'        CLEAR RSN CODES
         MVC   S99RSC+1(L'S99RSC-1),S99RSC
         LA    R15,S99RBEND        SET ADDRESS OF THE DTU PTRS
         LA    R15,0(,15)
         ST    R15,S99TXTPP
         MVI   S99RSV01,X'00'      CLEAR RESERVED AREA
         MVC   S99RSV01+1(L'S99RSV01-1),S99RSV01
         MVI   S99FLAG2,X'00'      CLEAR AUTH FLAGS
         MVC   S99FLAG2+1(L'S99FLAG2-1),S99FLAG2
         AGO   .EXIT
.****************************************************************
.*     ADDTU DETECTED
.ADDTU   ANOP
         AIF   (T'&TUKEY NE 'O').NED020
         MNOTE 12,'TUKEY MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED020  ANOP
         AIF   (T'&TXT NE 'O').NED021
         MNOTE 12,'TXT MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED021  ANOP
*
         AIF   ('&ERRSW' EQ 'Y').EXIT
         L     R15,DTUPTRCU        GET CURRENT TUPTR
         C     R15,DTUPTRMA        IS IT GREATER OR EQU THE MAX?
         BL    TUP&SYSNDX
         WTO   'TUPTR TABLE FULL',ROUTCDE=(2,11)
         ABEND 1234
TUP&SYSNDX EQU  *
         LA    R15,4(,R15)
         ST    R15,DTUPTRCU        SAVE UPDATED TUPTR
         USING S99TUPL,R15
         L     R14,DTUNITCU        GET ADDR OF NEXT AVAIL TXT UNIT
         ST    R14,S99TUPTR        ADD TXT UNIT ADDR TO TUPTR TABLE
*
         USING S99TUNIT,R14
         MVC   S99TUKEY,=AL2(&TUKEY)
         MVC   S99TUNUM,=AL2(1)
         MVC   S99TULNG,=AL2(L'&TXT)
         MVC   S99TUPAR(L'&TXT),&TXT
         DROP  R14
         LA    R14,6+L'&TXT.(,R14)
         ST    R14,DTUNITCU
         DROP  R15
         AGO   .EXIT
.****************************************************************
.*     EXEC DETECTED
.EXEC    ANOP
         AIF   (T'&RC NE 'O').NED030
         MNOTE 12,'RC MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED030  ANOP
         AIF   (T'&RSN NE 'O').NED031
         MNOTE 12,'TXT MUST BE SPECIFIED'
&ERRSW   SETC  'Y'
.NED031  ANOP
         AIF   ('&ERRSW' EQ 'Y').EXIT
         L     R15,DTUPTRCU        GET LAST TUPTR ADDED
         OI    0(R15),X'80'        TURN ON HIGH ORDER BIT
         LA    R1,DRBP             GET RB PTR
         SVC   99                  DO THE SVC 99
         L     R1,DRBP
         USING S99RB,R1
         MVC   &RC,S99ERROR
         MVC   &RSN,S99INFO
         DROP  R1
.EXIT    MEXIT
         MEND
./  ADD NAME=EREGS
         MACRO
&NAME    EREGS
.* SET UP REGISTER EQU
         AIF   (T'&NAME EQ 'O').NONAME
&NAME    EQU   *
.NONAME  ANOP
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
         MEND
./  ADD NAME=ERETURN
         MACRO
&NAME    ERETURN &RC=
.* RETURN TO SYSTEM WITH RETURN CODE
&NAME    L     13,4(0,13)          RESET TO CALLERS SAVE AREA
         RETURN (14,12),RC=&RC
         MEND
./  ADD NAME=ESTART
         MACRO
&NAME    ESTART &TYPE=CSECT,&DESC=,&VER=,&BASE=,&REGS=NO,              *
               &PLIF=
         AIF   (T'&TYPE EQ 'O').TYPERR
         AIF   (T'&VER EQ 'O').VERR
         AIF   (T'&DESC EQ 'O').DERR
         AIF   ('&TYPE' EQ 'CSECT').TYPEC
         AIF   ('&TYPE' EQ 'START').TYPES
.TYPERR  MNOTE 12,'*** TYPE MUST BE CSECT OR START ***'
         MEXIT
.VERR    MNOTE 12,'*** VER OMITTED ***'
         MEXIT
.DERR    MNOTE 12,'*** DESC OMITTED ***'
         MEXIT
.TYPEC   ANOP
&NAME    CSECT
         AGO   .OK
.TYPES   ANOP
&NAME    START
.OK      ANOP
         AIF   (T'&REGS EQ 'O').REGOK
         AIF   ('&REGS' EQ 'YES').REGOK
         AIF   ('&REGS' EQ 'NO').REGOK
         MNOTE 12,'*** REGS INVALID ***'
         MEXIT
.REGOK   ANOP
*
         AIF   (T'&PLIF EQ 'O').NOPL1
         MVI   18(15),C'N'            SET NO PLI ENTRY POINT
         B     12(15)                 SKIP HEADER STUFF
         ENTRY &PLIF
&PLIF    MVI   10(15),C'Y'            SET PL1 ENTRY POINT
         BALR  15,0
ID1&SYSNDX B     ID4&SYSNDX.(15)        SKIP HEADER STUFF
ID5&SYSNDX DS   C
IDP&SYSNDX EQU  ID5&SYSNDX-ID1&SYSNDX
         AGO   .NOPL12
.NOPL1   ANOP
ID1&SYSNDX B     ID4&SYSNDX.(15)       BRANCH AROUND IDENT CONSTANTS
.NOPL12  ANOP
         DC    AL1(ID3&SYSNDX-ID2&SYSNDX)
ID2&SYSNDX DC    CL8'&NAME'
         DC    C&VER
         DC    C' &SYSDATE &SYSTIME - '
         DC    C&DESC
         DS    0F
IDS&SYSNDX EQU   *-ID1&SYSNDX
         DS    18F
ID3&SYSNDX DS    0H
         AIF   (T'&REGS EQ 'O').NOREG
         AIF   ('&REGS' EQ 'NO').NOREG
         EREGS
.NOREG   ANOP
ID4&SYSNDX EQU   *-ID1&SYSNDX
*
         STM   14,12,12(13)
*
         LR    5,13                COPY CALLER'S SAVEAREA ADDR
         LA    13,IDS&SYSNDX.(15)  ESTABLISH MY SAVEAREA
         ST    5,4(,13)            BACK CHAIN SAVE AREAS
         ST    13,8(,5)            FORWARD CHAIN SAVE AREAS
         AIF   (T'&BASE EQ 'O').NOBASE
         BALR  &BASE.,0            ESTABLISH BASE REG VALUE
         USING *,&BASE
.NOBASE  ANOP
         AIF   (T'&PLIF EQ 'O').SKIPLI
         CLI   IDP&SYSNDX.(15),C'Y'     SET CC BASED ON PLI INDICATOR
.SKIPLI  ANOP
         MEND
./  ADD NAME=GETDIR
         MACRO ,
&NAME    GETDIR &DCB,&EODAD=
**********************************************************************
*
*    THE "GETDIR" MACRO ALLOWS YOU TO READ A PDS DIRECTORY
*    SEQUENTIALLY, AS FOLLOWS:
*
*        IT WORKS IN "LOCATE" MODE; AFTER EACH INVOKATION, THE
*        ADDRESS OF A DIRECTORY ENTRY (MEMBER NAME, TTR, ETC) IS
*        RETURNED IN R1.
*
*        IT USES A BPAM DCB (DSORG=PO,MACRF=R), NOT A QSAM DCB
*        (WHICH WOULD HAVE DSORG=PS).  THIS ALLOWS YOU TO READ THE
*        DIRECTORY AND THE MEMBERS WITH THE SAME DCB.
*
*        R1 IS SET TO ZERO WHEN THE END OF THE DIRECTORY IS REACHED.
*        THE "EODAD" KEY-WORD OF THE "GETDIR" MACRO MAY BE USED TO
*        SPECIFY THE ADDRESS OF THE END-OF-DIRECTORY ROUTINE.
*        THE EODAD KEY-WORD OF THE DCB IS ONLY USED AT END-OF-MEMBER.
*
*        THE "GETDIR" MACRO ONLY PROCESSES THE FIRST PDS IN A
*        CONCATENATION, AND WILL NOT WORK WITH A PDSE.
*
**********************************************************************
*
*   THE ORIGINAL SOURCE WAS OBTAINED FROM CBT THAT COME WITH TURNKEY
*           PDS : CBT429.FILE183(GETDIR)
*
*   V1        ORIGINAL SOURCE
*   V2   03/16/2010  ED LISS, CEDAR LAKE EMPORIUM LLC.
*        CLEANED UP AND SOME DOCUMENTATION ADDED.
*
**********************************************************************
         GBLC  &GETDIRD
&NAME    IHBINNRA &DCB
         L     R15,=A(@GETDIR)         ADDR OF "GETDIR" RTNE
         BALR  R14,R15                 GET A DIRECTORY ENTRY
         AIF   (T'&EODAD EQ 'O').GEN
         LTR   R1,R1                   END OF DIRECTORY?
         BZ    &EODAD                  YES, GO AWAY
.GEN     AIF   (T'&GETDIRD EQ 'Y').MEND
&GETDIRD SETC  'Y'
.*GEN     AIF   (D'GETDIR).MEND        D' NOT SUPPORTED BY IFOX00
.**********************************************************************
.*
.*       GETDIR SUB-ROUTINE
.*
.**********************************************************************
@GETDIR  CSECT
         SAVE  (14,12),,'GETDIR 840510'
         BALR  R2,0
         USING *,R2
         LR    R3,R1                   ADRESSE DU DCB
         ST    R13,@GETDSAV+4
         LA    R13,@GETDSAV
         LA    R5,X'000100'            TT=00, R=1, N=0
         ICM   R1,B'1111',@GETDR1      CURRENT POS IN BUFFER
         BZ    @GETD22                 PICK UP PARM LIST OFFSET
         LA    R14,X'1F'               MASQUE POUR "AND"
         IC    R0,11(,R1)              "C" BYTE
         NR    R14,R0                  NOMBRE DE HALFWORDS
         LA    R14,12(R14,R14)         LONGUEUR DU POSTE DANS R14
         LH    R15,@GETDBUF            LONGUEUR UTILISEE
         LA    R15,@GETDBUF-1(R15)     ADRESSE DERNIER OCTET UTILISE
         BXLE  R1,R14,@GETD24
         AL    R5,@GETDR5              INCREMENTER LE NUMERO D'ENREG
@GETD22  ST    R5,@GETDR5              TTRN DU BLOC SUIVANT
         POINT (R3),@GETDR5            POINTER SUR LE BON BLOC
         LH    R4,62(,R3)              SAUVER DCBBLKSI
         MVI   62(R3),1                BLKSIZE=256
         MVI   63(R3),0                BLKSIZE=256
         READ  @GETDECB,SF,(R3),@GETDBUF,'S'
         STH   R4,62(,R3)              RESTAURER DCBBLKSI
         CHECK @GETDECB
         LA    R1,@GETDBUF+2           1ER POSTE
@GETD24  CLI   0(R1),X'FF'             FIN DE BLOC?
         BNE   @GETD29                 NON, RENDRE L'ADRESSE
         SLR   R1,R1                   FIN-DE-REPERTOIRE
@GETD29  ST    R1,@GETDR1              GARDER L'ADRESSE
         L     R13,4(,R13)
         L     R14,12(,R13)            ADRESSE DE RETOUR
         LM    R2,R12,28(R13)          R2-R3
         BR    R14
@GETDR1  DS    A(@GETDBUF+2)           POINTEUR COURANT DANS LE BUFFER
@GETDR5  DS    F                       TTRN DU BLOC COURANT
@GETDSAV DS    18F
@GETDBUF DS    H,254X
&SYSECT  CSECT
.MEND    MEND
./  ADD NAME=GETPDS
GETPDS   TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM DOCUMENTATION'
         PUNCH ' ALIAS NCZ93205'
         PUNCH ' ALIAS GETPDSP'
***********************************************************************
*                                                                     *
* GETPDS - SUBROUTINE TO READ PDS DIRECTORIES AND MEMBERS             *
*          FORMERLY KNOWN AS NCZ93205                                 *
*                                                                     *
* V1.0.0 - MODULE RENAMED FROM NCZ93205.  A ALIAS WAS ADDED FOR       *
*          BACKWARD COMPATABILITY WITH OLDER VERSIONS.                *
*        = TWO NEW FUNCTIONS WERE ADDED - START SEQUENTIAL DIRECTORY  *
*          RETRIEVE AND GET NEXT MEMBER NAME.                         *
*        - SUPPORT FOR PL/1(F) ADDED.  SINCE PL/1 DOES NOT SUPPORT    *
*          RETURN CODE, A 4TH PARM FOR RETURN_CODE MUST BE INCLUDED.  *
*                                                                     *
* V1.1.0 - BUG FIX.  IF THE PDS HAD A LARGE BLKSIZE (>3200), THIS     *
*          ROUTINE WOULD ABORT WITH A S0C4. FOUND THE EX INSTRUCTION  *
*          WAS CONSISTANTLY MOVE 1 EXTRA BYTE OF DATA.                *
*                                                                     *
* V1.2.0 - BAND AID PATCH FOR BUG IN PDSDIR MACRO.  CONTROL INFO      *
*          IS NOT INITIALIZED ON AN OPEN.  THIS RESULTS IN AN SOC4.   *
*          APPLIED PATCH TO ZERO OUT CONTROL FIELDS.                  *
*        - REVISED DOCUMENTATION FOR COBOL CALLERS AND ADDED          *
*          DOCUMENTATION FOR PL/I CALLERS.                            *
*                                                                     *
***********************************************************************
***********************************************************************
*                                                                     *
* ID:          GETPDS  -  SUBROUTINE TO READ PDS MEMBERS              *
*                                                                     *
* NCZ93205.1 PROGRAM DESCRIPTION                                      *
*                                                                     *
* THIS PROGRAM CAN BE CALLED AS A SUBROUTINE FROM ASSEMBLER, COBOL OR *
* COBOL OR PL/I TO PROVIDE READ ACCESS TO MEMBER(S) OF A PDS. THE     *
* MEMBER NAME(S) MAY BE SPECIFIED DYNAMICALLY VIA THE PARAMETERS.     *
* THUS THIS PROGRAM ENABLES A CALLING PROGRAM TO ACCESS MANY MEMBERS  *
* OF A PDS WHEN THE NAMES OF THE MEMBERS ARE NOT KNOWN UNTIL EXECUTION*
* TIME.                                                               *
*                                                                     *
* ONLY COMBINATIONS OF RECFM = F/B/A ARE SUPPORTED.                   *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.2 USER INSTRUCTIONS FOR COBOL                              *
*                                                                     *
* PARAMETERS:                                                         *
* ----------                                                          *
* THE PROGRAM OBSERVES A STANDARD OS LINKAGE. IT SHOULD ALWAYS BE     *
* CALLED WITH 3 PARAMETERS WHICH ARE:                                 *
*                                                                     *
* P1 - REQUEST CODE      FULLWORD BINARY  COBOL PIC S9(8) COMP.       *
*      VALUES:                                                        *
*      0   -  OPEN DDNAME 'PDS'                                       *
*      1   -  START SEQUENTIAL PROCESSING OF DIRECTORY                *
*      4   -  LOCATE MEMBER (DOES NOT READ MEMBER)                    *
*      5   -  GET NEXT MEMBER NAME (DOES NOT READ MEMBER)             *
*      8   -  READ NEXT RECORD IN CURRENT MEMBER                      *
*      12  -  CLOSE DDNAME 'PDS'                                      *
*                                                                     *
* P2 - MEMBER NAME      CHARACTER STRING COBOL PIC X(8).              *
*      CONTAINS NAME OF MEMBER WHEN P1 = 4                            *
*                                                                     *
* P3 - INPUT AREA       CHARACTER STRING. IT IS THE RESPONSIBLITY OF  *
*      WHEN P1 = 8                        THE CALLING PROGRAM TO      *
*                                         ENSURE THAT THIS AREA WILL  *
*                                         ACCOMMODATE THE LONGEST     *
*                                         RECORD TO BE READ.          *
*                                                                     *
* RETURN CODE -                                                       *
*      VALUES:                                                        *
*      0   - REQUESTED FUNCTION COMPLETED WITHOUT ERROR               *
*      4   - REQUESTED FUNCTION DID NOT COMPLETE FOR REASON:          *
*            WHEN P1 = 0  FILE COULD NOT BE OPENED                    *
*                      1  FILE COULD NOT BE OPENED                    *
*                      4  MEMBER WAS NOT FOUND                        *
*                      5  END OF DIRECTORY                            *
*                      8  END OF FILE ON CURRENT MEMBER               *
*      8   - SERIOUS ERROR. PROCESSING SHOULD BE TERMINATED.          *
*                                                                     *
* EXAMPLE CALL FROM COBOL:                                            *
*                                                                     *
*      CALL 'GETPDS' USING FUNC, MEMBER, INPUT-AREA.                  *
*      IF RETURN-CODE = ZERO GO TO .....                              *
*                                                                     *
* THE SPECIAL REGISTER 'RETURN-CODE' SHOULD BE CLEARED TO ZERO BY     *
* THE APPLICATION PROGRAM BEFORE PROGRAM TERMINATION OTHERWISE        *
* IT MAY BE PROPAGATED UPWARDS TO OS WITH A NON ZERO VALUE.           *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.2 USER INSTRUCTIONS FOR PL/I                               *
*                                                                     *
* PARAMETERS:                                                         *
* ----------                                                          *
* THE PROGRAM OBSERVES A STANDARD OS LINKAGE. IT SHOULD ALWAYS BE     *
* CALLED WITH 4 PARAMETERS WHICH ARE DECLARED STATIC.                 *
*                                                                     *
* P1 - REQUEST CODE      FULLWORD BINARY  FIXED BINARY(31).           *
*      VALUES:                                                        *
*      0   -  OPEN DDNAME 'PDS'                                       *
*      1   -  START SEQUENTIAL PROCESSING OF DIRECTORY                *
*      4   -  LOCATE MEMBER (DOES NOT READ MEMBER)                    *
*      5   -  GET NEXT MEMBER NAME (DOES NOT READ MEMBER)             *
*      8   -  READ NEXT RECORD IN CURRENT MEMBER                      *
*      12  -  CLOSE DDNAME 'PDS'                                      *
*                                                                     *
* P2 - MEMBER NAME      CHARACTER STRING CHAR(8).                     *
*      CONTAINS NAME OF MEMBER WHEN P1 = 4                            *
*                                                                     *
* P3 - INPUT AREA       CHARACTER STRING. IT IS THE RESPONSIBLITY OF  *
*      WHEN P1 = 8                        THE CALLING PROGRAM TO      *
*                                         ENSURE THAT THIS AREA WILL  *
*                                         ACCOMMODATE THE LONGEST     *
*                                         RECORD TO BE READ.          *
*                                                                     *
* P4 - RETURN CODE      FULLWORD FIXED BINARY(31).                    *
*      WHEN P1 = 8                        PL/I (F) DOES NOT HAVE      *
*                                         A RETURN-CODE REGISTER      *
*                                         LIKE COBOL.                 *
*                                                                     *
* RETURN CODE -                                                       *
*      VALUES:                                                        *
*      0   - REQUESTED FUNCTION COMPLETED WITHOUT ERROR               *
*      4   - REQUESTED FUNCTION DID NOT COMPLETE FOR REASON:          *
*            WHEN P1 = 0  FILE COULD NOT BE OPENED                    *
*                      1  FILE COULD NOT BE OPENED                    *
*                      4  MEMBER WAS NOT FOUND                        *
*                      5  END OF DIRECTORY                            *
*                      8  END OF FILE ON CURRENT MEMBER               *
*      8   - SERIOUS ERROR. PROCESSING SHOULD BE TERMINATED.          *
*                                                                     *
* EXAMPLE CALL FROM COBOL:                                            *
*                                                                     *
*      CALL PDSGETP(FUNC, MEMBER, INPUT-AREA, RETURN_CODE);           *
*      IF RETURN_CODE = ZERO          ..                              *
*                                                                     *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.3 MESSAGES                                                 *
*                                                                     *
* NONE.                                                               *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.4 ABEND CODES                                              *
*                                                                     *
* NONE. IT IS THE RESPOSIBILITY OF THE CALLING PROGRAM TO TERMINATE   *
* AFTER A SERIOUS ERROR CONDITION.                                    *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.5 IMPLEMENTATION METHOD                                    *
*                                                                     *
* MOVE TO MAC1.ASM AND PASMAL.                                        *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.6 AMENDMENT HISTORY                                        *
*                                                                     *
* MAINTAIN THE RECORD BELOW.                                          *
*                                                                     *
* CHANGE HISTORY:                                                     *
* DATE      INITS VERSION COMMENTS                                    *
*                                                                     *
* 17 NOV 82 RH    V01     CREATED                                     *
*                                                                     *
* SUGGESTED ENHANCEMENTS:                                             *
*                                                                     *
* NONE YET.                                                           *
*                                                                     *
* DD MMM YY WHO : COMMENTS                                            *
*                                                                     *
*****                                                             *****
         EJECT
*****                                                             *****
*                                                                     *
* NCZ93205.7 MODULARISATION DETAILS                                   *
*                                                                     *
* SIMPLE LINEAR STRUCTURE.                                            *
* USES OPEN, FIND, CLOSE, READ, CHECK (BSAM).                         *
*                                                                     *
*****                                                             *****
         SPACE
*****                                                             *****
*                                                                     *
* NCZ93205.8 PROGRAM LOGIC OVERVIEW                                   *
*                                                                     *
* OMITTED. IT SHOULD BE NOTED THAT THIS PROGRAM HAS TO BE PRETTY      *
* RUGGED. DO NOT ASSUME THAT ALL CALLS ARE IN LOGICAL SEQUENCE.       *
*                                                                     *
*****                                                             *****
         TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM PROLOGUE'
*****                                                             *****
*                                                                     *
* ID:          GETPDS   - SUBROUTINE TO READ PDS MEMBERS              *
*                         FORMERLY KNOWN AS NCZ93205                  *
*                                                                     *
* ENTRY INTFCE: STANDARD OS - SEE ABOVE.                              *
*                                                                     *
* EXIT  INTFCE: STANDARD OS - SEE ABOVE.                              *
*                                                                     *
* RETURN CODES:RC = 0       - OK                                      *
*                                                                     *
*              RC = 4       - REQUESTED FUNCTION DID NOT COMPLETE     *
*                                                                     *
*              RC = 8       - SERIOUS ERROR - TERMINATION ADVISED     *
*                                                                     *
* REG USAGE:   R0 -                                                   *
*              R1 -                                                   *
*              R2 -      -> P1  (FUNCTION)                            *
*              R3 -      -> P2  (MEMBER)                              *
*              R4 -      -> P3  (INPUT AREA)                          *
*              R5 -      WORK: -> BUFFER                              *
*              R6 -      WORK: -> RECORD                              *
*              R7 -      -> P4 (RETURN CODE PL/1 ONLY)                *
*              R8 -                                                   *
*              R9                                                     *
*              R10 -                                                  *
*              R11 -     BASE REGISTER                                *
*              R12 -     -> DCB                                       *
*              R13 -     SAVE AREA                                    *
*              R14 -                                                  *
*              R15 -                                                  *
*                                                                     *
*****                                                             *****
         TITLE 'SUBROUTINE TO READ PARTITIONED DATASET MEMBERS         *
               - PROGRAM CODE'
GETPDS   ESTART TYPE=START,DESC='GETPDS - ACCESS PDS MEMBERS',         X
               VER='1.0.0',BASE=R11,REGS=YES,PLIF=GETPDSP
         ENTRY NCZ93205
NCZ93205 EQU   GETPDS
         BNE   NOTPLI
         LM    R2,R5,0(R1)         GET PLI PARMLIST OF DDB
         L     R2,0(0,R2)          GET DATA ADDRS
         L     R3,0(0,R3)
         L     R4,0(0,R4)
         L     R7,0(0,R5)              PL1 ONLY
         B     COMM
NOTPLI   EQU   *
         LM    R2,R4,0(R1)
         LA    R7,PLIRC                PL1 ONLY
COMM     EQU   *
         LA    R12,PDS
         USING IHADCB,R12
******** LM    R2,R4,0(R1)
         SPACE
         L     R15,0(R2)               GET FUNCTION CODE
         CH    R15,=H'0'               Q - OPEN ?
         BE    P01                     Y
         CH    R15,=H'4'               Q - FIND ?
         BE    P02                     Y
         CH    R15,=H'8'               Q - READ ?
         BE    P03                     Y
         CH    R15,=H'12'              Q - CLOSE ?
         BE    P04                     Y
         CH    R15,=H'1'               Q - START SEQ DIR ?
         BE    P05                     Y
         CH    R15,=H'5'               Q - NEXT SEQ DIR ?
         BE    P06                     Y
         B     P99                     BAD FUNCTION CODE
*********************************************************************
*        OPEN DATASET
P01      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BO    P99                      Y BOMB OUT
*********************************** START FIX V1.2.0 ****************
         L     R8,=A(@GETDR1)
         MVI   0(R8),X'00'
         MVC   1(256,R8),0(R8)          CLEAR OUT WORKING STORAGE
**************************************END FIX V1.2.0 ****************
         CLC   0(8,R3),=CL8' '          DDNAME SPECIFIED?
         BE    P01GO                    NO, DEFAULT=PDS
         MVC   PDSDDNAM(8),0(R3)        MOVE DDNAME TO DCB
P01GO    EQU   *
         OPEN  (PDS,INPUT)              OPEN IT
         TM    DCBOFLGS,DCBOFOPN        Q - OK ?
         BZ    P98                      N EXIT RC=4
         CLI   DCBDSORG,DCBDSGPO        Q - IS THIS A PDS ?
         BNE   P0105                    N GO CLOSE
         TM    DCBRECFM,X'FF'-(DCBRECF+DCBRECBR+DCBRECCA) Q - RECFM ?
         BZ    P0110                    OK
         SPACE
P0105    EQU   *
         CLOSE PDS
         B     P99
P0110    LH    R0,DCBBLKSI
         GETMAIN R,LV=(0)               GET A BUFFER
         ST    R1,BUFA
         B     P97                      EXIT OK
**********************************************************************
         SPACE
*        FIND MEMBER
P02      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         FIND  (12),(3),D               LOCATE MEMBER
         LTR   R15,R15                  Q - OK ?
         BNZ   P98                      N EXIT RC=4
         NI    FLAGS,X'FF'-EOM          Y CLEAR END OF MEMBER FLAG
         OI    FLAGS,DOREAD             SET FLAG TO DO READ
         B     P97                      EXIT RC=0
         EJECT
**********************************************************************
*        GET NEXT RECORD FOR THIS MEMBER
P03      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N - BOMB OUT
         SPACE
         TM    FLAGS,EOM                Q - EOM ALREADY ?
         BO    P99                      Y BOMB OUT
         SPACE
P0305    L     R5,BUFA                  R5 -> INPUT BUFFER
         L     R6,RECA                  R6 -> NEXT RECORD IN BUFFER
         TM    FLAGS,DOREAD             Q - SHOULD WE READ NEXT BLOCK ?
         BZ    P0310                    N
         SPACE
         READ  DECB1,                   READ A BLOCK OF RECORDS        *
               SF,                                                     *
               (12),                                                   *
               (5),                                                    *
               'S'
         CHECK DECB1                    WAIT FOR IO TO COMPLETE
         SPACE
         LH    R10,DCBBLKSI             # OF BYTES WE COULD HAVE READ
         L     R1,DCBIOBA
         SH    R10,22(R1)               LESS # OF BYTES WE DID'NT READ
         BZ    P03EOM                   NOTHING READ - MUST BE EOM
         AR    R10,R5
         ST    R10,BLOCKEND             SAVE ADDRESS END OF THIS BLOCK
         NI    FLAGS,X'FF'-DOREAD       SET FLAG OFF - WE DID A READ
         LR    R6,R5                    R6 -> FIRST RECORD IN BLOCK
         SPACE
P0310    LH    R15,DCBLRECL             GET RECORD LENGTH
*******  EX    R15,EXMVC                MOVE RECORD TO INPUT AREA
*******  AR    R6,R15                   R6 -> NEXT RECORD IN BUFFER
********************************* START FIX V1.1.0     ***************
         BCTR  R15,0
         EX    R15,EXMVC                MOVE RECORD TO INPUT AREA
         LA    R6,1(R15,R6)             R6 -> NEXT RECORD IN BUFFER
*********************************  END  FIX V1.1.0     ***************
         C     R6,BLOCKEND              Q - END OF THIS BLOCK ?
         BNL   P0320                    Y GO SET FLAG TO DO READ
         SPACE
         ST    R6,RECA                  SAVE CURRENT RECORD POINTER
         B     P97                      EXIT RC=0
         SPACE
P0320    OI    FLAGS,DOREAD             DO READ FOR NEXT RECORD
         B     P97                      EXIT RC=0
         SPACE
P03EOM   DS    0H
         OI    FLAGS,EOM                INDICATE END OF MEMBER
         B     P98                      EXIT RC = 4
         EJECT
**********************************************************************
*        CLOSE PDS
P04      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         LH    R0,DCBBLKSI              FREE MEMBER BLOCK
         L     R5,BUFA
         FREEMAIN R,LV=(0),A=(5)
         CLOSE PDS
         B     P97
**********************************************************************
*        START SEQUENTIAL DIR PROCESS
P05      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
         MVC   PDSRELAD(4),=4X'00'      RESET DIRECTORY TO START
         B     P97
         SPACE
**********************************************************************
*        GET NEXT MEMBER NAME
P06      DS    0H
         TM    DCBOFLGS,DCBOFOPN        Q - DATASET OPEN ?
         BZ    P99                      N BOMB OUT
GETD     GETDIR PDS,EODAD=P06EOF
         MVC   0(8,R3),0(R1)            SAVE MEMBER NAME AND TTR
         B     P97
P06EOF   EQU   *
         B     P98
*
         SPACE
P97      MVC   0(4,R7),=F'0'
         ERETURN RC=0
P98      MVC   0(4,R7),=F'4'
         ERETURN RC=4
P99      MVC   0(4,R7),=F'8'
         ERETURN RC=8
         SPACE
PDS      DCB   DDNAME=PDS,DSORG=PO,MACRF=(R),EODAD=P03EOM
PDSRELAD EQU   PDS                      CURRENT TTRN
PDSDDNAM EQU   PDS+40                                            *JLM*
FLAGS    DC    X'00'
EOM      EQU   X'80'                    REACHED END OF CURRENT MEMBER
DOREAD   EQU   X'40'                    INDICATES CURRENT BLOCK IS     *
                                        EXHAUSTED
EXMVC    MVC   0(0,R4),0(R6)            MOVE RECORD TO INPUT AREA
PLIRC    DS    F                        USED FOR PLI INTERFACE ONLY
BUFA     DS    F                        -> BUFFER
RECA     DS    F                        -> CURRENT RECORD IN BUFFER
BLOCKEND DS    F                        -> END OF CURRENT BLOCK
         DCBD  DSORG=BS,DEVD=DA
         END
./  ENDUP
