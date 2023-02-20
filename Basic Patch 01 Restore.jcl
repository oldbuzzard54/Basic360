//BASICPAT JOB CLASS=A,MSGCLASS=A,MSGLEVEL=(1,1),NOTIFY=HERC01,         00000105
//     USER=HERC01,PASSWORD=CUL8TR  ,TYPRUN=SCAN                        00000200
//********************************************************************  00000300
//*                                                                     00000400
//*  THIS JOB CREATES THE BASIC PATCH FILE.                             00000500
//*                                                                     00000600
//********************************************************************  00000700
//*                                                                     00000800
//RECV370  PROC                                                         00000900
//RECV370  EXEC PGM=RECV370,REGION=1024K                                00001000
//RECVLOG   DD SYSOUT=*                     RECV370 OUTPUT MESSAGES     00001100
//SYSPRINT  DD SYSOUT=*                     IEBCOPY OUTPUT MESSAGES     00001200
//SYSIN     DD DUMMY                        IEBCOPY REQUIRES            00001300
//SYSUT1    DD UNIT=SYSDA,                  WORK DATASET                00001400
//             SPACE=(CYL,(10,10)),                                     00001500
//             DCB=BLKSIZE=5600                                         00001600
//XMITIN    DD DDNAME=XMITIN                INPUT DATASET               00001700
//SYSUT2    DD DDNAME=XMITOUT               OUTPUT DATASET              00001800
// PEND                                                                 00001900
//*                                                                     00002000
//*                                                                     00002100
//*  DELETE PATCH DATASET                                               00002205
//*                                                                     00002300
//STEP01  EXEC PGM=IDCAMS                                               00002400
//SYSPRINT DD SYSOUT=*                                                  00002500
//SYSIN    DD *                                                         00002600
 DELETE 'HERC01.BASIC360.PATCH'     NONVSAM PURGE                       00002705
 SET LASTCC=0                                                           00002800
 SET MAXCC=0                                                            00002900
//********************************************************************  00003000
//*                                                                     00003100
//*  ALLOC THE NEW PATCH DATASET                                        00003203
//*                                                                     00003300
//********************************************************************  00003400
//STEP02  EXEC PGM=IEFBR14                                              00003500
//DDLOAD   DD DSN=HERC01.BASIC360.PATCH,DISP=(NEW,CATLG,DELETE),        00003605
//         UNIT=DISK,VOL=SER=PUB002,                                    00003700
//         SPACE=(TRK,(24,24,24)),                                      00003800
//         DCB=(RECFM=FB,LRECL=80,BLKSIZE=3200)                         00003903
//********************************************************************  00004000
//*                                                                     00004100
//*   RECEIVE THE BASIC PATCH XMI FILE.                                 00004200
//*                                                                     00004300
//********************************************************************  00004400
//*                                                                     00004500
//STEP03 EXEC RECV370                                                   00004603
//XMITIN   DD DSN=HERC01.BASICPAT.XMI,DISP=OLD                          00004705
//XMITOUT  DD DSN=HERC01.BASIC360.PATCH,DISP=OLD                        00004805
//*                                                                     00004900
