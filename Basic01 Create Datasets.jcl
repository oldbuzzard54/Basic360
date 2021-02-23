//BASIC01  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A   ,TYPRUN=SCAN
//*                                     CLASS A USES MY PROCLIBS
//*                                     CLASS S USES DEFAULT PROCLIBS
//*
//*   DELETE AND CREATE THE DATASETS TO CONTAIN BASIC360
//*
//*   THIS JOB DELETES AND RECREATES ALL THE DATASET FOR BASIC360.
//*   ALL DATASET ARE SET UP FOR USING 3350 VOLUME EVOL01 WITH THE
//*   USERID HERCEL.  YOU MAY WISH TO CHANGE ANY OF THESE HERE AND
//*   IN ALL OF THE SUBSEQUENT INSTALL STEPS.
//*
//S1  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN DD *
 DELETE 'HERCEL.BASIC.ASM' NONVSAM PURGE
 DELETE 'HERCEL.BASIC.PLI' NONVSAM PURGE
 DELETE 'HERCEL.BASICINC.PLI' NONVSAM PURGE
 DELETE 'HERCEL.BASICMON.PLI' NONVSAM PURGE
 DELETE 'HERCEL.BASIC1UP.PLI' NONVSAM PURGE
 DELETE 'HERCEL.BASIC.LOADLIB' NONVSAM PURGE
 DELETE 'HERCEL.BASICLIB.NEW' NONVSAM PURGE
 SET LASTCC = 0
 SET MAXCC = 0
//S2  EXEC PGM=IEFBR14
//DD1  DD  DSN=HERCEL.BASIC.ASM,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(5,5,10)),
//         DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB,DSORG=PO)
//DD2  DD  DSN=HERCEL.BASIC.PLI,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(35,5,10)),
//         DCB=(LRECL=80,BLKSIZE=400,RECFM=FB,DSORG=PO)
//DD3  DD  DSN=HERCEL.BASIC.LOADLIB,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(35,10,10)),
//         DCB=(BLKSIZE=19069,RECFM=U,DSORG=PO)
//DD4  DD  DSN=HERCEL.BASICMON.PLI,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(2,2,10)),
//         DCB=(LRECL=80,BLKSIZE=400,RECFM=FB,DSORG=PO)
//DD5  DD  DSN=HERCEL.BASIC1UP.PLI,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(3,3,10)),
//         DCB=(LRECL=80,BLKSIZE=400,RECFM=FB,DSORG=PO)
//DD6  DD  DSN=HERCEL.BASICINC.PLI,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(3,3,10)),
//         DCB=(LRECL=80,BLKSIZE=400,RECFM=FB,DSORG=PO)
//DD7  DD  DSN=HERCEL.BASICLIB.NEW,DISP=(NEW,CATLG,CATLG),
//         UNIT=3350,VOL=SER=EVOL01,SPACE=(TRK,(8,8,10)),
//         DCB=(LRECL=80,BLKSIZE=3120,RECFM=FB,DSORG=PO)