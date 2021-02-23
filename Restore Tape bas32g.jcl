//HERCEL0 JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A,NOTIFY=HERCEL
//********************************************************************
//*
//*   COPY THE BASIC360 V3.2 DISTRIBUTION FROM TAPE TO DISK
//*
//*   ALL DATASET RESTORED  BY THIS JOB WILL BE CREATED BY BASIC01.
//*
//*   THE DISTRIBUTION TAPE CONTAINS THE FOLLOWING DATASETS:
//*   LABEL   DSN        DESCRIPTION
//*       1   BASIC1     IEBCOPY UNLOADED HERCEL.BASIC.ASM
//*       2   BASIC2     IEBCOPY UNLOADED HERCEL.BASIC.PLI
//*       3   BASIC3     IEBCOPY UNLOADED HERCEL.BASICINC.PLI
//*       4   BASIC4     IEBCOPY UNLOADED HERCEL.BASICMON.PLI
//*       5   BASIC5     IEBCOPY UNLOADED HERCEL.BASIC1UP.PLI
//*       6   BASIC6     IEBCOPY UNLOADED HERCEL.BASIC.LOADLIB
//*       7   BASIC7     IEBCOPY UNLOADED HERCEL.BASICLIB.NEW
//*
//********************************************************************
//*
//*  THIS WILL RESTORE THE TAPE TO PDS USING IEBCOPY
//*
//********************************************************************
//STEP01  EXEC PGM=IEBCOPY
//TAPE01 DD DSN=BASIC1,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(1,SL)
//DISK01 DD DSN=HERCEL.BASIC.ASM,DISP=SHR
//*
//TAPE02 DD DSN=BASIC2,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(2,SL)
//DISK02 DD DSN=HERCEL.BASIC.PLI,DISP=SHR
//*
//TAPE03 DD DSN=BASIC3,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(3,SL)
//DISK03 DD DSN=HERCEL.BASICINC.PLI,DISP=SHR
//*
//TAPE04 DD DSN=BASIC4,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(4,SL)
//DISK04 DD DSN=HERCEL.BASICMON.PLI,DISP=SHR
//*
//TAPE05 DD DSN=BASIC5,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(5,SL)
//DISK05 DD DSN=HERCEL.BASIC1UP.PLI,DISP=SHR
//*
//TAPE06 DD DSN=BASIC6,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(6,SL)
//DISK06 DD DSN=HERCEL.BASIC.LOADLIB,DISP=SHR
//*
//TAPE07 DD DSN=BASIC7,DISP=(OLD,KEEP),
//       UNIT=(480,,DEFER),VOL=(,RETAIN,SER=BAS32G),
//       LABEL=(7,SL)
//DISK07 DD DSN=HERCEL.BASICLIB.NEW,DISP=SHR
//*
//SYSPRINT DD SYSOUT=*
//SYSIN  DD *
 COPY INDD=TAPE01,OUTDD=DISK01
 COPY INDD=TAPE02,OUTDD=DISK02
 COPY INDD=TAPE03,OUTDD=DISK03
 COPY INDD=TAPE04,OUTDD=DISK04
 COPY INDD=TAPE05,OUTDD=DISK05
 COPY INDD=TAPE06,OUTDD=DISK06
 COPY INDD=TAPE07,OUTDD=DISK07
//*
