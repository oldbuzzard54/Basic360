//BASIC03  JOB MSGCLASS=A,MSGLEVEL=(1,1),CLASS=A   ,TYPRUN=SCAN
//*                                     CLASS A USES MY PROCLIBS
//*                                     CLASS S USES DEFAULT PROCLIBS
//*
//* GENERATE THE EXECUTABLES FOR THE ASSEMBLER MODULES FOR BASIC360
//*
//S1  EXEC ASMFCL,MAC1='HERCEL.BASIC.ASM',PARM.ASM='OBJ,NODECK'
//ASM.SYSPRINT DD SYSOUT=C
//ASM.SYSPUNCH DD DUMMY
//ASM.SYSIN DD DSN=HERCEL.BASIC.ASM(BASALO),DISP=OLD
//LKED.SYSLMOD DD DSN=HERCEL.BASIC.LOADLIB(BASALO),DISP=OLD
//LKED.SYSPRINT DD SYSOUT=C
//*
//S2  EXEC ASMFCL,MAC1='HERCEL.BASIC.ASM',PARM.ASM='OBJ,NODECK'
//ASM.SYSPRINT DD SYSOUT=C
//ASM.SYSPUNCH DD DUMMY
//ASM.SYSIN DD DSN=HERCEL.BASIC.ASM(GETPDS),DISP=OLD
//LKED.SYSLMOD DD DSN=HERCEL.BASIC.LOADLIB(GETPDS),DISP=OLD
//LKED.SYSPRINT DD SYSOUT=C
//*
