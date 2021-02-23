# Basic360 Full install from source
This is project is the complete source code for generating the
Basic360 Compiler/Interpreter V3.2.

To install, submit each file per instruction.
Unless notes, all steps in each job must complete with RC= 0000

EXAMPLE:
14.31.20 JOB 4863  $HASP373 BASIC10  STARTED - INIT  1 - CLASS A - SYS BSP1
14.31.20 JOB 4863  IEF403I BASIC10 - STARTED - TIME=14.31.20
14.31.21 JOB 4863  IEFACTRT - Stepname  Procstep  Program   Retcode
14.31.21 JOB 4863  BASIC10    S1                  xxxxxxxx  RC= 0000        <=====
14.31.21 JOB 4863  IEF404I BASIC10 - ENDED - TIME=14.31.21
14.31.21 JOB 4863  $HASP395 BASIC10  ENDED

STEP 1a
======
Submit Basic01 Create Datasets.jcl

STEP 1b
======
There is an alternate install using a tape (.aws file).  All the datasets are
already built.  This option can be used if your HERCULES system has
problems submitting large "card decks".  STEPS 2 thru 8 can be skipped if you choose this option.
- using HERCULES, assign bas32g.aws to unit 480.  This can be done via 
  the .cnf file or devinit.
- submit Restore Tape bas32g.jcl
- skip to step 9.

STEPS 2 to 8
===========
- Submit Basic02 Create Maclib.jcl
- Submit Basic03 Generate Assembler executables.jcl
- Submit Basic04 Create Pli core file.jcl.
- Submit Basic05 Create Pli includes file.jcl
- Submit Basic06 Create PLI 1up file.jcl
- Submit Basic07 Create PLI mon file.jcl
- Submit Basic08 Compile.jcl
This step compiles and link edits both the 1up and mon versions
of BASIC360.  The compile steps will end with cc=0004 as follows:
13.46.14 JOB 4901  IEF677I WARNING MESSAGE(S) FOR JOB BASIC08  ISSUED
13.46.14 JOB 4901  $HASP373 BASIC08  STARTED - INIT  4 - CLASS S - SYS BSP1
13.46.14 JOB 4901  IEF403I BASIC08 - STARTED - TIME=13.46.14
13.46.16 JOB 4901  IEFACTRT - Stepname  Procstep  Program   Retcode
13.46.16 JOB 4901  BASIC08    P1        PL1L      IEMAA     RC= 0004
13.46.17 JOB 4901  BASIC08    P1        LKED      IEWL      RC= 0000
13.46.19 JOB 4901  BASIC08    P1        PL1L      IEMAA     RC= 0004
13.46.20 JOB 4901  BASIC08    P1        LKED      IEWL      RC= 0000
13.46.20 JOB 4901  IEF404I BASIC08 - ENDED - TIME=13.46.20
13.46.20 JOB 4901  $HASP395 BASIC08  ENDED

Step 9
=======
- Submit Basic09 Create Basiclib.jcl
    +++++++++++++++++++++ IMPORTANT NOTE +++++++++++++++++++++++ 
    For first time install, you will need to copy or rename HERCEL.BASICLIB.NEW to 
    HERCEL.BASICLIB.BAS.  For subsequent installs or reinstalls, the BASICLIB.BAS may 
    contain BASIC programs you saved and don't want to loose.  Therefore, I set up the 
    JCL to NOT replace any existing BASICLIB.BAS.  It is up to you to decide if you 
    want to copy BASICLIB.NEW to BASICLIB.BAS or do nothing with BASICLIB.NEW. 
    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Step 10
=======
- if you want to run all the sample programs, submit
  Basic10 Execute Samples.jcl

=======================================================================
=======================================================================

Starting with V3.2, the program is broken into 5 parts -

basenv.pli - this module contains all the environmental dependent
             code (i.e. batch vs online).
basinp.pli - this module supports the BASIC INPUT statement.
basrdr.pli - this module loads the BASIC programs into the source
             code table.
basprt.pli - this module handles the "printed" output from the
             BASIC program.  Note optional debugging output does
             not go through this module.
Bascore.pli - the heart of the package.  It contains all the compiler
             and interpreter code.  This module includes the above
             so it can be shared across environments.
