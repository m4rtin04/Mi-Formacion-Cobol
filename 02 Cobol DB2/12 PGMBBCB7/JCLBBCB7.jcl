 //KC03CB7D JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID  
 //JOBLIB  DD  DSN=DSND10.SDSNLOAD,DISP=SHR                       
 //***************************************************************
 //* EJECUTAR STEP1 SOLO LA PRIMERA VEZ PARA DEFINIR SALIDA      *
 //***************************************************************
 //*STEP1    EXEC PGM=IEFBR14                                     
 //*DDLIST   DD DSN=KC03CB7.LISTADO,UNIT=SYSDA,                   
 //*              DCB=(LRECL=94,BLKSIZE=0,RECFM=FBA),             
 //*              SPACE=(TRK,(1,1),RLSE),DISP=(,CATLG)            
 //***************************************************************
 //*      EJECUTAR PROGRAMA COBOL CON SQL EMBEBIDO               *
 //***************************************************************
 //STEP2    EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)             
 //SYSTSPRT DD SYSOUT=*                                           
 //DDLIST   DD DSN=KC03CB7.LISTADO,DISP=SHR                       
 //SYSOUT   DD SYSOUT=*                                           
 //SYSTSIN  DD *                                                  
   DSN SYSTEM(DBDG)                                               
   RUN  PROGRAM(PGMBBCB7) PLAN(CURSOCB7) +                        
        LIB('KC03CB7.CURSOS.PGMLIB')                              
   END                                                            
 //SYSPRINT DD SYSOUT=*                                           
 //SYSUDUMP DD SYSOUT=*                                           
 //SYSIN    DD *                                                  
 //*    
 //     
