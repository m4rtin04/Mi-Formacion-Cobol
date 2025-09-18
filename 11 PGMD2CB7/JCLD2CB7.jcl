//KC03CB7D JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID 
//JOBLIB  DD  DSN=DSND10.SDSNLOAD,DISP=SHR                      
//STEP1    EXEC PGM=IKJEFT01,DYNAMNBR=20,COND=(4,LT)     
//SYSTSPRT DD SYSOUT=*                                   
//SYSOUT   DD SYSOUT=*                                   
//SYSTSIN  DD *                                          
  DSN SYSTEM(DBDG)                                       
  RUN  PROGRAM(PGMD2CB7) PLAN(CURSOCB7) +                
       LIB('KC03CB7.CURSOS.PGMLIB')                      
  END                                                    
//SYSPRINT DD SYSOUT=*                                   
//SYSUDUMP DD SYSOUT=*                                   
//SYSIN    DD *                                          
//*                                                      
//                                                       
