//KC03CB7H JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,
//             TIME=(,5)    ,RESTART=STEP2                      
//JCLLIB       JCLLIB ORDER=KC02788.ALU9999.PROCLIB             
//************************************                          
//*        EJECUCION PROGRA PGMSUMA  *                          
//*                                  *                          
//************************************                          
//STEP2    EXEC PGM=PGMSUMA                                     
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR                
//*                                                             
//SYSOUT   DD SYSOUT=*                                          
//SYSUDUMP DD SYSOUT=*                                          
//*                                                             
//        
