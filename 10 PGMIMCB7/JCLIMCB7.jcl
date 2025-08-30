//KC03CB7M JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,   
//             TIME=(,5) ,RESTART=STEP5                            
//*                                                                
//*************************************                            
//*    IDCAMS | DELETE DE ARCHIVOS    *                            
//*************************************                            
//*                                                                
//STEP1    EXEC PGM=IDCAMS,COND=(8,LT)                             
//SYSPRINT DD SYSOUT=*                                             
//SYSIN    DD *                                                    
     DELETE   KC03CB7.CLIENTES.SORT                                
     DELETE   KC03CB7.PGMIMCB7.SALIDA                              
     SET MAXCC=0                                                   
//*                                                                
//*************************************                            
//*    SORT DE ARCHIVO ( CLIENTE )    *                            
//*************************************                            
//*                                                                
//STEP2    EXEC PGM=SORT,COND=EVEN                                 
//SYSOUT   DD SYSOUT=*                                             
//SORTIN   DD DSN=KC02788.ALU9999.CURSOS.CLIENTE,DISP=SHR          
//SORTOUT  DD DSN=KC03CB7.CLIENTES.SORT,                           
//             DISP=(,CATLG,DELETE),                               
//             SPACE=(TRK,(10,5),RLSE),        
//             RECFM=FB,LRECL=50                         
//SYSIN    DD *                                          
 SORT FIELDS=(16,2,CH,A,3,11,CH,A)                       
//*                                                      
//STEP3    EXEC PGM=PGMIMCB7                             
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR         
//DDCLIEN  DD DSN=KC03CB7.CLIENTES.SORT,DISP=SHR         
//DDLIST   DD DSN=KC03CB7.PGMIMCB7.SALIDA,UNIT=SYSDA,    
//            DCB=(LRECL=133,BLKSIZE=1330,RECFM=FBA),    
//         SPACE=(TRK,(1,1),RLSE),DISP=(NEW,CATLG,CATLG) 
//SYSOUT   DD SYSOUT=*                                   
//SYSUDUMP DD SYSOUT=*                                   
//SYSIN    DD *                                          
//*                                                      
//                                                       
       
