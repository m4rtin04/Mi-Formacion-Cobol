//KC03CB7V JOB CLASS=A,MSGCLASS=O,MSGLEVEL=(1,1),NOTIFY=&SYSUID,  
//             TIME=(,3)   ,RESTART=STEP4                         
//*                                                               
//*****************************************                       
//* BORRADO DE ARCHIVOS DE SALIDA PREVIOS *                       
//*****************************************                       
//*                                                               
//STEP1    EXEC PGM=IDCAMS,COND=(8,LT)                            
//SYSPRINT DD SYSOUT=*                                            
//SYSIN    DD *                                                   
   DELETE KC03CB7.CURSOS.SORT.NOVEDAD                             
   DELETE KC03CB7.CURSOS.SALIDA.PGMS18N1                          
   SET MAXCC=00                                                   
//*                                                               
//*************************************                           
//*    SORT DE ARCHIVO ( NOVEDAD )    *                           
//*************************************                           
//STEP2    EXEC PGM=SORT,COND=EVEN                                
//SYSOUT   DD SYSOUT=*                                            
//SORTIN   DD DSN=KC02788.ALU9999.NOVCLIEN,DISP=SHR               
//SORTOUT  DD DSN=KC03CB7.CURSOS.SORT.NOVEDAD,                    
//             DISP=(,CATLG,DELETE),                              
//             SPACE=(TRK,(10,5),RLSE),                           
//             RECFM=FB,LRECL=50                                  
//SYSIN    DD *                                                
   SORT FIELDS=(1,2,CH,A,3,11,CH,A)                            
//*                                                            
//*************************************                        
//*   MANEJO DE ARCHIVOS | PGMS18N1   *                        
//*************************************                        
//*                                                            
//STEP3    EXEC PGM=PGMS18N1                                   
//STEPLIB  DD DSN=KC03CB7.CURSOS.PGMLIB,DISP=SHR               
//DDCLIEN  DD DSN=KC02788.ALU9999.CLIENT1.KSDS.VSAM,DISP=SHR   
//DDNOVE   DD DSN=KC03CB7.CURSOS.SORT.NOVEDAD,DISP=SHR         
//DDSALIDA DD DSN=KC03CB7.CURSOS.SALIDA.PGMS18N1,              
//             DISP=(,CATLG,DELETE),                           
//             SPACE=(TRK,(10,5),RLSE),                        
//             RECFM=FB,LRECL=50                               
//SYSOUT   DD SYSOUT=*                                         
//SYSUDUMP DD SYSOUT=*                                         
//SYSIN    DD SYSOUT=*                                         
//*                                                            
//                                                             
