IDENTIFICATION DIVISION.               
PROGRAM-ID. PGM3CCB7.                  
                                       
ENVIRONMENT DIVISION.                  
CONFIGURATION SECTION.                 
SPECIAL-NAMES.                         
    DECIMAL-POINT IS COMMA.            
                                       
INPUT-OUTPUT SECTION.                  
FILE-CONTROL.                          
    SELECT CLIENTES ASSIGN DDCLIE      
             FILE STATUS IS FS-CLIENTE.
                                       
DATA DIVISION.                         
FILE SECTION.                          
FD CLIENTES                            
     BLOCK CONTAINS 0 RECORDS          
     RECORDING MODE IS F.              
                                       
01 REG-CLIENTES PIC X(50).             
                                       
WORKING-STORAGE SECTION.               
                                       
01 FS-CLIENTE PIC X(02) VALUE SPACES.  
 
77 FS-FIN-ARCHIVO PIC X.                                   
    88 WS-FIN-CLIENTE      VALUE 'Y'.                       
    88 WS-NO-FIN-CLIENTE   VALUE 'N'.                       
                                                            
01 WS-CONT-TOTAL PIC S9(15)V99  COMP-3 VALUE ZEROS.        
                                                            
01 WS-CONT-TOTAL-EDIT PIC -$ZZZ.ZZZ.ZZZ.ZZ9,99 VALUE ZEROS.

COPY CPCLI.                                                
                                                            
PROCEDURE DIVISION.                                        
                                                            
MAIN-PROGRAM.                                              
                                                            
     PERFORM 1000-I-INICIO THRU 1000-F-INICIO               
                                                            
     PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO             
                       UNTIL WS-FIN-CLIENTE                 
                                                            
     PERFORM 9999-I-FINAL THRU 9999-F-FINAL                 
     .                                                      
 F-MAIN-PROGRAM. GOBACK.                                    
                                                            
 1000-I-INICIO.      
     
     SET WS-NO-FIN-CLIENTE TO TRUE                         
                                                           
     PERFORM 1200-I-ABRIR-ARCHIVO THRU 1200-F-ABRIR-ARCHIVO
                                                           
     PERFORM 1400-I-LEER-ARCHIVO THRU 1400-F-LEER-ARCHIVO  
                                                           
     .                                                     
 1000-F-INICIO. EXIT.                                      
                                                           
 2000-I-PROCESO.                                           
                                                           
     IF  CLI-TIP-DOC EQUAL 'DU'                            
         ADD CLI-SALDO TO WS-CONT-TOTAL                    
     END-IF                                                
                                                           
     PERFORM 1400-I-LEER-ARCHIVO THRU 1400-F-LEER-ARCHIVO  
     .                                                     
 2000-F-PROCESO. EXIT.                                     
                                                           
 9999-I-FINAL.                                             
                                                           
     CLOSE CLIENTES                                        
                                                           
     IF FS-CLIENTE IS NOT EQUAL '00'                       
                    DISPLAY 'ERROR EN CERRAR ARCHIVO CLIENTES: ' FS-CLIENTE
           MOVE 9999 TO RETURN-CODE                               
           SET WS-FIN-CLIENTE TO TRUE                             
       END-IF                                                     
                                                                  
       MOVE WS-CONT-TOTAL TO WS-CONT-TOTAL-EDIT                   
       DISPLAY 'TOTAL IMPORTE PARA TIPO DE DOCUMENTO DU'          
       DISPLAY 'IMPORTE: '  WS-CONT-TOTAL-EDIT                    
       .                                                          
   9999-F-FINAL. EXIT.                                            
                                                                  
   1200-I-ABRIR-ARCHIVO.                                          
                                                                  
       OPEN INPUT CLIENTES                                        
                                                                  
       IF FS-CLIENTE NOT EQUAL '00'                               
           DISPLAY 'ERROR EN ABRIR ARCHIVO CLIENTES:' FS-CLIENTE  
           MOVE 9999 TO RETURN-CODE                               
           SET WS-FIN-CLIENTE TO TRUE                             
       END-IF                                                     
       .                                                          
   1200-F-ABRIR-ARCHIVO. EXIT.                                    
                                                                  
   1400-I-LEER-ARCHIVO.                                           
         READ CLIENTES INTO REG-CLIENTE                              
                                                                  
      EVALUATE FS-CLIENTE                                         
                                                                  
          WHEN '00'                                               
              CONTINUE                                            
                                                                  
          WHEN '10'                                               
              SET WS-FIN-CLIENTE TO TRUE                          
                                                                  
          WHEN OTHER                                              
              DISPLAY 'ERROR EN LEER ARCHIVO CLIENTE: ' FS-CLIENTE
              MOVE 9999 TO RETURN-CODE                            
              SET WS-FIN-CLIENTE TO TRUE                          
                                                                  
      END-EVALUATE                                                
      .                                                           
  1400-F-LEER-ARCHIVO. EXIT.                                      
