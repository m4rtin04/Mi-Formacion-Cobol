 IDENTIFICATION DIVISION.                                     
 PROGRAM-ID. PGMB2CB7.                                        
                                                              
 ENVIRONMENT DIVISION.                                        
 CONFIGURATION SECTION.                                       
 SPECIAL-NAMES.                                               
     DECIMAL-POINT IS COMMA.                                  
                                                              
 INPUT-OUTPUT SECTION.                                        
                                                              
 DATA DIVISION.                                               
                                                              
                                                              
 WORKING-STORAGE SECTION.                                     
                                                              
**********************************************                
*     FILE STATUS                            *                
**********************************************                
 77 FS-SQLCODE     PIC +++999 USAGE DISPLAY VALUE ZEROS.      
                                                              
**********************************************                
*   CONTROL DE CICLO                         *                
**********************************************                
 77 WS-STATUS-REG  PIC X(01). 
       88 WS-NO-FIN-PROCESO    VALUE 'Y'.                      
       88 WS-FIN-PROCESO       VALUE 'N'.                      
                                                               
  **********************************************               
  * VARIABLES PARA EL CORTE                    *               
  **********************************************               
   01 WS-SUCUEN-ANT  PIC 9(02) VALUE ZEROS.                    
   01 WS-CLAVE-ACT   PIC 9(02) VALUE ZEROS.                    
                                                               
  **********************************************               
  *                CONTADORES                  *               
  **********************************************               
   01 CNT-CUENTAS          PIC 9(02) VALUE ZEROS.              
   01 CNT-TOTAL-CUENTAS    PIC 9(02) VALUE ZEROS.              
                                                                  
                                                               
  **********************************************               
  *        AREA DE SQL                         *               
  **********************************************               
                                                          
      EXEC SQL                                            
        INCLUDE SQLCA                                     
      END-EXEC.                                           
                                                          
      EXEC SQL                                            
        INCLUDE TBCURCTA                                  
      END-EXEC.                                           
                                                          
      EXEC SQL                                            
        INCLUDE TBCURCLI                                  
      END-EXEC.                                           
                                                          
      EXEC SQL                                            
        DECLARE CLIENTE_CURSOR CURSOR FOR                 
         SELECT A.TIPCUEN,                                
              A.NROCUEN,                                  
              A.SUCUEN,                                   
              A.NROCLI,                                   
              B.NOMAPE,                                   
              A.SALDO,                                    
              A.FECSAL                                    
          FROM  KC02803.TBCURCTA A                        
                INNER JOIN                                
                KC02803.TBCURCLI B                        
            ON  A.NROCLI = B.NROCLI                             
            WHERE A.SALDO > 0                                   
            ORDER BY A.SUCUEN ASC                               
      END-EXEC.                                                 
 **********************************************************     
  PROCEDURE DIVISION.                                           
                                                                
  MAIN-PROGRAM.                                                 
                                                                
      PERFORM 1000-I-INICIO THRU  1000-F-INICIO                 
                                                                
      PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO                
                          UNTIL WS-FIN-PROCESO                  
                                                                
      PERFORM 9999-I-FINAL THRU 9999-F-FINAL                    
      .                                                         
  F-MAIN-PROGRAM. GOBACK.                                       
                                                                
  1000-I-INICIO.                                                
                                                                
      SET WS-NO-FIN-PROCESO TO TRUE                             
                                                                
      PERFORM 1200-I-ABRIR-CURSORES THRU 1200-F-ABRIR-CURSORES  
                                                                
      PERFORM 1400-I-LEER-CLIENTE-CURSOR THRU                   
                                     1400-F-LEER-CLIENTE-CURSOR   
                                                                  
      MOVE WS-CLAVE-ACT   TO WS-SUCUEN-ANT                        
      .                                                           
  1000-F-INICIO. EXIT.                                            
                                                                  
  2000-I-PROCESO.                                                 
      EVALUATE TRUE                                                                                                            
        WHEN WS-CLAVE-ACT = WS-SUCUEN-ANT                       
          PERFORM 2200-I-ACUMULAR-CTA THRU 2200-F-ACUMULAR-CTA
                                                                   
        WHEN WS-CLAVE-ACT  IS NOT EQUAL WS-SUCUEN-ANT           
              PERFORM 2400-I-CORTE THRU 2400-F-CORTE              
                                                                  
              PERFORM 2200-I-ACUMULAR-CTA THRU 2200-F-ACUMULAR-CTA
                                                                  
      END-EVALUATE                                                
                                                                  
      PERFORM 1400-I-LEER-CLIENTE-CURSOR THRU                     
                                     1400-F-LEER-CLIENTE-CURSOR   
      .                                                           
  2000-F-PROCESO. EXIT.                                           
                                                                  
  9999-I-FINAL.                                                   
                                                                  
      PERFORM 9800-I-CERRAR-CURSORES THRU 9800-F-CERRAR-CURSORES  
                                                                  
      DISPLAY '******************************************'        
      DISPLAY 'TOTAL GENERAL DE CUENTAS: ' CNT-TOTAL-CUENTAS      
      .                                                           
  9999-F-FINAL. EXIT.                                             
                                                                  
  1200-I-ABRIR-CURSORES.                                          
                                                                  
      EXEC SQL                                                    
          OPEN CLIENTE_CURSOR                                    
      END-EXEC                                                   
                                                                 
       IF SQLCODE IS NOT EQUAL ZEROS                             
           MOVE SQLCODE TO FS-SQLCODE                            
           DISPLAY 'ERROR EN ABRIR CLIENTE CURSOR: ' FS-SQLCODE  
           MOVE 9999 TO RETURN-CODE                              
           SET WS-FIN-PROCESO TO TRUE                            
       END-IF                                                    
       .                                                         
  1200-F-ABRIR-CURSORES. EXIT.                                   
                                                                 
  1400-I-LEER-CLIENTE-CURSOR.                                    
                                                                 
      EXEC SQL                                                   
         FETCH  CLIENTE_CURSOR                                   
                INTO                                             
                   :DCLTBCURCTA.WS-TIPCUEN,                      
                   :DCLTBCURCTA.WS-NROCUEN,                      
                   :DCLTBCURCTA.WS-SUCUEN,                       
                   :DCLTBCURCTA.WS-NROCLI,                       
                   :DCLTBCURCLI.WC-NOMAPE,                       
                   :DCLTBCURCTA.WS-SALDO,                        
                   :DCLTBCURCTA.WS-FECSAL                        
      END-EXEC                                                   
                                                                 
     EVALUATE TRUE                                               
         WHEN SQLCODE EQUAL ZEROS                                
             MOVE WS-SUCUEN TO WS-CLAVE-ACT                      
                                                                 
         WHEN SQLCODE EQUAL +100                                 
             SET WS-FIN-PROCESO TO TRUE                          
             PERFORM 2400-I-CORTE THRU 2400-F-CORTE              
                                                                 
         WHEN OTHER                                              
             MOVE SQLCODE TO FS-SQLCODE                          
             DISPLAY 'ERROR EN FETCH CLIENTE CURSOR: ' FS-SQLCODE
             MOVE 9999 TO RETURN-CODE                            
             SET WS-FIN-PROCESO TO TRUE                          
     END-EVALUATE                                                
     .                                                           
 1400-F-LEER-CLIENTE-CURSOR. EXIT.                               
                                                                 
 2200-I-ACUMULAR-CTA.                                            
                                                                 
     ADD 1 TO CNT-CUENTAS                                        
                                                                 
     COMPUTE CNT-TOTAL-CUENTAS = CNT-TOTAL-CUENTAS + 1           
     .                                                           
 2200-F-ACUMULAR-CTA. EXIT.                                      
                                                                
  2400-I-CORTE.                                                 
                                                                
      DISPLAY 'SUCURSAL:' WS-SUCUEN-ANT                         
      DISPLAY '    CANTIDAD DE CUENTAS: ' CNT-CUENTAS           
                                                                
      MOVE 0 TO CNT-CUENTAS                                     
      MOVE WS-CLAVE-ACT TO WS-SUCUEN-ANT                        
      .                                                         
  2400-F-CORTE. EXIT.                                           
                                                                
  9800-I-CERRAR-CURSORES.                                       
                                                                
      EXEC SQL                                                  
          CLOSE CLIENTE_CURSOR                                  
      END-EXEC                                                  
                                                                
      IF SQLCODE IS NOT EQUAL ZEROS                             
          MOVE SQLCODE TO FS-SQLCODE                            
          DISPLAY 'ERROR EN CERRAR CLIENTE CURSOR: ' FS-SQLCODE 
          MOVE 9999 TO RETURN-CODE                              
          SET WS-FIN-PROCESO TO TRUE                            
      END-IF                                                    
      .                                                         
  9800-F-CERRAR-CURSORES. EXIT.                                 
