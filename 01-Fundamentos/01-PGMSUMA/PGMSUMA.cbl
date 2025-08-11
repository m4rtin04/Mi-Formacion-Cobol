  IDENTIFICATION DIVISION.         
  PROGRAM-ID. PGMSUMA.             
 **********************************
                                   
  ENVIRONMENT DIVISION.            
  CONFIGURATION SECTION.           
  SPECIAL-NAMES.                   
      DECIMAL-POINT IS COMMA.      
                                   
 **********************************
  DATA DIVISION.                   
  WORKING-STORAGE SECTION.         
                                   
 **********************************
 *           CONTADORES           *
 **********************************
  01 WS-CONTADOR PIC 9(02) VALUE 1.
 **********************************
 *           ACUMULADORES         *
 **********************************
  01 WS-ACUMULADOR PIC 9(02).      
                                   
  PROCEDURE DIVISION.              

  MAIN-PROGRAM.                                   
                                                  
      PERFORM 1000-I-INICIO THRU 1000-F-INICIO    
                                                  
      PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO  
                             UNTIL WS-CONTADOR > 10
                                                  
      PERFORM 9999-I-FINAL THRU 9999-F-FINAL      
                                                  
      GOBACK                                      
      .                                           
  F-MAIN-PROGRAM.                                 
                                                  
  1000-I-INICIO.                                  
                                                  
      INITIALIZE WS-ACUMULADOR                    
      .                                           
  1000-F-INICIO. EXIT.                            
                                                  
  2000-I-PROCESO.                                 
                                                  
      ADD WS-CONTADOR TO WS-ACUMULADOR            
      ADD 1 TO WS-CONTADOR                        
      .                                           
  2000-F-PROCESO. EXIT.  

  9999-I-FINAL.                                   
                                                 
     DISPLAY 'LA SUMA DE LOS 10 PRIMEROS NUMEROS'
     DISPLAY 'RESULTADO: ' WS-ACUMULADOR         
     .                                           
  9999-F-FINAL. EXIT.
