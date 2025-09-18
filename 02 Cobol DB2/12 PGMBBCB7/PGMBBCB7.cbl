  IDENTIFICATION DIVISION.                                   
 *                                                        *  
  PROGRAM-ID. PGMBBCB7.                                      
 **********************************************************  
 *                                                        *  
 *  PROGRAMA PARA SQL EMBEBIDO                            *  
 *                                                        *  
 **********************************************************  
 *      MANTENIMIENTO DE PROGRAMA                         *  
 **********************************************************  
 *  FECHA   *    DETALLE        * COD *                      
 **************************************                      
 *          *                   *     *                      
 *          *                   *     *                      
 **************************************                      
  ENVIRONMENT DIVISION.                                      
  CONFIGURATION SECTION.                                     
  SPECIAL-NAMES.                                             
      DECIMAL-POINT IS COMMA.                                
                                                             
  INPUT-OUTPUT SECTION.                                      
  FILE-CONTROL.                                              
      SELECT LISTADO  ASSIGN DDLIST                          
                FILE STATUS IS FS-LISTADO.       
       DATA DIVISION.                                                   
   FILE SECTION.                                                    
                                                                    
   FD LISTADO                                                       
       BLOCK CONTAINS 0 RECORDS                                     
       RECORDING MODE IS F.                                         
                                                                    
   01 REG-LISTADO  PIC X(93).                                       
  **************************************                            
   WORKING-STORAGE SECTION.                                         
  **************************************                            
   77  FILLER        PIC X(26) VALUE '* INICIO WORKING-STORAGE *'.  
  *******************************************************           
  *                      CONTROL DE CICLO                *          
  ********************************************************          
   01  WS-FLAG-FIN      PIC X.                                      
       88  WS-SI-PROCESO      VALUE ' '.                            
       88  WS-FIN-PROCESO     VALUE 'F'.                            
  ********************************************************          
  *                VARIABLES                             *          
  ********************************************************          
                                                                    
   77  WS-SQLCODE    PIC +++999 USAGE DISPLAY VALUE ZEROS.          
   77  FS-LISTADO    PIC X(02)  VALUE SPACES.    
                                                                   
   01  WS-FECHA.                                               
       03  WS-FECHA-AA      PIC 99            VALUE ZEROS.     
       03  WS-FECHA-MM      PIC 99            VALUE ZEROS.     
       03  WS-FECHA-DD      PIC 9(02)         VALUE ZEROS.     
                                                               
   01  WS-CONTROL-PAGINA.                                      
       02  WS-MAX-LINEAS                PIC 9(02)  VALUE 10.   
       02  WS-LINEA-ACTUAL              PIC 9(02)  VALUE ZEROS.
       02  WS-PAGINA-ACTUAL             PIC 9(02)  VALUE ZEROS.
                                                               
  ********************************************************     
  *                CLAVE DE CORTES                       *     
  ********************************************************     
   77  WS-ANIO-ANT    PIC 9(04)    VALUE ZEROS.                
   77  WS-ANIO-ACT    PIC 9(04)    VALUE ZEROS.                
   77  WS-SEXO-ANT    PIC X(01)    VALUE ZEROS.                
   77  WS-SEXO-ACT    PIC X(01)    VALUE ZEROS.                
  ********************************************************     
  *                  CONTADORES                          *     
  ********************************************************     
   77  CNT-LEIDOS           PIC 9(02)    VALUE ZEROS.          
   77  WS-TOTAL-GRAL        PIC 9(02)    VALUE ZEROS.          
   77  CNT-LISTADO-GRABADOS PIC 9(04) VALUE ZEROS.             
  ********************************************************     
      *                 ACUMULADORES                         *          
  ********************************************************          
   01  ACM-CLIENTES-SEXO     PIC 9(04)    VALUE ZEROS.              
   01  ACM-CLIENTES-ANIO     PIC 9(04)    VALUE ZEROS.              
  ********************************************************          
  *               LAYOUT TITULO                          *          
  ********************************************************          
   01  WS-TITULO.                                                   
       03  FILLER         PIC X(20)    VALUE 'FECHA DE EJECUCION: '.
       03  WS-DD          PIC 99       VALUE ZEROS.                 
       03  FILLER         PIC X        VALUE '-'.                   
       03  WS-MM          PIC 99       VALUE ZEROS.                 
       03  FILLER         PIC X        VALUE '-'.                   
       03  FILLER         PIC 99       VALUE 20.                    
       03  WS-AA          PIC 9(02)    VALUE ZEROS.                 
       03  FILLER         PIC X(04)    VALUE SPACES.                
       03  FILLER         PIC X(19)    VALUE 'DETALLE DE CLIENTES'. 
       03  FILLER         PIC X(4)     VALUE SPACES.                
       03  FILLER         PIC X(8)     VALUE 'PGMBBCB7'.            
       03  FILLER         PIC X(02)    VALUE SPACES.                
       03  FILLER         PIC X(18)    VALUE  'NUMERO DE PAGINA: '. 
       03  WS-PAGINA      PIC Z9       VALUE ZEROS.                 
       03  FILLER         PIC X(10)    VALUE SPACES.                
  **********************************************************        
  *                    LAYOUT SUBTITULO SUCURSAL           *        
      **********************************************************       
   01 WS-SUBTITULO-ANIO.                                           
      02 FILLER             PIC X(18) VALUE ' ANIO NACIMIENTO: '.  
      02 WS-SUB-ANIO        PIC 9(04) VALUE ZEROS.                 
      02 FILLER             PIC X(54) VALUE SPACES.                
                                                                   
  **********************************************************       
  *                    LAYOUT SUBTITULO  TIPO CUENTA       *       
  **********************************************************       
   01 WS-SUBTITULO-SEXO.                                           
      02 FILLER     PIC X(08) VALUE SPACES.                        
      02 FILLER     PIC X(07) VALUE ' SEXO: '.                     
      02 WS-SUB-SEXO     PIC X(25) VALUE ZEROS.                    
      02 FILLER     PIC X(43) VALUE SPACES.                        
                                                                   
  **********************************************************       
  *                    LAYOUT DETALLE                      *       
  **********************************************************       
   01  WS-DETALLE.                                                 
       02  FILLER          PIC X(01) VALUE SPACES.                 
       02  DET-NROCLI      PIC ZZZ.                                
       02  FILLER          PIC X(06) VALUE SPACES.                 
       02  DET-NOMAPE      PIC X(30).                              
       02  FILLER          PIC X(02) VALUE SPACES.                 
       02  DET-FECNAC      PIC X(10).                              
            02  FILLER          PIC X(05) VALUE SPACES.               
        02  DET-SEXO        PIC X(01).                            
        02  FILLER          PIC X(36) VALUE SPACES.               
                                                                  
    01 WS-DETALLE-COLUMNAS.                                       
       02 FILLER PIC X(02) VALUE SPACES.                          
       02 FILLER PIC X(07) VALUE 'NROCLI '.                       
       02 FILLER PIC X(02) VALUE SPACES.                          
       02 FILLER PIC X(30) VALUE 'NOMAPE                        '.
       02 FILLER PIC X(02) VALUE SPACES.                          
       02 FILLER PIC X(10) VALUE 'FECNAC    '.                    
       02 FILLER PIC X(02) VALUE SPACES.                          
       02 FILLER PIC X(04) VALUE 'SEXO'.                          
       02 FILLER PIC X(34) VALUE SPACES.                          
   **********************************************************     
   *                    LAYOUT TOTALES                      *     
   **********************************************************     
    01  WS-TOTAL-SEXO.                                            
        02  FILLER          PIC X(05) VALUE SPACES.               
        02  FILLER          PIC X(10) VALUE 'CLIENTES: '.         
        02  TOT-CLI-SEXO    PIC 9(04).                            
        02  FILLER          PIC X(38) VALUE SPACES.               
                                                                  
    01  WS-TOTAL-ANIO.                                            
        02  FILLER          PIC X(02) VALUE SPACES.               
        02  FILLER          PIC X(18) VALUE 'TOTAL CLIENTES: '.    
       02  TOT-CLI-ANIO    PIC 9(04).                             
       02  FILLER          PIC X(05) VALUE SPACES.                
       02  FILLER          PIC X(57) VALUE SPACES.                
                                                                  
   77  FILLER        PIC X(26) VALUE '* VARIABLES SQL          *'.
                                                                  
  *********************************************                   
  * AREA DE COMUNICACION CON DB2              *                   
  *********************************************                   
        EXEC SQL                                                  
          INCLUDE SQLCA                                           
        END-EXEC.                                                 
                                                                  
  *********************************************                   
  * TABLAS DB2 A LAS QUE SE ACCEDERA          *                   
  *********************************************                   
        EXEC SQL                                                  
          INCLUDE TBCURCLI                                        
        END-EXEC.                                                 
                                                                  
  **********************************************                  
  *   CURSOR                                   *                  
  **********************************************                  
        EXEC SQL                                     
         DECLARE CLIE_CURSOR CURSOR FOR                           
          SELECT   NROCLI,                                         
                   NOMAPE,                                         
                   FECNAC,                                         
                   SEXO                                            
              FROM  KC02803.TBCURCLI                               
              ORDER BY YEAR(FECNAC), SEXO                                
        END-EXEC.                                                  
                                                                   
                                                                   
   77  FILLER        PIC X(26) VALUE '* FINAL  WORKING-STORAGE *'. 
                                                                   
  ***************************************************************. 
   PROCEDURE DIVISION.                                             
  **************************************                           
  *                                    *                           
  *  CUERPO PRINCIPAL DEL PROGRAMA     *                           
  *                                    *                           
  **************************************                           
   MAIN-PROGRAM.                                                   
                                                                   
       PERFORM 1000-I-INICIO   THRU  1000-F-INICIO                 
                                                                   
       PERFORM 2000-I-PROCESO  THRU   2000-F-PROCESO               
                                     UNTIL WS-FIN-PROCESO      
                                                          
       PERFORM 9999-I-FINAL    THRU   9999-F-FINAL    
       .                                              
   F-MAIN-PROGRAM. GOBACK.                            
                                                      
  **************************************              
  *                                    *              
  *  CUERPO INICIO APERTURA ARCHIVOS   *              
  *                                    *              
  **************************************              
   1000-I-INICIO.                                     
                                                      
       ACCEPT WS-FECHA FROM DATE                      
       MOVE WS-FECHA-AA TO WS-AA                      
       MOVE WS-FECHA-MM TO WS-MM                      
       MOVE WS-FECHA-DD TO WS-DD                      
                                                      
       SET WS-SI-PROCESO TO TRUE                      
                                                      
                                                      
       EXEC SQL                                       
          OPEN CLIE_CURSOR                            
       END-EXEC                                       
                                                      
       IF SQLCODE NOT EQUAL ZEROS                     
              MOVE SQLCODE   TO WS-SQLCODE                             
          DISPLAY '* ERROR OPEN CURSOR      = ' WS-SQLCODE         
          MOVE 9999 TO RETURN-CODE                                 
          SET  WS-FIN-PROCESO TO TRUE                              
       END-IF                                                      
                                                                   
       OPEN OUTPUT LISTADO                                         
                                                                   
       IF FS-LISTADO NOT EQUAL ZEROS                               
          DISPLAY '* ERROR OPEN OUTPUT LISTADO: ' FS-LISTADO       
          MOVE 9999 TO RETURN-CODE                                 
          SET  WS-FIN-PROCESO TO TRUE                              
       END-IF                                                      
                                                                   
       PERFORM 1200-I-FETCH-CURSOR THRU 1200-F-FETCH-CURSOR        
                                                                   
       IF WS-SI-PROCESO                                            
           PERFORM 1400-I-MOSTRAR-TITULO THRU 1400-F-MOSTRAR-TITULO
           MOVE WS-ANIO-ACT  TO WS-ANIO-ANT                        
           MOVE WS-SEXO-ACT TO WS-SEXO-ANT                         
           PERFORM 2500-I-INICIAR-ANIO  THRU 2500-F-INICIAR-ANIO   
           PERFORM 2300-I-INICIAR-SEXO THRU 2300-F-INICIAR-SEXO    
       END-IF                                                      
       .                                                           
   1000-F-INICIO.   EXIT.                                          
                                                                      
   **************************************                         
   *                                    *                         
   *  CUERPO PRINCIPAL DEL PROGRAMA     *                         
   *                                    *                         
   **************************************                         
    2000-I-PROCESO.                                               
                                                                  
        PERFORM 1600-I-CONTROL-TITULO THRU 1600-F-CONTROL-TITULO  
                                                                  
        IF WS-ANIO-ACT NOT EQUAL WS-ANIO-ANT                      
          IF CNT-LEIDOS > 1                                       
            PERFORM 2200-I-CORTE-SEXO THRU 2200-F-CORTE-SEXO      
            PERFORM 2400-I-CORTE-ANIO THRU 2400-F-CORTE-ANIO      
          END-IF                                                  
            PERFORM 2500-I-INICIAR-ANIO THRU 2500-F-INICIAR-ANIO  
            PERFORM 2300-I-INICIAR-SEXO  THRU 2300-F-INICIAR-SEXO 
        ELSE IF WS-SEXO-ACT NOT = WS-SEXO-ANT                     
          IF CNT-LEIDOS > 1                                       
            PERFORM 2200-I-CORTE-SEXO THRU 2200-F-CORTE-SEXO      
          END-IF                                                  
            PERFORM 2300-I-INICIAR-SEXO THRU 2300-F-INICIAR-SEXO  
        END-IF                                                    
                                                                  
        PERFORM 2600-I-SUMAR-ANIO         THRU 2600-F-SUMAR-ANIO  
           PERFORM 2800-I-SUMAR-SEXO         THRU 2800-F-SUMAR-SEXO     
       PERFORM 2900-I-IMPRIMIR-DETALLE  THRU 2900-F-IMPRIMIR-DETALLE
       PERFORM 1200-I-FETCH-CURSOR THRU 1200-F-FETCH-CURSOR         
       .                                                            
   2000-F-PROCESO. EXIT.                                            
                                                                    
                                                                    
  **************************************                            
  *                                    *                            
  *  CUERPO FINAL CIERRE DE FILES      *                            
  *                                    *                            
  **************************************                            
   9999-I-FINAL.                                                    
       IF WS-SI-PROCESO OR CNT-LEIDOS > 0                           
            PERFORM 2200-I-CORTE-SEXO THRU 2200-F-CORTE-SEXO        
            PERFORM 2400-I-CORTE-ANIO THRU 2400-F-CORTE-ANIO        
       END-IF                                                       
                                                                    
       EXEC SQL                                                     
          CLOSE CLIE_CURSOR                                         
       END-EXEC                                                     
                                                                    
       IF SQLCODE NOT EQUAL ZEROS                                   
          MOVE SQLCODE TO WS-SQLCODE                                
          DISPLAY '* ERROR CLOSE CURSOR: ' WS-SQLCODE               
               MOVE 9999 TO RETURN-CODE                              
        END-IF                                                   
                                                                 
        CLOSE LISTADO                                            
                                                                 
        IF FS-LISTADO NOT EQUAL ZEROS                            
           DISPLAY '* ERROR CERRAR LISTADO:' FS-LISTADO          
           MOVE 9999 TO RETURN-CODE                              
        END-IF                                                   
                                                                 
        DISPLAY ' TOTALES OBTENIDOS'                             
        DISPLAY 'TOTAL DE LEIDOS: ' CNT-LEIDOS                   
        DISPLAY 'TOTAL DE IMPRESOS: ' CNT-LISTADO-GRABADOS       
        .                                                        
    9999-F-FINAL. EXIT.                                          
                                                                 
    1200-I-FETCH-CURSOR.                                         
                                                                 
        EXEC SQL                                                 
           FETCH  CLIE_CURSOR                                    
                  INTO                                           
                     :DCLTBCURCLI.WC-NROCLI,                     
                     :DCLTBCURCLI.WC-NOMAPE,                     
                     :DCLTBCURCLI.WC-FECNAC,                     
                     :DCLTBCURCLI.WC-SEXO                        
           END-EXEC                                            
                                                           
       EVALUATE SQLCODE                                    
           WHEN ZEROS                                      
               ADD 1 TO CNT-LEIDOS                         
               MOVE WC-FECNAC(1:4) TO WS-ANIO-ACT          
               MOVE WC-SEXO    TO WS-SEXO-ACT              
                                                           
           WHEN +100                                       
                SET WS-FIN-PROCESO TO TRUE                 
           WHEN OTHER                                      
               MOVE SQLCODE TO WS-SQLCODE                  
               DISPLAY 'ERROR EN FETCH CURSOR:' WS-SQLCODE 
               MOVE 9999 TO RETURN-CODE                    
               SET WS-FIN-PROCESO TO TRUE                  
                                                           
       END-EVALUATE                                        
       .                                                   
   1200-F-FETCH-CURSOR. EXIT.                              
                                                           
   1400-I-MOSTRAR-TITULO.                                  
                                                           
       MOVE 0 TO WS-LINEA-ACTUAL                           
                                                           
       ADD 1 TO WS-PAGINA-ACTUAL                           
                                                                       
       MOVE WS-PAGINA-ACTUAL TO WS-PAGINA                          
                                                                   
       WRITE REG-LISTADO  FROM WS-TITULO  AFTER ADVANCING  PAGE    
                                                                   
       ADD 2 TO WS-LINEA-ACTUAL                                    
       .                                                           
   1400-F-MOSTRAR-TITULO. EXIT.                                    
                                                                   
   1600-I-CONTROL-TITULO.                                          
                                                                   
       IF WS-LINEA-ACTUAL >= WS-MAX-LINEAS                         
           PERFORM 1400-I-MOSTRAR-TITULO THRU 1400-F-MOSTRAR-TITULO
          MOVE WS-ANIO-ACT TO  WS-SUB-ANIO                         
          WRITE REG-LISTADO FROM WS-SUBTITULO-ANIO                 
                                            AFTER ADVANCING 1 LINE 
          ADD 1 TO WS-LINEA-ACTUAL                                 
                                                                   
          EVALUATE WS-SEXO-ACT                                     
             WHEN 'F'                                              
               MOVE 'F - FEMENINO   ' TO WS-SUB-SEXO               
             WHEN 'M'                                              
              MOVE 'M - MASCULINO   ' TO WS-SUB-SEXO               
             WHEN 'O'                                              
               MOVE 'O - OTROS      ' TO WS-SUB-SEXO               
                 END-EVALUATE                                          
                                                                   
          WRITE REG-LISTADO FROM WS-SUBTITULO-SEXO                 
                                             AFTER ADVANCING 1 LINE
          ADD 1 TO WS-LINEA-ACTUAL                                 
       END-IF                                                      
       .                                                           
   1600-F-CONTROL-TITULO. EXIT.                                    
                                                                   
                                                                   
   2200-I-CORTE-SEXO.                                              
       MOVE ACM-CLIENTES-SEXO TO TOT-CLI-SEXO                      
       WRITE REG-LISTADO FROM WS-TOTAL-SEXO AFTER ADVANCING 1 LINE 
       ADD 1 TO WS-LINEA-ACTUAL                                    
       ADD 1 TO CNT-LISTADO-GRABADOS                               
                                                                   
       MOVE SPACES TO REG-LISTADO                                  
       WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                   
       ADD 1 TO WS-LINEA-ACTUAL                                    
       .                                                           
   2200-F-CORTE-SEXO. EXIT.                                        
                                                                   
   2300-I-INICIAR-SEXO.                                            
                                                                   
       EVALUATE WS-SEXO-ACT                                        
             WHEN 'F'                                                   
            MOVE 'F - FEMENINO    ' TO WS-SUB-SEXO                  
         WHEN 'M'                                                   
            MOVE 'M - MASCULINO   ' TO WS-SUB-SEXO                  
         WHEN 'O'                                                   
            MOVE 'O - OTROS       ' TO WS-SUB-SEXO                  
       END-EVALUATE                                                 
                                                                    
       WRITE REG-LISTADO FROM WS-SUBTITULO-SEXO                     
                                          AFTER ADVANCING 1 LINE    
                                                                    
       WRITE REG-LISTADO FROM WS-DETALLE-COLUMNAS                   
                                          AFTER ADVANCING 1 LINE    
       ADD 1 TO WS-LINEA-ACTUAL                                     
       ADD 1 TO CNT-LISTADO-GRABADOS                                
                                                                    
       MOVE 0 TO ACM-CLIENTES-SEXO                                  
       MOVE WS-SEXO-ACT TO WS-SEXO-ANT                              
       .                                                            
   2300-F-INICIAR-SEXO. EXIT.                                       
                                                                    
   2400-I-CORTE-ANIO.                                               
       MOVE ACM-CLIENTES-ANIO  TO TOT-CLI-ANIO                      
                                                                    
       WRITE REG-LISTADO FROM WS-TOTAL-ANIO AFTER ADVANCING 1 LINE  
          ADD 1 TO WS-LINEA-ACTUAL                                  
      ADD 1 TO CNT-LISTADO-GRABADOS                             
                                                                
      MOVE ALL "-" TO REG-LISTADO                               
      WRITE REG-LISTADO  AFTER ADVANCING 1 LINE                 
      ADD 1 TO WS-LINEA-ACTUAL                                  
      .                                                         
  2400-F-CORTE-ANIO. EXIT.                                      
                                                                
  2500-I-INICIAR-ANIO.                                          
                                                                
      MOVE WS-ANIO-ACT TO  WS-SUB-ANIO                          
      WRITE REG-LISTADO FROM WS-SUBTITULO-ANIO                  
                                      AFTER ADVANCING 1 LINE    
      ADD 1 TO WS-LINEA-ACTUAL                                  
      ADD 1 TO CNT-LISTADO-GRABADOS                             
                                                                
      MOVE 0 TO ACM-CLIENTES-ANIO                               
      MOVE WS-ANIO-ACT TO WS-ANIO-ANT                           
      .                                                         
  2500-F-INICIAR-ANIO. EXIT.                                    
                                                                
  2600-I-SUMAR-ANIO.                                            
                                                                
      COMPUTE ACM-CLIENTES-ANIO = ACM-CLIENTES-ANIO + 1         
           .                                                        
   2600-F-SUMAR-ANIO. EXIT.                                     
                                                                
   2800-I-SUMAR-SEXO.                                           
                                                                
       COMPUTE ACM-CLIENTES-SEXO = ACM-CLIENTES-SEXO + 1        
       .                                                        
   2800-F-SUMAR-SEXO. EXIT.                                     
                                                                
   2900-I-IMPRIMIR-DETALLE.                                     
                                                                
       MOVE WC-NROCLI           TO DET-NROCLI                   
       MOVE WC-NOMAPE           TO DET-NOMAPE                   
       MOVE WC-FECNAC           TO DET-FECNAC                   
       MOVE WC-SEXO             TO DET-SEXO                     
                                                                
                                                                
       WRITE  REG-LISTADO FROM WS-DETALLE                       
                                    AFTER ADVANCING 1 LINE      
       ADD 1 TO WS-LINEA-ACTUAL                                 
       ADD 1 TO CNT-LISTADO-GRABADOS                            
       .                                                        
   2900-F-IMPRIMIR-DETALLE. EXIT.                               
                                                                
