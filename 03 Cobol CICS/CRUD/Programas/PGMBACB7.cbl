  IDENTIFICATION DIVISION.                                        
  PROGRAM-ID. PGMBACB7.                                           
  DATA DIVISION.                                                  
  FILE SECTION.                                                   
  WORKING-STORAGE SECTION.                                        
                                                                  
  01 CT-CONSTANTES.                                               
     03 CT-MSGO.                                                  
        05 CT-MNS-01         PIC X(72) VALUE                      
                       'INGRESE DATOS A CONSULTAR'.               
        05 CT-MNS-02         PIC X(72) VALUE                      
                       'CLIENTE DADO DE BAJA CORRECTAMENTE'.      
        05 CT-MNS-03         PIC X(72) VALUE                      
               'TIPO Y NÚMERO DOCUMENTO INEXISTENTES - REINGRESE'.
        05 CT-MNS-04         PIC X(72) VALUE                      
               'TIPO DE DOCUMENTO INVALIDO - INGRESE DU, PA O PE'.
        05 CT-MNS-05         PIC X(72) VALUE                      
      'NUMERO DE DOCUMENTO INVALIDO - INGRESE UN VALOR NUMERICO'. 
        05 CT-MNS-06         PIC X(72) VALUE                      
      'CLIENTE ENCONTRADO - PRESIONE PF2 PARA ELIMINAR'.          
        05 CT-MNS-07         PIC X(72) VALUE                      
      'NUMERO DE DOCUMENTO INVALIDO - INGRESE UN VALOR NUMERICO'. 
        05 CT-MNS-08         PIC X(72) VALUE                      
                       'PROBLEMA CON ARCHIVO PERSONA'.            
              05 CT-MNS-09         PIC X(72) VALUE 'TECLA INVALIDA'.  
          05 CT-MNS-10         PIC X(72) VALUE                    
        'SELECCIONE UN CLIENTE PARA PODER DARLO DE BAJA'.         
          05 CT-MNS-EXIT       PIC X(72) VALUE                    
                         'FIN TRANSACCION T415'.                  
       03 CT-DATASET           PIC X(08)  VALUE 'PERSONA'.        
       03 CT-DATASET-LEN       PIC S9(04) COMP VALUE 160.         
                                                                  
    01 WS-CHEQUEO              PIC X.                             
       88 WS-NO                                VALUE 'N'.         
       88 WS-SI                                VALUE 'Y'.         
                                                                  
    01 WS-VARIABLES.                                              
       03 WS-MAP               PIC X(07)       VALUE 'MAP4CB7'.   
       03 WS-MAPSET            PIC X(07)       VALUE 'MAP4CB7'.   
       03 WS-LONG              PIC S9(04) COMP.                   
       03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.          
       03 WS-FECHA             PIC X(10)       VALUE SPACES.      
       03 WS-SEP-DATE          PIC X           VALUE '/'.         
       03 WS-HORA              PIC X(08)       VALUE SPACES.      
       03 WS-SEP-HOUR          PIC X           VALUE ':'.         
       03 WS-RESP              PIC S9(04) COMP.                   
       03 SW-CONFIRMAR         PIC X VALUE 'Y'.                   
       03 WS-NORMAL    PIC X VALUE '*'.                           
       03 WS-ENTER     PIC X VALUE ' '.                           
                                                                   
   01 WS-COMMAREA.                                             
      03 WS-USER-DATA.                                         
         05 WS-USER-TIPDOC    PIC X(02).                       
         05 WS-USER-NRODOC    PIC 9(11).                       
      03 WS-TIP-DOC           PIC X(02).                       
         88 WS-TIP-DOC-BOOLEAN                   VALUE 'DU'    
                                                       'PA'    
                                                       'PE'.   
      03 WS-PRIMERA           PIC 9.                           
      03 FILLER               PIC X(4).                        
                                                               
   COPY MAP4CB7.                                               
   COPY DFHBMSCA.                                              
   COPY DFHAID.                                                
   COPY CPPERSON.                                              
                                                               
   LINKAGE SECTION.                                            
                                                               
     01 DFHCOMMAREA PIC X(20).                                 
                                                               
   PROCEDURE DIVISION.                                         
                                                               
   MAIN-PROGRAM.                                               
          PERFORM 1000-I-INICIO THRU 1000-F-INICIO.        
                                                       
      PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO.      
                                                       
      PERFORM 9999-I-FINAL THRU 9999-F-FINAL.          
                                                       
  1000-I-INICIO.                                       
                                                       
      MOVE LOW-VALUES TO MAP4CB7O.                     
      MOVE DFHCOMMAREA TO WS-COMMAREA.                 
                                                       
      IF WS-PRIMERA = 0                                
                                                       
          MOVE LENGTH OF MAP4CB7O TO WS-LONG           
          MOVE CT-MNS-01 TO MSGO                       
          PERFORM 7000-I-TIME THRU 7000-F-TIME         
          MOVE WS-USER-TIPDOC       TO  TIPDOCO        
          MOVE WS-USER-NRODOC       TO  NUMDOCO        
                                                       
          EXEC CICS                                    
             SEND MAP (WS-MAP)                         
                  MAPSET (WS-MAPSET)                   
                  FROM (MAP4CB7O)                      
                  LENGTH (WS-LONG)                     
                  ERASE                                
                      FREEKB                                          
          END-EXEC                                                
          MOVE 1 TO WS-PRIMERA                                    
          PERFORM 9999-I-FINAL THRU 9999-F-FINAL                  
                                                                  
      END-IF.                                                     
                                                                  
  1000-F-INICIO. EXIT.                                            
                                                                  
  2000-I-PROCESO.                                                 
                                                                  
          PERFORM 7000-I-TIME THRU 7000-F-TIME                    
          MOVE LENGTH OF MAP4CB7O TO WS-LONG                      
                                                                  
          EXEC CICS                                               
               RECEIVE MAP    (WS-MAP)                            
                       MAPSET (WS-MAPSET)                         
                       INTO   (MAP4CB7I)                          
                       RESP   (WS-RESP)                           
          END-EXEC                                                
                                                                  
          PERFORM 2500-I-PULSAR-TECLA THRU 2500-F-PULSAR-TECLA.   
                                                                  
  2000-F-PROCESO. EXIT.                                           
     2500-I-PULSAR-TECLA.                                        
                                                             
       EVALUATE EIBAID                                       
                                                             
         WHEN DFHENTER                                       
           PERFORM 3000-I-ENTER THRU 3000-F-ENTER            
                                                             
         WHEN DFHPF2                                         
           PERFORM 3200-I-PF2 THRU 3200-F-PF2                
                                                             
         WHEN DFHPF3                                         
           PERFORM 3500-I-PF3 THRU 3500-F-PF3                
                                                             
         WHEN DFHPF12                                        
           PERFORM 9000-I-PF12 THRU 9000-F-PF12              
         WHEN OTHER                                          
           MOVE CT-MNS-09 TO   MSGO                          
           PERFORM 7000-I-TIME THRU 7000-F-TIME              
           EXEC CICS                                         
                SEND MAP    (WS-MAP)                         
                  MAPSET (WS-MAPSET)                         
                  FROM   (MAP4CB7O)                          
                  LENGTH (WS-LONG)                           
                  ERASE                                      
           END-EXEC                                          
            END-EVALUATE.                                          
                                                               
  2500-F-PULSAR-TECLA. EXIT.                                   
                                                               
  3000-I-ENTER.                                                
      MOVE TIPDOCI TO WS-TIP-DOC.                              
      MOVE TIPDOCI TO WS-USER-TIPDOC                           
      MOVE NUMDOCI TO WS-USER-NRODOC                           
      IF NOT WS-TIP-DOC-BOOLEAN                                
         MOVE LOW-VALUES TO MAP4CB7O                           
         MOVE CT-MNS-04 TO MSGO                                
         PERFORM 7000-I-TIME THRU 7000-F-TIME                  
         MOVE WS-USER-TIPDOC       TO  TIPDOCO                 
         MOVE WS-USER-NRODOC       TO  NUMDOCO                 
      ELSE                                                     
         IF NUMDOCI NOT NUMERIC                                
            MOVE LOW-VALUES TO MAP4CB7O                        
            MOVE CT-MNS-05 TO MSGO                             
            PERFORM 7000-I-TIME THRU 7000-F-TIME               
            MOVE WS-USER-TIPDOC       TO  TIPDOCO              
            MOVE WS-USER-NRODOC       TO  NUMDOCO              
         ELSE                                                  
            EXEC CICS                                          
                 READ DATASET (CT-DATASET)                     
                      RIDFLD  (WS-USER-DATA)                   
                      INTO (REG-PERSONA)                      
                      LENGTH (CT-DATASET-LEN)                 
                      EQUAL                                   
                      RESP (WS-RESP)                          
         END-EXEC                                     
                                                      
         EVALUATE WS-RESP                             
         WHEN DFHRESP(NORMAL)                         
          MOVE WS-USER-TIPDOC TO  TIPDOCO             
          MOVE WS-USER-NRODOC TO  NUMDOCO             
          MOVE PER-CLI-NRO    TO  NROCLIO             
          MOVE PER-NOMAPE     TO  NOMAPEO             
          MOVE PER-DIRECCION  TO  DIREO               
          MOVE PER-EMAIL      TO  EMAILO              
          MOVE PER-TELEFONO   TO  TELO                
          MOVE CT-MNS-06      TO  MSGO                
          MOVE LENGTH OF MAP4CB7O TO WS-LONG          
          PERFORM 7000-I-TIME THRU 7000-F-TIME        
                                                      
    WHEN DFHRESP(NOTFND)                              
        MOVE LOW-VALUES TO MAP4CB7O                   
        MOVE CT-MNS-03            TO  MSGO            
        PERFORM 7000-I-TIME THRU 7000-F-TIME          
                                                      
    WHEN OTHER                                        
                MOVE LOW-VALUES TO MAP4CB7O                
                MOVE CT-MNS-08 TO  MSGO                    
                PERFORM 7000-I-TIME THRU 7000-F-TIME       
                                                           
            END-EVALUATE.                                  
                                                           
      EXEC CICS                                            
         SEND MAP    (WS-MAP)                              
         MAPSET (WS-MAPSET)                                
         FROM   (MAP4CB7O)                                 
         LENGTH (WS-LONG)                                  
         CURSOR                                            
         ERASE                                             
      END-EXEC.                                            
                                                           
  3000-F-ENTER. EXIT.                                      
                                                           
  3200-I-PF2.                                              
                                                           
         EXEC CICS DELETE                                  
            FILE (CT-DATASET)                              
            RIDFLD (WS-USER-DATA)                          
            RESP (WS-RESP)                                 
         END-EXEC                                          
              EVALUATE WS-RESP                         
             WHEN DFHRESP(NORMAL)                  
                MOVE LOW-VALUES TO MAP4CB7O        
                MOVE CT-MNS-02  TO MSGO            
                                                   
             WHEN DFHRESP(NOTFND)                  
                MOVE CT-MNS-10  TO MSGO            
                                                   
             WHEN OTHER                            
                MOVE CT-MNS-08  TO MSGO            
  *             MOVE WS-RESP    TO MSGO            
          END-EVALUATE.                            
                                                   
       MOVE LOW-VALUES TO WS-USER-DATA             
       PERFORM 7000-I-TIME THRU 7000-F-TIME.       
       EXEC CICS                                   
           SEND MAP    (WS-MAP)                    
           MAPSET (WS-MAPSET)                      
           FROM   (MAP4CB7O)                       
           LENGTH (WS-LONG)                        
           ERASE                                   
       END-EXEC.                                   
                                                   
   3200-F-PF2. EXIT.                               
      3500-I-PF3.                                         
                                                      
      MOVE LOW-VALUES TO WS-USER-DATA.                
      MOVE LOW-VALUES TO MAP4CB7O.                    
      PERFORM 7000-I-TIME THRU 7000-F-TIME.           
      EXEC CICS                                       
          SEND MAP    (WS-MAP)                        
          MAPSET (WS-MAPSET)                          
          FROM   (MAP4CB7O)                           
          LENGTH (WS-LONG)                            
          ERASE                                       
      END-EXEC.                                       
                                                      
  3500-F-PF3. EXIT.                                   
                                                      
  7000-I-TIME.                                        
      EXEC CICS ASKTIME                               
        ABSTIME (WS-ABSTIME)                          
      END-EXEC.                                       
      EXEC CICS FORMATTIME                            
        ABSTIME (WS-ABSTIME)                          
        DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)      
        TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)           
      END-EXEC.                                       
      MOVE WS-FECHA TO FECHAO.                        
                                                      
   7000-F-TIME. EXIT.                             
                                                  
   9000-I-PF12.                                   
                                                  
           EXEC CICS SEND CONTROL                 
              ERASE                               
           END-EXEC.                              
                                                  
           EXEC CICS XCTL                         
              PROGRAM ('PGMMECB7')                
           END-EXEC.                              
                                                  
           EXEC CICS                              
              RETURN                              
           END-EXEC.                              
                                                  
   9000-F-PF12. EXIT.                             
                                                  
   9999-I-FINAL.                                  
                                                  
       EXEC CICS                                  
         RETURN                                   
         TRANSID  ('ECB7')                        
         COMMAREA (WS-COMMAREA)                   
       END-EXEC.          
                          
  9999-F-FINAL. EXIT.     
                          
