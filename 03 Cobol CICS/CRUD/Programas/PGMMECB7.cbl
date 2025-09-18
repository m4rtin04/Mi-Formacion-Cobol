  IDENTIFICATION DIVISION.                                        
  PROGRAM-ID. PGMMECB7.                                           
 *                                                                
 *****************************************************************
 *                                                               *
 * MENú GENERAL                                                  *
 * SE AGREGA PF06 PARA CONSULTA GENERAL Y PAGINADO COMO EJEMPLO  *
 *                                                               *
 *****************************************************************
 *                                                                
  DATA DIVISION.                                                  
  FILE SECTION.                                                   
  WORKING-STORAGE SECTION.                                        
                                                                  
  01 CT-CONSTANTES.                                               
     03 CT-MSGO.                                                  
        05 CT-MNS-01         PIC X(72) VALUE                      
                             'TIPO DE DOCUMENTO INVALIDO'.        
        05 CT-MNS-02         PIC X(72) VALUE                      
                             'NUMERO DE DOCUMENTO INVALIDO'.      
        05 CT-MNS-03         PIC X(72) VALUE 'TECLA INVALIDA'.    
        05 CT-MNS-04         PIC X(72) VALUE 'BAJA REALIZADA'.    
        05 CT-MNS-05         PIC X(72) VALUE 'ALTA EFECTUADA'.    
        05 CT-MNS-06         PIC X(72) VALUE                      
                                 'MODIFICACION REALIZADA'.            
        05 CT-MNS-07         PIC X(72) VALUE 'CONSULTA REALIZADA'.
        05 CT-MNS-08         PIC X(72) VALUE 'DOCUMENTO INVALIDO'.
        05 CT-MNS-EXIT       PIC X(72) VALUE                      
                             'FIN TRANSACCION BD1F'.              
                                                                  
  01 WS-VARIABLES.                                                
     03 WS-MAP               PIC X(07)       VALUE 'MAP2CB7'.     
     03 WS-MAPSET            PIC X(07)       VALUE 'MAP2CB7'.     
     03 WS-LONG              PIC S9(04) COMP.                     
     03 WS-ABSTIME           PIC S9(16) COMP VALUE +0.            
     03 WS-FECHA             PIC X(10)       VALUE SPACES.        
     03 WS-SEP-DATE          PIC X           VALUE '/'.           
     03 WS-HORA              PIC X(08)       VALUE SPACES.        
     03 WS-SEP-HOUR          PIC X           VALUE ':'.           
     03 WS-RESP              PIC S9(04) COMP.                     
     03 WS-ERR               PIC X(15).                           
                                                                  
  01 WS-COMMAREA.                                                 
     03 WS-USER-DATA.                                             
        05 WS-USER-TIPDOC    PIC X(02).                           
        05 WS-USER-NUMDOC    PIC 9(11).                           
     03 WS-TIP-DOC           PIC X(02).                           
        88 WS-TIP-DOC-BOOLEAN                   VALUE 'DU'        
                                                      'PA'        
                                                          'PE'.     
     03 WS-PRIMERA           PIC 9.                             
     03 FILLER               PIC X(4).                          
                                                                
                                                                
  COPY MAP2CB7.                                                 
  COPY DFHBMSCA.                                                
  COPY DFHAID.                                                  
                                                                
  LINKAGE SECTION.                                              
  01 DFHCOMMAREA PIC X(20).                                     
                                                                
  PROCEDURE DIVISION.                                           
  MAIN-PROGRAM.                                                 
                                                                
      PERFORM 1000-I-INICIO THRU 1000-F-INICIO.                 
                                                                
      PERFORM 2000-I-PROCESO THRU 2000-F-PROCESO.               
                                                                
                                                                
  1000-I-INICIO.                                                
                                                                
      MOVE DFHCOMMAREA TO WS-COMMAREA.                          
                                                                
      IF EIBCALEN = 0 OR DFHAID = DFHPF5                        
          MOVE LENGTH OF MAP2CB7O TO WS-LONG                      
          MOVE LOW-VALUES TO MAP2CB7O                             
          MOVE 'INGRESE LA OPCION DESEADA'  TO MSGO               
          PERFORM 9500-I-SENDMAP THRU 9500-F-SENDMAP              
      ELSE                                                        
          EXEC CICS                                               
             RECEIVE MAP (WS-MAP)                                 
                   MAPSET (WS-MAPSET)                             
                   INTO (MAP2CB7I)                                
                   RESP(WS-RESP)                                  
           END-EXEC                                               
                                                                  
       END-IF.                                                    
                                                                  
  1000-F-INICIO. EXIT.                                            
                                                                  
  2000-I-PROCESO.                                                 
                                                                  
      EVALUATE WS-RESP                                            
        WHEN DFHRESP (NORMAL)                                     
          MOVE LENGTH OF MAP2cb7O TO WS-LONG                      
          PERFORM 2500-I-PULSAR-TECLA THRU 2500-F-PULSAR-TECLA    
                                                                  
                                                                  
        WHEN OTHER                                                
                MOVE 'ERROR 2000-PROC' TO WS-ERR                  
            EXEC CICS SEND TEXT FROM (WS-ERR) END-EXEC        
            PERFORM 5500-I-PF12 THRU 5500-F-PF12              
                                                              
      END-EVALUATE.                                           
                                                              
      PERFORM 9500-I-SENDMAP THRU 9500-F-SENDMAP.             
                                                              
  2000-F-PROCESO. EXIT.                                       
                                                              
  2500-I-PULSAR-TECLA.                                        
                                                              
        EVALUATE EIBAID                                       
                                                              
        WHEN DFHENTER                                         
          MOVE LENGTH OF MAP2CB7O TO WS-LONG                  
          MOVE LOW-VALUES TO MAP2CB7O                         
          MOVE 'INGRESE LA OPCION DESEADA'  TO MSGO           
                                                              
        WHEN DFHPF1                                           
            PERFORM 3000-I-PF1 THRU 3000-F-PF1                
                                                              
        WHEN DFHPF2                                           
            PERFORM 3500-I-PF2 THRU 3500-F-PF2                
        WHEN DFHPF3                                            
            PERFORM 4000-I-PF3 THRU 4000-F-PF3                 
                                                               
        WHEN DFHPF4                                            
            PERFORM 4500-I-PF4 THRU 4500-F-PF4                 
                                                               
        WHEN DFHPF5                                            
            PERFORM 4600-I-PF5 THRU 4600-F-PF5                 
                                                               
        WHEN DFHPF6                                            
            PERFORM 4700-I-PF6 THRU 4700-F-PF6                 
                                                               
        WHEN DFHPF12                                           
            PERFORM 5500-I-PF12 THRU 5500-F-PF12               
                                                               
        WHEN OTHER                                             
            MOVE CT-MNS-03 TO MSGO                             
                                                               
        END-EVALUATE.                                          
                                                               
  2500-F-PULSAR-TECLA. EXIT.                                   
                                                               
  3000-I-PF1.                                                  
                                                               
 *    HACER XCTL A LA TRANSACCIóN DD1F. PROGRAMA PGMALD1F      
          MOVE 0 TO WS-PRIMERA.                                  
      MOVE CT-MNS-05 TO MSGO.                                
                                                             
      EXEC CICS XCTL                                         
           PROGRAM('PGMALCB7')                               
           COMMAREA (WS-COMMAREA)                            
      END-EXEC.                                              
                                                             
  3000-F-PF1. EXIT.                                          
                                                             
  3500-I-PF2.                                                
                                                             
 *    HACER XCTL A LA TRANSACCIóN ED1F. PROGRAMA PGMBAD1F    
                                                             
      MOVE 0 TO WS-PRIMERA.                                  
      MOVE CT-MNS-04 TO MSGO.                                
                                                             
      EXEC CICS XCTL                                         
           PROGRAM('PGMBACB7')                               
           COMMAREA (WS-COMMAREA)                            
      END-EXEC.                                              
                                                             
  3500-F-PF2. EXIT.                                          
                                                             
  4000-I-PF3.                                                
                                                                    
       MOVE CT-MNS-06 TO MSGO.                                  
       MOVE 0 TO WS-PRIMERA.                                    
  *    HACER XCTL A LA TRANSACCIóN FD1F. PROGRAMA PGMMOD1F      
                                                                
       EXEC CICS                                                
            XCTL PROGRAM ('PGMMOCB7')                           
            COMMAREA (WS-COMMAREA)                              
       END-EXEC.                                                
                                                                
                                                                
                                                                
   4000-F-PF3. EXIT.                                            
                                                                
   4500-I-PF4.                                                  
                                                                
  *    MOVE 'CONSULTA REALIZADA' TO MSGO.                       
       MOVE TIPDOCI TO WS-TIP-DOC.                              
       IF NOT WS-TIP-DOC-BOOLEAN                                
          INITIALIZE MAP2CB7O                                   
          MOVE CT-MNS-08 TO MSGO                                
       ELSE                                                     
          IF NUMDOCI NOT NUMERIC                                
             INITIALIZE MAP2CB7O                                
             MOVE CT-MNS-08 TO MSGO                             
              ELSE                                                  
             MOVE TIPDOCI TO WS-USER-TIPDOC                     
             MOVE NUMDOCI TO WS-USER-NUMDOC                     
  *    HACER XCTL A LA TRANSACCIóN AD1F. PROGRAMA PGMPRD1F      
                                                                
             EXEC CICS XCTL                                     
                  PROGRAM('PGMPRCB7')                           
                  COMMAREA (WS-COMMAREA)                        
             END-EXEC                                           
          END-IF                                                
       END-IF.                                                  
                                                                
       PERFORM 9500-I-SENDMAP THRU 9500-F-SENDMAP.              
                                                                
   4500-F-PF4. EXIT.                                            
                                                                
   4600-I-PF5.                                                  
                                                                
       MOVE LOW-VALUES TO MAP2CB7O.                             
                                                                
   4600-F-PF5. EXIT.                                            
                                                                
   4700-I-PF6.                                                  
                                                                
       MOVE TIPDOCI TO WS-TIP-DOC.                              
          IF NOT WS-TIP-DOC-BOOLEAN                               
         INITIALIZE MAP2CB7O                                  
         MOVE CT-MNS-08 TO MSGO                               
      ELSE                                                    
         IF NUMDOCI NOT NUMERIC                               
            INITIALIZE MAP2CB7O                               
            MOVE CT-MNS-08 TO MSGO                            
         ELSE                                                 
            MOVE TIPDOCI TO WS-USER-TIPDOC                    
            MOVE NUMDOCI TO WS-USER-NUMDOC                    
                                                              
            EXEC CICS                                         
                 XCTL PROGRAM ('PGMACCB7')                    
                 COMMAREA (WS-COMMAREA)                       
            END-EXEC                                          
         END-IF                                               
      END-IF.                                                 
                                                              
      PERFORM 9500-I-SENDMAP THRU 9500-F-SENDMAP.             
                                                              
  4700-F-PF6. EXIT.                                           
                                                              
  5500-I-PF12.                                                
                                                              
      EXEC CICS                                               
               SEND CONTROL ERASE                              
      END-EXEC                                             
                                                           
      EXEC CICS                                            
           SEND TEXT                                       
                FROM (CT-MNS-EXIT)                         
      END-EXEC                                             
                                                           
      EXEC CICS                                            
           RETURN                                          
      END-EXEC.                                            
                                                           
  5500-F-PF12. EXIT.                                       
                                                           
  9500-I-SENDMAP.                                          
                                                           
      PERFORM 9600-I-TIME THRU 9600-F-TIME                 
      EXEC CICS                                            
           SEND MAP    (WS-MAP)                            
                MAPSET (WS-MAPSET)                         
                FROM   (MAP2CB7O)                          
                LENGTH (WS-LONG)                           
                ERASE                                      
                FREEKB                                     
      END-EXEC.                                            
                                                                 
       EXEC CICS                                             
            RETURN                                           
            TRANSID  ('Bcb7')                                
            COMMAREA (WS-COMMAREA)                           
       END-EXEC.                                             
   9500-F-SENDMAP. EXIT.                                     
                                                             
   9600-I-TIME.                                              
                                                             
       EXEC CICS ASKTIME                                     
         ABSTIME (WS-ABSTIME)                                
       END-EXEC.                                             
                                                             
       EXEC CICS FORMATTIME                                  
         ABSTIME (WS-ABSTIME)                                
         DDMMYYYY (WS-FECHA) DATESEP(WS-SEP-DATE)            
         TIME (WS-HORA) TIMESEP(WS-SEP-HOUR)                 
       END-EXEC.                                             
                                                             
       MOVE WS-FECHA TO FECHAO.                              
                                                             
   9600-F-TIME. EXIT.                                        
                                                             
   9999-I-FINAL.                                             
                                     
       EXEC CICS                 
         RETURN                  
       END-EXEC.                 
                                 
   9999-F-FINAL. EXIT.           
