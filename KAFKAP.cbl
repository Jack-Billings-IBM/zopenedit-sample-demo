       PROCESS CICS('cobol3,sp')
       PROCESS ARITH(EXTEND) TRUNC(BIN) LIST MAP XREF RENT
       IDENTIFICATION DIVISION.
       PROGRAM-ID. KAFKAP.
      *****************************************************************
      * MODULE NAME = KAFKAP                                      *
      *                                                               *
      * DESCRIPTIVE NAME = CICS Event Process and Transform           *
      *                                                               *
      *     This program will be passed a CICS channel in             *
      *     container-based (CCE) format. The CICS channel has a      *
      *     collection of containers. The containers are:             *
      *     DFHEP.CCECONTEXT, DFHEP.NAME.nnnnn, and DFHEP.DATA.nnnnn. *
      *     It will extract the EPFE-DATE-TIME value from the         *
      *     DFHE.CCECONTEXT container. The copybook DFHEPFEO in       *
      *     CTS560.CICS.SDFHCOB describes the CICS Event Processing   *
      *     contextual header (DFHE.CCECONTEXT). This program will    *
      *     also extract the DFHEP.DATA.nnnnn associated with each    *
      *     DFHEP.NAME.nnnnn, where nnnnn is an ascending number      *
      *     starting with 00001. The DFHEP.NAME.nnnnn values are the  *
      *     information source names defined for an event capture.    *
      *     DFHEP.DATA.nnnnn is the data associated with each         *
      *     information source name. The EPFE-DATE-TIME value and     *
      *     DFHEP.DATA.nnnnn values will be placed in the INSREQ      *
      *     copybook. Once all the data from the event is placed in   *
      *     the INSREQ copybook, we will transform the data to JSON   *
      *     format and put the data in a container named              *
      *     JSON-DOCUMENT in a channel named TRANSFORM. We will then  *
      *     pass that channel to the Java program PUTQ.               *
      *                                                               *
      *     In order to keep this sample short and easy to understand *
      *     the recommended testing of CICS return codes has been     *
      *     omitted wherever the default CICS exception handling has  *
      *     a suitable effect.                                        *
      *                                                               *
      *     Note that this program is unable to format binary and     *
      *     decimal floating point numbers.                           *
      *                                                               *
      * NOTES :                                                       *
      *  DEPENDENCIES = S/390                                         *
      *  RESTRICTIONS = None                                          *
      *  REGISTER CONVENTIONS = Normal                                *
      *  MODULE TYPE = Executable                                     *
      *  PROCESSOR = COBOL                                            *
      *  ATTRIBUTES = Read only, Reentrant                            *
      *                                                               *
      *---------------------------------------------------------------*
      *                                                               *
      * CHANGE ACTIVITY :                                             *
      *                                                               *
      *                                                               *
      *****************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01 PROGRAM-WORKING-STORAGE.
          03 EPCONTEXT              PIC S9(8) COMP.
          03 EPCONTEXTLENGTH        PIC S9(8) COMP.
          03 EPDESCRIPTORLENGTH     PIC S9(8) COMP.
          03 EPADAPTPARMLENGTH      PIC S9(8) COMP.
          03 EPADAPTERLENGTH        PIC S9(8) COMP.
          03 EPNAME                 PIC x(32).
          03 EPNAMELENGTH           PIC S9(8) COMP.
          03 EPDATA                 PIC x(40).
          03 EPDATALENGTH           PIC S9(8) COMP.
          03 EPVALUE                PIC 9(4).
          03 EPVALUELENGTH          PIC S9(8) COMP VALUE +4.
          03 RESP                   PIC S9(8) COMP.
          03 RESP2                  PIC S9(8) COMP.
          03 RECOVERABILITY         PIC S9(8) COMP.
             88 RECOVERABILITY-YES       VALUE dfhvalue (RECOVERABLE).
             88 RECOVERABILITY-NO        VALUE dfhvalue (NOTRECOVABLE).
          03 ITEMNUM                PIC 9(5).

       01 CONTAINER-DATA.
          10 C-Stock-Item-Reference            PIC x(5).
          10 C-Stock-level                     PIC x(5).
          10 C-Order-quantity                  PIC x(5).
          10 C-Description                     PIC x(40).

      *   Copy INSREQ copybook that we will copy the event data to
       01 INSREQ-COPYBOOK.
          COPY INSREQ.

      *10 epfe-date-time                  PIC x(29).
      *10 stock-item-reference            PIC 9(4).
      *10 description                     PIC x(40)
      *10 stock-level                     PIC 9(4).
      *10 order-quantity                  PIC 9(4).

      *   Copy the DFHEPFEO copybook that has the event context data
       01 EPCONTEXT-COPYBOOK.
          COPY DFHEPFEO.

      *10 EPFE.
      *   Context data
      *   15 EPFE-CONTEXT-DATA.
      *     Structure Identifier
      *     20 EPFE-STRUCID            PIC X(4).
      *       88 EPFE-STRUC-ID                      VALUE 'EPFE'.
      *     Version
      *     20 EPFE-VERSION            PIC X(4).
      *       88 EPFE-VERSION-1                     VALUE '0001'.
      *       88 EPFE-VERSION-2                     VALUE '0002'.
      *     Event Binding Name
      *     20 EPFE-EVENT-BINDING      PIC X(32).
      *     Event Binding user tag
      *     20 EPFE-EBUSERTAG          PIC X(8).
      *     Business event name
      *     20 EPFE-BUSINESSEVENT      PIC X(32).
      *     Network UOW ID
      *     20 EPFE-NETWORK-UOWID      PIC X(54).
      *     Network Applid Qualifier Applid
      *     20 EPFE-NETQUAL-APPLID     PIC X(17).
      *     Date Time
      *     20 EPFE-DATE-TIME          PIC X(29).
      *     Capture Specification Name
      *     20 EPFE-CS-NAME            PIC X(32).
      *     Item count                                         @R145050A
      *     20 EPFE-ITEMCOUNT          PIC s9(4) COMP.
      *     Reserved                                           @R145050C
      *     20 FILLER                  PIC X(14).


       77  NO-PAYLOAD    PIC X(15) VALUE 'NO PAYLOAD DATA'.

       01  COMMAND-RESPONSE-FIELDS.
           05  COMMAND-RESPONSE              PIC S9(8) COMP-5.
           05  COMMAND-RESPONSE-CHAR
                 REDEFINES COMMAND-RESPONSE
                                             PIC X(4).
           05  COMMAND-RESP2                 PIC S9(8) COMP-5.
           05  LINK-RESPONSE                 PIC S9(8) COMP-5.
           05  LINK-RESP2                    PIC S9(8) COMP-5.
           05  PUT-RESP                      PIC S9(8) COMP-5.
           05  PUT-RESP2                     PIC S9(8) COMP-5.
           05  GET-RESP                      PIC S9(8) COMP-5.
           05  GET-RESP2                     PIC S9(8) COMP-5.

       01  JSON-DOCUMENT                     PIC X(255).

       01  CHANNEL-CONTAINER-NAMES.
           05  CHANNEL-NAME                  PIC X(16).
           05  JSON-CONTAINER                PIC X(16).
           05  COPYBOOK-CONTAINER            PIC X(16).
           05  CONTAINERNAME                 PIC X(16).

       01  JSON-ROUTINE-WORKING-FIELDS.
           05  JSON-DOCUMENT-LEN             PIC S9(8) COMP-5.
           05  COPYBOOK-LEN                  PIC S9(8) COMP-5.
           05  JSON-ERROR                    PIC x(400).
           05  JSON-ERRORLEN                 PIC S9(8) COMP-5.


      ******************************************************************
       LINKAGE SECTION.
      ******************************************************************
      *
       PROCEDURE DIVISION.
      ******************************************************************
       MAIN-PROGRAM SECTION.
      ******************************************************************
      *
           PERFORM INITIAL-PROCESSING.
      *
      *    Process the event data items
           PERFORM PROCESS-DATA-ITEM
              VARYING ITEMNUM FROM 1 BY 1
              UNTIL ITEMNUM > EPFE-ITEMCOUNT.

      *    Transform event data to JSON format
           PERFORM TRANSFORM-DATA-TO-JSON.

      *    Call PUTQ Java program with JSON formatted data in          *
      *    CHANNEL-NAME                                                *
           EXEC CICS LINK PROGRAM('PUTQ') CHANNEL(CHANNEL-NAME)
                END-EXEC.
      *
      *    Return to caller
           EXEC CICS RETURN
                END-EXEC.
      *
       MAIN-PROGRAM-EXIT.
           EXIT.
      *
      ******************************************************************
       INITIAL-PROCESSING SECTION.
      ******************************************************************
      *
           MOVE LENGTH OF EPCONTEXT-COPYBOOK TO EPCONTEXTLENGTH.
      *    Obtain the DFHEP.CCECONTEXT container
           EXEC CICS GET CONTAINER('DFHEP.CCECONTEXT')
                INTO (EPCONTEXT-COPYBOOK)
                FLENGTH(EPCONTEXTLENGTH)
                END-EXEC.

      *    Copy the date and time of event to copybook
           MOVE EPFE-DATE-TIME OF EPCONTEXT-COPYBOOK
              TO EPFE-DATE-TIME OF INSREQ-COPYBOOK.
      *    Just for testing

           COMPUTE EPFE-ITEMCOUNT = 4.
      *
       INITIAL-PROCESSING-EXIT.
           EXIT.
      *
      ******************************************************************
       PROCESS-DATA-ITEM SECTION.
      ******************************************************************
      *
      *    Build the data container name: DFHEP.DATA.nnnnn
           STRING 'DFHEP.NAME.' DELIMITED BY SIZE
                  ITEMNUM DELIMITED BY SIZE
              INTO CONTAINERNAME
           END-STRING.

      *    Obtain the DFHEP.DATA.nnnnn container - if present
           MOVE LENGTH OF EPNAME TO EPNAMELENGTH.
           EXEC CICS GET CONTAINER(CONTAINERNAME)
                INTO (EPNAME)
                FLENGTH(EPNAMELENGTH)
                RESP(RESP) RESP2(RESP2)
                END-EXEC.

           STRING 'DFHEP.DATA.' DELIMITED BY SIZE
                  ITEMNUM DELIMITED BY SIZE
              INTO CONTAINERNAME
           END-STRING.

           IF EPNAME(1:EPNAMELENGTH) = 'Stock_item_reference'
                MOVE LENGTH OF C-STOCK-ITEM-REFERENCE TO EPVALUELENGTH
                EXEC CICS GET CONTAINER(CONTAINERNAME)
                    INTO (C-STOCK-ITEM-REFERENCE)
                    FLENGTH(EPVALUELENGTH)
                    RESP(RESP) RESP2(RESP2)
                END-EXEC
                SUBTRACT 1 FROM EPVALUELENGTH
                MOVE C-STOCK-ITEM-REFERENCE(2:EPVALUELENGTH)
                    TO STOCK-ITEM-REFERENCE OF INSREQ-COPYBOOK
           END-IF.

           IF EPNAME(1:EPNAMELENGTH) = 'Description'
                MOVE LENGTH OF C-DESCRIPTION TO EPVALUELENGTH
                EXEC CICS GET CONTAINER(CONTAINERNAME)
                    INTO (C-DESCRIPTION)
                    FLENGTH(EPVALUELENGTH)
                    RESP(RESP) RESP2(RESP2)
                END-EXEC
                MOVE C-DESCRIPTION(1:EPVALUELENGTH)
                    TO DESCRIPTION OF INSREQ-COPYBOOK
           END-IF.

           IF EPNAME(1:EPNAMELENGTH) = 'Stock_level'
                MOVE LENGTH OF C-STOCK-LEVEL TO EPVALUELENGTH
                EXEC CICS GET CONTAINER(CONTAINERNAME)
                    INTO (C-STOCK-LEVEL)
                    FLENGTH(EPVALUELENGTH)
                    RESP(RESP) RESP2(RESP2)
                END-EXEC
                SUBTRACT 1 FROM EPVALUELENGTH
                MOVE C-STOCK-LEVEL(2:EPVALUELENGTH)
                    TO STOCK-LEVEL OF INSREQ-COPYBOOK
           END-IF.

           IF EPNAME(1:EPNAMELENGTH) = 'Order_quantity'
                MOVE LENGTH OF C-ORDER-QUANTITY TO EPVALUELENGTH
                EXEC CICS GET CONTAINER(CONTAINERNAME)
                    INTO (C-ORDER-QUANTITY)
                    FLENGTH(EPVALUELENGTH)
                    RESP(RESP) RESP2(RESP2)
                END-EXEC
                SUBTRACT 1 FROM EPVALUELENGTH
                MOVE C-ORDER-QUANTITY(2:EPVALUELENGTH)
                    TO ORDER-QUANTITY OF INSREQ-COPYBOOK
           END-IF.

      *
       PROCESS-DATA-ITEM-EXIT.
           EXIT.
      ******************************************************************
       TRANSFORM-DATA-TO-JSON SECTION.
      ******************************************************************
      *
      *    Transform container data to JSON format
      *
      * On input, data will be in TRANSFORM-COPYBOOK
      * On output, JSON document should be in field JSON-DOCUMENT

           MOVE 'Transform' TO CHANNEL-NAME.
           MOVE 'JSON-document' TO JSON-CONTAINER.
           MOVE 'COBOL-copybook' TO COPYBOOK-CONTAINER.

      *    Create container with data from INSREQ-COPYBOOK
           MOVE LENGTH OF INSREQ-COPYBOOK TO COPYBOOK-LEN.
           EXEC CICS PUT CONTAINER(COPYBOOK-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                BIT
                FROM (INSREQ-COPYBOOK)
                FLENGTH(COPYBOOK-LEN)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.

      *    Transform data to JSON format
      *    TRANSFORMER('') needs to be the same value in
      *    JSONTRANSFRM= from your LS2JS01 job
           EXEC CICS TRANSFORM DATATOJSON
                CHANNEL(CHANNEL-NAME)
                INCONTAINER(COPYBOOK-CONTAINER)
                OUTCONTAINER(JSON-CONTAINER)
                TRANSFORMER('d2json')
                RESP(COMMAND-RESPONSE)
                RESP2(COMMAND-RESP2)
                END-EXEC.

           EVALUATE COMMAND-RESPONSE
                WHEN DFHRESP(NORMAL)
                    CONTINUE
                WHEN OTHER
                    MOVE LENGTH OF JSON-ERROR TO JSON-ERRORLEN
                    EXEC CICS GET CONTAINER('DFHJSON-ERRORMSG')
                       CHANNEL(CHANNEL-NAME)
                       INTO(JSON-ERROR)
                       FLENGTH(JSON-ERRORLEN)
                       RESP(GET-RESP)
                       RESP2(GET-RESP2)
                    END-EXEC
                DISPLAY JSON-ERROR
           END-EVALUATE.

      *    Put JSON formatted data into JSON-DOCUMENT
           MOVE LENGTH OF JSON-DOCUMENT TO JSON-DOCUMENT-LEN.
           EXEC CICS GET CONTAINER(JSON-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                INTO (JSON-DOCUMENT)
                FLENGTH(JSON-DOCUMENT-LEN)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.
           EXEC CICS DELETE CONTAINER(JSON-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.
           EXEC CICS PUT CONTAINER(JSON-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                CHAR
                FROM(JSON-DOCUMENT)
                FLENGTH(JSON-DOCUMENT-LEN)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.
           EXEC CICS DELETE CONTAINER(COPYBOOK-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.
           EXEC CICS PUT CONTAINER(COPYBOOK-CONTAINER)
                CHANNEL(CHANNEL-NAME)
                CHAR
                FROM (INSREQ-COPYBOOK)
                FLENGTH(COPYBOOK-LEN)
                RESP(PUT-RESP)
                RESP2(PUT-RESP2)
                END-EXEC.

       TRANSFORM-DATA-TO-JSON-EXIT.
           EXIT.