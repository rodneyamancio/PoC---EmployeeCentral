*  ======================================================================*
*                                                                        *
*                          HR Solutions Tecnologia                       *
*                                                                        *
*  ======================================================================*
*   Programa    : ZHRST_SFSF_SYNC_PICKLIST                               *
*   Tipo        : Interface Outbound                                     *
*   Módulo      : HCM                                                    *
*   Transação   : ZHRST_SFSF_SYNC_PICK                                   *
*   Descrição   : Interface para Sincronização das Picklists SFSF x SAP  *
*   Autor       : Fábrica HRST                                           *
*   Data        : 31/08/2015                                             *
*  ----------------------------------------------------------------------*
*   Changes History                                                      *
*  ----------------------------------------------------------------------*
*   Data       | Autor     | Request    | Descrição                      *
*  ------------|-----------|------------|--------------------------------*
*   31/08/2015 |           | E03K9XY383 | Início do desenvolvimento      *
*  ------------|-----------|------------|--------------------------------*
*  ======================================================================*
REPORT  zhrst_sfsf_sync_picklist.

*------------------------------------------------------------------------*
* Tabela Interna
*------------------------------------------------------------------------*
DATA: t_credenciais TYPE TABLE OF zhrst_sfsf_crede,
      t_picklist    TYPE TABLE OF zhrst_sfsf_pickl.

*------------------------------------------------------------------------*
* Constantes
*------------------------------------------------------------------------*
CONSTANTS: c_error(1)   TYPE c VALUE 'E',
           c_success(1) TYPE c VALUE 'S',
           c_warning(1) TYPE c VALUE 'W'.
*------------------------------------------------------------------------*
* Tela de Seleção
*------------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS: rb_todos RADIOBUTTON GROUP g1 DEFAULT 'X',
            rb_ativo RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK b1.

*------------------------------------------------------------------------*
* Início da Execução
*------------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM zf_query_picklist.
  PERFORM zf_atualiza_sap.

*&---------------------------------------------------------------------*
*&      Form  ZF_QUERY_PICKLIST
*&---------------------------------------------------------------------*
FORM zf_query_picklist .

  DATA: l_count             TYPE i,
        l_count_tot         TYPE i,
        l_lote              TYPE i,
        l_o_erro            TYPE REF TO cx_root,
        l_o_erro_apl        TYPE REF TO cx_ai_application_fault,
        l_o_erro_fault      TYPE REF TO zsfi_cx_dt_fault,
        l_text              TYPE string,
        l_sessionid         TYPE string,
        l_batchsize         TYPE i,
        l_tabix             TYPE sy-tabix,
        l_o_query           TYPE REF TO zsfi_co_si_query_picklist_opti,
        w_request           TYPE zsfi_mt_query_request,
        w_response          TYPE zsfi_mt_query_picklist_respons,
        t_sfobject          TYPE zsfi_query_picklist_option_tab,
        w_query             LIKE w_request-mt_query_request-query,
        w_sfobject          LIKE LINE OF t_sfobject,
        l_has_more          TYPE c,
        w_credenciais       LIKE LINE OF t_credenciais,
        w_picklist          LIKE LINE OF t_picklist,
        l_status            TYPE string.

  DEFINE add_internal_table.
    w_picklist-empresa      = w_credenciais-empresa.
    w_picklist-id           = w_sfobject-id+5.
    w_picklist-picklistid   = w_sfobject-picklist_id.
    w_picklist-status       = w_sfobject-status.
    w_picklist-externalcode = w_sfobject-externalcode.
    w_picklist-en_us        = w_sfobject-en_us.
    w_picklist-uname        = sy-uname.
    w_picklist-aedtm        = sy-datum.
    w_picklist-aezet        = sy-uzeit.
    append w_picklist to t_picklist.
    clear w_picklist.
  END-OF-DEFINITION.

  SELECT * INTO TABLE t_credenciais FROM zhrst_sfsf_crede.

  LOOP AT t_credenciais INTO w_credenciais.

    PERFORM zf_login_successfactors USING w_credenciais-empresa
                                 CHANGING l_sessionid
                                          l_batchsize.



    TRY.

        IF rb_ativo EQ abap_true.
          CONCATENATE ' where status = ' text-000 'ACTIVE' text-000 INTO l_status.
        ELSE.
          CLEAR l_status.
        ENDIF.

        CREATE OBJECT l_o_query.

        CONCATENATE 'select picklistId, status, externalcode, en_us from PicklistOption'
                    l_status
               INTO w_query-query_string RESPECTING BLANKS.

        w_request-mt_query_request-query      = w_query.
        CONCATENATE 'JSESSIONID=' l_sessionid INTO w_request-mt_query_request-session_id.

        CALL METHOD l_o_query->si_query_picklist_option
          EXPORTING
            output = w_request
          IMPORTING
            input  = w_response.

      CATCH cx_ai_system_fault INTO l_o_erro.
        PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

        l_text = l_o_erro->get_text( ).
        PERFORM zf_log USING space c_error l_text space.

      CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
        PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

        l_text = l_o_erro_fault->standard-fault_text.
        PERFORM zf_log USING space c_error l_text space.

      CATCH cx_ai_application_fault INTO l_o_erro_apl.
        PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

        l_text = l_o_erro_apl->get_text( ).
        PERFORM zf_log USING space c_error l_text space.

    ENDTRY.

    LOOP AT w_response-mt_query_picklist_response-sfobject INTO w_sfobject.

      add_internal_table.

    ENDLOOP.

*/ QueryMore
    DATA: l_query_more        TYPE REF TO zsfi_co_si_query_more_picklist,
          w_request_more      TYPE zsfi_mt_query_more_request,
          w_response_more     TYPE zsfi_mt_query_picklist_respons,
          l_tentativa         TYPE i.

    l_has_more = w_response-mt_query_picklist_response-has_more.
    w_request_more-mt_query_more_request-query-query_session_id = w_response-mt_query_picklist_response-query_session_id.

    WHILE l_has_more EQ abap_true.

      w_request_more-mt_query_more_request-session_id = w_request-mt_query_request-session_id.

      TRY.

          CREATE OBJECT l_query_more.

          CALL METHOD l_query_more->si_query_more_picklist_option
            EXPORTING
              output = w_request_more
            IMPORTING
              input  = w_response_more.

        CATCH cx_ai_system_fault INTO l_o_erro.
          PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

          l_text = l_o_erro->get_text( ).
          PERFORM zf_log USING space c_error l_text space.

        CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
          PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

          l_text = l_o_erro_fault->standard-fault_text.
          PERFORM zf_log USING space c_error l_text space.

        CATCH cx_ai_application_fault INTO l_o_erro_apl.
          PERFORM zf_log USING space c_error 'Erro ao Efetuar QUERY para a Empresa'(020) ''.

          l_text = l_o_erro_apl->get_text( ).
          PERFORM zf_log USING space c_error l_text space.

      ENDTRY.

      LOOP AT w_response_more-mt_query_picklist_response-sfobject INTO w_sfobject.

        add_internal_table.

      ENDLOOP.

      IF l_tentativa EQ 0.
        l_has_more = abap_true.
      ELSE.
        l_has_more = w_response_more-mt_query_picklist_response-has_more.
      ENDIF.

      ADD 1 TO l_tentativa.

      w_request_more-mt_query_more_request-query-query_session_id = w_response_more-mt_query_picklist_response-query_session_id.

    ENDWHILE.
*/

  ENDLOOP.

ENDFORM.                    " ZF_QUERY_PICKLIST

*  &---------------------------------------------------------------------*
*  &      Form  ZF_LOGIN_SUCCESSFACTORS
*  &---------------------------------------------------------------------*
FORM zf_login_successfactors  USING    p_empresa
                              CHANGING c_sessionid
                                       c_batchsize.

  DATA: l_o_login TYPE REF TO zsfi_co_si_login_request,
        l_o_erro  TYPE REF TO cx_root,
        l_text    TYPE string.

  DATA: w_credenciais LIKE LINE OF t_credenciais,
        w_request     TYPE zsfi_mt_login_request,
        w_response    TYPE zsfi_mt_login_response.

*  / Lógica responsável por efetuar o Login no SuccessFactors

*  / Seleciona os dados de acesso da empresa
  READ TABLE t_credenciais INTO w_credenciais WITH KEY empresa = p_empresa BINARY SEARCH.
  IF sy-subrc NE 0.
    PERFORM zf_log USING space c_error text-009 p_empresa.
  ENDIF.
*  /

*  / Converte a senha para efetuar o Login
  PERFORM zf_decode_pass CHANGING w_credenciais-password.
*  /

  c_batchsize = w_credenciais-batchsize.

*  / Se não foi cadastrado um BatchSize para a empresa, assume o valor Default
  IF c_batchsize IS INITIAL.
    c_batchsize = '200'.
  ENDIF.
*  /

  w_request-mt_login_request-credential-company_id = w_credenciais-companyid.
  w_request-mt_login_request-credential-username   = w_credenciais-username.
  w_request-mt_login_request-credential-password   = w_credenciais-password.

  TRY.

      CREATE OBJECT l_o_login.

      CALL METHOD l_o_login->si_login_request
        EXPORTING
          output = w_request
        IMPORTING
          input  = w_response.

      IF w_response-mt_login_response-session_id IS INITIAL.
        PERFORM zf_log USING space c_error 'Erro ao Efetuar Login para a Empresa'(015) p_empresa.
      ELSE.
        c_sessionid = w_response-mt_login_response-session_id.
        PERFORM zf_log USING space c_success 'Login Efetuado com Sucesso para a Empresa'(016) p_empresa.
        PERFORM zf_log USING space c_success 'SessionID'(017) c_sessionid.
      ENDIF.

    CATCH cx_ai_system_fault INTO l_o_erro.
      PERFORM zf_log USING space c_error 'Erro ao Efetuar Login para a Empresa'(015) p_empresa.

      l_text = l_o_erro->get_text( ).
      PERFORM zf_log USING space c_error l_text space.

  ENDTRY.

*  /

ENDFORM.                    " ZF_LOGIN_SUCCESSFACTORS

*  &---------------------------------------------------------------------*
*  &      Form  ZF_LOG
*  &---------------------------------------------------------------------*
FORM zf_log  USING p_pernr
                   p_type
                   p_message1
                   p_message2.

  WRITE: / p_message1, p_message2.

ENDFORM.                    " ZF_LOG

*  &---------------------------------------------------------------------*
*  &      Form  ZF_DECODE_PASS
*  &---------------------------------------------------------------------*
FORM zf_decode_pass CHANGING c_password.

  DATA: l_obj_utility TYPE REF TO cl_http_utility,
        l_pass        TYPE string.

  CREATE OBJECT l_obj_utility.

  l_pass = c_password.

  CALL METHOD l_obj_utility->decode_base64
    EXPORTING
      encoded = l_pass
    RECEIVING
      decoded = l_pass.

  c_password = l_pass.

ENDFORM.                    " ZF_DECODE_PASS

*&---------------------------------------------------------------------*
*&      Form  ZF_ATUALIZA_SAP
*&---------------------------------------------------------------------*
FORM zf_atualiza_sap .

  DATA: l_lines TYPE i.

  IF NOT t_picklist IS INITIAL.

    DELETE FROM zhrst_sfsf_pickl.
    COMMIT WORK AND WAIT.

    INSERT zhrst_sfsf_pickl FROM TABLE t_picklist.
    COMMIT WORK AND WAIT.

    DESCRIBE TABLE t_picklist LINES l_lines.

    PERFORM zf_log USING '' c_success l_lines ' Picklists Atualizadas'.

  ENDIF.

ENDFORM.                    " ZF_ATUALIZA_SAP
