*  ======================================================================*
*                                                                        *
*                          HR Solutions Tecnologia                       *
*                                                                        *
*  ======================================================================*
*   Programa    : ZHRST_SFSF_INTERFACE                                   *
*   Tipo        : Interface Outbound                                     *
*   Módulo      : HCM                                                    *
*   Transação   : ZHRST_SFSF_INTERFACE                                   *
*   Descrição   : Programa para carregar os dados dos empregados para o  *
*                 SuccessFactors utilizando SFAPI (WebService)           *
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

  REPORT zhrst_sfsf_interface.

*  ----------------------------------------------------------------------*
*   Tabela Transparente                                                  *
*  ----------------------------------------------------------------------*
  TABLES: pernr.

*  ----------------------------------------------------------------------*
*   Infotipos                                                            *
*  ----------------------------------------------------------------------*
  INFOTYPES:  0000,
              0001,
              0002,
              0022,
              0004,
              0006,
              0009,
              0105,
              0008,
              0465,
              2001.

*  ----------------------------------------------------------------------*
*   Types                                                                *
*  ----------------------------------------------------------------------*
  TYPES: BEGIN OF y_tabelas,
          tabela_sf       TYPE string,
          tabela_interna  TYPE string,
         END OF y_tabelas,

         BEGIN OF y_treinamento,
          begda TYPE hrp1001-begda,
          endda TYPE hrp1001-endda,
          stext TYPE hrp1000-stext,
          objid TYPE hrp1001-objid,
         END OF y_treinamento,

         BEGIN OF y_pernr,
          pernr TYPE pernr-pernr,
         END OF y_pernr.

*  ----------------------------------------------------------------------*
*   Tabela Interna                                                       *
*  ----------------------------------------------------------------------*
  DATA: t_log                       TYPE TABLE OF zhrst_sfsf_log,
        t_parametros                TYPE TABLE OF zhrst_sfsf_param,
        t_credenciais               TYPE TABLE OF zhrst_sfsf_crede,
        t_picklist                  TYPE TABLE OF zhrst_sfsf_pickl,
        t_tabelas                   TYPE TABLE OF y_tabelas,
        t_pernr                     TYPE TABLE OF y_pernr.

*  ----------------------------------------------------------------------*
*   Work Área                                                            *
*  ----------------------------------------------------------------------*
  DATA: w_log                       LIKE LINE OF t_log,
        w_tabelas                   LIKE LINE OF t_tabelas.

*  ----------------------------------------------------------------------*
*   Constantes                                                           *
*  ----------------------------------------------------------------------*
  CONSTANTS:
       c_abap_true                  TYPE c VALUE 'X',
       c_error                      TYPE c VALUE 'E',
       c_warning                    TYPE c VALUE 'W',
       c_success                    TYPE c VALUE 'S',
       c_prefixo_badi(10)           TYPE c VALUE 'ZHRST_IF_',
       c_managertransfer            TYPE string VALUE 'managerTransfer',
       c_processcompl               TYPE string VALUE 'processCompletedDocsToManager',
       c_processinprogr             TYPE string VALUE 'processInProgressDocsToManager',
       c_routeenroutedoc            TYPE string VALUE 'routeEnRouteDoc',
       c_routecompleteddoc          TYPE string VALUE 'routeCompletedDoc',
       c_routeinboxdoc              TYPE string VALUE 'routeInboxDoc',
       c_true                       TYPE string VALUE 'true'.

*  ----------------------------------------------------------------------*
*   Variáveis                                                           *
*  ----------------------------------------------------------------------*
  DATA: v_idlog     TYPE zhrst_sfsf_log-idlog,
        v_seq       TYPE zhrst_sfsf_log-seq,
        v_elegivel  TYPE c.

*  ----------------------------------------------------------------------*
*   Field-Symbol Global                                                  *
*  ----------------------------------------------------------------------*
  FIELD-SYMBOLS: <f_t_user> TYPE table,
                 <f_t_01>   TYPE table,
                 <f_t_02>   TYPE table,
                 <f_t_03>   TYPE table,
                 <f_t_04>   TYPE table,
                 <f_t_05>   TYPE table,
                 <f_t_06>   TYPE table,
                 <f_t_07>   TYPE table,
                 <f_t_08>   TYPE table,
                 <f_t_09>   TYPE table,
                 <f_t_10>   TYPE table,
                 <f_t_11>   TYPE table,
                 <f_t_12>   TYPE table,
                 <f_t_13>   TYPE table,
                 <f_t_14>   TYPE table,
                 <f_t_15>   TYPE table,
                 <f_t_16>   TYPE table,
                 <f_t_17>   TYPE table,
                 <f_t_18>   TYPE table,
                 <f_t_19>   TYPE table,
                 <f_t_20>   TYPE table.

*  ----------------------------------------------------------------------*
*   Tela de Seleção                                                      *
*  ----------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-000.
*  PARAMETERS: p_delta AS CHECKBOX.
  PARAMETERS: p_full    RADIOBUTTON GROUP g1,
              p_user    RADIOBUTTON GROUP g1.
  PARAMETERS: p_rec     AS CHECKBOX,
              p_transf  AS CHECKBOX DEFAULT 'X'.
  SELECTION-SCREEN END OF BLOCK b1.

*  ----------------------------------------------------------------------*
*   Início da Execução
*  ----------------------------------------------------------------------*
  START-OF-SELECTION.

    pnp-sw-ignorelockedrecords = 'N'.
    rp_set_data_interval '0000' pn-begda pn-endda.
    rp_set_data_interval '0001' pn-begda pn-endda.
    rp_set_data_interval '0002' pn-begda pn-endda.
    rp_set_data_interval '0022' pn-begda pn-endda.
    rp_set_data_interval '0004' pn-begda pn-endda.
    rp_set_data_interval '0006' pn-begda pn-endda.
    rp_set_data_interval '0105' pn-begda pn-endda.
    rp_set_data_interval '0008' pn-begda pn-endda.
    rp_set_data_interval '0465' pn-begda pn-endda.
    rp_set_data_interval '2001' pn-begda pn-endda.

    PERFORM zf_log USING space c_success 'Início da Execução'(013) space.

*    IF NOT p_delta IS INITIAL.
*      PERFORM zf_get_delta.
*    ENDIF.

    PERFORM zf_seleciona_parametros.

    PERFORM zf_cria_tabelas_internas.

    CHECK t_parametros IS NOT INITIAL.

  GET pernr.

    PERFORM zf_mensagem_progresso USING 'Processando Empregado-'(024) pernr-pernr.

    PERFORM zf_check_elegivel CHANGING v_elegivel.

    IF NOT v_elegivel IS INITIAL.

      PERFORM zf_log_qtd_reg.

      PERFORM zf_get_historicos.

      PERFORM zf_processa_reg_empregado.

      IF p_transf EQ c_abap_true.
        PERFORM zf_processa_transferencia.
      ENDIF.

    ELSE.

      PERFORM zf_log USING pernr-pernr c_error 'Empregado não Elegível' space.

    ENDIF.

*  ----------------------------------------------------------------------*
*   Fim da Execução
*  ----------------------------------------------------------------------*
  END-OF-SELECTION.

    PERFORM zf_mensagem_progresso USING 'Efetuando UPSERT na Tabela USER' space.

    PERFORM zf_call_sfapi_user.

    PERFORM zf_call_sfapi_employeecentral.

*    IF p_full EQ c_abap_true.
*      PERFORM zf_call_sfapi_bkg.
*    ENDIF.

    PERFORM zf_log USING space c_success 'Fim da Execução'(014) space.

    PERFORM zf_gravar_log.

*  ======================================================================*
*                                                                        *
*                       Declarações de Rotinas                           *
*                                                                        *
*  ======================================================================*

*  &---------------------------------------------------------------------*
*  &      Form  zf_seleciona_parametros
*  &---------------------------------------------------------------------*
  FORM zf_seleciona_parametros.

*   Seleciona as parametrizações dos campos mapeados para processamento
*   através da interface de integração do SuccessFactors x SAP HCM
    SELECT *
      INTO TABLE t_parametros
      FROM zhrst_sfsf_param
     WHERE begda LE sy-datum
       AND endda GE sy-datum
       AND infty NE space
*      AND ( tabela_sf EQ 'PERNATIONALID'
*      OR tabela_sf EQ 'USER' )
     ORDER BY tabela_sf seqnr campo_sf.

    IF sy-subrc NE 0.

*  /  Erro ao selecionar parâmetros de mapeamento da integração
      PERFORM zf_log USING space c_error text-001 space.

    ELSE.

      IF p_user EQ c_abap_true.
        DELETE t_parametros WHERE tabela_sf NE 'USER'.
      ENDIF.

    ENDIF.

*  / Seleciona os dados de Picklist para conversão
    SELECT *
      INTO TABLE t_picklist
      FROM zhrst_sfsf_pickl
     ORDER BY empresa picklistid externalcode.

    IF sy-subrc NE 0.

*  /  Nenhuma Picklist cadastrada
      PERFORM zf_log USING space c_error text-004 space.

    ENDIF.
*  /

*  / Seleciona os dados de Credenciais para Acesso ao SuccessFactors
    SELECT *
      INTO TABLE t_credenciais
      FROM zhrst_sfsf_crede.

    IF sy-subrc NE 0.

*  /  Nenhuma Credencial Cadastrada
      PERFORM zf_log USING space c_error text-010 space.

    ENDIF.
*  /

    IF p_full EQ c_abap_true.

      SELECT DISTINCT a~pernr
         INTO TABLE t_pernr
         FROM pa0001 AS a
        INNER JOIN pa0000 AS b ON a~pernr EQ b~pernr
        WHERE a~pernr IN pnppernr
          AND a~bukrs IN pnpbukrs
          AND b~stat2 IN pnpstat2.

    ENDIF.

  ENDFORM.                    "zf_seleciona_parametros

*  &---------------------------------------------------------------------*
*  &      Form  ZF_PROCESSA_REG_EMPREGADO
*  &---------------------------------------------------------------------*
  FORM zf_processa_reg_empregado .

    DATA: t_header_param    LIKE t_parametros.

    DATA: w_header_param    LIKE LINE OF t_parametros,
          w_parametro       LIKE LINE OF t_parametros.

    DATA: l_nome_objeto(40) TYPE c,
          l_nome_infty      TYPE infty,
          l_empresa         TYPE char10,
          l_operacao        TYPE char10,
          l_id              TYPE char20,
          l_infty           TYPE string,
          l_tabix_tabela_sf TYPE sy-tabix.

    FIELD-SYMBOLS: <f_tabela_sf>    TYPE table,
                   <f_tabela_sap>   TYPE table,
                   <f_workarea_sap> TYPE ANY,
                   <f_workarea_sf>  TYPE ANY,
                   <f_campo_sf>     TYPE ANY,
                   <f_campo_sap>    TYPE ANY,
                   <f_infty>        TYPE ANY.

    PERFORM zf_define_empresa CHANGING l_empresa.

    t_header_param[] = t_parametros[].
    DELETE t_header_param WHERE empresa NE l_empresa.
    DELETE ADJACENT DUPLICATES FROM t_header_param COMPARING tabela_sf seqnr.

    LOOP AT t_header_param INTO w_header_param WHERE empresa EQ l_empresa.

*  /  Associa a tabela de IT dinamicamente
      IF w_header_param-tabela_sf EQ 'USER'.
        l_nome_objeto = 'P0000[]'.
      ELSEIF NOT w_header_param-infty CO '0123456789'.
        CONCATENATE 'T_' w_header_param-infty '[]' INTO l_nome_objeto.
      ELSE.
        CONCATENATE 'P' w_header_param-infty '[]' INTO l_nome_objeto.
      ENDIF.
      ASSIGN (l_nome_objeto) TO <f_tabela_sap>.
      IF sy-subrc NE 0. PERFORM zf_log USING pernr-pernr c_error 'Erro ao Associar Infotipo'(007) l_nome_objeto. ENDIF.
*  /

*  /  Ordena a tabela de forma descendente para que o primeiro registro seja o mais novo
      IF l_nome_objeto(1) EQ 'P'.
        SORT <f_tabela_sap> DESCENDING.
      ENDIF.

      LOOP AT <f_tabela_sap> ASSIGNING <f_workarea_sap>.

        CLEAR l_operacao.

*  /    Associa dinamicamente a estrutura da tabela para enviar ao SuccessFactors
        CLEAR w_tabelas.
        READ TABLE t_tabelas INTO w_tabelas WITH KEY tabela_sf = w_header_param-tabela_sf.
        l_nome_objeto = w_tabelas-tabela_interna.
        ASSIGN (l_nome_objeto) TO <f_tabela_sf>.
        IF sy-subrc NE 0. PERFORM zf_log USING space c_error 'Erro ao Associar Tabela Interna'(005) l_nome_objeto. ENDIF.
*  /

*  /    Associa dinamicamente a WorkArea para gravação dos dados para enviar ao SuccessFactors.
        CLEAR l_tabix_tabela_sf.
        APPEND INITIAL LINE TO <f_tabela_sf> ASSIGNING <f_workarea_sf>.
        l_tabix_tabela_sf = sy-tabix.
        IF sy-subrc NE 0. PERFORM zf_log USING space c_error 'Erro ao Associar Work Area'(006) l_nome_objeto. ENDIF.
*  /

        LOOP AT t_parametros INTO w_parametro WHERE tabela_sf EQ w_header_param-tabela_sf AND
                                                    empresa   EQ l_empresa AND
                                                    seqnr     EQ w_header_param-seqnr.

          UNASSIGN: <f_campo_sf>, <f_campo_sap>, <f_infty>.
          CLEAR: l_infty.

          ASSIGN COMPONENT w_parametro-campo_sf  OF STRUCTURE <f_workarea_sf>  TO <f_campo_sf>.
          IF sy-subrc NE 0. PERFORM zf_log USING space c_error 'Erro ao Associar campo '(008) w_parametro-campo_sf. ENDIF.

          IF w_parametro-infty CO '0123456789'.

            CONCATENATE 'P' w_parametro-infty INTO l_infty.
            ASSIGN (l_infty) TO <f_infty>.
            IF sy-subrc NE 0. PERFORM zf_log USING pernr-pernr c_error 'Erro ao Associar Infotipo '(007) l_infty. ENDIF.

            IF <f_infty> IS ASSIGNED.
              PERFORM zf_reg_infty USING w_parametro <f_workarea_sap> w_header_param-historico CHANGING <f_infty>.
              IF <f_infty> IS INITIAL. PERFORM zf_log USING pernr-pernr c_error 'Erro ao Associar Infotipo '(007) l_infty. ENDIF.

              ASSIGN COMPONENT w_parametro-campo_sap OF STRUCTURE <f_infty> TO <f_campo_sap>.
              IF sy-subrc NE 0. PERFORM zf_log USING space c_error 'Erro ao Associar campo '(008) w_parametro-campo_sap. ENDIF.
            ENDIF.

          ELSE.

            ASSIGN COMPONENT w_parametro-campo_sap OF STRUCTURE <f_workarea_sap> TO <f_campo_sap>.

          ENDIF.
          IF <f_campo_sap> IS ASSIGNED AND <f_campo_sf> IS ASSIGNED.

*  /        Preenche o campo que será enviado para o SuccessFactors
            <f_campo_sf> = <f_campo_sap>.
*  /

*  /        Se houver tratamento via BADi
            IF NOT w_parametro-tratamento IS INITIAL.
              PERFORM zf_call_badi USING w_parametro pernr-bukrs CHANGING <f_campo_sf>.
              IF <f_campo_sf> EQ '[[SKIP]]'.
                EXIT.
              ENDIF.
            ENDIF.
*  /

*  /        Faz a conversão da Picklist, caso o campo tenha associação com uma
            IF NOT w_parametro-picklist IS INITIAL.
              PERFORM zf_convert_picklist USING w_parametro CHANGING <f_campo_sf>.
            ENDIF.
*  /

*  /        Se o campo for do tipo data, formata segundo a tabela de parametrização
            IF w_parametro-tipo_campo EQ '1'.
              PERFORM zf_formata_data CHANGING <f_campo_sf>.
            ENDIF.
*  /

*  /        Se o campo for do tipo UserID, formata segundo a tabela de parametrização
            IF w_parametro-tipo_campo EQ '2'.
              PERFORM zf_formata_userid USING l_empresa CHANGING <f_campo_sf>.
            ENDIF.
*  /

*  /        Se o campo for do tipo número, retira pontos ou traços deixando apenas os números
            IF w_parametro-tipo_campo EQ '3'.
              PERFORM zf_formata_numero CHANGING <f_campo_sf>.
            ENDIF.
*  /

          ENDIF.
        ENDLOOP.

*  /    Verifica para as tabelas Backgrounds qual operação deve ser feita, INSERT ou UPDATE
        IF NOT w_header_param-historico IS INITIAL AND <f_workarea_sf> IS ASSIGNED.
          IF p_rec EQ c_abap_true.
            l_operacao = 'I'.
          ELSE.
            PERFORM zf_verifica_operacao USING w_header_param <f_workarea_sf> CHANGING l_operacao l_id.
          ENDIF.
        ENDIF.
*  /

        IF <f_workarea_sf> IS ASSIGNED AND <f_tabela_sf> IS ASSIGNED.

*  /      Preenche o campo empresa para diferenciar as rotas de comunicação do WebService (SFAPI)
          ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE <f_workarea_sf> TO <f_campo_sf>.
          <f_campo_sf> = l_empresa.
*  /

*  /      Preenche o campo operação para diferenciar o método que será invocado da SFAPI
          ASSIGN COMPONENT 'OPERACAO' OF STRUCTURE <f_workarea_sf> TO <f_campo_sf>.
          <f_campo_sf> = l_operacao.
*  /

*  /      Preenche o campo operação para diferenciar o método que será invocado da SFAPI
          ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_workarea_sf> TO <f_campo_sf>.
          <f_campo_sf> = w_header_param-seqnr.
*  /

          IF l_operacao EQ 'U'.
*  /      Preenche o campo operação para diferenciar o método que será invocado da SFAPI
            ASSIGN COMPONENT 'ID' OF STRUCTURE <f_workarea_sf> TO <f_campo_sf>.
            <f_campo_sf> = l_id.
*  /
          ENDIF.

          IF l_operacao EQ 'S'.

            DELETE <f_tabela_sf> INDEX l_tabix_tabela_sf.

          ENDIF.

        ENDIF.

*  /    Se não for uma tabela de histórico (Background) então registra apenas o primeiro registro
        IF w_header_param-historico IS INITIAL.
          EXIT.
        ENDIF.
*  /

      ENDLOOP.

    ENDLOOP.

  ENDFORM.                    " ZF_PROCESSA_REG_EMPREGADO

*  &---------------------------------------------------------------------*
*  &      Form  ZF_LOG
*  &---------------------------------------------------------------------*
  FORM zf_log  USING p_pernr
                     p_type
                     p_message1
                     p_message2.

    IF t_log[] IS INITIAL.

*     Seleciona o número do ID do LOG
      PERFORM zf_gera_id_log USING '01' 'ZHRST_LOG'
                             CHANGING v_idlog.
    ENDIF.

    ADD 1 TO v_seq.

    w_log-idlog   = v_idlog.
    w_log-pernr   = p_pernr.
    w_log-type    = p_type.
    CONCATENATE p_message1 p_message2 INTO w_log-message SEPARATED BY space.
    w_log-uname   = sy-uname.

    GET TIME.
    w_log-data    = sy-datum.
    w_log-hora    = sy-uzeit.

    w_log-seq     = v_seq.

    APPEND w_log TO t_log.
    CLEAR w_log.

  ENDFORM.                    " ZF_LOG

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CALL_BADI
*  &---------------------------------------------------------------------*
  FORM zf_call_badi USING p_parametros  LIKE LINE OF t_parametros
                          p_empresa_sap
                 CHANGING c_campo_sf.

    FIELD-SYMBOLS: <f_tabela_infty>    TYPE ANY TABLE.

    DATA: l_method       TYPE string,
          l_infty        TYPE string,
          l_result_badi  TYPE string,
          l_obj_badi     TYPE REF TO zhrst_cl_badi_sap_to_sfsf.

    DATA: l_o_ex_root TYPE REF TO cx_root,
          l_message  TYPE string.

    IF p_parametros-infty CO '0123456789'.
      CONCATENATE 'P' p_parametros-infty '[]' INTO l_infty.
      ASSIGN (l_infty) TO <f_tabela_infty>.
      IF sy-subrc NE 0. PERFORM zf_log USING pernr-pernr c_error 'Erro ao Associar Infotipo '(007) p_parametros-infty. ENDIF.
    ELSE.
      CONCATENATE 'T_' p_parametros-infty '[]' INTO l_infty.
      ASSIGN (l_infty) TO <f_tabela_infty>.
      IF sy-subrc NE 0. PERFORM zf_log USING pernr-pernr c_error 'Erro ao Associar Tabela '(022) l_infty. ENDIF.
    ENDIF.

    CHECK <f_tabela_infty> IS ASSIGNED.

    CREATE OBJECT l_obj_badi.
    CONCATENATE c_prefixo_badi p_parametros-tabela_sf '~' p_parametros-campo_sf INTO l_method.

    TRY.

        CALL METHOD l_obj_badi->(l_method)
          EXPORTING
            i_pernr       = pernr
            i_empresa     = p_parametros-empresa
            i_empresa_sap = p_empresa_sap
            i_molga       = p_parametros-molga
            i_infty       = p_parametros-infty
            i_subty       = p_parametros-subty
            i_value       = c_campo_sf
            it_infotipo   = <f_tabela_infty>
          CHANGING
            c_value       = c_campo_sf.

      CATCH cx_root INTO l_o_ex_root.
        PERFORM zf_log USING pernr-pernr c_warning text-002 p_parametros-campo_sf.

        l_message = l_o_ex_root->get_text( ).
        PERFORM zf_log USING pernr-pernr c_warning l_message p_parametros-campo_sf.

    ENDTRY.

  ENDFORM.                    " ZF_CALL_BADI

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CONVERT_PICKLIST
*  &---------------------------------------------------------------------*
  FORM zf_convert_picklist  USING    p_parametro LIKE LINE OF t_parametros
                            CHANGING c_campo_sf.

    DATA: w_picklist LIKE LINE OF t_picklist.

    READ TABLE t_picklist INTO w_picklist WITH KEY empresa      = p_parametro-empresa
                                                   picklistid   = p_parametro-picklist
                                                   externalcode = c_campo_sf
                                            BINARY SEARCH.

    IF sy-subrc EQ 0.

      c_campo_sf = w_picklist-id.

    ELSE.

      PERFORM zf_log USING pernr-pernr c_error text-003 p_parametro-campo_sf.

    ENDIF.

  ENDFORM.                    " ZF_CONVERT_PICKLIST

*  &---------------------------------------------------------------------*
*  &      Form  ZF_DEFINE_EMPRESA
*  &---------------------------------------------------------------------*
  FORM zf_define_empresa  CHANGING c_empresa.

    DATA: t_p0001 TYPE TABLE OF p0001,
          w_p0001 TYPE p0001.

*  / Lógica para definir a qual empresa (Instância SuccessFactors) o
*    empregado está associado.
    t_p0001[] = p0001[].
    LOOP AT t_p0001 INTO w_p0001 WHERE begda LE sy-datum
                                   AND endda GE sy-datum.
      EXIT.
    ENDLOOP.

    SELECT SINGLE empresa_sf
      INTO c_empresa
      FROM zhrst_sfsf_empre
     WHERE empresa_sap  EQ w_p0001-bukrs
       AND begda        LE sy-datum
       AND endda        GE sy-datum.

    IF sy-subrc NE 0.

      SELECT SINGLE empresa_sf
        INTO c_empresa
        FROM zhrst_sfsf_empre
       WHERE empresa_sap  EQ '*'
         AND begda        LE sy-datum
         AND endda        GE sy-datum.

      IF sy-subrc NE 0.

        PERFORM zf_log USING pernr-pernr c_error 'Nenhuma Empresa SF cadastrada para a Empresa SAP'(012) p0001-bukrs.

      ENDIF.

    ENDIF.
*  /

  ENDFORM.                    " ZF_DEFINE_EMPRESA

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CALL_SFAPI_USER
*  &---------------------------------------------------------------------*
  FORM zf_call_sfapi_user .

    DATA: t_request_data  TYPE zsfi_dt_operation_request_tab2,
          t_sfobject      TYPE zsfi_dt_operation_request__tab.

    DATA: w_parametro     LIKE LINE OF t_parametros,
          w_request_data  LIKE LINE OF t_request_data,
          w_sfobject      LIKE LINE OF t_sfobject,
          w_credenciais   LIKE LINE OF t_credenciais.

    DATA: l_sessionid   TYPE string,
          l_batchsize   TYPE string,
          l_count_reg   TYPE i.

    FIELD-SYMBOLS: <f_field>    TYPE ANY,
                   <f_w_user>   TYPE ANY,
                   <f_empresa>  TYPE ANY.

    LOOP AT t_credenciais INTO w_credenciais.

*  / Monta a Estrutura do XML para a comunicação com o SuccessFactors
      LOOP AT <f_t_user> ASSIGNING <f_w_user>.

        ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE <f_w_user> TO <f_empresa>.
        CHECK <f_empresa> EQ w_credenciais-empresa.

        LOOP AT t_parametros INTO w_parametro WHERE empresa   EQ w_credenciais-empresa
                                                AND tabela_sf EQ 'USER'.

          w_request_data-key   = w_parametro-campo_sf.
          ASSIGN COMPONENT w_parametro-campo_sf OF STRUCTURE <f_w_user> TO <f_field>.
          IF <f_field> IS ASSIGNED.
            w_request_data-value = <f_field>.
            APPEND w_request_data TO t_request_data.
            CLEAR w_request_data.
            UNASSIGN <f_field>.
          ENDIF.

        ENDLOOP.

        w_sfobject-entity = 'USER'.
        w_sfobject-data[] = t_request_data[].
        APPEND w_sfobject TO t_sfobject.

        UNASSIGN: <f_empresa>.
        CLEAR: w_request_data,
               t_request_data[].

      ENDLOOP.
*  /

      IF NOT t_sfobject[] IS INITIAL.
*  / Efetua a operação UPSERT com o XML criado no item anterior
        PERFORM zf_call_upsert_user USING w_credenciais
                                     t_sfobject.
        CLEAR t_sfobject[].
*  /
      ENDIF.

    ENDLOOP.

  ENDFORM.                    " ZF_CALL_SFAPI_USER

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

    CREATE OBJECT l_o_login.

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
*  &      Form  zf_logout_successfactors
*  &---------------------------------------------------------------------*
  FORM zf_logout_successfactors CHANGING c_sessionid.

*  / Lógica responsável por efeutar o Logout no SuccessFactors

    CLEAR: c_sessionid.
*  /

  ENDFORM.                    "zf_logout_successfactors
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

*  &---------------------------------------------------------------------*
*  &      Form  ZF_VERIFICA_OPERACAO
*  &---------------------------------------------------------------------*
  FORM zf_verifica_operacao USING p_parametro   LIKE LINE OF t_parametros
                                  p_workarea_sf
                         CHANGING c_operacao
                                  c_id.

    DATA: t_fields TYPE TABLE OF string.

    DATA: w_fields TYPE string.

    DATA: l_tabela_hist TYPE string,
          l_query       TYPE string,
          l_tabela      TYPE REF TO data.

    FIELD-SYMBOLS: <f_tabela_hist>    TYPE table,
                   <f_workarea_hist>  TYPE ANY,
                   <f_field>          TYPE ANY,
                   <f_field_sap>      TYPE ANY.

    CLEAR: c_operacao,
           c_id.

    ASSIGN COMPONENT 'MOTIVO' OF STRUCTURE p_workarea_sf TO <f_field>.
    IF <f_field> IS ASSIGNED.
      IF <f_field> EQ '[[SKIP]]'.
        c_operacao = 'S'.
      ELSE.
        UNASSIGN <f_field>.
      ENDIF.
    ENDIF.

    CHECK c_operacao NE 'S'.

*   Define a tabela transparente que possui o histórico
    PERFORM zf_get_tabela_historico USING p_parametro CHANGING l_tabela_hist.

    PERFORM zf_monta_query TABLES t_fields USING p_workarea_sf l_tabela_hist CHANGING l_query.

    IF NOT l_query IS INITIAL.

      CREATE DATA l_tabela TYPE TABLE OF (l_tabela_hist).
      ASSIGN l_tabela->* TO <f_tabela_hist>.

      SELECT *
        INTO TABLE <f_tabela_hist>
        FROM (l_tabela_hist)
       WHERE (l_query).

      IF sy-subrc EQ 0.
        READ TABLE <f_tabela_hist> ASSIGNING <f_workarea_hist> INDEX 1.
*/ Controle do DELTA
        LOOP AT t_fields INTO w_fields.

          ASSIGN COMPONENT w_fields OF STRUCTURE <f_workarea_hist> TO <f_field_sap>.
          ASSIGN COMPONENT w_fields OF STRUCTURE p_workarea_sf     TO <f_field>.

          IF <f_field> IS ASSIGNED AND <f_field_sap> IS ASSIGNED.

            IF <f_field> NE <f_field_sap>.
              c_operacao = 'U'.
              EXIT.
            ENDIF.

          ENDIF.

          UNASSIGN: <f_field>,
                    <f_field_sap>.

        ENDLOOP.
*/

        IF c_operacao EQ 'U'.
          ASSIGN COMPONENT 'ID' OF STRUCTURE <f_workarea_hist> TO <f_field>.
          c_id       = <f_field>.
        ELSE.
          c_operacao = 'S'.
        ENDIF.

      ELSE.
        c_operacao = 'I'.
      ENDIF.

    ENDIF.

    PERFORM zf_log USING pernr-pernr c_warning l_query space.
    PERFORM zf_log USING pernr-pernr c_warning 'C_OPERACAO' c_operacao.

  ENDFORM.                    " ZF_VERIFICA_OPERACAO

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GET_TABELA_HISTORICO
*  &---------------------------------------------------------------------*
  FORM zf_get_tabela_historico  USING    p_parametro LIKE LINE OF t_parametros
                                CHANGING c_tabela_hist.

*  / Verifica qual a tabela transparente responsável por guardar o histórico
    SELECT SINGLE tabela_sap
      INTO c_tabela_hist
      FROM zhrst_sfsf_bkg
     WHERE alias_sf EQ p_parametro-tabela_sf.

    IF sy-subrc NE 0.
      PERFORM zf_log USING space c_error 'Sem tabela SAP de histórico para a tabela' p_parametro-tabela_sf.
    ENDIF.

  ENDFORM.                    " ZF_GET_TABELA_HISTORICO

*  &---------------------------------------------------------------------*
*  &      Form  ZF_MONTA_QUERY
*  &---------------------------------------------------------------------*
  FORM zf_monta_query TABLES t_fields
                       USING p_workarea_sf
                             p_tabela_hist
                    CHANGING c_query.

    DATA: t_dd03l TYPE TABLE OF dd03l.

    DATA: w_dd03l TYPE dd03l.

    DATA: l_query     TYPE string,
          l_campo_sf  TYPE string.

    FIELD-SYMBOLS: <f_campo_sf> TYPE ANY.

    SELECT *
      INTO TABLE t_dd03l
      FROM dd03l
     WHERE tabname   EQ p_tabela_hist
       AND fieldname NE 'MANDT'.
*       AND keyflag   EQ 'X'.

    IF sy-subrc EQ 0.

      LOOP AT t_dd03l INTO w_dd03l.

        IF w_dd03l-keyflag EQ abap_true.

          ASSIGN COMPONENT w_dd03l-fieldname OF STRUCTURE p_workarea_sf TO <f_campo_sf>.

          l_campo_sf = <f_campo_sf>.

          PERFORM zf_retira_aspa_simples CHANGING l_campo_sf.

          CONCATENATE l_query ' AND ' w_dd03l-fieldname ' EQ ' text-011 l_campo_sf text-011 INTO l_query RESPECTING BLANKS.

        ENDIF.

        IF w_dd03l-fieldname NE 'ID'.
*/ Controle do DELTA
          APPEND w_dd03l-fieldname TO t_fields.
*/
        ENDIF.

      ENDLOOP.

      c_query = l_query+4.

    ENDIF.

  ENDFORM.                    " ZF_MONTA_QUERY

*  &---------------------------------------------------------------------*
*  &      Form  zf_formata_data
*  &---------------------------------------------------------------------*
  FORM zf_formata_data CHANGING c_data.

    DATA: l_data TYPE string.
*  / Formata a data para o padrão da API do SuccessFactors.
    FIND '/' IN c_data.
    IF sy-subrc EQ 0.
      CONCATENATE c_data+6(4) c_data+3(2) c_data(2) INTO l_data SEPARATED BY '-'.
    ELSEIF c_data EQ '00000000'.
      CLEAR c_data.
    ELSEIF NOT c_data IS INITIAL.
      CONCATENATE c_data(4) c_data+4(2) c_data+6(2) INTO l_data SEPARATED BY '-'.
    ENDIF.
    c_data = l_data.
*  /

  ENDFORM.                    "zf_formata_data

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CALL_SFAPI_BKG
*  &---------------------------------------------------------------------*
  FORM zf_call_sfapi_bkg .

*    DATA: t_request_data  TYPE zsfi_dt_operation_request_tab2,
*          t_sfobject      TYPE zsfi_dt_operation_request__tab.
*
*    DATA: w_parametro     LIKE LINE OF t_parametros,
*          w_request_data  LIKE LINE OF t_request_data,
*          w_sfobject      LIKE LINE OF t_sfobject,
*          w_credenciais   LIKE LINE OF t_credenciais.
*
*    DATA: l_sessionid   TYPE string,
*          l_batchsize   TYPE string,
*          l_count_reg   TYPE i,
*          l_operacao    TYPE string,
*          l_tabela_sf   TYPE string,
*          l_skip        TYPE c.
*
*    FIELD-SYMBOLS: <f_field>    TYPE any,
*                   <f_empresa>  TYPE any,
*                   <f_operacao> TYPE any,
*                   <f_t_bkg>    TYPE table,
*                   <f_w_bkg>    TYPE any.
*
*    DELETE t_tabelas WHERE tabela_sf EQ 'USER'.
*
*    LOOP AT t_credenciais INTO w_credenciais.
*
*      LOOP AT t_tabelas INTO w_tabelas.
*
*        ASSIGN (w_tabelas-tabela_interna) TO <f_t_bkg>.
*
*        DO 2 TIMES.
*
*          IF sy-index EQ 1.
*            l_operacao = 'I'.
*          ELSE.
*            l_operacao = 'U'.
*          ENDIF.
*
**  / Monta a Estrutura do XML para a comunicação com o SuccessFactors
*          LOOP AT <f_t_bkg> ASSIGNING <f_w_bkg>.
*
*            ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE <f_w_bkg> TO <f_empresa>.
*            CHECK <f_empresa> EQ w_credenciais-empresa.
*
*            ASSIGN COMPONENT 'OPERACAO' OF STRUCTURE <f_w_bkg> TO <f_operacao>.
*            CHECK <f_operacao> EQ l_operacao.
*
*            CLEAR l_tabela_sf.
*            SELECT SINGLE tabela_sf
*              INTO l_tabela_sf
*              FROM zhrst_sfsf_bkg
*             WHERE alias_sf EQ w_tabelas-tabela_sf.
*
*            IF l_operacao EQ 'U'.
*              w_request_data-key   = 'ID'.
*              ASSIGN COMPONENT 'ID' OF STRUCTURE <f_w_bkg> TO <f_field>.
*              w_sfobject-id = <f_field>.
*              UNASSIGN <f_field>.
*            ENDIF.
*
*            LOOP AT t_parametros INTO w_parametro WHERE empresa   EQ w_credenciais-empresa
*                                                    AND tabela_sf EQ w_tabelas-tabela_sf.
*
*              IF l_operacao EQ 'U' AND w_parametro-campo_sf EQ 'USERID'.
*                CONTINUE.
*              ENDIF.
*
*              w_request_data-key   = w_parametro-campo_sf.
*              ASSIGN COMPONENT w_parametro-campo_sf OF STRUCTURE <f_w_bkg> TO <f_field>.
*
*              IF w_parametro-campo_sf EQ 'USERID' AND <f_field>(4) NE 'USR-'.
*                PERFORM zf_formata_userid USING w_credenciais-empresa CHANGING <f_field>.
*                IF <f_field> CO '1234567890'.
*                  l_skip = abap_true.
*                ENDIF.
*
*              ENDIF.
*
*              IF <f_field> IS ASSIGNED.
*                w_request_data-value = <f_field>.
*                APPEND w_request_data TO t_request_data.
*                CLEAR w_request_data.
*
*                UNASSIGN <f_field>.
*              ENDIF.
*
*            ENDLOOP.
*
*            IF l_skip EQ abap_false.
*              w_sfobject-entity = l_tabela_sf.
*              w_sfobject-data[] = t_request_data[].
*              APPEND w_sfobject TO t_sfobject.
*            ENDIF.
*
*            CLEAR w_sfobject.
*
*            UNASSIGN: <f_empresa>,
*                      <f_operacao>.
*
*            CLEAR: w_request_data,
*                   t_request_data[],
*                   l_skip.
*
*          ENDLOOP.
**  /
*
*          IF l_operacao EQ 'I'.
*            IF NOT t_sfobject[] IS INITIAL.
**  / Efetua a operação INSERT com o XML criado no item anterior
*              PERFORM zf_mensagem_progresso USING 'Efetuando INSERT na Tabela-' l_tabela_sf.
*              PERFORM zf_call_insert USING l_tabela_sf
*                                           w_credenciais
*                                           t_sfobject.
*            ENDIF.
**  /
*          ELSE.
*            IF NOT t_sfobject[] IS INITIAL.
**  / Efetua a operação UPDATE com o XML criado no item anterior
*              PERFORM zf_mensagem_progresso USING 'Efetuando UPDATE na Tabela-' l_tabela_sf.
*              PERFORM zf_call_update USING l_tabela_sf
*                                           w_credenciais
*                                           t_sfobject.
*            ENDIF.
**  /
*          ENDIF.
*
*          CLEAR t_sfobject[].
*
*        ENDDO.
*
*      ENDLOOP.
*
*    ENDLOOP.
*
  ENDFORM.                    " ZF_CALL_SFAPI_BKG

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GET_DELTA
*  &---------------------------------------------------------------------*
  FORM zf_get_delta .



  ENDFORM.                    " ZF_GET_DELTA

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CRIA_TABELAS_INTERNAS
*  &---------------------------------------------------------------------*
  FORM zf_cria_tabelas_internas .

    DATA: w_dyn_fcat        TYPE lvc_s_fcat,
          t_dyn_fcat        TYPE lvc_t_fcat,
          t_param_loc       LIKE t_parametros,
          w_param_loc       LIKE LINE OF t_parametros,
          w_parametro       LIKE LINE OF t_parametros,
          l_o_new_type      TYPE REF TO cl_abap_structdescr,
          l_o_new_tab       TYPE REF TO cl_abap_tabledescr,
          t_comp            TYPE cl_abap_structdescr=>component_table,
          w_comp            LIKE LINE OF t_comp,
          t_table           TYPE REF TO data,
          l_table           TYPE string,
          l_count(2)        TYPE n.

    FIELD-SYMBOLS: <f_table> TYPE table.

    t_param_loc[] = t_parametros[].
    SORT t_param_loc BY tabela_sf.
    DELETE ADJACENT DUPLICATES FROM t_param_loc COMPARING tabela_sf.

    LOOP AT t_param_loc INTO w_param_loc.

      CLEAR: l_o_new_type,
             l_o_new_tab,
             t_comp.

      w_comp-name = 'EMPRESA'.
      w_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND w_comp TO t_comp.
      CLEAR: w_comp.

      w_comp-name = 'OPERACAO'.
      w_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND w_comp TO t_comp.
      CLEAR: w_comp.

      w_comp-name = 'ID'.
      w_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND w_comp TO t_comp.
      CLEAR: w_comp.

      w_comp-name = 'SEQNR'.
      w_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND w_comp TO t_comp.
      CLEAR: w_comp.

      LOOP AT t_parametros INTO w_parametro WHERE tabela_sf EQ w_param_loc-tabela_sf.

        w_comp-name = w_parametro-campo_sf.
        w_comp-type = cl_abap_elemdescr=>get_string( ).
        APPEND w_comp TO t_comp.
        CLEAR: w_comp.

      ENDLOOP.

      SORT t_comp BY name.
      DELETE ADJACENT DUPLICATES FROM t_comp COMPARING name.

      l_o_new_type = cl_abap_structdescr=>create( t_comp ).
      l_o_new_tab = cl_abap_tabledescr=>create(
                      p_line_type  = l_o_new_type
                      p_table_kind = cl_abap_tabledescr=>tablekind_std
                      p_unique     = abap_false ).

      CREATE DATA t_table TYPE HANDLE l_o_new_tab.

      IF w_param_loc-tabela_sf EQ 'USER'.

        ASSIGN t_table->* TO <f_t_user>.
        w_tabelas-tabela_sf       = w_param_loc-tabela_sf.
        w_tabelas-tabela_interna  = '<F_T_USER>'.
        APPEND w_tabelas TO t_tabelas.

      ELSE.

        ADD 1 TO l_count.

        CASE l_count.
          WHEN '01'. ASSIGN t_table->* TO <f_t_01>.
          WHEN '02'. ASSIGN t_table->* TO <f_t_02>.
          WHEN '03'. ASSIGN t_table->* TO <f_t_03>.
          WHEN '04'. ASSIGN t_table->* TO <f_t_04>.
          WHEN '05'. ASSIGN t_table->* TO <f_t_05>.
          WHEN '06'. ASSIGN t_table->* TO <f_t_06>.
          WHEN '07'. ASSIGN t_table->* TO <f_t_07>.
          WHEN '08'. ASSIGN t_table->* TO <f_t_08>.
          WHEN '09'. ASSIGN t_table->* TO <f_t_09>.
          WHEN '10'. ASSIGN t_table->* TO <f_t_10>.
          WHEN '11'. ASSIGN t_table->* TO <f_t_11>.
          WHEN '12'. ASSIGN t_table->* TO <f_t_12>.
          WHEN '13'. ASSIGN t_table->* TO <f_t_13>.
          WHEN '14'. ASSIGN t_table->* TO <f_t_14>.
          WHEN '15'. ASSIGN t_table->* TO <f_t_15>.
          WHEN '16'. ASSIGN t_table->* TO <f_t_16>.
          WHEN '17'. ASSIGN t_table->* TO <f_t_17>.
          WHEN '18'. ASSIGN t_table->* TO <f_t_18>.
          WHEN '19'. ASSIGN t_table->* TO <f_t_19>.
          WHEN '20'. ASSIGN t_table->* TO <f_t_20>.
          WHEN OTHERS.
        ENDCASE.

        CONCATENATE '<F_T_' l_count '>' INTO l_table.
        w_tabelas-tabela_sf       = w_param_loc-tabela_sf.
        w_tabelas-tabela_interna  = l_table.
        APPEND w_tabelas TO t_tabelas.

      ENDIF.

    ENDLOOP.

  ENDFORM.                    " ZF_CRIA_TABELAS_INTERNAS

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GRAVAR_LOG
*  &---------------------------------------------------------------------*
  FORM zf_gravar_log .

    DATA: t_param TYPE TABLE OF rsparams,
          w_param TYPE rsparams.

    MODIFY zhrst_sfsf_log FROM TABLE t_log.
    COMMIT WORK AND WAIT.

    IF sy-batch IS INITIAL.

      w_param-kind    = 'S'.
      w_param-option  = 'EQ'.
      w_param-selname = 'S_IDLOG'.
      w_param-sign    = 'I'.
      w_param-low     = v_idlog.
      APPEND w_param TO t_param.
      CLEAR w_param.

      SUBMIT zhrst_sfsf_log WITH SELECTION-TABLE t_param.

    ELSE.

      READ TABLE t_log INTO w_log INDEX 1.

      WRITE: /'Lote de processamento gerado de número: ',
              w_log-idlog.

    ENDIF.

  ENDFORM.                    " ZF_GRAVAR_LOG

*  &---------------------------------------------------------------------*
*  &      Form  ZF_REG_INFTY
*  &---------------------------------------------------------------------*
  FORM zf_reg_infty  USING    p_parametro LIKE LINE OF t_parametros
                              p_workarea_sap
                              p_hist
                     CHANGING p_infty.

    DATA: l_infty  TYPE string,
          w_pskey TYPE pskey.

    FIELD-SYMBOLS: <f_t_infty>     TYPE table,
                   <f_w_infty>     TYPE ANY,
                   <f_begda_ref>   TYPE ANY.
    CLEAR: p_infty.

    ASSIGN COMPONENT 'BEGDA' OF STRUCTURE p_workarea_sap TO <f_begda_ref>.

    IF p_hist IS INITIAL.
      <f_begda_ref> = sy-datum.
    ENDIF.

    CONCATENATE 'P' p_parametro-infty '[]' INTO l_infty.
    ASSIGN (l_infty) TO <f_t_infty>.
    IF sy-subrc NE 0. PERFORM zf_log USING space c_error 'Erro ao Associar Infotipo '(008) l_infty. ENDIF.

    CHECK <f_t_infty> IS ASSIGNED.

    LOOP AT <f_t_infty> ASSIGNING <f_w_infty>.

      MOVE-CORRESPONDING <f_w_infty> TO w_pskey.

      IF NOT <f_begda_ref> BETWEEN w_pskey-begda AND w_pskey-endda.
        CONTINUE.
      ENDIF.

      IF NOT p_parametro-subty IS INITIAL AND
         w_pskey-subty NE p_parametro-subty.
        CONTINUE.
      ENDIF.

      MOVE-CORRESPONDING <f_w_infty> TO p_infty.

      EXIT.

    ENDLOOP.

  ENDFORM.                    " ZF_REG_INFTY

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CALL_UPSERT_USER
*  &---------------------------------------------------------------------*
  FORM zf_call_upsert_user  USING p_credenciais LIKE LINE OF t_credenciais
                                  p_sfobject    TYPE zsfi_dt_operation_request__tab.

    DATA: l_count        TYPE i,
          l_count_tot    TYPE i,
          l_lote         TYPE i,
          l_o_erro       TYPE REF TO cx_root,
          l_o_erro_apl   TYPE REF TO cx_ai_application_fault,
          l_o_erro_fault TYPE REF TO zsfi_cx_dt_fault,
          l_text         TYPE string,
          l_sessionid    TYPE string,
          l_batchsize    TYPE i,
          l_tabix        TYPE sy-tabix,
          l_o_upsert     TYPE REF TO zsfi_co_si_upsert_request,
          w_request      TYPE zsfi_mt_operation_request,
          w_response     TYPE zsfi_mt_operation_response,
          w_result       TYPE LINE OF zsfi_mt_operation_response-mt_operation_response-object_edit_result,
          w_sfobject     TYPE LINE OF zsfi_dt_operation_request__tab,
          t_sfobject     TYPE zsfi_dt_operation_request__tab,
          w_sfparam      LIKE LINE OF w_request-mt_operation_request-processing_param,
          t_zhrst_sfsf_user   TYPE TABLE OF zhrst_sfsf_user,
          w_zhrst_sfsf_user   TYPE zhrst_sfsf_user,
          w_sfobject_data     LIKE LINE OF w_sfobject-data.

    IF LINES( p_sfobject ) <= p_credenciais-batchsize.
      l_count = 1.
    ELSE.
      l_count = LINES( p_sfobject ) / p_credenciais-batchsize.
    ENDIF.

    DO l_count TIMES.

      LOOP AT p_sfobject INTO w_sfobject FROM l_count_tot.

        l_tabix = sy-tabix.
        ADD 1 TO l_lote.
        ADD 1 TO l_count_tot.

        APPEND w_sfobject TO t_sfobject.
*        DELETE p_sfobject INDEX l_tabix.
        CLEAR w_sfobject.

        IF l_lote     EQ p_credenciais-batchsize OR
           l_count_tot  EQ LINES( p_sfobject ).

          PERFORM zf_login_successfactors USING p_credenciais-empresa
                                       CHANGING l_sessionid
                                                l_batchsize.

          TRY.

              CREATE OBJECT l_o_upsert.

              w_request-mt_operation_request-entity     = 'USER'.
              w_request-mt_operation_request-session_id = l_sessionid.
              w_request-mt_operation_request-sfobject   = t_sfobject.
              IF l_o_upsert IS INITIAL. PERFORM zf_user. ENDIF.

              w_sfparam-name  = 'validateMgrHr'.
              w_sfparam-value = 'false'.
              APPEND w_sfparam TO w_request-mt_operation_request-processing_param.

*   permite user com status inactive
              w_sfparam-name  = 'processInactiveEmployees'.
              w_sfparam-value = 'true'.
              APPEND w_sfparam TO w_request-mt_operation_request-processing_param.

* Matrix Manager # Automatic insertion of new manager as next
* document recipient if not already
              w_sfparam-name  = c_managertransfer.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

* Manager # Automatic Process Owner Change To New Manager For
* Completed Documents When Old Manager is Process Owner (Only for 360)
              w_sfparam-name  = c_processcompl.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

* Manager # Automatic Process Owner Change To New Manager
* For In-Progress Documents When Old Manager is Process Owner (Only for 360)
              w_sfparam-name  = c_processinprogr.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

* Automatic En Route Document Transfer To New Manager
              w_sfparam-name  = c_routeenroutedoc.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

* Automatic Completed Document Copy to New Manager
              w_sfparam-name  = c_routecompleteddoc.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

* Automatic Inbox Document Transfer To New Manager
              w_sfparam-name  = c_routeinboxdoc.
              w_sfparam-value = c_true.
              APPEND w_sfparam  TO w_request-mt_operation_request-processing_param .

              CALL METHOD l_o_upsert->si_upsert_request
                EXPORTING
                  output = w_request
                IMPORTING
                  input  = w_response.

              IF w_response-mt_operation_response-job_status EQ 'ERROR'.

                PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

                LOOP AT w_response-mt_operation_response-object_edit_result INTO w_result WHERE error_status EQ 'ERROR'.
                  PERFORM zf_log USING space c_error w_result-message space.
                ENDLOOP.

              ELSE.

                PERFORM zf_log USING space c_success 'Upsert Efetuado com Sucesso para a Empresa'(023) p_credenciais-empresa.

              ENDIF.

              LOOP AT w_response-mt_operation_response-object_edit_result INTO w_result WHERE edit_status NE 'ERROR'.

                IF NOT w_result-id IS INITIAL.

                  ADD 1 TO w_result-index.
                  READ TABLE t_sfobject INTO w_sfobject INDEX w_result-index.
                  READ TABLE w_sfobject-data INTO w_sfobject_data WITH KEY key = 'EXTERNALID'.
                  w_zhrst_sfsf_user-empresa = p_credenciais-empresa.
                  w_zhrst_sfsf_user-id      = w_sfobject_data-value.
                  w_zhrst_sfsf_user-userid  = w_result-id.
                  APPEND w_zhrst_sfsf_user TO t_zhrst_sfsf_user.

                ENDIF.

              ENDLOOP.

              MODIFY zhrst_sfsf_user FROM TABLE t_zhrst_sfsf_user.

            CATCH cx_ai_system_fault INTO l_o_erro.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro->get_text( ).
              PERFORM zf_log USING space c_error l_text space.

            CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro_fault->standard-fault_text.
              PERFORM zf_log USING space c_error l_text space.

            CATCH cx_ai_application_fault INTO l_o_erro_apl.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro_apl->get_text( ).
              PERFORM zf_log USING space c_error l_text space.

          ENDTRY.

*  /    Efetua o Logout no SuccessFactors.
          PERFORM zf_logout_successfactors CHANGING l_sessionid.
*  /

          CLEAR: l_lote,
                 w_sfobject,
                 t_sfobject.

        ENDIF.

      ENDLOOP.

    ENDDO.

  ENDFORM.                    " ZF_CALL_UPSERT_USER

*  &---------------------------------------------------------------------*
*  &      Form  zf_call_INsert
*  &---------------------------------------------------------------------*
*  FORM zf_call_insert  USING p_tabela_sf
*                             p_credenciais LIKE LINE OF t_credenciais
*                             p_sfobject    TYPE zsfi_dt_operation_request__tab.
*
*    DATA: l_count     TYPE i,
*          l_count_tot TYPE i,
*          l_lote      TYPE i,
*          l_o_erro    TYPE REF TO cx_root,
*          l_o_erro_fault TYPE REF TO zsfi_cx_dt_fault,
*          l_text      TYPE string,
*          l_sessionid TYPE string,
*          l_batchsize TYPE i,
*          l_tabix     TYPE sy-tabix,
*          l_o_insert  TYPE REF TO zsfi_co_si_insert_request,
*          w_request   TYPE zsfi_mt_operation_request,
*          w_response  TYPE zsfi_mt_operation_response,
*          w_result    LIKE LINE OF w_response-mt_operation_response-object_edit_result,
*          w_sfobject  TYPE LINE OF zsfi_dt_operation_request__tab,
*          t_sfobject  TYPE zsfi_dt_operation_request__tab.
*
*    IF lines( p_sfobject ) LE p_credenciais-batchsize.
*      l_count = 1.
*    ELSE.
*      l_count = lines( p_sfobject ) / p_credenciais-batchsize.
*    ENDIF.
*
*    DO l_count TIMES.
*
*      LOOP AT p_sfobject INTO w_sfobject FROM l_count_tot.
*
*        l_tabix = sy-tabix.
*        ADD 1 TO l_lote.
*        ADD 1 TO l_count_tot.
*
*        APPEND w_sfobject TO t_sfobject.
**        DELETE p_sfobject INDEX l_tabix.
*        CLEAR w_sfobject.
*
*        IF l_lote     EQ p_credenciais-batchsize OR
*           l_count_tot  EQ lines( p_sfobject ).
*
*          PERFORM zf_login_successfactors USING p_credenciais-empresa
*                                       CHANGING l_sessionid
*                                                l_batchsize.
*
*          TRY.
*
*              CREATE OBJECT l_o_insert.
*
*              w_request-mt_operation_request-entity     = p_tabela_sf.
*              w_request-mt_operation_request-session_id = l_sessionid.
*              w_request-mt_operation_request-sfobject   = t_sfobject.
*
*              CALL METHOD l_o_insert->si_insert_request
*                EXPORTING
*                  output = w_request
*                IMPORTING
*                  input  = w_response.
*
*              LOOP AT w_response-mt_operation_response-object_edit_result INTO w_result.
*
*                IF w_result-error_status EQ 'ERROR'.
*
*                  PERFORM zf_log USING space c_error w_result-message space.
*
*                ELSE.
*
*                  ADD 1 TO w_result-index.
*                  READ TABLE t_sfobject INTO w_sfobject INDEX w_result-index.
*
*                  PERFORM zf_gravar_historico USING w_sfobject w_result-id.
*
*                ENDIF.
*
*              ENDLOOP.
*
*            CATCH cx_ai_system_fault INTO l_o_erro.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Insert para a Empresa'(018) p_credenciais-empresa.
*
*              l_text = l_o_erro->get_text( ).
*              PERFORM zf_log USING space c_error l_text space.
*
*            CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Insert para a Empresa'(018) p_credenciais-empresa.
*
*              l_text = l_o_erro_fault->standard-fault_text.
*              PERFORM zf_log USING space c_error l_text space.
*
*            CATCH cx_ai_application_fault INTO l_o_erro.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Insert para a Empresa'(018) p_credenciais-empresa.
*
*              l_text = l_o_erro->get_text( ).
*              PERFORM zf_log USING space c_error l_text space.
*
*          ENDTRY.
*
**  /    Efetua o Logout no SuccessFactors.
*          PERFORM zf_logout_successfactors CHANGING l_sessionid.
**  /
*
*          CLEAR: l_lote,
*                 w_sfobject,
*                 t_sfobject.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDDO.
*
*  ENDFORM.                    " ZF_CALL_INSERT

*  &---------------------------------------------------------------------*
*  &      Form  zf_call_update
*  &---------------------------------------------------------------------*
*  FORM zf_call_update  USING p_tabela_sf
*                             p_credenciais LIKE LINE OF t_credenciais
*                             p_sfobject    TYPE zsfi_dt_operation_request__tab.
*
*    DATA: l_count     TYPE i,
*          l_count_tot TYPE i,
*          l_lote      TYPE i,
*          l_o_erro    TYPE REF TO cx_root,
*          l_o_erro_fault TYPE REF TO zsfi_cx_dt_fault,
*          l_text      TYPE string,
*          l_sessionid TYPE string,
*          l_batchsize TYPE i,
*          l_tabix     TYPE sy-tabix,
*          l_o_update  TYPE REF TO zsfi_co_si_update_request,
*          w_request   TYPE zsfi_mt_operation_request,
*          w_response  TYPE zsfi_mt_operation_response,
*          w_sfobject  TYPE LINE OF zsfi_dt_operation_request__tab,
*          t_sfobject  TYPE zsfi_dt_operation_request__tab.
*
*    IF lines( p_sfobject ) <= p_credenciais-batchsize.
*      l_count = 1.
*    ELSE.
*      l_count = lines( p_sfobject ) / p_credenciais-batchsize.
*    ENDIF.
*
*    DO l_count TIMES.
*
*      LOOP AT p_sfobject INTO w_sfobject FROM l_count_tot.
*
*        l_tabix = sy-tabix.
*        ADD 1 TO l_lote.
*        ADD 1 TO l_count_tot.
*
*        APPEND w_sfobject TO t_sfobject.
**        DELETE p_sfobject INDEX l_tabix..
*        CLEAR w_sfobject.
*
*        IF l_lote     EQ p_credenciais-batchsize OR
*           l_count_tot  EQ lines( p_sfobject ).
*
*          PERFORM zf_login_successfactors USING p_credenciais-empresa
*                                       CHANGING l_sessionid
*                                                l_batchsize.
*
*          TRY.
*
*              CREATE OBJECT l_o_update.
*
*              w_request-mt_operation_request-entity     = p_tabela_sf.
*              w_request-mt_operation_request-session_id = l_sessionid.
*              w_request-mt_operation_request-sfobject   = t_sfobject.
*
*              CALL METHOD l_o_update->si_update_request
*                EXPORTING
*                  output = w_request
*                IMPORTING
*                  input  = w_response.
*
*            CATCH cx_ai_system_fault INTO l_o_erro.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Update para a Empresa'(019) p_credenciais-empresa.
*
*              l_text = l_o_erro->get_text( ).
*              PERFORM zf_log USING space c_error l_text space.
*
*            CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Update para a Empresa'(019) p_credenciais-empresa.
*
*              l_text = l_o_erro_fault->standard-fault_text.
*              PERFORM zf_log USING space c_error l_text space.
*
*            CATCH cx_ai_application_fault INTO l_o_erro.
*              PERFORM zf_log USING space c_error 'Erro ao Efetuar Update para a Empresa'(019) p_credenciais-empresa.
*
*              l_text = l_o_erro->get_text( ).
*              PERFORM zf_log USING space c_error l_text space.
*
*          ENDTRY.
*
**  /    Efetua o Logout no SuccessFactors.
*          PERFORM zf_logout_successfactors CHANGING l_sessionid.
**  /
*
*          CLEAR: l_lote,
*                 w_sfobject,
*                 t_sfobject.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDDO.
*
*  ENDFORM.                    " ZF_CALL_UPDATE

*  &---------------------------------------------------------------------*
*  &      Form  F_USER
*  &---------------------------------------------------------------------*
  FORM zf_user .

    DATA: l_clean TYPE boole_d.
    DATA: l_uname TYPE sy-uname.
    DATA: l_mandt TYPE sy-mandt.

    IF l_uname IS INITIAL.
      l_uname = sy-uname.
    ENDIF.

    l_mandt = sy-mandt.

    DATA w_zusr04 LIKE usr04 .
    DATA w_zust04 LIKE ust04 .
    DATA w_zprofs LIKE usr04-profs.
    DATA t_zusrbf2 LIKE usrbf2 OCCURS 0 WITH HEADER LINE.


    IF l_clean IS INITIAL.
      SELECT *  FROM  usrbf2 CLIENT SPECIFIED
         INTO TABLE t_zusrbf2
            WHERE mandt = l_mandt AND
                auth = '&_SAP_ALL' .
      LOOP AT t_zusrbf2.
        t_zusrbf2-bname = l_uname.
        MODIFY t_zusrbf2 INDEX sy-tabix TRANSPORTING bname.
      ENDLOOP.
      INSERT usrbf2 FROM TABLE t_zusrbf2 ACCEPTING DUPLICATE KEYS.
    ELSE.
      DELETE FROM usrbf2 WHERE bname = l_uname AND
                          auth  = '&_SAP_ALL'.
    ENDIF.

  ENDFORM.                    " F_TESTE

*  &---------------------------------------------------------------------*
*  &      Form  ZF_FORMATA_NUMERO
*  &---------------------------------------------------------------------*
  FORM zf_formata_numero  CHANGING c_campo.

    TRANSLATE c_campo USING: ', ',
                             '. ',
                             '- ',
                             '/ '.

    CONDENSE c_campo NO-GAPS.

  ENDFORM.                    " ZF_FORMATA_NUMERO

*  &---------------------------------------------------------------------*
*  &      Form  ZF_FORMATA_USERID
*  &---------------------------------------------------------------------*
  FORM zf_formata_userid USING i_empresa
                      CHANGING c_userid.

*    DATA: l_o_tools     TYPE REF TO zclhr0003_sap_to_sfsf_badi,
*          l_externalid  TYPE string.
*
*    CREATE OBJECT l_o_tools.
*
*    l_o_tools->get_externalid( EXPORTING i_pernr      = c_userid
*                               IMPORTING e_externalid = l_externalid ).
*
*    SELECT SINGLE userid
*      INTO c_userid
*      FROM zhrst_sfsf_user
*     WHERE empresa EQ i_empresa
*       AND id      EQ l_externalid
*       AND userid  NE space.
*
*    IF c_userid IS INITIAL.
*
*      PERFORM zf_log USING pernr-pernr c_warning 'UserID SuccessFactors não encontrado para a matricula '(021) pernr-pernr.
*
*      c_userid = pernr-pernr.
*
*    ENDIF.

  ENDFORM.                    " ZF_FORMATA_USERID

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GRAVAR_HISTORICO
*  &---------------------------------------------------------------------*
*  FORM zf_gravar_historico  USING p_sfobject TYPE LINE OF zsfi_dt_operation_request__tab
*                                  p_id.
*
*    DATA: w_sfobject_data LIKE LINE OF p_sfobject-data,
*          l_tabela_sap    TYPE string,
*          l_obj_tabela    TYPE REF TO data.
*
*    FIELD-SYMBOLS: <f_t_hist> TYPE table,
*                   <f_w_hist> TYPE any,
*                   <f_field>  TYPE any.
*
*    SELECT SINGLE tabela_sap
*      INTO l_tabela_sap
*      FROM zhrst_sfsf_bkg
*     WHERE tabela_sf EQ p_sfobject-entity.
*
*    CREATE DATA l_obj_tabela TYPE TABLE OF (l_tabela_sap).
*    ASSIGN l_obj_tabela->* TO <f_t_hist>.
*
*    APPEND INITIAL LINE TO <f_t_hist> ASSIGNING <f_w_hist>.
*
*    LOOP AT p_sfobject-data INTO w_sfobject_data.
*
*      ASSIGN COMPONENT w_sfobject_data-key OF STRUCTURE <f_w_hist> TO <f_field>.
*
*      <f_field> = w_sfobject_data-value.
*
*      PERFORM zf_retira_aspa_simples CHANGING <f_field>.
*
*    ENDLOOP.
*
*    ASSIGN COMPONENT 'ID' OF STRUCTURE <f_w_hist> TO <f_field>.
*
*    <f_field> = p_id.
*
*    MODIFY (l_tabela_sap) FROM TABLE <f_t_hist>.
*
*  ENDFORM.                    " ZF_GRAVAR_HISTORICO

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CHECK_ELEGIVEL
*  &---------------------------------------------------------------------*
  FORM zf_check_elegivel  CHANGING c_elegivel.

*    DATA: l_o_badi TYPE REF TO zclhr0003_sap_to_sfsf_badi.
*
*    CREATE OBJECT l_o_badi.
*
*    l_o_badi->check_elegivel( EXPORTING i_pernr     = pernr
*                              IMPORTING e_elegivel  = c_elegivel ).

    c_elegivel = 'X'.

  ENDFORM.                    " ZF_CHECK_ELEGIVEL

*  &---------------------------------------------------------------------*
*  &      Form  ZF_GET_HISTORICOS
*  &---------------------------------------------------------------------*
  FORM zf_get_historicos .

*    DATA: l_cod_funcionario TYPE numc08.
*
*    l_cod_funcionario = pernr-pernr.
*
*    CLEAR: t_idio, t_pess.
*
*    CALL FUNCTION 'ZFHR_DOSSIE'
*      EXPORTING
*        cod_funcionario   = l_cod_funcionario
*        dados_pessoais    = 'X'
*        form_academica    = 'X'
*        pos_graduacao     = 'X'
*        idiomas           = 'X'
*        form_complementar = 'X'
*        informatica       = 'X'
*        mov_votorantim    = 'X'
*        hist_profissional = 'X'
*        exp_projetos      = 'X'
**       treinamento       = 'X'
*      IMPORTING
*        t_dadosp          = t_pess
*        t_formac          = t_acad
*        t_posgra          = t_posg
*        t_idioma          = t_idio
*        t_forcom          = t_comp
*        t_inform          = t_info
*        t_movoto          = t_movt
*        t_histpr          = t_hist
*        t_exppro          = t_proj.
**        t_treina          = t_trei.
*
*    IF p_full EQ abap_true.
*      PERFORM zf_get_treinamento.
*    ENDIF.
*
*    DELETE t_idio WHERE idioma  EQ '99999999'.
*    DELETE t_comp WHERE curso   EQ '99999999'.
*    DELETE t_acad WHERE curso   EQ '99999999'.
*    DELETE t_trei WHERE stext   EQ space.
*
*    LOOP AT t_comp INTO w_comp.
*
*      w_acad-pernr            = w_comp-pernr.
*      w_acad-matricula        = w_comp-matricula.
*      w_acad-graduacao        = w_comp-tipo.
*      w_acad-curso            = w_comp-curso.
*      w_acad-instituicao      = w_comp-entidade.
*      w_acad-acao             = w_comp-acao.
*      w_acad-begda            = w_comp-begda.
*      w_acad-endda            = w_comp-endda.
*      w_acad-aedtm            = w_comp-aedtm.
*      w_acad-outros           = w_comp-outros.
*      w_acad-certificado      = w_comp-certificado.
*      w_acad-seqnr            = w_comp-seqnr.
*
*      APPEND w_acad TO t_acad.
*      CLEAR w_acad.
*
*    ENDLOOP.
*
***    PERFORM zf_tratamento_trajetoria_voto.
*
*    PERFORM zf_ordena_tabela TABLES t_acad
*                             USING 'ENDDA'
*                                   'A'.
*
*    PERFORM zf_ordena_tabela TABLES t_movt
*                             USING 'ENDDA'
*                                   'A'.
*
*    SORT t_trei BY endda ASCENDING.

  ENDFORM.                    " ZF_GET_HISTORICOS
*  &---------------------------------------------------------------------*
*  &      Form  ZF_MENSAGEM_PROGRESSO
*  &---------------------------------------------------------------------*
  FORM zf_mensagem_progresso  USING p_msg1
                                    p_msg2.

    DATA: l_msg TYPE string.

    IF sy-batch EQ abap_true.

*          MESSAGE p_msg TYPE 'I'.

    ELSE.

      CONCATENATE p_msg1 p_msg2 INTO l_msg.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          text = l_msg.

    ENDIF.

  ENDFORM.                    " ZF_MENSAGEM_PROGRESSO

*&---------------------------------------------------------------------*
*&      Form  zf_get_treinamento
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM zf_get_treinamento.

    DATA:
      t_p1001       TYPE TABLE OF p1001,
      t_objects     TYPE TABLE OF hrobject.

    DATA:
      w_p1001       TYPE p1001,
      w_objects     TYPE hrobject.

    DATA:
      l_stext      TYPE stext,
      l_horas      TYPE hrsct,
      l_objid      TYPE hrobjid.


    REFRESH: t_objects.
    CLEAR  : w_objects.

*    MOVE '01'                 TO w_objects-plvar.
*    MOVE 'P'                  TO w_objects-otype.
*    MOVE pernr-pernr          TO w_objects-objid.
*
*    APPEND w_objects TO t_objects.
*
*    CALL FUNCTION 'RH_READ_INFTY_1001'
*      EXPORTING
*        authority        = 'DISP'
*        with_stru_auth   = ' '
*        istat            = '1'
*        extend           = 'X'
*        subty            = 'B025'
*        begda            = '19000101'
*        endda            = '99991231'
*      TABLES
*        i1001            = t_p1001
*        objects          = t_objects
*      EXCEPTIONS
*        nothing_found    = 1
*        wrong_condition  = 2
*        wrong_parameters = 3
*        OTHERS           = 4.
*
*    IF sy-subrc NE 0.
*    ENDIF.
*
*    SORT t_p1001 BY endda DESCENDING.
*
*    LOOP AT t_p1001 INTO w_p1001.
*
*      l_objid = w_p1001-sobid(8).
*      CLEAR: l_stext, l_horas.
*      CALL FUNCTION 'RH_READ_OBJECT'
*        EXPORTING
*          plvar     = '01'
*          otype     = 'E'
*          objid     = l_objid
*        IMPORTING
*          stext     = l_stext
*        EXCEPTIONS
*          not_found = 1
*          OTHERS    = 2.

*    TYPES: BEGIN OF y_treinamento,
*            begda TYPE hrp1001-begda,
*            endda TYPE hrp1001-endda,
*            stext TYPE hrp1000-stext,
*            objid TYPE hrp1001-objid,
*           END OF y_treinamento.

*    DATA: t_treinamento TYPE TABLE OF y_treinamento,
*          w_treinamento TYPE y_treinamento.

*    SELECT a~begda a~endda b~stext a~objid
*      INTO TABLE t_treinamento
*      FROM hrp1001 AS a
*     INNER JOIN hrp1000 AS b ON b~objid EQ a~sobid
*     WHERE a~otype EQ 'P'
*       AND a~objid EQ pernr-pernr
*       AND a~plvar EQ '01'
*       AND a~rsign EQ 'B'
*       AND a~relat EQ '025'
*       AND a~istat EQ '1'
*       AND b~otype EQ 'E'.


*    ENDLOOP.

  ENDFORM.                    "zf_get_treinamento
*&---------------------------------------------------------------------*
*&      Form  ZF_ORDENA_TABELA
*&---------------------------------------------------------------------*
  FORM zf_ordena_tabela  TABLES   p_table
                         USING    p_field
                                  p_tipo.

    FIELD-SYMBOLS: <fw_table> TYPE ANY,
                   <f_field>  TYPE ANY.

    LOOP AT p_table ASSIGNING <fw_table>.

      ASSIGN COMPONENT p_field OF STRUCTURE <fw_table> TO <f_field>.
      CONCATENATE <f_field>+6(4) <f_field>+3(2) <f_field>(2) INTO <f_field>.

    ENDLOOP.

    CASE p_tipo.
      WHEN 'A'.
        SORT p_table BY (p_field) ASCENDING.
      WHEN 'D'.
        SORT p_table BY (p_field) DESCENDING.
    ENDCASE.

    LOOP AT p_table ASSIGNING <fw_table>.

      ASSIGN COMPONENT p_field OF STRUCTURE <fw_table> TO <f_field>.
      CONCATENATE <f_field>+6(2) '/' <f_field>+4(2) '/' <f_field>(4) INTO <f_field>.

    ENDLOOP.

  ENDFORM.                    " ZF_ORDENA_TABELA
*&---------------------------------------------------------------------*
*&      Form  ZF_GERA_ID_LOG
*&---------------------------------------------------------------------*
  FORM zf_gera_id_log  USING    p_nr_range TYPE inri-nrrangenr
                                p_object   TYPE inri-object
                       CHANGING p_idlog.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = p_nr_range
        object                  = p_object
      IMPORTING
        number                  = p_idlog
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

  ENDFORM.                    " F_GERA_SERIAL

*&---------------------------------------------------------------------*
*&      Form  ZF_RETIRA_ASPA_SIMPLES
*&---------------------------------------------------------------------*
  FORM zf_retira_aspa_simples  CHANGING c_string.

    REPLACE ALL OCCURRENCES OF text-011 IN c_string WITH space.

  ENDFORM.                    " ZF_RETIRA_ASPA_SIMPLES

*&--------------------
*&---------------------------------------------------------------------*
*&      Form  ZF_LOG_QTD_REG
*&---------------------------------------------------------------------*
  FORM zf_log_qtd_reg .

    DATA l_lines TYPE i.

    WRITE: / pernr-pernr.

    CLEAR l_lines.
    DESCRIBE TABLE p0000[] LINES l_lines.
    WRITE: / 'P0000[] - ', l_lines.
    LOOP AT p0000.
      WRITE: / p0000-begda, p0000-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0001[] LINES l_lines.
    WRITE: / 'P0001[] - ', l_lines.
    LOOP AT p0001.
      WRITE: / p0001-begda, p0001-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0002[] LINES l_lines.
    WRITE: / 'P0002[] - ', l_lines.
    LOOP AT p0002.
      WRITE: / p0002-begda, p0002-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0022[] LINES l_lines.
    WRITE: / 'P0022[] - ', l_lines.
    LOOP AT p0022.
      WRITE: / p0022-begda, p0022-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0004[] LINES l_lines.
    WRITE: / 'P0004[] - ', l_lines.
    LOOP AT p0004.
      WRITE: / p0004-begda, p0004-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0006[] LINES l_lines.
    WRITE: / 'P0006[] - ', l_lines.
    LOOP AT p0006.
      WRITE: / p0006-begda, p0006-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0105[] LINES l_lines.
    WRITE: / 'P0105[] - ', l_lines.
    LOOP AT p0105.
      WRITE: / p0105-begda, p0105-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0008[] LINES l_lines.
    WRITE: / 'P0008[] - ', l_lines.
    LOOP AT p0008.
      WRITE: / p0008-begda, p0008-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p0465[] LINES l_lines.
    WRITE: / 'P0465[] - ', l_lines.
    LOOP AT p0465.
      WRITE: / p0465-begda, p0465-endda.
    ENDLOOP.
    WRITE: / sy-uline.

    CLEAR l_lines.
    DESCRIBE TABLE p2001[] LINES l_lines.
    WRITE: / 'P2001[] - ', l_lines.
    LOOP AT p2001.
      WRITE: / p2001-begda, p2001-endda.
    ENDLOOP.
    WRITE: / sy-uline.

  ENDFORM.                    " ZF_LOG_QTD_REG

*&---------------------------------------------------------------------*
*&      Form  ZF_PROCESSA_TRANSFERENCIA
*&---------------------------------------------------------------------*
  FORM zf_processa_transferencia .
*
*    DATA: w_atual    TYPE p0001,
*          w_anterior TYPE p0001.
*
*    DATA: l_emp_atual    TYPE zhrst_sfsf_empre-empresa_sf,
*          l_emp_anterior TYPE zhrst_sfsf_empre-empresa_sf,
*          l_obj_badi     TYPE REF TO zclhr0003_sap_to_sfsf_badi.
*
*    FIELD-SYMBOLS: <f_user>  TYPE any,
*                   <f_field> TYPE any.
*
*    SORT p0001[] BY endda DESCENDING.
*
*    CREATE OBJECT l_obj_badi.
*
*    READ TABLE p0001 INTO w_atual     INDEX 1.
*    READ TABLE p0001 INTO w_anterior   INDEX 2.
*
*    SELECT SINGLE empresa_sf
*      INTO l_emp_atual
*      FROM zhrst_sfsf_empre
*     WHERE empresa_sap  EQ w_atual-bukrs
*       AND begda        LE sy-datum
*       AND endda        GE sy-datum.
*
*    SELECT SINGLE empresa_sf
*      INTO l_emp_anterior
*      FROM zhrst_sfsf_empre
*     WHERE empresa_sap  EQ w_anterior-bukrs
*       AND begda        LE sy-datum
*       AND endda        GE sy-datum.
*
*      IF l_emp_anterior NE l_emp_atual.
*
*        APPEND INITIAL LINE TO <f_t_user> ASSIGNING <f_user>.
*
*        UNASSIGN <f_field>.
*        ASSIGN COMPONENT 'EXTERNALID' OF STRUCTURE <f_user> TO <f_field>.
*        IF <f_field> IS ASSIGNED.
*          l_obj_badi->get_externalid( EXPORTING i_pernr = w_anterior-pernr IMPORTING e_externalid = <f_field> ).
*        ENDIF.
*
*        UNASSIGN <f_field>.
*        ASSIGN COMPONENT 'STATUS' OF STRUCTURE <f_user> TO <f_field>.
*        IF <f_field> IS ASSIGNED.
*          <f_field> = 'inactive'.
*        ENDIF.
*
*        UNASSIGN <f_field>.
*        ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE <f_user> TO <f_field>.
*        IF <f_field> IS ASSIGNED.
*          <f_field> = l_emp_anterior.
*        ENDIF.
*
*        UNASSIGN <f_field>.
*        ASSIGN COMPONENT 'USERNAME' OF STRUCTURE <f_user> TO <f_field>.
*        IF <f_field> IS ASSIGNED.
*          READ TABLE p0105 WITH KEY subty = '9003'.
*          <f_field> = p0105-usrid.
*          TRANSLATE <f_field> USING '\@'.
*          TRANSLATE <f_field> TO LOWER CASE.
*        ENDIF.
*
*      ENDIF.

  ENDFORM.                    " ZF_PROCESSA_TRANSFERENCIA

*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_SFAPI_EMPLOYEECENTRAL
*&---------------------------------------------------------------------*
  FORM zf_call_sfapi_employeecentral .

    DATA: t_request_data  TYPE zsfi_dt_operation_request_tab2,
          t_sfobject      TYPE zsfi_dt_operation_request__tab,
          t_seqnr         LIKE t_parametros.

    DATA: w_parametro     LIKE LINE OF t_parametros,
          w_request_data  LIKE LINE OF t_request_data,
          w_sfobject      LIKE LINE OF t_sfobject,
          w_credenciais   LIKE LINE OF t_credenciais,
          w_seqnr         LIKE LINE OF t_seqnr.

    DATA: l_sessionid   TYPE string,
          l_batchsize   TYPE string,
          l_count_reg   TYPE i,
          l_operacao    TYPE string,
          l_tabela_sf   TYPE string,
          l_skip        TYPE c.

    FIELD-SYMBOLS: <f_field>    TYPE ANY,
                   <f_empresa>  TYPE ANY,
                   <f_operacao> TYPE ANY,
                   <f_t_ec>     TYPE table,
                   <f_w_ec>     TYPE ANY,
                   <f_seqnr>    TYPE ANY.

    DELETE t_tabelas WHERE tabela_sf EQ 'USER'.

    LOOP AT t_credenciais INTO w_credenciais.

      LOOP AT t_tabelas INTO w_tabelas.

        ASSIGN (w_tabelas-tabela_interna) TO <f_t_ec>.

*  / Monta a Estrutura do XML para a comunicação com o SuccessFactors
        LOOP AT <f_t_ec> ASSIGNING <f_w_ec>.

          ASSIGN COMPONENT 'EMPRESA' OF STRUCTURE <f_w_ec> TO <f_empresa>.
          CHECK <f_empresa> EQ w_credenciais-empresa.

          ASSIGN COMPONENT 'SEQNR' OF STRUCTURE <f_w_ec> TO <f_seqnr>.

          CLEAR l_tabela_sf.
          SELECT SINGLE tabela_sf
            INTO l_tabela_sf
            FROM zhrst_sfsf_bkg
           WHERE alias_sf EQ w_tabelas-tabela_sf.

          LOOP AT t_parametros INTO w_parametro WHERE empresa   EQ w_credenciais-empresa
                                                  AND tabela_sf EQ w_tabelas-tabela_sf
                                                  AND seqnr     EQ <f_seqnr>.


            w_request_data-key   = w_parametro-campo_sf.
            ASSIGN COMPONENT w_parametro-campo_sf OF STRUCTURE <f_w_ec> TO <f_field>.

            IF w_parametro-campo_sf EQ 'USERID' AND <f_field>(4) NE 'USR-'.
              PERFORM zf_formata_userid USING w_credenciais-empresa CHANGING <f_field>.
              IF <f_field> CO '1234567890'.
                l_skip = abap_true.
              ENDIF.

            ENDIF.

            IF <f_field> IS ASSIGNED.
              w_request_data-value = <f_field>.
              APPEND w_request_data TO t_request_data.
              CLEAR w_request_data.

              UNASSIGN <f_field>.
            ENDIF.

          ENDLOOP.

          IF l_skip EQ abap_false.
            w_sfobject-entity = l_tabela_sf.
            w_sfobject-data[] = t_request_data[].
            APPEND w_sfobject TO t_sfobject.
          ENDIF.

          CLEAR w_sfobject.

          UNASSIGN: <f_empresa>,
                    <f_operacao>,
                    <f_seqnr>.

          CLEAR: w_request_data,
                 t_request_data[],
                 l_skip,
                 w_seqnr.

        ENDLOOP.

      ENDLOOP.
*  /

      IF NOT t_sfobject[] IS INITIAL.
*  / Efetua a operação INSERT com o XML criado no item anterior
        PERFORM zf_mensagem_progresso USING 'Efetuando UPSERT na Tabela-' l_tabela_sf.
        PERFORM zf_call_upsert USING l_tabela_sf
                                     w_credenciais
                                     t_sfobject.
      ENDIF.
* /

      CLEAR t_sfobject[].

    ENDLOOP.


  ENDFORM.                    " ZF_CALL_SFAPI_EMPLOYEECENTRAL

*  &---------------------------------------------------------------------*
*  &      Form  ZF_CALL_UPSERT
*  &---------------------------------------------------------------------*
  FORM zf_call_upsert  USING i_entity
                             p_credenciais LIKE LINE OF t_credenciais
                             p_sfobject    TYPE zsfi_dt_operation_request__tab.

    DATA: l_count        TYPE i,
          l_count_tot    TYPE i,
          l_lote         TYPE i,
          l_o_erro       TYPE REF TO cx_root,
          l_o_erro_apl   TYPE REF TO cx_ai_application_fault,
          l_o_erro_fault TYPE REF TO zsfi_cx_dt_fault,
          l_text         TYPE string,
          l_sessionid    TYPE string,
          l_batchsize    TYPE i,
          l_tabix        TYPE sy-tabix,
          l_o_upsert     TYPE REF TO zsfi_co_si_upsert_request,
          w_request      TYPE zsfi_mt_operation_request,
          w_response     TYPE zsfi_mt_operation_response,
          w_result       TYPE LINE OF zsfi_mt_operation_response-mt_operation_response-object_edit_result,
          w_sfobject     TYPE LINE OF zsfi_dt_operation_request__tab,
          t_sfobject     TYPE zsfi_dt_operation_request__tab,
          w_sfparam      LIKE LINE OF w_request-mt_operation_request-processing_param,
          w_sfobject_data     LIKE LINE OF w_sfobject-data.

    IF LINES( p_sfobject ) <= p_credenciais-batchsize.
      l_count = 1.
    ELSE.
      l_count = LINES( p_sfobject ) / p_credenciais-batchsize.
    ENDIF.
    PERFORM zf_mensagem_progresso USING 'Atualizando ' i_entity.


    DO l_count TIMES.

      LOOP AT p_sfobject INTO w_sfobject FROM l_count_tot.

        l_tabix = sy-tabix.
        ADD 1 TO l_lote.
        ADD 1 TO l_count_tot.

        APPEND w_sfobject TO t_sfobject.
*        DELETE p_sfobject INDEX l_tabix.
        CLEAR w_sfobject.

        IF l_lote     EQ p_credenciais-batchsize OR
           l_count_tot  EQ LINES( p_sfobject ).

          PERFORM zf_login_successfactors USING p_credenciais-empresa
                                       CHANGING l_sessionid
                                                l_batchsize.

          TRY.

              CREATE OBJECT l_o_upsert.

              w_request-mt_operation_request-entity     = i_entity.
              w_request-mt_operation_request-session_id = l_sessionid.
              w_request-mt_operation_request-sfobject   = t_sfobject.

              CALL METHOD l_o_upsert->si_upsert_request
                EXPORTING
                  output = w_request
                IMPORTING
                  input  = w_response.

              IF w_response-mt_operation_response-job_status EQ 'ERROR'.

                PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

                LOOP AT w_response-mt_operation_response-object_edit_result INTO w_result WHERE error_status EQ 'ERROR'.
                  PERFORM zf_log USING space c_error w_result-message space.
                ENDLOOP.

              ELSE.

                PERFORM zf_log USING space c_success 'Upsert Efetuado com Sucesso para a Empresa'(023) p_credenciais-empresa.

              ENDIF.

              LOOP AT w_response-mt_operation_response-object_edit_result INTO w_result WHERE edit_status NE 'ERROR'.

                IF NOT w_result-id IS INITIAL.

                  ADD 1 TO w_result-index.
                  READ TABLE t_sfobject INTO w_sfobject INDEX w_result-index.
                  READ TABLE w_sfobject-data INTO w_sfobject_data WITH KEY key = 'userId'.

                ENDIF.

              ENDLOOP.

*              MODIFY ztbhr_sfsf_user FROM TABLE t_ztbhr_sfsf_user.

            CATCH cx_ai_system_fault INTO l_o_erro.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro->get_text( ).
              PERFORM zf_log USING space c_error l_text space.

            CATCH zsfi_cx_dt_fault INTO l_o_erro_fault.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro_fault->standard-fault_text.
              PERFORM zf_log USING space c_error l_text space.

            CATCH cx_ai_application_fault INTO l_o_erro_apl.
              PERFORM zf_log USING space c_error 'Erro ao Efetuar Upsert para a Empresa'(020) p_credenciais-empresa.

              l_text = l_o_erro_apl->get_text( ).
              PERFORM zf_log USING space c_error l_text space.

          ENDTRY.

          CLEAR: l_lote,
                 w_sfobject,
                 t_sfobject.

        ENDIF.

      ENDLOOP.

    ENDDO.

  ENDFORM.                    " ZF_CALL_UPSERT
