CLASS zcl_taler_marshall DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS:
      start,
      reboot,
      stop.

  PRIVATE SECTION.
    CLASS-DATA gv_running TYPE abap_bool VALUE abap_false.

    CLASS-METHODS create_webhook
      IMPORTING is_cfg TYPE ztlr_config.
ENDCLASS.



CLASS zcl_taler_marshall IMPLEMENTATION.


  METHOD start.
    IF gv_running = abap_true.
      RETURN.
    ENDIF.

    DATA: lo_general TYPE REF TO zcl_taler_general.

    lo_general = NEW zcl_taler_general( ).

*– 1) load last valid config --------------------------------------------------
    DATA(ls_cfg) = lo_general->get_last_correct_config( ).
    IF ls_cfg IS INITIAL.
      SUBMIT ztlr_server_retrigger AND RETURN.
      RETURN.
    ENDIF.

*– 2) register / verify webhook ----------------------------------------------
    create_webhook( ls_cfg ).

    gv_running = abap_true.
  ENDMETHOD.



  METHOD stop.
    gv_running = abap_false.
  ENDMETHOD.



  METHOD reboot.
    stop( ).
    start( ).
  ENDMETHOD.



  METHOD create_webhook.
*----------------------------------------------------------------------------*
*  private helper: (re-)create webhook on merchant backend                   *
*----------------------------------------------------------------------------*
    DATA: lo_general TYPE REF TO zcl_taler_general,
          lv_code TYPE i.

    lo_general = NEW zcl_taler_general( ).

*– Load last valid config --------------------------------------------------
    DATA(ls_cfg) = lo_general->get_last_correct_config( ).

    " Maybe change to taler-req-uri aber who cares
    DATA(lv_url)
      = |{ ls_cfg-taler_req_uri }webhooks|.

    DATA: lo_cl TYPE REF TO if_http_client.
    cl_http_client=>create_by_url(
      EXPORTING url    = lv_url
      IMPORTING client = lo_cl ).

*– JSON body with body_template ----------------------------------------------
    "TODO: We might want to provide it as setting option
    DATA(lv_body) = |\{"webhook_id":"sap-settled","event_type":"order_settled","url":"https://vlhsapd30.hevs.ch/sap/taler_webhook","http_method":"POST","body_template":"\{\{order_id\}\}\|\{\{wtid\}\}"\}|.

    lo_cl->request->set_method( if_http_request=>co_request_method_post ).
    lo_cl->request->set_header_field( name = 'Authorization'
                                      value = |Bearer secret-token:{ ls_cfg-taler_password }| ).
    lo_cl->request->set_content_type( 'application/json' ).
    lo_cl->request->set_cdata( lv_body ).

    lo_cl->send( ).
    lo_cl->receive( ).

    lo_cl->response->get_status( IMPORTING code = lv_code ).

    IF lv_code = 204 OR lv_code = 409.
      RETURN.                      "nothing to log
    ENDIF.

    DATA(lv_long_message) = |\{ "context":"webhook", "type":"error", "information":"Webhook creation failed with HTTP { lv_code }. Payload: { lv_body }" \}|.

*– anything else is an error --------------------------------------------------
    NEW zcl_taler_general( )->add_notification(
      iv_system_part   = 'config'
      iv_not_type      = 'error'
      iv_short_message = |Webhook create failed (HTTP { lv_code })|
      iv_long_message  = lv_body ).

  ENDMETHOD.

ENDCLASS.

