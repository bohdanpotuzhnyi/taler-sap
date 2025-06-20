CLASS zcl_taler_marshall_w DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

  PRIVATE SECTION.
    CLASS-METHODS update_order_log
      IMPORTING
        iv_billing_doc TYPE ztlr_order_log-billing_doc
        iv_wt_id       TYPE ztlr_order_log-taler_wt_id.
ENDCLASS.



CLASS zcl_taler_marshall_w IMPLEMENTATION.

METHOD if_http_extension~handle_request.

    DATA(lo_req)  = server->request.
    DATA(lo_resp) = server->response.

*– only POST ------------------------------------------------------------------
    IF lo_req->get_header_field( '~request_method' ) <> 'POST'.
      lo_resp->set_status( code = 405 reason = 'Method Not Allowed' ).
      RETURN.
    ENDIF.

    DATA(lv_body) = lo_req->get_cdata( ).

    DATA: lv_bdoc TYPE ztlr_order_log-billing_doc,
          lv_wtid TYPE ztlr_order_log-taler_wt_id.

*– 1) preferred payload “billingDoc|wtid” -------------------------------------
    SPLIT lv_body AT '|' INTO lv_bdoc lv_wtid.
    IF lv_bdoc IS NOT INITIAL AND lv_wtid IS NOT INITIAL.
      update_order_log( iv_billing_doc = lv_bdoc
                        iv_wt_id       = lv_wtid ).
      lo_resp->set_status( code = 204 reason = 'No Content' ).
      RETURN.
    ENDIF.

    IF lv_bdoc IS INITIAL OR lv_wtid IS INITIAL.
      lo_resp->set_status( code = 400 reason = 'Missing data' ).
      RETURN.
    ENDIF.

    update_order_log( iv_billing_doc = lv_bdoc
                      iv_wt_id       = lv_wtid ).
    lo_resp->set_status( code = 204 reason = 'No Content' ).

  ENDMETHOD.



  METHOD update_order_log.

    UPDATE ztlr_order_log
       SET taler_state = 'settled',
           taler_wt_id = @iv_wt_id
     WHERE billing_doc = @iv_billing_doc.

    IF sy-subrc <> 0.
*     – nothing updated → create a visible error notification -----------------
      NEW zcl_taler_general( )->add_notification(
        iv_system_part   = 'orders'
        iv_not_type      = 'error'
        iv_short_message = |No order_log row for billing_doc { iv_billing_doc }|
        iv_long_message  = |\{ "billing_doc":"{ iv_billing_doc }", "type":"error", "information":"Webhook received but no matching record found. WTID={ iv_wt_id }" \}|
        ).

    ENDIF.

    COMMIT WORK.

  ENDMETHOD.


ENDCLASS.
