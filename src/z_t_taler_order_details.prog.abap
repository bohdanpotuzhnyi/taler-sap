*&---------------------------------------------------------------------*
*& Report z_t_taler_order_details
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_order_details.

PARAMETERS: p_order TYPE vbeln OBLIGATORY.

START-OF-SELECTION.

  DATA(lo_reader) = NEW zcl_taler_order( ).
  DATA(lv_json) = lo_reader->get_order_json( iv_sales_order = p_order iv_billing_doc = '90000031' ).
  lo_reader->display_order_info( iv_sales_order = p_order ).


  WRITE: 'Generated JSON:'.
  WRITE: lv_json.

  " DATA(lv_response) = lo_reader->send_order_to_taler( iv_sales_order = p_order ).

  WRITE: / 'API Response:'.
  " WRITE: / lv_response-content.
