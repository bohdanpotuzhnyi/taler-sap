CLASS zcl_taler_order DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: summary TYPE string VALUE 'Order from SAP'.
    TYPES: BEGIN OF ty_product_list,
             product_id TYPE matnr,
           END OF ty_product_list,
           ty_product_list_tab TYPE STANDARD TABLE OF ty_product_list WITH EMPTY KEY.

    METHODS: get_sales_order_info
      IMPORTING
        iv_sales_order TYPE vbeln
      EXPORTING
        ev_amount      TYPE netwr
        ev_currency    TYPE waerk
        et_product_ids TYPE ty_product_list_tab,

      display_order_info
      IMPORTING
        iv_sales_order TYPE vbeln.

    METHODS: get_order_json
        IMPORTING
            iv_sales_order TYPE vbeln
        RETURNING
            VALUE(rv_json) TYPE string.
    METHODS: send_order_to_taler
        IMPORTING
            iv_sales_order TYPE vbeln
        RETURNING
            VALUE(rv_response) TYPE string.
ENDCLASS.



CLASS zcl_taler_order IMPLEMENTATION.

  METHOD get_sales_order_info.
    CLEAR: ev_amount, et_product_ids.

    " Get sales order amount from VBAK
    SELECT SINGLE netwr
      INTO ev_amount
      FROM vbak
      WHERE vbeln = iv_sales_order.

    " Get list of product IDs from VBAP
    SELECT matnr
      INTO TABLE @DATA(lt_matnr)
      FROM vbap
      WHERE vbeln = @iv_sales_order.

    LOOP AT lt_matnr INTO DATA(lv_matnr).
      APPEND VALUE #( product_id = lv_matnr ) TO et_product_ids.
    ENDLOOP.

    SELECT SINGLE netwr waerk
        INTO (ev_amount, ev_currency)
        FROM vbak
        WHERE vbeln = iv_sales_order.

  ENDMETHOD.




  METHOD display_order_info.
    DATA: lv_amount     TYPE netwr,
          lv_currency   TYPE waerk,
          lt_products   TYPE ty_product_list_tab.

    CALL METHOD me->get_sales_order_info
      EXPORTING
        iv_sales_order = iv_sales_order
      IMPORTING
        ev_amount      = lv_amount
        ev_currency    = lv_currency
        et_product_ids = lt_products.

    IF sy-subrc <> 0 OR lv_amount IS INITIAL.
      WRITE: / 'Order not found or has no amount.'.
      RETURN.
    ENDIF.

    WRITE: / 'summary:', me->summary.
    WRITE: / 'order_id:', iv_sales_order.
    WRITE: / 'amount:', lv_currency, ':', lv_amount.
    WRITE: / 'products:'.

    LOOP AT lt_products INTO DATA(ls_product).
      WRITE: / ls_product-product_id.

    ENDLOOP.

    ENDMETHOD.

  METHOD get_order_json.
    DATA: lv_amount     TYPE netwr,
          lv_currency   TYPE waerk,
          lt_products   TYPE ty_product_list_tab,
          lt_json_products TYPE STANDARD TABLE OF string WITH EMPTY KEY,
          lv_product    TYPE string,
          rv_line       TYPE string.

    CALL METHOD me->get_sales_order_info
      EXPORTING
        iv_sales_order = iv_sales_order
      IMPORTING
        ev_amount      = lv_amount
        ev_currency    = lv_currency
        et_product_ids = lt_products.

    IF lv_amount IS INITIAL.
      rv_json = '{"error": "Order not found or empty."}'.
      RETURN.
    ENDIF.

    " Build product JSON array
    LOOP AT lt_products INTO DATA(ls_product).
      lv_product = '{ "product_id": "' && ls_product-product_id && '" }'.
      APPEND lv_product TO lt_json_products.
    ENDLOOP.

    DATA(lv_products_json) = ||.
    LOOP AT lt_json_products INTO rv_line.
      IF lv_products_json IS INITIAL.
        lv_products_json = rv_line.
      ELSE.
        CONCATENATE lv_products_json ',' rv_line INTO lv_products_json.
      ENDIF.
    ENDLOOP.

    " Format amount
    DATA(lv_amount_str) = |KUDOS:{ lv_amount }|.

    " Final JSON
   rv_json = '{"order": {' &&
    '"summary": "' && me->summary && '",' &&
    '"order_id": "' && iv_sales_order && '",' &&
    '"amount": "' && lv_amount_str && '",' &&
    '"products": [' && lv_products_json && ']' &&
    '}}'.


  ENDMETHOD.


  METHOD send_order_to_taler.

  DATA: lv_json            TYPE string,
        lo_http_client     TYPE REF TO if_http_client,
        lv_url             TYPE string VALUE 'https://backoffice.talerintosap.us/private/orders',
        lv_token           TYPE string VALUE 'Bearer secret-token:TALERintoSAP2527',
        lv_status_code     TYPE i,
        lv_response_body   TYPE string.

  " Get JSON for the order
  lv_json = me->get_order_json( iv_sales_order = iv_sales_order ).

  " Create HTTP client instance
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = lv_url
    IMPORTING
      client             = lo_http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      others             = 4.

  IF sy-subrc <> 0 OR lo_http_client IS INITIAL.
    rv_response = 'Failed to create HTTP client.'.
    RETURN.
  ENDIF.

  " Set HTTP method to POST
  lo_http_client->request->set_method( if_http_request=>co_request_method_post ).

  " Set required headers
  lo_http_client->request->set_header_field( name = 'Content-Type'  value = 'application/json' ).
  lo_http_client->request->set_header_field( name = 'Accept'        value = 'application/json' ).
  lo_http_client->request->set_header_field( name = 'Authorization' value = lv_token ).

  " Set request body to the JSON
  lo_http_client->request->set_cdata( lv_json ).

  " Send the HTTP request
  CALL METHOD lo_http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      others                     = 4.

  IF sy-subrc <> 0.
    rv_response = 'HTTP send failed.'.
    RETURN.
  ENDIF.

  " Receive the response
  CALL METHOD lo_http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      others                     = 4.

  IF sy-subrc <> 0.
    rv_response = 'HTTP receive failed.'.
    RETURN.
  ENDIF.

  " Get status code
  CALL METHOD lo_http_client->response->get_status
    IMPORTING
      code = lv_status_code.

  " Get response body
  lv_response_body = lo_http_client->response->get_cdata( ).

  " Return result
  rv_response = |HTTP { lv_status_code }: { lv_response_body }|.

ENDMETHOD.





ENDCLASS.
