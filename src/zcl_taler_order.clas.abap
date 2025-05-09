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

    TYPES: BEGIN OF ty_http_result,
         code    TYPE i,
         content TYPE string,
         req_body TYPE string,
       END OF ty_http_result.

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
            iv_billing_doc TYPE vbeln_vf
        RETURNING
            VALUE(rv_json) TYPE string.
    METHODS: send_order_to_taler
        IMPORTING
            iv_sales_order TYPE vbeln
            iv_billing_doc TYPE vbeln_vf
        RETURNING
            VALUE(rv_result) TYPE ty_http_result.

    TYPES: BEGIN OF ty_billing_header,
             vbeln TYPE vbrk-vbeln,   " Billing doc number
             netwr TYPE vbrk-netwr,   " Amount
             waerk TYPE vbrk-waerk,   " Currency
             vgbel TYPE vbrp-vgbel,   " Sales order
           END OF ty_billing_header,
           ty_billing_header_tab TYPE STANDARD TABLE OF ty_billing_header WITH EMPTY KEY.

    TYPES: BEGIN OF ty_billing_item,
             vbeln TYPE vbrp-vbeln,   " Billing doc number (header key)
             vgbel TYPE vbrp-vgbel,   " Sales document
             matnr TYPE vbrp-matnr,   " Material
             arktx TYPE vbrp-arktx,   " Short text
             fkimg TYPE vbrp-fkimg,   " Billing quantity
             meins TYPE vbrp-meins,   " Units of the product
           END OF ty_billing_item,
           ty_billing_item_tab TYPE STANDARD TABLE OF ty_billing_item WITH EMPTY KEY.

    METHODS update_taler_billing_docs.

    METHODS fetch_new_taler_billing_docs.

    METHODS post_created_orders_to_taler.

    METHODS update_taler_payment_uris.

    " DO NOT USE UNLESS UnitedResearchF
    METHODS clear_all_taler_tables.


    METHODS get_billing_docs
      EXPORTING
        et_billing_headers TYPE ty_billing_header_tab
        et_billing_items   TYPE ty_billing_item_tab.

    METHODS: check_if_order_exists
        IMPORTING
            iv_billing_doc TYPE vbrk-vbeln
        RETURNING
            VALUE(rv_exists) TYPE abap_bool.

    METHODS: insert_order_to_taler_tables
        IMPORTING
            is_header TYPE ty_billing_header
            it_items  TYPE ty_billing_item_tab
            iv_state  TYPE ztlr_order_log-state.

    PROTECTED SECTION.
    PRIVATE SECTION.
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

    " TODO: Terminate the usage of this function, and use only the taler tables 2 calls might be needed:
    " 1) ZTLR_ORDER_LOG to get the basic info, such as amount, currency,
    " 2) ZTLR_ORDER_PROD to get the products
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
    " TODO: GET THE AMOUNT FROM THE ORDER AND ONLY THEN REPLACE IT FOR KUDOS
    DATA(lv_amount_str) = |KUDOS:{ lv_amount }|.

    " Final JSON
   rv_json = '{"order": {' &&
    '"summary": "' && me->summary && '",' &&
    '"order_id": "' && iv_billing_doc && '",' &&
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

  rv_result-code    = -1.
  rv_result-content = 'HTTP client error.'.

  " Get JSON for the order
  lv_json = me->get_order_json( iv_sales_order = iv_sales_order
                                iv_billing_doc = iv_billing_doc ).

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
    rv_result-content = 'Failed to create HTTP client.'.
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
    rv_result-content = 'HTTP send failed.'.
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
    rv_result-content = 'HTTP receive failed.'.
    RETURN.
  ENDIF.

  " Get status code
  CALL METHOD lo_http_client->response->get_status
    IMPORTING
      code = lv_status_code.

  " Get response body
  lv_response_body = lo_http_client->response->get_cdata( ).

  " Return result
  rv_result-code    = lv_status_code.
  rv_result-content = lv_response_body.
  rv_result-req_body = lv_json.

ENDMETHOD.


METHOD get_billing_docs.
    DATA: lt_headers    TYPE ty_billing_header_tab,
          lt_items_temp TYPE STANDARD TABLE OF ty_billing_item WITH EMPTY KEY.

    CLEAR: et_billing_headers, et_billing_items.

    " 1) Fetch all billing headers from VBRK matching the filters
    SELECT vbeln,
           netwr,
           waerk
      FROM vbrk
      INTO TABLE @lt_headers
     WHERE zlsch = 'D'
       AND land1 = 'DE'
       AND vkorg = 'DS00'.

    " 2) Loop headers, append to export table and fetch corresponding items
    LOOP AT lt_headers INTO DATA(ls_header).
      APPEND ls_header TO et_billing_headers.

      " Fetch all items for this billing doc from VBRP
      SELECT vbeln,
             vgbel,
             matnr,
             arktx,
             fkimg,
             meins
        FROM vbrp
        INTO TABLE @lt_items_temp
       WHERE vbeln = @ls_header-vbeln.

      " Append each item
      LOOP AT lt_items_temp INTO DATA(ls_item).
        APPEND ls_item TO et_billing_items.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

METHOD check_if_order_exists.

  DATA: lv_dummy TYPE ztlr_order_log-billing_doc.

  SELECT SINGLE billing_doc
    INTO lv_dummy
    FROM ztlr_order_log
    WHERE billing_doc = iv_billing_doc.

  IF sy-subrc = 0.
    rv_exists = abap_true.
  ELSE.
    rv_exists = abap_false.
  ENDIF.

ENDMETHOD.

METHOD insert_order_to_taler_tables.

  " Declare compatible structure
  DATA: ls_order_log  TYPE ztlr_order_log,
        ls_order_prod TYPE ztlr_order_prod.

  " Fill and insert ZTLR_ORDER_LOG
  ls_order_log-billing_doc = is_header-vbeln.
  ls_order_log-sales_order = is_header-vgbel.
  ls_order_log-currency    = is_header-waerk.
  ls_order_log-amount      = is_header-netwr.
  ls_order_log-state       = iv_state.

  INSERT ztlr_order_log FROM @ls_order_log.
  IF sy-subrc <> 0.
    WRITE: / 'Failed to insert billing header:', is_header-vbeln.
    RETURN.
  ENDIF.

  " Loop over items and insert into ZTLR_ORDER_PROD
  LOOP AT it_items INTO DATA(ls_item).

    CLEAR ls_order_prod.
    ls_order_prod-sales_order     = ls_item-vgbel.
    ls_order_prod-material_number = ls_item-matnr.
    ls_order_prod-quantity        = ls_item-fkimg.
    ls_order_prod-units           = ls_item-meins.

    INSERT ztlr_order_prod FROM @ls_order_prod.
    IF sy-subrc <> 0.
      WRITE: / 'Failed to insert item for:', ls_item-vbeln, ls_item-matnr.
    ENDIF.

  ENDLOOP.

ENDMETHOD.

METHOD update_taler_billing_docs.

  CALL METHOD fetch_new_taler_billing_docs.

  " Add new function for checking for the return billing docs

  " Add new function for posting the order to taler and getting back the results of it
  CALL METHOD post_created_orders_to_taler.

  " Check posted orders, and fetch the token to create the url of format:
  " the body is usually next:
  " {
  "      "order_id": "5",
  "      "token": "Y7W6BBKRJFR62Y5CBABX8XFMNC"
  " }
  " The url that we need to make is consists of next objects
  " http://backend.talerintosap.us/orders/{billing_doc}?token={token fetched}
  CALL METHOD update_taler_payment_uris.

  " Add a new function

  " Add new function for updating the payment states of all orders

  " ...


ENDMETHOD.

METHOD clear_all_taler_tables.

  " Delete all history first (least dependent)
  DELETE FROM ztlr_order_hstat.
  IF sy-subrc = 0.
    WRITE: / 'ZTLR_ORDER_HSTAT cleared.'.
  ENDIF.

  " Delete all products (dependent on order log)
  DELETE FROM ztlr_order_prod.
  IF sy-subrc = 0.
    WRITE: / 'ZTLR_ORDER_PROD cleared.'.
  ENDIF.

  " Delete all order logs
  DELETE FROM ztlr_order_log.
  IF sy-subrc = 0.
    WRITE: / 'ZTLR_ORDER_LOG cleared.'.
  ENDIF.

  COMMIT WORK.

ENDMETHOD.

METHOD update_taler_payment_uris.

  DATA: lt_orders       TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
        ls_order        TYPE ztlr_order_log,
        lv_token_json   TYPE string,
        lv_token        TYPE string,
        lv_pay_uri      TYPE string.

  " Fetch orders with state 'posted'
  SELECT * FROM ztlr_order_log
    INTO TABLE @lt_orders
    WHERE state = 'posted'.

  LOOP AT lt_orders INTO ls_order.

    " Get latest token-containing response for the billing_doc
    SELECT http_response
        FROM ztlr_order_hstat
        WHERE billing_doc = @ls_order-billing_doc
            AND on_state    = 'created'
            AND http_code   = 200
        ORDER BY timestamp DESCENDING
        INTO @lv_token_json
        UP TO 1 ROWS.
    ENDSELECT.

    IF lv_token_json IS INITIAL.
      CONTINUE. " No response found
    ENDIF.

    " Extract the token value using REGEX
    CLEAR lv_token.
    FIND REGEX '"token"\s*:\s*"([^"]+)"' IN lv_token_json SUBMATCHES lv_token.

    IF lv_token IS INITIAL.
      CONTINUE. " Token not found in response
    ENDIF.

    " Construct the Taler pay URI
    CONCATENATE 'https://backoffice.talerintosap.us/orders/' ls_order-billing_doc
                '?token=' lv_token INTO lv_pay_uri.

    " Update the taler_pay_uri in the order log
    UPDATE ztlr_order_log
      SET taler_pay_uri = @lv_pay_uri,
          state         = 'posted_processed'
      WHERE billing_doc = @ls_order-billing_doc.

  ENDLOOP.

ENDMETHOD.


METHOD fetch_new_taler_billing_docs.

  DATA: lt_headers      TYPE ty_billing_header_tab,
        lt_items_temp   TYPE ty_billing_item_tab.

  " Fetch billing headers from VBRK
  SELECT vbeln,
         netwr,
         waerk
    INTO TABLE @lt_headers
    FROM vbrk
   WHERE zlsch = 'D'
     AND land1 = 'DE'
     AND vkorg = 'DS00'.

  LOOP AT lt_headers INTO DATA(ls_header).

    " Skip if it already exists in the custom table
    IF me->check_if_order_exists( iv_billing_doc = ls_header-vbeln ) = abap_true.
      CONTINUE.
    ENDIF.

    " Get vgbel from VBRP for the billing document
    SELECT SINGLE vgbel
      INTO @ls_header-vgbel
      FROM vbrp
      WHERE vbeln = @ls_header-vbeln.


    " Fetch corresponding items
    SELECT vbeln,
           vgbel,
           matnr,
           arktx,
           fkimg,
           meins
      INTO TABLE @lt_items_temp
      FROM vbrp
     WHERE vbeln = @ls_header-vbeln.

    " Insert into ZTLR_ORDER_LOG and ZTLR_ORDER_PROD
    me->insert_order_to_taler_tables(
      is_header = ls_header
      it_items  = lt_items_temp
      iv_state  = 'created'
    ).

  ENDLOOP.

ENDMETHOD.

METHOD post_created_orders_to_taler.

  DATA: lt_created_orders TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
        ls_created_order  TYPE ztlr_order_log,
        lv_response       TYPE ty_http_result,
        ls_hist           TYPE ztlr_order_hstat,
        lv_timestamp      TYPE timestamp,
        lv_timestamp_str TYPE string,
        lv_unique_id     TYPE string.

  " 1. Select orders with state 'created'
  SELECT *
    INTO TABLE @lt_created_orders
    FROM ztlr_order_log
    WHERE state = 'created'.

  LOOP AT lt_created_orders INTO ls_created_order.

    " 2. Call the method to post the order to Taler
    lv_response = me->send_order_to_taler( iv_sales_order = ls_created_order-sales_order
                                           iv_billing_doc = ls_created_order-billing_doc ).

    " Getting the SAP timestamp + generating the unique_id
    GET TIME STAMP FIELD lv_timestamp.
    lv_timestamp_str = |{ lv_timestamp TIMESTAMP = ISO }|.
    CONCATENATE lv_timestamp_str ls_created_order-billing_doc INTO lv_unique_id SEPARATED BY '_'.

    " 3. Log the response to ZTLR_ORDER_HSTAT
    CLEAR ls_hist.
    ls_hist-entry_id       = lv_unique_id.
    ls_hist-billing_doc    = ls_created_order-billing_doc.
    ls_hist-on_state       = 'created'.
    ls_hist-http_code      = lv_response-code.
    ls_hist-http_response  = lv_response-content.
    ls_hist-req_body       = lv_response-req_body.
    ls_hist-timestamp      = lv_timestamp.

    INSERT ztlr_order_hstat FROM @ls_hist.

    " UPDATE ztlr_order_log SET state = 'posted' WHERE billing_doc = @ls_created_order-billing_doc.
    IF 200 = lv_response-code.
        UPDATE ztlr_order_log SET error = '' WHERE billing_doc = @ls_created_order-billing_doc.
        UPDATE ztlr_order_log SET state = 'posted' WHERE billing_doc = @ls_created_order-billing_doc.
    ELSE.
        " TODO: ADD INFO TO NOTIFICATION TABLE
        UPDATE ztlr_order_log SET error = 'X' WHERE billing_doc = @ls_created_order-billing_doc.
    ENDIF.

  ENDLOOP.

ENDMETHOD.

ENDCLASS.
