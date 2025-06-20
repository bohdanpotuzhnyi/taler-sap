CLASS zcl_taler_order DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_product_list,
             product_id          TYPE matnr,
             product_description TYPE maktx,
           END OF ty_product_list,
           ty_product_list_tab TYPE STANDARD TABLE OF ty_product_list WITH EMPTY KEY.

    TYPES: BEGIN OF ty_http_result,
             code     TYPE i,
             content  TYPE string,
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
        iv_sales_order   TYPE vbeln
        iv_billing_doc   TYPE vbeln_vf
      RETURNING
        VALUE(rv_result) TYPE ty_http_result.

    TYPES: BEGIN OF ty_billing_header,
             vbeln TYPE vbrk-vbeln,   " Billing doc number
             netwr TYPE vbrk-netwr,   " Amount
             waerk TYPE vbrk-waerk,   " Currency
             sfakn TYPE vbrk-sfakn,   " Cancelled billing document number
             fksto TYPE vbrk-fksto,   " Billing document is canceled
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

    METHODS fetch_new_taler_cancel_bil_doc.

    METHODS post_created_orders_to_taler.

    METHODS update_taler_payment_uris.

    METHODS check_paid_orders.

    METHODS process_refunds.

    METHODS check_refunds.

    METHODS check_completed_orders.

    METHODS post_incoming_payment
      IMPORTING
        iv_billing_doc TYPE vbrk-vbeln          "billing doc (= invoice, open item)
        iv_wiref_id    TYPE char70              "wire-transfer reference / text
      RETURNING
        VALUE(rt_return) TYPE bapiret2_t.       "BAPI return table

    METHODS: log_http_interaction
      IMPORTING
        iv_billing_doc TYPE ztlr_order_log-billing_doc
        iv_http_code   TYPE i
        iv_response    TYPE string
        iv_req_body    TYPE string
        iv_req_url     TYPE string
        iv_on_state    TYPE ztlr_order_hstat-on_state.


    " DO NOT USE UNLESS UnitedResearchForever
    METHODS clear_all_taler_tables.



    METHODS get_billing_docs
      EXPORTING
        et_billing_headers TYPE ty_billing_header_tab
        et_billing_items   TYPE ty_billing_item_tab.

    METHODS: check_if_order_exists
      IMPORTING
        iv_billing_doc   TYPE vbrk-vbeln
      RETURNING
        VALUE(rv_exists) TYPE abap_bool.

    METHODS: insert_order_to_taler_tables
      IMPORTING
        is_header TYPE ty_billing_header
        it_items  TYPE ty_billing_item_tab
        iv_state  TYPE ztlr_order_log-state.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS raise_note
      IMPORTING
        iv_type TYPE char20
        iv_short TYPE char1024_cs
        iv_long  TYPE dstring.

    "---------------------------------------------------------------
    " HTTP helpers for cancel / refund
    "---------------------------------------------------------------
    METHODS send_delete_to_taler
      IMPORTING iv_billing_doc TYPE vbrk-vbeln
      RETURNING VALUE(rv_code) TYPE i.

    METHODS send_refund_request_to_taler
      IMPORTING iv_billing_doc TYPE vbrk-vbeln
                iv_req_body    TYPE string
      RETURNING VALUE(rv_code) TYPE i.



ENDCLASS.



CLASS zcl_taler_order IMPLEMENTATION.

  METHOD raise_note.
    DATA(lo_gen) = NEW zcl_taler_general( ).
    lo_gen->add_notification(
      iv_system_part   = 'order'    " 'inventory'
      iv_not_type      = iv_type        " 'info' | 'warning' | 'error'
      iv_short_message = iv_short
      iv_long_message  = iv_long ).
  ENDMETHOD.

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
    DATA: lv_amount   TYPE netwr,
          lv_currency TYPE waerk,
          lt_products TYPE ty_product_list_tab,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

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

    WRITE: / 'summary:', ls_config-def_orded_desc.
    WRITE: / 'order_id:', iv_sales_order.
    WRITE: / 'amount:', lv_currency, ':', lv_amount.
    WRITE: / 'products:'.

    LOOP AT lt_products INTO DATA(ls_product).
      WRITE: / ls_product-product_id.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_order_json.
    DATA: lv_amount        TYPE netwr,
          lv_tax_amount    TYPE mwsbp,
          lv_currency      TYPE waerk,
          lv_total_amount  TYPE p DECIMALs 2,
          lt_products      TYPE ty_product_list_tab,
          lt_json_products TYPE STANDARD TABLE OF string WITH EMPTY KEY,
          lv_product       TYPE string,
          rv_line          TYPE string,
          ls_config   TYPE ztlr_config,
          lv_long_message  TYPE string.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    " 1) Read basic info from ZTLR_ORDER_LOG
    SELECT SINGLE amount, tax_amount, currency
        INTO (@lv_amount, @lv_tax_amount, @lv_currency)
        FROM ztlr_order_log
        WHERE billing_doc = @iv_billing_doc.

    IF sy-subrc <> 0 OR lv_amount IS INITIAL.
      CONCATENATE
        '{ "billing_doc":"-",'
        '"type":"error",'
        '"information":"Billing doc ' iv_sales_order ' not found"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |BD { iv_sales_order } – not found|
        iv_long  = lv_long_message ).
      RETURN.
    ENDIF.

    " 2) Read product list from ZTLR_ORDER_PROD
    " TODO: Low Priority
    " Could be updated, when Taler properly supports the quantity, to also fetch quantity
    SELECT material_number, material_desc
        INTO TABLE @lt_products
        FROM ztlr_order_prod
    WHERE sales_order = @iv_sales_order.

    IF lt_products IS INITIAL.
      CONCATENATE
        '{ "billing_doc":"-",'
        '"type":"error",'
        '"information":"Billing doc ' iv_sales_order ' products not found"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |BD { iv_sales_order } – prod not found|
        iv_long  = lv_long_message ).
      RETURN.
    ENDIF.

    " Build product JSON array
    LOOP AT lt_products INTO DATA(ls_product).
      lv_product = '{ "product_id": "' && ls_product-product_id && '", "description":"' && ls_config-def_prod_desc && '" }'.
      "lv_product = '{ "product_id": "' && ls_product-product_id && '"}'.
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
    lv_total_amount = lv_amount + lv_tax_amount.
    DATA(lv_amount_str) = |{ lv_currency }:{ lv_total_amount }|.
    IF ls_config-taler_cur_repl IS NOT INITIAL.
      REPLACE lv_currency IN lv_amount_str WITH ls_config-taler_cur_repl.
    ENDIF.


    " Final JSON
    rv_json = '{"order": {' &&
     '"summary": "' && ls_config-def_orded_desc && '",' &&
     '"order_id": "' && iv_billing_doc && '",' &&
     '"amount": "' && lv_amount_str && '",' &&
     '"products": [' && lv_products_json && ']' &&
     '}}'.


  ENDMETHOD.


  METHOD send_order_to_taler.

    DATA: lv_json          TYPE string,
          lo_http_client   TYPE REF TO if_http_client,
          lv_add_url       TYPE string VALUE 'orders',
          lv_url           TYPE string,
          lv_add_token     TYPE string VALUE 'Bearer secret-token:',
          lv_token         TYPE string,
          lv_status_code   TYPE i,
          lv_status_code_c TYPE c,
          lv_response_body TYPE string,
          ls_config   TYPE ztlr_config,
          lv_long_message  TYPE string.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    lv_url = ls_config-taler_req_uri && lv_add_url.
    lv_token = lv_add_token && ls_config-taler_password.


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
        OTHERS             = 4.

    IF sy-subrc <> 0 OR lo_http_client IS INITIAL.
      CONCATENATE
        '{ "billing_doc":"' iv_billing_doc '",'
        '"type":"error",'
        '"information":"HTTP client init failed"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |{ iv_billing_doc } – client init failed|
        iv_long  = lv_long_message ).
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
        OTHERS                     = 4.

    IF sy-subrc <> 0.
      CONCATENATE
        '{ "billing_doc":"' iv_billing_doc '",'
        '"type":"error",'
        '"information":"HTTP send/receive failed"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |{ iv_billing_doc } – send/recv error|
        iv_long  = lv_long_message ).
      rv_result-content = 'HTTP send/receive failed.'.
      RETURN.
    ENDIF.

    " Receive the response
    CALL METHOD lo_http_client->receive
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4.

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
    lv_status_code_c = lv_status_code.

    IF lv_status_code >= 400.

      CONCATENATE
        '{ "billing_doc":"' iv_billing_doc '",'
        '"type":"error",'
        '"information":"HTTP ' lv_status_code_c ' : body: ' lv_response_body '"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |{ iv_billing_doc } – HTTP { lv_status_code }|
        iv_long  = lv_long_message ).

    ELSEIF lv_status_code = 200.

      CONCATENATE
        '{ "billing_doc":"' iv_billing_doc '",'
        '"type":"info",'
        '"information":"order posted – HTTP 200"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'info'
        iv_short = |{ iv_billing_doc } – order posted|
        iv_long  = lv_long_message ).
    ENDIF.

    " Return result
    rv_result-code     = lv_status_code.
    rv_result-content  = lv_response_body.
    rv_result-req_body = lv_json.

  ENDMETHOD.


  METHOD get_billing_docs.
    DATA: lt_headers    TYPE ty_billing_header_tab,
          lt_items_temp TYPE STANDARD TABLE OF ty_billing_item WITH EMPTY KEY,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    CLEAR: et_billing_headers, et_billing_items.

    " 1) Fetch all billing headers from VBRK matching the filters
    SELECT vbeln,
           netwr,
           waerk,
           sfakn,
           fksto
      FROM vbrk
      INTO TABLE @lt_headers
     WHERE zlsch = @ls_config-sap_pay_method
       AND land1 = @ls_config-sap_country
       AND vkorg = @ls_config-sap_sales_org.

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
          ls_order_prod TYPE ztlr_order_prod,
          lv_timestamp  TYPE timestamp,
          lv_erdat TYPE vbak-erdat,
          lv_erzet TYPE vbak-erzet,
          lv_tzone         TYPE ttzz-tzone,
          lv_sap_created_ts_conv TYPE timestamp,
          lv_long_message TYPE string,
          lv_tax_amount TYPE mwsbp.


    " Fill and insert ZTLR_ORDER_LOG
    ls_order_log-billing_doc = is_header-vbeln.
    ls_order_log-sales_order = is_header-vgbel.
    ls_order_log-currency    = is_header-waerk.
    ls_order_log-amount      = is_header-netwr.
    ls_order_log-state       = iv_state.
    GET TIME STAMP FIELD lv_timestamp.
    ls_order_log-timestamp   = lv_timestamp.

    " Get tax amount (sum of MWSBP from VBAP for sales order)
    SELECT SUM( mwsbp ) INTO @lv_tax_amount
      FROM vbrp
      WHERE vbeln = @is_header-vbeln.

    IF sy-subrc <> 0 OR lv_tax_amount IS INITIAL.
      lv_tax_amount = 0.
    ENDIF.

    ls_order_log-tax_amount = lv_tax_amount.

    SELECT SINGLE erdat, erzet
      INTO (@lv_erdat, @lv_erzet)
      FROM vbak
      WHERE vbeln = @is_header-vgbel.

    DATA(lv_sap_created_ts) = |{ lv_erdat }{ lv_erzet }|. " Format: YYYYMMDDHHMMSS

    lv_tzone = 'CET'.

    " Convert to timestamp
    CONVERT DATE lv_erdat TIME lv_erzet
        INTO TIME STAMP lv_sap_created_ts_conv
        TIME ZONE lv_tzone.

    ls_order_log-sap_timestamp = lv_sap_created_ts_conv.


    INSERT ztlr_order_log FROM @ls_order_log.
    IF sy-subrc <> 0.
      CONCATENATE
        '{ "billing_doc":"' is_header-vbeln '",'
        '"type":"error",'
        '"information":"Insert into ZTLR_ORDER_LOG failed"}'
        INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |{ is_header-vbeln } – header insert failed|
        iv_long  = lv_long_message ).
      RETURN.
    ENDIF.

    " Loop over items and insert into ZTLR_ORDER_PROD
    LOOP AT it_items INTO DATA(ls_item).

      " TODO: Low Priority
      " Maybe we would like to fetch the description_i18n here too...
      " Try to get the description from MAKT
      DATA(lv_maktx) = ||.
      SELECT SINGLE maktx
          INTO @lv_maktx
          FROM makt
          WHERE matnr = @ls_item-matnr
              AND spras = @sy-langu.

      " If no description found, use fallback
      IF lv_maktx IS INITIAL.
        lv_maktx = |{ ls_item-matnr }: product from SAP|.
      ENDIF.

      CLEAR ls_order_prod.
      ls_order_prod-sales_order     = ls_item-vgbel.
      ls_order_prod-material_number = ls_item-matnr.
      ls_order_prod-material_desc   = lv_maktx.
      ls_order_prod-quantity        = ls_item-fkimg.
      ls_order_prod-units           = ls_item-meins.

      INSERT ztlr_order_prod FROM @ls_order_prod.
      IF sy-subrc <> 0.
        CONCATENATE
            '{ "billing_doc":"' ls_item-vbeln '",'
            '"type":"warning",'
            '"information":"Item ' ls_item-matnr ' not inserted"}'
            INTO lv_long_message.

        raise_note(
          iv_type  = 'warning'
          iv_short = |{ ls_item-vbeln } – item miss|
          iv_long  = lv_long_message ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD update_taler_billing_docs.

    " Fetching the taler billing docs
    CALL METHOD fetch_new_taler_billing_docs.

    " Fetching billing docs that were canceled
    CALL METHOD fetch_new_taler_cancel_bil_doc.

    " Add new function for posting the order to taler and getting back the results of it
    CALL METHOD post_created_orders_to_taler.

    " Check posted orders, and fetch the token to create the url
    CALL METHOD update_taler_payment_uris.

    " Check that the orders have been paid
    CALL METHOD check_paid_orders.

    " Just to cross check that the payment uri's have been updated if the repost happend
    CALL METHOD update_taler_payment_uris.

    " Initial refund logic
    CALL METHOD process_refunds.

    " Check for refund being callected.
    CALL METHOD check_refunds.

    " Change status to completed if cleared
    CALL METHOD check_completed_orders.

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

    " Delete settings
    DELETE FROM ztlr_config.
    IF sy-subrc = 0.
      WRITE: / 'ZTLR_CONFIG cleared.'.
    ENDIF.

    COMMIT WORK.

  ENDMETHOD.

  METHOD update_taler_payment_uris.

    DATA: lt_orders     TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
          ls_order      TYPE ztlr_order_log,
          lv_token_json TYPE string,
          lv_token      TYPE string,
          lv_pay_uri    TYPE string,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

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
      FIND REGEX '(?:"token"\s*:\s*"|token=)([^"&]+)' IN lv_token_json SUBMATCHES lv_token.

      IF lv_token IS INITIAL.
        CONTINUE. " Token not found in response
      ENDIF.

      " Construct the Taler pay URI
      CONCATENATE ls_config-taler_uri '/orders/' ls_order-billing_doc
                  '?token=' lv_token INTO lv_pay_uri.

      " Update the taler_pay_uri in the order log
      UPDATE ztlr_order_log
        SET taler_pay_uri = @lv_pay_uri,
            state         = 'posted_processed'
        WHERE billing_doc = @ls_order-billing_doc.

    ENDLOOP.

  ENDMETHOD.


  METHOD fetch_new_taler_billing_docs.

    DATA: lt_headers    TYPE ty_billing_header_tab,
          lt_items_temp TYPE ty_billing_item_tab,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    " Fetch billing headers from VBRK
    SELECT vbeln,
           netwr,
           waerk,
           sfakn,
           fksto
      INTO TABLE @lt_headers
      FROM vbrk
     WHERE zlsch = @ls_config-sap_pay_method
       AND land1 = @ls_config-sap_country
       AND vkorg = @ls_config-sap_sales_org.


    LOOP AT lt_headers INTO DATA(ls_header).

      " Skip if it already exists in the custom table
      IF me->check_if_order_exists( iv_billing_doc = ls_header-vbeln ) = abap_true.
        CONTINUE.
      ENDIF.

      " Skip if the amount is 0
      IF ls_header-netwr = 0.
        CONTINUE.
      ENDIF.

      " Skip if this billing doc cancels other one
      IF ls_header-sfakn IS NOT INITIAL.
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


  METHOD fetch_new_taler_cancel_bil_doc.

      DATA: ls_config      TYPE ztlr_config,
            lo_general     TYPE REF TO zcl_taler_general,
            lt_cancel_hdr  TYPE ty_billing_header_tab,
            ls_can         TYPE ty_billing_header,
            ls_log         TYPE ztlr_order_log,
            lv_long_msg    TYPE string.

      "--------------------------------------------------------------------
      " 0)  Read config once
      "--------------------------------------------------------------------
      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).

      "--------------------------------------------------------------------
      " 1)  Pick cancellation billing documents (FKSTO = 'X')
      "--------------------------------------------------------------------
      SELECT vbeln, netwr, waerk, sfakn, fksto
             FROM vbrk
             INTO TABLE @lt_cancel_hdr
             WHERE zlsch = @ls_config-sap_pay_method
               AND land1 = @ls_config-sap_country
               AND vkorg = @ls_config-sap_sales_org
               AND fksto = 'X'.          " <— cancellation flag

      "--------------------------------------------------------------------
      " 2)  Walk across the cancellations
      "--------------------------------------------------------------------
      LOOP AT lt_cancel_hdr INTO ls_can.

        " does that original exist in our own order log ?
        " it should yet better to cross check
        SELECT SINGLE *
          INTO @ls_log
          FROM ztlr_order_log
          WHERE billing_doc = @ls_can-vbeln
            AND refund = ''.  " make sense to filter out the one already put for refund

        IF sy-subrc <> 0.
          CONTINUE.                 " we never exported that one – ignore
        ENDIF.

        "----------------------------------------------------------------
        " 3)  Assign refund logic
        "----------------------------------------------------------------
        ls_log-refund = abap_true.

        CASE ls_log-state.

          WHEN 'created' OR 'error' OR space.
            ls_log-state = 'canceled'.                " → simply drop locally

          WHEN OTHERS.
            " keep state, just mark refund flag
        ENDCASE.

        "----------------------------------------------------------------
        " 4)  Persist
        "----------------------------------------------------------------
        UPDATE ztlr_order_log
          SET state  = @ls_log-state,
              refund = @ls_log-refund
          WHERE billing_doc = @ls_log-billing_doc.

        "----------------------------------------------------------------
        " 5)  Optional notification
        "----------------------------------------------------------------
        CONCATENATE
          '{ "billing_doc":"'  ls_log-billing_doc  '",'
          '"type":"info",'
          '"information":"cancel detected – new state '
          ls_log-state
          '"}'
          INTO lv_long_msg.

        raise_note(
          iv_type  = 'info'
          iv_short = |{ ls_log-billing_doc } – cancel handled|
          iv_long  = lv_long_msg ).

      ENDLOOP.

      COMMIT WORK AND WAIT.   " flush refund flags / notes

  ENDMETHOD.


  METHOD post_created_orders_to_taler.

    DATA: lt_created_orders TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
          ls_created_order  TYPE ztlr_order_log,
          lv_response       TYPE ty_http_result,
          ls_hist           TYPE ztlr_order_hstat,
          lv_timestamp      TYPE timestamp,
          lv_timestamp_str  TYPE string,
          lv_unique_id      TYPE string.

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
      lv_timestamp_str = |{ lv_timestamp }|.
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
        UPDATE ztlr_order_log SET taler_state = 'unpaid' WHERE billing_doc = @ls_created_order-billing_doc.
      ELSE.
        " TODO: LOW PRIORITY
        " ADD INFO TO NOTIFICATION TABLE
        UPDATE ztlr_order_log SET error = 'X' WHERE billing_doc = @ls_created_order-billing_doc.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_paid_orders.

    DATA: lt_orders      TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
          ls_order       TYPE ztlr_order_log,
          lo_http_client TYPE REF TO if_http_client,
          lv_url         TYPE string,
          lv_token       TYPE string,
          lv_response    TYPE string,
          lv_status      TYPE string,
          lv_code        TYPE i,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    CONCATENATE 'Bearer secret-token:' ls_config-taler_password INTO lv_token.

    " Get all posted_processed orders
    SELECT * FROM ztlr_order_log
      INTO TABLE @lt_orders
      WHERE state = 'posted_processed'.

    LOOP AT lt_orders INTO ls_order.

      " Build the callback URL
      lv_url = |{ ls_config-taler_req_uri }orders/{ ls_order-billing_doc }|.

      " Create HTTP client
      CALL METHOD cl_http_client=>create_by_url
        EXPORTING
          url    = lv_url
        IMPORTING
          client = lo_http_client
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc <> 0 OR lo_http_client IS INITIAL.
        CONTINUE.
      ENDIF.

      lo_http_client->request->set_method( if_http_request=>co_request_method_get ).
      lo_http_client->request->set_header_field( name = 'Authorization'
                                                 value = lv_token ).

      " Send request
      CALL METHOD lo_http_client->send
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " Receive response
      CALL METHOD lo_http_client->receive
        EXCEPTIONS
          OTHERS = 1.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CALL METHOD lo_http_client->response->get_status
        IMPORTING
          code = lv_code.

      lv_response = lo_http_client->response->get_cdata( ).

      " Save the info to the transaction table
      CALL METHOD log_http_interaction
        EXPORTING
          iv_billing_doc = ls_order-billing_doc
          iv_http_code   = lv_code
          iv_response    = lv_response
          iv_req_body    = ''
          iv_req_url     = lv_url
          iv_on_state    = 'check_paid'.


      IF lv_code = 200.

        " Extract the order_status using regex
        FIND REGEX '"order_status"\s*:\s*"([^"]+)"' IN lv_response SUBMATCHES lv_status.

        IF lv_status = 'claimed'.
          UPDATE ztlr_order_log SET taler_state = 'claimed' WHERE billing_doc = @ls_order-billing_doc.
        ENDIF.

        IF lv_status = 'paid'.
          UPDATE ztlr_order_log SET state = 'paid' WHERE billing_doc = @ls_order-billing_doc.
          UPDATE ztlr_order_log SET taler_state = 'paid' WHERE billing_doc = @ls_order-billing_doc.
        ENDIF.

      ENDIF.

    "------------------------------------------------------------------
    " 404 – order vanished from Taler
    "------------------------------------------------------------------
    IF lv_code = 404.

      "------------------------------------------------------------------
      " A) The order had been cancelled locally (refund flag already set)
      "------------------------------------------------------------------
      IF ls_order-refund = abap_true.

        UPDATE ztlr_order_log
          SET state = 'canceled'
          WHERE billing_doc = @ls_order-billing_doc.

        CONCATENATE
          '{ "billing_doc":"' ls_order-billing_doc '",'
          '"type":"info",'
          '"information":"404 received + cancel billing doc present – state → canceled"}'
          INTO lv_response.

        raise_note(
          iv_type  = 'info'
          iv_short = |{ ls_order-billing_doc } – auto-canceled (404)|
          iv_long  = lv_response ).

      "------------------------------------------------------------------
      " B) Normal case – we need to recreate the order in Taler
      "------------------------------------------------------------------
      ELSE.

        DATA(lv_repost) = me->send_order_to_taler(
                            iv_sales_order = ls_order-sales_order
                            iv_billing_doc = ls_order-billing_doc ).

        " log the HTTP result
        CALL METHOD log_http_interaction
          EXPORTING
            iv_billing_doc = ls_order-billing_doc
            iv_http_code   = lv_repost-code
            iv_response    = lv_repost-content
            iv_req_body    = lv_repost-req_body
            iv_req_url     = lv_url
            iv_on_state    = 'repost'.

        IF lv_repost-code = 200.

          UPDATE ztlr_order_log
            SET state       = 'posted',
                taler_state = 'unpaid',
                error       = ''
            WHERE billing_doc = @ls_order-billing_doc.

          CONCATENATE
            '{ "billing_doc":"' ls_order-billing_doc '",'
            '"type":"info",'
            '"information":"reposted – HTTP 200"}'
            INTO lv_response.

          raise_note(
            iv_type  = 'info'
            iv_short = |{ ls_order-billing_doc } – reposted OK|
            iv_long  = lv_response ).

        ELSE.

          UPDATE ztlr_order_log
            SET error = 'X'
            WHERE billing_doc = @ls_order-billing_doc.

          DATA lv_repost_c TYPE c.
          lv_repost_c = lv_repost-code.

          CONCATENATE
            '{ "billing_doc":"' ls_order-billing_doc '",'
            '"type":"error",'
            '"information":"repost failed – HTTP ' lv_repost_c '"}'
            INTO lv_response.

          raise_note(
            iv_type  = 'error'
            iv_short = |{ ls_order-billing_doc } – repost HTTP { lv_repost-code }|
            iv_long  = lv_response ).

        ENDIF.

      ENDIF.  "refund flag
    ENDIF.     "404

    ENDLOOP.

  ENDMETHOD.

  METHOD process_refunds.

    DATA: lt_refund TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
          lo_general TYPE REF TO zcl_taler_general,
          ls_config  TYPE ztlr_config,
            ls_ref    TYPE ztlr_order_log,
            lv_code   TYPE i,
            lv_code_c TYPE c,
            lv_resp   TYPE string,
            lv_msg    TYPE string,
            lv_total_amount TYPE  p DECIMALs 2,
            lv_req_body TYPE string.


      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).

      "---------------------------------------------------------------
      " 1) pick candidates: refund flag = X, but not yet finalised
      "---------------------------------------------------------------
      SELECT *
        INTO TABLE @lt_refund
        FROM ztlr_order_log
        WHERE refund = 'X'
          AND state  NOT IN ( 'canceled',
                              'refunded',
                              'refunding',
                              'completed' ).

      LOOP AT lt_refund INTO ls_ref.

        CASE ls_ref-state.
          "==========================================
          " A. order never paid → try DELETE
          "==========================================
          WHEN 'posted_processed' OR 'posted'.

            lv_code = send_delete_to_taler( iv_billing_doc = ls_ref-billing_doc ).

            " --- happy path -------------------------------------------------
            IF lv_code = 204.

              UPDATE ztlr_order_log
                SET state = 'canceled'
                WHERE billing_doc = @ls_ref-billing_doc.

              CONCATENATE
                '{ "billing_doc":"' ls_ref-billing_doc '",'
                '"type":"info","information":"refund DELETE 204 – state→canceled"}'
                INTO lv_msg.

              raise_note( iv_type = 'info'
                          iv_short = |{ ls_ref-billing_doc } – refund DELETE 204|
                          iv_long = lv_msg ).

            " --- order is paid/claimed (40x) → fall-through to refund ------
            ELSEIF lv_code BETWEEN 400 AND 499.

              ls_ref-state = 'paid'.   " pretend paid → let next WHEN run again

            ELSE.                       " other errors
              UPDATE ztlr_order_log
                SET state  = 'refund_f',
                    error  = 'X'
                WHERE billing_doc = @ls_ref-billing_doc.

              lv_code_c = lv_code.

              CONCATENATE
                '{ "billing_doc":"' ls_ref-billing_doc '",'
                '"type":"error","information":"refund DELETE HTTP ' lv_code_c '"}'
                INTO lv_msg.

              raise_note( iv_type  = 'error'
                          iv_short = |{ ls_ref-billing_doc } – refund DELETE { lv_code }|
                          iv_long  = lv_msg ).
              CONTINUE.
            ENDIF.

          "==========================================
          " B. order paid/claimed → POST /refund
          "==========================================
          WHEN 'paid' OR 'refund_fail'.

            lv_total_amount = ls_ref-amount + ls_ref-tax_amount.
            DATA(lv_amount_str) = |{ ls_ref-currency }:{ ls_ref-amount }|.
            IF ls_config-taler_cur_repl IS NOT INITIAL.
              REPLACE ls_ref-currency IN lv_amount_str WITH ls_config-taler_cur_repl.
            ENDIF.

            CONCATENATE '{ "refund":"' lv_amount_str '", "reason":"refund from sap" }' INTO lv_req_body.

            lv_code = send_refund_request_to_taler( iv_billing_doc = ls_ref-billing_doc
                                                    iv_req_body    = lv_req_body ).

            IF lv_code = 200.

              UPDATE ztlr_order_log
                SET state = 'refunding'
                WHERE billing_doc = @ls_ref-billing_doc.

              lv_code_c = lv_code.

              CONCATENATE
                '{ "billing_doc":"' ls_ref-billing_doc '",'
                '"type":"info","information":"refund POST ' lv_code_c ' – state→refunding"}'
                INTO lv_msg.

              raise_note( iv_type  = 'info'
                          iv_short = |{ ls_ref-billing_doc } – refund POST { lv_code }|
                          iv_long  = lv_msg ).

            ELSE.

              UPDATE ztlr_order_log
                SET state  = 'refund_fail',
                    error  = 'X'
                WHERE billing_doc = @ls_ref-billing_doc.

              lv_code_c = lv_code.

              CONCATENATE
                '{ "billing_doc":"' ls_ref-billing_doc '",'
                '"type":"error","information":"refund POST fail – HTTP ' lv_code_c '"}'
                INTO lv_msg.

              raise_note( iv_type  = 'error'
                          iv_short = |{ ls_ref-billing_doc } – refund POST { lv_code }|
                          iv_long  = lv_msg ).

            ENDIF.

        ENDCASE.

      ENDLOOP.

      COMMIT WORK AND WAIT.

  ENDMETHOD.


  METHOD check_refunds.
  "--------------------------------------------------------------------
  " Check whether previously-requested refunds are now completed
  "--------------------------------------------------------------------

      DATA: lt_refunding TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
            ls_ref       TYPE ztlr_order_log,
            lo_client    TYPE REF TO if_http_client,
            ls_config    TYPE ztlr_config,
            lo_general   TYPE REF TO zcl_taler_general,
            lv_url       TYPE string,
            lv_token     TYPE string,
            lv_resp      TYPE string,
            lv_code      TYPE i,
            lv_pending   TYPE string,
            lv_msg       TYPE string.

      "-------------------------------------------------------------
      " 0) read last good config and auth token
      "-------------------------------------------------------------
      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).
      CONCATENATE 'Bearer secret-token:' ls_config-taler_password INTO lv_token.

      "-------------------------------------------------------------
      " 1) pick candidates – state = 'refunding'
      "-------------------------------------------------------------
      SELECT *
        INTO TABLE @lt_refunding
        FROM ztlr_order_log
        WHERE state = 'refunding'.

      LOOP AT lt_refunding INTO ls_ref.

        CLEAR: lv_resp, lv_pending, lv_code.

        "-----------------------------------------------------------
        " 2) call GET /orders/{billing_doc}
        "-----------------------------------------------------------
        lv_url = |{ ls_config-taler_req_uri }orders/{ ls_ref-billing_doc }|.

        cl_http_client=>create_by_url( EXPORTING url = lv_url IMPORTING client = lo_client ).
        IF lo_client IS INITIAL.  CONTINUE.  ENDIF.

        lo_client->request->set_method( if_http_request=>co_request_method_get ).
        lo_client->request->set_header_field( name = 'Authorization' value = lv_token ).

        lo_client->send( ).  lo_client->receive( ).
        lo_client->response->get_status( IMPORTING code = lv_code ).
        lv_resp = lo_client->response->get_cdata( ).

        "-----------------------------------------------------------
        " 3) log the interaction
        "-----------------------------------------------------------
        log_http_interaction(
          iv_billing_doc = ls_ref-billing_doc
          iv_http_code   = lv_code
          iv_response    = lv_resp
          iv_req_body    = ''
          iv_req_url     = lv_url
          iv_on_state    = 'check_refund' ).

        "-----------------------------------------------------------
        " 4) analyse result
        "-----------------------------------------------------------
        IF lv_code = 200.

          "look for  "refund_pending": false
          FIND REGEX '"refund_pending"\s*:\s*(\w+)' IN lv_resp SUBMATCHES lv_pending.

          IF lv_pending = 'false'.

            UPDATE ztlr_order_log
              SET state = 'refunded',
                  taler_state = 'refunded'
              WHERE billing_doc = @ls_ref-billing_doc.

            CONCATENATE
              '{ "billing_doc":"' ls_ref-billing_doc '",'
              '"type":"info","information":"refund completed"}'
              INTO lv_msg.

            raise_note(
              iv_type  = 'info'
              iv_short = |{ ls_ref-billing_doc } – refund completed|
              iv_long  = lv_msg ).

          ENDIF.

        ENDIF.

      ENDLOOP.

      COMMIT WORK AND WAIT.

  ENDMETHOD.

  METHOD log_http_interaction.

    DATA: ls_hist          TYPE ztlr_order_hstat,
          lv_timestamp     TYPE timestamp,
          lv_timestamp_str TYPE string,
          lv_unique_id     TYPE string,
          lv_rand          TYPE i,
          lv_rand_c        TYPE c.

    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_min = 1
        ran_int_max = 999999
      IMPORTING
        ran_int     = lv_rand.

    GET TIME STAMP FIELD lv_timestamp.
    lv_timestamp_str = |{ lv_timestamp }|.
    lv_unique_id     = |{ lv_timestamp_str }_{ iv_billing_doc }_{ lv_rand }|.

    CLEAR ls_hist.
    ls_hist-entry_id       = lv_unique_id.
    ls_hist-billing_doc    = iv_billing_doc.
    ls_hist-on_state       = iv_on_state.
    ls_hist-http_code      = iv_http_code.
    ls_hist-http_response  = iv_response.
    ls_hist-req_body       = iv_req_body.
    ls_hist-req_url        = iv_req_url.
    ls_hist-timestamp      = lv_timestamp.

    INSERT ztlr_order_hstat FROM @ls_hist.

  ENDMETHOD.

  METHOD send_delete_to_taler.
      DATA: ls_config   TYPE ztlr_config,
            lo_general  TYPE REF TO zcl_taler_general,
            lo_client   TYPE REF TO if_http_client,
            lv_url      TYPE string,
            lv_code     TYPE i,
            lv_resp     TYPE string.

      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).

      lv_url = |{ ls_config-taler_req_uri }orders/{ iv_billing_doc }|.

      cl_http_client=>create_by_url( EXPORTING url = lv_url IMPORTING client = lo_client ).
      IF lo_client IS INITIAL.  rv_code = 900.  RETURN.  ENDIF.

      lo_client->request->set_method( 'DELETE' ).
      lo_client->request->set_header_field( name = 'Authorization'
                                            value = |Bearer secret-token:{ ls_config-taler_password }| ).
      lo_client->send( ).  lo_client->receive( ).
      lo_client->response->get_status( IMPORTING code = rv_code ).
      lv_resp = lo_client->response->get_cdata( ).

      log_http_interaction(
        iv_billing_doc = iv_billing_doc
        iv_http_code   = rv_code
        iv_response    = lv_resp
        iv_req_body    = ''         " none for DELETE
        iv_req_url     = lv_url
        iv_on_state    = '' ).
  ENDMETHOD.


  METHOD send_refund_request_to_taler.

      DATA: ls_config   TYPE ztlr_config,
            lo_general  TYPE REF TO zcl_taler_general,
            lo_client   TYPE REF TO if_http_client,
            lv_url      TYPE string,
            lv_resp    TYPE string.

      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).

      lv_url = |{ ls_config-taler_req_uri }orders/{ iv_billing_doc }/refund|.

      cl_http_client=>create_by_url( EXPORTING url = lv_url IMPORTING client = lo_client ).
      IF lo_client IS INITIAL.  rv_code = 900.  RETURN.  ENDIF.

      lo_client->request->set_method( if_http_request=>co_request_method_post ).
      lo_client->request->set_header_field( name = 'Authorization'
                                            value = |Bearer secret-token:{ ls_config-taler_password }| ).
      lo_client->request->set_cdata( iv_req_body ).

      lo_client->send( ).  lo_client->receive( ).
      lo_client->response->get_status( IMPORTING code = rv_code ).

      lv_resp = lo_client->response->get_cdata( ).

      log_http_interaction(
            iv_billing_doc = iv_billing_doc
            iv_http_code   = rv_code
            iv_response    = lv_resp
            iv_req_body    = iv_req_body
            iv_req_url     = lv_url
            iv_on_state    = '' ).
  ENDMETHOD.

  METHOD check_completed_orders.

      DATA: lt_candidates TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY,
            ls_order      TYPE ztlr_order_log,
            ls_config     TYPE ztlr_config,
            lo_general    TYPE REF TO zcl_taler_general,
            lv_belnr      TYPE bsad-belnr,
            lv_msg        TYPE string.

      "------------------------------------------------------------
      " 0)  Read last good config once (for BUKRS etc.)
      "------------------------------------------------------------
      lo_general = NEW zcl_taler_general( ).
      lo_general->get_last_correct_config( RECEIVING rs_config = ls_config ).

      "------------------------------------------------------------
      " 1)  Pick orders that Taler reports as *settled* but
      "     are still only at state = 'paid' in our table
      "------------------------------------------------------------
      SELECT *
        INTO TABLE @lt_candidates
        FROM ztlr_order_log
        WHERE taler_state = 'settled'
          AND state       = 'paid'
          AND refund      = ''            " skip anything under refund flow
          AND error       = ''.           " skip errored ones – will be retried later

      LOOP AT lt_candidates INTO ls_order.

        "----------------------------------------------------------
        " 2)  Was the FI open item really cleared?  -> BSAD
        "     XBLNR (reference) carries the billing-doc number
        "----------------------------------------------------------
        " TODO: bukrs clearly needs to be
        CLEAR lv_belnr.
        SELECT SINGLE belnr
          INTO @lv_belnr
          FROM bsad
          WHERE xblnr = @ls_order-billing_doc
            AND bukrs = 'DE00'.   " company code from config

        IF sy-subrc = 0.   "found → cleared!

          UPDATE ztlr_order_log
            SET state = 'completed'
            WHERE billing_doc = @ls_order-billing_doc.

          CONCATENATE
            '{ "billing_doc":"'  ls_order-billing_doc  '",'
            '"type":"info",'
            '"information":"cleared in BSAD – state→completed"}'
            INTO lv_msg.

          raise_note(
            iv_type  = 'info'
            iv_short = |{ ls_order-billing_doc } – completed|
            iv_long  = lv_msg ).

        ENDIF.

      ENDLOOP.

      COMMIT WORK AND WAIT.   " flush status updates / notes

  ENDMETHOD.


  METHOD post_incoming_payment.
  " WE REALLY NEED SOMEONE TO FIGURE OUT HOW THIS SUPPOSED TO WORK...

    DATA ls_bsid TYPE bsid.

    SELECT SINGLE kunnr,
                   wrbtr,
                   waers,
                   gjahr,
                   belnr
      INTO CORRESPONDING FIELDS OF @ls_bsid
      FROM bsid
      WHERE xblnr = @iv_billing_doc
        AND bukrs = 'DE00'.

    IF sy-subrc <> 0.
      rt_return = VALUE #( ( type = 'E'
                             id = 'ZR'
                             number = '001'
                             message = |No open item for XBLNR { iv_billing_doc }.| ) ).
      RETURN.
    ENDIF.

    DATA: ls_header TYPE bapiache09,
          lt_ar     TYPE TABLE OF bapiacar09,
          lt_gl     TYPE TABLE OF bapiacgl09,
          lt_curr   TYPE TABLE OF bapiaccr09.

    ls_header = VALUE #(
      bus_act     = 'RFBU'
      comp_code   = 'DE00'
      doc_date    = sy-datum
      pstng_date  = sy-datum
      vatdate     = sy-datum
      fis_period  = '06'
      fisc_year   = ls_bsid-gjahr
      doc_type    = 'DZ'
      ref_doc_no  = iv_wiref_id
      header_txt  = iv_wiref_id
      username    = sy-uname ).

    " Customer credit (implied by negative amount in currency)
    APPEND VALUE bapiacar09(
      itemno_acc   = '0000000001'
      customer     = ls_bsid-kunnr
      comp_code    = 'DE00'
      alloc_nmbr   = iv_wiref_id
      item_text    = iv_wiref_id
    ) TO lt_ar.

    " Bank clearing G/L account (positive value)
    APPEND VALUE bapiacgl09(
      itemno_acc   = '0000000002'
      gl_account   = '0000135001'
      comp_code    = 'DE00'
      alloc_nmbr   = iv_wiref_id
      item_text    = iv_wiref_id
    ) TO lt_gl.

    " Currency amounts (credit line must be negative)
    APPEND VALUE bapiaccr09(
      itemno_acc   = '0000000001'
      currency_iso = ls_bsid-waers
      amt_doccur   = - ls_bsid-wrbtr
    ) TO lt_curr.

    APPEND VALUE bapiaccr09(
      itemno_acc   = '0000000002'
      currency_iso = ls_bsid-waers
      amt_doccur   =   ls_bsid-wrbtr
    ) TO lt_curr.

    DATA lv_paydoc TYPE bapiache09-obj_key.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader    = ls_header
      IMPORTING
        obj_key           = lv_paydoc
      TABLES
        accountreceivable = lt_ar
        accountgl         = lt_gl
        currencyamount    = lt_curr
        return            = rt_return.

    IF line_exists( rt_return[ type = 'E' ] ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.

    " Clear via classic FI module
    DATA: lt_ftpost TYPE TABLE OF ftpost,
          ls_ftpost TYPE ftpost,
          lt_blntab TYPE TABLE OF blntab,
          ls_blntab TYPE blntab,
          lt_ftclear TYPE TABLE OF ftclear,
          lt_fttax TYPE TABLE OF fttax.

    CLEAR ls_blntab.
    ls_blntab-bukrs = 'DE00'.
    ls_blntab-gjahr = ls_bsid-gjahr.
    ls_blntab-belnr = lv_paydoc.
    APPEND ls_blntab TO lt_blntab.

    CLEAR ls_ftpost.
    ls_ftpost-stype = 'K'.                      "Line item
    ls_ftpost-count = 1.
    ls_ftpost-fnam  = 'BKPF-BUKRS'.
    ls_ftpost-fval  = 'DE00'.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam  = 'RF05A-BELNR'.
    ls_ftpost-fval  = ls_bsid-belnr.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam  = 'RF05A-GJAHR'.
    ls_ftpost-fval  = ls_bsid-gjahr.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam  = 'RF05A-BELNR'.
    ls_ftpost-fval  = lv_paydoc.
    APPEND ls_ftpost TO lt_ftpost.

    ls_ftpost-fnam  = 'RF05A-GJAHR'.
    ls_ftpost-fval  = ls_bsid-gjahr.
    APPEND ls_ftpost TO lt_ftpost.

    CALL FUNCTION 'POSTING_INTERFACE_CLEARING'
      EXPORTING
        i_auglv   = 'EINGZAHL'
        i_tcode   = 'FB05'
        i_sgfunct = 'C'
      TABLES
        t_ftpost  = lt_ftpost
        t_blntab  = lt_blntab
        t_ftclear = lt_ftclear
        t_fttax   = lt_fttax.


  ENDMETHOD.



ENDCLASS.
