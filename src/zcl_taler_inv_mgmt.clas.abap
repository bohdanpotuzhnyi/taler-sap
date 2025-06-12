*&---------------------------------------------------------------------*
*&  Class  ZCL_TALER_INV_MGMT
*&---------------------------------------------------------------------*
*&  - Keeps a local shadow table ZTLR_INVENTORY
*&  - Synchronises the shadow table with SAP stock (MARD / MBEW …)
*&  - Propagates changes to the GNU Taler back-end
*&
*&  Status life-cycle
*&    READY  → sent   → POSTED
*&    UPDATE → sent   → POSTED
*&    DELETE → sent   → DELETE_COMPLETED
*&---------------------------------------------------------------------*
CLASS zcl_taler_inv_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "------------------------------------------------------------------------
    " JSON-ready structure for Taler
    "------------------------------------------------------------------------
    TYPES: BEGIN OF ty_custom_json,
             product_id  TYPE matnr,
             description TYPE maktx,
             unit        TYPE meins,
             price       TYPE string,
             total_stock TYPE i,
           END OF ty_custom_json.

    "------------------------------------------------------------------------
    " Main entry points
    "------------------------------------------------------------------------
    METHODS get_material_data
      IMPORTING
        p_matnr TYPE char_40
        p_plant TYPE werks_d
        p_lgort TYPE lgort_d
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS sync_inventory
      IMPORTING
        p_plant TYPE werks_d
        p_lgort TYPE lgort_d.

  PRIVATE SECTION.
    "------------------------------------------------------------------------
    " Local shadow record
    "------------------------------------------------------------------------
    TYPES: BEGIN OF ty_inventory_data,
             product_id TYPE matnr,
             description TYPE maktx,
             unit        TYPE meins,
             price TYPE p LENGTH 8 DECIMALS 2,
             currency TYPE waers,
             stock TYPE labst,
             status TYPE char10,
           END   OF ty_inventory_data.

    "------------------------------------------------------------------------
    " DB helpers
    "------------------------------------------------------------------------
    METHODS save_or_update_inventory
      IMPORTING
        is_data TYPE ty_inventory_data
      RETURNING
        VALUE(rv_success) TYPE abap_bool.

    METHODS mark_inventory_as_posted
      IMPORTING
        iv_product_id TYPE matnr.

    "------------------------------------------------------------------------
    " HTTP helpers
    "------------------------------------------------------------------------
    METHODS log_inventory_http_interaction
      IMPORTING
        iv_product_id TYPE matnr
        iv_on_action  TYPE char20
        iv_http_code  TYPE int4
        iv_response   TYPE dstring
        iv_req_body   TYPE dstring
        iv_req_url    TYPE dstring.

    METHODS post_inventory_to_taler
      IMPORTING
        iv_json       TYPE string
        iv_product_id TYPE matnr
      RETURNING
        VALUE(rv_status_code) TYPE i.

    METHODS patch_inventory_to_taler
      IMPORTING
        iv_json       TYPE string
        iv_product_id TYPE matnr
      RETURNING
        VALUE(rv_status_code) TYPE i.

    METHODS delete_inventory_from_taler
      IMPORTING
        iv_product_id TYPE matnr
      RETURNING
        VALUE(rv_status_code) TYPE i.

    "------------------------------------------------------------------------
    " Sync helpers
    "------------------------------------------------------------------------
    METHODS build_json_for_inventory
      IMPORTING
        is_data TYPE ty_inventory_data
      RETURNING
        VALUE(rv_json) TYPE string.

    METHODS post_ready_products
      IMPORTING p_plant TYPE werks_d.

    METHODS patch_updated_products
      IMPORTING p_plant TYPE werks_d.

    METHODS delete_marked_products
      IMPORTING p_plant TYPE werks_d.

    METHODS cleanup_deleted_entries.

    METHODS send_http
      IMPORTING
        iv_method     TYPE string
        iv_url        TYPE string
        iv_json       TYPE string
        iv_product_id TYPE matnr
        iv_action     TYPE char20
      RETURNING
        VALUE(rv_status_code) TYPE i.

    METHODS raise_note
      IMPORTING
        iv_type TYPE char20
        iv_short TYPE char1024_cs
        iv_long  TYPE dstring.

ENDCLASS.



CLASS zcl_taler_inv_mgmt IMPLEMENTATION.

  METHOD raise_note.
    DATA(lo_gen) = NEW zcl_taler_general( ).
    lo_gen->add_notification(
      iv_system_part   = 'inventory'    " 'inventory'
      iv_not_type      = iv_type        " 'info' | 'warning' | 'error'
      iv_short_message = iv_short
      iv_long_message  = iv_long ).
  ENDMETHOD.


  METHOD get_material_data.
*---------------------------------------------------------------------*
* 1.  LOW-LEVEL : read one material and upsert local shadow table
*---------------------------------------------------------------------*

    "--------------------------------------------------------------
    " Local helper structure
    "--------------------------------------------------------------
    TYPES: BEGIN OF ty_material_info,
             material     TYPE matnr,
             description  TYPE maktx,
             base_unit    TYPE meins,
             price        TYPE p LENGTH 8 DECIMALS 2,
             currency     TYPE waers,
             stock        TYPE labst,
           END OF ty_material_info.

    DATA: lv_maktx     TYPE maktx,
          lv_base_unit TYPE meins,
          lv_return    TYPE bapireturn,
          lv_price_raw TYPE mbew-stprs,
          lv_bukrs     TYPE t001k-bukrs,
          lv_currency  TYPE t001-waers,
          lv_stock     TYPE mard-labst,
          rs_info      TYPE ty_material_info,
          ls_inv       TYPE ty_inventory_data,
          lv_long_message TYPE string.

    CLEAR rs_info.

    "--------------------------------------------------------------
    " 1.  General material data (MAKT + MARA)
    "--------------------------------------------------------------

    "--- fetch description ----------------------------------------
    SELECT SINGLE maktx
           INTO @lv_maktx
           FROM makt
           WHERE matnr = @p_matnr
             AND spras = @sy-langu.

    IF sy-subrc <> 0.                    " no MAKT entry for this language
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"error",`
        `"information":"MAKT entry not found"}` INTO lv_long_message.

      raise_note(
        iv_type ='error'
        iv_short = |{ p_matnr } – MAKT missing|
        iv_long  = lv_long_message ).

      RETURN.
    ENDIF.

    "--- base unit of measure -------------------------------------
    SELECT SINGLE meins
           INTO @lv_base_unit
           FROM mara
           WHERE matnr = @p_matnr.

    IF sy-subrc <> 0.                    " no MARA entry – should not happen
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"error",`
        `"information":"MARA entry not found"}` INTO lv_long_message.

      raise_note(
        iv_type ='error'
        iv_short = |{ p_matnr } – MARA missing|
        iv_long  = lv_long_message ).

      RETURN.
    ENDIF.

    "--- fill data ------------------------------------------------
    rs_info-material     = p_matnr.
    rs_info-description  = lv_maktx.
    rs_info-base_unit    = lv_base_unit.

    "--------------------------------------------------------------
    " 2.  Standard price (MBEW)
    "--------------------------------------------------------------
    SELECT SINGLE stprs
      INTO lv_price_raw
      FROM mbew
      WHERE matnr = p_matnr
        AND bwkey = p_plant.

    rs_info-price = lv_price_raw.

    IF sy-subrc <> 0.
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"warning",`
        `"information":"Standard price not found – STPRS = 0"}` INTO lv_long_message.

      raise_note(
        iv_type = 'warning'
        iv_short = |{ p_matnr } – price missing|
        iv_long  = lv_long_message ).
    ENDIF.

    "--------------------------------------------------------------
    " 3.  Currency (T001K→T001)
    "--------------------------------------------------------------
    SELECT SINGLE bukrs
      INTO lv_bukrs
      FROM t001k
      WHERE bwkey = p_plant.

    SELECT SINGLE waers
      INTO lv_currency
      FROM t001
      WHERE bukrs = lv_bukrs.

    IF sy-subrc <> 0.
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"warning",`
        `"information":"Currency not resolved for plant"}` INTO lv_long_message.

      raise_note(
        iv_type = 'warning'
        iv_short = |{ p_matnr } – currency missing|
        iv_long  = lv_long_message ).
    ENDIF.

    rs_info-currency = lv_currency.

    "--------------------------------------------------------------
    " 4.  Un-restricted stock (MARD)
    "--------------------------------------------------------------
    SELECT SINGLE labst
      INTO lv_stock
      FROM mard
      WHERE matnr = p_matnr
        AND werks = p_plant
        AND lgort = p_lgort.

    IF sy-subrc <> 0.
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"warning",`
        `"information":"No MARD stock row – stock set to 0"}` INTO lv_long_message.

      raise_note(
        iv_type = 'warning'
        iv_short = |{ p_matnr } – stock row missing|
        iv_long  = lv_long_message ).
    ENDIF.

    rs_info-stock = trunc( lv_stock ).

    "--------------------------------------------------------------
    " 5.  Build local shadow record
    "--------------------------------------------------------------
    ls_inv = VALUE ty_inventory_data(
                product_id  = rs_info-material
                description = rs_info-description
                price       = rs_info-price
                currency    = rs_info-currency
                stock       = rs_info-stock
                unit        = rs_info-base_unit
                status      = 'READY' ).

    DATA(lv_ok) = save_or_update_inventory( ls_inv ).
    IF lv_ok = abap_false.
      CONCATENATE
        `{ "product_id":"`  p_matnr `",`
        `"type":"error",`
        `"information":"Shadow table write failed"}` INTO lv_long_message.

      raise_note(
        iv_type = 'error'
        iv_short = |{ p_matnr } – cannot write shadow|
        iv_long  = lv_long_message ).
      RETURN.
    ENDIF.

    "--------------------------------------------------------------
    " 6.  Return JSON representation (convenience only)
    "--------------------------------------------------------------
    rv_json = build_json_for_inventory( ls_inv ).

  "get_material_data
  ENDMETHOD.

  METHOD sync_inventory.
*---------------------------------------------------------------------*
* 2.  PUBLIC  :  main synchronisation entry point
*---------------------------------------------------------------------*

    DATA: lt_sap_mat TYPE TABLE OF matnr WITH EMPTY KEY,
          lt_db_mat  TYPE TABLE OF ztlr_inventory-product_id WITH EMPTY KEY,
          lv_matnr   TYPE matnr,
          lv_long_message TYPE string.

    CONCATENATE '{ "product_id":"-", "type":"info",'
      '"information":"Sync started for plant' p_plant '/' p_lgort '"'
    INTO lv_long_message.

    raise_note(
        iv_type  = 'info'
        iv_short = |Sync started ({ p_plant }/{ p_lgort })|
        iv_long  = lv_long_message ).


    "--------------------------------------------------------------
    " 1.  Fetch current SAP stock snapshot for plant / lgort
    "--------------------------------------------------------------
    SELECT DISTINCT matnr
      INTO TABLE @lt_sap_mat
      FROM mard
      WHERE werks = @p_plant
        AND lgort = @p_lgort
        AND labst > 0.   " skip zero stock

    "--------------------------------------------------------------
    " 2.  Read shadow table once
    "--------------------------------------------------------------
    SELECT product_id
      INTO TABLE lt_db_mat
      FROM ztlr_inventory.

    " TODO: Check that it really works
    " SORT lt_sap_mat.
    " SORT lt_db_mat.

    "--------------------------------------------------------------
    " 3.  Detect NEW or CHANGED materials
    "--------------------------------------------------------------
    LOOP AT lt_sap_mat INTO lv_matnr.

      READ TABLE lt_db_mat WITH KEY table_line = lv_matnr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " → new product → insert (READY will be set in get_material_data)
        get_material_data(
          EXPORTING
            p_matnr = lv_matnr
            p_plant = p_plant
            p_lgort = p_lgort ).
      ELSE.
        " → exists: compare values – if changed mark UPDATE
        DATA(ls_db) = VALUE ztlr_inventory( ).
        SELECT SINGLE *
          INTO @ls_db
          FROM ztlr_inventory
          WHERE product_id = @lv_matnr.

        " Re-read current SAP values for comparison
        DATA(lv_json_dummy) = get_material_data(      " returns JSON but we only use shadow write
          EXPORTING
            p_matnr = lv_matnr
            p_plant = p_plant
            p_lgort = p_lgort ).

        " get_material_data already refreshed the shadow record;
        " check if something changed and status was POSTED → move to UPDATE
        IF ls_db-status = 'POSTED'.
          UPDATE ztlr_inventory
            SET status = 'UPDATE'
            WHERE product_id = @lv_matnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "--------------------------------------------------------------
    " 4.  Detect deletions (in DB but not in SAP snapshot)
    "--------------------------------------------------------------
    LOOP AT lt_db_mat INTO lv_matnr.
      READ TABLE lt_sap_mat WITH KEY table_line = lv_matnr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        UPDATE ztlr_inventory
          SET status = 'DELETE'
          WHERE product_id = @lv_matnr
            AND status <> 'DELETE_COMPLETED'.
      ENDIF.
    ENDLOOP.

    COMMIT WORK AND WAIT.

    "--------------------------------------------------------------
    " 5.  Propagate data to Taler
    "--------------------------------------------------------------
    post_ready_products( p_plant = p_plant ).
    patch_updated_products( p_plant = p_plant ).
    delete_marked_products( p_plant = p_plant ).

    "--------------------------------------------------------------
    " 6.  House-keeping
    "--------------------------------------------------------------
    cleanup_deleted_entries( ).

    CONCATENATE '{ "product_id":"-", "type":"info",'
      '"information":"Sync finished for plant' p_plant '/' p_lgort '"'
    INTO lv_long_message.

    raise_note(
        iv_type  = 'info'
        iv_short = |Sync finished ({ p_plant }/{ p_lgort })|
        iv_long  = lv_long_message ).

  "sync_inventory
  ENDMETHOD.

  METHOD save_or_update_inventory.
*---------------------------------------------------------------------*
* 3.  PRIVATE : upsert local shadow record
*---------------------------------------------------------------------*

    DATA: ls_db TYPE ztlr_inventory,
          lv_timestamp TYPE timestamp.

    GET TIME STAMP FIELD lv_timestamp.

    MOVE-CORRESPONDING is_data TO ls_db.
    ls_db-timestamp = lv_timestamp.

    MODIFY ztlr_inventory FROM ls_db.
    rv_success = xsdbool( sy-subrc = 0 ).

    IF rv_success = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.
  "save_or_update_inventory
  ENDMETHOD.


  METHOD build_json_for_inventory.
*---------------------------------------------------------------------*
* 4.  PRIVATE : build JSON for a single shadow record
*---------------------------------------------------------------------*
" TODO: Still need replace configuration values here for product description
    DATA: ls_json TYPE ty_custom_json,
          lo_ser  TYPE REF TO cl_fdt_json_serializer,
          ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.

    ls_json-product_id  = is_data-product_id.
    ls_json-description = is_data-description.
    ls_json-unit        = is_data-unit.

    DATA(lv_price) = |{ is_data-currency }:{ is_data-price }|.
    IF ls_config-taler_cur_repl IS NOT INITIAL.
        REPLACE is_data-currency IN lv_price WITH ls_config-taler_cur_repl.
    ENDIF.
    ls_json-price = lv_price.
    ls_json-total_stock = is_data-stock.

    lo_ser = NEW cl_fdt_json_serializer( ).
    rv_json = lo_ser->serialize( REF #( ls_json ) ).

    " Lower-case field names
    REPLACE ALL OCCURRENCES OF '"PRODUCT_ID"'  IN rv_json WITH '"product_id"'.
    REPLACE ALL OCCURRENCES OF '"DESCRIPTION"' IN rv_json WITH '"description"'.
    REPLACE ALL OCCURRENCES OF '"UNIT"'        IN rv_json WITH '"unit"'.
    REPLACE ALL OCCURRENCES OF '"PRICE"'       IN rv_json WITH '"price"'.
    REPLACE ALL OCCURRENCES OF '"TOTAL_STOCK"' IN rv_json WITH '"total_stock"'.
  "build_json_for_inventory
  ENDMETHOD.


  METHOD post_ready_products.
*---------------------------------------------------------------------*
* 5.  PRIVATE : propagate READY → POSTED
*---------------------------------------------------------------------*
    DATA: lt_ready TYPE TABLE OF ztlr_inventory WITH EMPTY KEY,
          lv_long_message TYPE string.

    SELECT *
      FROM ztlr_inventory
      INTO TABLE lt_ready
      WHERE status = 'READY'.

    LOOP AT lt_ready INTO DATA(ls_inv).

      DATA(lv_json) = build_json_for_inventory( CORRESPONDING ty_inventory_data( ls_inv ) ).
      DATA(lv_code) = post_inventory_to_taler(
                        iv_json       = lv_json
                        iv_product_id = ls_inv-product_id ).

      IF lv_code = 204.
        mark_inventory_as_posted( ls_inv-product_id ).

        CONCATENATE
          `{ "product_id":"` ls_inv-product_id `",`
          `"type":"info",`
          `"information":" post ok – HTTP 204"}`
        INTO lv_long_message.

        raise_note(
          iv_type  = 'info'
          iv_short = |{ ls_inv-product_id } – posted OK|
          iv_long  = lv_long_message ).
      ENDIF.

    ENDLOOP.
  "post_ready_products
  ENDMETHOD.


  METHOD patch_updated_products.
*---------------------------------------------------------------------*
* 6.  PRIVATE : propagate UPDATE → POSTED
*---------------------------------------------------------------------*
    DATA: lt_upd TYPE TABLE OF ztlr_inventory WITH EMPTY KEY,
          lv_long_message TYPE string.

    SELECT *
      FROM ztlr_inventory
      INTO TABLE lt_upd
      WHERE status = 'UPDATE'.

    LOOP AT lt_upd INTO DATA(ls_inv).

      DATA(lv_json) = build_json_for_inventory( CORRESPONDING ty_inventory_data( ls_inv ) ).
      DATA(lv_code) = patch_inventory_to_taler(
                        iv_json       = lv_json
                        iv_product_id = ls_inv-product_id ).

      IF lv_code = 204.
        mark_inventory_as_posted( ls_inv-product_id ).

        CONCATENATE
          `{ "product_id":"` ls_inv-product_id `",`
          `"type":"info",`
          `"information":" update ok – HTTP 204"}`
        INTO lv_long_message.

        raise_note(
          iv_type  = 'info'
          iv_short = |{ ls_inv-product_id } – updated OK|
          iv_long  = lv_long_message ).
      ENDIF.

    ENDLOOP.
  "patch_updated_products
  ENDMETHOD.


  METHOD delete_marked_products.
*---------------------------------------------------------------------*
* 7.  PRIVATE : propagate DELETE → DELETE_COMPLETED
*---------------------------------------------------------------------*
    DATA: lt_del TYPE TABLE OF ztlr_inventory WITH EMPTY KEY,
          lv_long_message TYPE string.

    SELECT *
      FROM ztlr_inventory
      INTO TABLE lt_del
      WHERE status = 'DELETE'.

    LOOP AT lt_del INTO DATA(ls_inv).

      DATA(lv_code) = delete_inventory_from_taler( ls_inv-product_id ).

      IF lv_code = 204.
        UPDATE ztlr_inventory
          SET status = 'DELETE_COMPLETED'
          WHERE product_id = @ls_inv-product_id.

        CONCATENATE
          `{ "product_id":"` ls_inv-product_id `",`
          `"type":"info",`
          `"information":" delete ok – HTTP 204"}`
        INTO lv_long_message.

        raise_note(
          iv_type  = 'info'
          iv_short = |{ ls_inv-product_id } – delete OK|
          iv_long  = lv_long_message ).
      ENDIF.

    ENDLOOP.
    COMMIT WORK AND WAIT.
  "delete_marked_products
  ENDMETHOD.


  METHOD cleanup_deleted_entries.
*---------------------------------------------------------------------*
* 8.  PRIVATE : purge entries older than 1 day with DELETE_COMPLETED
*---------------------------------------------------------------------*
    DATA: lv_cutoff TYPE timestamp,
          lv_current_timestamp TYPE timestamp.

    GET TIME STAMP FIELD lv_current_timestamp.

    " 24 h ago
    lv_cutoff = cl_abap_tstmp=>subtractsecs(
                  tstmp     = lv_current_timestamp
                  secs   = 86400 ).

    DELETE FROM ztlr_inventory
      WHERE status    = 'DELETE_COMPLETED'
        AND timestamp <  @lv_cutoff.

    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  "cleanup_deleted_entries
  ENDMETHOD.



  METHOD mark_inventory_as_posted.
*---------------------------------------------------------------------*
* 9.  PRIVATE : mark as POSTED helper
*---------------------------------------------------------------------*
    DATA: lv_ts TYPE timestamp.
    GET TIME STAMP FIELD lv_ts.

    UPDATE ztlr_inventory
      SET status    = 'POSTED',
          timestamp = @lv_ts
      WHERE product_id = @iv_product_id.

    COMMIT WORK AND WAIT.
  "mark_inventory_as_posted
  ENDMETHOD.


  METHOD post_inventory_to_taler.
*---------------------------------------------------------------------*
* 10. HTTP : POST /private/products
*---------------------------------------------------------------------*
    DATA ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.
    DATA(lv_url) = |{ ls_config-taler_req_uri }products|.
    DATA(lv_code) = me->send_http(
                      iv_method     = if_http_request=>co_request_method_post
                      iv_url        = lv_url
                      iv_json       = iv_json
                      iv_product_id = iv_product_id
                      iv_action     = 'create' ).
    rv_status_code = lv_code.
  "post_inventory_to_taler
  ENDMETHOD.


  METHOD patch_inventory_to_taler.
*---------------------------------------------------------------------*
* 11. HTTP : PATCH /private/products/{id}
*---------------------------------------------------------------------*
    DATA ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.
    DATA(lv_url)  = |{ ls_config-taler_req_uri }products/{ iv_product_id }|.
    DATA(lv_code) = me->send_http(
                      iv_method     = 'PATCH'
                      iv_url        = lv_url
                      iv_json       = iv_json
                      iv_product_id = iv_product_id
                      iv_action     = 'update' ).
    rv_status_code = lv_code.
  "patch_inventory_to_taler
  ENDMETHOD.


  METHOD delete_inventory_from_taler.
*---------------------------------------------------------------------*
* 12. HTTP : DELETE /private/products/{id}
*---------------------------------------------------------------------*
    DATA ls_config   TYPE ztlr_config.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.
    DATA(lv_url)  = |{ ls_config-taler_req_uri }products/{ iv_product_id }|.
    DATA(lv_code) = me->send_http(
                      iv_method     = 'DELETE'
                      iv_url        = lv_url
                      iv_json       = ''
                      iv_product_id = iv_product_id
                      iv_action     = 'delete' ).
    rv_status_code = lv_code.
  "delete_inventory_from_taler
  ENDMETHOD.


  METHOD send_http.
*---------------------------------------------------------------------*
* 13.  HTTP low-level helper (POST/PATCH/DELETE)
*---------------------------------------------------------------------*

    """"" Re-usable low level HTTP helper  """""
    """"" Parameters  """""
    """"   iv_method     : 'POST' / 'PATCH' / 'DELETE'
    """"   iv_url        : target URL
    """"   iv_json       : body (omit for DELETE)
    """"   iv_product_id : material number – for logging
    """"   iv_action     : 'create'/'update'/'delete' – for logging
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    DATA: lo_client   TYPE REF TO if_http_client,
          lv_auth_hdr TYPE string,
          lv_code     TYPE i,
          lv_code_c   TYPE c,
          lv_resp     TYPE string,
          ls_config   TYPE ztlr_config,
          lv_long_message TYPE string.

    DATA(lo_general)  = NEW zcl_taler_general( ).

    CALL METHOD lo_general->get_last_correct_config
      RECEIVING
        rs_config = ls_config.


    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url    = iv_url
      IMPORTING
        client = lo_client
      EXCEPTIONS
        others = 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_auth_hdr = |Bearer secret-token:{ ls_config-taler_password }|.

    lo_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_client->request->set_header_field( name = 'Authorization' value = lv_auth_hdr ).
    lo_client->request->set_method( iv_method ).

    IF iv_method = if_http_request=>co_request_method_post
    OR iv_method = 'PATCH'.
      lo_client->request->set_cdata( iv_json ).
    ENDIF.

    lo_client->send( ).
    lo_client->receive( ).

    lo_client->response->get_status( IMPORTING code = lv_code ).
    lv_resp = lo_client->response->get_cdata( ).

    log_inventory_http_interaction(
      iv_product_id = iv_product_id
      iv_on_action  = iv_action
      iv_http_code  = lv_code
      iv_response   = lv_resp
      iv_req_body   = iv_json
      iv_req_url    = iv_url ).

    IF lv_code >= 400.

      lv_code_c = lv_code.

      CONCATENATE
        `{ "product_id":"` iv_product_id `",`
        `"type":"error",`
        '"information":"HTTP ' lv_code_c ' – ' lv_resp '"}'
      INTO lv_long_message.

      raise_note(
        iv_type  = 'error'
        iv_short = |{ iv_product_id } – HTTP ` && lv_code|
        iv_long  = lv_long_message ).

    ENDIF.

    rv_status_code = lv_code.
  "send_http   (private helper)
  ENDMETHOD.


  METHOD log_inventory_http_interaction.
*---------------------------------------------------------------------*
* 14.  HTTP interaction history
*---------------------------------------------------------------------*

    DATA: ls_hist TYPE ztlr_inv_hstat,
          lv_ts   TYPE timestamp.

    GET TIME STAMP FIELD lv_ts.

    ls_hist-entry_id      = |{ lv_ts TIMESTAMP = ISO }_{ iv_product_id }|.
    ls_hist-mandt         = sy-mandt.
    ls_hist-product_id    = iv_product_id.
    ls_hist-on_action     = iv_on_action.
    ls_hist-http_code     = iv_http_code.
    ls_hist-http_response = iv_response.
    ls_hist-timestamp     = lv_ts.
    ls_hist-req_body      = iv_req_body.
    ls_hist-req_url       = iv_req_url.

    INSERT ztlr_inv_hstat FROM ls_hist.
  "log_inventory_http_interaction
  ENDMETHOD.

ENDCLASS.
