*&---------------------------------------------------------------------*
*& Class zcl_taler_inv_mgmt
*&---------------------------------------------------------------------*
*& Get json structure like
*& { "product_id":"TALER_BOTTLE01",
*& "description":"Taler Branded Water Bottle",
*& "unit":"ST",
*& "price":"EUR:45.00",
*& "total_stock":1000 }
*& from SAP material by material n number, plant and storage location.
*& Make POST request to Taler backend and output response code
*&---------------------------------------------------------------------*


CLASS zcl_taler_inv_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

     " JSON-ready structure
     TYPES: BEGIN OF ty_custom_json,
         product_id   TYPE char18,
         description  TYPE maktx,
         unit         TYPE meins,
         price        TYPE string,
         total_stock  TYPE i,
       END OF ty_custom_json.



    METHODS get_material_data
      IMPORTING
        p_matnr TYPE char18
        p_plant TYPE char18
        p_lgort TYPE lgort_d
      RETURNING
        VALUE(rv_json) TYPE string.

ENDCLASS.



CLASS zcl_taler_inv_mgmt IMPLEMENTATION.

  METHOD get_material_data.

  " Raw structure for internal use
    TYPES: BEGIN OF ty_material_info,
             material     TYPE char18,
             description  TYPE maktx,
             base_unit    TYPE meins,
             price        TYPE p LENGTH 8 DECIMALS 2,
             currency     TYPE waers,
             stock        TYPE labst,
           END OF ty_material_info.

    DATA: lv_material_general_data TYPE bapimatdoa,
          lv_return     TYPE bapireturn,
          "ls_mbew      TYPE mbew,
          lv_price_raw  TYPE mbew-stprs,
          "ls_t001k     TYPE t001k,
          lv_bukrs      TYPE t001k-bukrs,
          "ls_t001 TYPE t001,
          lv_currency   TYPE t001-waers,
          "ls_mard      TYPE mard,
          lv_stock      TYPE mard-labst,
          rs_info       TYPE ty_material_info,
          ls_json_ready TYPE ty_custom_json,
          lv_price_str  TYPE string,
          lv_stock_int  TYPE i,
          lv_json       TYPE string.

    CLEAR: rs_info.


    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING
        material = p_matnr
      IMPORTING
        material_general_data = lv_material_general_data
        return   = lv_return.

    IF lv_return-type = 'E'.
      rv_json = '{"error": "Material not found"}'.
      RETURN.
    ENDIF.

    rs_info-material    = p_matnr.
    rs_info-description = lv_material_general_data-matl_desc.
    rs_info-base_unit   = lv_material_general_data-base_uom.




    "Get standart price from MBEW
    SELECT SINGLE stprs INTO lv_price_raw
      FROM mbew
      WHERE matnr = p_matnr
        AND bwkey = p_plant.

    IF sy-subrc = 0.
      rs_info-price = lv_price_raw.
    ENDIF.



    " Get currency via T001K and T001
    SELECT SINGLE bukrs INTO lv_bukrs
      FROM t001k
      WHERE bwkey = p_plant.

    IF sy-subrc = 0.
      SELECT SINGLE waers INTO lv_currency
        FROM t001
        WHERE bukrs = lv_bukrs.

      IF sy-subrc = 0.
        rs_info-currency = lv_currency.
      ENDIF.
    ENDIF.



    " Get stock from MARD
    SELECT SINGLE labst INTO lv_stock
        FROM mard
        WHERE matnr = p_matnr
          AND werks = p_plant
          AND lgort = p_lgort.

    IF sy-subrc = 0.
      rs_info-stock = lv_stock.
    ENDIF.

    " Format output
    lv_price_str = |{ rs_info-currency }:{ rs_info-price }|.
    lv_stock_int = rs_info-stock.

    " Fill JSON structure
    ls_json_ready-product_id   = rs_info-material.
    ls_json_ready-description  = rs_info-description.
    ls_json_ready-unit         = rs_info-base_unit.
    ls_json_ready-price        = lv_price_str.
    ls_json_ready-total_stock  = lv_stock_int.

    "Serialize to JSON
    "DATA(lo_serializer) = NEW cl_trex_json_serializer( ls_json_ready ).
    "lo_serializer->serialize( ).
    "rv_json = lo_serializer->get_data( ).

    DATA(lo_serializer) = NEW cl_fdt_json_serializer( ).
    DATA(lx_data) = REF #( ls_json_ready ).

    rv_json = lo_serializer->serialize( lx_data ).

    REPLACE ALL OCCURRENCES OF '"PRODUCT_ID"' IN rv_json WITH '"product_id"'.
    REPLACE ALL OCCURRENCES OF '"DESCRIPTION"' IN rv_json WITH '"description"'.
    REPLACE ALL OCCURRENCES OF '"UNIT"' IN rv_json WITH '"unit"'.
    REPLACE ALL OCCURRENCES OF '"PRICE"' IN rv_json WITH '"price"'.
    REPLACE ALL OCCURRENCES OF '"TOTAL_STOCK"' IN rv_json WITH '"total_stock"'.







    " POST REQUEST TO TALER
    DATA: lv_url        TYPE string VALUE 'https://tutorial.talerintosap.us/private/products',
      lv_token      TYPE string VALUE 'secret-token:TALERintoSAP2527',
      lv_auth_hdr   TYPE string,
      lo_http_client TYPE REF TO if_http_client,
      lv_response   TYPE string,
      lv_code       TYPE i.

    " Create HTTP client
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

    IF sy-subrc <> 0.
        rv_json = '{"error": "Failed to create HTTP client"}'.
        RETURN.
    ENDIF.

    " Set headers
    lv_auth_hdr = |Bearer { lv_token }|.

    CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
            name  = 'Content-Type'
            value = 'application/json'.

    CALL METHOD lo_http_client->request->set_header_field
        EXPORTING
            name  = 'Authorization'
            value = lv_auth_hdr.

    " Set HTTP method and body
    lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
    lo_http_client->request->set_cdata( rv_json ).

    " Send the request
    CALL METHOD lo_http_client->send
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            others                     = 4.

    IF sy-subrc <> 0.
        rv_json = '{"error": "HTTP request failed"}'.
        RETURN.
    ENDIF.

    " Receive response
    CALL METHOD lo_http_client->receive
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state         = 2
            http_processing_failed     = 3
            others                     = 4.

    IF sy-subrc <> 0.
        rv_json = '{"error": "HTTP response error"}'.
        RETURN.
   ENDIF.

    " Get response body and status code
    lv_response = lo_http_client->response->get_cdata( ).
    DATA(lv_status_header) = lo_http_client->response->get_header_field( '~status_code' ).

    " Print status code
    WRITE: lv_status_header.
  ENDMETHOD.

ENDCLASS.

