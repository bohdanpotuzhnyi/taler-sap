CLASS zcl_taler_inv_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_material_data
      IMPORTING
        p_matnr TYPE char18
        p_plant TYPE char18
        p_lgort TYPE lgort_d
      EXPORTING
        ev_price TYPE p
        ev_currency TYPE waers
        ev_stock TYPE labst.

ENDCLASS.



CLASS zcl_taler_inv_mgmt IMPLEMENTATION.

  METHOD get_material_data.

    DATA: lv_material_general_data TYPE bapimatdoa,
          lv_return TYPE bapireturn,
          ls_mbew TYPE mbew,
          ls_t001k TYPE t001k,
          ls_t001 TYPE t001,
          ls_mard TYPE mard.

    CLEAR: ev_price, ev_currency.


    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING
        material = p_matnr
      IMPORTING
        material_general_data = lv_material_general_data
        return   = lv_return.

    IF lv_return-type = 'E'.
      WRITE: / 'Error:', lv_return-message.
      RETURN.
    ENDIF.

    "Get standart price from MBEW
    SELECT SINGLE * INTO ls_mbew
      FROM mbew
      WHERE matnr = p_matnr
        AND bwkey = p_plant.

    IF sy-subrc = 0.
      ev_price    = ls_mbew-stprs.
     ELSE.
        WRITE: / 'Price not found in MBEW.'.
    ENDIF.

    " Get currency via T001K and T001
    SELECT SINGLE * INTO ls_t001k
      FROM t001k
      WHERE bwkey = p_plant.

    IF sy-subrc = 0.
      SELECT SINGLE * INTO ls_t001
        FROM t001
        WHERE bukrs = ls_t001k-bukrs.

      IF sy-subrc = 0.
        ev_currency = ls_t001-waers.
      ENDIF.
    ENDIF.

    " Get stock from MARD
    SELECT SINGLE * INTO ls_mard
        FROM mard
        WHERE matnr = p_matnr
            AND werks = p_plant
            AND lgort = p_lgort.

    IF sy-subrc = 0.
        ev_stock = ls_mard-labst.
    ELSE.
        WRITE: / 'Stock not found for given storage location.'.
    ENDIF.

    " Output collected data
    WRITE: / 'Material:'       , p_matnr,
           / 'Description:'    , lv_material_general_data-matl_desc,
           / 'Base Unit:'      , lv_material_general_data-base_uom,
           / 'Standard Price:', ev_price,
           / 'Currency:'      , ev_currency,
           / 'Stock (Unrestricted):', ev_stock.


  ENDMETHOD.

ENDCLASS.

