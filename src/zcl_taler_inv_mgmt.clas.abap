CLASS zcl_taler_inv_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_material_data
      IMPORTING
        p_matnr TYPE char18.

ENDCLASS.



CLASS zcl_taler_inv_mgmt IMPLEMENTATION.

  METHOD get_material_data.

    DATA: lv_material_general_data TYPE bapimatdoa,
          lv_return TYPE bapireturn.


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



    " Output collected data
    WRITE: / 'Material:'       , p_matnr,
           / 'Description:'    , lv_material_general_data-matl_desc,
           / 'Base Unit:'      , lv_material_general_data-base_uom.

  ENDMETHOD.

ENDCLASS.

