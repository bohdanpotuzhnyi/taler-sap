CLASS zcl_taler_inv_mgmt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS get_material_data
      IMPORTING
        iv_matnr TYPE matnr
        iv_plant TYPE werks_d.
ENDCLASS.



CLASS zcl_taler_inv_mgmt IMPLEMENTATION.

  METHOD get_material_data.

    DATA: lv_return  TYPE bapiret2,
          lt_general TYPE TABLE OF bapi_mara,
          lt_plant   TYPE TABLE OF bapi_marc,
          lt_storage TYPE TABLE OF bapi_mard,
          lt_texts   TYPE TABLE OF bapi_mltx,
          lv_descr   TYPE bapi_mltx-text_line.

    " Step 1: Get material master data
    CALL FUNCTION 'BAPI_MATERIAL_GET_DETAIL'
      EXPORTING
        material = iv_matnr
        plant    = iv_plant
      IMPORTING
        return   = lv_return
      TABLES
        materialgeneraldata         = lt_general
        materialplantdata           = lt_plant
        materialstoragelocationdata = lt_storage.

    IF lv_return-type = 'E'.
      WRITE: / 'Error:', lv_return-message.
      RETURN.
    ENDIF.

    READ TABLE lt_plant INTO DATA(ls_plant) INDEX 1.
    READ TABLE lt_storage INTO DATA(ls_storage) INDEX 1.

    " Step 2: Get material description lines
    CALL FUNCTION 'BAPI_MATERIAL_GETTEXT'
      EXPORTING
        material = iv_matnr
        language = sy-langu
      TABLES
        material_descriptions = lt_texts.

    READ TABLE lt_texts INTO DATA(ls_text) INDEX 1.
    IF sy-subrc = 0.
      lv_descr = ls_text-text_line.  " <--- Correct field from BAPI_MLTX
    ELSE.
      lv_descr = 'N/A'.
    ENDIF.

    " Output collected data
    WRITE: / 'Material:'       , iv_matnr,
           / 'Description:'    , lv_descr,
           / 'Plant:'          , iv_plant,
           / 'Base Unit:'      , ls_plant-prod_unit,
           / 'Storage Loc:'    , ls_storage-stge_loc.

  ENDMETHOD.

ENDCLASS.

