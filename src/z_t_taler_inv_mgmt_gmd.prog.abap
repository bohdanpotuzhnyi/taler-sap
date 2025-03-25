*&---------------------------------------------------------------------*
*& Report z_t_taler_inv_mgmt_gmd
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_inv_mgmt_gmd.

PARAMETERS: p_matnr TYPE char18 DEFAULT 'TALER_BOTTLE01',
            p_plant TYPE char18 DEFAULT 'HD00',
            p_lgort TYPE lgort_d DEFAULT 'BIEL'.

START-OF-SELECTION.

  DATA(lo_inv_mgmt) = NEW zcl_taler_inv_mgmt( ).
  DATA: lv_price    TYPE p,
        lv_currency TYPE waers,
        lv_stock    TYPE labst.

  lo_inv_mgmt->get_material_data(
    EXPORTING
      p_matnr     = p_matnr
      p_plant     = p_plant
      p_lgort     = p_lgort
    IMPORTING
      ev_price    = lv_price
      ev_currency = lv_currency
      ev_stock    = lv_stock
  ).
