*&---------------------------------------------------------------------*
*& Report  z_t_taler_billing_docs
*&---------------------------------------------------------------------*
*& Fetch and display all billing documents where
*&   ZLSCH = 'D', LAND1 = 'DE', VKORG = 'DS00'
*&---------------------------------------------------------------------*
REPORT z_t_taler_billing_docs.

DATA: lo_reader TYPE REF TO zcl_taler_order,
      lt_headers TYPE zcl_taler_order=>ty_billing_header_tab,
      lt_items   TYPE zcl_taler_order=>ty_billing_item_tab.

START-OF-SELECTION.

  "-- Instantiate the class
  CREATE OBJECT lo_reader.

  CALL METHOD lo_reader->clear_all_taler_tables.

  "-- Retrieve billing headers + items
  CALL METHOD lo_reader->get_billing_docs
    IMPORTING
      et_billing_headers = lt_headers
      et_billing_items   = lt_items.

  "-- No results?
  IF lt_headers IS INITIAL.
    WRITE: / 'No billing documents found for:',
           ' ZLSCH=D, LAND1=DE, VKORG=DS00'.
    RETURN.
  ENDIF.

  "-- Display header data
  WRITE: / '=== Billing Headers ==='.
  LOOP AT lt_headers INTO DATA(ls_header).
    WRITE: / ls_header-vbeln NO-GAP,
             ls_header-netwr   NO-GAP,
             ls_header-waerk.
  ENDLOOP.

  "-- Display item data
  WRITE: / '=== Billing Items ==='.
  LOOP AT lt_items INTO DATA(ls_item).
    WRITE: / ls_item-vbeln   NO-GAP,
             lo_reader->get_order_json( iv_sales_order = ls_item-vgbel iv_billing_doc = ls_item-vbeln )   NO-GAP,
             ls_item-matnr   NO-GAP,
             ls_item-arktx   NO-GAP,
             ls_item-fkimg.
  ENDLOOP.
