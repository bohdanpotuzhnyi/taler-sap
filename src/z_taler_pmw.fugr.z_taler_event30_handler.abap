DATA: lo_order     TYPE REF TO zcl_taler_order,
      lv_response  TYPE string,
      lv_vbeln     TYPE vbeln.

" 1. Extract the Sales Order number (adjust if needed)
lv_vbeln = i_payp-vblnr.  " Assumes VBELN is used for order reference

" 2. Create instance of the Taler order class
lo_order = NEW zcl_taler_order( ).

" 3. Send order to Taler API
lv_response = lo_order->send_order_to_taler(
                iv_sales_order = lv_vbeln ).

" 4. Pass back the result as output text (for review/output)
e_record = lv_response.
