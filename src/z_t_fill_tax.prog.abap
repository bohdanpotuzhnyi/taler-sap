*&---------------------------------------------------------------------*
*& Report z_t_fill_tax
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_fill_tax.

DATA: lt_order_log TYPE TABLE OF ztlr_order_log,
      lt_vbap      TYPE TABLE OF vbap,
      lv_tax_total TYPE vbap-mwsbp.

SELECT * FROM ztlr_order_log INTO TABLE lt_order_log.

LOOP AT lt_order_log INTO DATA(ls_log).
  CLEAR lv_tax_total.

  " Get tax values from VBAP
  SELECT SUM( mwsbp ) INTO lv_tax_total
    FROM vbap
    WHERE vbeln = ls_log-sales_order.

  " Update the tax amount if tax found
  IF sy-subrc = 0 AND lv_tax_total IS NOT INITIAL.
    UPDATE ztlr_order_log
      SET tax_amount = lv_tax_total
      WHERE sales_order = ls_log-sales_order.
  ENDIF.
ENDLOOP.

COMMIT WORK.
