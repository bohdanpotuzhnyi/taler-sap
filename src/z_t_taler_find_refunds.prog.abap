*&---------------------------------------------------------------------*
*& Report z_t_taler_find_refunds
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_t_taler_find_refunds.

UPDATE ZTLR_ORDER_LOG
    SET state = 'refunded'
    WHERE state = 'refund_completed'.

TYPES: BEGIN OF ty_billing_header,
             vbeln TYPE vbrk-vbeln,   " Billing doc number
             netwr TYPE vbrk-netwr,   " Amount
             waerk TYPE vbrk-waerk,   " Currency
             sfakn TYPE vbrk-sfakn,   " Cancelled billing document number
             fksto TYPE vbrk-fksto,   " Billing document is canceled
             vgbel TYPE vbrp-vgbel,   " Sales order
           END OF ty_billing_header,
           ty_billing_header_tab TYPE STANDARD TABLE OF ty_billing_header WITH EMPTY KEY.


DATA: ls_config      TYPE ztlr_config,
            lo_general     TYPE REF TO zcl_taler_general,
            lt_cancel_hdr  TYPE ty_billing_header_tab,
            ls_can         TYPE ty_billing_header,
            ls_log         TYPE ztlr_order_log,
            lv_long_msg    TYPE string,
            lt_refund TYPE STANDARD TABLE OF ztlr_order_log WITH EMPTY KEY.

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
               AND fksto = 'X'.

cl_demo_output=>display( lt_cancel_hdr ).


SELECT *
        INTO TABLE @lt_refund
        FROM ztlr_order_log
        WHERE refund = 'X'
          AND state  NOT IN ( 'canceled',
                              'refund_completed',
                              'refund_fail',
                              'refunding',
                              'completed' ).


cl_demo_output=>display( lt_refund ).
