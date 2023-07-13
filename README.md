# ZATCA-QR-CODE-Generator-
A Class written in ABAP to Generate ZATCA QR CODE ( it is compatible with Both S/4 Hana and S/4 Hana Cloud  )

Here is a sample for using the class =>


zksa_qrcode_generator=>GENERATE(
  EXPORTING
    IM_SNAME       = 'TEST COMPANY'
    IM_TAXNUM      = '123456789123456789'
    IM_TIME_STAMP  = '2020-09-12T10:30:30Z'
    IM_TOTAM       = '2000.90'
    IM_TAXAM       = '5000.30'
  IMPORTING
    EX_QRCODE_LINE = data(qrcode)
).

Just note the Time stamp Format ..
