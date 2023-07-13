CLASS zksa_qrcode_generator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS : generate IMPORTING im_sname       TYPE  string
                                       im_taxnum      TYPE  string
                                       im_time_stamp  TYPE  string
                                       im_totam       TYPE  string
                                       im_taxam       TYPE  string

                             RETURNING VALUE(ex_qrcode_line)  TYPE  string.


  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES : BEGIN OF ty_tlv ,
              tag    TYPE n LENGTH 2,
              length TYPE n LENGTH 3,
              value  TYPE string,
              hexa_v TYPE xstring,
            END OF ty_tlv .

    TYPES : BEGIN OF ty_tlv_hex ,
              tag    TYPE xstring,
              length TYPE xstring,
              value  TYPE xstring,
            END OF ty_tlv_hex .

    CLASS-DATA :gt_tlv TYPE TABLE OF ty_tlv,
                gs_tlv TYPE  ty_tlv.

    CLASS-DATA : gt_tlv_hex TYPE TABLE OF ty_tlv_hex,
                 gs_tlv_hex TYPE ty_tlv_hex.

    CLASS-DATA : gv_final_hex TYPE xstring,
                 gv_final     TYPE string.


    CLASS-METHODS : create_tlv IMPORTING im_sname      TYPE  string
                                         im_taxnum     TYPE  string
                                         im_time_stamp TYPE  string
                                         im_totam      TYPE  string
                                         im_taxam      TYPE  string
                               ,
      get_final_hexa_serial RETURNING VALUE(rn_qrcode_hex)  TYPE  string ,

      dec_to_hexa IMPORTING im_dec_value   TYPE n
                  RETURNING VALUE(rn_xstr) TYPE xstring ,

      string_to_hexa IMPORTING im_str_value   TYPE string
                     RETURNING VALUE(rn_xstr) TYPE xstring ,

      to_base64 IMPORTING im_xstr_value    TYPE xstring
                RETURNING VALUE(rn_base64) TYPE string               .


ENDCLASS.

CLASS zksa_qrcode_generator IMPLEMENTATION.

  METHOD generate .

    create_tlv(   im_sname = im_sname
                  im_taxnum = im_taxnum
                  im_time_stamp = im_time_stamp
                  im_totam = im_totam
                  im_taxam = im_taxam
) .

    ex_qrcode_line = get_final_hexa_serial(  ) .

  ENDMETHOD.

  METHOD create_tlv .
********************** CREATE tlv******************************
*- seller name
    gs_tlv-tag = 1 .
    gs_tlv-value = im_sname .
    gs_tlv-length = strlen( gs_tlv-value ) .

    gs_tlv_hex-tag = dec_to_hexa( gs_tlv-tag ) .
    gs_tlv_hex-length = dec_to_hexa( gs_tlv-length ) .
    gs_tlv_hex-value = string_to_hexa( gs_tlv-value  ) .
    CONCATENATE gs_tlv_hex-tag gs_tlv_hex-length gs_tlv_hex-value INTO gs_tlv-hexa_v IN BYTE MODE .

    APPEND gs_tlv TO gt_tlv .
    CLEAR : gs_tlv , gs_tlv_hex .

*-Tax number
    gs_tlv-tag = 2 .
    gs_tlv-value = im_taxnum .
    gs_tlv-length = strlen( gs_tlv-value ) .

    gs_tlv_hex-tag = dec_to_hexa( gs_tlv-tag ) .
    gs_tlv_hex-length = dec_to_hexa( gs_tlv-length ) .
    gs_tlv_hex-value = string_to_hexa( gs_tlv-value  ) .

    CONCATENATE gs_tlv_hex-tag gs_tlv_hex-length gs_tlv_hex-value INTO gs_tlv-hexa_v IN BYTE MODE .

    APPEND gs_tlv TO gt_tlv .
    CLEAR : gs_tlv , gs_tlv_hex .

*-date / time
*IF p_invtim > 0 .
*  CONCATENATE im_invdat 'T' into data(lv_date).
*  CONCATENATE _invtim 'Z' into data(lv_time) .
*  CONCATENATE lv_date lv_time INTO DATA(lv_dat_tim) SEPARATED BY '-'.
*ENDIF .

    gs_tlv-tag = 3 .
    gs_tlv-value = im_time_stamp ."lv_dat_tim .
    gs_tlv-length = strlen( gs_tlv-value ) .

    gs_tlv_hex-tag = dec_to_hexa( gs_tlv-tag ) .
    gs_tlv_hex-length = dec_to_hexa( gs_tlv-length ) .
    gs_tlv_hex-value = string_to_hexa( gs_tlv-value  ) .

    CONCATENATE gs_tlv_hex-tag gs_tlv_hex-length gs_tlv_hex-value INTO gs_tlv-hexa_v IN BYTE MODE .

    APPEND gs_tlv TO gt_tlv .
    CLEAR : gs_tlv , gs_tlv_hex .


*- total amount
    gs_tlv-tag = 4 .
    gs_tlv-value =  im_totam .
    gs_tlv-length = strlen( gs_tlv-value ) .

    gs_tlv_hex-tag = dec_to_hexa( gs_tlv-tag ) .
    gs_tlv_hex-length = dec_to_hexa( gs_tlv-length ) .
    gs_tlv_hex-value = string_to_hexa( gs_tlv-value  ) .

    CONCATENATE gs_tlv_hex-tag gs_tlv_hex-length gs_tlv_hex-value INTO gs_tlv-hexa_v IN BYTE MODE .

    APPEND gs_tlv TO gt_tlv .
    CLEAR : gs_tlv , gs_tlv_hex .

*- tax amount
    gs_tlv-tag = 5 .
    gs_tlv-value =  im_taxam .
    gs_tlv-length = strlen( gs_tlv-value ) .

    gs_tlv_hex-tag = dec_to_hexa( gs_tlv-tag ) .
    gs_tlv_hex-length = dec_to_hexa( gs_tlv-length ) .
    gs_tlv_hex-value = string_to_hexa( gs_tlv-value  ) .

    CONCATENATE gs_tlv_hex-tag gs_tlv_hex-length gs_tlv_hex-value INTO gs_tlv-hexa_v IN BYTE MODE .

    APPEND gs_tlv TO gt_tlv .
    CLEAR : gs_tlv , gs_tlv_hex .


  ENDMETHOD.

  METHOD get_final_hexa_serial .

*- get serial hexa line
    LOOP AT gt_tlv INTO gs_tlv .
      CONCATENATE gv_final_hex gs_tlv-hexa_v INTO gv_final_hex IN BYTE MODE .
    ENDLOOP .


    gv_final = to_base64( gv_final_hex ) .
    rn_qrcode_hex = gv_final .

  ENDMETHOD.

  METHOD dec_to_hexa .

    """"" copy of cl_trex_utility=>conv_dec_to_hex( ) .
    CONSTANTS:
         l_c_max_int32 TYPE i VALUE 2147483647.

    IF im_dec_value <= l_c_max_int32.
      rn_xstr = im_dec_value.
      RETURN.
    ENDIF.

*   handling for 64 bit values
    DATA:
      l_n19  TYPE n LENGTH 19  , " value 137438953472,
      l_itab TYPE STANDARD TABLE OF c,
      l_n2   TYPE n LENGTH 2,
      l_c    TYPE c LENGTH 1.

    l_n19 = im_dec_value.
    WHILE ( l_n19 > 0 ).
      l_n2 = l_n19 MOD 16.

      IF l_n2 < 10.
        l_c = l_n2+1(1).
      ELSE.
        l_c = im_dec_value+l_n2(1).
      ENDIF.
      APPEND l_c TO l_itab.
      l_n19 = l_n19 DIV 16.
    ENDWHILE.

    DATA:
      l_x  TYPE x LENGTH 1,
      l_c2 TYPE c LENGTH 2.

    LOOP AT l_itab INTO l_c.
      IF sy-tabix MOD 2 = 1.
        CLEAR l_c2.
        l_c2+1(1) = l_c.

        IF sy-tabix = lines( l_itab ).
*         last handle of this loop. '0' needs to be added at the beginning.
          l_c2(1) = '0'.
          l_x = l_c2.
          rn_xstr = l_x && rn_xstr.
        ENDIF.
      ELSE.
        l_c2(1) = l_c.
        l_x = l_c2.
        rn_xstr = l_x && rn_xstr.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD string_to_hexa .

    rn_xstr = cl_abap_conv_codepage=>create_out( )->convert( im_str_value ).

  ENDMETHOD.

  METHOD to_base64 .

    """"" copy content of function scm_base64_encode_str
    DATA:
      input_length TYPE i,
      xx(3)        TYPE x, x TYPE x, c4(4) TYPE c,
      i            TYPE i, k TYPE i,

      b64tab4(256) TYPE c VALUE
       'AAAABBBBCCCCDDDDEEEEFFFFGGGGHHHHIIIIJJJJKKKKLLLLMMMM' &
      'NNNNOOOOPPPPQQQQRRRRSSSSTTTTUUUUVVVVWWWWXXXXYYYYZZZZ' &
      'aaaabbbbccccddddeeeeffffgggghhhhiiiijjjjkkkkllllmmmm' &
      'nnnnooooppppqqqqrrrrssssttttuuuuvvvvwwwwxxxxyyyyzzzz' &
      '0000111122223333444455556666777788889999++++///'.

    b64tab4+255(1) = '/'.   "text literal cannot have more than 255 chars

    input_length = xstrlen( im_xstr_value ).
    i = 0.

    CLEAR rn_base64.

    k = input_length DIV 3.
    DO k TIMES.

      xx(3) = im_xstr_value+i(3).

      x = xx(1).
      c4(1) = b64tab4+x(1).
      xx = xx * 64.

      x = xx(1).
      c4+1(1) = b64tab4+x(1).
      xx = xx * 64.

      x = xx(1).
      c4+2(1) = b64tab4+x(1).
      xx = xx * 64.

      x = xx(1).
      c4+3(1) = b64tab4+x(1).

      CONCATENATE rn_base64 c4 INTO rn_base64.
      i = i + 3.

    ENDDO.


    IF i LT input_length.

      xx(3) = im_xstr_value+i.

      x = xx(1).
      CONCATENATE rn_base64 b64tab4+x(1) INTO rn_base64.
      xx = xx * 64.
      i = i + 1.

      x = xx(1).
      CONCATENATE rn_base64 b64tab4+x(1) INTO rn_base64.
      xx = xx * 64.

      IF i EQ input_length.
        CONCATENATE rn_base64 '==' INTO rn_base64.
      ELSE.

        x = xx(1).
        CONCATENATE rn_base64 b64tab4+x(1) INTO rn_base64.
        xx = xx * 64.
        i = i + 1.
        IF i EQ input_length.
          CONCATENATE rn_base64 '=' INTO rn_base64.
        ELSE.
          x = xx(1).
          CONCATENATE rn_base64 b64tab4+x(1) INTO rn_base64.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDMETHOD.


ENDCLASS.
