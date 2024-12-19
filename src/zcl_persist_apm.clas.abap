CLASS zcl_persist_apm DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

************************************************************************
* apm Persistence
*
* Copyright 2024 apm.to Inc. <https://apm.to>
* SPDX-License-Identifier: MIT
************************************************************************
  PUBLIC SECTION.

    INTERFACES zif_persist_apm.

    CLASS-METHODS get_instance
      RETURNING
        VALUE(result) TYPE REF TO zif_persist_apm.

    CLASS-METHODS injector
      IMPORTING
        !mock TYPE REF TO zif_persist_apm.

    CLASS-METHODS validate_key
      IMPORTING
        !key          TYPE csequence
      RETURNING
        VALUE(result) TYPE abap_bool.

    CLASS-METHODS explain_key
      IMPORTING
        !key          TYPE csequence
      RETURNING
        VALUE(result) TYPE zif_persist_apm=>ty_explained.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA db_instance TYPE REF TO zif_persist_apm.

ENDCLASS.



CLASS zcl_persist_apm IMPLEMENTATION.


  METHOD explain_key.

    SPLIT key AT ':' INTO DATA(key_type) DATA(name) DATA(extra).

    CASE key_type.
      WHEN zif_persist_apm=>c_key_type-package.
        result-key_type    = 'Package'.
        result-description = lcl_persist_utils=>get_package_description( name ).

        IF extra = zif_persist_apm=>c_key_extra-package_json.
          result-extra        = 'Package JSON'.
          result-content_type = zif_persist_apm=>c_content_type-json.
        ELSEIF extra = zif_persist_apm=>c_key_extra-package_readme.
          result-extra        = 'Readme'.
          result-content_type = zif_persist_apm=>c_content_type-markdown.
        ELSE.
          " Should not happen. Open issue
          result-extra        = 'Unknown key extra'.
          result-content_type = zif_persist_apm=>c_content_type-text.
        ENDIF.

      WHEN zif_persist_apm=>c_key_type-settings.
        IF name = zif_persist_apm=>c_key_name-global_settings.
          result-key_type    = 'Global Settings'.
          result-description = 'For All Users'.
        ELSE.
          result-key_type    = 'Personal Settings'.
          result-description = lcl_persist_utils=>get_user_description( name ).
        ENDIF.
        result-content_type = zif_persist_apm=>c_content_type-json.

      WHEN OTHERS.
        result-key_type     = 'Unknown type of key'.
        result-content_type = zif_persist_apm=>c_content_type-text.

    ENDCASE.

  ENDMETHOD.


  METHOD get_instance.

    IF db_instance IS INITIAL.
      CREATE OBJECT db_instance TYPE zcl_persist_apm.
    ENDIF.

    result = db_instance.

  ENDMETHOD.


  METHOD injector.

    db_instance = mock.

  ENDMETHOD.


  METHOD validate_key.

    SPLIT key AT ':' INTO DATA(key_type) DATA(rest).

    result = boolc( sy-subrc = 0 AND
      ( key_type = zif_persist_apm=>c_key_type-package OR
        key_type = zif_persist_apm=>c_key_type-settings ) ).

  ENDMETHOD.


  METHOD zif_persist_apm~delete.

    DELETE FROM (zif_persist_apm=>c_tabname) WHERE keys = key.
    IF sy-subrc <> 0.
      zcx_error=>raise( |Error deleting { key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_persist_apm~list.

    DATA:
      db_entries TYPE STANDARD TABLE OF zif_persist_apm=>ty_zabappm WITH DEFAULT KEY,
      db_entry   LIKE LINE OF result.

    IF filter IS INITIAL.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE db_entries
        WHERE timestamp BETWEEN from AND to
        ORDER BY PRIMARY KEY.
    ELSE.
      SELECT * FROM (zif_persist_apm=>c_tabname) INTO TABLE db_entries
        WHERE timestamp BETWEEN from AND to AND keys LIKE filter
        ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT db_entries ASSIGNING FIELD-SYMBOL(<data>).
      CLEAR db_entry.
      db_entry-keys      = <data>-keys.
      db_entry-value     = <data>-value.
      db_entry-user      = <data>-luser.
      db_entry-timestamp = <data>-timestamp.
      SPLIT <data>-keys AT ':' INTO db_entry-key_type db_entry-key_name db_entry-key_extra.
      INSERT db_entry INTO TABLE result.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_persist_apm~load.

    SELECT SINGLE * FROM (zif_persist_apm=>c_tabname) INTO result WHERE keys = key.
    IF sy-subrc <> 0.
      zcx_error=>raise( |Error loading { key }| ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_persist_apm~lock.

    CALL FUNCTION 'ENQUEUE_EZABAPPM'
      EXPORTING
        mode_zabappm   = mode
        keys           = key
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc <> 0.
      zcx_error=>raise_t100( ).
    ENDIF.

    DATA(dummy_update_function) = lcl_persist_utils=>get_update_function( ).

    " trigger dummy update task to automatically release locks at commit
    CALL FUNCTION dummy_update_function IN UPDATE TASK.

  ENDMETHOD.


  METHOD zif_persist_apm~save.

    IF validate_key( key ) = abap_false.
      zcx_error=>raise( |Invalid key { key }| ).
    ENDIF.

    DATA(db_entry) = VALUE zif_persist_apm=>ty_zabappm(
     keys  = key
     value = replace(
       val  = value
       sub  = cl_abap_char_utilities=>cr_lf
       with = cl_abap_char_utilities=>newline
       occ  = 0 )
     luser = sy-uname ).

    GET TIME STAMP FIELD db_entry-timestamp.

    UPDATE (zif_persist_apm=>c_tabname) FROM db_entry.
    IF sy-subrc <> 0.
      INSERT (zif_persist_apm=>c_tabname) FROM db_entry.
      IF sy-subrc <> 0.
        zcx_error=>raise( |Error saving { key }| ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
