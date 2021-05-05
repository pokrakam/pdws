REPORT zpdws.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING i_wf            TYPE sww_task
                         RETURNING VALUE(r_result) TYPE REF TO lcl_main.
    METHODS run.


  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA wf TYPE sww_task.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT r_result.
    r_result->wf = i_wf.
  ENDMETHOD.



  METHOD run.
    DATA: ls_wf_definition_key TYPE swd_wfdkey,

          lo_wfd_xml           TYPE REF TO cl_xml_document_base,
          lo_wfd_export        TYPE REF TO if_swf_pdef_export,
          lt_versions          TYPE TABLE OF swd_versns,

          lo_node              TYPE REF TO if_ixml_element.

    CALL FUNCTION 'SWD_GET_VERSIONS_OF_WORKFLOW'
      EXPORTING
        im_task          = wf
        im_exetyp        = 'S'
      IMPORTING
        ex_active_wfdkey = ls_wf_definition_key
      TABLES
        ex_versions      = lt_versions.

    CREATE OBJECT lo_wfd_export TYPE cl_wfd_convert_def_to_ixml.

    lo_wfd_xml = lo_wfd_export->convert( load_from_db = abap_true
                                         language = sy-langu
                                         wfd_key = ls_wf_definition_key ).

*    lo_node->append_child( lo_wfd_xml->get_first_node( ) ).
    lo_wfd_xml->render_2_string(
      EXPORTING
        pretty_print = 'X'              " Format Output
      IMPORTING
        retcode      = DATA(retcode)
        stream       = DATA(stream)
        size         = DATA(size)
    ).
    cl_demo_output=>display_xml( stream ).
  ENDMETHOD.

ENDCLASS.




PARAMETERS p_wf TYPE sww_task DEFAULT 'WS90000001'.

START-OF-SELECTION.
  lcl_main=>create( p_wf )->run( ).
