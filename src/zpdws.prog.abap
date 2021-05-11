REPORT zpdws.

CLASS lcl_workflow_definition DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS load  IMPORTING iv_wf            TYPE sww_task
                        RETURNING VALUE(rv_result) TYPE REF TO lcl_workflow_definition
                        RAISING   zcx_abapgit_exception.

    METHODS serialize RETURNING VALUE(rv_result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA mv_wf TYPE sww_task.
    DATA mv_objid TYPE hrobjid.
    DATA mo_wfdef TYPE REF TO cl_workflow_task_ws.

    METHODS supply_instance RAISING zcx_abapgit_exception.
    METHODS check_subrc_for IMPORTING iv_call TYPE clike OPTIONAL
                            RAISING   zcx_abapgit_exception.
    METHODS get_active_definition_key RETURNING VALUE(rs_wf_definition_key) TYPE swd_wfdkey.
ENDCLASS.

CLASS lcl_workflow_definition IMPLEMENTATION.

  METHOD load.
    DATA lo_def TYPE REF TO lcl_workflow_definition.

    CREATE OBJECT lo_def.
    lo_def->mv_wf = iv_wf.
    lo_def->mv_objid = iv_wf+2(8).
    lo_def->supply_instance( ).
    rv_result = lo_def.
  ENDMETHOD.


  METHOD supply_instance.

    cl_workflow_factory=>create_ws(
       EXPORTING
         objid                        = mv_objid
       RECEIVING
         ws_inst                      = mo_wfdef
       EXCEPTIONS
         workflow_does_not_exist = 1
         object_could_not_be_locked   = 2
         objid_not_given              = 3
         OTHERS                       = 4 )  ##SUBRC_OK.

    check_subrc_for( 'CREATE_TS' ).

  ENDMETHOD.


  METHOD check_subrc_for.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( iv_call && ' returned ' && sy-subrc ).
    ENDIF.
  ENDMETHOD.

  METHOD serialize.

    DATA: ls_wf_definition_key TYPE swd_wfdkey,

          lo_wfd_xml           TYPE REF TO cl_xml_document_base,
          lo_wfd_export        TYPE REF TO if_swf_pdef_export,
          lt_versions          TYPE TABLE OF swd_versns,

          lo_node              TYPE REF TO if_ixml_element,
          lo_def               TYPE REF TO lcl_workflow_definition,
          lo_error             TYPE REF TO zcx_abapgit_exception,
          lv_retcode           TYPE sysubrc,
          lv_stream            TYPE string,
          lv_size              TYPE sytabix.

    TRY.
        lo_def = load( mv_wf ).
      CATCH zcx_abapgit_exception INTO lo_error.
        WRITE / lo_error->get_text( ).
    ENDTRY.

    ls_wf_definition_key = get_active_definition_key( ).

    CREATE OBJECT lo_wfd_export TYPE cl_wfd_convert_def_to_ixml.

    lo_wfd_xml = lo_wfd_export->convert( load_from_db = abap_true
                                         language = sy-langu
                                         wfd_key = ls_wf_definition_key ).

    IF lo_wfd_xml IS BOUND.

*  Blah foo bar lo_node->append_child( lo_wfd_xml->get_first_node( ) ).

      lo_wfd_xml->render_2_string(
        EXPORTING
          pretty_print = 'X'
        IMPORTING
          retcode      = lv_retcode
          stream       = lv_stream
          size         = lv_size ).

      cl_demo_output=>display_xml( lv_stream ).
      rv_result = lv_stream.
    ELSE.
      cl_demo_output=>display( 'No XML' ).
    ENDIF.

  ENDMETHOD.

  METHOD get_active_definition_key.
    DATA: lt_versions TYPE STANDARD TABLE OF swd_versns.

    CALL FUNCTION 'SWD_GET_VERSIONS_OF_WORKFLOW'
      EXPORTING
        im_task          = mv_wf
        im_exetyp        = 'S'
      IMPORTING
        ex_active_wfdkey = rs_wf_definition_key
      TABLES
        ex_versions      = lt_versions.

  ENDMETHOD.


ENDCLASS.

CLASS lcl_main DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS create IMPORTING iv_wf            TYPE sww_task
                         RETURNING VALUE(ro_result) TYPE REF TO lcl_main.

    METHODS run.


  PROTECTED SECTION.

  PRIVATE SECTION.
    TYPES:
      ty_lt_versions TYPE STANDARD TABLE OF swd_versns WITH DEFAULT KEY.
    METHODS get_active_definition_key RETURNING VALUE(rs_wf_definition_key) TYPE swd_wfdkey.
    DATA mv_wf TYPE sww_task.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_result.
    ro_result->mv_wf = iv_wf.
  ENDMETHOD.



  METHOD run.

    DATA: ls_wf_definition_key TYPE swd_wfdkey,

          lo_wfd_xml           TYPE REF TO cl_xml_document_base,
          lo_wfd_export        TYPE REF TO if_swf_pdef_export,
          lt_versions          TYPE TABLE OF swd_versns,

          lo_node              TYPE REF TO if_ixml_element,
          lo_def               TYPE REF TO lcl_workflow_definition,
          lo_error             TYPE REF TO zcx_abapgit_exception,
          lv_retcode           TYPE sysubrc,
          lv_stream            TYPE string,
          lv_size              TYPE sytabix.

    TRY.
        lo_def = lcl_workflow_definition=>load( mv_wf ).
      CATCH zcx_abapgit_exception INTO lo_error.
        WRITE / lo_error->get_text( ).
    ENDTRY.

    ls_wf_definition_key = get_active_definition_key( ).

    CREATE OBJECT lo_wfd_export TYPE cl_wfd_convert_def_to_ixml.

    lo_wfd_xml = lo_wfd_export->convert( load_from_db = abap_true
                                         language = sy-langu
                                         wfd_key = ls_wf_definition_key ).

    IF lo_wfd_xml IS BOUND.

*  Blah foo bar lo_node->append_child( lo_wfd_xml->get_first_node( ) ).

      lo_wfd_xml->render_2_string(
        EXPORTING
          pretty_print = 'X'
        IMPORTING
          retcode      = lv_retcode
          stream       = lv_stream
          size         = lv_size ).
      cl_demo_output=>display( lv_stream ).
    ELSE.
      cl_demo_output=>display( 'No XML' ).
    ENDIF.

  ENDMETHOD.

  METHOD get_active_definition_key.

    DATA lt_versions TYPE ty_lt_versions.

    CALL FUNCTION 'SWD_GET_VERSIONS_OF_WORKFLOW'
      EXPORTING
        im_task          = mv_wf
        im_exetyp        = 'S'
      IMPORTING
        ex_active_wfdkey = rs_wf_definition_key
      TABLES
        ex_versions      = lt_versions.

  ENDMETHOD.



ENDCLASS.


PARAMETERS p_wf TYPE sww_task DEFAULT 'WS90000005'.

START-OF-SELECTION.
  lcl_main=>create( p_wf )->run( ).


CLASS lcl_text_lines DEFINITION.
  PUBLIC SECTION.
    METHODS add IMPORTING iv_str TYPE string.
    METHODS get RETURNING VALUE(rv_result) TYPE string.
  PRIVATE SECTION.
    DATA mv_text TYPE string.
ENDCLASS.

CLASS lcl_text_lines IMPLEMENTATION.

  METHOD add.
    mv_text = mv_text && iv_str."  && cl_abap_char_utilities=>newline.
  ENDMETHOD.

  METHOD get.
    rv_result = mv_text.
  ENDMETHOD.

ENDCLASS.


CLASS ltd_workflow DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  PUBLIC SECTION.
    CLASS-METHODS create IMPORTING iv_wf_id         TYPE sww_task
                         RETURNING VALUE(ro_result) TYPE REF TO ltd_workflow.
    METHODS get_xml RETURNING VALUE(rv_result) TYPE string.

  PRIVATE SECTION.
    DATA mo_cut TYPE REF TO lcl_workflow_definition.
    DATA mv_wfid TYPE sww_task.

    METHODS dummy FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltd_workflow IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_result.
    ro_result->mv_wfid = iv_wf_id.
  ENDMETHOD.


  METHOD get_xml.
    DATA lv_ts TYPE tzonref-tstamps.
    DATA lo_xml TYPE REF TO lcl_text_lines.

    CREATE OBJECT lo_xml.

    GET TIME STAMP FIELD lv_ts.

    lo_xml->add( |<workflow_exchange xmlns="http://www.sap.com/bc/bmt/wfm/def" type="internal" release="752" version="1.0" xml:lang="EN">| ).
    lo_xml->add( | <workflow id="{ mv_wfid }(0000)S">| ).
    lo_xml->add( |  <task>| ).
    lo_xml->add( |   <TASK>{ mv_wfid }</TASK>| ).
    lo_xml->add( |   <SHORT>zTest01</SHORT>| ).
    lo_xml->add( |  </task>| ).
    lo_xml->add( |  <header>| ).
    lo_xml->add( |   <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |   <VERSION>0000</VERSION>| ).
    lo_xml->add( |   <EXETYP>S</EXETYP>| ).
    lo_xml->add( |   <OBJID>90000005</OBJID>| ).
    lo_xml->add( |   <ACTIV>X</ACTIV>| ).
    lo_xml->add( |   <LANGUAGE>E</LANGUAGE>| ).
    lo_xml->add( |   <TASK>{ mv_wfid }</TASK>| ).
    lo_xml->add( |   <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |   <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |   <CREATED_AT>22:17:08</CREATED_AT>| ).
    lo_xml->add( |   <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   <CHANGED_BY>DEVELOPER</CHANGED_BY>| ).
    lo_xml->add( |   <CHANGED_ON>2021-05-07</CHANGED_ON>| ).
    lo_xml->add( |   <CHANGED_AT>22:17:08</CHANGED_AT>| ).
    lo_xml->add( |   <CHANGED_RL>752</CHANGED_RL>| ).
    lo_xml->add( |   <ACTIVAT_BY>DEVELOPER</ACTIVAT_BY>| ).
    lo_xml->add( |   <ACTIVAT_ON>2021-05-07</ACTIVAT_ON>| ).
    lo_xml->add( |   <ACTIVAT_AT>22:17:08</ACTIVAT_AT>| ).
    lo_xml->add( |   <ACTIVAT_RL>752</ACTIVAT_RL>| ).
    lo_xml->add( |   <ORIG_VERS>0000</ORIG_VERS>| ).
    lo_xml->add( |   <PRS_PROFIL>0002</PRS_PROFIL>| ).
    lo_xml->add( |   <ORIG_UUID>AAwpFpwyHtur7zKST/cGOA==</ORIG_UUID>| ).
    lo_xml->add( |  </header>| ).
    lo_xml->add( |  <workflow_container>| ).
    lo_xml->add( |   <CONTAINER>| ).
    lo_xml->add( |    <PROPERTIES>| ).
    lo_xml->add( |     <OWN_ID>| ).
    lo_xml->add( |      <INSTID>{ mv_wfid }0000S</INSTID>| ).
    lo_xml->add( |      <TYPEID>CL_SWF_CNT_WS_PERSISTENCE</TYPEID>| ).
    lo_xml->add( |      <CATID>CL</CATID>| ).
    lo_xml->add( |     </OWN_ID>| ).
    lo_xml->add( |     <INCLUDES>| ).
    lo_xml->add( |      <item>| ).
    lo_xml->add( |       <NAME>_TASK_HRS_CONTAINER</NAME>| ).
    lo_xml->add( |       <POR>| ).
    lo_xml->add( |        <INSTID>{ mv_wfid }</INSTID>| ).
    lo_xml->add( |        <TYPEID>CL_SWF_CNT_HRS_PERSISTENCE</TYPEID>| ).
    lo_xml->add( |        <CATID>CL</CATID>| ).
    lo_xml->add( |       </POR>| ).
    lo_xml->add( |      </item>| ).
    lo_xml->add( |     </INCLUDES>| ).
    lo_xml->add( |     <PROPSTRING>23</PROPSTRING>| ).
    lo_xml->add( |     <XMLVERSION>0002</XMLVERSION>| ).
    lo_xml->add( |     <INTERNAL>X</INTERNAL>| ).
    lo_xml->add( |    </PROPERTIES>| ).
    lo_xml->add( |    <ELEMENTS>| ).
    lo_xml->add( |     <A NAME="_ADHOC_OBJECTS:_Adhoc_Objects:" TYPE=":BO::h:0:0" PROPS="0C925A51" LTEXTS="EE014Ad Hoc ObjectsAd Hoc Objects of Workflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <B NAME="_ATTACH_OBJECTS:_Attach_Objects:" TYPE="SOFM:BO::h:0:0" PROPS="0C925A51" LTEXTS="EE011AttachmentsAttachments of Workflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <C NAME="_WF_INITIATOR:_Wf_Initiator:" TYPE="::WFSYST-INITIATOR:C:0:0" PROPS="0C003211" LTEXTS="EE018Workflow InitiatorInitiator of Workflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <D NAME="_WF_PRIORITY:_Wf_Priority:" TYPE="::SWFCN_TYPE_PRIORITY:N:0:0" PROPS="0C001A1" LTEXTS="EE008PriorityPriority of Workflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:">5</D>| ).
    lo_xml->add( |     <E NAME="_WI_GROUP_ID:_Wi_Group_ID:" TYPE=":BO::u:0:0" PROPS="0C921A11" LTEXTS="EE017Grouping Charact.Grouping Characteristic for Workflow Instances" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <F NAME="_WORKITEM:_Workitem:" TYPE="FLOWITEM:BO::u:0:0" PROPS="0C921A11" LTEXTS="EE008WorkflowWorkflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <G NAME="_WF_VERSION:_Wf_Version:" TYPE="::SWD_VERSIO:C:0:0" PROPS="0C000A11" LTEXTS="EE016Workflow VersionDefinition Version of this Workflow Instance" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <H NAME="_WF_NESTING_LEVEL:_WF_Nesting_Level:" TYPE="::SYINDEX:I:0:0" PROPS="0C001A31" LTEXTS="EE013Nesting DepthCurrent Subworkflow Nesting Depth" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <I NAME="_PREDECESSOR_WI:_Predecessor_Wi:" TYPE="WORKITEM:BO::u:0:0" PROPS="0C920011" LTEXTS="EE011PredecessorPrevious Work Item" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <J NAME="_RFC_DESTINATION:_Rfc_Destination:" TYPE="::RFCDEST:C:0:0" PROPS="0C001231" LTEXTS="EE015RFC DestinationRFC Destination" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <K NAME="_ATTACH_COMMENT_OBJECTS:_Attach_Comment_Objects:" TYPE="SOFM:BO::h:0:0" PROPS="0C925A71" LTEXTS="EE007CommentComment" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <L NAME="_START_EVENT_IDENTIFIER:_Start_Event_Identifier:" TYPE="CL_SWF_UTL_EVT_IDENTIFIER:CL::h:0:0" PROPS="0CC20231" LTEXTS="EE017ID of Start EventID of Start Event" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add(
|     <M NAME="_WF_TYPENAME_MAPPING:_WF_Typename_Mapping:" TYPE="::SWF_CNT_MAPPING_TAB:h:0:0" PROPS="0C120271" LTEXTS="EE022Relation of Type NamesRelation of Type Names (Original and Copy)" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <N NAME="_WF_START_QUERY:_WF_Start_Query:" TYPE="::SWF_STRING:g:0:0" PROPS="0C001231" LTEXTS="EE011Start QueryWorkflow Start Query in URL Syntax" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |     <O NAME="_WF_LAST_CALLBACK_WI:_WF_Last_Callback_Wi:" TYPE="WORKITEM:BO::u:0:0" PROPS="0C920031" LTEXTS="EE018Callback Work ItemCallback Work Item" CHGDTA="752:{ lv_ts }:DEVELOPER::00000000000000:"/>| ).
    lo_xml->add( |    </ELEMENTS>| ).
    lo_xml->add( |   </CONTAINER>| ).
    lo_xml->add( |  </workflow_container>| ).
    lo_xml->add( |  <texts>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <LANGUAGE>E</LANGUAGE>| ).
    lo_xml->add( |    <NODEID>0000999998</NODEID>| ).
    lo_xml->add( |    <TEXTTYP>ND</TEXTTYP>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |  </texts>| ).
    lo_xml->add( |  <steps>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000000001</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>XOR</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>STRT</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>O</MODELEMENT>| ).
    lo_xml->add( |    <EXP_TOKENS>001</EXP_TOKENS>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>01</NEST_LEVEL>| ).
    lo_xml->add( |    <START_NODE>X</START_NODE>| ).
    lo_xml->add( |    <XOR_FLAG>X</XOR_FLAG>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |    <CHANGED_BY>DEVELOPER</CHANGED_BY>| ).
    lo_xml->add( |    <CHANGED_ON>{ sy-datum DATE = ISO }</CHANGED_ON>| ).
    lo_xml->add( |    <CHANGED_AT>{ sy-uzeit TIME = ISO }</CHANGED_AT>| ).
    lo_xml->add( |    <CHANGED_RL>752</CHANGED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000000002</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>Undefined</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>VOID</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>S</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000002</BLOCKID>| ).
    lo_xml->add( |    <PAR_BLCKID>0000000001</PAR_BLCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>02</NEST_LEVEL>| ).
    lo_xml->add( |    <START_NODE>X</START_NODE>| ).
    lo_xml->add( |    <DES_SZ_EXP>%ZONLO%</DES_SZ_EXP>| ).
    lo_xml->add( |    <LAT_SZ_EXP>%ZONLO%</LAT_SZ_EXP>| ).
    lo_xml->add( |    <DES_EZ_EXP>%ZONLO%</DES_EZ_EXP>| ).
    lo_xml->add( |    <LAT_EZ_EXP>%ZONLO%</LAT_EZ_EXP>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000000003</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>Undefined</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>EVOI</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>E</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000002</BLOCKID>| ).
    lo_xml->add( |    <PAR_BLCKID>0000000001</PAR_BLCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>02</NEST_LEVEL>| ).
    lo_xml->add( |    <END_NODE>X</END_NODE>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000999502</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>Complete Workflow</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>EFUN</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>S</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>01</NEST_LEVEL>| ).
    lo_xml->add( |    <START_NODE>X</START_NODE>| ).
    lo_xml->add( |    <DES_SZ_EXP>%ZONLO%</DES_SZ_EXP>| ).
    lo_xml->add( |    <LAT_SZ_EXP>%ZONLO%</LAT_SZ_EXP>| ).
    lo_xml->add( |    <DES_EZ_EXP>%ZONLO%</DES_EZ_EXP>| ).
    lo_xml->add( |    <LAT_EZ_EXP>%ZONLO%</LAT_EZ_EXP>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000999503</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>Workflow completed</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>EVTG</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>E</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>01</NEST_LEVEL>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000999998</NODEID>| ).
    lo_xml->add( |    <NODETYPE>OFF</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>O</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <XOR_FLAG>X</XOR_FLAG>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <WFD_ID>{ mv_wfid }</WFD_ID>| ).
    lo_xml->add( |    <VERSION>0000</VERSION>| ).
    lo_xml->add( |    <EXETYP>S</EXETYP>| ).
    lo_xml->add( |    <NODEID>0000999999</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>XOR</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>END</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>O</MODELEMENT>| ).
    lo_xml->add( |    <FORK_TOKEN>001</FORK_TOKEN>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>01</NEST_LEVEL>| ).
    lo_xml->add( |    <END_NODE>X</END_NODE>| ).
    lo_xml->add( |    <XOR_FLAG>X</XOR_FLAG>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>2021-05-07</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>22:17:03</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |    <CHANGED_BY>DEVELOPER</CHANGED_BY>| ).
    lo_xml->add( |    <CHANGED_ON>2021-05-07</CHANGED_ON>| ).
    lo_xml->add( |    <CHANGED_AT>22:17:03</CHANGED_AT>| ).
    lo_xml->add( |    <CHANGED_RL>752</CHANGED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <NODEID>0000999504</NODEID>| ).
    lo_xml->add( |    <DESCRIPT>Workflow started</DESCRIPT>| ).
    lo_xml->add( |    <NODETYPE>SGVT</NODETYPE>| ).
    lo_xml->add( |    <MODELEMENT>E</MODELEMENT>| ).
    lo_xml->add( |    <BLOCKID>0000000001</BLOCKID>| ).
    lo_xml->add( |    <NEST_LEVEL>01</NEST_LEVEL>| ).
    lo_xml->add( |    <CREATED_BY>DEVELOPER</CREATED_BY>| ).
    lo_xml->add( |    <CREATED_ON>{ sy-datum DATE = ISO }</CREATED_ON>| ).
    lo_xml->add( |    <CREATED_AT>{ sy-uzeit TIME = ISO }</CREATED_AT>| ).
    lo_xml->add( |    <CREATED_RL>752</CREATED_RL>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |  </steps>| ).
    lo_xml->add( |  <lines>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000000002</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000000001</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000000002</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000000003</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000000002</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000000003</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000000004</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000000003</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000999999</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000000005</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000999502</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000999503</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000000006</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000999999</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000999502</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |   <item>| ).
    lo_xml->add( |    <LINEID>0000999501</LINEID>| ).
    lo_xml->add( |    <PRED_NODE>0000999504</PRED_NODE>| ).
    lo_xml->add( |    <SUCC_NODE>0000000001</SUCC_NODE>| ).
    lo_xml->add( |   </item>| ).
    lo_xml->add( |  </lines>| ).
    lo_xml->add( | </workflow>| ).
    lo_xml->add( |</workflow_exchange>| ).

    rv_result = lo_xml->get( ).

  ENDMETHOD.

  METHOD dummy.
  ENDMETHOD.

ENDCLASS.

CLASS ltc_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    CONSTANTS c_test_wf TYPE sww_task VALUE 'WS90000005'.
    DATA: mo_cut TYPE REF TO lcl_workflow_definition.
    METHODS:
      setup,
      invalid_id_doesnt_load FOR TESTING RAISING cx_static_check,
      serialize FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltc_test IMPLEMENTATION.

  METHOD setup.
    TRY.
        mo_cut = lcl_workflow_definition=>load( c_test_wf ).
      CATCH zcx_abapgit_exception.
        cl_abap_unit_assert=>fail( level = if_aunit_constants=>fatal ).
    ENDTRY.
  ENDMETHOD.

  METHOD invalid_id_doesnt_load.
    DATA lo_cut TYPE REF TO lcl_workflow_definition.
    TRY.
        lo_cut = lcl_workflow_definition=>load( 'WS99999990' ).
        cl_abap_unit_assert=>fail( 'Exception not raised' ).
      CATCH zcx_abapgit_exception.
        "As expected
    ENDTRY.
  ENDMETHOD.

  METHOD serialize.
    DATA: lo_mock TYPE REF TO ltd_workflow,
          lv_xml  TYPE string,
          lv_exp  TYPE string.

    lo_mock = ltd_workflow=>create( c_test_wf ).

    lv_xml = mo_cut->serialize( ).
    lv_xml = zcl_abapgit_xml_pretty=>print(
               iv_xml           = lv_xml
               iv_ignore_errors = abap_false
               iv_unpretty      = abap_false ).
    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_xml
      WITH '<?xml version="1.0" encoding="utf-8"?>'.

    lv_exp = lo_mock->get_xml( ).
    lv_exp = zcl_abapgit_xml_pretty=>print(
               iv_xml           = lv_exp
               iv_ignore_errors = abap_false
               iv_unpretty      = abap_false ).
    REPLACE FIRST OCCURRENCE
      OF REGEX '<\?xml version="1\.0" encoding="[\w-]+"\?>'
      IN lv_exp
      WITH '<?xml version="1.0" encoding="utf-8"?>'.

    cl_abap_unit_assert=>assert_equals( act = lv_xml
                                        exp = lv_exp ).

  ENDMETHOD.


ENDCLASS.
