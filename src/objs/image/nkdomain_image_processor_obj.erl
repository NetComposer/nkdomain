-module(nkdomain_image_processor_obj).

-export([object_info/0, object_parse/2, object_schema_types/0, object_es_mapping/0]).
-export([get_default/0, object_admin_info/0]).

-include_lib("nkimage/include/nkimage.hrl").
-include("nkdomain.hrl").



%% ===================================================================
%% Types
%% ===================================================================



%% ===================================================================
%% API
%% ===================================================================


%% @doc
-spec get_default() ->
    {ok, #obj_id_ext{}, map()} | {error, term()}.

get_default() ->
    case ?CALL_NKROOT(config_nkdomain_nkroot, []) of
        #nkdomain_config_cache{image_processor=ProcessorId} ->
            do_get_processor(ProcessorId);
        _ ->
            {error, image_processor_id_missing}
    end.



%% ===================================================================
%% nkdomain_obj behaviour
%% ===================================================================


object_info() ->
    #{ type => ?IMAGE_PROCESSOR,
       schema_type => 'ImageProcessor',
       subtype => [?DOMAIN_CONFIG]
     }.


object_parse(_Mode, Obj) ->
    #{?IMAGE_PROCESSOR:=Config} = Obj,
    case nkimage:parse_processor(?NKROOT, Config, #{path=>?IMAGE_PROCESSOR}) of
        {ok, Processor, UnknownTypes} ->
            {type_obj, Processor, UnknownTypes};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
object_schema_types() ->
    #{
        'Processor' => #{
            fields => #{
            },
            is_object => true,
            comment => "An image processor"
        }
    }.


%% @doc
object_admin_info() ->
    #{
        class => resource,
        weight => 2101,
        type_view_mod => nkdomain_image_processor_obj_type_view,
        obj_view_mod => nkdomain_image_processor_obj_view
    }.


%% @private
object_es_mapping() ->
    not_indexed.



%% ===================================================================
%% Internal
%% ===================================================================


%% @private
do_get_processor(ProcessorId) ->
    case nkdomain_db:load(ProcessorId) of
        #obj_id_ext{obj_id=ProcessorObjId, type = ?IMAGE_PROCESSOR} ->
            case nkdomain:get_obj(ProcessorObjId) of
                {ok, #{?IMAGE_PROCESSOR:=Data}} ->
                    {ok, ProcessorObjId, Data};
                _ ->
                    {error, image_processor_id_invalid}
            end;
        _ ->
            {error, image_processor_id_invalid}
    end.