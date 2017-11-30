-module(nkdomain_transcoder_server_obj).
-export([object_info/0, object_parse/2, object_schema_types/0, object_es_mapping/0]).
-export([get_default/0]).
-include_lib("nktranscoder/include/nktranscoder.hrl").
-include("../../../include/nkdomain.hrl").

object_info() ->
    #{ type => ?TRANSCODER_SERVER,
       schema_type => 'Transcoder',
       subtype => [?DOMAIN_CONFIG]
     }.

object_parse(_Mode, Obj) ->
    #{?TRANSCODER_SERVER:=Config} = Obj,
    case nktranscoder:parse_transcoder(?NKROOT, Config, #{path=>?TRANSCODER_SERVER}) of
        {ok, Server, UnknownTypes} ->
            {type_obj, Server, UnknownTypes};
        {error, Error} ->
            {error, Error}
    end.

%% @doc
object_schema_types() ->
    #{
        'Transcoder' => #{
            fields => #{
            },
            is_object => true,
            comment => "A Transcoder Server"
        }
    }.

%% @private
object_es_mapping() ->
    not_indexed.


get_default() ->
    case ?CALL_NKROOT(config_nkdomain_nkroot, []) of
        #nkdomain_config_cache{transcoder_server=TranscoderId} ->
            do_get_transcoder(TranscoderId);
        _ ->
            {error, transcoder_server_id_missing}
    end.

%% @private
do_get_transcoder(TranscoderId) ->
    case nkdomain_db:load(TranscoderId) of
        #obj_id_ext{obj_id=TranscoderObjId, type = ?TRANSCODER_SERVER} ->
            case nkdomain:get_obj(TranscoderObjId) of
                {ok, #{?TRANSCODER_SERVER:=Data}} ->
                    {ok, TranscoderObjId, Data};
                _ ->
                    {error, transcoder_server_id_invalid}
            end;
        _ ->
            {error, transcoder_id_invalid}
    end.
