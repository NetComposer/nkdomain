-module(nkdomain_transcoder_job_obj).
-export([object_info/0, object_parse/2, object_schema_types/0, object_es_mapping/0]).
-export([object_api_syntax/2, object_api_cmd/2]).
-export([make_job_id/0]).
%-export([object_sync_op/3]).
-export([create/7, update/2, subscribe/2, unsubscribe/2, notify/2, nkevent/2]).
-export([update_output_file/2]).
-include_lib("nktranscoder/include/nktranscoder.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include("../../../include/nkdomain.hrl").

object_info() ->
    #{ type => ?TRANSCODER_JOB,
       schema_type => 'TranscoderJob',
       subtype => []
     }.

object_schema_types() ->
    #{
        'TranscoderJob' => #{
            fields => #{
                contentType => {no_null, string},
                storeType => {no_null, string},
                input => {no_null, string},
                output => {no_null, string},
                serverId => {no_null, string}
            },
            is_object => true,
            comment => "A transcoding job"
        }
    }.

object_es_mapping() ->
    not_indexed.

object_parse(update, _Obj) ->
    #{ status => binary,
       progress => float
     };

object_parse(create, _Obj) ->
    #{ content_type => binary,
       store_type => binary,
       input => binary,
       output => binary,
       transcoder_id => binary,
       status => binary,
       progress => float,
       callback_url => binary,
       '__mandatory' => [ content_type, 
                          store_type, 
                          input, 
                          output, 
                          transcoder_id],
       '__defaults' => #{ status => <<"not_started">>,
                          progress => 0,
                          callback_url => <<"">> }
     };

object_parse(load, Obj) ->
    maps:merge( object_parse(create, Obj), 
                object_parse(update, Obj)).

object_api_syntax(Cmd, Syntax) ->
    nkdomain_transcoder_job_obj_syntax:syntax(Cmd, Syntax).

object_api_cmd(Cmd, Req) ->
    nkdomain_transcoder_job_obj_cmd:cmd(Cmd, Req).

make_job_id() ->
    <<"transcoder.job-", (nklib_util:luid())/binary>>.


subscribe(SrvId, JobId) -> 
    Event = nkevent(SrvId, JobId),
    nkapi_server:subscribe(self(), Event),
    ?DEBUG("~p subscribed to events on transcoder job ~p", [self(),
                                                            JobId]),
    ok.

unsubscribe(SrvId, JobId) -> 
    Event = nkevent(SrvId, JobId),
    nkevent:unreg(Event).

notify(JobId, TranscodingInfo) ->
    Event = nkevent(?NKROOT, JobId, TranscodingInfo),
    ?DEBUG("notifying transcoder event: ~p", [Event]),
    nkevent:send(Event).


nkevent(_SrvId, JobId) -> 
    #{ srv_id => ?NKROOT,
       class => ?DOMAIN_EVENT_CLASS,
       subclass => ?TRANSCODER_JOB,
       type => ?TRANSCODER_JOB,
       obj_id => JobId }.

nkevent(_SrvId, JobId, Body) -> 
    #nkevent{class=?DOMAIN_EVENT_CLASS,
             subclass=?TRANSCODER_JOB,
             type=?TRANSCODER_JOB,
             obj_id=JobId,
             body = Body,
             srv_id=?NKROOT}.

create(SrvId, Domain, UserId, TranscoderId, StoreId, CallbackUrl, #{ input := #{ type := StoreType,
                                                          path := Input,
                                                          content_type := ContentType },
                                              output := #{ type := StoreType,
                                                           path := Output }}) ->
    JobId = make_job_id(),
    Obj = #{
      srv_id => SrvId,
      obj_id => JobId,
      type => ?TRANSCODER_JOB,
      domain_id => Domain,
      created_by => UserId,
      ?TRANSCODER_JOB => #{ content_type => ContentType,
                            input => Input,
                            output => Output,
                            store_type => StoreType,
                            transcoder_id => TranscoderId,
                            callback_url => CallbackUrl }},
    
    OutputContentType = <<"video/mp4">>,
    case nkdomain_obj_make:create(Obj) of
        {ok, #obj_id_ext{path=Path}, _Unknown} ->
            case nkdomain_file_obj:create(StoreId, UserId, Domain, Output, SrvId, OutputContentType, 0, [#{ type => original, id => Input }]) of
                {ok, _, _} ->
                    case nkdomain_file_obj:update(Input, #{ links => [#{ type => transcoding,
                                                                         id => Output }]}) of
                        {ok, []} ->
                            {ok, Path, Obj};
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

update(JobId, JobData) ->
    case validate(JobData) of 
        {ok, JobData2} ->
            case nkdomain:update(JobId, #{?TRANSCODER_JOB => JobData2}) of
                {ok, _} ->
                    case nkdomain:get_obj(JobId) of
                        {ok, #{ ?TRANSCODER_JOB := #{ output := OutputFile }}=JobData3} ->
                            io:format("updating transcoding output file: ~p~n", [JobData2]),
                            case update_output_file(OutputFile, JobData2) of
                                {ok, _} -> 
                                    notify(JobId, JobData3);
                                {error, Error} ->
                                    {error, Error}
                            end;
                        Other ->
                            {error, Other}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} -> 
            {error, Error}
    end.

update_output_file(FileId, #{ size := Size,
                            content_type := ContentType }) ->
    nkdomain_file_obj:update(FileId, #{ content_type => ContentType,
                                        size => Size });

update_output_file(FileId, #{ content_type := ContentType }) ->
    nkdomain_file_obj:update(FileId, #{ content_type => ContentType });

update_output_file(_FileId, _) -> {ok, []}.

validate(#{ status := Status, info := none }) ->
    {ok, #{ status =>  Status,
            progress => 0 }};

validate(#{ status := Status, info := #{ <<"progress">> := Progress, 
                                         <<"content_type">> := ContentType,
                                         <<"filesize">> := FileSize }}) ->
    {ok, #{ status => Status,
            progress => Progress,
            content_type => ContentType,
            size => FileSize
          }};

validate(#{ status := Status, info := #{ <<"progress">> := Progress, 
                                         <<"content_type">> := ContentType }}) ->
    {ok, #{ status => Status,
            progress => Progress,
            content_type => ContentType
          }};

validate(#{ status := Status, info := #{ <<"progress">> := Progress }}) ->
    {ok, #{ status => Status,
            progress => Progress }};

validate(#{ status := Status }) ->
    {ok, #{ status => Status }}.
