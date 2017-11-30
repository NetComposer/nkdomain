-module(nkdomain_transcoder_job_obj).
-export([object_info/0, object_parse/2, object_schema_types/0, object_es_mapping/0]).
-export([object_api_syntax/2, object_api_cmd/2]).
-export([create/4, subscribe/2, start/4, update/2]).
-export([unsubscribe/2, notify/2, nkevent/2]).
-export([update_output_file/2]).
-export([nktranscoder_event/1]).
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

subscribe(#{srv_id := SrvId,
            obj_id := JobId}, Req) ->
    Ev = nkevent(SrvId, JobId),
    {ok, _, Req2} = nkservice_api:api(<<"event/subscribe">>, Ev, Req),
    {ok, Req2}.

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

create(SrvId, Domain, UserId, #{file_id := FileId}=Req) -> 
    case nkdomain_transcoder_server_obj:get_default() of
        {ok, TranscoderId, Transcoder} ->
            case find_file_and_store(FileId) of
                {ok, File, Store} ->
                    case map_store_type(Store) of
                        {ok, StoreType} ->
                            OutputFileId = nkdomain_file_obj:make_file_id(),
                            Req2 = Req#{store_type => StoreType,
                                        output => OutputFileId},
                            JobObj = job_obj(SrvId, Domain, UserId,
                                               job_props(File, TranscoderId, Req2)),
                            case nkdomain_obj_make:create(JobObj) of
                                {ok, _, _} ->
                                    case create_output_file(Store, JobObj) of 
                                        ok -> 
                                            {ok, JobObj, Transcoder, File};  
                                        {error, Error} ->
                                            {error, Error}
                                    end;
                                {error, Error} ->
                                    {error, Error}
                            end;
                        {error, Error} -> 
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

create_output_file(#{id:=StoreId}, #{ srv_id := SrvId,
                                      domain_id := Domain,
                                      created_by := UserId,
                                      ?TRANSCODER_JOB := #{ input := FileId,
                                                            output := OutputFileId,
                                                            content_type := Mime }}) -> 
                    
    LinkToOriginal = #{ type => original,
                        id => FileId},
    case nkdomain_file_obj:create(StoreId, UserId, Domain, OutputFileId,
                                  SrvId, Mime, 0, [LinkToOriginal]) of
        {ok, _, _} ->
            case nkdomain:get_obj(FileId) of
                 {ok, #{ <<"file">> := #{ links := Links} }} ->
                    NewLinks = [#{ type => <<"transcoding">>, 
                                   id => OutputFileId
                                 }|Links],
                    case nkdomain:update(FileId, #{ ?DOMAIN_FILE =>
                                                    #{ links => NewLinks }}) of
                        {ok, _} ->
                            ok;
                        {error, Error} ->
                            {error, Error}
                    end;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.

start(SrvId, File, Transcoder, Job) ->
    #{ obj_id := FileId,
       ?DOMAIN_FILE := #{ content_type := InputMime }} = File,
        
    #{ obj_id := JobId,
       ?TRANSCODER_JOB := #{ content_type := OutputMime,
                             output := OutputFileId,
                             store_type := StoreType,
                             callback_url := _CallbackUrl }} = Job,

    Args = #{ callback => {?MODULE, nktranscoder_event, [JobId]},
              input => #{ type => StoreType,
                          path => FileId,
                          content_type => InputMime },
              output => #{ type => StoreType,
                           path => OutputFileId,
                           content_type => OutputMime }},

    nktranscoder:transcode(SrvId, Transcoder, Args).

nktranscoder_event([JobId, Ev, Pid, Msg]) ->
    ?DEBUG("=> got event ~p with Pid: ~p, JobId: ~p, Msg: ~p", [Ev, Pid, JobId, Msg]),
    case update(JobId, #{ status => Ev,
                          pid => Pid,
                          info => Msg }) of 
        ok -> 
            ?DEBUG("succesfully updated transcoding job ~p", [JobId]);
        {error, Error} ->
            ?ERROR("error while updating transcoding job ~p: ~p", [JobId, Error])
    end.

mime(#{ format:= <<>>}, Default) -> 
    Default;

mime(#{ format:= CT}, _Default) ->
    CT;

mime(_, Default) -> 
    Default.

job_obj(SrvId, Domain, UserId, Props) ->
    #{ srv_id => SrvId,
       obj_id => make_job_id(),
       type => ?TRANSCODER_JOB,
       domain_id => Domain,
       created_by => UserId,
       ?TRANSCODER_JOB=> Props
     }.

job_props(#{ obj_id := FileId, 
             ?DOMAIN_FILE := #{ content_type := InputMime }}, 
          TranscoderId, #{ store_type := StoreType,
                                   output := Output,
                                   callback_url := CallbackUrl}=Req) -> 
    
    OutputMime = mime(Req, InputMime),
    
    #{ content_type => OutputMime,
       input => FileId,
       output => Output,
       store_type => StoreType,
       status => <<"in progress">>,
       progress => 0,
       transcoder_id => TranscoderId,
       callback_url => CallbackUrl }.

map_store_type(#{ class := fs}) -> {ok, <<"fs">>};
map_store_type(_) -> {ok, <<"s3">>}.


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

update_output_file(FileId, #{ size := Size}) ->
    nkdomain_file_obj:update(FileId, #{ size => Size });

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

find_file_and_store(FileId) -> 
    case nkdomain:get_obj(FileId) of
        {ok, File} ->
            case nkdomain_file_obj:get_store(File) of
                {ok, StoreId, Store} ->
                    {ok, File, Store#{id => StoreId}};
                {error, Error} -> 
                    {error, Error}
            end;
        {error, Error} -> 
            {error, Error}
    end.
