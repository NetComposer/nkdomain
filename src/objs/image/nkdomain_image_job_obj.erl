-module(nkdomain_image_job_obj).
-export([object_info/0, object_parse/2, object_schema_types/0, object_es_mapping/0]).
-export([create/4, start/7]).
-export([subscribe/2, unsubscribe/2, notify/2]).
-include_lib("nkimage/include/nkimage.hrl").
-include_lib("nkevent/include/nkevent.hrl").
-include("../../../include/nkdomain.hrl").

object_info() ->
    #{ type => ?IMAGE_JOB, 
       schema_type => 'ImageJob',
       subtype => []
     }.

object_schema_types() ->
    #{
        'ImageJob' => #{
            fields => #{
            },
            is_object => true,
            comment => "An image processing job"
        }
    }.

object_es_mapping() ->
    not_indexed.

object_parse(_Mode, _Obj) ->
    #{ input => binary,
       output => binary,
       processor_id => binary,
       status => binary,
       progress => float,
       width => integer,
       height => integer,
       format => binary,
       callback_url => binary,
       options => map,
       '__mandatory' => [ input, 
                          progress,
                          status,
                          processor_id],
       '__defaults' => #{ status => <<"not_started">>,
                          progress => 0,
                          callback_url => <<"">>,
                          options => #{} }
     }.

make_job_id() ->
    <<?IMAGE_JOB/binary, <<"-">>/binary, (nklib_util:luid())/binary>>.

subscribe(#{srv_id := SrvId,
            obj_id := JobId}, Req) ->
    Ev = nkevent(SrvId, JobId),
    {ok, _, Req2} = nkservice_api:api(<<"event/subscribe">>, Ev, Req),
    {ok, Req2}.

unsubscribe(SrvId, JobId) -> 
    Event = nkevent(SrvId, JobId),
    nkevent:unreg(Event).

notify(JobId, ThumbnailInfo ) ->
    Event = nkevent(?NKROOT, JobId, ThumbnailInfo),
    lager:debug("~p notifying thumbnail event: ~p", [?MODULE, Event]),
    nkevent:send(Event).

nkevent(_SrvId, JobId) -> 
    #{ srv_id => ?NKROOT,
       class => ?DOMAIN_EVENT_CLASS,
       subclass => ?IMAGE_JOB,
       type => ?IMAGE_JOB,
       obj_id => JobId }.

nkevent(_SrvId, JobId, Body) -> 
    #nkevent{class=?DOMAIN_EVENT_CLASS,
             subclass=?IMAGE_JOB,
             type=?IMAGE_JOB,
             obj_id=JobId,
             body = Body,
             srv_id=?NKROOT}.


create(SrvId, Domain, UserId, #{file_id := FileId}=Req) -> 
    case nkdomain_image_processor_obj:get_default() of
        {ok, ProcessorId, Processor} ->
            case find_file_and_store(FileId) of
                {ok, File, Store} -> 
                    {_JobId, JobObj} = job_obj(SrvId, Domain, UserId, 
                                               job_props(FileId, ProcessorId, Req)),
                    case nkdomain_obj_make:create(JobObj) of
                        {ok, _Job, _} ->
                            {ok, JobObj, Processor, File, Store};
                        {error, Error} -> 
                            {error, Error}
                    end;
                {error, Error} -> 
                    {error, Error}
            end;
        {error, Error} -> 
            {error, Error}
    end.

start(SrvId, Domain, UserId, File, Store, Processor,  #{ obj_id := JobId,
                                                           ?IMAGE_JOB := JobData }) -> 
    case nkimage(File, Store, SrvId, Processor, JobData) of
        {ok, Body} ->
            case upload(Store, Body) of
                {ok, OutputFileId} ->
                    case create_output_file(SrvId, Domain, UserId, 
                                            Store, File, 
                                            JobData, OutputFileId, byte_size(Body)) of 
                        ok -> 
                            case finish(JobId, OutputFileId) of
                                {ok, Job2} ->
                                    notify(JobId, Job2),
                                    {ok, Job2};
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
            case finish(JobId) of
                {ok, Job2} -> 
                    notify(JobId, Job2),
                    {ok, Job2};
                {error, Error} ->
                    {error, Error}
            end
    end.


job_obj(SrvId, Domain, UserId, Props) -> 
    JobId = make_job_id(),
    {JobId, #{ srv_id => SrvId,
       obj_id => JobId,
       type => ?IMAGE_JOB,
       domain_id => Domain,
       created_by => UserId,
       ?IMAGE_JOB => Props
     }}.

job_props(FileId, ProcessorId, #{action := Action,
                                 format := Fmt,
                                 width := Width,
                                 height := Height,
                                 callback_url:=CallbackUrl,
                                 options := Options }) -> 
    #{ input => FileId,
       action => Action,
       output => <<>>,
       format => Fmt,
       width => Width,
       height => Height,
       status => <<"not_started">>,
       progress => 0,
       processor_id => ProcessorId,
       callback_url => CallbackUrl,
       options => Options };


job_props(FileId, ProcessorId, #{action := Action,
                                 format := Fmt,
                                 callback_url:=CallbackUrl, 
                                 options := Options }) -> 
    #{ input => FileId,
       action => Action,
       output => <<>>,
       format => Fmt,
       status => <<"not_started">>,
       progress => 0,
       processor_id => ProcessorId,
       callback_url => CallbackUrl,
       options => Options };

job_props(FileId, ProcessorId, #{action := _}=Req) -> 
    job_props(FileId, ProcessorId, Req#{callback_url=> <<>>}).

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

finish_with(JobId, Data) ->
    case nkdomain:get_obj(JobId) of
        {ok, #{ ?IMAGE_JOB := JobData}=Job} -> 
            JobData2 = maps:merge(JobData, Data),
            case nkdomain:update(JobId, #{ ?IMAGE_JOB => JobData2}) of  
                {ok, _} ->
                    {ok, Job#{ ?IMAGE_JOB => JobData2 }};
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} -> 
            {error, Error}
    end.


finish(JobId, OutputId) -> 
    finish_with(JobId, #{ status => <<"finished">>,
                                   progress => 100,
                                   output => OutputId }).

finish(JobId) -> 
    finish_with(JobId, #{ status => <<"error">>,
                                   progress => 0,
                                   output => <<>> }).
    
create_output_file(SrvId, Domain, UserId, 
                   #{id:=StoreId}, 
                   #{obj_id:=FileId, ?DOMAIN_FILE:=#{ content_type := Mime }}, 
                   Req, OutputFileId, Size) ->
    
    OutputMime = mime(Req, Mime),
    LinkToOriginal = #{ type => original,
                        id => FileId},

    case nkdomain_file_obj:create(StoreId, UserId, Domain, OutputFileId, 
                                  SrvId, OutputMime, Size, [LinkToOriginal]) of
        {ok, _, _} -> 
            case nkdomain:get_obj(FileId) of
                 {ok, #{ <<"file">> := #{ links := Links} }} ->
                    NewLinks = [#{ type => link_name(Req),
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


mime(#{format := CT}, _Default) ->
    CT;

mime(#{content_type :=CT}, _Default) ->
    CT;

mime(_, Default) ->
    Default.

link_name(#{ action := resize, 
             width := Width,
             height := Height }) ->
   binary_join([ <<"thumb">>,
                 nklib_util:to_binary(Width),
                 nklib_util:to_binary(Height) ], <<"-">>);

link_name(#{ action := convert, 
             format := <<"image/", Fmt/binary>>}) ->
    Fmt;

link_name(#{ action := convert, 
             format := <<"application/", Fmt/binary>>}) ->
    Fmt.

nkimage(#{obj_id:=FileId, ?DOMAIN_FILE:=#{ content_type := SourceMime }=File}, 
          Store, SrvId, Processor, #{ format := DestMime }=Req) ->
    
    case nkfile:download(?NKROOT, Store, File#{name=>FileId}) of
        {ok, _, Body} ->
            Req2 = Req#{body => Body, 
                        from => SourceMime,
                        to => DestMime },
            Req3 = maps:remove(format, Req2),
            nkimage:process(SrvId, Processor, Req3);
        {error, Error} -> 
            {error, Error}
    end.

upload(#{id := StoreId }=Store, Body) ->
    FileId = nkdomain_file_obj:make_file_id(),
    case nkdomain_file_obj:upload(StoreId, Store, FileId, Body) of 
        {ok, _} ->
            {ok, FileId};
        {error, Error} ->
            {error, Error}
    end.

binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

