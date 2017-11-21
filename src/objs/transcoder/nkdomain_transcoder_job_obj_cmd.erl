-module(nkdomain_transcoder_job_obj_cmd).
-export([cmd/2]).
-include_lib("nktranscoder/include/nktranscoder.hrl").
-include_lib("nkservice/include/nkservice.hrl").

cmd(<<"start">>, #nkreq{srv_id=_SrvId, data=#{domain := Domain,
                                              file_id := FileId,
                                              srv_id := SrvId, 
                                              user_id := UserId }}=Req) ->
    case nkdomain:get_obj(FileId) of
        {ok, #{obj_id := FileId, <<"file">> := #{ content_type := Mime}=File}} ->
            case nkdomain_file_obj:get_store(File) of
                {ok, _StoreId, #{class := Store}} ->
                    case map_store_type(Store) of
                        {ok, StoreType} ->
                            case nkdomain_transcoder_server_obj:get_default() of
                                {ok, TranscoderId, Transcoder} ->
                                    Args = #{ input => #{ type => StoreType,
                                                          path => FileId,
                                                          content_type => Mime },
                                              output => #{ type => StoreType,
                                                           path => nkdomain_file_obj:make_file_id() }},
                                    ?DEBUG("Creating a new transcoding job using server: ~p", [TranscoderId ]),
                                    case nkdomain_transcoder_job_obj:create(SrvId, Domain, UserId, TranscoderId, Args) of
                                        {ok, JobPath, #{ obj_id := JobId }} ->
                                            {ok, _, Req2} = subscribe(SrvId, JobId, Req),
                                            ?DEBUG("Starting transcoding job ~p (~p) using server ~p",
                                               [JobPath, JobId, TranscoderId]),
                                            Args2 = Args#{ job_id => JobId },
                                            SrvId:nktranscoder_transcode(SrvId, Transcoder, Args2),
                                            {ok, #{ status => <<"in progress">>}, Req2 };
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

subscribe(SrvId, JobId, Req) -> 
    Ev= nkdomain_transcoder_job_obj:nkevent(SrvId, JobId),
    nkservice_api:api(<<"event/subscribe">>, Ev, Req).


map_store_type(fs) -> {ok, <<"fs">>};
map_store_type(filesystem) -> {ok, <<"fs">>};
map_store_type(s3) -> {ok, <<"s3">>};
map_store_type(s3_mini) -> {ok, <<"s3">>};
map_store_type(Other) -> {error, {unsupported_store, Other}}.

