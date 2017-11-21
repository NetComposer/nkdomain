-module(nkdomain_transcoder_job_obj_syntax).
-export([syntax/2]).

syntax(<<"start">>, Syntax) ->
    Syntax#{
        file_id => binary,
        user_id  => binary,
        domain => binary,
        srv_id => binary,
        '__mandatory' => [ file_id,
                           user_id,
                           domain,
                           srv_id ]
    }.
