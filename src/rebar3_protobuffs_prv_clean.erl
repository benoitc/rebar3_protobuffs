-module(rebar3_protobuffs_prv_clean).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'clean').
-define(NAMESPACE, protobuffs).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, ?NAMESPACE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 protobuffs clean"},
            {opts, []},
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,

    [begin
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.proto\$"),
         proto_clean(AppInfo, FoundFiles)
     end || AppInfo <- Apps],
    {ok, State}.

proto_clean(AppInfo, ProtoFiles) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    HrlFiles = [hrl_file(OutDir, Proto) || Proto <- ProtoFiles ],
    delete_each(HrlFiles).

hrl_file(OutDir, Proto) ->
    filename:join([OutDir, "include", filename:basename(Proto, ".proto") ++ "_pb.hrl"]).

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            rebar_api:error("Failed to delete ~s: ~p", [File, Reason])
    end,
    delete_each(Rest).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
