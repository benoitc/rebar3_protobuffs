-module('rebar3_protobuffs_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'compile').
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
            {example, "rebar3 protobuffs compile"},
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
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         OutDir = rebar_app_info:out_dir(AppInfo),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.proto\$"),
         CompileFun = fun(Source, _Opts) ->
                              proto_compile(Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],
    {ok, State}.


proto_compile(Source, OutDir) ->
    %% make sure an "include" exists, fixing waiting for a fix
    filelib:ensure_dir("include/"),
    IncDir = filename:join([OutDir, "include"]),
    filelib:ensure_dir(IncDir ++ "/"),
    EbinDir = filename:join(OutDir, "ebin") ++ "/",
    filelib:ensure_dir(EbinDir ++ "/"),
    ok = protobuffs_compile:scan_file(Source, [{output_include_dir, IncDir},
                                               {output_ebin_dir, EbinDir}]).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
