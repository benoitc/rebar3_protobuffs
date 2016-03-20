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
            {opts, [{imports_dir, $i, "imports_dir", string, "imports dir"}]},
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

    {Args, _} = rebar_state:command_parsed_args(State),
    ImportsDir =
        case proplists:get_value(imports_dir, Args) of
            undefined ->
                ProtobuffsOpts = rebar_state:get(State, protobuffs_opts, []),
                proplists:get_value(imports_dir, ProtobuffsOpts, []);
            ImportsDir ->
                ImportsDir
        end,

    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         IncDir = filename:join(rebar_app_info:dir(AppInfo), "include"),
         OutDir = rebar_app_info:out_dir(AppInfo),
         %% ensure all dirs exist
         filelib:ensure_dir(IncDir ++ "/"),
         OutIncDir = filename:join([OutDir, "include"]),
         filelib:ensure_dir(OutIncDir ++ "/"),
         OutEbinDir = filename:join(OutDir, "ebin") ++ "/",
         filelib:ensure_dir(OutEbinDir ++ "/"),

         CompileFun = fun(Source, _Target, _Opts) ->
                              proto_compile(Source, OutIncDir, OutEbinDir, ImportsDir)
                      end,

         rebar_base_compiler:run(Opts, [], SourceDir, ".proto", OutEbinDir, "_pb.beam", CompileFun)
     end || AppInfo <- Apps],
    {ok, State}.


proto_compile(Source, IncDir, EbinDir, ImportsDir) ->
    ok = protobuffs_compile:scan_file(Source, [{output_include_dir, IncDir},
                                               {output_ebin_dir, EbinDir},
                                               {imports_dir, ImportsDir}]).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
