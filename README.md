rebar3 protobuffs provider
==========================

Provider to compile protobuffers files using
[protobuffers](https://github.com/basho/erlang_protobuffs).

Place your `.proto` files in `src` directory and they will be automatically
build.

Usage
-----

Add the plugin to your rebar config:

```erlang
    {plugins, [rebar3_protobuffs]}.
```

The compile function is under the protobuffs namespace. To automatically compile `.proto` files before
the Erlang compiler add the pre_hook to rebar.config:

```erlang
{provider_hooks, [
                 {pre, [{compile, {protobuffs, compile}}, {clean, {protobuffs, clean}}]}
                 ]}.
```

Add opts for protobuffs compiler, but just support `imports_dir` now :
```erlang
{protobuffs_opts, [
    {imports_dir, ["src/protobuf/"]}
]}.
```
