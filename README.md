rebar3_protobuffs
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        { rebar3_protobuffs, ".*", {git, "git@host:user/rebar3_protobuffs.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 rebar3_protobuffs
    ===> Fetching rebar3_protobuffs
    ===> Compiling rebar3_protobuffs
    <Plugin Output>
