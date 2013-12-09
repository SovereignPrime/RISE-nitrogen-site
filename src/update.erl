-module(update).
-compile([export_all]).
%#!/usr/bin/escript
%%% -*- erlang -*-
%%%! -smp enable -sname update -pa ./ebin -pa ./deps/*/ebin
%-mode(compile).
%-compile(export_all).

main(VSN, Address, From) ->
    TName = wf:f("scratch/u_~s.torrent", [VSN]),
    etorrent_mktorrent:create(wf:f("scratch/u_~s.tar.gz",[ VSN ]), undefined, TName),
    {ok, Tor} = file:read_file(io_lib:format("scratch/u_~s.torrent", [VSN])),
    bitmessage:send_message(From, Address, <<"Update">>, <<(list_to_binary(VSN))/bytes, ";u_", (list_to_binary(VSN))/bytes, ";", Tor/bytes>>, 6).

