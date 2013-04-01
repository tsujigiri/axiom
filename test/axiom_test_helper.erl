-module(axiom_test_helper).
-compile(export_all).

build_request() ->
	build_request([]).

build_request(Params) ->
	Socket = undefined,
	Transport = ranch_tcp,
	Peer = undefined,
	Method = proplists:get_value(method, Params, <<"GET">>),
	Path = proplists:get_value(path, Params, <<"/">>),
	Query = proplists:get_value('query', Params, undefined),
	Fragment = undefined,
	Version = {1,1},
	Headers = [],
	Host = proplists:get_value(host, Params, <<"example.com">>),
	Port = 80,
	Buffer = <<>>,
	CanKeepalive = false,
	Compress = false,
	OnResponse = undefined,
	cowboy_req:new(Socket, Transport, Peer, Method, Path, Query, Fragment,
		Version, Headers, Host, Port, Buffer, CanKeepalive,
		Compress, OnResponse).

