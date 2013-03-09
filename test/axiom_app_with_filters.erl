-module(axiom_app_with_filters).
%-compile(export_all).
%
%start() ->
%	axiom:start(?MODULE).
%
%before_filter(Req) ->
%	Meta = Req#http_req.meta,
%	Meta2 = lists:keystore(filter_test, 1, Meta, {filter_test, <<"It works!">>}),
%	Req#http_req{meta = Meta2}.
%
%after_filter(Resp, Req) ->
%	<<"It works!">> = proplists:get_value(filter_test, Req#http_req.meta),
%	Meta = Req#http_req.meta,
%	Meta2 = lists:keystore(filter_test, 1, Meta, {filter_test, <<"It still works!">>}),
%	{Resp, Req#http_req{meta = Meta2}}.
%
%
%handle('GET', [], Req) ->
%	FilterTest = proplists:get_value(filter_test, Req#http_req.meta),
%	<<"It works!">> = FilterTest,
%	FilterTest.
