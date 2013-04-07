-module(axiom_app_redirect_in_before_filter).
-compile(export_all).

start() ->
	axiom:start(?MODULE).

before_filter(Req) ->
	axiom:redirect("/somewhere/else", Req).

