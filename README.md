# Axiom

Axiom aims at being a lightweight web framework. It inspired by
[Sinatra](http://sinatrarb.com) and built on top of
[Cowboy](https://github.com/extend/cowboy).

## Getting started

Axiom is built to make creating web applications fast and easy.
A minimal setup would look like this:

```erlang
-module(my_app).
-export([start/0, handle/3]).

start() -> axiom:start(?MODULE).

handle('GET', [<<"hi">>], _Params) ->
	<<"Hello world!">>.

```

This example handles requests for `GET /hi` and returns "Hello world!".

The third argument, given to the handler contains a proplist of things
we know about the request, such as `params`, `headers` and many more.

There are two ways to be more specific about the response. The first is
to use the `response` record. For that to work you need to include
Axiom's response header file:

```erlang
-include_lib("axiom/include/response.hrl").
```

Then, in your handler specify the body, the status and/or some HTTP
headers:

```erlang
handle('GET', [<<"foo">>], Request) ->
	Response = #response{},
	Headers = Response#response.headers ++ [{'Set-Cookie', "ping=pong"}],
	#response{headers = Headers, body = <<"Go away!">>}.
```

The `response` record defines sane defaults for all the fields, so you
don't need to specify every one of them:

```erlang
-record(response, {status = 200, headers = [{'Content-Type', "text/html"}], body = <<"">>}).
```

The other way to specify the response is to but the above params into a
proplist:

```erlang
[{status, 403}, {body, <<"Go away!">>}, {headers, []}]
```

Again, you don't need to specify all of them, sane defaults also apply
to the proplist-way.

## Installation

To use it in your OTP application, add this to your `rebar.config`:

```erlang
{lib_dirs, ["deps"]}.
{deps, [
	{'axiom', ".*", {git, "git://github.com/tsujigiri/axiom.git", "HEAD"}}
]}.
```

then, as usual:

```
rebar get-deps
rebar compile
```



