# Axiom [![Build Status](https://secure.travis-ci.org/tsujigiri/axiom.png?branch=master)](http://travis-ci.org/tsujigiri/axiom)

Axiom is a lightweight web framework, inspired by
[Sinatra](http://sinatrarb.com) and built on top of
[Cowboy](https://github.com/extend/cowboy).

## Getting Started

Axiom is built to make creating web applications fast and easy.
A minimal application would look like this:

```erlang
-module(my_app).
-export([start/0, handle/3]).

start() -> axiom:start(?MODULE).

handle('GET', [<<"hi">>], _Request) ->
	<<"Hello world!">>.

```

This handles requests for `GET /hi` and returns "Hello world!".

The third argument, given to the handler contains a record of type
`http_req`, [as known from Cowboy](https://github.com/extend/cowboy/blob/0c2e2224e372f01e6cf51a8e12d4856edb4cb8ac/include/http.hrl#L16).
Include Cowboy's `http.hrl` if you want to use it:

```erlang
-include_lib("cowboy/include/http.hrl").
```

The return value can be a binary string or iolist. To be more specific
about the response, use the `response` record. For that to work you
need to include Axiom's response header file:

```erlang
-include_lib("axiom/include/response.hrl").
```

Then, in your handler specify the body, the status and/or some HTTP
headers:

```erlang
handle('GET', [<<"foo">>], _Request) ->
	Resp = #response{},
	Headers = lists:keystore(<<"X-My-Header">>, 1, Resp, {<<"X-My-Header">>, <<"O HAI!">>}),
	#response{headers = Headers, body = <<"<h1>It works!</h1>">>}.
```

The `response` record defines sane defaults for all the fields, so you
don't need to specify every one of them:

```erlang
-record(response, {status = 200, headers = [{'Content-Type', "text/html"}], body = <<"">>}).
```

To get the request parameters out of the request, you can use the two
handy functions `axiom:params(Req)` and `axiom:param(Name, Req)`.
The first returns a proplist of all parameters, the second one returns
the named parameter's value.


## Configuration

`axiom:start/1` has a bigger brother called `axiom:start/2`, taking a
proplist as the second argument. Possible properties and their defaults
are as follows:

```erlang
[
	{nb_acceptors: 100},		% acceptor pool size
	{host, '_'},				% host IP
	{port, 7654},				% host port
	{public, "public"}			% custom path for static files
]
```


## Static Files

Static files are served via the `cowboy_http_static` handler. By
default, every directory in your application's `./public` directory
will be made accessible via a URL path prefix by the same name. E.g. the
file `./public/css/style.css` can be accessed via `GET /css/style.css`.

`./public/index.html` will not be served (yet), as `cowboy_http_static`
requires the URL path prefix.

You can specify a custom directory via the `public` option.

When you use this feature, it is advisable to start Erlang with the
`+A n` flag. This will start `n` async threads.
Rule of thumb is to use your machine's number of CPU cores.


## Redirects

You can redirect requests with `redirect/2`:

```erlang
handle('GET', [<<"foo">>], Request) ->
	axiom:redirect("/bar", Request);

handle('GET', [<<"bar">>], Request) ->
	<<"<h1>Welcome back!</h1>">>.
```

## Templates

Axiom comes with [Django](https://github.com/django/django) template
support via [erlydtl](https://github.com/evanmiller/erlydtl). To make
use of it in your application, create a directory named `templates` and
in it, create a template, e.g. `my_template.dtl`:

```dtl
<h1>Hello {{who}}</h1>
```

In your handler, specify the template to be rendered:

```erlang
handle('GET', [<<"hello">>], _Request) ->
	axiom:dtl(my_template, [{who, "you"}]).
```

For convenience, the second argument, a proplist of parameters, can have
atoms, lists or binaries as keys. That way request parameters can be put
in there, without you having to convert them first.

The templates are compiled into modules when `rebar compile` is
called.

To see what else erlydtl can do for you, take a look at
[its project page](https://code.google.com/p/erlydtl/).

## Sessions

Axiom comes with a basic session handler and ets based session store. To
use it, add this tuple to the configuration proplist:

```erlang
{sessions, []}
```

In your handler you can then use
`axiom_session:set(Key, Value, Request)` and
`axiom_session:get(Key, Request)`.

To set attributes for the cookie, storing the session ID, add some
parameters to the session configuration in a tuple with the key
`cookies`:

```erlang
{sessions, [{cookies, [param()]}]}
```

Possible parameters are:

```erlang
param() = {max_age, integer()} |
		  {local_time, calendar:datetime()} |
		  {domain, binary()} |
		  {path, binary()} |
		  {secure, true | false} |
		  {http_only, true | false}
```

The default session store is the `axiom_session_ets` module. You can use
your own by adding a `store` tuple to the sessions tuple:

```erlang
{sessions, [{store, my_session_store, []}]}
```

For implementation details take a look into the `axiom_session_ets`
module.

## Errors

### Not Found

To overwrite Axiom's response to 404 errors, just create a catch-all
handler:

```erlang
handle(_Method, _Path, _Req) ->
	#response{status = 404, body = <<"nope.">>}.
```

Note that you have to take care of the status code yourself, as
otherwise the default of 200 is sent back to the client.

### Internal Server Error

To handle these yourself, you can implement a function named `error/1`.
The argument is the `http_req` record, otherwise it works like the
`handle` function, only with a default status code of 500.


## Installation

To use it in your OTP application, add this to your `rebar.config`:

```erlang
{lib_dirs, ["deps"]}.
{deps, [
	{'axiom', "0.0.9", {git, "git://github.com/tsujigiri/axiom.git", {tag, "v0.0.9"}}}
]}.
```

then, as usual:

```
rebar get-deps
rebar compile
```

## License

Please take a look at the `LICENSE` file! (tl;dr: it's the MIT License)
