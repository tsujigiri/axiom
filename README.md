# Axiom [![Build Status](https://secure.travis-ci.org/tsujigiri/axiom.png?branch=master)](http://travis-ci.org/tsujigiri/axiom)

Axiom is a micro-framework for building web applications in Erlang.
It is inspired by [Sinatra](http://sinatrarb.com) and built on top of
[Cowboy](https://github.com/extend/cowboy).

## [Maintainer wanted!](https://github.com/tsujigiri/axiom/issues/28)

## Getting Started

Axiom is built to make creating web applications fast and easy.
A minimal application would look like this:

```erlang
-module(my_app).
-export([start/0, handle/3]).

start() ->
	axiom:start(?MODULE).

handle(<<"GET">>, [<<"hi">>], _Request) ->
	<<"Hello world!">>.

```

This handles requests for `GET /hi` and returns "Hello world!".

The third argument given to the handler is of type `cowboy_req:req()`. Use the
`cowboy_req` module, if you need anything out of the request.

The return value can be a binary string or iolist. So, this also works:

```erlang
handle(<<"GET">>, [<<"hello">>, Who], _Request) ->
	[<<"Hello ">>, Who, <<"!">>].
```

If you want to specify a response status code and/or headers, use a tuple with
either the status code and body or status code, headers and body, in these
respective orders.

Examples:

```erlang
{418, <<"<h1>I'm a teapot!</h1>">>}
```
or
```erlang
{418, [{<<"X-Foo">>, <<"bar">>}], <<"<h1>I'm a teapot!</h1>">>}
```

As a third option a `cowboy_req:req()` can be returned. In this case, to set the
response headers and body, use the `cowboy_req:set_resp_header/3` and
`cowboy_req:set_resp_body/2` functions. To set the status code, use
`axiom:set_resp_status/2`. These functions return a new `cowboy_req:req()` to be
used further and to be returned from `YourHandler:handle/3`.

The full spec of `YourHandler:handle/3` is expected to look like this:

```erlang
handle(Method, Path, Req) -> Body | Req | {Status, Body} | {Status, Headers, Body}.

  Types:
    Method = binary(),
    Path = [PathSegment]
    PathSegment = binary()
    Req = cowboy_req:req()
    Body = iodata()
    Status = non_neg_integer()
    Headers = [Header]
    Header = {binary(), binary()}
```

## Request parameters

To get the request parameters out of the request, you can use the two
handy functions `axiom:params(Req)` and `axiom:param(Name, Req)`.
The first returns a proplist of all parameters, the second one returns
the named parameter's value. Keys and values are binary strings.

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

Static files are served via the `cowboy_static` handler. By default, every file
in the ./public directory and all its subdirectories will be made accessible via
URL path the same as file's relative path. E.g. the file `./public/about.html`
can be accessed via `GET /about.html`. **Note**: Currently, if the contents of
the ./public subtree change, Axiom needs to be restarted to reflect the change.

You can specify a custom directory via the `public` option.

When you use this feature, it is advisable to start Erlang with the
`+A n` flag. This will start `n` async threads.
Rule of thumb is to use your machine's number of CPU cores.


## Redirects

You can redirect requests with `redirect/2`:

```erlang
handle(<<"GET">>, [<<"foo">>], Req) ->
  Req1 = axiom:redirect("/bar", Req),
  Req;

handle(<<"GET">>, [<<"bar">>], Request) ->
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
handle(<<"GET">>, [<<"hello">>], _Request) ->
	axiom:dtl(my_template, [{who, "you"}]).
```

For convenience, the second argument, a proplist of parameters, can have
atoms, lists or binaries as keys. That way request parameters can be put
in there, without you having to convert them first.

The templates are compiled into modules when `rebar compile` is
called.

To see what else erlydtl can do for you, take a look at
[its project page](https://github.com/erlydtl/erlydtl).


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

## Filters

The functions `before_filter/1` and `after_filter/1` can be implemented
to deal with the `cowboy_req:req()` before and after `YourHandler:handle/3`.
When implemented, these are called no matter which `handle` function matches the
request.

In your handler module:

```erlang
before_filter(Req) ->
	% do stuff
	Req.

after_filter(Req) ->
	% do more stuff
	Req.
```

## Errors

### Not Found

To overwrite Axiom's response to 404 errors, just create a catch-all
handler:

```erlang
handle(_Method, _Path, _Req) ->
	{404, <<"nope.">>}.
```

Note that you have to take care of the status code yourself, as
otherwise the default of 200 is sent back to the client.

### Internal Server Error

To handle these yourself, you can implement a function named `error/1`.
The argument is the `cowboy_req:req()` object, otherwise it works like your
`Handler:handle/3` function.

## Streaming

To send a chunked reply, call `axiom:chunk/2` for each chunk:

```erlang
chunk(Data::iodata(), Req::cowboy_req:req()) -> {ok, Req1::cowboy_req:req()}.
```

The returned `cowboy_req:req()` object has to be given as an argument to
subsequent calls to `chunk` and as the return value of your
`Handler:handle/3` function.

To stream data with a Content-Type other than `text/html`, use
`chunk/3`, which has an additional parameter, to be set to the type you
want:

```erlang
chunk(Data::iodata(), Req::cowboy_req:req(), Type::binary()) -> {ok, Req1::cowboy_req:req()}.
```

### Example

```erlang
handle(<<"GET">>, [<<"stream">>], Req) ->
	{ok, Req1} = axiom:chunk(<<"Hello">>, Req, <<"text/plain">>),
	{ok, Req2} = axiom:chunk(<<" world">>, Req1),
	{ok, Req3} = axiom:chunk(<<"!">>, Req2),
	Req3.
```

## Installation

To use it in your OTP application, add this to your `rebar.config`:

```erlang
{lib_dirs, ["deps"]}.
{deps, [
	{'axiom', "0.1.0", {git, "git://github.com/tsujigiri/axiom.git", {tag, "v0.1.0"}}}
]}.
```

then, as usual:

```
rebar get-deps
rebar compile
```

## License

Please take a look at the `LICENSE` file! (tl;dr: it's the MIT License)
