# Katja Echo

[![Build Status](https://travis-ci.org/katja-beam/katja_echo.png)](https://travis-ci.org/katja-beam/katja_echo) [![Coverage Status](https://coveralls.io/repos/nifoc/katja_echo/badge.png?branch=master)](https://coveralls.io/r/katja-beam/katja_echo?branch=master)

Katja Echo is a test library tool that helps performing tests on these specific use cases:

* Testing riemann client implementations: instead of starting and running the [riemann.io](https://www.riemann.io) server, one can use Katja Echo that will listen and interact with the any riemann.io client faking the riemann.io server behaviour
* If an application uses riemann.io, Katja Echo allows you to run tests without defining special configurations to start the riemann.io before running any test. In other words, you don't need to start a riemann.io server
* During specific tests (like: load, performance or integration), Katja Echo can be used to track the values sended. Later, use Riemann queries (or calling Katja Echo APIs) to inspect the values.

Besides the above listed itens, riemann.io implements a simple protocol and query parsing which are good exercises as well suitable to use Erlang.

This project has an aim to help testing and not intends to be a full Riemann implementation.

## Build

This project uses rebar3 as main build tool.

* Compile:
  
    $ rebar3 compile

* Running tests:

    $ rebar3 as test check

## How to use

Add `katja_echo` as an application dependency (use test profile if needed):

```
{deps, [
    {katja_echo, "0.1.0"}
]}.
```

The following configuration is assumed as default values:

```
[
    {katja_echo, [
        {port, 5555},
        {pool, []},
        {callback, katja_echo_user}
    ]}
].
```

When testing, a typical test case looks like this:

```
    Fun = fun (_Event) -> ok end,
    {ok, Pid} = katja_echo:start_link(),

    % sending riemann events using any client

    % call katja_echo query. Or call the riemann client query function
    Events = katja_echo:query(Fun, "service = \"my service\"").
```

## Resources

### Riemann

Katja Echo can receive and send messages using [Riemann Protocol Buffer](https://github.com/riemann/riemann-java-client/blob/master/riemann-java-client/src/main/proto/riemann/proto.proto).

Also query message was based on [Riemann Query](https://github.com/riemann/riemann/blob/master/src/riemann/query.clj) and the respective [tests](https://github.com/riemann/riemann/blob/master/test/riemann/query_test.clj) implementation.

The grammar was implemented based on [Riemann antlr grammar](https://github.com/riemann/riemann/blob/master/resources/query.g4).

### Scanner and Parser

The following links are great source of knowledge about how to write a scanner and parer using yecc/leex:

* [yecc](https://erlang.org/doc/man/yecc.html)
* [leex](https://erlang.org/doc/man/leex.html)
* [Tokenizing and parsing in Elixir with yecc and leex](https://andrealeopardi.com/posts/tokenizing-and-parsing-in-elixir-using-leex-and-yecc/)
* [Internationalization and localization support for Elixir](https://github.com/elixir-gettext/gettext/blob/e2e3d42edd2a8fa5aa2deada2e5779f122594e71/src/gettext_po_parser.yrl)
* [Leex And Yecc](http://web.archive.org/web/20170921125618/http://relops.com/blog/2014/01/13/leex_and_yecc/)
* [A simple example of how to use Leex and Yecc](https://github.com/relops/leex_yecc_example)
* [HTML parsing in Elixir with leex and yecc](https://notes.eellson.com/2017/01/22/html-parsing-in-elixir-with-leex-and-yecc/)
* [How to use leex and yecc in Elixir](https://cameronp.svbtle.com/how-to-use-leex-and-yecc)
* [Writing a lexer and parser](https://arjanvandergaag.nl/blog/write-your-own-parser.html)
* [Parsing with leex and yecc](http://raol.io/post/parsing-with-leex-and-yecc/)
* [Simple project to play with leex and yecc.](https://github.com/raol/ecalculator)

## License

[MIT](https://spdx.org/licenses/MIT.html).