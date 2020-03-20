
# erl-jt-log

[![CircleCI](https://circleci.com/gh/jobteaser-oss/erl-jt-log/tree/master.svg?style=svg)](https://circleci.com/gh/jobteaser-oss/erl-jt-log/tree/master)

## Introduction
The repository contains logging tools which follow JobTeaser conventions.

## Build
The project can be built with:

```sh
    make
```

## Tests
Tests can be run with:

```sh
    make test
```

## Usage
### Log formatter
The `jt_log_formatter` module can be used with the standard Erlang logger, in
the configuration of the `logger` application.

For example:

```erlang
[
 {kernel,
  [{logger, [{handler, default, logger_std_h,
              #{filter_default => log,
                formatter => {jt_log_formatter,
                              #{format => text, color => true}}}
             }]},
   {logger_level, info}
  ]}
].
```

You also need to make sure that `jt_log` is a dependency of your Rebar3
project, and that your release loads the `jt_log` application, e.g. in the
Rebar3 configuration:

```erlang
{relx, [{release, {my_app, "dev"}, [inets, sasl, jt_log, my_app]}]}.
```

The formatter supports two formats: `text` (with or without colors) and `json`.

## Documentation
Code documentation is available at
[https://jobteaser-oss.github.io/erl-jt-log/doc](https://jobteaser-oss.github.io/erl-jt-log/doc).
