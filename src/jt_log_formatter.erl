%% Copyright 2020 JobTeaser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(jt_log_formatter).

-export([format/2, default_config/0]).

-export_type([log_format/0, config/0]).

-type log_format() :: text | json.

-type level() :: info | error.

-type config() :: #{format => log_format(),
                    color => boolean()}.
%% The configuration of the log formatter.

-record(message, {domain :: binary(),
                  level :: level(),
                  time :: integer(), % microseconds
                  text :: binary(),
                  data :: map()}).
-type message() :: #message{}.

%% @doc Return the default configuration of a formatter.
-spec default_config() -> config().
default_config() ->
  #{format => text,
    color => true}.

%% @doc Format a log event.
%%
%% This is the callback required for any log formatter.
-spec format(Event, Config) -> Entry when
    Event :: logger:log_event(),
    Config :: config(),
    Entry :: unicode:chardata().
format(Event, Config) ->
  Config2 = maps:merge(default_config(), Config),
  Msg = event_message(Event),
  format_message(Msg, Config2).

-spec format_message(message(), config()) -> unicode:chardata().
format_message(Msg, Config = #{format := text}) ->
  Domain = Msg#message.domain,
  DomainWidth = 32,
  TextColor = case Msg#message.level of
                info -> default;
                error -> red
              end,
  Text = maybe_colorize(indent_text(Msg#message.text, DomainWidth + 2),
                        TextColor,
                        Config),
  BasePart = io_lib:format("~-*s  ~s", [DomainWidth,
                                        maybe_colorize(Domain, green, Config),
                                        Text]),
  FormatDatum = fun (Name, Value, Acc) ->
                    NameString = maybe_colorize(atom_to_list(Name), blue,
                                                Config),
                    String = case Value of
                               Bin when is_binary(Bin) ->
                                 io_lib:format(" ~s=~s", [NameString, Bin]);
                               _ ->
                                 io_lib:format(" ~s=~p", [NameString, Value])
                             end,
                    [String | Acc]
                end,
  DataPart = maps:fold(FormatDatum, [], Msg#message.data),
  [BasePart, $\ , DataPart, $\n];
format_message(Msg, #{format := json}) ->
  BaseObj = #{domain => Msg#message.domain,
              level => Msg#message.level,
              ts => format_time(Msg#message.time),
              message => Msg#message.text},
  Obj = maps:merge(Msg#message.data, BaseObj),
  [jsx:encode(Obj), $\n].

-spec event_message(logger:log_event()) -> message().
event_message(Event) ->
  #message{domain = event_message_domain(Event),
           level = event_message_level(Event),
           time = event_message_time(Event),
           text = event_message_text(Event),
           data = event_message_data(Event)}.

-spec event_message_domain(logger:log_event()) -> binary().
event_message_domain(#{meta := #{domain := Domain}}) ->
  Strings = lists:map(fun erlang:atom_to_list/1, Domain),
  String = string:join(Strings, "."),
  unicode:characters_to_binary(String);
event_message_domain(_) ->
  <<"unknown">>.

-spec event_message_level(logger:log_event()) -> level().
event_message_level(#{level := Level}) when
    Level == debug; Level == info; Level == notice ->
  info;
event_message_level(_) ->
  error.

-spec event_message_time(logger:log_event()) -> integer().
event_message_time(#{meta := #{time := Time}}) ->
  Time;
event_message_time(_) ->
  os:system_time(microsecond).

-spec event_message_text(logger:log_event()) -> binary().
event_message_text(Msg = #{msg := {report, Report}}) when is_map(Report) ->
  event_message_text(Msg#{msg => {report, maps:to_list(Report)}});
event_message_text(#{msg := {report, Report}}) when is_list(Report) ->
  String = io_lib:format("~p", [Report]),
  unicode:characters_to_binary(String);
event_message_text(#{msg := {string, String}}) ->
  unicode:characters_to_binary(String);
event_message_text(#{msg := {Format, Terms}}) ->
  String = io_lib:format(Format, Terms),
  unicode:characters_to_binary(String).

-spec event_message_data(logger:log_event()) -> map().
event_message_data(#{meta := Metadata}) ->
  IgnoredMetadata = [domain, time, % duplicate
                     error_logger, logger_formatter, report_cb, % useless
                     mfa, file, line, % added by log macros
                     pid, gl, time], % added by the logger
  Metadata2 = maps:without(IgnoredMetadata, Metadata),
  maps:map(fun format_event_metadata_entry/2, Metadata2).

-spec format_event_metadata_entry(atom(), term()) -> binary().
format_event_metadata_entry(_Name, Value) when is_binary(Value) ->
  Value;
format_event_metadata_entry(_Name, Value) when is_list(Value) ->
  case lists:all(fun erlang:is_integer/1, Value) of
    true -> unicode:characters_to_binary(Value); % this is a reasonable guess
    false -> io_format:format("~p", [Value])
  end;
format_event_metadata_entry(_Name, Value) when is_number(Value) ->
  Value;
format_event_metadata_entry(_Name, Value) when is_boolean(Value) ->
  Value;
format_event_metadata_entry(_Name, Value) ->
  io_format:format("~p", [Value]).

-spec format_time(integer()) -> binary().
format_time(Time) ->
  String = calendar:system_time_to_rfc3339(Time, [{unit, microsecond},
                                                  {offset, "Z"}]),
  unicode:characters_to_binary(String).

-spec indent_text(string(), non_neg_integer()) -> string().
indent_text(Text, IndentSize) ->
  Padding = io_lib:format("~*s", [IndentSize, ""]),
  string:replace(Text, "\n", [$\n, Padding], all).

-spec maybe_colorize(iodata(), jt_log_term:color(), config()) -> iodata().
maybe_colorize(Text, Color, #{color := true}) ->
  jt_log_term:colorize(Text, Color);
maybe_colorize(Text, _Color, _Config) ->
  Text.
