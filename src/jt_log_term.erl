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

-module(jt_log_term).

-export([colorize/2, colorize/3]).

-export_type([color/0]).

-type color() :: black | red | green | yellow | blue | magenta | cyan | white |
                 default.

-spec color_number(color()) -> integer().
color_number(black) -> 0;
color_number(red) -> 1;
color_number(green) -> 2;
color_number(yellow) -> 3;
color_number(blue) -> 4;
color_number(magenta) -> 5;
color_number(cyan) -> 6;
color_number(white) -> 7;
color_number(default) -> 9.

-spec fg_color_sequence(color()) -> string().
fg_color_sequence(Color) ->
  io_lib:format("\e[0;~bm", [30 + color_number(Color)]).

-spec bg_color_sequence(color()) -> string().
bg_color_sequence(Color) ->
  io_lib:format("\e[0;~bm", [40 + color_number(Color)]).

-spec reset_sequence() -> string().
reset_sequence() ->
  "\033[0m".

-spec colorize(iodata(), color()) -> iodata().
colorize(String, FgColor) ->
  [fg_color_sequence(FgColor),
   String,
   reset_sequence()].

-spec colorize(iodata(), color(), color()) -> iodata().
colorize(String, FgColor, BgColor) ->
  [fg_color_sequence(FgColor), bg_color_sequence(BgColor),
   String,
   reset_sequence()].
