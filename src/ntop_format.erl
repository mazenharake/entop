%% Copyright (c) 2010, Mazen Harake
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%     * Redistributions of source code must retain the above copyright notice,
%%       this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(ntop_format).
-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([init/0, header/2, row/2]).

%% Records
-record(state, { cache = [] }).

%% =============================================================================
%% Module API
%% =============================================================================
init() ->
    Columns = [{"Pid", 15, [{align, right}]},
	       {"Name", 15, []},
	       {"Reductions", 10, []},
	       {"Queue", 5, []},
	       {"HSize", 5, []},
	       {"SSize", 5, []},
	       {"HTot", 5, []}],
    {ok, Columns, #state{}}.

%% Header Callback
header(SystemInfo, State) ->
    Row1 = io_lib:format("Process Count: ~p", [proplists:get_value(process_count, SystemInfo, 0)]),
    Row2 = io_lib:format("Uptime: ~p", [element(1, proplists:get_value(uptime, SystemInfo, 0))]),
    Row3 = "",
    Row4 = "",
    {ok, [ lists:flatten(Row) || Row <- [Row1, Row2, Row3, Row4] ], State}.

%% Column Specific Callbacks
row(ProcessInfo, State) ->
    Pid = erlang:pid_to_list(proplists:get_value(pid, ProcessInfo)),
    RegName = case proplists:get_value(registered_name, ProcessInfo) of
		  undefined ->
		      "N/A";
		  Name ->
		      atom_to_list(Name)
	      end,
    Reductions = proplists:get_value(reductions, ProcessInfo, 0),
    Queue = proplists:get_value(message_queue_len, ProcessInfo, 0),
    Heap = proplists:get_value(heap_size, ProcessInfo, 0),
    Stack = proplists:get_value(stack_size, ProcessInfo, 0),
    HeapTot = proplists:get_value(total_heap_size, ProcessInfo, 0),    
    {ok, {Pid, RegName, Reductions, Queue, Heap, Stack, HeapTot}, State}.
