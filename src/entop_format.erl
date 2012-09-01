%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(entop_format).

-author('mazen.harake@erlang-solutions.com').

-include_lib("cecho/include/cecho.hrl").

%% Module API
-export([init/1, header/2, row/3, row_reductions/1]).

%% Records
-record(state, { node = undefined, cache = [] }).

%% Defines
-define(KIB,(1024)).
-define(MIB,(?KIB*1024)).
-define(GIB,(?MIB*1024)).
-define(SECONDS_PER_MIN, 60).
-define(SECONDS_PER_HOUR, (?SECONDS_PER_MIN*60)).
-define(SECONDS_PER_DAY, (?SECONDS_PER_HOUR*24)).
-define(R(V,N), string:right(integer_to_list(V),N,$0)).

%% =============================================================================
%% Module API
%% =============================================================================
init(Node) ->
    Columns = [
            {"Pid", 16, [{align, left}]},
            {"Process", 30, [{align, left}]},
            {"Current Function", 30, [{align, left}]},
            {"Status", 8, [{align, left}]},
            {"Reductions", 13, [{align, right}]},
            {"Reductions+", 13, [{align, right}]},
            {"Message Queue", 14, [{align, right}]},
            {"Stack Size", 11, [{align, right}]},
            {"Heap Size", 12, [{align, right}]}
        ],
    {ok, {Columns, 6}, #state{ node = Node }}.

%% Header Callback
header(SystemInfo, State) ->
    Uptime = millis2uptimestr(element(1, proplists:get_value(uptime, SystemInfo, 0))),
    LocalTime = local2str(element(2, proplists:get_value(local_time, SystemInfo))),
    PingTime = element(1,timer:tc(net_adm, ping, [State#state.node])) div 1000,
    CPUInfo = proplists:get_value(cpu, SystemInfo, []),
    CPUAvg1 = proplists:get_value(avg1, CPUInfo, 0.0),
    CPUAvg5 = proplists:get_value(avg5, CPUInfo, 0.0),
    CPUAvg15 = proplists:get_value(avg15, CPUInfo, 0.0),
    Row1 = io_lib:format("Time: ~s, up for ~s, ~pms latency, load average: ~.2f, ~.2f, ~5.2f",
			 [LocalTime, Uptime, PingTime, CPUAvg1, CPUAvg5, CPUAvg15]),

    PTotal = proplists:get_value(process_count, SystemInfo),
    RQueue = proplists:get_value(run_queue, SystemInfo),
    RedTotal = element(2,proplists:get_value(reduction_count, SystemInfo)),
    Row2 = io_lib:format("Processes: ~6w,  Run Queue: ~5w,  Reductions: ~9w~n",
			 [PTotal, RQueue, RedTotal]),

    PMemUsed = mem2str(proplists:get_value(process_memory_used, SystemInfo)),
    PMemTotal = mem2str(proplists:get_value(process_memory_total, SystemInfo)),
    MemInfo = proplists:get_value(memory, SystemInfo),
    SystemMem = mem2str(proplists:get_value(system, MemInfo)),
    AtomMem = mem2str(proplists:get_value(atom, MemInfo)),
    AtomUsedMem = mem2str(proplists:get_value(atom_used, MemInfo)),
    BinMem = mem2str(proplists:get_value(binary, MemInfo)),
    CodeMem = mem2str(proplists:get_value(code, MemInfo)),
    EtsMem = mem2str(proplists:get_value(ets, MemInfo)),
    Row3 = io_lib:format("Memory: ~s total, ~s allocated (~s system, ~s/~s atom, ~s binary, ~s code, ~s ets)",
			 [PMemTotal, PMemUsed, SystemMem, AtomUsedMem, AtomMem, BinMem, CodeMem, EtsMem]),
    Row4 = "",
    {ok, [ lists:flatten(Row) || Row <- [Row1, Row2, Row3, Row4] ], State}.

%% Column Specific Callbacks
row([{pid,_}|undefined], _LastReductions, State) ->
    {ok, skip, State};
row(ProcessInfo, LastReductions, State) ->
    Pid = proplists:get_value(pid, ProcessInfo),
    ProcessName = proc_name(ProcessInfo),
    CurrentFunction = proplists:get_value(current_function, ProcessInfo),
    Reductions = proplists:get_value(reductions, ProcessInfo, 0),
    ReductionsDiff = Reductions - LastReductions,
    Queue = proplists:get_value(message_queue_len, ProcessInfo, 0),
    StackSize = proplists:get_value(stack_size, ProcessInfo, 0),
    TotalHeapSize = proplists:get_value(total_heap_size, ProcessInfo, 0),
    Status = proplists:get_value(status, ProcessInfo),
    {ok, {Pid, ProcessName, CurrentFunction, Status, Reductions, ReductionsDiff, Queue, StackSize, TotalHeapSize}, State}.

row_reductions({Pid, _, _, _, Reductions, _, _, _, _} = _Row) ->
	{Pid, Reductions}.

proc_name(ProcessInfo) ->
    case proplists:get_value(registered_name, ProcessInfo, []) of
        [] -> initial_call(ProcessInfo);
        Name -> Name
    end.

initial_call(ProcessInfo) ->
    ProcessDict = proplists:get_value('dictionary', ProcessInfo, []),
    case proplists:get_value('$initial_call', ProcessDict, []) of
        [] -> proplists:get_value(initial_call, ProcessInfo);
        Call -> Call
    end.

mem2str(Mem) ->
    if Mem > ?GIB -> io_lib:format("~.1fG",[Mem/?GIB]);
       Mem > ?MIB -> io_lib:format("~.1fM",[Mem/?MIB]);
       Mem > ?KIB -> io_lib:format("~.1fK",[Mem/?KIB]);
       Mem >= 0 -> io_lib:format("~.1fb",[Mem/1.0])
    end.

millis2uptimestr(Millis) ->
    SecTime = Millis div 1000,
    Days = ?R(SecTime div ?SECONDS_PER_DAY,3),
    Hours = ?R((SecTime rem ?SECONDS_PER_DAY) div ?SECONDS_PER_HOUR,2),
    Minutes = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) div ?SECONDS_PER_MIN, 2),
    Seconds = ?R(((SecTime rem ?SECONDS_PER_DAY) rem ?SECONDS_PER_HOUR) rem ?SECONDS_PER_MIN, 2),
    io_lib:format("~s:~s:~s:~s",[Days,Hours,Minutes,Seconds]).

local2str({Hours,Minutes,Seconds}) ->
    io_lib:format("~s:~s:~s",[?R(Hours,2),?R(Minutes,2),?R(Seconds,2)]).
