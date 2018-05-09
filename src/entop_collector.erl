%%==============================================================================
%% Copyright (c) 2017, Mazen Harake
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
%%==============================================================================

-module(entop_collector).

%% Module API
-export([get_data/0]).
-export([lookup_name/1]).

%% =============================================================================
%% Module API
%% =============================================================================
get_data() ->
  HeaderProplist = [{uptime, erlang:statistics(wall_clock)},
                    {local_time, calendar:local_time()},
                    {process_count, erlang:system_info(process_count)},
                    {run_queue, erlang:statistics(run_queue)},
                    {reduction_count, erlang:statistics(reductions)},
                    {process_memory_used, erlang:memory(processes_used)},
                    {process_memory_total, erlang:memory(processes)},
                    {memory, erlang:memory([system, atom, atom_used, binary, code, ets])}
                   ],
  HeaderProplist1 = HeaderProplist
    ++ case os_mon_started() of
         true ->
           [{cpu, [{avg1, cpu_sup:avg1() / 256},
                   {avg5, cpu_sup:avg5() / 256},
                   {avg15, cpu_sup:avg15() / 256}]}];
         false ->
           []
       end,
  Self = self(),
  ProcessesProplist =
    [ [ {pid,erlang:pid_to_list(P)}, {realpid, P} | process_info_items(P) ]
      || P <- erlang:processes(), P /= Self ],

  {ok, HeaderProplist1, ProcessesProplist}.

%% =============================================================================
%% Internal Functions
%% =============================================================================
process_info_items(P) ->
  erlang:process_info(P, [registered_name,
                          reductions,
                          message_queue_len,
                          heap_size,
                          stack_size,
                          total_heap_size,
                          memory,
                          dictionary,
                          initial_call,
                          current_function,
                          status]).

os_mon_started() ->
  [App || {os_mon, _, _} = App <- application:which_applications()] /= [].

lookup_name(Pid) when is_pid(Pid) ->
  case whereis(gproc) of
    undefined -> undefined;
    _ ->
      {gproc, Props} = gproc:info(Pid, gproc),
      case [ E || {E,_} <- Props, element(1,E)==n ] of
        [] -> undefined;
        [Ret] -> Ret
      end
  end.
