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

-module(ntop).
-include("ntop.hrl").
-include_lib("cecho/include/cecho.hrl").

%% Application API
-export([start/2]).

%% =============================================================================
%% Application API
%% =============================================================================
start(Node, Options) ->
    State = (read_options(Options))#state{ node = Node },
    case net_kernel:connect(Node) of
	true ->
	    ViewPid = ntop_view:start(State),
	    control(ViewPid);
	false ->
	    halt(101)
    end.

read_options(Options) ->
    read_options(Options, #state{}).

read_options([], State) ->
    State;
read_options([{interval, Intv} | Rest], State) when is_integer(Intv) andalso Intv > 500 andalso
						    (Intv rem 500) == 0 ->
    read_options(Rest, State#state{ interval = Intv });
read_options([{interval, Intv}|_], _) ->
    erlang:error({badarg, {invalid_interval, Intv}});
read_options([{sort, Way} | Rest], State) when Way == reductions orelse Way == memory orelse %
					       Way == foo orelse Way == bar ->
    read_options(Rest, State#state{ sort = Way });
read_options([Option|_],_) ->
    erlang:error({badarg, {invalid_option, Option}}).

control(ViewPid) ->
    P = cecho:getch(),
    case P of
	N when N >= 49 andalso N =< 57 -> ViewPid ! {sort, N - 48}, control(ViewPid);
	$> -> ViewPid ! {sort, next}, control(ViewPid);
	$< -> ViewPid ! {sort, prev}, control(ViewPid);
	$r -> ViewPid ! reverse_sort, control(ViewPid);
	$q -> do_exit(ViewPid);
	3 -> do_exit(ViewPid); %Ctrl-C
	_ -> ViewPid ! force_update, control(ViewPid)
    end.

do_exit(ViewPid) ->
    exit(ViewPid, normal), application:stop(cecho), halt(0).

