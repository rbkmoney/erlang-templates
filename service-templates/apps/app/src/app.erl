%%% @doc Public API, supervisor and application startup.
%%% @end

-module({{name}}).
-behaviour(supervisor).
-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).

%% Supervisor callbacks
-export([init/1]).

%% Application callbacks
-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 8080).
-define(DEFAULT_HANDLING_TIMEOUT, 60_000).

-type cowboy_route_path() :: {Path :: iodata(), Handler :: module(), Opts :: any()}.

%%
%% API
%%
-spec start() -> {ok, _}.
start() ->
    application:ensure_all_started({{name}}).

-spec stop() -> ok.
stop() ->
    application:stop({{name}}).

%%
%% Supervisor callbacks
%%
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Opts for woody handlers and services
    %% Opts = #{
    %%     default_handling_timeout =>
    %%         genlib_app:env({{name}}, default_woody_handling_timeout, ?DEFAULT_HANDLING_TIMEOUT)
    %% },

    WoodyServerSpec =
        woody_server:child_spec(
            ?MODULE,
            #{
                ip => get_ip(),
                port => genlib_app:env(?MODULE, port, ?DEFAULT_PORT),
                transport_opts => genlib_app:env(?MODULE, transport_opts, #{}),
                protocol_opts => genlib_app:env(?MODULE, protocol_opts, #{}),
                event_handler => {scoper_woody_event_handler, get_event_handler_opts()},
                %% Woody services to call are to be placed here
                handlers => [],
                additional_routes => get_additional_routes(),
                shutdown_timeout => genlib_app:env(?MODULE, shutdown_timeout, 0)
            }
        ),
    {ok,
        {
            #{
                strategy => one_for_all,
                intensity => 0,
                period => 1
            },
            [WoodyServerSpec]
        }}.

%%
%% Application callbacks
%%
-spec start(normal, any()) -> {ok, pid()} | {error, any()}.
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec stop(any()) -> ok.
stop(_State) ->
    ok.

-spec get_ip() -> inet:ip_address().
get_ip() ->
    {ok, Ip} = inet:parse_address(genlib_app:env(?MODULE, ip, "::")),
    Ip.

-spec get_event_handler_opts() -> scoper_woody_event_handler:options().
get_event_handler_opts() ->
    genlib_app:env(?MODULE, scoper_event_handler_options, #{}).

-spec get_additional_routes() -> [cowboy_route_path()].
get_additional_routes() ->
    get_health_routes(genlib_app:env(?MODULE, health_check, #{})) ++
        get_prometheus_routes().

-spec get_health_routes(erl_health:check()) -> [cowboy_route_path()].
get_health_routes(Checks) ->
    EvHandler = {erl_health_event_handler, []},
    HealthCheck = maps:map(fun(_, V = {_, _, _}) -> #{runner => V, event_handler => EvHandler} end, Checks),
    [erl_health_handle:get_route(HealthCheck)].

-spec get_prometheus_routes() -> [cowboy_route_path()].
get_prometheus_routes() ->
    [{"/metrics/[:registry]", prometheus_cowboy2_handler, []}].
