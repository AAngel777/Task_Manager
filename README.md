# Todlist-erlang

```
%% Módulo de gestión de tareas usando GenServer.
-module(task_manager).
-behaviour(gen_server).

%% Exportación de las funciones API y los callbacks de GenServer.
-export([start_link/0, add_task/2, remove_task/2, list_tasks/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Definición del registro del estado que almacena una lista de tareas.
-record(state, {tasks = []}).

%%% API Functions %%%

% Iniciar el GenServer.
% start_link/0 arranca el servidor y lo registra localmente.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Agregar una tarea.
% add_task/2 envía una solicitud síncrona para agregar una tarea.
add_task(Pid, Task) ->
    gen_server:call(Pid, {add_task, Task}).

% Eliminar una tarea.
% remove_task/2 envía una solicitud síncrona para eliminar una tarea.
remove_task(Pid, Task) ->
    gen_server:call(Pid, {remove_task, Task}).

% Listar las tareas.
% list_tasks/1 envía una solicitud síncrona para obtener la lista de tareas.
list_tasks(Pid) ->
    gen_server:call(Pid, list_tasks).

%%% GenServer Callbacks %%%

% Inicializar el estado del GenServer.
% init/1 inicializa el servidor con una lista vacía de tareas.
init([]) ->
    {ok, #state{}}.

% Manejar las llamadas sincrónicas (gen_server:call/2).
% handle_call/3 procesa las operaciones de agregar, eliminar y listar tareas.
handle_call({add_task, Task}, _From, State) ->
    % Agregar la tarea al inicio de la lista de tareas.
    NewState = State#state{tasks = [Task | State#state.tasks]},
    {reply, ok, NewState};

handle_call({remove_task, Task}, _From, State) ->
    % Eliminar la tarea de la lista de tareas.
    NewTasks = lists:delete(Task, State#state.tasks),
    NewState = State#state{tasks = NewTasks},
    {reply, ok, NewState};

handle_call(list_tasks, _From, State) ->
    % Devolver la lista actual de tareas.
    {reply, State#state.tasks, State}.

% Manejar las operaciones asíncronas (no utilizadas en este ejemplo).
handle_cast(_Msg, State) ->
    {noreply, State}.

% Finalizar el GenServer.
terminate(_Reason, _State) ->
    ok.

```
