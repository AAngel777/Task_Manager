
-module(task_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, add_task/2, remove_task/2, list_tasks/1]).

%% GenServer Callbacks
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(state, {tasks = []}).

%%% API Functions %%%

% Iniciar el GenServer
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Agregar una tarea
add_task(Pid, Task) ->
    gen_server:call(Pid, {add_task, Task}).

% Eliminar una tarea
remove_task(Pid, Task) ->
    gen_server:call(Pid, {remove_task, Task}).

% Listar las tareas
list_tasks(Pid) ->
    gen_server:call(Pid, list_tasks).

%%% GenServer Callbacks %%%

% Inicializa el estado
init([]) ->
    {ok, #state{}}.

% Manejar las llamadas sincrónicas
handle_call({add_task, Task}, _From, State) ->
    NewState = State#state{tasks = [Task | State#state.tasks]},
    {reply, ok, NewState};

handle_call({remove_task, Task}, _From, State) ->
    NewTasks = lists:delete(Task, State#state.tasks),
    NewState = State#state{tasks = NewTasks},
    {reply, ok, NewState};

handle_call(list_tasks, _From, State) ->
    {reply, State#state.tasks, State}.

% Manejar los "casts" asíncronos (opcional si decides implementar operaciones asíncronas)
handle_cast(_Msg, State) ->
    {noreply, State}.

% Finalizar el GenServer
terminate(_Reason, _State) ->
    ok.

