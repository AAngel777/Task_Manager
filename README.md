# Aplicación de Gestión de Tareas

**Fecha:** 24 de septiembre de 2024  
**Institución:** Instituto Tecnológico de Tijuana  
**Equipo:**  
- Integrante 1: Garcia Cerrillo Angel Albino
- Integrante 2: Ortega Jimenez Jordi Joel 
- Integrante 3: Rosas Javier
- Integrante 4: Gonzalez Reyes Jesus Eduardo

---

Este proyecto implementa una aplicación de gestión de tareas utilizando **GenServer** en Erlang. La aplicación permite agregar, eliminar y listar tareas en tiempo real, manteniendo un estado centralizado en el servidor.

## Descripción del Proyecto

La aplicación se basa en un servidor GenServer que almacena y gestiona tareas. El servidor recibe mensajes para manipular una lista de tareas que se mantiene en el estado del servidor.

### Características:
- **Agregar Tarea:** Permite añadir tareas a la lista de tareas.
- **Eliminar Tarea:** Elimina una tarea específica de la lista.
- **Listar Tareas:** Devuelve la lista completa de tareas almacenadas en el servidor.

---

## Código del Proyecto

### Módulo 1: Servidor de Gestión de Tareas

```erlang
-module(task_manager).
-behaviour(gen_server).

%% Exportación de las funciones API y los callbacks de GenServer.
-export([start_link/0, add_task/2, remove_task/2, list_tasks/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

%% Definición del registro del estado que almacena una lista de tareas.
-record(state, {tasks = []}).

%%% API Functions %%%

% Iniciar el GenServer.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Agregar una tarea.
add_task(Pid, Task) ->
    gen_server:call(Pid, {add_task, Task}).

% Eliminar una tarea.
remove_task(Pid, Task) ->
    gen_server:call(Pid, {remove_task, Task}).

% Listar las tareas.
list_tasks(Pid) ->
    gen_server:call(Pid, list_tasks).

%%% GenServer Callbacks %%%

% Inicializar el estado del GenServer.
init([]) ->
    {ok, #state{}}.

% Manejar las llamadas sincrónicas.
handle_call({add_task, Task}, _From, State) ->
    NewState = State#state{tasks = [Task | State#state.tasks]},
    {reply, ok, NewState};

handle_call({remove_task, Task}, _From, State) ->
    NewTasks = lists:delete(Task, State#state.tasks),
    NewState = State#state{tasks = NewTasks},
    {reply, ok, NewState};

handle_call(list_tasks, _From, State) ->
    {reply, State#state.tasks, State}.

% Finalizar el GenServer.
terminate(_Reason, _State) ->
    ok.
```
## Pasos para la ejecución
1. Iniciar el servidor GenServer ejecutando el siguiente comando en el shell de Erlang:
`{ok, Pid} = task_manager:start_link().`
2. Agregar una tarea a la lista usando:
`task_manager:add_task(Pid, "Mi primera tarea").`
3. Elimina una tarea con:
`task_manager:remove_task(Pid, "Mi primera tarea").`
4. Listar tareas almacenadas:
`task_manager:list_tasks(Pid).`
## Código ejecutado
[![asciicast](https://asciinema.org/a/4vJtbAe1crwMANCt1ho55JgLD.svg)](https://asciinema.org/a/4vJtbAe1crwMANCt1ho55JgLD)
