# venato

A day-based todo list for the terminal. Tasks belong to the day they were
added: you can only add and change today's tasks, and past days are read-only.

## Build

```
cabal install    # installs the `vn` command
```

## Usage

```
vn add <task>                           add a task for today
vn list [yesterday | YYYY-MM-DD | all]  show tasks (default: today)
vn done <id>                            mark one of today's tasks done
vn rm <id>                              remove one of today's tasks
```

Tasks are stored in `~/.venato`.
