# venato

A day-based todo list for the terminal. Tasks belong to the day they were
added: you can only add and change today's tasks, and past days are read-only.

## Build

```
cabal install
```

## Usage

```
venato add <task>                           add a task for today
venato list [yesterday | YYYY-MM-DD | all]  show tasks (default: today)
venato done <id>                            mark one of today's tasks done
venato rm <id>                              remove one of today's tasks
```

`vn` is installed as a short alias, so `vn add <task>` works the same way.

Tasks are stored in `~/.venato`.
