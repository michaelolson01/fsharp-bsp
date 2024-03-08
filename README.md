# Graphs

Graphs in F#.

## Setup

```
dotnet tool restore
dotnet paket restore
```

## Testing

make a .env file with the connection string environment variables in the tests folder

## Running Documentation

```
dotnet fsdocs watch
```

this will open your browser to the documentation and auto-refresh with updates to the files in the `docs/` directory.

## Building

To build all projects and run all build steps, run:

```
dotnet fake build
```

To perform a specific build step, run:
```
dotnet fake build -t <STEP NAME>
```

To build (or run a build step on) a specific project, run:
```
dotnet fake build --project <PROJECT NAME>
```

