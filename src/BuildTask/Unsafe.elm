module BuildTask.Unsafe exposing (commandInReadonlyDirectory, commandInWritableDirectory, commandInWritableDirectoryOutput, commandInWritableDirectoryOutputWith, commandWithFile, downloadImmutable, named, patchFileInDirectory, pipeThrough)

{-| -}

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, Command, FileOrDirectory)
import BuildTask.Internal as Internal exposing (Error, Input)
import CommandOptions exposing (CommandOptions)
import FatalError exposing (FatalError)
import Hash exposing (Hash)
import List.Extra
import Pages.Script as Script
import Path
import Utils


{-| Defines a named step. This function is useful to define steps where a computation is expensive,
and can be skipped when the inputs don't change. _Make sure to read the correctness requirements._

**CORRECTNESS:**

1.  The name must be unique
2.  The second parameter must completely encode the parameter: if the input changes then the result of applying it to the input must change
3.  The last parameter must not use any value which is derived from previous steps

To satisfy the second condition you can consider using `Json.Encode.encode` or a [Codec](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/Codec#Codec). Files need to be output separately because there is no way to convert them into a `String`.

To satisfy the third condition the best strategy is to define a new top level function without explicit arguments.

Example:

    module Example.Mod exposing (foo)

    import Cache.Unsafe

    foo : String -> BuildTask FileOrDirectory
    foo =
        Cache.Unsafe.named "Example.Mod.foo" (\input -> { files = [], additionalData = [ input ] }) (\input ->
            -- do something with the input
        )

Notice how `foo` is a function but the definition doesn't specify any explicit parameters.

This approach is inspired by [noredink/elm-review-html-lazy](https://package.elm-lang.org/packages/noredink/elm-review-html-lazy/latest/UseMemoizedLazyLambda).

-}
named : String -> (a -> { files : List FileOrDirectory, additionalData : List String }) -> (a -> BuildTask tools e FileOrDirectory) -> a -> BuildTask tools e FileOrDirectory
named name toString action param =
    Internal.named name toString action param


{-| Pipe a file through a command.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
pipeThrough :
    (tools -> Command)
    -> List String
    -> FileOrDirectory
    -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
pipeThrough toCmd args hash =
    BuildTask.do (BuildTask.getTool toCmd) <| \tool ->
    pipeThrough_ tool args hash


pipeThrough_ :
    Command
    -> List String
    -> FileOrDirectory
    -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
pipeThrough_ cmd args hash =
    BuildTask.do (Internal.extendHashWith (Hash.toString cmd.hash :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("pipeThrough" :: cmd.name :: args)) outputHash <| \{ prefix, buildPath, env } target ->
    let
        label : String -> String
        label i =
            (prefix
                ++ String.padLeft 6 ' ' i
                :: Hash.toPath buildPath hash
                :: "through"
                :: Utils.viewEnv env
                :: cmd.name
                :: args
            )
                |> List.Extra.removeWhen String.isEmpty
                |> String.join " "
    in
    BackendTask.Extra.timed
        (label "Piping")
        (label "Piped")
        (Stream.fileRead (Hash.toPath buildPath hash)
            |> Stream.pipe (Stream.command (cmd.hash |> Hash.toPath buildPath) args)
            |> Stream.pipe (Stream.fileWrite (Hash.toPathTemporary buildPath target))
            |> Stream.readMetadata
            |> BackendTask.mapError Internal.UserError
        )


{-| Run a command in a specific generated directory and save the result to a file.

To avoid needing to copy files, the directory will be read-only. If you need a writable directory use `commandInWritableDirectory` instead. Only stdout is captured and cached.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandInReadonlyDirectory :
    Command
    -> List String
    -> FileOrDirectory
    -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandInReadonlyDirectory cmd args hash =
    BuildTask.do (Internal.extendHashWith (Hash.toString cmd.hash :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInReadonlyDirectory" :: cmd.name :: args)) outputHash <| \({ buildPath } as input) target ->
    Do.do
        (Internal.commandLog input cmd args
            |> BackendTask.inDir (Hash.toPath buildPath hash)
            |> BackendTask.mapError Internal.UserError
        )
    <| \output ->
    Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output }
        |> BackendTask.allowFatal
        |> BackendTask.mapError Internal.InternalError


{-| Run a command in a writable temporary directory seeded from a cached directory.

This is similar to `commandInWritableDirectoryOutput` but returns a `FileOrDirectory` containing the output instead of the output.

Unlike `commandInReadonlyDirectory` (which runs in the read-only cached directory directly),
this creates a writable copy so the command can create temporary files
(like `elm-stuff/` during compilation). Only stdout is captured and cached;
the temporary directory is discarded after the command completes.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandInWritableDirectory : Command -> List String -> FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandInWritableDirectory cmd args hash =
    commandInWritableDirectoryWith CommandOptions.default cmd args hash


{-| Run a command in a writable temporary directory seeded from a cached directory.

This is similar to `commandInWritableDirectoryOutput` but returns a `FileOrDirectory` containing the output instead of the output.

Unlike `commandInReadonlyDirectory` (which runs in the read-only cached directory directly),
this creates a writable copy so the command can create temporary files
(like `elm-stuff/` during compilation). Only stdout is captured and cached;
the temporary directory is discarded after the command completes.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandInWritableDirectoryWith : CommandOptions -> Command -> List String -> FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandInWritableDirectoryWith options cmd args hash =
    BuildTask.do (Internal.extendHashWith ("commandInWritableDirectoryWith" :: CommandOptions.toStringList options ++ Hash.toString cmd.hash :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInWritableDirectory" :: cmd.name :: args)) outputHash <| \({ internalTools, buildPath, debug } as input) target ->
    withWorkspace input (Hash.toWorkspace target) <| \workspacePath ->
    Do.do
        (Script.exec (Hash.toPath buildPath internalTools.cp.hash) [ "-r", Hash.toPath buildPath hash, workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec (Hash.toPath buildPath internalTools.chmod.hash) [ "-R", "u+w", workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (if debug then
            Script.log ("inDir: " ++ workspacePath)

         else
            BackendTask.succeed ()
        )
    <| \() ->
    Do.do
        (Internal.commandLogWith input options cmd args
            |> BackendTask.inDir workspacePath
            |> BackendTask.mapError Internal.UserError
        )
    <| \output ->
    Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output }
        |> BackendTask.allowFatal
        |> BackendTask.mapError Internal.InternalError


{-| Run a command in a writable temporary directory seeded from a cached directory.

This is similar to `commandInWritableDirectory` but returns the output instead of a `FileOrDirectory` containing the output.

Unlike `commandInReadonlyDirectory` (which runs in the read-only cached directory directly),
this creates a writable copy so the command can create temporary files
(like `elm-stuff/` during compilation). Only stdout is captured and cached;
the temporary directory is discarded after the command completes.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandInWritableDirectoryOutput : Command -> List String -> FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } String
commandInWritableDirectoryOutput cmd args hash =
    commandInWritableDirectoryOutputWith CommandOptions.default cmd args hash


{-| Run a command in a writable temporary directory seeded from a cached directory.

This is similar to `commandInWritableDirectory` but returns the output instead of a `FileOrDirectory` containing the output.

Unlike `commandInReadonlyDirectory` (which runs in the read-only cached directory directly),
this creates a writable copy so the command can create temporary files
(like `elm-stuff/` during compilation). Only stdout is captured and cached;
the temporary directory is discarded after the command completes.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandInWritableDirectoryOutputWith : CommandOptions -> Command -> List String -> FileOrDirectory -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } String
commandInWritableDirectoryOutputWith options cmd args hash =
    BuildTask.do
        (commandInWritableDirectoryWith options cmd args hash)
    <| \target ->
    BuildTask.withFile target BuildTask.succeed
        |> BuildTask.allowFatal
        |> Internal.fatalToInternal


{-| Run a command passing in a file (or directory) as last argument and save the result to a file.

**CORRECTNESS:**
The command must be deterministic. The same input should correspond to the same output.

In particular, the command must not:

  - get data from the internet,
  - read other files,
  - use the current time,
  - use any other source of randomness.

-}
commandWithFile :
    (tools -> Command)
    -> List String
    -> FileOrDirectory
    -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandWithFile toCmd args hash =
    BuildTask.do (BuildTask.getTool toCmd) <| \tool ->
    commandWithFile_ tool args hash


commandWithFile_ :
    Command
    -> List String
    -> FileOrDirectory
    -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandWithFile_ cmd args hash =
    BuildTask.do (Internal.extendHashWith (Hash.toString cmd.hash :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandWithFile" :: cmd.name :: args)) outputHash <| \({ buildPath } as input) target ->
    Do.do
        (Internal.commandLog input cmd (args ++ [ Hash.toPath buildPath hash ])
            |> BackendTask.mapError Internal.UserError
        )
    <| \output ->
    Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output }
        |> BackendTask.allowFatal
        |> BackendTask.mapError Internal.InternalError


{-| Downloads a file given its URL.

**CORRECTNESS:**
The file must never change on the server.

-}
downloadImmutable : String -> BuildTask tools { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
downloadImmutable url =
    BuildTask.do (Internal.hashFromString url) <| \outputHash ->
    Internal.derive ("downloadImmutable " ++ url) outputHash <| \{ buildPath } target ->
    Stream.http
        { url = url
        , method = "GET"
        , headers = []
        , body = Http.emptyBody
        , retries = Nothing
        , timeoutInMs = Nothing
        }
        |> Stream.pipe (Stream.fileWrite (Hash.toPathTemporary buildPath target))
        |> Stream.readMetadata
        |> BackendTask.mapError (\e -> Internal.UserError e)


{-| Patch a file.

**CORRECTNESS:**
If you change the function you must also change the description

-}
patchFileInDirectory :
    FileOrDirectory
    -> String
    -> { description : String }
    -> (String -> String)
    -> BuildTask tools { fatal : FatalError, recoverable : File.FileReadError e } FileOrDirectory
patchFileInDirectory hash filename { description } patch =
    BuildTask.do (Internal.extendHashWith [ "Patch", filename, description ] hash) <| \outputHash ->
    -- BuildTask.do (BuildTask.fail (FatalError.fromString "MEEP") |> Internal.fatalToInternal) <| \_ ->
    Internal.derive ("Patch " ++ filename ++ " with " ++ description) outputHash <| \({ internalTools, buildPath } as input) target ->
    withWorkspace input (Hash.toWorkspace target) <| \workspacePath ->
    Do.do
        (Script.exec (Hash.toPath buildPath internalTools.cp.hash) [ "-r", Hash.toPath buildPath hash, workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec (Hash.toPath buildPath internalTools.chmod.hash) [ "-R", "u+w", workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    File.rawFile (workspacePath ++ "/" ++ filename)
        |> BackendTask.mapError Internal.UserError
        |> BackendTask.andThen
            (\raw ->
                Script.writeFile
                    { path = workspacePath ++ "/" ++ filename
                    , body = patch raw
                    }
                    |> BackendTask.allowFatal
                    |> BackendTask.mapError Internal.InternalError
            )
        |> BackendTask.and
            (Internal.execLog input internalTools.cp [ "-rl", workspacePath, Hash.toPathTemporary buildPath target ]
                |> BackendTask.mapError Internal.InternalError
            )


withWorkspace : Input tools -> Hash Hash.Workspace -> (String -> BackendTask (Error e) ()) -> BackendTask (Error e) ()
withWorkspace input workspace task =
    let
        workspacePath : String
        workspacePath =
            Hash.toPathWorkspace input.buildPath workspace
    in
    Do.do
        (BackendTask.File.Extra.removeFileIfExists workspacePath
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    task workspacePath
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\r ->
                case r of
                    Err e ->
                        if input.keepFailed || input.debug then
                            BackendTask.fail e

                        else
                            BackendTask.File.Extra.removeFileIfExists workspacePath
                                |> BackendTask.mapError Internal.InternalError
                                |> BackendTask.andThen (\_ -> BackendTask.fail e)

                    Ok o ->
                        if input.debug then
                            BackendTask.succeed o

                        else
                            BackendTask.File.Extra.removeFileIfExists workspacePath
                                |> BackendTask.mapError Internal.InternalError
                                |> BackendTask.map (\_ -> o)
            )
