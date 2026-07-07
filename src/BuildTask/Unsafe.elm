module BuildTask.Unsafe exposing (commandInReadonlyDirectory, commandInWritableDirectory, commandInWritableDirectoryOutput, commandInWritableDirectoryOutputWith, commandWithFile, downloadImmutable, named, patchFileInDirectory, pipeThrough)

{-| -}

import BackendTask
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.File.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, DownloadError, FileOrDirectory)
import BuildTask.Internal as Internal exposing (Error(..))
import CommandOptions exposing (CommandOptions)
import FatalError exposing (FatalError)
import Hash
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
named : String -> (a -> { files : List FileOrDirectory, additionalData : List String }) -> (a -> BuildTask e FileOrDirectory) -> a -> BuildTask e FileOrDirectory
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
    String
    -> List String
    -> FileOrDirectory
    -> BuildTask { fatal : FatalError, recoverable : Stream.Error () String } FileOrDirectory
pipeThrough cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("pipeThrough" :: cmd :: args)) outputHash <| \{ prefix, buildPath, env } target ->
    let
        label : String -> String
        label i =
            (prefix
                ++ String.padLeft 6 ' ' i
                :: Hash.toPath buildPath hash
                :: "through"
                :: Utils.viewEnv env
                :: cmd
                :: args
            )
                |> List.Extra.removeWhen String.isEmpty
                |> String.join " "
    in
    BackendTask.Extra.timed
        (label "Piping")
        (label "Piped")
        (Stream.fileRead (Hash.toPath buildPath hash)
            |> Stream.pipe (Stream.command cmd args)
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
    String
    -> List String
    -> FileOrDirectory
    -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandInReadonlyDirectory cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInReadonlyDirectory" :: cmd :: args)) outputHash <| \{ prefix, env, buildPath } target ->
    Do.do
        (Internal.commandLog prefix env cmd args
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
commandInWritableDirectory : String -> List String -> FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
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
commandInWritableDirectoryWith : CommandOptions -> String -> List String -> FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandInWritableDirectoryWith options cmd args hash =
    BuildTask.do (Internal.extendHashWith ("commandInWritableDirectoryWith" :: CommandOptions.toStringList options ++ cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInWritableDirectory" :: cmd :: args)) outputHash <| \{ prefix, buildPath, env, keepFailed } target ->
    let
        workspacePath : String
        workspacePath =
            Hash.toPathWorkspace buildPath (Hash.toWorkspace target)
    in
    Do.do
        (BackendTask.File.Extra.removeFileIfExists workspacePath
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec "cp" [ "-r", Hash.toPath buildPath hash, workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec "chmod" [ "-R", "u+w", workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Internal.commandLogWith options prefix env cmd args
            |> BackendTask.inDir workspacePath
            |> BackendTask.mapError Internal.UserError
            |> BackendTask.Extra.finally
                (if keepFailed then
                    BackendTask.succeed ()

                 else
                    BackendTask.File.Extra.removeFileIfExists workspacePath
                        |> BackendTask.mapError Internal.InternalError
                )
        )
    <| \output ->
    Do.do
        (BackendTask.File.Extra.removeFileIfExists workspacePath
            |> BackendTask.mapError Internal.InternalError
        )
    <| \() ->
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
commandInWritableDirectoryOutput : String -> List String -> FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } String
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
commandInWritableDirectoryOutputWith : CommandOptions -> String -> List String -> FileOrDirectory -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } String
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
    String
    -> List String
    -> FileOrDirectory
    -> BuildTask { fatal : FatalError, recoverable : Stream.Error Int String } FileOrDirectory
commandWithFile cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandWithFile" :: cmd :: args)) outputHash <| \{ prefix, buildPath, env } target ->
    Do.do
        (Internal.commandLog prefix env cmd (args ++ [ Hash.toPath buildPath hash ])
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
downloadImmutable : String -> BuildTask { recoverable : Stream.Error () String, fatal : FatalError } FileOrDirectory
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
    -> BuildTask { fatal : FatalError, recoverable : File.FileReadError e } FileOrDirectory
patchFileInDirectory hash filename { description } patch =
    BuildTask.do (Internal.extendHashWith [ "Patch", filename, description ] hash) <| \outputHash ->
    -- BuildTask.do (BuildTask.fail (FatalError.fromString "MEEP") |> Internal.fatalToInternal) <| \_ ->
    Internal.derive ("Patch " ++ filename ++ " with " ++ description) outputHash <| \{ keepFailed, prefix, env, buildPath } target ->
    let
        workspacePath : String
        workspacePath =
            Hash.toPathWorkspace buildPath (Hash.toWorkspace target)
    in
    Do.do
        (BackendTask.File.Extra.removeFileIfExists workspacePath
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec "cp" [ "-r", Hash.toPath buildPath hash, workspacePath ]
            |> BackendTask.mapError Internal.InternalError
        )
    <| \_ ->
    Do.do
        (Script.exec "chmod" [ "-R", "u+w", workspacePath ]
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
                    |> BackendTask.Extra.finally
                        (if keepFailed then
                            BackendTask.succeed ()

                         else
                            BackendTask.File.Extra.removeFileIfExists workspacePath
                                |> BackendTask.mapError Internal.InternalError
                        )
            )
        |> BackendTask.and
            (Internal.execLog prefix env "cp" [ "-rl", workspacePath, Hash.toPathTemporary buildPath target ]
                |> BackendTask.mapError Internal.InternalError
            )
        |> BackendTask.and
            (if keepFailed then
                BackendTask.File.Extra.removeFileIfExists workspacePath
                    |> BackendTask.mapError Internal.InternalError

             else
                BackendTask.succeed ()
            )
