module BuildTask.Unsafe exposing (commandInReadonlyDirectory, commandInWritableDirectory, commandWithFile, downloadImmutable, named, pipeThrough)

{-| -}

import BackendTask
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.Http as Http
import BackendTask.Stream as Stream
import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Internal as Internal
import Hash
import Pages.Script as Script


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
named : String -> (a -> { files : List FileOrDirectory, additionalData : List String }) -> (a -> BuildTask FileOrDirectory) -> a -> BuildTask FileOrDirectory
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
pipeThrough : String -> List String -> FileOrDirectory -> BuildTask FileOrDirectory
pipeThrough cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("pipeThrough" :: cmd :: args)) outputHash <| \{ prefix, buildPath } target ->
    BackendTask.Extra.timed
        (String.join " " (prefix ++ "Piping" :: Hash.toPath buildPath hash :: "through" :: cmd :: args))
        (String.join " " (prefix ++ "Piped " :: Hash.toPath buildPath hash :: "through" :: cmd :: args))
        (Stream.fileRead (Hash.toPath buildPath hash)
            |> Stream.pipe (Stream.command cmd args)
            |> Stream.pipe (Stream.fileWrite (Hash.toPathTemporary buildPath target))
            |> Stream.run
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
commandInReadonlyDirectory : String -> List String -> FileOrDirectory -> BuildTask FileOrDirectory
commandInReadonlyDirectory cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInReadonlyDirectory" :: cmd :: args)) outputHash <| \{ prefix, buildPath } target ->
    Do.do (Internal.commandLog prefix cmd args |> BackendTask.inDir (Hash.toPath buildPath hash)) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output })


{-| Run a command in a writable temporary directory seeded from a cached directory.

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
commandInWritableDirectory : String -> List String -> FileOrDirectory -> BuildTask FileOrDirectory
commandInWritableDirectory cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandInWritableDirectory" :: cmd :: args)) outputHash <| \{ prefix, buildPath } target ->
    let
        workspacePath : String
        workspacePath =
            Hash.toPathWorkspace buildPath (Hash.toWorkspace target)
    in
    Do.exec "rm" [ "-rf", workspacePath ] <| \_ ->
    Do.exec "cp" [ "-r", Hash.toPath buildPath hash, workspacePath ] <| \_ ->
    Do.exec "chmod" [ "-R", "u+w", workspacePath ] <| \_ ->
    Do.do
        (Internal.commandLog prefix cmd args
            |> BackendTask.inDir workspacePath
            |> BackendTask.Extra.finally
                (Script.exec "rm" [ "-rf", workspacePath ])
        )
    <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output })


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
    -> BuildTask FileOrDirectory
commandWithFile cmd args hash =
    BuildTask.do (Internal.extendHashWith (cmd :: args) hash) <| \outputHash ->
    Internal.derive (String.join " " ("commandWithFile" :: cmd :: args)) outputHash <| \{ prefix, buildPath } target ->
    Do.do (Internal.commandLog prefix cmd (args ++ [ Hash.toPath buildPath hash ])) <| \output ->
    BackendTask.allowFatal (Script.writeFile { path = Hash.toPathTemporary buildPath target, body = output })


downloadImmutable : String -> BuildTask FileOrDirectory
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
        |> Stream.run
