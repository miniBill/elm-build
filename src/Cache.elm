module Cache exposing
    ( FileOrDirectory, input, inputs
    , Monad, do, succeed, fail
    , writeFile, run
    , map, map2, andThen, combine, combineBy, each, sequence
    , pipeThrough, commandWithFile, commandInFolder, withFile
    , withPrefix, timed
    , cpuCount, triggerDebugger
    )

{-|


## Input

@docs FileOrDirectory, input, inputs


## Building blocks

@docs Monad, do, succeed, fail


## Output

@docs writeFile, run


## Transforming and combining `Monad` values

@docs map, map2, andThen, combine, combineBy, each, sequence


## Operations

@docs pipeThrough, commandWithFile, commandInFolder, withFile


## Output control

@docs withPrefix, timed


## Utils

@docs cpuCount, triggerDebugger

-}

import BackendTask exposing (BackendTask)
import BackendTask.Custom
import BackendTask.Do as Do
import BackendTask.Extra
import BackendTask.File as File
import BackendTask.Stream as Stream
import Dict
import Dict.Extra
import Env
import FNV1a
import FatalError exposing (FatalError)
import Hex
import Json.Decode
import Json.Encode
import Pages.Script as Script
import Path exposing (Path)
import Result.Extra
import Set exposing (Set)


{-| -}
type Monad a
    = Monad
        (HashSet -- deps
         -> HashSet -- existing
         -> List String
         -> Path
         -> BackendTask FatalError ( a, HashSet )
        )


{-| -}
succeed : a -> Monad a
succeed v =
    Monad (\deps _ _ _ -> BackendTask.succeed ( v, deps ))


{-| -}
map : (a -> b) -> Monad a -> Monad b
map f (Monad m) =
    Monad
        (\deps existing prefix path ->
            m deps existing prefix path
                |> BackendTask.map
                    (\( v, newDeps ) ->
                        ( f v, newDeps )
                    )
        )


{-| -}
map2 : (a -> b -> c) -> Monad a -> Monad b -> Monad c
map2 f (Monad a) (Monad b) =
    Monad
        (\deps existing prefix path ->
            BackendTask.map2
                (\( va, depsA ) ( vb, depsB ) ->
                    ( f va vb, hashSetUnion depsA depsB )
                )
                (a deps existing prefix path)
                (b deps existing prefix path)
        )


{-| -}
andThen : (a -> Monad b) -> Monad a -> Monad b
andThen f (Monad m) =
    Monad
        (\deps existing prefix buildPath ->
            m deps existing prefix buildPath
                |> BackendTask.andThen
                    (\( v, newDeps ) ->
                        let
                            (Monad n) =
                                f v
                        in
                        n newDeps existing prefix buildPath
                    )
        )


{-| -}
fail : String -> Monad a
fail msg =
    triggerDebugger <|
        \_ ->
            Monad (\_ _ _ _ -> BackendTask.fail (FatalError.fromString msg))


{-| -}
triggerDebugger : (() -> Monad a) -> Monad a
triggerDebugger k =
    Monad
        (\deps _ _ _ ->
            BackendTask.Custom.run "triggerDebugger" Json.Encode.null (Json.Decode.succeed ( (), deps ))
                |> BackendTask.allowFatal
        )
        |> andThen k


{-| -}
input : Path -> (FileOrDirectory -> Monad a) -> Monad a
input inputPath k =
    Monad
        (\deps _ prefix _ ->
            Do.do (commandLog prefix "b3sum" [ Path.toString inputPath ]) <|
                \body ->
                    case inputHash body of
                        Err e ->
                            BackendTask.fail (FatalError.fromString e)

                        Ok hash ->
                            BackendTask.succeed ( hash, deps )
        )
        |> andThen
            (\hash ->
                derive {- "input" -} hash <|
                    \target prefix buildPath ->
                        execLog prefix "cp" [ Path.toString inputPath, hashToPath buildPath target ]
            )
        |> andThen k


{-| -}
inputs :
    List Path
    ->
        BackendTask
            FatalError
            (List
                ( Path
                , Monad FileOrDirectory
                )
            )
inputs inputPaths =
    Do.do (commandLog [] "b3sum" (List.map Path.toString inputPaths)) <|
        \body ->
            List.map2
                (\inputPath line ->
                    case inputHash line of
                        Ok hash ->
                            ( inputPath
                            , derive {- "inputs" -} hash <| \target prefix buildPath -> execLog prefix "cp" [ Path.toString inputPath, hashToPath buildPath target ]
                            )
                                |> Ok

                        Err e ->
                            Err (FatalError.fromString e)
                )
                inputPaths
                (String.lines body)
                |> Result.Extra.combine
                |> BackendTask.fromResult


derive :
    -- String ->
    FileOrDirectory
    ->
        (FileOrDirectory
         -> List String
         -> Path
         -> BackendTask FatalError ()
        )
    -> Monad FileOrDirectory
derive {- description -} target inner =
    Monad
        (\deps existing prefix buildPath ->
            -- Do.do
            --     (appendLog
            --         (hashToPath buildPath  target
            --             ++ ": "
            --             ++ description
            --             ++ " from "
            --             ++ (prev
            --                     |> Set.toList
            --                     |> String.join ", "
            --                )
            --         )
            --         |> BackendTask.quiet
            --     )
            -- <| \_ ->
            if hashSetMember target existing || hashSetMember target deps then
                BackendTask.succeed ( target, hashSetInsert target deps )

            else
                let
                    tmp : FileOrDirectory
                    tmp =
                        hashToTmp target
                in
                Do.exec "rm" [ "-rf", hashToPath buildPath tmp ] <|
                    \_ ->
                        Do.do
                            (inner tmp prefix buildPath
                                |> BackendTask.onError
                                    (\e ->
                                        Do.exec "rm" [ "-rf", hashToPath buildPath tmp ] <|
                                            \_ ->
                                                BackendTask.fail e
                                    )
                            )
                        <|
                            \_ ->
                                Do.exec "mv" [ hashToPath buildPath tmp, hashToPath buildPath target ] <|
                                    \_ ->
                                        Do.exec "chmod" [ "-R", "a=rX", hashToPath buildPath target ] <|
                                            \_ ->
                                                BackendTask.succeed ( target, hashSetInsert target deps )
        )



-- appendLog : String -> BackendTask FatalError ()
-- appendLog line =
--     BackendTask.Custom.run "appendLog" (Json.Encode.string line) (Json.Decode.succeed ())
--         |> BackendTask.allowFatal


{-| -}
combine : List { filename : Path, hash : FileOrDirectory } -> Monad FileOrDirectory
combine files =
    let
        deduplicated : List { filename : Path, hash : FileOrDirectory }
        deduplicated =
            files
                |> Dict.Extra.groupBy (\{ filename } -> Path.toString filename)
                |> Dict.values
                |> List.concatMap (List.take 1)
                |> List.sortBy (\{ filename, hash } -> ( Path.toString filename, hashToString hash ))

        outputHash : FileOrDirectory
        outputHash =
            deduplicated
                |> List.map
                    (\{ filename, hash } ->
                        extendHashWith [ Path.toString filename ] hash
                    )
                |> combineHashes
    in
    derive {- "combine" -} outputHash <|
        \target prefix buildPath ->
            Do.do Env.parallelism <|
                \parallelism ->
                    let
                        withOutput : List ( { hash : FileOrDirectory, filename : Path }, String )
                        withOutput =
                            files
                                |> List.map
                                    (\file ->
                                        let
                                            outputFilename : String
                                            outputFilename =
                                                hashToPath buildPath target
                                                    ++ "/"
                                                    ++ Path.toString file.filename
                                        in
                                        ( file, outputFilename )
                                    )

                        dirs : List String
                        dirs =
                            withOutput
                                |> List.map
                                    (\( _, outputFilename ) ->
                                        outputFilename
                                            |> Path.path
                                            |> Path.directory
                                            |> Path.toString
                                    )
                                |> Set.fromList
                                |> Set.toList
                    in
                    Do.do
                        (dirs
                            |> List.map (\dir -> execLog prefix "mkdir" [ "-p", dir ])
                            |> BackendTask.Extra.combineBy_ parallelism
                        )
                    <|
                        \_ ->
                            withOutput
                                |> List.map (\( file, outputFilename ) -> execLog prefix "cp" [ "-rl", hashToPath buildPath file.hash, outputFilename ])
                                |> BackendTask.Extra.combineBy_ parallelism


{-| -}
pipeThrough : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
pipeThrough cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("pipeThrough" :: cmd :: args)) -} outputHash <|
        \target prefix buildPath ->
            BackendTask.Extra.timed
                (String.join " " (prefix ++ "Piping" :: hashToPath buildPath hash :: "through" :: cmd :: args))
                (String.join " " (prefix ++ "Piped " :: hashToPath buildPath hash :: "through" :: cmd :: args))
                (Stream.fileRead (hashToPath buildPath hash)
                    |> Stream.pipe (Stream.command cmd args)
                    |> Stream.pipe (Stream.fileWrite (hashToPath buildPath target))
                    |> Stream.run
                )
    )
        |> andThen k


{-| Run a command in a specific generated folder and save the result to a file.

**IMPORTANT**: The command should only read files from that folder, otherwise elm-build has no way to know when to re-run it.

-}
commandInFolder : String -> List String -> FileOrDirectory -> (FileOrDirectory -> Monad a) -> Monad a
commandInFolder cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("commandInFolder" :: cmd :: args)) -} outputHash <|
        \target prefix buildPath ->
            Do.do (commandLog prefix cmd args |> BackendTask.inDir (hashToPath buildPath hash)) <|
                \output ->
                    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = output })
    )
        |> andThen k


{-| Run a command passing in a file (or folder) as last argument and save the result to a file.

**IMPORTANT**: The command should only read that file or folder, otherwise elm-build has no way to know when to re-run it.

-}
commandWithFile :
    String
    -> List String
    -> FileOrDirectory
    -> (FileOrDirectory -> Monad a)
    -> Monad a
commandWithFile cmd args hash k =
    let
        outputHash : FileOrDirectory
        outputHash =
            extendHashWith (cmd :: args) hash
    in
    (derive {- (String.join " " ("commandWithFile" :: cmd :: args)) -} outputHash <|
        \target prefix buildPath ->
            Do.do (commandLog prefix cmd (args ++ [ hashToPath buildPath hash ])) <|
                \output ->
                    BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = output })
    )
        |> andThen k


{-| -}
execLog : List String -> String -> List String -> BackendTask FatalError ()
execLog prefix cmd args =
    logCommand prefix cmd args (Script.exec cmd args)


{-| -}
commandLog : List String -> String -> List String -> BackendTask FatalError String
commandLog prefix cmd args =
    logCommand prefix
        cmd
        args
        (Stream.commandWithOptions
            (Stream.defaultCommandOptions |> Stream.withOutput Stream.PrintStderr)
            cmd
            args
            |> Stream.read
            |> BackendTask.map .body
            |> BackendTask.allowFatal
        )


{-| -}
logCommand : List String -> String -> List String -> BackendTask error a -> BackendTask error a
logCommand prefix cmd args task =
    BackendTask.Extra.timed
        (String.join " " (prefix ++ "Running" :: cmd :: args))
        (String.join " " (prefix ++ "Ran    " :: cmd :: args))
        task


{-| -}
do : Monad b -> (b -> Monad a) -> Monad a
do x f =
    andThen f x


{-| -}
withFile : FileOrDirectory -> (String -> Monad a) -> (a -> Monad b) -> Monad b
withFile hash f k =
    Monad
        (\existing deps prefix buildPath ->
            Do.allowFatal (File.rawFile (hashToPath buildPath hash)) <|
                \raw ->
                    let
                        (Monad m) =
                            f raw
                    in
                    m existing (hashSetInsert hash deps) prefix buildPath
        )
        |> andThen k


{-| -}
writeFile : String -> (FileOrDirectory -> Monad a) -> Monad a
writeFile content k =
    let
        hash : FileOrDirectory
        hash =
            stringToHash content
    in
    (derive {- "writeFile" -} hash <|
        \target _ buildPath ->
            BackendTask.allowFatal (Script.writeFile { path = hashToPath buildPath target, body = content })
    )
        |> andThen k


{-| -}
run : Path -> Monad FileOrDirectory -> BackendTask FatalError { output : Path, dependencies : List Path }
run buildPath (Monad m) =
    Do.do (listExisting buildPath) <|
        \existing ->
            m hashSetEmpty existing [] buildPath
                |> BackendTask.map
                    (\( output, deps ) ->
                        { output = Path.path (hashToPath buildPath output)
                        , dependencies =
                            deps
                                |> hashSetToList
                                |> List.map (\raw -> raw |> hashToPath buildPath |> Path.path)
                        }
                    )


listExisting : Path -> BackendTask FatalError HashSet
listExisting path =
    BackendTask.Custom.run "readdir"
        (Json.Encode.string (Path.toString path))
        (Json.Decode.list Json.Decode.string)
        |> BackendTask.allowFatal
        |> BackendTask.map
            (\raw ->
                HashSet (Set.fromList raw)
            )


{-| -}
combineBy : Int -> List (Monad a) -> Monad (List a)
combineBy n ops =
    Monad
        (\deps existing prefix buildPath ->
            ops
                |> List.indexedMap
                    (\i (Monad m) -> BackendTask.map (Tuple.pair i) (m deps existing prefix buildPath))
                |> BackendTask.Extra.combineBy n
                |> BackendTask.map
                    (\res ->
                        res
                            |> List.sortBy Tuple.first
                            |> List.map Tuple.second
                            |> List.unzip
                            |> Tuple.mapSecond (List.foldl hashSetUnion hashSetEmpty)
                    )
        )


{-| -}
each : List a -> (a -> Monad b) -> (List b -> Monad c) -> Monad c
each l f k =
    l |> List.map f |> sequence |> andThen k


{-| -}
sequence : List (Monad a) -> Monad (List a)
sequence ops =
    Monad
        (\deps existing prefix buildPath ->
            ops
                |> List.map
                    (\(Monad m) -> m deps existing prefix buildPath)
                |> BackendTask.sequence
                |> BackendTask.map
                    (\res ->
                        res
                            |> List.unzip
                            |> Tuple.mapSecond (List.foldl hashSetUnion hashSetEmpty)
                    )
        )


{-| -}
timed : String -> String -> Monad a -> Monad a
timed before after (Monad task) =
    Monad (\deps existing prefix buildPath -> BackendTask.Extra.timed before after (task deps existing prefix buildPath))


{-| -}
withPrefix : String -> Monad a -> Monad a
withPrefix newPrefix (Monad m) =
    Monad (\deps existing prefix buildPath -> m deps existing (newPrefix :: prefix) buildPath)


cpuCount : (Int -> Monad a) -> Monad a
cpuCount k =
    Monad
        (\deps _ _ _ ->
            Do.command "nproc" [ "--all" ] <|
                \raw ->
                    let
                        trimmed : String
                        trimmed =
                            String.trim raw
                    in
                    case String.toInt trimmed of
                        Nothing ->
                            BackendTask.fail (FatalError.fromString ("Could not parse nproc output: " ++ Json.Encode.encode 0 (Json.Encode.string trimmed)))

                        Just n ->
                            BackendTask.succeed ( n, deps )
        )
        |> andThen k



------------
-- HASHES --
------------


{-| -}
type FileOrDirectory
    = Hash String


{-| Build an hashed file from the output of shaXsum/b3sum.
-}
inputHash : String -> Result String FileOrDirectory
inputHash raw =
    let
        clean : String
        clean =
            String.left 8 raw
    in
    Hex.fromString clean
        |> Result.map (\_ -> Hash clean)


hashToPath : Path -> FileOrDirectory -> String
hashToPath buildPath (Hash hash) =
    Path.toString buildPath ++ "/" ++ hash


hashToString : FileOrDirectory -> String
hashToString (Hash hash) =
    hash


hashToTmp : FileOrDirectory -> FileOrDirectory
hashToTmp (Hash hash) =
    Hash ("tmp-" ++ hash)


combineHashes : List FileOrDirectory -> FileOrDirectory
combineHashes deps =
    let
        h : String
        h =
            deps
                |> List.map hashToString
                |> String.join "|"
    in
    stringToHash h


extendHashWith : List String -> FileOrDirectory -> FileOrDirectory
extendHashWith l (Hash r) =
    stringToHash (String.join "|" (r :: l))


stringToHash : String -> FileOrDirectory
stringToHash raw =
    raw
        |> FNV1a.hash
        |> Hex.toString
        |> String.padLeft 8 '0'
        |> Hash


type HashSet
    = HashSet (Set String)


hashSetEmpty : HashSet
hashSetEmpty =
    HashSet Set.empty


hashSetMember : FileOrDirectory -> HashSet -> Bool
hashSetMember (Hash x) (HashSet s) =
    Set.member x s


hashSetInsert : FileOrDirectory -> HashSet -> HashSet
hashSetInsert (Hash x) (HashSet s) =
    HashSet (Set.insert x s)


hashSetUnion : HashSet -> HashSet -> HashSet
hashSetUnion (HashSet a) (HashSet b) =
    HashSet (Set.union a b)


hashSetToList : HashSet -> List FileOrDirectory
hashSetToList (HashSet s) =
    Set.foldr (\e a -> Hash e :: a) [] s
