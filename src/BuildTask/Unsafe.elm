module BuildTask.Unsafe exposing (named)

{-| -}

import BuildTask exposing (BuildTask, FileOrDirectory)
import BuildTask.Internal as Internal


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
