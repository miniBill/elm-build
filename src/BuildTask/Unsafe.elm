module BuildTask.Unsafe exposing (named)

{-| -}

import BuildTask exposing (FileOrDirectory)
import BuildTask.Internal as Internal


{-| Defines a named step. This function is useful to define steps where a computation is expensive,
and can be skipped when the inputs don't change. _Make sure to read the correctness requirements._

**CORRECTNESS:**

1.  The name must be unique
2.  The second parameter must completely encode the parameter. It should be some form of `toString`.
3.  The last parameter must not use any value which is derived from previous steps

To satisfy the second condition you can use `Json.Encode.encode` or a [Codec](https://package.elm-lang.org/packages/miniBill/elm-codec/latest/Codec#Codec).

To satisfy the third condition the best strategy is to define a new top level function without explicit arguments.

Example:

    module Example.Mod exposing (foo)

    import Cache.Unsafe

    foo : String -> Cache.Monad FileOrDirectory
    foo =
        Cache.Unsafe.named "Example.Mod.foo" (Codec.encodeToString 0 codec) (\input ->
            -- do something with the input
        )

Notice how `foo` is a function but the definition doesn't specify any explicit parameters.

This approach is inspired by [noredink/elm-review-html-lazy](https://package.elm-lang.org/packages/noredink/elm-review-html-lazy/latest/UseMemoizedLazyLambda).

-}
named : String -> (a -> String) -> (a -> Cache.Monad FileOrDirectory) -> a -> Cache.Monad FileOrDirectory
named name toString action param =
    Internal.named name toString action param
