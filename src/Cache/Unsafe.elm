module Cache.Unsafe exposing (named)

{-| -}

import Cache exposing (FileOrDirectory)
import Cache.Internal as Internal


{-| Defines a named step. This function is useful to define steps where a computation is expensive,
and can be skipped when the inputs don't change. _Make sure to read the correctness requirements._

**CORRECTNESS:**

1.  The name must be unique
2.  The function passed in must not use any value which is derived from previous steps

To satisfy the second condition the best strategy is to define a new top level function without explicit arguments.

Example:

    module Example.Mod exposing (foo)

    import Cache.Unsafe

    foo : String -> Cache.Monad FileOrDirectory
    foo =
        Cache.Unsafe.named "Example.Mod.foo" identity (\input ->
            -- do something with the input
        )

Notice how `foo` is a function but the definition doesn't specify any explicit parameters.

This approach is inspired by [noredink/elm-review-html-lazy](https://package.elm-lang.org/packages/noredink/elm-review-html-lazy/latest/UseMemoizedLazyLambda).

-}
named : String -> (a -> String) -> (a -> Cache.Monad FileOrDirectory) -> a -> Cache.Monad FileOrDirectory
named name encode action param =
    Internal.named name encode action param
