# elm-build
`elm-build` is a generic build system. It contains helpers for Elm projects but can be used for any build task.

The way to use it is to describe the final folder by using the `BuildTask` type.  
More specifically, you give it a `BuildTask FileOrDirectory` and it builds the file (or directory) according to the `BuildTask`.

`BuildTask`s can be built from either:
1. `input`: this takes a file from your project, hashes it and returns a `BuildTask FileOrDirectory`;
2. various transformations - mostly `pipeThrough` for piping files through programs such as `uglify` or `magick convert` or `elm-format`;
3. the standard building blocks (`succeed`/`mapN`/`andThen`);
4. combining multiple `FileOrDirectory` in a new `FileOrDirectory`.

An example of what the user would write is at https://github.com/miniBill/elm-build/blob/main/src/Buildfile.elm#L75

This reads images and fonts, and generates `Images.elm` and `Fonts.elm` to reference them, while also converting the images to jpegxl/avif/webp and resizing them so they can be used in `srcset`s

If you've used `elm-pages` scripts this will familiar. The main difference between a `BackendTask` and `BuildTask` is that `BuildTask`s are designed to be deterministic, and that they automatically keep track of dependencies

The main advantages over `make` are that:
1. you can write your rules in Elm and
2. there is no dependency on modification times - `elm-build` always hashes the inputs and then infers what to rebuild

It's _stateless_ and _self-tracking_.
