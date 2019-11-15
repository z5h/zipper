module Zipper exposing
    ( Zipper
    , zip, unzip, value
    , into, intoMaybe
    , map, mapMaybe
    , andThenInto
    )

{-|


# Definition

@docs Zipper


# Creating & returning

@docs zip, unzip, value


# Traversing

Traverse into a structure, maintaining a focus which can be mapped,
and maintaining a way to "unzip" out of the zipper having the focused operations
affect the structure.

@docs into, intoMaybe, andThenIntoMaybe


# Mapping

@docs map, mapMaybe

-}


{-| Represents a Zipper that started at a `root` but has traversed into the
structure and is now at `focus`

For example, here we traverse into `.a` so are focused on the value there.

    example : Zipper { b : number } { a : { b : number } }
    example =
        { a = { b = True } }
            |> zip
            |> into .a (\a parent -> { parent | a = a })

-}
type Zipper focus root
    = Zipper { focus : focus, up : focus -> root }


{-| Create a zipper from a value, focused on that value.
-}
zip : root -> Zipper root root
zip root =
    Zipper { focus = root, up = identity }


{-| Return the value of the current focus.

    { a = { b = True } }
        |> zip
        |> into .a (\a parent -> { parent | a = a })
        |> value
        == { b = True }

-}
value : Zipper focus root -> focus
value (Zipper { focus }) =
    focus


{-| Return the root of the zipper, with all updates that have occurred to
focused parts of the zipper.

e.g.

    { a = { b = 1 } }
        |> zip
        |> into .a (\a parent -> { parent | a = a })
        |> into .b (\b parent -> { parent | b = b })
        |> map (\n -> n + 1)
        |> unzip
        == { a = { b = 2 } }

-}
unzip : Zipper focus root -> root
unzip (Zipper { focus, up }) =
    up focus


{-| Traverses into a a structure. You need to provide a way into the new focus

        (focus -> newFocus)

and a way to apply an updated newFocus to the current focus.

        (newFocus -> focus -> focus)

Typically the boilerplate will follow this pattern:

        {a = someNestedData }
            |> zip
            |> into .a (\a parent -> { parent | a = a})

-}
into :
    (focus -> newFocus)
    -> (newFocus -> focus -> focus)
    -> Zipper focus root
    -> Zipper newFocus root
into f update (Zipper z) =
    Zipper
        { focus = f z.focus
        , up = \newFocus -> z.up (update newFocus z.focus)
        }


{-| Like `into`, but for when your focus is a `Maybe focus`.
Note the new focus will also be a maybe. Continue using `intoMaybe` as
you traverse into your structure (until you need `andThenInto`).
-}
intoMaybe :
    (focus -> newFocus)
    -> (newFocus -> focus -> focus)
    -> Zipper (Maybe focus) root
    -> Zipper (Maybe newFocus) root
intoMaybe f update =
    into (Maybe.map f) (Maybe.map2 update)


{-| Borrowing from the name and idea of `Maybe.andThen`,
this function is useful for when the current focus is a `Maybe focus` and
the traversal function is `(focus -> Maybe newFocus)`. As you traverse deeper
into your structure you'll choose between `intoMaybe` or `andThenInto` based on
whether your
-}
andThenInto :
    (focus -> Maybe newFocus)
    -> (Maybe newFocus -> focus -> focus)
    -> Zipper (Maybe focus) root
    -> Zipper (Maybe newFocus) root
andThenInto f update (Zipper z) =
    Zipper
        { focus = z.focus |> Maybe.andThen f
        , up =
            \maybeNewFocus ->
                z.up (z.focus |> Maybe.map (\zFocus -> update maybeNewFocus zFocus))
        }


{-| Map the focused value. When this zipper is unzipped, the new value will
replace the old.
-}
map : (focus -> focus) -> Zipper focus root -> Zipper focus root
map f (Zipper { focus, up }) =
    Zipper { focus = f focus, up = up }


{-| Like `map`, but for when the focus is a Maybe.
-}
mapMaybe : (focus -> focus) -> Zipper (Maybe focus) root -> Zipper (Maybe focus) root
mapMaybe f =
    map (Maybe.map f)
