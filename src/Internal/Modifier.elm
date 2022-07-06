module Internal.Modifier exposing
    ( Modifier(..)
    , dig
    , listItem
    , maybeItem
    , mget
    , root
    , set
    )

import Internal.Channel exposing (Channel)


{-| -}
type Modifier m m1
    = Modifier
        { mget : m -> Maybe m1
        , set : m1 -> m -> m
        }


{-| -}
mget : Modifier m m1 -> m -> Maybe m1
mget (Modifier mod) =
    mod.mget


{-| -}
set : Modifier m m1 -> m1 -> m -> m
set (Modifier mod) =
    mod.set


{-| -}
root : Modifier m m
root =
    Modifier
        { mget = Just
        , set = \m _ -> m
        }


{-| -}
dig :
    { get : m1 -> m2
    , set : m2 -> m1 -> m1
    }
    -> Modifier m m1
    -> Modifier m m2
dig r (Modifier mod) =
    Modifier
        { mget =
            \m0 ->
                mod.mget m0
                    |> Maybe.map r.get
        , set =
            \m2 m0 ->
                mod.mget m0
                    |> Maybe.map
                        (\m1 -> mod.set (r.set m2 m1) m0)
                    |> Maybe.withDefault m0
        }


{-| -}
maybeItem :
    { get : m1 -> Maybe ( Channel, m2 )
    , set : ( Channel, m2 ) -> m1 -> m1
    }
    -> Channel
    -> Modifier m m1
    -> Modifier m m2
maybeItem r expected (Modifier mod) =
    Modifier
        { mget =
            \m0 ->
                mod.mget m0
                    |> Maybe.andThen r.get
                    |> Maybe.andThen
                        (\( c, m2 ) ->
                            if c == expected then
                                Just m2

                            else
                                Nothing
                        )
        , set =
            \m2 m0 ->
                mod.mget m0
                    |> Maybe.andThen
                        (\m1 ->
                            r.get m1
                                |> Maybe.andThen
                                    (\( c, _ ) ->
                                        if c == expected then
                                            Just <| mod.set (r.set ( c, m2 ) m1) m0

                                        else
                                            Nothing
                                    )
                        )
                    |> Maybe.withDefault m0
        }


{-| -}
listItem :
    { get : m1 -> List ( Channel, m2 )
    , set : List ( Channel, m2 ) -> m1 -> m1
    }
    -> Channel
    -> Modifier m m1
    -> Modifier m m2
listItem r expected (Modifier mod) =
    Modifier
        { mget =
            \m0 ->
                mod.mget m0
                    |> Maybe.andThen
                        (\m1 ->
                            r.get m1
                                |> List.filterMap
                                    (\( c, m2 ) ->
                                        if c == expected then
                                            Just m2

                                        else
                                            Nothing
                                    )
                                |> List.head
                        )
        , set =
            \m2 m0 ->
                mod.mget m0
                    |> Maybe.map
                        (\m1 ->
                            let
                                newLs =
                                    r.get m1
                                        |> List.map
                                            (\( c, a ) ->
                                                if c == expected then
                                                    ( c, m2 )

                                                else
                                                    ( c, a )
                                            )
                            in
                            mod.set (r.set newLs m1) m0
                        )
                    |> Maybe.withDefault m0
        }
