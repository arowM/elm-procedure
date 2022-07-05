module Procedure.Observer exposing
    ( Observer
    , root
    , inherit
    , child
    , listItem
    )

{-|


# Observer

@docs Observer
@docs root
@docs inherit
@docs child
@docs listItem

-}

import Internal.Observer as Observer
import Internal.ObserverId as ObserverId exposing (ObserverId)


{-| -}
type alias Observer m e m1 =
    Observer.Observer m e m1


{-| Root Observer, from which you can _dig_ up some interesting Observers.
-}
root : Observer m e m
root =
    Observer.fromRecord
        { mget = \m0 -> Just (ObserverId.init, m0)
        , set = \(_, m1) _ -> m1
        , expected = ObserverId.init
        }


{-| Build a child Observer from its parent.

  - arg1:
    - mget: function to get the memory state from its parent
    - set: function to set the memory state on its parent
  - arg2: expected Observer ID for the child

-}
child :
    { mget : m1 -> Maybe ( ObserverId e2, m2 )
    , set : ( ObserverId e2, m2 ) -> m1 -> m1
    }
    -> ObserverId e2
    -> Observer m e1 m1
    -> Observer m e2 m2
child r expected o =
    Observer.fromRecord
        { mget =
            \m0 ->
                Observer.mget o m0
                    |> Maybe.andThen
                        (\(_, m1) -> r.mget m1)
        , set =
            \p2 m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        m0

                    Just (oid1, m1) ->
                        Observer.set o (oid1, r.set p2 m1) m0
        , expected = expected
        }


{-| Build a new list item Observer from its parent.

  - arg1:
    - get: function to get the memory state from its parent
    - set: function to set the memory state on its parent
  - arg2: expected Observer ID for the list item

-}
listItem :
    { get : m1 -> List ( ObserverId e2, m2 )
    , set : List ( ObserverId e2, m2 ) -> m1 -> m1
    }
    -> ObserverId e2
    -> Observer m e1 m1
    -> Observer m e2 m2
listItem r expected o =
    Observer.fromRecord
        { mget =
            \m0 ->
                Observer.mget o m0
                    |> Maybe.andThen
                        (\(_, m1) ->
                            r.get m1
                                |> List.filter
                                    (\(oid2, _) -> oid2 == expected)
                                |> List.head
                        )
        , set =
            \p2 m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        m0

                    Just (oid1, m1) ->
                        let
                            p2s = List.map
                                (\(oid2, m2) ->
                                    if oid2 == Tuple.first p2 then
                                        (oid2, Tuple.second p2)
                                    else
                                        (oid2, m2)
                                )
                                (r.get m1)

                        in
                        Observer.set o (oid1, r.set p2s m1) m0
        , expected = expected
        }


{-| Build a new Observer that inherits the parent Observer Id. See [`Page.Login` in `spa-sample`](https://github.com/arowM/elm-procedure-architecture/blob/main/spa-sample/src/Page/Login.elm) for real use case.

  - get: function to get the memory state from its parent
  - set: function to set the memory state on its parent

-}
inherit :
    { get : m1 -> m2
    , set : m2 -> m1 -> m1
    }
    -> Observer m e m1
    -> Observer m e m2
inherit r o =
    Observer.fromRecord
        { mget =
            \m0 ->
                Observer.mget o m0
                    |> Maybe.map
                        (\(oid1, m1) -> (oid1, r.get m1))
        , set =
            \(_, m2) m0 ->
                case Observer.mget o m0 of
                    Nothing ->
                        m0

                    Just (oid1, m1) ->
                        Observer.set o (oid1, r.set m2 m1) m0
        , expected = Observer.expected o
        }
