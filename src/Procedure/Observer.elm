module Procedure.Observer exposing
    ( Observer
    , root
    , inherit
    , dig
    , digListElem
    )

{-|


# Observer

@docs Observer
@docs root
@docs inherit
@docs dig
@docs digListElem

-}

import Internal
    exposing
        ( Observer
        , ObserverId
        , initObserverId
        )


{-| Represent an _Observer_, which is a concept that links Views to Procedures. By specifying a view with its Observer, you can update the memory state of the view or capture events that occur on that view.
-}
type alias Observer m e m1 e1 =
    Internal.Observer m e m1 e1


{-| Root Observer, from which you can _dig_ up some interesting Observers.
-}
root : Observer m e m e
root =
    Internal.Observer
        { mget = Just
        , set = \x _ -> x
        , unwrap = Just
        , wrap = identity
        , id = initObserverId
        }


{-| Dig up a new `Observer` for the specific element.

  - mget: function to get the memory state from its parent
  - set: function to set the memory state on its parent
  - unwrap: function to unwrap parent Event
  - wrap: function to wrap the Event into its parent Event

-}
dig :
    { mget : m1 -> Maybe ( ObserverId, m2 )
    , set : ( ObserverId, m2 ) -> m1 -> m1
    , unwrap : e1 -> Maybe e2
    , wrap : e2 -> e1
    , id : ObserverId
    }
    -> Observer m e m1 e1
    -> Observer m e m2 e2
dig r (Internal.Observer o) =
    Internal.Observer
        { mget =
            \m0 ->
                o.mget m0
                    |> Maybe.andThen r.mget
                    |> Maybe.andThen
                        (\( oid2, m2 ) ->
                            if oid2 == o.id then
                                Just m2

                            else
                                Nothing
                        )
        , set =
            \m2 m0 ->
                case o.mget m0 of
                    Nothing ->
                        m0

                    Just m1 ->
                        o.set (r.set ( r.id, m2 ) m1) m0
        , unwrap =
            \e0 ->
                o.unwrap e0
                    |> Maybe.andThen r.unwrap
        , wrap =
            r.wrap >> o.wrap
        , id = r.id
        }


{-| Dig up a new Observer for the specific list element.

  - arg1:
      - get: function to get the list from its parent
      - set: function to set the list on its parent
      - unwrap: function to unwrap parent Event
      - wrap: function to wrap the Event into its parent Event
  - arg2: ObserverId for the list element

-}
digListElem :
    { get : m1 -> List ( ObserverId, m2 )
    , set : List ( ObserverId, m2 ) -> m1 -> m1
    , unwrap : e1 -> Maybe e2
    , wrap : e2 -> e1
    , id : ObserverId
    }
    -> Observer m e m1 e1
    -> Observer m e m2 e2
digListElem r =
    dig
        { id = r.id
        , mget =
            \m1 ->
                r.get m1
                    |> List.filter (\( oid, _ ) -> oid == r.id)
                    |> List.head
        , set =
            \( oid2, m2 ) m1 ->
                if oid2 == r.id then
                    r.set
                        (r.get m1
                            |> List.map
                                (\( oid, a ) ->
                                    if oid == r.id then
                                        ( oid, m2 )

                                    else
                                        ( oid, a )
                                )
                        )
                        m1

                else
                    m1
        , unwrap = r.unwrap
        , wrap = r.wrap
        }


{-| Build a new `Observer` that inherits the parent `ObserverId`. See [`Page.Login` in `spa-sample`](https://github.com/arowM/elm-procedure-architecture/blob/main/spa-sample/src/Page/Login.elm) for real use case.

  - get: function to get the memory state from its parent
  - set: function to set the memory state on its parent
  - unwrap: function to unwrap parent Event
  - wrap: function to wrap the Event into its parent Event

-}
inherit :
    { get : m1 -> m2
    , set : m2 -> m1 -> m1
    , unwrap : e1 -> Maybe e2
    , wrap : e2 -> e1
    }
    -> Observer m e m1 e1
    -> Observer m e m2 e2
inherit r (Internal.Observer o) =
    Internal.Observer
        { id = o.id
        , mget =
            \m0 ->
                o.mget m0
                    |> Maybe.map r.get
        , set =
            \m2 m0 ->
                case o.mget m0 of
                    Nothing ->
                        m0

                    Just m1 ->
                        o.set (r.set m2 m1) m0
        , unwrap =
            \e0 ->
                o.unwrap e0
                    |> Maybe.andThen r.unwrap
        , wrap =
            r.wrap >> o.wrap
        }
