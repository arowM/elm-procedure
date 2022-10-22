module Internal.TestBuilder exposing
    ( TestBuilder
    , run
    )

{-
@docs TestBuilder
@docs run
-}


import Test exposing (Test)
import Test.Expect exposing (Expectation)
import Test.Sequence as TestSequence

{-| -}
type TestBuilder a = TestBuilder (TestBuilder_ a)


type alias TestBuilder_ a =
    { currentTitle : String
    , currentSequence : TestSequence a
    , run : Test -> Test
    }


type alias TestSequence a = TestSequence.Sequence a


{-| -}
run : String -> TestBuilder a -> Test
run description (TestBuilder builder) =
    ( (builder.currentSequence
        |> TestSequence.run
      ) :: builder.accumulatedTests
    )
    |> List.reverse
    |> Test.describe description



{-| -}
newTest : String -> TestBuilder ()
newTest description =
    TestBuilder
        { currentTitle = 
        , currentSequence = TestSequence.description description
        , accumulatedTests = []
        }

{-| -}
newSection : String -> TestBuilder () -> TestBuilder ()
newSection description (TestBuilder builder) =
    TestBuilder
        { currentSequence = TestSequence.describe description
        , accumulatedTests =
            TestSequence.run builder.currentSequence
                :: builder.accumulatedTests
        }

{-| -}
map : (a -> b) -> TestBuilder a -> TestBuilder b
map f (TestBuilder builder) =
    TestBuilder
        { currentSequence = TestSequence.map f builder.currentSequence
        , accumulatedTests = builder.accumulatedTests
        }


{-| -}
andThen : String -> (a -> Maybe b) -> TestBuilder a -> TestBuilder b
andThen description f (TestBuilder builder) =
    TestBuilder
        { currentSequence =
            TestSequence.andThen description f builder.currentSequence
        , accumulatedTests = builder.accumulatedTests
        }

{-| -}
assert : String -> (a -> Expectation) -> TestBuilder a -> TestBuilder a
assert description f (TestBuilder builder) =
    TestBuilder
        { currentSequence =
            TestSequence.assert description f builder.currentSequence
        , accumulatedTests = builder.accumulatedTests
        }


{-| -}
sequenceCases : (a -> List (Sequence ())) -> Sequence a -> Sequence ()
sequenceCases f seq =
    seq
        |> TestSequence.andThen
            (\a ->
                List.foldl
                    (\seq acc ->
                        TestSequence.run seq
                    )
                    ...
                    (f a)
