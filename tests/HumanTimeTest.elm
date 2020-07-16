module HumanTimeTest exposing (..)

import Expect exposing (Expectation)
import HumanTime exposing (formatTimeAgo)
import Iso8601
import Test exposing (..)
import Time


suite : Test
suite =
    describe "formatTimeAgo"
        [ test "one year difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:00:00Z")
                        (toTime "2019-01-01T00:00:00Z")
                    )
                    "1 year ago"
        , test "one year and 11 month difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-12-01T00:00:00Z")
                        (toTime "2019-01-01T00:00:00Z")
                    )
                    "1 year ago"
        , test "two year difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2021-01-01T00:00:00Z")
                        (toTime "2019-01-01T00:00:00Z")
                    )
                    "2 years ago"
        , test "11 month difference within same year" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2019-12-01T00:00:00Z")
                        (toTime "2019-01-01T00:00:00Z")
                    )
                    "11 months ago"
        , test "11 month difference over different years" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:00:00Z")
                        (toTime "2019-02-01T00:00:00Z")
                    )
                    "11 months ago"
        , test "one month difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-02-01T00:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "1 month ago"
        , test "one month and 30 days difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-03-31T00:00:00Z")
                        (toTime "2020-02-01T00:00:00Z")
                    )
                    "1 month ago"
        , test "two month difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-04-01T00:00:00Z")
                        (toTime "2020-02-01T00:00:00Z")
                    )
                    "2 months ago"
        , test "30 days difference within same month" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-31T00:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "30 days ago"
        , test "30 days difference over different months" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-02-09T00:00:00Z")
                        (toTime "2020-01-10T00:00:00Z")
                    )
                    "30 days ago"
        , test "29 days difference over different months" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-05-09T00:00:00Z")
                        (toTime "2020-04-10T00:00:00Z")
                    )
                    "29 days ago"
        , test "one day difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-02T00:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "1 day ago"
        , test "two days difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-03T00:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "2 days ago"
        , test "23 hours difference within the same day" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T23:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "23 hours ago"
        , test "23 hours difference over different days" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-02T00:00:00Z")
                        (toTime "2020-01-01T01:00:00Z")
                    )
                    "23 hours ago"
        , test "one hour difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T01:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "1 hour ago"
        , test "two hours difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T02:00:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "2 hours ago"
        , test "59 minutes difference within the same hour" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:59:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "59 minutes ago"
        , test "59 minutes difference over different hours" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T01:00:00Z")
                        (toTime "2020-01-01T00:01:00Z")
                    )
                    "59 minutes ago"
        , test "one minute difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:01:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "1 minute ago"
        , test "two minutes difference" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:02:00Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "2 minutes ago"
        , test "59 seconds difference within the same minute" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:00:59Z")
                        (toTime "2020-01-01T00:00:00Z")
                    )
                    "59 seconds ago"
        , test "59 seconds difference over different minutes" <|
            \_ ->
                Expect.equal
                    (formatTimeAgo Time.utc
                        (toTime "2020-01-01T00:01:00Z")
                        (toTime "2020-01-01T00:00:01Z")
                    )
                    "59 seconds ago"
        ]


toTime : String -> Time.Posix
toTime =
    Iso8601.toTime >> Result.toMaybe >> Maybe.withDefault (Time.millisToPosix 0)
