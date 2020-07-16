module HumanTime exposing (format, formatTimeAgo)

import Date exposing (Date)
import Time exposing (Month(..))


format : Time.Zone -> Time.Posix -> String
format timeZone time =
    toDay timeZone time
        ++ " "
        ++ toMonth timeZone time
        ++ " "
        ++ toYear timeZone time
        ++ " "
        ++ toHour timeZone time
        ++ ":"
        ++ toMinute timeZone time


toYear timeZone =
    Time.toYear timeZone >> String.fromInt


toMonth timeZone =
    Time.toMonth timeZone >> monthToString


toDay timeZone =
    Time.toDay timeZone >> String.fromInt


toHour timeZone =
    Time.toHour timeZone >> String.fromInt >> String.padLeft 2 '0'


toMinute timeZone =
    Time.toMinute timeZone >> String.fromInt >> String.padLeft 2 '0'


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"


formatTimeAgo : Time.Zone -> Time.Posix -> Time.Posix -> String
formatTimeAgo zone now time =
    if Time.posixToMillis now - Time.posixToMillis time > 0 then
        formatDuration (calcDiff zone time now) ++ " ago"

    else
        "in " ++ formatDuration (calcDiff zone now time)


calcDiff : Time.Zone -> Time.Posix -> Time.Posix -> Duration
calcDiff zone start end =
    case calcDateDiff (Date.fromPosix zone start) (Date.fromPosix zone end) of
        Day 0 ->
            timeDuration (secondDiff zone start end)

        Day days ->
            let
                seconds =
                    secondDiff zone start end
            in
            if seconds < 0 then
                case days - 1 of
                    0 ->
                        timeDuration (24 * 3600 + seconds)

                    x ->
                        Day x

            else
                Day days

        x ->
            x


calcDateDiff : Date -> Date -> Duration
calcDateDiff start end =
    let
        years =
            Date.diff Date.Years start end
    in
    if years /= 0 then
        Year years

    else
        let
            months =
                Date.diff Date.Months start end
        in
        if months /= 0 then
            Month months

        else
            Day (Date.diff Date.Days start end)


secondDiff : Time.Zone -> Time.Posix -> Time.Posix -> Int
secondDiff zone start end =
    secondsOfDay zone end - secondsOfDay zone start


secondsOfDay : Time.Zone -> Time.Posix -> Int
secondsOfDay zone time =
    60 * (60 * Time.toHour zone time + Time.toMinute zone time) + Time.toSecond zone time


timeDuration : Int -> Duration
timeDuration seconds =
    case seconds // 60 of
        0 ->
            Second seconds

        minutes ->
            case minutes // 60 of
                0 ->
                    Minute minutes

                hours ->
                    Hour hours


type Duration
    = Second Int
    | Minute Int
    | Hour Int
    | Day Int
    | Month Int
    | Year Int


formatDuration : Duration -> String
formatDuration duration =
    let
        plural n =
            if n > 1 then
                "s"

            else
                ""

        ( durationAmount, unit ) =
            case duration of
                Second seconds ->
                    ( seconds, "second" )

                Minute minutes ->
                    ( minutes, "minute" )

                Hour hours ->
                    ( hours, "hour" )

                Day days ->
                    ( days, "day" )

                Month months ->
                    ( months, "month" )

                Year years ->
                    ( years, "year" )
    in
    String.fromInt durationAmount ++ " " ++ unit ++ plural durationAmount
