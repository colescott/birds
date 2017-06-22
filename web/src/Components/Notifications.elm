module Components.Notifications exposing (notifications)

import Html exposing (button, div, text, span, Html)
import Html.Events exposing (onClick)
import Html.Attributes exposing (type_, class)
import Types exposing (Msg(RemoveError))
import Array exposing (Array)


notifications : Array Types.Notification -> Html Msg
notifications errs =
    div [] <|
        List.map
            (\n ->
                case (Tuple.second n) of
                    Types.Error x ->
                        makeNotification "alert-danger" x <| Tuple.first n

                    Types.Warning x ->
                        makeNotification "alert-warning" x <| Tuple.first n

                    Types.Info x ->
                        makeNotification "alert-info" x <| Tuple.first n

                    Types.Success x ->
                        makeNotification "alert-success" x <| Tuple.first n
            )
        <|
            Array.toIndexedList errs


makeNotification : String -> String -> Int -> Html Msg
makeNotification class text id =
    div [ Html.Attributes.class <| "alert alert-dismissible fade show " ++ class ]
        [ button [ Html.Attributes.class "close", type_ "button", onClick <| RemoveError id ] [ span [] [ Html.text "×" ] ]
        , Html.text text
        ]
