module Lesson exposing (Lesson, decodeLesson, encodeLesson)


type alias Lesson =
    { id_ : String
    , title : String
    , branch : String
    , prerequisites : List Prerequisite
    , data : Maybe String
    }


decodeLesson : Decoder Lesson
decodeLesson =
    D.map5 Lesson
        (D.field "id" D.string)
        (D.field "title" D.string)
        (D.field "branch" D.string)
        (D.field "prerequisites" <| D.list decodePrerequisite)
        (D.maybe <| D.field "data" D.string)


encodeLesson : Lesson -> Value
encodeLesson lesson =
    E.object
        [ ( "id", E.string lesson.id_ )
        , ( "title", E.string lesson.title )
        , ( "branch", E.string lesson.branch )
        , ( "prerequisites", E.list <| List.map encodePrerequisite lesson.prerequisites )
        , ( "data", Maybe.withDefault E.null <| Maybe.map E.string lesson.data )
        ]
