port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (..)


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias ResumeModel =
    List Section


type alias Section =
    { heading : String
    , caption : String
    , class : String
    , data : SectionDataType
    }


type SectionDataType
    = TextString String
    | ListTextString (List String)
    | SectionData Section
    | ListSectionData (List Section)



-- TODO: Need to keep option values and Test if values are not there.


init : () -> ( ResumeModel, Cmd Msg )
init _ =
    ( List.singleton (Section "Initial Data" "" "first" (TextString "first data"))
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ResumeContent ResumeModel


update : Msg -> ResumeModel -> ( ResumeModel, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ResumeContent result ->
            ( result, Cmd.none )



-- Subscriptions


subscriptions : ResumeModel -> Sub Msg
subscriptions _ =
    onFetchingData (\payload -> ResumeContent <| runResumeModelDecoder payload)


port onFetchingData : (Encode.Value -> msg) -> Sub msg


runResumeModelDecoder : Encode.Value -> ResumeModel
runResumeModelDecoder payload =
    case Decode.decodeValue resumeDataDecoder payload of
        Ok resumeContent ->
            resumeContent

        Err err ->
            List.singleton (Section "Error" "" "temp" (TextString (Decode.errorToString err)))



-- VIEW


view : ResumeModel -> Html Msg
view model =
    div [ class "resume" ] (List.map resumeSectionVew model)


resumeSectionVew : Section -> Html Msg
resumeSectionVew section =
    div [ class section.class, class "section-format" ]
        [ strong [] [ text section.heading ]
        , text section.caption
        , div [] (resumeSectionDataView section.data)
        ]


resumeSectionDataView : SectionDataType -> List (Html Msg)
resumeSectionDataView sectiondata =
    case sectiondata of
        TextString data ->
            List.singleton (text data)

        ListTextString data ->
            List.map text data

        SectionData data ->
            List.singleton (resumeSectionVew data)

        ListSectionData data ->
            List.map resumeSectionVew data



-- Decoders


resumeDataDecoder : Decode.Decoder ResumeModel
resumeDataDecoder =
    Decode.list resumeSectionDecoder


resumeSectionDecoder : Decode.Decoder Section
resumeSectionDecoder =
    Decode.succeed Section
        |> required "heading" Decode.string
        |> optional "caption" Decode.string "default"
        |> optional "class" Decode.string ""
        |> required "data" resumeSectionDataDecoder


resumeSectionDataDecoder : Decode.Decoder SectionDataType
resumeSectionDataDecoder =
    Decode.oneOf
        [ Decode.map TextString Decode.string
        , Decode.map ListTextString (Decode.list Decode.string)
        , Decode.map SectionData (Decode.lazy (\_ -> resumeSectionDecoder))
        , Decode.map ListSectionData (Decode.list (Decode.lazy (\_ -> resumeSectionDecoder)))
        ]
