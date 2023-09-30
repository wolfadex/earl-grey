module Home exposing (Effect, InternalModel, Model, Msg, branch)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Route (Maybe User) InternalModel Msg Effect
branch =
    Sprig.branch
        { path = []
        }
        { init = init
        , subscriptions = subscriptions
        , update = update
        , urlChanged = urlChanged
        , view = view
        }


init : Sprig.Context (Maybe User) -> Sprig InternalModel Msg Effect
init context =
    { tag = Nothing
    }
        |> Sprig.save


type alias InternalModel =
    { tag : Maybe String
    }


type alias Model =
    Sprig.RouteModel InternalModel


type alias Effect =
    Never


type Msg
    = TagSelected String


subscriptions : Sprig.Context (Maybe User) -> InternalModel -> Sub Msg
subscriptions _ _ =
    Sub.none


update : Sprig.Context (Maybe User) -> Msg -> InternalModel -> Sprig InternalModel Msg Effect
update _ msg model =
    case msg of
        TagSelected tag ->
            { model | tag = Just tag }
                |> Sprig.save


urlChanged : Sprig.Context (Maybe User) -> InternalModel -> Sprig InternalModel Msg Effect
urlChanged _ model =
    Sprig.save model


view : Sprig.Context (Maybe User) -> InternalModel -> Html Msg
view context model =
    Html.div [ Html.Attributes.class "home-page" ]
        [ viewBanner
        , viewPageContainer context model
        ]


viewBanner : Html msg
viewBanner =
    Html.div [ Html.Attributes.class "banner" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.h1 [ Html.Attributes.class "logo-font" ] [ Html.text "conduit" ]
            , Html.p [] [ Html.text "A place to share your knowledge." ]
            ]
        ]


viewPageContainer : Sprig.Context (Maybe User) -> InternalModel -> Html Msg
viewPageContainer context model =
    Html.div [ Html.Attributes.class "container page" ]
        [ Html.div [ Html.Attributes.class "row" ]
            [ Html.div [ Html.Attributes.class "col-md-9" ]
                [ viewFeedToggle context model
                , viewArticlePreview
                , viewArticlePreview
                , viewPagination
                ]
            , Html.div [ Html.Attributes.class "col-md-3" ]
                [ viewSidebar
                ]
            ]
        ]


viewFeedToggle : Sprig.Context (Maybe User) -> InternalModel -> Html msg
viewFeedToggle context model =
    Html.div [ Html.Attributes.class "feed-toggle" ]
        [ Html.ul [ Html.Attributes.class "nav nav-pills outline-active" ]
            [ case Sprig.flags context of
                Nothing ->
                    Html.text ""

                Just _ ->
                    Html.li [ Html.Attributes.class "nav-item" ]
                        [ Html.a [ Html.Attributes.class "nav-link" ] [ Html.text "Your Feed" ] ]
            , Html.li [ Html.Attributes.class "nav-item" ]
                [ Html.a [ Html.Attributes.class "nav-link active" ] [ Html.text "Global Feed" ] ]
            , case model.tag of
                Nothing ->
                    Html.text ""

                Just tag ->
                    Html.li [ Html.Attributes.class "nav-item" ]
                        [ Html.span [ Html.Attributes.class "nav-link active" ] [ Html.text ("# " ++ tag) ] ]
            ]
        ]


viewArticlePreview : Html msg
viewArticlePreview =
    Html.div [ Html.Attributes.class "article-preview" ]
        [ viewArticleMeta
        , viewArticlePreviewLink
        ]


viewArticleMeta : Html msg
viewArticleMeta =
    Html.div [ Html.Attributes.class "article-meta" ]
        [ Html.a [ Html.Attributes.href "/profile/eric-simons" ]
            [ Html.img [ Html.Attributes.src "http://i.imgur.com/Qr71crq.jpg" ] [] ]
        , Html.div [ Html.Attributes.class "info" ]
            [ Html.a [ Html.Attributes.href "/profile/eric-simons", Html.Attributes.class "author" ] [ Html.text "Eric Simons" ]
            , Html.span [ Html.Attributes.class "date" ] [ Html.text "January 20th" ]
            ]
        , Html.button [ Html.Attributes.class "btn btn-outline-primary btn-sm pull-xs-right" ]
            [ Html.i [ Html.Attributes.class "ion-heart" ] []
            , Html.text " 29"
            ]
        ]


viewArticlePreviewLink : Html msg
viewArticlePreviewLink =
    Html.a [ Html.Attributes.href "/article/how-to-build-webapps-that-scale", Html.Attributes.class "preview-link" ]
        [ Html.h1 [] [ Html.text "How to build webapps that scale" ]
        , Html.p [] [ Html.text "This is the description for the post." ]
        , Html.span [] [ Html.text "Read more..." ]
        , Html.ul [ Html.Attributes.class "tag-list" ]
            [ Html.li [ Html.Attributes.class "tag-default tag-pill tag-outline" ] [ Html.text "realworld" ]
            , Html.li [ Html.Attributes.class "tag-default tag-pill tag-outline" ] [ Html.text "implementations" ]
            ]
        ]


viewPagination : Html msg
viewPagination =
    Html.ul [ Html.Attributes.class "pagination" ]
        [ Html.li [ Html.Attributes.class "page-item active" ]
            [ Html.a [ Html.Attributes.class "page-link", Html.Attributes.href "" ] [ Html.text "1" ] ]
        , Html.li [ Html.Attributes.class "page-item" ]
            [ Html.a [ Html.Attributes.class "page-link", Html.Attributes.href "" ] [ Html.text "2" ] ]
        ]


viewSidebar : Html Msg
viewSidebar =
    Html.div [ Html.Attributes.class "sidebar" ]
        [ Html.p [] [ Html.text "Popular Tags" ]
        , Html.div [ Html.Attributes.class "tag-list" ]
            [ viewTagPill "programming"
            , viewTagPill "javascript"
            , viewTagPill "emberjs"
            , viewTagPill "angularjs"
            , viewTagPill "react"
            , viewTagPill "mean"
            , viewTagPill "node"
            , viewTagPill "rails"
            ]
        ]


viewTagPill : String -> Html Msg
viewTagPill tag =
    Html.span
        [ Html.Attributes.class "tag-pill tag-default"
        , Html.Events.onClick (TagSelected tag)
        ]
        [ Html.text tag ]
