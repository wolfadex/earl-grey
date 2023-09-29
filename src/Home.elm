module Home exposing (Effect, Model, Msg, branch)

import Html exposing (Html)
import Html.Attributes
import Sprig exposing (Sprig)
import User exposing (User)


branch : Sprig.Branch (Maybe User) Model Msg Effect
branch =
    { init = init
    , subscriptions = subscriptions
    , update = update
    , urlChanged = urlChanged
    , view = view
    }


init : Sprig.Context (Maybe User) -> Sprig Model Msg Effect
init context =
    {}
        |> Sprig.save


type alias Model =
    {}


type alias Effect =
    Never


type Msg
    = NoOp


subscriptions : Sprig.Context (Maybe User) -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


update : Sprig.Context (Maybe User) -> Msg -> Model -> Sprig Model Msg Effect
update _ _ model =
    Sprig.save model


urlChanged : Sprig.Context (Maybe User) -> Model -> Sprig Model Msg Effect
urlChanged _ model =
    Sprig.save model


view : Sprig.Context (Maybe User) -> Model -> Html Msg
view _ _ =
    Html.div [ Html.Attributes.class "home-page" ]
        [ viewBanner
        , viewPageContainer
        ]


viewBanner : Html msg
viewBanner =
    Html.div [ Html.Attributes.class "banner" ]
        [ Html.div [ Html.Attributes.class "container" ]
            [ Html.h1 [ Html.Attributes.class "logo-font" ] [ Html.text "conduit" ]
            , Html.p [] [ Html.text "A place to share your knowledge." ]
            ]
        ]


viewPageContainer : Html msg
viewPageContainer =
    {-
       <div class="container page">
           <div class="row">
           <div class="col-md-9">
               <div class="feed-toggle">
               <ul class="nav nav-pills outline-active">
                   <li class="nav-item">
                   <a class="nav-link" href="">Your Feed</a>
                   </li>
                   <li class="nav-item">
                   <a class="nav-link active" href="">Global Feed</a>
                   </li>
               </ul>
               </div>

               <div class="article-preview">
               <div class="article-meta">
                   <a href="/profile/eric-simons"><img src="http://i.imgur.com/Qr71crq.jpg" /></a>
                   <div class="info">
                   <a href="/profile/eric-simons" class="author">Eric Simons</a>
                   <span class="date">January 20th</span>
                   </div>
                   <button class="btn btn-outline-primary btn-sm pull-xs-right">
                   <i class="ion-heart"></i> 29
                   </button>
               </div>
               <a href="/article/how-to-build-webapps-that-scale" class="preview-link">
                   <h1>How to build webapps that scale</h1>
                   <p>This is the description for the post.</p>
                   <span>Read more...</span>
                   <ul class="tag-list">
                   <li class="tag-default tag-pill tag-outline">realworld</li>
                   <li class="tag-default tag-pill tag-outline">implementations</li>
                   </ul>
               </a>
               </div>

               <div class="article-preview">
               <div class="article-meta">
                   <a href="/profile/albert-pai"><img src="http://i.imgur.com/N4VcUeJ.jpg" /></a>
                   <div class="info">
                   <a href="/profile/albert-pai" class="author">Albert Pai</a>
                   <span class="date">January 20th</span>
                   </div>
                   <button class="btn btn-outline-primary btn-sm pull-xs-right">
                   <i class="ion-heart"></i> 32
                   </button>
               </div>
               <a href="/article/the-song-you" class="preview-link">
                   <h1>The song you won't ever stop singing. No matter how hard you try.</h1>
                   <p>This is the description for the post.</p>
                   <span>Read more...</span>
                   <ul class="tag-list">
                   <li class="tag-default tag-pill tag-outline">realworld</li>
                   <li class="tag-default tag-pill tag-outline">implementations</li>
                   </ul>
               </a>
               </div>

               <ul class="pagination">
               <li class="page-item active">
                   <a class="page-link" href="">1</a>
               </li>
               <li class="page-item">
                   <a class="page-link" href="">2</a>
               </li>
               </ul>
           </div>

           <div class="col-md-3">
               <div class="sidebar">
               <p>Popular Tags</p>

               <div class="tag-list">
                   <a href="" class="tag-pill tag-default">programming</a>
                   <a href="" class="tag-pill tag-default">javascript</a>
                   <a href="" class="tag-pill tag-default">emberjs</a>
                   <a href="" class="tag-pill tag-default">angularjs</a>
                   <a href="" class="tag-pill tag-default">react</a>
                   <a href="" class="tag-pill tag-default">mean</a>
                   <a href="" class="tag-pill tag-default">node</a>
                   <a href="" class="tag-pill tag-default">rails</a>
               </div>
               </div>
           </div>
           </div>
       </div>
    -}
    Html.div [ Html.Attributes.class "container page" ]
        [ Html.div [ Html.Attributes.class "row" ]
            [ Html.div [ Html.Attributes.class "col-md-9" ]
                [ viewFeedToggle
                , viewArticlePreview
                , viewArticlePreview
                , viewPagination
                ]
            , Html.div [ Html.Attributes.class "col-md-3" ]
                [ viewSidebar
                ]
            ]
        ]


viewFeedToggle : Html msg
viewFeedToggle =
    Html.div [ Html.Attributes.class "feed-toggle" ]
        [ Html.ul [ Html.Attributes.class "nav nav-pills outline-active" ]
            [ Html.li [ Html.Attributes.class "nav-item" ]
                [ Html.a [ Html.Attributes.class "nav-link" ] [ Html.text "Your Feed" ] ]
            , Html.li [ Html.Attributes.class "nav-item" ]
                [ Html.a [ Html.Attributes.class "nav-link active" ] [ Html.text "Global Feed" ] ]
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


viewSidebar : Html msg
viewSidebar =
    Html.div [ Html.Attributes.class "sidebar" ]
        [ Html.p [] [ Html.text "Popular Tags" ]
        , Html.div [ Html.Attributes.class "tag-list" ]
            [ Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "programming" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "javascript" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "emberjs" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "angularjs" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "react" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "mean" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "node" ]
            , Html.a [ Html.Attributes.class "tag-pill tag-default", Html.Attributes.href "" ] [ Html.text "rails" ]
            ]
        ]
