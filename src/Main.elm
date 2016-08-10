module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String

import Http
import Task exposing (Task)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

main : Program Never
main = 
    Html.program 
        { init = init 
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
    
{- 
    MODEL
    * Model type 
    * Initialize model with empty values
    * Initialize with a random quote
-}

type alias Model =
    { username : String
    , password : String
    , token : String
    , quote : String
    , errorMsg : String
    }
    
init : (Model, Cmd Msg)
init =
    ( Model "" "" "" "" "", fetchRandomQuoteCmd )
    
{-
    UPDATE
    * API routes
    * GET and POST
    * Encode request body 
    * Decode responses
    * Messages
    * Update case
-}

-- API request URLs
    
api : String
api =
     "http://localhost:3001/"    
    
randomQuoteUrl : String
randomQuoteUrl =    
    api ++ "api/random-quote"
    
registerUrl : String
registerUrl =
    api ++ "users"       

-- GET a random quote (unauthenticated)
    
fetchRandomQuote : Platform.Task Http.Error String
fetchRandomQuote =
    Http.getString randomQuoteUrl
    
fetchRandomQuoteCmd : Cmd Msg
fetchRandomQuoteCmd =
    Task.perform HttpError FetchQuoteSuccess fetchRandomQuote     

-- Encode user to construct POST request body (for Register and Log In)
    
userEncoder : Model -> Encode.Value
userEncoder model = 
    Encode.object 
        [ ("username", Encode.string model.username)
        , ("password", Encode.string model.password) 
        ]          

-- POST register / login request
    
authUser : Model -> String -> Task Http.Error String
authUser model apiUrl =
    { verb = "POST"
    , headers = [ ("Content-Type", "application/json") ]
    , url = apiUrl
    , body = Http.string <| Encode.encode 0 <| userEncoder model
    }
    |> Http.send Http.defaultSettings
    |> Http.fromJson tokenDecoder    
    
authUserCmd : Model -> String -> Cmd Msg    
authUserCmd model apiUrl = 
    Task.perform AuthError GetTokenSuccess <| authUser model apiUrl
    
-- Decode POST response to get token
    
tokenDecoder : Decoder String
tokenDecoder =
    "id_token" := Decode.string         
    
-- Messages

type Msg 
    = GetQuote
    | FetchQuoteSuccess String
    | HttpError Http.Error
    | AuthError Http.Error
    | SetUsername String
    | SetPassword String
    | ClickRegisterUser
    | GetTokenSuccess String

-- Update

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetQuote ->
            ( model, fetchRandomQuoteCmd )

        FetchQuoteSuccess newQuote ->
            ( { model | quote = newQuote }, Cmd.none )

        HttpError _ ->
            ( model, Cmd.none )  

        AuthError error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )  

        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        SetPassword password ->
            ( { model | password = password }, Cmd.none )

        ClickRegisterUser ->
            ( model, authUserCmd model registerUrl )

        GetTokenSuccess newToken ->
            ( { model | token = newToken, errorMsg = "" } |> Debug.log "got new token", Cmd.none )  
                       
{-
    VIEW
    * Hide sections of view depending on authenticaton state of model
    * Get a quote
    * Register
-}

view : Model -> Html Msg
view model =
    let 
        -- Is the user logged in?
        loggedIn : Bool
        loggedIn =
            if String.length model.token > 0 then True else False 

        -- If the user is logged in, show a greeting; if logged out, show the login/register form
        authBoxView =
            let
                -- If there is an error on authentication, show the error alert
                showError : String
                showError = 
                    if String.isEmpty model.errorMsg then "hidden" else ""  

                -- Greet a logged in user by username
                greeting : String
                greeting =
                    "Hello, " ++ model.username ++ "!"

            in        
                if loggedIn then
                    div [id "greeting" ][
                        h3 [ class "text-center" ] [ text greeting ]
                        , p [ class "text-center" ] [ text "You have super-secret access to protected quotes." ]  
                    ] 
                else
                    div [ id "form" ] [
                        h2 [ class "text-center" ] [ text "Log In or Register" ]
                        , p [ class "help-block" ] [ text "If you already have an account, please Log In. Otherwise, enter your desired username and password and Register." ]
                        , div [ class showError ] [
                            div [ class "alert alert-danger" ] [ text model.errorMsg ]
                        ]
                        , div [ class "form-group row" ] [
                            div [ class "col-md-offset-2 col-md-8" ] [
                                label [ for "username" ] [ text "Username:" ]
                                , input [ id "username", type' "text", class "form-control", Html.Attributes.value model.username, onInput SetUsername ] []
                            ]    
                        ]
                        , div [ class "form-group row" ] [
                            div [ class "col-md-offset-2 col-md-8" ] [
                                label [ for "password" ] [ text "Password:" ]
                                , input [ id "password", type' "password", class "form-control", Html.Attributes.value model.password, onInput SetPassword ] []
                            ]    
                        ]
                        , div [ class "text-center" ] [
                            button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                        ] 
                    ]
                           
    in
        div [ class "container" ] [
            h2 [ class "text-center" ] [ text "Chuck Norris Quotes" ]
            , p [ class "text-center" ] [
                button [ class "btn btn-success", onClick GetQuote ] [ text "Grab a quote!" ]
            ]
            -- Blockquote with quote
            , blockquote [] [ 
                p [] [text model.quote] 
            ]
            , div [ class "jumbotron text-left" ] [
                -- Login/Register form or user greeting
                authBoxView
            ]
        ]