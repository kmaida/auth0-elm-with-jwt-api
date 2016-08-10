port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import String

import Http
import Http.Decorators
import Task exposing (Task)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)

main : Program (Maybe Model)
main = 
    Html.programWithFlags
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
    , protectedQuote : String 
    , errorMsg : String
    }
    
init : Maybe Model -> (Model, Cmd Msg)
init model =
    case model of 
        Just model ->
            ( model, fetchRandomQuoteCmd )

        Nothing ->
            ( Model "" "" "" "" "" "", fetchRandomQuoteCmd )    
    
{-
    UPDATE
    * API routes
    * GET and POST
    * Encode request body 
    * Decode responses
    * Messages
    * Ports
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
    
loginUrl : String
loginUrl =
    api ++ "sessions/create"   
    
protectedQuoteUrl : String
protectedQuoteUrl = 
    api ++ "api/protected/random-quote"      

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
    
-- GET request for random protected quote (authenticated)
    
fetchProtectedQuote : Model -> Task Http.Error String
fetchProtectedQuote model = 
    { verb = "GET"
    , headers = [ ("Authorization", "Bearer " ++ model.token) ]
    , url = protectedQuoteUrl
    , body = Http.empty
    }
    |> Http.send Http.defaultSettings  
    |> Http.Decorators.interpretStatus -- decorates Http.send result so error type is Http.Error instead of RawError
    |> Task.map responseText    
    
fetchProtectedQuoteCmd : Model -> Cmd Msg
fetchProtectedQuoteCmd model = 
    Task.perform HttpError FetchProtectedQuoteSuccess <| fetchProtectedQuote model 
    
-- Extract GET plain text response to get protected quote    
    
responseText : Http.Response -> String
responseText response = 
    case response.value of 
        Http.Text t ->
            t 
        _ ->
            ""

-- Helper to update model and set localStorage with the updated model

setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model = 
    ( model, setStorage model )   

-- Messages

type Msg 
    = GetQuote
    | FetchQuoteSuccess String
    | HttpError Http.Error
    | AuthError Http.Error
    | SetUsername String
    | SetPassword String
    | ClickRegisterUser
    | ClickLogIn
    | GetTokenSuccess String
    | GetProtectedQuote
    | FetchProtectedQuoteSuccess String
    | LogOut

-- Ports

port setStorage : Model -> Cmd msg  
port removeStorage : Model -> Cmd msg

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

        ClickLogIn ->
            ( model, authUserCmd model loginUrl ) 

        GetTokenSuccess newToken ->
            setStorageHelper { model | token = newToken, password = "", errorMsg = "" }

        GetProtectedQuote ->
            ( model, fetchProtectedQuoteCmd model )

        FetchProtectedQuoteSuccess newPQuote ->
            setStorageHelper { model | protectedQuote = newPQuote }
            
        LogOut ->
            ( { model | username = "", protectedQuote = "", token = "" }, removeStorage model )
                       
{-
    VIEW
    * Hide sections of view depending on authenticaton state of model
    * Get a quote
    * Log In or Register
    * Get a protected quote
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
                        , p [ class "text-center" ] [
                            button [ class "btn btn-danger", onClick LogOut ] [ text "Log Out" ]
                        ]   
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
                            button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                            , button [ class "btn btn-link", onClick ClickRegisterUser ] [ text "Register" ]
                        ] 
                    ]

        -- If user is logged in, show button and quote; if logged out, show a message instructing them to log in
        protectedQuoteView = 
            let
                -- If no protected quote, apply a class of "hidden"
                hideIfNoProtectedQuote : String
                hideIfNoProtectedQuote = 
                    if String.isEmpty model.protectedQuote then "hidden" else ""

            in        
                if loggedIn then
                    div [] [
                        p [ class "text-center" ] [
                            button [ class "btn btn-info", onClick GetProtectedQuote ] [ text "Grab a protected quote!" ]
                        ]
                        -- Blockquote with protected quote: only show if a protectedQuote is present in model
                        , blockquote [ class hideIfNoProtectedQuote ] [ 
                            p [] [text model.protectedQuote] 
                        ]
                    ]    
                else
                    p [ class "text-center" ] [ text "Please log in or register to see protected quotes." ]  

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
            ], div [] [
                h2 [ class "text-center" ] [ text "Protected Chuck Norris Quotes" ]
                -- Protected quotes
                , protectedQuoteView
            ]
        ]