module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
{-
   Elm Calculator
   by: maxhero A.K.A Marcelo Amâncio de Lima Santos
-}

main =
    Html.beginnerProgram { model = { lastNum = "", operator = None, display = "" }, view = view, update = update }

unsafeToFloat : Result a Float -> Float
unsafeToFloat result =
    case result of
        Ok float ->
            float

        Err _ ->
            0.0

applyOperator : BOperator -> Float -> Float -> String
applyOperator a b c =
    case a of
        Add ->
            Basics.toString (b + c)

        Subtract ->
            Basics.toString (b - c)

        Divide ->
            if not <| b == 0 && c == 0 then
                "No Divisions by 0 here"
            else
                Basics.toString (b / c)

        Multiply ->
            Basics.toString (b * c)

        Power ->
            Basics.toString (b ^ c)

        _ ->
            Debug.crash "Algo deu errado"

type Msg
    = Input String
    | BinaryOperation BOperator
    | RootOperation VOperator
    | Percent
    | Squared
    | UnderOne
    | ChangeSignal
    | Comma
    | CleanEntry
    | CleanAll
    | Backspace
    | Apply

type BOperator 
    = Add
    | Subtract
    | Divide
    | Multiply
    | Power
    | None

type VOperator
    = Sqrt 

update msg ({display, lastNum, operator} as model) =
    case msg of
        Input string ->
            if display == "" && string == "0" then
                model
            else
                {model | display = display ++ string}

        BinaryOperation op ->
            {lastNum = display, operator = op, display = ""}
        
        RootOperation op ->
            {model | display = display
                        |> String.toFloat 
                        |> unsafeToFloat
                        |> sqrt
                        |> toString
            }
        
        Percent -> 
            if not <| lastNum == "" then
        
                case operator of
        
                    Multiply -> 
                        {model | display = display
                                           |> String.toFloat
                                           |> unsafeToFloat
                                           |> flip (/) 100.0
                                           |> toString
                        }
        
                    _ ->
                        let
                            left =
                                lastNum
                                |>String.toFloat
                                |> unsafeToFloat
                                |> flip (/) 100.0

                            right =
                                display
                                |> String.toFloat
                                |> unsafeToFloat
                        in
                        {model | display = toString(left * right)}

            else
                model

        Squared -> 
            {model | display = display
                                |> String.toFloat
                                |> unsafeToFloat
                                |> flip (^) 2
                                |> toString
                            }

        
        UnderOne -> 
            {model | display = display
                                |> String.toFloat
                                |> unsafeToFloat
                                |> (/) 1
                                |> toString
                            }
        ChangeSignal ->
            {model | display = display
                                |> String.toFloat
                                |> unsafeToFloat
                                |> (*) -1
                                |> toString
                            }
        
        Comma ->
            let 
                comma     = String.contains "." display
                nocomma   = not <|  comma
                empty     = String.length display == 0
                notempty  = not empty
            in 
            if nocomma && empty then
                {model | display = display ++ "0."}
            else if nocomma && notempty then
                {model | display = display ++ "."}
            else
                model
        
        Backspace ->
            {model | display = String.dropRight 1 display}
        
        CleanEntry ->
            {model | display = ""}
        
        CleanAll -> 
            {display = "", lastNum = "", operator = None}
        
        Apply ->
            let 
                a = 
                    unsafeToFloat(String.toFloat(lastNum))
                b = 
                    unsafeToFloat(String.toFloat(display))
            in
            { model | display = 
                                applyOperator operator a b,
                                operator = None,
                                lastNum = ""
                            }

view model =
    let
        buttonStyle = 
            style[("height", "30px"), ("width", "30px")] 
    in
    div []
    [ div [][text model.lastNum]
    , div [][text model.display]
    , button [buttonStyle, onClick Percent][text "%"]
    , button [buttonStyle, onClick (RootOperation Sqrt)][text "√"]
    , button [buttonStyle, onClick Squared][text "x²"]
    , button [buttonStyle, onClick UnderOne][text "1/x"]
    , Html.br [][]
    , button [buttonStyle, onClick CleanEntry][text "CE"]
    , button [buttonStyle, onClick CleanAll][text "C"]
    , button [buttonStyle, onClick Backspace][text "BS"]
    , button [buttonStyle, onClick (BinaryOperation Divide)][text "÷"]
    , Html.br [][]
    , button [buttonStyle, onClick (Input "7")][text "7"]
    , button [buttonStyle, onClick (Input "8")][text "8"]
    , button [buttonStyle, onClick (Input "9")][text "9"]
    , button [buttonStyle, onClick (BinaryOperation Multiply)][text "*"]
    , Html.br [][]
    , button [buttonStyle, onClick (Input "4")][text "4"]
    , button [buttonStyle, onClick (Input "5")][text "5"]
    , button [buttonStyle, onClick (Input "6")][text "6"]
    , button [buttonStyle, onClick (BinaryOperation Subtract)][text "-"]
    , Html.br [][]
    , button [buttonStyle, onClick (Input "1")][text "1"]
    , button [buttonStyle, onClick (Input "2")][text "2"]
    , button [buttonStyle, onClick (Input "3")][text "3"]
    , button [buttonStyle, onClick (BinaryOperation Add)][text "+"]
    , Html.br [][]
    , button [buttonStyle, onClick ChangeSignal][text "±"]
    , button [buttonStyle, onClick (Input "0")][text "0"]
    , button [buttonStyle, onClick Comma][text ","]
    , button [buttonStyle, onClick Apply][text "="]
    ]
