{-
   Elm Calculator
   by: maxhero A.K.A Marcelo Amâncio de Lima Santos
-}


module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


type alias Manequin =
    { lastNum : String
    , operator : BOperator
    , display : String
    }


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


main : Program Never Manequin Msg
main =
    Html.beginnerProgram { model = { lastNum = "", operator = None, display = "" }, view = view, update = update }


convAux : Manequin -> Manequin
convAux x =
    case String.toFloat x.lastNum of
        Ok floatA ->
            case String.toFloat x.display of
                Ok floatB ->
                    { x | display = applyOperator x.operator floatA floatB }

                Err str ->
                    { x | display = str }

        Err str ->
            { x | display = str }


applyOperator : BOperator -> Float -> Float -> String
applyOperator a b c =
    case a of
        Add ->
            Basics.toString (b + c)

        Subtract ->
            Basics.toString (b - c)

        Divide ->
            if b == 0 || c == 0 then
                "Impossível Dividir por Zero"
            else
                Basics.toString (b / c)

        Multiply ->
            Basics.toString (b * c)

        Power ->
            Basics.toString (b ^ c)

        _ ->
            Debug.crash "Operador inválido: " a


applySqrt : String -> String
applySqrt x =
    case String.toFloat x of
        Ok float ->
            toString (sqrt float)

        Err str ->
            str


applyPercentage : String -> String -> String
applyPercentage x y =
    case String.toFloat x of
        Ok floatA ->
            case String.toFloat y of
                Ok floatB ->
                    toString ((floatA / 100) * floatB)

                Err str ->
                    str

        Err str ->
            str


applySquared : String -> String
applySquared x =
    case String.toFloat x of
        Ok float ->
            toString (float ^ 2)

        Err str ->
            str


applyUnderOne : String -> String
applyUnderOne x =
    case String.toFloat x of
        Ok float ->
            toString (1 / float)

        Err str ->
            str


applySignalChange : String -> String
applySignalChange x =
    case String.toFloat x of
        Ok float ->
            toString (-1 * float)

        Err str ->
            str


uncurryResult : Result e a -> Result e b -> Result e ( a, b )
uncurryResult resultA resultB =
    case resultA of
        Ok a ->
            case resultB of
                Ok b ->
                    Ok ( a, b )

                Err e ->
                    Err e

        Err e ->
            Err e


update : Msg -> Manequin -> Manequin
update msg ({ display, lastNum, operator } as model) =
    case msg of
        Input string ->
            if not <| display == "" then
                case String.toFloat display of
                    Ok float ->
                        if display == "" && string == "0" then
                            model
                        else
                            { model | display = display ++ string }

                    Err str ->
                        model
            else if display == "" && string == "0" then
                model
            else
                { model | display = display ++ string }

        BinaryOperation op ->
            { model | lastNum = display, operator = op, display = "" }

        RootOperation op ->
            { model | display = applySqrt display }

        Percent ->
            if not <| lastNum == "" then
                { model | display = applyPercentage lastNum display }
            else
                model

        Squared ->
            { model | display = applySquared display }

        UnderOne ->
            { model | display = applyUnderOne display }

        ChangeSignal ->
            if model.display == "" then
                model
            else
                { model | display = applySignalChange display }

        Comma ->
            let
                comma =
                    String.contains "." display

                nocomma =
                    not comma

                empty =
                    String.length display == 0

                notempty =
                    not empty
            in
            if nocomma && empty then
                { model | display = display ++ "0." }
            else if nocomma && notempty then
                { model | display = display ++ "." }
            else
                model

        Backspace ->
            { model | display = String.dropRight 1 display }

        CleanEntry ->
            { model | display = "" }

        CleanAll ->
            { display = "", lastNum = "", operator = None }

        Apply ->
            let
                a =
                    String.toFloat lastNum

                b =
                    String.toFloat display

                result =
                    uncurryResult a b
            in
            case result of
                Ok ( floatA, floatB ) ->
                    { display = applyOperator operator floatA floatB, operator = None, lastNum = "" }

                Err str ->
                    { model | display = str }


view : Manequin -> Html Msg
view model =
    let
        buttonStyle =
            style [ ( "height", "30px" ), ( "width", "30px" ) ]
    in
    div
        [ style
            [ ( "position", "fixed" )
            , ( "left", "40%" )
            , ( "top", "25%" )
            , ( "z-index", "-1" )
            , ( "width", "200px" )
            , ( "border", "3px solid #FF0000" )
            ]
        ]
        [ div [ style [ ( "height", "30px" ), ( "width", "120px" ) ] ] [ text model.lastNum ]
        , div [ style [ ( "height", "30px" ), ( "width", "120px" ) ] ] [ text model.display ]
        , button [ buttonStyle, onClick Percent ] [ text "%" ]
        , button [ buttonStyle, onClick (RootOperation Sqrt) ] [ text "√" ]
        , button [ buttonStyle, onClick Squared ] [ text "x²" ]
        , button [ buttonStyle, onClick UnderOne ] [ text "1/x" ]
        , Html.br [] []
        , button [ buttonStyle, onClick CleanEntry ] [ text "CE" ]
        , button [ buttonStyle, onClick CleanAll ] [ text "C" ]
        , button [ buttonStyle, onClick Backspace ] [ text "BS" ]
        , button [ buttonStyle, onClick (BinaryOperation Divide) ] [ text "÷" ]
        , Html.br [] []
        , button [ buttonStyle, onClick (Input "7") ] [ text "7" ]
        , button [ buttonStyle, onClick (Input "8") ] [ text "8" ]
        , button [ buttonStyle, onClick (Input "9") ] [ text "9" ]
        , button [ buttonStyle, onClick (BinaryOperation Multiply) ] [ text "*" ]
        , Html.br [] []
        , button [ buttonStyle, onClick (Input "4") ] [ text "4" ]
        , button [ buttonStyle, onClick (Input "5") ] [ text "5" ]
        , button [ buttonStyle, onClick (Input "6") ] [ text "6" ]
        , button [ buttonStyle, onClick (BinaryOperation Subtract) ] [ text "-" ]
        , Html.br [] []
        , button [ buttonStyle, onClick (Input "1") ] [ text "1" ]
        , button [ buttonStyle, onClick (Input "2") ] [ text "2" ]
        , button [ buttonStyle, onClick (Input "3") ] [ text "3" ]
        , button [ buttonStyle, onClick (BinaryOperation Add) ] [ text "+" ]
        , Html.br [] []
        , button [ buttonStyle, onClick ChangeSignal ] [ text "±" ]
        , button [ buttonStyle, onClick (Input "0") ] [ text "0" ]
        , button [ buttonStyle, onClick Comma ] [ text "," ]
        , button [ buttonStyle, onClick Apply ] [ text "=" ]
        ]
