module Main exposing (main)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


{-
   Elm Calculator
   by: maxhero A.K.A Marcelo Amâncio de Lima Santos
-}
{-
   TODO: M Buttons

-}


main =
    Html.beginnerProgram { model = { lastNum = "", operator = None, display = "" }, view = view, update = update }



{-
   unsafeToFloat
-}


unsafeToFloat : Result a Float -> Float
unsafeToFloat result =
    case result of
        Ok float ->
            float

        Err _ ->
            0.0



{-
   Applying Operations
-}


applyOperator : Operator -> Float -> Float -> String
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

        Sqrt ->
            Basics.toString (sqrt c)

        _ ->
            Debug.crash "Algo deu errado"



{-
   Defining Types
-}


type Msg
    = N1
    | N2
    | N3
    | N4
    | N5
    | N6
    | N7
    | N8
    | N9
    | N0
    | IC
    | DC
    | MP
    | DV
    | S
    | SR
    | Per
    | BS
    | C
    | CE
    | UO
    | EQ
    | CS
    | CM
    | PW


type Operator
    = Sqrt
    | Add
    | Subtract
    | Divide
    | Multiply
    | Power
    | None



{-
   Update
-}


update msg model =
    case msg of
        N0 ->
            if not <| model.display == "" then
                { model | display = model.display ++ "0" }
            else
                model

        N1 ->
            { model | display = model.display ++ "1" }

        N2 ->
            { model | display = model.display ++ "2" }

        N3 ->
            { model | display = model.display ++ "3" }

        N4 ->
            { model | display = model.display ++ "4" }

        N5 ->
            { model | display = model.display ++ "5" }

        N6 ->
            { model | display = model.display ++ "6" }

        N7 ->
            { model | display = model.display ++ "7" }

        N8 ->
            { model | display = model.display ++ "8" }

        N9 ->
            { model | display = model.display ++ "9" }

        Per ->
            if not <| model.lastNum == "" then
                if not <| model.operator == Multiply then
                    { model
                        | display =
                            toString
                                ((unsafeToFloat (String.toFloat model.lastNum) / 100.0)
                                    * unsafeToFloat (String.toFloat model.display)
                                )
                    }
                else
                    { model
                        | display =
                            toString
                                (unsafeToFloat (String.toFloat model.display) / 100.0)
                    }
            else
                model

        CM ->
            let
                ameno1 =
                    not <| String.contains "." model.display

                dorime1 =
                    String.length model.display == 0

                dorimereo1 =
                    if ameno1 then
                        { model | display = model.display ++ "." }
                    else
                        model

                skol =
                    { model | display = model.display ++ "0." }
            in
            if dorime1 && ameno1 then
                skol
            else
                dorimereo1

        S ->
            { model
                | display =
                    applyOperator
                        Power
                        2.0
                        (unsafeToFloat (String.toFloat model.display))
            }

        SR ->
            { model
                | display =
                    applyOperator
                        Sqrt
                        0.0
                        (unsafeToFloat (String.toFloat model.display))
            }

        BS ->
            { model | display = String.dropRight 1 model.display }

        C ->
            { model | lastNum = "", operator = None, display = "" }

        CE ->
            { model | display = "" }

        UO ->
            { model
                | display =
                    applyOperator
                        Divide
                        1.0
                        (unsafeToFloat (String.toFloat model.display))
            }

        EQ ->
            let
                ameno =
                    String.right 1 model.display == "."

                dorime =
                    String.right 1 model.lastNum == "."

                adapare =
                    not <| model.operator == None
            in
            if adapare then
                if ameno then
                    { model
                        | display =
                            applyOperator
                                model.operator
                                (unsafeToFloat (String.toFloat model.lastNum))
                                (unsafeToFloat (String.toFloat (model.display ++ "0")))
                        , lastNum = ""
                        , operator = None
                    }
                else if ameno && dorime then
                    { model
                        | display =
                            applyOperator
                                model.operator
                                (unsafeToFloat (String.toFloat (model.lastNum ++ "0")))
                                (unsafeToFloat (String.toFloat (model.display ++ "0")))
                        , lastNum = ""
                        , operator = None
                    }
                else if dorime then
                    { model
                        | display =
                            applyOperator
                                model.operator
                                (unsafeToFloat (String.toFloat (model.lastNum ++ "0")))
                                (unsafeToFloat (String.toFloat model.display))
                        , lastNum = ""
                        , operator = None
                    }
                else
                    { model
                        | display =
                            applyOperator
                                model.operator
                                (unsafeToFloat (String.toFloat model.lastNum))
                                (unsafeToFloat (String.toFloat model.display))
                        , lastNum = ""
                        , operator = None
                    }
            else
                model

        CS ->
            { model | display = applyOperator Multiply (unsafeToFloat (String.toFloat "-1")) (unsafeToFloat (String.toFloat model.display)), lastNum = "", operator = None }

        PW ->
            { model | lastNum = model.display, display = "", operator = Power }

        IC ->
            { model | lastNum = model.display, display = "", operator = Add }

        DC ->
            { model | lastNum = model.display, display = "", operator = Subtract }

        MP ->
            { model | lastNum = model.display, display = "", operator = Multiply }

        DV ->
            { model | lastNum = model.display, display = "", operator = Divide }



{-
   View
-}


view model =
    let
        buttonStyle =
            style [ ( "height", "30px" ), ( "width", "30px" ) ]
    in
    div []
        [ div [] [ text model.lastNum ]
        , div []
            [ text <|
                if model.display == "" then
                    "0"
                else
                    model.display
            ]
        , button [ buttonStyle, onClick Per ] [ text "%" ]
        , button [ buttonStyle, onClick SR ] [ text "√" ]
        , button [ buttonStyle, onClick S ] [ text "x²" ]
        , button [ buttonStyle, onClick UO ] [ text "1/X" ]
        , Html.br [] []
        , button [ buttonStyle, onClick CE ] [ text "CE" ]
        , button [ buttonStyle, onClick C ] [ text "C" ]
        , button [ buttonStyle, onClick BS ] [ text "BS" ]
        , button [ buttonStyle, onClick DV ] [ text "÷" ]
        , Html.br [] []
        , button [ buttonStyle, onClick N7 ] [ text "7" ]
        , button [ buttonStyle, onClick N8 ] [ text "8" ]
        , button [ buttonStyle, onClick N9 ] [ text "9" ]
        , button [ buttonStyle, onClick MP ] [ text "*" ]
        , Html.br [] []
        , button [ buttonStyle, onClick N4 ] [ text "4" ]
        , button [ buttonStyle, onClick N5 ] [ text "5" ]
        , button [ buttonStyle, onClick N6 ] [ text "6" ]
        , button [ buttonStyle, onClick DC ] [ text "-" ]
        , Html.br [] []
        , button [ buttonStyle, onClick N1 ] [ text "1" ]
        , button [ buttonStyle, onClick N2 ] [ text "2" ]
        , button [ buttonStyle, onClick N3 ] [ text "3" ]
        , button [ buttonStyle, onClick IC ] [ text "+" ]
        , Html.br [] []
        , button [ buttonStyle, onClick CS ] [ text "±" ]
        , button [ buttonStyle, onClick N0 ] [ text "0" ]
        , button [ buttonStyle, onClick CM ] [ text "," ]
        , button [ buttonStyle, onClick EQ ] [ text "=" ]
        ]
