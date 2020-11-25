module Main exposing (..)
import GraphicSVG exposing(..)
import GraphicSVG.EllieApp exposing (..)
import Random
import List
import Round

main : EllieAppWithTick () Model Msg

main = ellieAppWithTick Tick { init = \ _ -> (init, randomGenerator) 
                               , update = update 
                               , view = \ model -> { title = "Fraction Game", body = view model } 
                               , subscriptions = \_ -> Sub.none }

randomGenerator = Random.generate Setup (Random.list 8 (Random.int 1 100)) 
upper_lower_generator low high = Random.generate Setup2 (Random.list 2 (Random.float low high))

--Model

type alias Model = { angularSpeed : Float, time : Float, angel : Int, 
                     numbers : List (Int), score : Int, lower : Float, upper : Float
                   , numenator : Int, denominator : Int, count_decrease : Int, count_increase : Int, blink : Bool }

init : Model
init = { angularSpeed = 0, time = 0, angel = 0, numbers = [0, 0 ,0 ,0 ,0, 0, 0, 0], score = 0, lower = 0, upper = 0
       , numenator = 0, denominator = 0, count_decrease = 0, count_increase = 0, blink = False
        }

--Update

type Msg = Tick Float GetKeyState | Increase | Decrease | Setup (List Int) | Setup2 (List Float) | Reset | Changescore

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick t _ -> 
            ( { model | time = t 
              , angel = remainderBy 360 ( model.angel + round ( model.angularSpeed * (t - model.time)))
              , numenator = choosen_numerator model
              , denominator = choosen_denominator model
              , blink = False
              } 
              , Cmd.none)

        Increase ->
            ({ model | angularSpeed = 35, count_increase = if (model.count_increase - model.count_decrease) < 1  
                                                           then model.count_increase + 1
                                                           else model.count_increase }
            , Cmd.none)

        Decrease -> 
            ({ model | angularSpeed = 0, count_decrease = if (model.count_decrease - model.count_increase < 1) 
                                                          then model.count_decrease + 1
                                                          else model.count_decrease }
            , Cmd.none)     

        Changescore -> ({ model | score = model.score + scoreChanger model.numenator model.denominator model
                        , blink = if (scoreChanger model.numenator model.denominator model == 0) then False else True  }
                        , Cmd.none)

        Setup list -> ( { model | numbers = list } 
                        , upper_lower_generator (minimumfinder list) (maximumfinder list)  )

        Setup2 list -> ({ model | lower = fromJust (index 0 (List.sort list)), upper = fromJust(index 1 (List.sort list)) }
                        , Cmd.none )

        Reset -> ( { model | angel = 0, count_decrease = 0, count_increase = 0, denominator = 0, numenator = 0, angularSpeed = 0}, randomGenerator )

scoreChanger : Int -> Int -> Model -> Int
scoreChanger n d model =  if (n == 0 || d == 0) then 0
                          else if ( (toFloat (n) / toFloat (d)) < model.upper && (toFloat(n) / toFloat(d)) > model.lower ) then 1 
                          else -1 
    
minimumfinder : List Int -> Float
minimumfinder l = toFloat (fromJust (index 0 (List.sort l))) / toFloat (fromJust (index 7 (List.sort l))) 

maximumfinder : List Int -> Float
maximumfinder l = toFloat (fromJust (index 7 (List.sort l)) ) / toFloat (fromJust (index 0 (List.sort l)))

choosen_numerator : Model -> Int
choosen_numerator model =
    if ((remainderBy 2 model.count_decrease == 1) && (remainderBy 2 model.count_increase) == 1 && not (model.angel == 0) && (model.angularSpeed == 0)) 
    then determineNumber model 
    else if ((remainderBy 2 model.count_decrease == 0) || (remainderBy 2 model.count_increase == 0)) 
    then model.numenator
    else 0

choosen_denominator : Model -> Int
choosen_denominator model = 
    if ((remainderBy 2 model.count_decrease == 0) && (remainderBy 2 model.count_increase == 0) && not (model.angel == 0) && ( model.angularSpeed == 0 ))
    then determineNumber model 
    else 0

determineNumber : Model -> Int
determineNumber model =  
    if ( model.angel > 0 && model.angel < 45 ) then fromJust( index 0 model.numbers )
    else if (model.angel > 45 && model.angel < 90) then fromJust( index 1 model.numbers )
    else if (model.angel > 90 && model.angel < 135) then fromJust( index 2 model.numbers )
    else if (model.angel > 135 && model.angel < 180) then fromJust( index 3 model.numbers )
    else if (model.angel > 180 && model.angel < 225) then fromJust( index 4 model.numbers )
    else if (model.angel > 225 && model.angel < 270) then fromJust( index 5 model.numbers )
    else if (model.angel > 270 && model.angel < 315) then fromJust( index 6  model.numbers )
    else fromJust( index 7  model.numbers )

--view 

view : Model -> Collage Msg
view model = collage 500 500 (myEnvironment model)

myEnvironment model = [ wedge 10 0.125
                     |> filled grey
                     |> scale 3.25
                 , text ( String.fromInt ( fromJust( index 0 model.numbers ) ) )
                      |> filled black 
                      |> scale 0.75
                      |> move(20, 0)
                 , wedge 10 0.125
                     |> filled blue
                     |> scale 3.25
                     |> rotate (degrees 45)
                 , text (String.fromInt ( fromJust( index 1 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (12, 15)
                 , wedge 10 0.125
                     |> filled red
                     |> scale 3.25
                     |> rotate (degrees 90)
                 , text (String.fromInt ( fromJust( index 2 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (-5, 20)
                 , wedge 10 0.125
                     |> filled yellow
                     |> scale 3.25
                     |> rotate (degrees 135)
                 , text (String.fromInt ( fromJust( index 3 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (-22, 15)   
                 , wedge 10 0.125
                     |> filled green
                     |> scale 3.25
                     |> rotate (degrees 180)
                 , text (String.fromInt ( fromJust( index 4 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (-28, -3)
                 , wedge 10 0.125
                     |> filled purple
                     |> scale 3.25
                     |> rotate (degrees 225)
                 , text (String.fromInt ( fromJust( index 5 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move ( -20, -20 ) 
                 , wedge 10 0.125
                     |> filled pink
                     |> scale 3.25
                     |> rotate (degrees 270)
                 , text (String.fromInt ( fromJust( index 6 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (-4, -23)
                 , wedge 10 0.125
                     |> filled brown
                     |> scale 3.25
                     |> rotate (degrees 315)
                 , text (String.fromInt ( fromJust( index 7 model.numbers ) ) )
                     |> filled black
                     |> scale 0.75
                     |> move (12, -20)                
                 ]
            ++
            --the arrow/hand
            [ hand |> rotate ( degrees ( toFloat (model.angel - 20) ) ) |> move (-0.5, 0) ]
            ++
            --where the inequilty shows up
            [ rect 120 15 |> outlined (solid 1) black |> move (0, -50)
            , text (Round.round 2 (model.lower) ++ " < " ++ String.fromInt (model.numenator) ++ " / " ++ String.fromInt (model.denominator) ++ " < " ++ Round.round 2 (model.upper)) 
                |> filled black 
                |> move (-50, -54)  ]
            --where score shows up
            ++
            [ scoreBoard model |> move (60, 0)]
            --Start button
            ++
            [ button_start |> move (90 , -100) |> notifyTap Increase  ]
            --Stop button
            ++
            [ button_stop |> move (-90, -100) |> notifyTap Decrease ]
            ++
            --Reset button
            [ button_reset |> move (0, -100) |> notifyTap Reset ]
            ++
            --check button
            [ button_check |> move (0, -78) |> notifyTap Changescore ]
            ++
            [ text ("The goal of this game is to construct a fraction that is within the given interval") 
                |> filled black 
                |> scale 0.35
                |> move (-160, -25)
            , text ("Move the hand onto your desired number to select your numenator") 
                |> filled black 
                |> scale 0.35
                |> move (-160, -35)
            , text ("Do this again to select your denominator then")
                |> filled black 
                |> scale 0.35
                |> move (-160, -45)
            , text ("Click the check symbol to get your score")
                |> filled black
                |> scale 0.35
                |> move (-160, -55)
            , text("Click reset to play a new round dont worry your score will reamin the same")
                |> filled black 
                |> scale 0.35
                |> move (-160, -65)
                ]

hand =  group   [ rect 1 2.5
                    |> filled (rgb 226 178 45)
                    |> makeTransparent 0
                  ,rect 10 2.5
                    |> filled (rgb 226 178 45)
                    |> move (6, 0)
                  ,triangle 10
                    |> filled red
                    |> scale 0.6
                    |> move (14 ,0)
                ]

button_start = group [ rect 50 15 |> filled green, text "start" |> filled black |> move (-20, -2) ] 

button_stop = group [ rect 50 15 |> filled red, text "stop" |> filled black |> move (-20, -2) ]

button_reset = group [ rect 50 15 |> filled lightBlue, text "Reset" |> filled black |> move (-20, -2) ]

button_check = group [ 
                    curve (-29.27,7.4700) [Pull (-28.06,8.8832) (-26.85,10.296),Pull (-26.44,10.094) (-26.04,9.8927),Pull (-20.18,5.4511) (-14.33,1.0094),Pull (-2.336,17.623) (15.141,29.678),Pull (17.564,29.274) (19.987,28.870),Pull (20.425,28.946) (19.583,28.063),Pull (-0.413,8.2967) (-13.93,-15.94),Pull (-20.96,-3.643) (-29.67,6.6624),Pull (-29.79,7.3462) (-29.27,7.4700)]
                        |> filled green
                        |> scale 0.5 ] 

scoreBoard model = group [ text "Score: " 
                        |> filled green
                        |> scale 1.5
                    , text ( if (model.blink == False) then String.fromInt (model.score) else "")
                        |> filled black
                        |> scale 1.5
                        |> move (50, 0)
                         ]

index : Int -> List ( number ) -> Maybe number
index n l = 
    let
        droppedList = List.drop n l
    in 
        List.head ( droppedList )

fromJust : Maybe number -> number
fromJust x = case x of
    Just y -> y
    Nothing -> -1
