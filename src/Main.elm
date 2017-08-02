module Main exposing (main)

import Color as C
import Element as E exposing (Element)
import Element.Attributes as A exposing (px)
import Html exposing (Html)
import String.Extra as String
import Style as S exposing (StyleSheet)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL --


type alias Model =
    { ourLibrary : List Card
    , ourHand : List Card
    , ourBattlefield : List ( Bool, Card )
    , theirBattlefield : List ( Bool, Card )
    , activePlayer : Player
    , priorityPlayer : Maybe Player
    }


init : ( Model, Cmd Msg )
init =
    ( { ourLibrary =
            [ kozileksSentinel
            , silentSkimmer
            , plummet
            , volcanicUpheaval
            , fortifiedRampart
            , stoneHavenMedic
            , vestigeOfEmrakul
            , courierGriffin
            ]
      , ourHand = [ plummet ]
      , ourBattlefield = []
      , theirBattlefield = []
      , activePlayer = Us
      , priorityPlayer = Nothing
      }
    , Cmd.none
    )



-- CARDS --


kozileksSentinel : Card
kozileksSentinel =
    { name = "Kozilek's Sentinel"
    , manaCosts = [ ( 1, Colorless ), ( 1, Red ) ]
    , abilities =
        [ Devoid
        , Whenever (CastSpell Colorless Us)
            [ ModifyPowerToughness 1 0 Self UntilEndOfTurn ]
        ]
    , types =
        [ Creature
            { power = 1
            , toughness = 4
            , creatureTypes = [ EldraziDrone ]
            }
        ]
    }


silentSkimmer : Card
silentSkimmer =
    { name = "Silent Skimmer"
    , manaCosts = [ ( 3, Colorless ), ( 1, Black ) ]
    , abilities =
        [ Devoid
        , Flying
        , Whenever (Attacks Self) [ LoseLife 2 DefendingPlayer ]
        ]
    , types =
        [ Creature
            { power = 0
            , toughness = 4
            , creatureTypes = [ EldraziDrone ]
            }
        ]
    }


fortifiedRampart : Card
fortifiedRampart =
    { name = "Fortified Rampart"
    , manaCosts = [ ( 1, Colorless ), ( 1, White ) ]
    , abilities = [ Defender ]
    , types =
        [ Creature
            { power = 0
            , toughness = 6
            , creatureTypes = [ Wall ]
            }
        ]
    }


stoneHavenMedic : Card
stoneHavenMedic =
    { name = "Stone Haven Medic"
    , manaCosts = [ ( 1, Colorless ), ( 1, White ) ]
    , abilities =
        [ Activated [ ManaCost [ ( 1, Colorless ) ], Tap ] [ GainLife 1 You ] ]
    , types =
        [ Creature
            { power = 1
            , toughness = 3
            , creatureTypes = [ KorCleric ]
            }
        ]
    }


vestigeOfEmrakul : Card
vestigeOfEmrakul =
    { name = "Vestige of Emrakul"
    , manaCosts = [ ( 3, Colorless ), ( 1, Red ) ]
    , abilities =
        [ Devoid
        , Trample
        ]
    , types =
        [ Creature
            { power = 3
            , toughness = 4
            , creatureTypes = [ EldraziDrone ]
            }
        ]
    }


courierGriffin : Card
courierGriffin =
    { name = "Courier Griffin"
    , manaCosts = [ ( 3, Colorless ), ( 1, White ) ]
    , abilities =
        [ Flying
        , Whenever (EntersTheBattlefield Self) [ GainLife 2 You ]
        ]
    , types =
        [ Creature
            { power = 2
            , toughness = 3
            , creatureTypes = [ Griffin ]
            }
        ]
    }


plummet : Card
plummet =
    { name = "Plummet"
    , manaCosts = [ ( 1, Colorless ), ( 1, Green ) ]
    , abilities = []
    , types =
        [ Instant
            { effects = [ Destroy (TargetCreature [ Flying ]) ] }
        ]
    }


volcanicUpheaval : Card
volcanicUpheaval =
    { name = "Volcanic Upheaval"
    , manaCosts = [ ( 3, Colorless ), ( 1, Red ) ]
    , abilities = []
    , types =
        [ Instant
            { effects = [ Destroy TargetLand ] }
        ]
    }


type alias Card =
    { name : String
    , manaCosts : List ( Int, Mana )
    , types : List Type
    , abilities : List Ability
    }


type Type
    = Creature
        { power : Int
        , toughness : Int
        , creatureTypes : List CreatureType
        }
    | Instant
        { effects : List Effect
        }


writeType : Type -> String
writeType type_ =
    case type_ of
        Creature _ ->
            "Creature"

        Instant _ ->
            "Instant"


type CreatureType
    = EldraziDrone
    | Griffin
    | KorCleric
    | Wall


writeCreatureType : CreatureType -> String
writeCreatureType creatureType =
    case creatureType of
        EldraziDrone ->
            "Eldrazi Drone"

        Griffin ->
            "Griffin"

        KorCleric ->
            "Kor Cleric"

        Wall ->
            "Wall"


type Player
    = Us
    | Them


writePlayer : Player -> String
writePlayer player =
    case player of
        Us ->
            "you"

        Them ->
            "the other player"


type Mana
    = White
    | Blue
    | Black
    | Red
    | Green
    | Colorless


writeMana : Mana -> String
writeMana mana =
    case mana of
        White ->
            "white"

        Blue ->
            "blue"

        Black ->
            "black"

        Red ->
            "red"

        Green ->
            "green"

        Colorless ->
            "colorless"


type Ability
    = Devoid
    | Flying
    | Defender
    | Trample
    | Whenever Condition (List Effect)
    | Activated (List Cost) (List Effect)


writeAbility : String -> Ability -> String
writeAbility cardName ability =
    case ability of
        Devoid ->
            "devoid"

        Flying ->
            "flying"

        Defender ->
            "defender"

        Trample ->
            "trample"

        Whenever condition effects ->
            "whenever "
                ++ writeCondition cardName condition
                ++ ", "
                ++ writeEffects cardName effects

        Activated costs effects ->
            writeCosts costs
                ++ ": "
                ++ writeEffects cardName effects


type Condition
    = Attacks Target
    | CastSpell Mana Player
    | EntersTheBattlefield Target


writeCondition : String -> Condition -> String
writeCondition cardName condition =
    case condition of
        Attacks target ->
            writeTarget cardName target ++ " attacks"

        CastSpell mana player ->
            writePlayer player
                ++ " cast a "
                ++ writeMana mana
                ++ " spell"

        EntersTheBattlefield target ->
            writeTarget cardName target ++ " enters the battlefield"


type Cost
    = ManaCost (List ( Int, Mana ))
    | Tap


writeCosts : List Cost -> String
writeCosts costs =
    -- TODO: we need to create E.el instead
    costs
        |> List.map writeCost
        |> String.join ", "


writeCost : Cost -> String
writeCost cost =
    case cost of
        ManaCost mana ->
            -- TODO
            "some mana"

        Tap ->
            "tap"


type Effect
    = Destroy Target
    | LoseLife Int Target
    | GainLife Int Target
    | ModifyPowerToughness Int Int Target Duration


writeEffects : String -> List Effect -> String
writeEffects cardName effects =
    effects
        |> List.map (writeEffect cardName)
        |> String.join " and "


writeEffect : String -> Effect -> String
writeEffect cardName effect =
    case effect of
        Destroy target ->
            "destroy " ++ writeTarget cardName target

        LoseLife amount target ->
            writeTarget cardName target
                ++ " loses "
                ++ toString amount
                ++ " life"

        GainLife amount target ->
            writeTarget cardName target
                ++ " gain "
                ++ toString amount
                ++ " life"

        ModifyPowerToughness power toughness target duration ->
            writeTarget cardName target
                ++ " gets "
                ++ toString power
                ++ "/"
                ++ toString toughness
                ++ " "
                ++ writeDuration duration


type Target
    = Self
    | TargetCreature (List Ability)
    | TargetLand
    | DefendingPlayer
    | You


writeTarget : String -> Target -> String
writeTarget cardName target =
    case target of
        Self ->
            cardName

        TargetCreature abilities ->
            "target creature with "
                ++ (abilities
                        |> List.map (writeAbility cardName)
                        |> String.join " and "
                   )

        TargetLand ->
            "target land"

        DefendingPlayer ->
            "defending player"

        You ->
            "you"


type Duration
    = UntilEndOfTurn


writeDuration : Duration -> String
writeDuration duration =
    case duration of
        UntilEndOfTurn ->
            "until end of turn"



--- UPDATE ---


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



--- SUBSCRIPTIONS ---


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--- VIEW ---


view : Model -> Html Msg
view model =
    E.layout stylesheet <|
        E.column NoStyle
            [ A.padding 10
            , A.spacing 10
            ]
            [ viewCards "Our Library" model.ourLibrary
            , viewCards "Our Hand" model.ourHand
            ]


viewCards : String -> List Card -> Element Style variation Msg
viewCards title cards =
    E.column NoStyle
        [ A.padding 2
        , A.spacing 6
        ]
        [ E.text title
        , cards
            |> List.map viewCard
            |> E.wrappedRow NoStyle
                [ A.padding 10
                , A.spacing 10
                ]
        ]


viewCard : Card -> Element Style variation Msg
viewCard card =
    let
        header =
            E.row NoStyle
                [ A.padding 4 ]
                [ E.el NoStyle
                    [ A.verticalCenter
                    , A.alignLeft
                    ]
                  <|
                    E.text card.name
                , E.el NoStyle
                    [ A.alignRight
                    , A.verticalCenter
                    ]
                    (card.manaCosts
                        |> List.map viewManaCost
                        |> E.row NoStyle
                            [ A.padding 2
                            , A.spacing 4
                            ]
                    )
                ]

        subHeader =
            E.el NoStyle
                [ A.padding 4 ]
                ((card.types
                    |> List.map writeType
                    |> String.join " "
                 )
                    ++ (card.types
                            |> List.map writeSubTypes
                            |> String.join " "
                       )
                    |> E.text
                )

        writeSubTypes type_ =
            case type_ of
                Creature info ->
                    " - "
                        ++ (info.creatureTypes
                                |> List.map writeCreatureType
                                |> String.join " "
                           )

                _ ->
                    ""
    in
    E.column CardStyle
        [ A.padding 4
        , A.width (px 230)
        , A.height (px 250)
        ]
        [ header
        , E.hairline Hairline
        , subHeader
        , E.hairline Hairline
        , viewAbilities card.name card.abilities
        , E.el NoStyle [ A.alignBottom ] <|
            viewTypes card.name card.types
        ]


viewManaCost : ( Int, Mana ) -> Element Style variation Msg
viewManaCost ( amount, mana ) =
    E.circle 9 (ManaStyle mana) [] <|
        E.el ManaNumber
            [ A.center
            , A.verticalCenter
            ]
        <|
            E.text (toString amount)


viewAbilities : String -> List Ability -> Element Style variation Msg
viewAbilities cardName abilities =
    abilities
        |> List.map (viewAbility cardName)
        |> E.column NoStyle
            [ A.padding 4
            , A.spacing 5
            ]


viewAbility : String -> Ability -> Element Style variation Msg
viewAbility cardName ability =
    writeAbility cardName ability
        |> toSentence
        |> E.text


viewTypes : String -> List Type -> Element Style variation Msg
viewTypes cardName types =
    types
        |> List.map (viewType cardName)
        |> E.column NoStyle []


viewType : String -> Type -> Element Style variation Msg
viewType cardName type_ =
    case type_ of
        Creature { power, toughness } ->
            E.text (toString power ++ "/" ++ toString toughness)

        Instant { effects } ->
            effects
                |> List.map (viewEffect cardName)
                |> E.column NoStyle []


viewEffect : String -> Effect -> Element Style variation Msg
viewEffect cardName effect =
    writeEffect cardName effect
        |> toSentence
        |> E.text


toSentence : String -> String
toSentence text =
    if (String.split " " text |> List.length) > 1 then
        String.toSentenceCase text ++ "."
    else
        String.toSentenceCase text



--- STYLESHEET ---


type Style
    = NoStyle
    | CardStyle
    | Hairline
    | ManaStyle Mana
    | ManaNumber


stylesheet : StyleSheet Style variation
stylesheet =
    S.stylesheet
        [ S.style CardStyle
            [ Border.all 1
            , Color.background C.lightGrey
            ]
        , S.style Hairline
            [ Color.background C.black
            ]
        , S.style (ManaStyle White)
            [ Color.background C.white
            ]
        , S.style (ManaStyle Blue)
            [ Color.background C.blue
            ]
        , S.style (ManaStyle Black)
            [ Color.background C.black
            ]
        , S.style (ManaStyle Red)
            [ Color.background C.red
            ]
        , S.style (ManaStyle Green)
            [ Color.background C.green ]
        , S.style (ManaStyle Colorless)
            [ Color.background C.grey ]
        , S.style ManaNumber
            [ Color.text C.lightGrey
            , Font.typeface [ "sans-serif" ]
            , Font.bold
            ]
        ]
