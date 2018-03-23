{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Bank where

import           Miso
import           Miso.String (MisoString, ms, (<>))
import qualified Data.Map as Map (fromList)
import           Data.List.Extra

import           Model

run :: IO ()
run = startApp App {..}
  where
    initialAction = NoOp
    model         = initExperiment
    update        = updateExperiment
    view          = viewExperiment
    events        = defaultEvents
    subs          = []
    mountPoint    = Nothing

updateExperiment :: Action -> Experiment -> Effect Action Experiment
updateExperiment NoOp ex = noEff ex
updateExperiment (ChangeParameter ClerksNum action) ex = noEff ex
  { parameters = (parameters ex)
      { clerksNum = case action of
          Sub -> clerksNum (parameters ex) - 1
          Add -> clerksNum (parameters ex) + 1
      }
  , bank = case action of
      Sub -> delClerk (bank ex)
      Add -> addClerk (bank ex)
  }
updateExperiment (ChangeParameter field action) ex = noEff ex
  { parameters = case field of
      TimeStep       -> (parameters ex)
        { timeStep = case action of
            Sub -> timeStep (parameters ex) - 10
            Add -> timeStep (parameters ex) + 10
        }
      ClerksNum      -> (parameters ex)
        { clerksNum = case action of
            Sub -> clerksNum (parameters ex) - 1
            Add -> clerksNum (parameters ex) + 1
        }
      QueueLenLimit  -> (parameters ex)
        { queueLenLimit = case action of
            Sub -> queueLenLimit (parameters ex) - 5
            Add -> queueLenLimit (parameters ex) + 5
        }
      ServiceMinTime -> (parameters ex)
        { serviceMinTime = case action of
            Sub -> serviceMinTime (parameters ex) - 4
            Add -> serviceMinTime (parameters ex) + 4
        }
      ServiceMaxTime -> (parameters ex)
        { serviceMaxTime = case action of
            Sub -> serviceMaxTime (parameters ex) - 4
            Add -> serviceMaxTime (parameters ex) + 4
        }
  }
updateExperiment StartExperiment ex = noEff ex
  { isStarted = True
  }
updateExperiment RestartExperiment _ = noEff initExperiment

showMS :: Show a => a -> MisoString
showMS = ms . show

showMSTime :: Minutes -> MisoString
showMSTime m = ms $ hours ++ " : " ++ minutes
  where
    hours   = takeEnd 2 $ '0' : show (m `div` hour)
    minutes = takeEnd 2 $ '0' : show (m `mod` hour)

showMSDay :: Day -> MisoString
showMSDay d = case d of
  Monday    -> "Понедельник"
  Tuesday   -> "Вторник"
  Wednesday -> "Среда"
  Thursday  -> "Четверг"
  Friday    -> "Пятница"
  Saturday  -> "Суббота"
  Sunday    -> "Воскресенье"

showMSMoney :: Money -> MisoString
showMSMoney m = showMS (m * 1000) <> " руб."

showMSInterval :: Show a => (a,a) -> MisoString
showMSInterval (x,y) = showMS x <> " - " <> showMS y

viewExperiment :: Experiment -> View Action
viewExperiment ex =
  div_
  [ lineBlockStyle ]
  [ div_
    [ dataStyle ]
    [ parametersView
    , statisticView
    ]
  , bankView
  ]
  where

    bankView =
      div_
      [ activeStyle
      , bankStyle ]
      [ clerksView
      , div_
        [ lineBlockStyle ]
        [ queueView
        , inOutStatsView
        ]
      , tableView
      ]

    clerksView =
      div_
      [ clerksBlockStyle
      , lineBlockStyle ]
      (clerkToDiv_ <$> clerks (bank ex))

    queueView =
      div_
      [ queueStyle
      , centerStyle ]
      (clientToDiv_ <$> queue (bank ex))

    inOutStatsView =
      div_
      [ inOutStyle
      , centerStyle ]
      [ div_
        [ lineBlockStyle
        , centerStyle ]
        [ div_
          [ labelStyle ]
          [ text "Пришло:" ]
        , div_
          []
          [ text $ ms $ show $ clientsCome (statistic ex) ]
        ]
      , div_
        [ lineBlockStyle
        , centerStyle ]
        [ div_
          [ labelStyle ]
          [ text "Ушло:" ]
        , div_
          []
          [ text $ ms $ show $ clientsLeft (statistic ex) ]
        ]
      ]

    tableView =
      div_
      [ centerStyle
      , tableStyle ]
      (tableLineToDiv_ <$> tableLines (infoTable (bank ex)))

    parametersView =
      div_
      []
      [ clerkParameterView
      , timeStepParameterView
      , maxQueueLenParameterView
      , serviceTimeParameterView
      , parametersButtonsView
      ]

    statisticView =
      div_
      []
      [ currentTimeView
      , spentDaysView
      , currentDayView
      , servicedClientsNumView
      , leftClientsNumView
      , minQueueLenView
      , medQueueLenView
      , maxQueueLenView
      , medClientWaitingTimeView
      , medClerksWorkTimeView
      , bankProfitView
      ]

    currentTimeView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Текущее время: " ]
      , div_
        []
        [ text $ showMSTime (currentTime $ statistic ex) ]
      ]

    spentDaysView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Дней прошло: " ]
      , div_
        []
        [ text $ showMS (spentDays $ statistic ex) ]
      ]

    currentDayView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Текущий день недели: " ]
      , div_
        []
        [ text $ showMSDay (currentDay $ statistic ex) ]
      ]

    servicedClientsNumView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Число обслужанных клиентов: " ]
      , div_
        []
        [ text $ showMS (servicedClientsNum $ statistic ex) ]
      ]

    leftClientsNumView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Число потерянных клиентов: " ]
      , div_
        []
        [ text $ showMS (leftClientsNum $ statistic ex) ]
      ]

    minQueueLenView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Минимальная длина очереди: " ]
      , div_
        []
        [ text $ showMS (minQueueLen $ statistic ex) ]
      ]

    medQueueLenView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Средняя длина очереди: " ]
      , div_
        []
        [ text $ showMS (medQueueLen $ statistic ex) ]
      ]

    maxQueueLenView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Максимальная длина очереди: " ]
      , div_
        []
        [ text $ showMS (maxQueueLen $ statistic ex) ]
      ]

    medClientWaitingTimeView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Среднее время ожидания клиентов: " ]
      , div_
        []
        [ text $ showMS (medClientWaitingTime $ statistic ex) <> " мин." ]
      ]

    medClerksWorkTimeView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Среднее рабочее время клерков: " ]
      , div_
        []
        [ text $ showMS (medClerksWorkTime $ statistic ex) <> " мин." ]
      ]

    bankProfitView =
      div_
      [ lineBlockStyle ]
      [ div_
        [ labelStyle
        , textLineStyle ]
        [ text "Прибыль банка: " ]
      , div_
        []
        [ text $ showMSMoney (bankProfit $ statistic ex) ]
      ]

    timeStepParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Шаг моделирования (" <> showMSInterval defaultTimeSteps <> "):" ]
      , div_
        [ lineBlockStyle ]
        [ button_
          [ timeStepParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter TimeStep Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ (showMS $ timeStep (parameters ex)) <> " мин." ]
        , button_
          [ timeStepParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter TimeStep Add ]
          [ "+" ]
        ]
      ]

    maxQueueLenParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Максимальный размер очереди (" <> showMSInterval defaultQueueLenLimits <> "):" ]
      , div_
        [ lineBlockStyle ]
        [ button_
          [ maxQueueLenParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter QueueLenLimit Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ showMS $ queueLenLimit (parameters ex) ]
        , button_
          [ maxQueueLenParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter QueueLenLimit Add ]
          [ "+" ]
        ]
      ]

    serviceTimeParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Время обслуживания (" <> showMSInterval defaultServiceTimes <> "):" ]
      , div_
        []
        [ div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "От:" ]
          , button_
            [ serviceMinTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ServiceMinTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ serviceMinTime (parameters ex)) <> " мин." ]
          , button_
            [ serviceMinTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ServiceMinTime Add ]
            [ "+" ]
          ]
        , div_
          [ lineBlockStyle ]
          [ div_
            [ labelStyle ]
            [ text "До:" ]
          , button_
            [ serviceMaxTimeParameterSubStyle
            , subButtonStatus
            , onClick $ ChangeParameter ServiceMaxTime Sub ]
            [ "-" ]
          , div_
            [ parameterValView ]
            [ text $ (showMS $ serviceMaxTime (parameters ex)) <> " мин." ]
          , button_
            [ serviceMaxTimeParameterAddStyle
            , addButtonStatus
            , onClick $ ChangeParameter ServiceMaxTime Add ]
            [ "+" ]
          ]
        ]
      ]

    clerkParameterView =
      div_
      [ blockStyle ]
      [ div_
        [ textLineStyle ]
        [ text $ "Число клерков (" <> showMSInterval defaultClerksNums <> "):" ]
      , div_
        [ lineBlockStyle ]
        [ button_
          [ clerkParameterSubStyle
          , subButtonStatus
          , onClick $ ChangeParameter ClerksNum Sub ]
          [ "-" ]
        , div_
          [ parameterValView ]
          [ text $ showMS $ clerksNum (parameters ex) ]
        , button_
          [ clerkParameterAddStyle
          , addButtonStatus
          , onClick $ ChangeParameter ClerksNum Add ]
          [ "+" ]
        ]
      ]

    parametersButtonsView =
      div_
      [ blockStyle
      , lineBlockStyle ]
      [ startButton
      , nextStepButton
      , goToEndButton
      , restartButton
      ]

    startButton =
      button_
      [ startButtonStatus
      , buttonStyle
      , onClick StartExperiment ]
      [ "Начать" ]

    nextStepButton =
      button_
      [ nextStepButtonStatus
      , buttonStyle ]
      [ "Сделать шаг" ]

    goToEndButton =
      button_
      [ goToEndButtonStatus
      , buttonStyle ]
      [ "Перейти в конец" ]

    restartButton =
      button_
      [ buttonStyle
      , onClick RestartExperiment ]
      [ "Перезагрузить" ]

-- =================================================================
-- |                        Elements styles                        |
-- =================================================================

    centerStyle = style_ $ Map.fromList
      [ ("margin-left",  "auto")
      , ("margin-right", "auto") ]

    dataStyle = style_ $ Map.fromList
      [ ("width", "420px") ]

    textLineStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px") ]

    clerksBlockStyle = style_ $ Map.fromList
      [ ("margin-bottom",   "50px")
      , ("justify-content", "space-around") ]

    queueStyle = style_ $ Map.fromList
      [ ("display",          "flex")
      , ("background-color", "linen")
      , ("height",           "200px")
      , ("border-style",     "solid")
      , ("border-color",     "gray")
      , ("border-radius",    "2px") 
      , ("border-width",     "2px")
      , ("flex-wrap",        "wrap")
      , ("justify-content",  "space-around")
      , ("align-items",      "center")
      , ("width",            "700px") ]

    inOutStyle = style_ $ Map.fromList
      [ ("display",         "flex")
      , ("border-style",    "solid")
      , ("border-color",    "gray")
      , ("border-radius",   "2px") 
      , ("border-width",    "2px")
      , ("height",          "200px")
      , ("width",           "150px")
      , ("flex-direction",  "column")
      , ("justify-content", "space-around") ]

    tableStyle = style_ $ Map.fromList
      [ ("display",          "flex")
      , ("background-color", "lightgoldenrodyellow")
      , ("padding-top",      "10px")
      , ("border-style",     "solid")
      , ("border-color",     "gray")
      , ("border-radius",    "2px") 
      , ("border-width",     "2px")
      , ("margin-top",       "20px")
      , ("height",           "200px")
      , ("width",            "170px")
      , ("flex-direction",   "column")
      , ("justify-content",  "space-around") ]

    buttonStyle = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    activeStyle
      | isStarted ex = style_ $ Map.fromList [ ("opacity", "1") ]
      | otherwise    = style_ $ Map.fromList [ ("opacity", "0.3") ]

    bankStyle = style_ $ Map.fromList
      [ ("width", "900px") ]

    startButtonStatus
      | isStarted ex = disabled_ True
      | otherwise    = disabled_ False

    nextStepButtonStatus
      | isStarted ex = disabled_ False
      | otherwise    = disabled_ True

    goToEndButtonStatus
      | isStarted ex = disabled_ False
      | otherwise    = disabled_ True

    blockStyle = style_ $ Map.fromList
      [ ("margin-bottom", "20px") ]

    parameterValView = style_ $ Map.fromList
      [ ("margin-top", "2px") ]

    subButtonStatus = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    addButtonStatus = style_ $ Map.fromList
      [ ("margin-left", "10px") ]

    labelStyle = style_ $ Map.fromList
      [ ("margin-right", "10px") ]

    clerkParameterSubStyle
      | clerksNum (parameters ex) == fst defaultClerksNums
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    clerkParameterAddStyle
      | clerksNum (parameters ex) == snd defaultClerksNums
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    timeStepParameterSubStyle
      | timeStep (parameters ex) == fst defaultTimeSteps = disabled_ True
      | otherwise                                        = disabled_ False

    timeStepParameterAddStyle
      | timeStep (parameters ex) == snd defaultTimeSteps = disabled_ True
      | otherwise                                        = disabled_ False

    maxQueueLenParameterSubStyle
      | queueLenLimit (parameters ex) == fst defaultQueueLenLimits
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    maxQueueLenParameterAddStyle
      | queueLenLimit (parameters ex) == snd defaultQueueLenLimits
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    serviceMinTimeParameterSubStyle
      | serviceMinTime (parameters ex) == fst defaultServiceTimes
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    serviceMinTimeParameterAddStyle
      | serviceMinTime (parameters ex) == snd defaultServiceTimes
        || serviceMinTime (parameters ex) >= serviceMaxTime (parameters ex)
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    serviceMaxTimeParameterSubStyle
      | serviceMaxTime (parameters ex) == fst defaultServiceTimes
        || serviceMaxTime (parameters ex) <= serviceMinTime (parameters ex)
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    serviceMaxTimeParameterAddStyle
      | serviceMaxTime (parameters ex) == snd defaultServiceTimes
        || isStarted ex = disabled_ True
      | otherwise       = disabled_ False

    lineBlockStyle = style_ $ Map.fromList [ ("display", "flex") ]

clerkToDiv_ :: Clerk -> View Action
clerkToDiv_ clerk =
  div_
  []
  [ div_
    [ nameStyle ]
    [ text $ ms $ name clerk ]
  , div_
    [ clerkBodyStyle
    , clerkBodyColor ]
    []
  , clientPlaceView
  ]
  where

    clientPlaceView =
      div_
      [ clientPlaceStyle ]
      [ div_
        [ numberStyle ]
        [ text servicedClientLabel ]
      , div_
        [ servicedClientBodyStyle
        , servicedClientBodyVisibility ]
        []
      ]

    servicedClientLabel = case work clerk of
      Nothing -> "___"
      Just cl -> ms $ takeEnd 3 $ "00" ++ show (number cl)

    servicedClientBodyStyle = style_ $ Map.fromList
      [ ("width",            "30px")
      , ("height",           "30px")
      , ("background-color", "lightblue") ]

    servicedClientBodyVisibility
      | work clerk == Nothing =
        style_ $ Map.fromList [ ("visibility", "hidden") ]
      | otherwise = style_ $ Map.fromList [ ("visibility", "visible") ]

    nameStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("margin-left",   "3px") ]

    numberStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("text-align",    "center") ]

    clerkBodyStyle = style_ $ Map.fromList
      [ ("width",         "30px")
      , ("height",        "30px")
      , ("margin-left",   "12px")
      , ("margin-top",    "10px")
      , ("margin-bottom", "20px") ]

    clerkBodyColor
      | work clerk == Nothing = style_ $ Map.fromList
        [ ("background-color", "darkseagreen ") ]
      | otherwise             = style_ $ Map.fromList
        [ ("background-color", "lightcoral") ]

    clientPlaceStyle = style_ $ Map.fromList
      [ ("border-style",  "solid")
      , ("border-color",  "gray")
      , ("border-radius", "2px") 
      , ("border-width",  "2px")
      , ("padding",       "10px") ]

clientToDiv_ :: Client -> View Action
clientToDiv_ client =
  div_
  []
  [ div_
    [ numberStyle ]
    [ text clientLabel ]
  , div_
    [ clientBodyStyle ]
    []
  ]
  where

    numberStyle = style_ $ Map.fromList
      [ ("margin-bottom", "5px")
      , ("text-align",    "center") ]

    clientBodyStyle = style_ $ Map.fromList
      [ ("width",            "30px")
      , ("height",           "30px")
      , ("margin-left",      "20px")
      , ("margin-right",     "20px")
      , ("margin-bottom",    "10px")
      , ("background-color", "lightblue") ]

    clientLabel = ms $ takeEnd 3 $ "00" ++ show (number client)

tableLineToDiv_ :: TableLine -> View Action
tableLineToDiv_ tl =
  div_
  [ tableLineStyle
  , tableLineStatusColor ]
  [ div_
    [ tableClerkLabel ]
    [ text $ ms $ tableClerk tl ++ ":" ]
  , div_
    []
    [ text tableRequest ]
  ]
  where

    tableRequest = case tableClient tl of
      Nothing  -> "свободен"
      Just num -> ms $ takeEnd 3 $ "00" ++ show num

    tableLineStyle = style_ $ Map.fromList
      [ ("display",       "flex")
      , ("margin-left",   "20px")
      , ("margin-bottom", "10px")
      , ("font-weight",   "bold") ]

    tableLineStatusColor = case tableClient tl of
      Nothing -> style_ $ Map.fromList [ ("color", "darkseagreen") ]
      Just _  -> style_ $ Map.fromList [ ("color", "lightcoral") ]

    tableClerkLabel = style_ $ Map.fromList
      [ ("margin-right", "10px") ]
