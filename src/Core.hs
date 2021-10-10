{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}

module Core where

import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map

data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if noStepsFailedSoFar
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    noStepsFailedSoFar = List.all (== StepSucceeded) build.completedSteps
    nextStep = List.find (not . isStepCompleted) build.pipeline.steps
    isStepCompleted = (`Map.member` build.completedSteps) . (.name)

data Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show)

data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Image
  }
  deriving (Eq, Show)

newtype StepName = StepName Text deriving (Eq, Show, Ord)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

newtype Image = Image Text deriving (Eq, Show)

imageToText :: Image -> Text
imageToText (Image image) = image

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCodeToInt exit == 0
    then StepSucceeded
    else StepFailed exit

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildRunningState = BuildRunningState
  { step :: StepName
  }
  deriving (Eq, Show)

data BuildResult = BuildSucceeded | BuildFailed deriving (Show, Eq)

progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Right nextStep -> do
          let runningState = BuildRunningState {step = nextStep.name}
          return $ build {state = BuildRunning runningState}
        Left buildResult -> return $ build {state = BuildFinished buildResult}
    --
    BuildRunning runningState -> do
      -- Let's pretend for now that container exited with 0.
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit

      return
        build
          { state = BuildReady,
            completedSteps = Map.insert runningState.step result build.completedSteps
          }
    --
    BuildFinished _ -> return build
