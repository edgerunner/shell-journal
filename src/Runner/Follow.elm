module Runner.Follow exposing (init)

import Bullet
import Command.Path as Path exposing (Path)
import Flags exposing (Flags)
import Page
import Runner exposing (Msg, Runner, Update)
import Style exposing (escape)
import Utilities


type alias Context =
    { flags : Flags
    , path : Path
    , lineNumber : Int
    }


init : Flags -> Path -> Int -> ( Update, Cmd Msg )
init flags path lineNumber =
    Runner.loadPageThen flags path (step1 <| Context flags path lineNumber)


step1 : Context -> Runner
step1 context =
    let
        withTarget target =
            Runner.loadPageThen context.flags
                (Tuple.first target)
                (step2 context target)
                |> logOrigin

        withPage page =
            Page.get context.lineNumber page
                |> Maybe.map .bullet
                |> Maybe.andThen Bullet.target
                |> Maybe.map withTarget
                |> Maybe.withDefault notFound

        logOrigin =
            escape [ Style.dim ]
                ++ "  Following line #"
                ++ String.fromInt context.lineNumber
                ++ " on "
                ++ Path.toTitle context.flags context.path
                ++ escape [ Style.reset ]
                |> Runner.log

        notFound =
            Runner.logError "Can't follow that. There's no link there" (Runner.done 1)
    in
    Runner.run
        |> Runner.handlePageLoad withPage


step2 : Context -> ( Path, Int ) -> Runner
step2 { flags } ( path, lineNumber ) =
    let
        checkChain page =
            page
                |> Page.get lineNumber
                |> Maybe.map .bullet
                |> Maybe.andThen Bullet.target
                |> Maybe.map followChain
                |> Maybe.withDefault (printDestination page)

        followChain (( nextPath, _ ) as target) =
            Runner.loadPageThen
                flags
                nextPath
                (step1 (Utilities.combineWith (Context flags) target))

        printDestination =
            Page.highlight lineNumber
                >> Result.map
                    (Page.clip 2
                        >> Runner.putPage flags path
                        >> Runner.doneWith
                    )
                >> Result.withDefault (Runner.done 1)
    in
    Runner.run
        |> Runner.handlePageLoad checkChain
