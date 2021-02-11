module Runner.List exposing (..)

import FS
import Flags exposing (Flags)
import Runner exposing (Msg, Runner, Update(..))


init : Flags -> ( Update, Cmd Msg )
init { basePath } =
    Runner.init
        step1
        Runner.onPageList
        (FS.readdir basePath)


step1 : Runner
step1 =
    Runner.run
        |> Runner.handleGotPageList
            (List.filter (String.endsWith ".shjo")
                >> clump
                >> String.join "\n"
                >> String.replace ".shjo" ""
                >> String.cons '\n'
                >> Runner.log
                >> (|>) (Runner.done 0)
            )


clump : List String -> List String
clump list =
    case list of
        s1 :: s2 :: s3 :: s4 :: rest ->
            ([ s1, s2, s3, s4 ] |> String.join " \t") :: clump rest

        other ->
            other |> String.join " \t" |> List.singleton
