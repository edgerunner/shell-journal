module Runner.List exposing (..)

import FS
import Runner exposing (Msg, Runner, Update(..))


init : ( Update, Cmd Msg )
init =
    Runner.init
        step1
        Runner.onPageList
        (FS.readdir ".shjo")


step1 : Runner
step1 =
    Runner.run
        |> Runner.handleGotPageList
            (clump
                >> String.join "\n"
                >> String.replace ".shjo" ""
                >> String.replace "-w" " week "
                >> String.replace "-q" " quarter "
                >> String.cons '\n'
                >> Runner.log
                >> (|>) Runner.done
            )


clump : List String -> List String
clump list =
    case list of
        s1 :: s2 :: s3 :: s4 :: rest ->
            ([ s1, s2, s3, s4 ] |> String.join " \t") :: clump rest

        other ->
            other |> String.join " \t" |> List.singleton
