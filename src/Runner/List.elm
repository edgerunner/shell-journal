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
            (String.join "\n"
                >> String.replace ".shjo" ""
                >> Runner.log
                >> (|>) Runner.done
            )
