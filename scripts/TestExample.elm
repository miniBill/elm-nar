module TestExample exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Bytes
import Bytes.Decoder
import Bytes.Encode
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser exposing (OptionsParser)
import Cli.Program as Program
import FatalError exposing (FatalError)
import Nix.Archive
import Pages.Script as Script exposing (Script)
import TestRoundtrip
import XBytes


run : Script
run =
    Script.withCliOptions config toTask


type alias CliOptions =
    { path : String }


config : Program.Config CliOptions
config =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with (Option.requiredPositionalArg "path")
                |> OptionsParser.end
            )


toTask : CliOptions -> BackendTask FatalError ()
toTask { path } =
    File.binaryFile path
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\file ->
                case Bytes.Decoder.decode Nix.Archive.decoder file of
                    Ok nar ->
                        nar |> TestRoundtrip.toJson |> Script.log

                    Err e ->
                        BackendTask.fail (FatalError.fromString (errorToString e))
            )


errorToString : Bytes.Decoder.Error Nix.Archive.DecodeContext Nix.Archive.DecodeError -> String
errorToString err =
    errorToStringHelper 0 err


errorToStringHelper : Int -> Bytes.Decoder.Error Nix.Archive.DecodeContext Nix.Archive.DecodeError -> String
errorToStringHelper indentation err =
    case err of
        Bytes.Decoder.InContext { label, start } child ->
            atString start ++ String.repeat indentation " " ++ "In context: " ++ Debug.toString label ++ "\n" ++ errorToStringHelper (indentation + 2) child

        Bytes.Decoder.OutOfBounds { at } ->
            atString at ++ String.repeat indentation " " ++ Debug.toString err

        Bytes.Decoder.Custom { at } _ ->
            atString at ++ String.repeat indentation " " ++ Debug.toString err

        Bytes.Decoder.BadOneOf { at } _ ->
            atString at ++ String.repeat indentation " " ++ Debug.toString err


atString : Int -> String
atString pos =
    "@ " ++ XBytes.toHex (XBytes.fromBytes (Bytes.Encode.encode (Bytes.Encode.unsignedInt32 Bytes.BE pos))) ++ " "
