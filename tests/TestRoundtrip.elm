module TestRoundtrip exposing (suite, toJson)

import Bytes
import Bytes.Decoder
import Bytes.Encode
import Expect
import Fuzz exposing (Fuzzer)
import Json.Encode
import Nix.Archive
import Test exposing (Test)


suite : Test
suite =
    Test.fuzz (narFuzzer 3) "NAR roundtrips" <|
        \nar ->
            case
                nar
                    |> Nix.Archive.encode
                    |> Bytes.Encode.encode
                    |> Bytes.Decoder.decode Nix.Archive.decoder
            of
                Ok decoded ->
                    decoded
                        |> toJson
                        |> Expect.equal (toJson nar)

                Err e ->
                    Expect.fail ("Failed to decode: " ++ Debug.toString e)


narFuzzer : Int -> Fuzzer Nix.Archive.Nar
narFuzzer budget =
    narObjectFuzzer budget


narObjectFuzzer : Int -> Fuzzer Nix.Archive.NarObject
narObjectFuzzer budget =
    if budget <= 0 then
        Fuzz.oneOf
            [ Fuzz.map Nix.Archive.RegularObject regularFuzzer
            , Fuzz.map Nix.Archive.SymlinkObject symlinkFuzzer
            , Fuzz.map Nix.Archive.DirectoryObject (directoryFuzzer (budget - 1))
            ]

    else
        Fuzz.oneOf
            [ Fuzz.map Nix.Archive.RegularObject regularFuzzer
            , Fuzz.map Nix.Archive.SymlinkObject symlinkFuzzer
            ]


directoryFuzzer : Int -> Fuzzer (List Nix.Archive.DirectoryEntry)
directoryFuzzer budget =
    Fuzz.list (directoryEntryFuzzer budget)


regularFuzzer : Fuzzer Nix.Archive.Regular
regularFuzzer =
    Fuzz.map2 Nix.Archive.Regular Fuzz.bool bytesFuzzer


bytesFuzzer : Fuzzer Bytes.Bytes
bytesFuzzer =
    Fuzz.map Bytes.Encode.unsignedInt8 (Fuzz.intRange 0 0xFF)
        |> Fuzz.list
        |> Fuzz.map (\l -> l |> Bytes.Encode.sequence |> Bytes.Encode.encode)


symlinkFuzzer : Fuzzer Nix.Archive.Symlink
symlinkFuzzer =
    Fuzz.map Nix.Archive.Symlink Fuzz.string


directoryEntryFuzzer : Int -> Fuzzer Nix.Archive.DirectoryEntry
directoryEntryFuzzer budget =
    Fuzz.map2 Nix.Archive.DirectoryEntry
        Fuzz.string
        (narObjectFuzzer budget)


toJson : Nix.Archive.Nar -> String
toJson nar =
    encodeNar nar |> Json.Encode.encode 2


encodeNar : Nix.Archive.Nar -> Json.Encode.Value
encodeNar nar =
    encodeNarObject nar


encodeNarObject : Nix.Archive.Nar -> Json.Encode.Value
encodeNarObject nar =
    case nar of
        Nix.Archive.RegularObject argA ->
            Json.Encode.list identity
                [ Json.Encode.string "regular"
                , encodeRegular argA
                ]

        Nix.Archive.SymlinkObject argA ->
            Json.Encode.list identity
                [ Json.Encode.string "symlink"
                , encodeSymlink argA
                ]

        Nix.Archive.DirectoryObject argA ->
            Json.Encode.list identity
                [ Json.Encode.string "directory"
                , Json.Encode.list encodeDirectoryEntry argA
                ]


encodeRegular : Nix.Archive.Regular -> Json.Encode.Value
encodeRegular rec =
    Json.Encode.object [ ( "executable", Json.Encode.bool rec.executable ), ( "contents", encodeBytes rec.contents ) ]


encodeBytes : Bytes.Bytes -> Json.Encode.Value
encodeBytes b =
    case Bytes.Decoder.decode (Bytes.Decoder.repeat Bytes.Decoder.unsignedInt8 (Bytes.width b)) b of
        Ok sequence ->
            Json.Encode.list Json.Encode.int sequence

        Err _ ->
            Json.Encode.string "Failed to encode bytes "


encodeSymlink : Nix.Archive.Symlink -> Json.Encode.Value
encodeSymlink rec =
    Json.Encode.object [ ( "target", Json.Encode.string rec.target ) ]


encodeDirectoryEntry : Nix.Archive.DirectoryEntry -> Json.Encode.Value
encodeDirectoryEntry rec =
    Json.Encode.object [ ( "name", Json.Encode.string rec.name ), ( "node", encodeNarObject rec.node ) ]
