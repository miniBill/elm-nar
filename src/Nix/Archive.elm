module Nix.Archive exposing (DecodeContext(..), DecodeError(..), Directory, DirectoryEntry, Nar, NarObject(..), Regular, Symlink, decoder, encode)

import Bytes exposing (Bytes)
import Bytes.Decoder
import Bytes.Encode


type alias Nar =
    NarObject


type NarObject
    = RegularObject Regular
    | SymlinkObject Symlink
    | DirectoryObject Directory


type alias Regular =
    { executable : Bool
    , contents : Bytes
    }


type alias Symlink =
    { target : String
    }


type alias Directory =
    List DirectoryEntry


type alias DirectoryEntry =
    { name : String
    , node : NarObject
    }


type DecodeError
    = ExpectedPaddingToBeZero
    | StringTooLong
      -- The first argument is the found value, the second one is the expected one
    | Unexpected String String


type DecodeContext
    = NarObjectContext


encode : Nar -> Bytes.Encode.Encoder
encode obj =
    [ encodeStr "nix-archive-1"
    , encodeNarObject obj
    ]
        |> Bytes.Encode.sequence


decoder : Bytes.Decoder.Decoder DecodeContext DecodeError Nar
decoder =
    Bytes.Decoder.succeed identity
        |> Bytes.Decoder.ignore (ensureStr "nix-archive-1")
        |> Bytes.Decoder.keep decodeNarObject


encodeNarObject : NarObject -> Bytes.Encode.Encoder
encodeNarObject obj =
    [ encodeStr "("
    , encodeNarObjectInner obj
    , encodeStr ")"
    ]
        |> Bytes.Encode.sequence


decodeNarObject : Bytes.Decoder.Decoder DecodeContext DecodeError NarObject
decodeNarObject =
    Bytes.Decoder.succeed identity
        |> Bytes.Decoder.ignore (ensureStr "(")
        |> Bytes.Decoder.keep decodeNarObjectInner
        |> Bytes.Decoder.ignore (ensureStr ")")
        |> Bytes.Decoder.inContext NarObjectContext


encodeNarObjectInner : NarObject -> Bytes.Encode.Encoder
encodeNarObjectInner obj =
    case obj of
        RegularObject regular ->
            encodeStr "type"
                :: encodeStr "regular"
                :: encodeRegular regular
                |> Bytes.Encode.sequence

        SymlinkObject symlink ->
            encodeStr "type"
                :: encodeStr "symlink"
                :: encodeSymlink symlink
                |> Bytes.Encode.sequence

        DirectoryObject directory ->
            encodeStr "type"
                :: encodeStr "directory"
                :: encodeDirectory directory
                |> Bytes.Encode.sequence


decodeNarObjectInner : Bytes.Decoder.Decoder DecodeContext DecodeError NarObject
decodeNarObjectInner =
    Bytes.Decoder.succeed identity
        |> Bytes.Decoder.ignore (ensureStr "type")
        |> Bytes.Decoder.keep decodeStr
        |> Bytes.Decoder.andThen
            (\type_ ->
                case type_ of
                    "regular" ->
                        Bytes.Decoder.map RegularObject decodeRegular

                    "symlink" ->
                        Bytes.Decoder.map SymlinkObject decodeSymlink

                    "directory" ->
                        Bytes.Decoder.map DirectoryObject decodeDirectory

                    _ ->
                        Bytes.Decoder.fail (Unexpected type_ "One of regular, symlink, directory")
            )


encodeDirectory : Directory -> List Bytes.Encode.Encoder
encodeDirectory entries =
    entries
        |> List.sortBy .name
        |> List.map encodeDirectoryEntry


decodeDirectory : Bytes.Decoder.Decoder DecodeContext DecodeError Directory
decodeDirectory =
    Bytes.Decoder.loop
        (\acc ->
            Bytes.Decoder.position
                |> Bytes.Decoder.andThen
                    (\position ->
                        Bytes.Decoder.randomAccess { offset = 0, relativeTo = position } decodeStr
                    )
                |> Bytes.Decoder.andThen
                    (\peek ->
                        case peek of
                            "entry" ->
                                Bytes.Decoder.succeed (\e -> Bytes.Decoder.Loop (e :: acc))
                                    |> Bytes.Decoder.keep decodeDirectoryEntry

                            _ ->
                                Bytes.Decoder.succeed (Bytes.Decoder.Done (List.reverse acc))
                    )
        )
        []


encodeSymlink : Symlink -> List Bytes.Encode.Encoder
encodeSymlink { target } =
    [ encodeStr "target"
    , encodeStr target
    ]


decodeSymlink : Bytes.Decoder.Decoder DecodeContext DecodeError Symlink
decodeSymlink =
    Bytes.Decoder.succeed (\target -> { target = target })
        |> Bytes.Decoder.ignore (ensureStr "target")
        |> Bytes.Decoder.keep decodeStr


encodeRegular : Regular -> List Bytes.Encode.Encoder
encodeRegular regular =
    if regular.executable then
        [ encodeStr "executable"
        , encodeStr ""
        , encodeStr "contents"
        , encodeStrBytes regular.contents
        ]

    else
        [ encodeStr "contents"
        , encodeStrBytes regular.contents
        ]


decodeRegular : Bytes.Decoder.Decoder DecodeContext DecodeError Regular
decodeRegular =
    decodeStr
        |> Bytes.Decoder.andThen
            (\executableOrEmpty ->
                case executableOrEmpty of
                    "executable" ->
                        Bytes.Decoder.succeed (Regular True)
                            |> Bytes.Decoder.ignore (ensureStr "")
                            |> Bytes.Decoder.ignore (ensureStr "contents")
                            |> Bytes.Decoder.keep decodeStrBytes

                    "contents" ->
                        Bytes.Decoder.succeed (Regular False)
                            |> Bytes.Decoder.keep decodeStrBytes

                    _ ->
                        Bytes.Decoder.fail (Unexpected executableOrEmpty "One of executable or \"\"")
            )


encodeDirectoryEntry : DirectoryEntry -> Bytes.Encode.Encoder
encodeDirectoryEntry directoryEntry =
    [ encodeStr "entry"
    , encodeStr "("
    , encodeStr "name"
    , encodeStr directoryEntry.name
    , encodeStr "node"
    , encodeNarObject directoryEntry.node
    , encodeStr ")"
    ]
        |> Bytes.Encode.sequence


decodeDirectoryEntry : Bytes.Decoder.Decoder DecodeContext DecodeError DirectoryEntry
decodeDirectoryEntry =
    Bytes.Decoder.succeed identity
        |> Bytes.Decoder.ignore (ensureStr "entry")
        |> Bytes.Decoder.ignore (ensureStr "(")
        |> Bytes.Decoder.ignore (ensureStr "name")
        |> Bytes.Decoder.keep decodeStr
        |> Bytes.Decoder.ignore (ensureStr "node")
        |> Bytes.Decoder.andThen
            (\name ->
                Bytes.Decoder.succeed (DirectoryEntry name)
                    |> Bytes.Decoder.keep decodeNarObject
                    |> Bytes.Decoder.ignore (ensureStr ")")
            )


encodeStrBytes : Bytes -> Bytes.Encode.Encoder
encodeStrBytes s =
    let
        width : Int
        width =
            Bytes.width s
    in
    Bytes.Encode.sequence
        (Bytes.Encode.unsignedInt32 Bytes.LE width
            :: Bytes.Encode.unsignedInt32 Bytes.LE 0
            :: Bytes.Encode.bytes s
            :: List.repeat
                ((8 - modBy 8 width) |> modBy 8)
                (Bytes.Encode.unsignedInt8 0)
        )


decodeStrBytes : Bytes.Decoder.Decoder DecodeContext DecodeError Bytes
decodeStrBytes =
    Bytes.Decoder.map2 Tuple.pair
        (Bytes.Decoder.unsignedInt32 Bytes.LE)
        (Bytes.Decoder.unsignedInt32 Bytes.LE)
        |> Bytes.Decoder.andThen
            (\( low, high ) ->
                let
                    paddingSize : Int
                    paddingSize =
                        (8 - modBy 8 low) |> modBy 8
                in
                if high /= 0 then
                    Bytes.Decoder.fail StringTooLong

                else
                    Bytes.Decoder.succeed identity
                        |> Bytes.Decoder.keep (Bytes.Decoder.bytes low)
                        |> Bytes.Decoder.ignore (Bytes.Decoder.repeat ensureZero paddingSize)
            )


encodeStr : String -> Bytes.Encode.Encoder
encodeStr s =
    let
        width : Int
        width =
            Bytes.Encode.getStringWidth s
    in
    Bytes.Encode.sequence
        (Bytes.Encode.unsignedInt32 Bytes.LE width
            :: Bytes.Encode.unsignedInt32 Bytes.LE 0
            :: Bytes.Encode.string s
            :: List.repeat
                ((8 - modBy 8 width) |> modBy 8)
                (Bytes.Encode.unsignedInt8 0)
        )


ensureStr : String -> Bytes.Decoder.Decoder DecodeContext DecodeError ()
ensureStr expected =
    decodeStr
        |> Bytes.Decoder.andThen
            (\found ->
                if found == expected then
                    Bytes.Decoder.succeed ()

                else
                    Bytes.Decoder.fail (Unexpected found expected)
            )


decodeStr : Bytes.Decoder.Decoder DecodeContext DecodeError String
decodeStr =
    Bytes.Decoder.map2 Tuple.pair
        (Bytes.Decoder.unsignedInt32 Bytes.LE)
        (Bytes.Decoder.unsignedInt32 Bytes.LE)
        |> Bytes.Decoder.andThen
            (\( low, high ) ->
                let
                    paddingSize : Int
                    paddingSize =
                        (8 - modBy 8 low) |> modBy 8
                in
                if high /= 0 then
                    Bytes.Decoder.fail StringTooLong

                else
                    Bytes.Decoder.succeed identity
                        |> Bytes.Decoder.keep (Bytes.Decoder.string low)
                        |> Bytes.Decoder.ignore (Bytes.Decoder.repeat ensureZero paddingSize)
            )


ensureZero : Bytes.Decoder.Decoder DecodeContext DecodeError ()
ensureZero =
    Bytes.Decoder.unsignedInt8
        |> Bytes.Decoder.andThen
            (\u ->
                if u == 0 then
                    Bytes.Decoder.succeed ()

                else
                    Bytes.Decoder.fail ExpectedPaddingToBeZero
            )
