(* RGB data of the image *)
open Bigarray

let g_width = 320
let g_height = 480
let image = Array.make_matrix g_height g_width 0

(* The board is g_boardSize x g_boardSize tiles *)
let g_boardSize = 6

(* The tile "bodies" information - filled by DetectTileBodies()
   via heuristics on the center pixel of the tile *)

type tileKind =
    Empty
  | Block
  | Prisoner

(* The top and bottom "borders" of each tile
   (hence the 2x in the vertical direction)
   filled by DetectTopAndBottomTileBorders()  *)

type borderKind =
    NotBorder
  | White
  | Black

(* The board is a list of Blocks *)
let blockId = ref (-1)

type block = {
    _id: int;            (* ...uniquely identify each block    *)
    _y: int;             (* block's top-left tile coordinates  *)
    _x: int;             (* block's top-left tile coordinates  *)
    _isHorizontal: bool; (* whether the block is Horiz/Vert    *)
    _kind: tileKind;     (* can only be block or prisoner      *)
    _length: int;        (* how many tiles long this block is  *)
}
let make_block y x isHorizontal kind length =
    blockId := !blockId + 1;
    {
        _id = !blockId;
        _y = y; _x = x; _isHorizontal = isHorizontal; _kind = kind; _length = length;
    }

(* This function (called at startup) scans the tiles and borders
   arrays, and understands where the blocks are.

   Returns a list of the detected Blocks *)
let detectHorizontalSpan y x blocks tiles borders isTileKnown marker =
    (* If a tile has white on top and black on bottom,
       then it is part of a horizontal block *)
    let xend = ref (x+1) in
    (* Scan horizontally to find its end *)
    while   !xend < g_boardSize &&
            borders.(2*y+1).(!xend) = Black &&
            borders.(2*y).(!xend) = White do
        isTileKnown.(y).(!xend) <- true;
        xend := !xend + 1;
    done;
    (* two adjacent blocks of length 2 would lead
       to a 'block' of length 4... *)
    let length = !xend - x in
    if length = 4 then (
        (* ...in that case, emit two blocks of length 2 *)
        Printf.printf "Horizontal blocks at %d, %d of length 2 %s\n" y x marker;
        blocks := (make_block y x true tiles.(y).(x) 2) ::
                  (make_block y (x+2) true tiles.(y).(x+2) 2) ::
                  !blocks
    ) else (
        (* ... otherwise emit only one block *)
        Printf.printf "Horizontal block at %d, %d of length %d %s\n" y x length marker;
        blocks := (make_block y x true tiles.(y).(x) length) :: !blocks
    )


let detectVerticalSpan y x blocks tiles borders isTileKnown marker =
    (* If a tile has white on top, but no black
       on bottom, then it is part of a vertical block. *)
    let yend = ref (y+1) in
    (* Scan vertically to find its end *)
    while !yend<g_boardSize && borders.(2* !yend+1).(x) <> Black
    do
        isTileKnown.(!yend).(x) <- true;
        yend := !yend + 1;
    done;
    let length = !yend - y + 1 in
    Printf.printf "Vertical block at %d, %d of length %d\n" y x length;
    blocks :=
        make_block y x false tiles.(y).(x) length ::
        !blocks ;
    ()

let detectSpans y x blocks tiles borders isTileKnown =
    let marker = match tiles.(y).(x) with
    | Prisoner -> " (marker)"
    | _ -> "" in
    (* Use the border information *)
    match borders.(2*y).(x), borders.(2*y+1).(x) with
    | White, Black ->
        detectHorizontalSpan y x blocks tiles borders isTileKnown marker
    | White, _ ->
        detectVerticalSpan y x blocks tiles borders isTileKnown marker
    | _, _ ->
        ()
    ;
    ()

let scanBodiesAndBordersAndEmitStartingBlockPositions tiles borders =
    let blocks = ref [] in
    (* Initially, we don't have a clue what each tile has *)
    let isTileKnown = Array.make_matrix g_boardSize g_boardSize false in
    let allDone = ref false in
    while true <> !allDone do
        for y=0 to pred g_boardSize do
            for x=0 to pred g_boardSize do
                match isTileKnown.(y).(x) , tiles.(y).(x) with
                | true, _ ->
                    (* Skip over known tiles *)
                    ();
                | false, Empty ->
                    (* Skip over empty tiles *)
                    isTileKnown.(y).(x) <- true
                | false, _ ->
                    isTileKnown.(y).(x) <- true;
                    detectSpans y x blocks tiles borders isTileKnown
            done
        done ;
        allDone := true ;
        for y=0 to pred g_boardSize do
            for x=0 to pred g_boardSize do
                allDone := !allDone && isTileKnown.(y).(x);
            done
        done ;
    done ;
    blocks

(* This function looks at the center pixel of each tile,
   and guesses what TileKind it is.
   (Heuristics on the snapshots taken from my iPhone) *)
let detectTileBodies all_channels =
    Printf.printf "Detecting tile bodies...\n";
    let tiles = Array.make_matrix g_boardSize g_boardSize Empty in
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do
            let line = 145 + y*50 in
            let column = 34 + x*50 in
            (* The red channel, surprisingly, was not necessary *)
            let g = all_channels.{1,column,line} in
            let b = all_channels.{2,column,line} in
            if (b > 30) then
                tiles.(y).(x) <- Empty
            else if (g < 30) then
                tiles.(y).(x) <- Prisoner
            else
                tiles.(y).(x) <- Block;
        done;
    done;
    tiles

let printTiles tiles borders =
    let printBorder = function
        | NotBorder -> Printf.printf "       ";
        | White     -> Printf.printf "------ ";
        | Black     -> Printf.printf "====== " in
    let printBlock = function
        | Empty    -> Printf.printf "       ";
        | Block    -> Printf.printf "OOOOOO ";
        | Prisoner -> Printf.printf "XXXXXX " in
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do printBorder borders.(2*y).(x) done;
        Printf.printf "\n";
        for x=0 to pred g_boardSize do printBlock tiles.(y).(x) done;
        Printf.printf "\n";
        for x=0 to pred g_boardSize do printBorder borders.(2*y+1).(x) done;
        Printf.printf "\n"
    done;
    Printf.printf "\n"


let detectTopAndBottomTileBorders all_channels =
    let borders = Array.make_matrix (2*g_boardSize) g_boardSize NotBorder in
    Printf.printf "Detecting top and bottom tile borders...\n\n";
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do
            let line    = 145 + y*50 in
            let column  =  34 + x*50 in
            let ytop    = line - 23 in
            let ybottom = line + 23 in
            let r = all_channels.{0,column,ytop} in
            let g = all_channels.{1,column,ytop} in
            if       r > 200 && g > 160  then borders.(y*2).(x) <- White
            else if  r < 40 && g < 30    then borders.(y*2).(x) <- Black
            else                              borders.(y*2).(x) <- NotBorder;
            let r = all_channels.{0,column,ybottom} in
            let g = all_channels.{1,column,ybottom} in
            if       r > 200 && g > 160  then borders.(y*2+1).(x) <- White
            else if  r < 40 && g < 30    then borders.(y*2+1).(x) <- Black
            else                              borders.(y*2+1).(x) <- NotBorder;
        done
    done;
    borders

let _ =
    let ic = open_in "data.rgb" in
    let all_channels =
        let kind = Bigarray.int8_unsigned
        and layout = Bigarray.c_layout in
        Bigarray.Array3.create kind layout 3 g_width g_height in
    let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
    and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
    and b_channel = Bigarray.Array3.slice_left_2 all_channels 2 in
    for y = 0 to pred g_height do
        for x = 0 to pred g_width do
            r_channel.{x,y} <- (input_byte ic);
            g_channel.{x,y} <- (input_byte ic);
            b_channel.{x,y} <- (input_byte ic);
        done;
    done;
    close_in ic;
    let tiles = detectTileBodies all_channels in
    let borders = detectTopAndBottomTileBorders all_channels in
    printTiles tiles borders;
    scanBodiesAndBordersAndEmitStartingBlockPositions tiles borders
