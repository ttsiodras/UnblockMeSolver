(* for RGB data of the image *)
open Bigarray
let g_width = 320
let g_height = 480

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
        Printf.printf "Horizontal blocks at %d,%d of length 2 %s\n" y x marker;
        blocks := (make_block y x true tiles.(y).(x) 2) ::
                  (make_block y (x+2) true tiles.(y).(x+2) 2) ::
                  !blocks
    ) else (
        (* ... otherwise emit only one block *)
        Printf.printf "Horizontal block at %d,%d of length %d %s\n" y x length marker;
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
    Printf.printf "Vertical   block at %d,%d of length %d\n" y x length;
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

(* This function (called at startup) scans the tiles and borders
 * arrays, and understands where the blocks are.
 * It then returns a list of the detected Blocks *)
let scanBodiesAndBordersAndEmitStartingBlockPositions tiles borders =
    let blocks = ref [] in
    (* Initially, we don't have a clue what each tile has *)
    let isTileKnown = Array.make_matrix g_boardSize g_boardSize false in
    let allDone = ref false in
    while not !allDone do
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
    List.rev !blocks

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


(* A board is indeed represented as a list of Blocks.
 * However, when we move Blocks around, we need to be able
 * to detect if a tile is empty or not - so a 2D representation
 * (for quick tile access) is required. *)
type board = {
    _data : tileKind array array;
}
let make_board listOfBlocks =
    let brd = { _data = Array.make_matrix g_boardSize g_boardSize Empty; } in
    List.iter (fun blk ->
        if blk._isHorizontal then
            for i=0 to pred blk._length do
                brd._data.(blk._y).(blk._x+i) <- blk._kind
            done
        else
            for i=0 to pred blk._length do
                brd._data.(blk._y+i).(blk._x) <- blk._kind
            done) listOfBlocks;
    brd

(* This function pretty-prints a list of blocks *)
let printBoard listOfBlocks =
    (* start from an empty buffer *)
    let tmp = Array.make_matrix g_boardSize g_boardSize ' ' in
    List.iter (fun blk ->
        (* character emitted for this tile *)
        let c = match blk._kind with
        | Empty -> ' '
        | Prisoner -> 'Z'
        | Block ->
            (* ... and use a different letter for each block *)
            Char.chr (65 + blk._id)
        in
        if blk._isHorizontal then
            for i=0 to pred blk._length do
                tmp.(blk._y).(blk._x+i) <- c
            done
        else
            for i=0 to pred blk._length do
                tmp.(blk._y+i).(blk._x) <- c
            done
        ) listOfBlocks;
    Printf.printf "+------------------+\n|";
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do
            Printf.printf "%c%c " tmp.(y).(x) tmp.(y).(x)
        done;
        Printf.printf "|\n|";
    done;
    Printf.printf "\b+------------------+\n";
    ()

(* When we find the solution, we also need to backtrack
 * to display the moves we used to get there...
 *
 * "Move" stores what block moved and to what direction *)
type direction =
    Left
    | Right
    | Up
    | Down

type move = {
    _blockId: int;
    _direction: direction;
}

let ( |> ) x fn = fn x

let findBlockMoves board b =
    let alternatives = ref [] in
    if b._isHorizontal then (
        (*  Can the b move to the left? *)
        if b._x>0 && Empty = board._data.(b._y).(b._x-1) then
            alternatives := ({b with _x=b._x-1},{_blockId=b._id; _direction=Left}) :: !alternatives;
        (*  Can the b move to the right? *)
        if b._x+b._length<g_boardSize && Empty = board._data.(b._y).(b._x+b._length) then
            alternatives := ({b with _x=b._x+1}, {_blockId=b._id; _direction=Right}) :: !alternatives
    ) else (
        (*  Can the b move up? *)
        if b._y>0 && Empty = board._data.(b._y-1).(b._x) then
            alternatives := ({b with _y=b._y-1}, {_blockId=b._id; _direction=Up}) :: !alternatives;
        (*  Can the b move down? *)
        if b._y+b._length<g_boardSize && Empty = board._data.(b._y+b._length).(b._x) then
            alternatives := ({b with _y=b._y+1}, {_blockId=b._id; _direction=Down}) :: !alternatives
    );
    List.rev !alternatives

(* The brains of the operation - basically a Breadth-First-Search
 * of the problem space:
 *     http://en.wikipedia.org/wiki/Breadth-first_search
 *)
let solveBoard listOfBlocks =
    Printf.printf "\nSearching for a solution...\n";
    (* We need to store the last move that got us to a specific *)
    (*  board state - that way we can backtrack from a final board *)
    (*  state to the list of moves we used to achieve it. *)
    let previousMoves = Hashtbl.create 1000000 in
    (*  Start by storing a "sentinel" value, for the initial board *)
    (*  state - we used no Move to achieve it, so store a block id *)
    (*  of -1 to mark it: *)
    Hashtbl.add previousMoves (make_board listOfBlocks) {_blockId= -1; _direction=Left};
    (*  We must not revisit board states we have already examined, *)
    (*  so we need a 'visited' set: *)
    let visited = Hashtbl.create 100000 in
    (*  Now, to implement Breadth First Search, all we need is a Queue *)
    (*  storing the states we need to investigate - so it needs to *)
    (*  be a list of board states... i.e. a list of list of Blocks! *)
    let queue = Queue.create () in
    (*  Start with our initial board state *)
    Queue.add listOfBlocks queue;

    while not (Queue.is_empty queue) do
        (*  Extract first element of the queue *)
        let blocks = Queue.take queue in
        (*  Create a Board for fast 2D access to tile state *)
        let board = make_board blocks in
        (*  Have we seen this board before? *)
        if Hashtbl.mem visited board then
            (*  Yep - skip it *)
            ()
        else (
            (*  No, we haven't - store it so we avoid re-doing *)
            (*  the following work again in the future... *)
            Hashtbl.add visited board 1;
            (*  Check if this board state is a winning state: *)
            (*  Find prisoner block... *)
            let it = List.find (fun blk -> blk._kind = Prisoner) blocks in
            (*  Can he escape? Check to his right! *)
            let allClear = ref true in
            for x=it._x+it._length to pred g_boardSize do
                allClear := !allClear && Empty = board._data.(it._y).( x);
            done;
            if !allClear then (
                (*  Yes, he can escape - we did it! *)
                Printf.printf "Solved!\n";
                (*  To print the Moves we used in normal order, we will *)
                (*  backtrack through the board states to print *)
                (*  the Move we used at each one... *)
                let solution = Stack.create () in
                Stack.push blocks solution;
                let currentBoard = ref board in
                let currentBlocks = ref blocks in
                let foundSentinel = ref false in
                let hack = Hashtbl.create 1000 in
                while not !foundSentinel && Hashtbl.mem previousMoves !currentBoard do
                    let itMove = Hashtbl.find previousMoves !currentBoard in
                    if itMove._blockId = -1 then
                        (*  Sentinel - reached starting board *)
                        foundSentinel := true
                    else (
                        (*  Find the block we moved, and move it *)
                        (*  (in reverse direction - we are going back) *)
                        let backStep = !currentBlocks |>
                            List.map (fun blk ->
                            if blk._id = itMove._blockId then
                                match itMove._direction with
                                | Left  -> {blk with _x = blk._x+1 }
                                | Right -> {blk with _x = blk._x-1 }
                                | Up    -> {blk with _y = blk._y+1 }
                                | Down  -> {blk with _y = blk._y-1 }
                            else
                                blk) in
                        (*  Add this board to the front of the list... *)
                        currentBlocks := backStep;
                        Stack.push !currentBlocks solution;
                        currentBoard := make_board backStep;
                        if Hashtbl.mem hack !currentBoard then
                            foundSentinel := true
                        else
                            Hashtbl.add hack !currentBoard 1
                    )
                done;
                (*  Now that we have the full list, emit it in order *)
                solution |> Stack.iter (fun listOfBlocks -> (
                    print_endline "Press ENTER to see next move\n";
                    let dummy = input_line stdin in
                    printBoard listOfBlocks))
                ;
                Printf.printf "Run free, prisoner, run! :-)\n";
                exit 0;
            ) else (
                (*  Nope, the prisoner is still trapped. *)
                (*  *)
                (*  Add all potential states arrising from immediate *)
                (*  possible moves to the end of the queue. *)

                let arrayOfBlocks = Array.of_list blocks in
                for i=0 to pred (Array.length arrayOfBlocks) do
                    let oldBlock = arrayOfBlocks.(i) in
                    findBlockMoves board oldBlock |>
                        List.iter (fun (block, move) ->
                            arrayOfBlocks.(i) <- block ;
                            (* Add to the end of the queue for further study :-) *)
                            let newListOfBlocks = Array.to_list arrayOfBlocks in
                            let newBoard = make_board newListOfBlocks in
                            if not (Hashtbl.mem visited newBoard) then (
                                Queue.add newListOfBlocks queue;
                                (* Store board and move, so we can backtrack later *)
                                Hashtbl.add previousMoves newBoard move; (*
                                let msg = match move._direction with
                                | Left -> "Left"
                                | Right -> "Right"
                                | Up -> "Up"
                                | Down -> "Down" in
                                Printf.printf "Block %c moved %s generates this:\n" (Char.chr (65 + block._id)) msg;
                                printBoard newListOfBlocks; *)
                            )
                        )
                    ;
                    arrayOfBlocks.(i) <- oldBlock
                done;
            );
        )
        (*  and go recheck the queue, from the top! *)
    done

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
    let listOfBlocks = scanBodiesAndBordersAndEmitStartingBlockPositions tiles borders in
    solveBoard listOfBlocks
