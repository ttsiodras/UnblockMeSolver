(* for RGB data of the image *)
open Bigarray
let g_width = 320
let g_height = 480

(* The board is g_boardSize x g_boardSize tiles *)
let g_boardSize = 6

(* Debugging mode *)
let g_debug = ref false

(* Very useful syntactic sugar *)
let ( |> ) x fn = fn x

(* The tile "bodies" information - filled by detectTileBodies below,
   via heuristics on the center pixel of the tiles *)
type tileKind = Empty | Block | Prisoner

(* This function looks at the center pixel of each tile,
   and guesses what tileKind it is.
   (Heuristics on the snapshots taken from my iPhone) *)
let detectTileBodies all_channels =
    if !g_debug then
        print_endline "Detecting tile bodies...";
    let tiles = Array.make_matrix g_boardSize g_boardSize Empty in
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do
            let line = 145 + y*50 in
            let column = 34 + x*50 in
            (* The red channel, surprisingly, was not necessary *)
            let g = all_channels.{1,column,line} in
            let b = all_channels.{2,column,line} in
            if (b > 30) then       tiles.(y).(x) <- Empty
            else if (g < 30) then  tiles.(y).(x) <- Prisoner
            else                   tiles.(y).(x) <- Block;
        done;
    done;
    tiles

(* The top and bottom "borders" of each tile
   filled by detectTopAndBottomTileBorders below
   via heuristics on the top/bottom centerPixel of the tiles *)
type borderKind = NotBorder | White | Black
type tileBorderInfo = {
    _topBorder: borderKind;
    _bottomBorder: borderKind;
}

let detectTopAndBottomTileBorders all_channels =
    let borders =
        Array.make_matrix g_boardSize g_boardSize
            { _topBorder = NotBorder; _bottomBorder = NotBorder }
        in
    if !g_debug then
        print_endline "Detecting top and bottom tile borders...\n";
    for y=0 to pred g_boardSize do
        for x=0 to pred g_boardSize do
            let line    = 145 + y*50 in
            let column  =  34 + x*50 in
            let ytop    = line - 23 in
            let ybottom = line + 23 in
            let rgbToBorderKind r g =
                if r > 200 && g > 160 then White
                else if r < 40 && g < 30 then Black
                else NotBorder
                in
            let rtop = all_channels.{0,column,ytop} in
            let gtop = all_channels.{1,column,ytop} in
            let topBorderKind = rgbToBorderKind rtop gtop in

            let rbottom = all_channels.{0,column,ybottom} in
            let gbottom = all_channels.{1,column,ybottom} in
            let bottomBorderKind = rgbToBorderKind rbottom gbottom in

            borders.(y).(x) <-
                { _topBorder=topBorderKind; _bottomBorder=bottomBorderKind }
        done
    done;
    borders

(* function to debug stage 1, i.e. parsing imageData into tiles/borders *)
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
        for x=0 to pred g_boardSize do
            printBorder borders.(y).(x)._topBorder done;
        print_newline ();
        for x=0 to pred g_boardSize do printBlock tiles.(y).(x) done;
        print_newline ();
        for x=0 to pred g_boardSize do
            printBorder borders.(y).(x)._bottomBorder done;
        print_newline ();
    done;
    print_newline ()

(* The board is represented as a list of blocks *)
let g_blockIdSeq = ref (-1) (* global counter used to assign blk id *)
type block = {
    _id: int;               (* ... and uniquely identify each block *)
    _y: int;                (* block's top-left tile coordinates    *)
    _x: int;                (* block's top-left tile coordinates    *)
    _isHorizontal: bool;    (* whether the block is Horiz/Vert      *)
    _kind: tileKind;        (* can only be block or prisoner        *)
    _length: int;           (* how many tiles long this block is    *)
}
let make_block y x isHorizontal kind length =
    g_blockIdSeq := !g_blockIdSeq + 1;
    {
        _id = !g_blockIdSeq;
        _y = y; _x = x; _isHorizontal = isHorizontal;
        _kind = kind; _length = length;
    }

let detectHorizontalSpan y x tiles borders isTileKnown =
    let msg = match tiles.(y).(x) with
    | Prisoner -> " (prisoner)"
    | _ -> "" in
    (* If a tile has white on top and black on bottom,
       then it is part of a horizontal block *)
    let xend = ref (x+1) in
    (* Scan horizontally to find its end *)
    while   !xend < g_boardSize &&
            borders.(y).(!xend)._topBorder = White &&
            borders.(y).(!xend)._bottomBorder = Black do
        isTileKnown.(y).(!xend) <- true;
        xend := !xend + 1;
    done;
    (* two adjacent blocks of length 2 would lead
       to a 'block' of length 4... *)
    let length = !xend - x in
    if length = 4 then (
        (* ...in that case, emit two blocks of length 2 *)
        if !g_debug then
            Printf.printf
                "Horizontal blocks at %d,%d of length 2 %s\n" y x msg;
        [ (make_block y (x+2) true tiles.(y).(x+2) 2) ;
          (make_block y x true tiles.(y).(x) 2) ]
    ) else (
        (* ... otherwise emit only one block *)
        if !g_debug then
            Printf.printf
                "Horizontal block  at %d,%d of length %d %s\n" y x length msg;
        [ (make_block y x true tiles.(y).(x) length) ]
    )

let detectVerticalSpan y x tiles borders isTileKnown =
    (* If a tile has white on top, but no black
       on bottom, then it is part of a vertical block. *)
    let yend = ref (y+1) in
    (* Scan vertically to find its end *)
    while !yend<g_boardSize && borders.(!yend).(x)._bottomBorder <> Black
    do
        isTileKnown.(!yend).(x) <- true;
        yend := !yend + 1;
    done;
    let length = !yend - y + 1 in
    if !g_debug then
        Printf.printf "Vertical   block  at %d,%d of length %d\n" y x length;
    [ ( make_block y x false tiles.(y).(x) length ) ]

let detectBlockSpans y x tiles borders isTileKnown =
    (* Use the border information *)
    match borders.(y).(x)._topBorder, borders.(y).(x)._bottomBorder with
    | White, Black ->
        detectHorizontalSpan y x tiles borders isTileKnown
    | White, _ ->
        detectVerticalSpan y x tiles borders isTileKnown
    | _, _ ->
        []

(* This function (called at startup) scans the tiles and borders
 * arrays, and understands where the blocks are.
 * It then returns a list of the detected blocks *)
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
                    ()
                | false, Empty ->
                    (* Skip over empty tiles *)
                    isTileKnown.(y).(x) <- true
                | false, _ ->
                    isTileKnown.(y).(x) <- true;
                    blocks :=
                        List.append
                            (detectBlockSpans y x tiles borders isTileKnown)
                            !blocks
            done
        done ;
        allDone := true ;
        for y=0 to pred g_boardSize do
            for x=0 to pred g_boardSize do
                allDone := !allDone && isTileKnown.(y).(x)
            done
        done
    done ;
    List.rev !blocks

(* A board is represented as a list of blocks.
 * However, when we move blocks around, we need to be able
 * to detect if a tile is empty or not - so a 2D representation
 * (for quick tile access) is required. *)
type board = {
    _data : tileKind array array;
}
let make_board listOfBlocks =
    let brd = {
        _data = Array.make_matrix g_boardSize g_boardSize Empty;
    } in
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
 * "move" stores what block moved and in what direction *)
type direction = Left | Right | Up | Down
type move = {
    _blockId: int;
    _direction: direction;
}

(* find the possible moves a block can do, and return
 * a list of tuples: (newBlock, move)   *)
let findBlockMoves board b =
    let myConcat a b =
        match a, b with
        | [], [] -> []
        | x , [] -> x
        | [], y  -> y
        | x,  y  -> List.append y x in
    let isEmptyTile dx dy dir =
        let tileToCheckCoord v dv = match dv with
        | 0          -> v
        | x when x<0 -> v + dv
        | _          -> v + b._length in
        let tx = tileToCheckCoord b._x dx in
        let ty = tileToCheckCoord b._y dy in
        if tx>=0 && tx<g_boardSize && ty>=0 && ty<g_boardSize &&
            Empty = board._data.(ty).(tx)
            then [ ({ b with _x=b._x+dx; _y=b._y+dy },
                    { _blockId=b._id; _direction=dir}) ]
            else []
        in
    if b._isHorizontal then
        myConcat
            (*  Can the block move to the left? *)
            (isEmptyTile (-1) 0 Left)
            (*  Can the block move to the right? *)
            (isEmptyTile 1 0 Right)
    else
        myConcat
            (*  Can the block move up? *)
            (isEmptyTile 0 (-1) Up)
            (*  Can the block move down? *)
            (isEmptyTile 0 1 Down)

(* The brains of the operation - basically a Breadth-First-Search
   of the problem space:
       http://en.wikipedia.org/wiki/Breadth-first_search *)
let solveBoard listOfBlocks =
    if !g_debug then
        print_endline "\nSearching for a solution...";
    (* We need to store the last move that got us to a specific *)
    (*  board state - that way we can backtrack from a final board *)
    (*  state to the list of moves we used to achieve it. *)
    let previousMoves = Hashtbl.create 1000000 in
    (*  Start by storing a "sentinel" value, for the initial board *)
    (*  state - we used no Move to achieve it, so store a block id *)
    (*  of -1 to mark it: *)
    Hashtbl.add previousMoves
        (make_board listOfBlocks)
        {_blockId= -1; _direction=Left};
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
        if not (Hashtbl.mem visited board) then (
            (*  No, we haven't - store it so we avoid re-doing *)
            (*  the following work again in the future... *)
            Hashtbl.add visited board 1;
            (*  Check if this board state is a winning state: *)
            (*  Find prisoner block... *)
            let it = List.find (fun blk -> blk._kind = Prisoner) blocks in
            (*  Can he escape? Check to his right! *)
            let allClear = ref true in
            for x=it._x+it._length to pred g_boardSize do
                allClear := !allClear && Empty = board._data.(it._y).( x)
            done;
            if !allClear then (
                (*  Yes, he can escape - we did it! *)
                print_endline "Solved!";
                (*  To print the Moves we used in normal order, we will *)
                (*  backtrack through the board states to store in a Stack *)
                (*  the Move we used at each step... *)
                let solution = Stack.create () in
                Stack.push blocks solution;
                let currentBoard = ref board in
                let currentBlocks = ref blocks in
                let foundSentinel = ref false in
                while not !foundSentinel &&
                        Hashtbl.mem previousMoves !currentBoard do
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
                        currentBoard := make_board backStep
                    )
                done;
                (*  Now that we have the moves, emit them in order *)
                solution |> Stack.iter (fun listOfBlocks -> (
                    printBoard listOfBlocks;
                    print_endline "Press ENTER to see next move";
                    let dummy = input_line stdin in
                    print_endline dummy))
                ;
                print_endline "Run free, prisoner, run! :-)";
                exit 0;
            ) else (
                (*  Nope, the prisoner is still trapped. *)
                (*  *)
                (*  Add all potential states arrising from immediate *)
                (*  possible moves to the end of the queue. *)
                if !g_debug then (
                    print_endline "Creating moves for this:";
                    printBoard blocks);
                let arrayOfBlocks = Array.of_list blocks in
                for i=0 to pred (Array.length arrayOfBlocks) do
                    let oldBlock = arrayOfBlocks.(i) in
                    let moves = findBlockMoves board oldBlock in
                    if !g_debug && 0 <> List.length moves then
                        Printf.printf
                            "Block %d: %d moves\n" oldBlock._id
                            (List.length moves);
                    moves |> List.iter (
                        fun (block, move) ->
                            arrayOfBlocks.(i) <- block ;
                            (* Add to the end of the queue *)
                            let newListOfBlocks = Array.to_list arrayOfBlocks
                            in
                            let newBoard = make_board newListOfBlocks in
                            if not (Hashtbl.mem visited newBoard) then (
                                Queue.add newListOfBlocks queue;
                                (* Store board,move - so we can backtrack *)
                                Hashtbl.add previousMoves newBoard move;
                                if !g_debug then (
                                    let msg = match move._direction with
                                    | Left -> "Left"
                                    | Right -> "Right"
                                    | Up -> "Up"
                                    | Down -> "Down" in
                                    Printf.printf
                                        "Moving %c %s generates this:\n"
                                        (Char.chr (65 + block._id)) msg;
                                    printBoard newListOfBlocks;
                                )
                            )
                        )
                    ;
                    arrayOfBlocks.(i) <- oldBlock
                done
            )
        )
        (*  and go recheck the queue, from the top! *)
    done

let parseImageData =
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
    tiles, borders

let _ =
    (* let any = List.fold_left (||) false
     * ..is slower than ... *)
    let rec any l =
        match l with
        | []        -> false
        | true::xs  -> true
        | false::xs -> any xs in
    let inArgs args str =
        any(Array.to_list (Array.map (fun x -> (x = str)) args)) in
    g_debug := inArgs Sys.argv "-debug" ;
    let tiles, borders = parseImageData in
    if !g_debug then
        printTiles tiles borders;
    let listOfBlocks =
        scanBodiesAndBordersAndEmitStartingBlockPositions tiles borders in
    solveBoard listOfBlocks
