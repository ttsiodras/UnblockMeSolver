#include <assert.h>

#include <cstring>
#include <cstdlib>
#include <iostream>
#include <algorithm>
#include <fstream>
#include <map>
#include <set>
#include <list>

using namespace std;

// RGB data of the image
static unsigned char g_image[480][320][3];

// The board is SIZE x SIZE tiles
#define SIZE 6

// The tile "bodies" information - filled by DetectTileBodies()
// via heuristics on the center pixel of the tile
//
enum TileKind {
    empty    = 0,
    block    = 1,
    prisoner = 2
};
static TileKind g_tiles[SIZE][SIZE];

// The top and bottom "borders" of each tile
// (hence the 2x in the vertical direction)
// filled by DetectTopAndBottomTileBorders()
//
enum BorderKind {
    notBorder = 0,
    white     = 1,
    black     = 2
};
static BorderKind g_borders[2*SIZE][SIZE];

// The board is a list of Blocks:
struct Block {
    static int BlockId; // class-global counter, used to...
    int _id;            // ...uniquely identify each block
    int _y, _x;         // block's top-left tile coordinates
    bool _isHorizontal; // whether the block is Horiz/Vert
    TileKind _kind;     // can only be block or prisoner
    int _length;        // how many tiles long this block is
    Block(int y, int x, bool isHorizontal, TileKind kind, int length):
        _id(BlockId++), _y(y), _x(x), _isHorizontal(isHorizontal),
        _kind(kind), _length(length)
        {}
    // Since _id, _y and _x fit in 8 bits,
    // a Block hash can be easily obtained via plain shifts:
    unsigned hash() { return _id | (_y << 8) | (_x << 16); }
};
int Block::BlockId = 0;

// This function (called at startup) scans the g_tiles and g_borders
// arrays, and understands where the blocks are.
//
// Returns a list of the detected Blocks
list<Block> ScanBodiesAndBordersAndEmitStartingBlockPositions()
{
    list<Block> blocks;
    bool isTileKnown[SIZE][SIZE];

    // Initially, we don't have a clue what each tile has
    memset(isTileKnown, false, sizeof(isTileKnown));
    while (true) {
        for(int y=0; y<SIZE; y++) {
            for(int x=0; x<SIZE; x++) {
                if (isTileKnown[y][x])
                    // Skip over tiles we already know
                    continue;

                if (empty == g_tiles[y][x]) {
                    // Skip over empty tiles
                    isTileKnown[y][x] = true;
                    continue;
                }
                bool isMarker = (g_tiles[y][x]==prisoner);
                const char *marker = isMarker?" (marker)":"";

                // Use the border information:
                if (g_borders[2*y][x] == white &&
                        g_borders[2*y+1][x] == black) {
                    // If a tile has white on top and black on bottom,
                    // then it is part of a horizontal block
                    isTileKnown[y][x] = true;
                    int xend = x+1;
                    // Scan horizontally to find its end
                    while(xend<SIZE && g_borders[2*y+1][xend] == black &&
                            g_borders[2*y][xend] == white) {
                        isTileKnown[y][xend] = true;
                        xend++;
                    }
                    // two adjacent blocks of length 2 would lead
                    // to a 'block' of length 4...
                    if (xend-x==4) {
                        // ...in that case, emit two blocks of length 2
                        cout << "Horizontal blocks at " << y << "," << x;
                        cout << " of length 2 " << marker << "\n";
                        blocks.push_back(
                            Block(y,x, true, g_tiles[y][x], 2));
                        blocks.push_back(
                            Block(y,x+2, true, g_tiles[y][x+2], 2));
                    } else {
                        // ... otherwise emit only one block
                        cout << "Horizontal block at " << y << "," << x;
                        cout << " of length " << xend-x << marker << "\n";
                        blocks.push_back(
                            Block(y,x, true, g_tiles[y][x], xend-x));
                    }
                } else if (g_borders[2*y][x] == white) {
                    // If a tile has white on top, but no black
                    // on bottom, then it is part of a vertical block.
                    isTileKnown[y][x] = true;
                    int yend = y+1;
                    // Scan vertically to find its end
                    while(yend<SIZE && g_borders[2*yend+1][x] != black) {
                        isTileKnown[yend][x] = true;
                        yend++;
                    }
                    cout << "Vertical   block at " << y << "," << x;
                    cout << " of length " << yend-y+1 << marker << "\n";
                    blocks.push_back(
                        Block(y,x, false, g_tiles[y][x], yend-y+1));
                } else
                    // either an empty, or a body-of-block tile
                    isTileKnown[y][x] = true;
            }
        }
        bool allDone = true;
        for(int y=0; y<SIZE; y++)
            for(int x=0; x<SIZE; x++)
                allDone = allDone && isTileKnown[y][x];
        if (allDone)
            break;
    }
    return blocks;
}

// This function pretty-prints a list of blocks
void printBoard(const list<Block>& blocks)
{
    unsigned char tmp[SIZE][SIZE];
    // start from an empty buffer
    memset(tmp, ' ', sizeof(tmp));
    for(auto& block: blocks) {
        char c; // character emitted for this tile
        switch (block._kind) {
        case TileKind::empty:
            c = ' ';
            break;
        case TileKind::prisoner:
            c = 'Z'; // Our Zorro tile :-)
            break;
        // ... and use a different letter for each block
        case TileKind::block:
            c = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"[block._id];
            break;
        default:
            assert(0);
        }
        if (block._isHorizontal)
            for(int i=0; i<block._length; i++)
                tmp[block._y][block._x+i] = c;
        else
            for(int i=0; i<block._length; i++)
                tmp[block._y+i][block._x] = c;
    }

    cout << "+------------------+\n|";
    for(int i=0; i<36; i++) {
        char c = tmp[i/SIZE][i%SIZE];
        cout << c << c << " ";
        if (5 == (i%SIZE)) {
            if (i == 17) cout << " \n|"; // Freedom path
            else         cout << "|\n|"; // walls
        }
    }
    cout << "\b+------------------+\n";
}

// A board is indeed represented as a list of Blocks.
// However, when we move Blocks around, we need to be able
// to detect if a tile is empty or not - so a 2D representation
// (for quick tile access) is required.
struct Board {
    TileKind _data[SIZE*SIZE];

    // UPDATE, Jan 26, 2013:
    // Connor Duggan correctly reported that just using tile state
    // is not enough to represent a board - what if we have a different
    // arrangement of vertical and horizontal blocks that cover
    // the same tiles?  e.g.
    //
    //             AA AA          AA BB
    //             BB BB    vs    AA BB
    //
    // if we just compare tile data, the two boards will seem identical
    // when we check the 'visited' set in the main loop - but they aren't!
    //
    // We therefore store a hash per block, formed by combining the
    // block index, and the block coordinates (simple shifts, since
    // all 3 numbers easily fit in 8 bits - see Block::hash() above)
    //
    // How much space to reserve for these hashes? Since blocks are
    // at least 2-tile sized:
    unsigned _hashes[SIZE*SIZE/2];

    // 2D access operator
    // i.e. instead of 'arr[y][x]' you do 'arr(y,x)'
    inline TileKind& operator()(int y, int x) {
        return _data[y*SIZE+x];
    }
    // This type is also used in both sets and maps as a key -
    // so it needs a comparison operator. Using the block
    // _hashes instead of the tile _data, we avoid misidentifying
    // a board state as identical when different block arrangements
    // end up covering the same tiles.
    bool operator<(const Board& r) const {
        return memcmp(_hashes, r._hashes, sizeof(_hashes)) < 0;
    }
    // Initial state: set all tiles to empty
    Board() {
        memset(&_data, empty, sizeof(_data));
        memset(&_hashes, 0, sizeof(_hashes));
    }
};

// This function takes a list of blocks, and 'renders' them
// into a Board - for quick tile access. It also stores
// the block hashes into the Board.
Board renderBlocks(list<Block>& blocks)
{
    unsigned idx=0;
    Board tmp;
    for(auto& p: blocks) {
        if (p._isHorizontal)
            for(int i=0; i<p._length; i++)
                tmp(p._y, p._x+i) = p._kind;
        else
            for(int i=0; i<p._length; i++)
                tmp(p._y+i, p._x) = p._kind;
        assert(idx < SIZE*SIZE/2);
        tmp._hashes[idx++] = p.hash();
    }
    return tmp;
}

// When we find the solution, we also need to backtrack
// to display the moves we used to get there...
//
// "Move" stores what block moved and to what direction
struct Move {
    int _blockId;
    int _distance;
    enum Direction {left, right, up, down} _move;
    Move(int blockID, Direction d, int steps):
        _blockId(blockID),
        _distance(steps),
        _move(d) {}
};

// Utility function - we need to be able to "deep copy"
// a list of Blocks, to form alternate board states (see SolveBoard)
list<Block> copyBlocks(const list<Block>& blocks)
{
    list<Block> copied;
    for(auto& block : blocks) copied.push_back(block);
    return copied;
}

// The brains of the operation - basically a Breadth-First-Search
// of the problem space:
//    http://en.wikipedia.org/wiki/Breadth-first_search
//
void SolveBoard(list<Block>& blocks)
{
    cout << "\nSearching for a solution...\n";

    // We need to store the last move that got us to a specific
    // board state - that way we can backtrack from a final board
    // state to the list of moves we used to achieve it.
    map< Board, Move> previousMoves;
    // Start by storing a "sentinel" value, for the initial board
    // state - we used no Move to achieve it, so store a block id
    // of -1 to mark it:
    previousMoves.insert(
        pair<Board,Move>(renderBlocks(blocks), Move(-1, Move::left, 1)));

    // We must not revisit board states we have already examined,
    // so we need a 'visited' set:
    set<Board> visited;

    // Now, to implement Breadth First Search, all we need is a Queue
    // storing the states we need to investigate - so it needs to
    // be a list of board states... We'll also be maintaining
    // the depth we traversed to reach this board state, so we use
    // a pair of int (depth) and list of blocks (state).
    typedef pair<int, list<Block> > DepthAndStateTuple;
    list<DepthAndStateTuple> queue;

    // Start with our initial board state, and playedMoveDepth set to 1
    queue.push_back(DepthAndStateTuple(1, blocks));
    int oldLevel = 0;
    cout << "Depth searched:   " << oldLevel;

    while(!queue.empty()) {

        // Extract first element of the queue
        auto qtop = *queue.begin();
        auto level = qtop.first;
        auto blocks = qtop.second;
        queue.pop_front();

        // Report depth increase when it happens
        if (level > oldLevel) {
            cout << "\b\b\b"; cout.width(3); cout << level; cout.flush();
            oldLevel = level;
        }

        // Create a Board for fast 2D access to tile state
        Board board = renderBlocks(blocks);

        // Have we seen this board before?
        if (visited.find(board) != visited.end())
            // Yep - skip it
            continue;

        // No, we haven't - store it so we avoid re-doing
        // the following work again in the future...
        visited.insert(board);

        // Check if this board state is a winning state:
        // Find prisoner block...
        auto it=find_if(blocks.begin(), blocks.end(),
            [](Block& x) { return x._kind == prisoner; });
        assert(it != blocks.end()); // The prisoner is always there!

        // Can he escape? Check to his right!
        bool allClear = true;
        for (int x=it->_x+it->_length; x<SIZE ; x++) {
            allClear = allClear && !board(it->_y, x);
            if (!allClear)
                break;
        }
        if (allClear) {
            // Yes, he can escape - we did it!
            cout << "\n\nSolved!\n";

            // To print the Moves we used in normal order, we will
            // backtrack through the board states to print
            // the Move we used at each one...
            list<list<Block> > solution;
            solution.push_front(copyBlocks(blocks));

            auto itMove = previousMoves.find(board);
            while (itMove != previousMoves.end()) {
                if (itMove->second._blockId == -1)
                    // Sentinel - reached starting board
                    break;
                // Find the block we moved, and move it
                // (in reverse direction - we are going back)
                auto it = find_if(blocks.begin(), blocks.end(),
                    [itMove](Block& block) {
                        return block._id == itMove->second._blockId;
                    });
                assert(it != blocks.end());

                switch(itMove->second._move) {
                case Move::left:
                    it->_x+=itMove->second._distance; break;
                case Move::right:
                    it->_x-=itMove->second._distance; break;
                case Move::up:
                    it->_y+=itMove->second._distance; break;
                case Move::down:
                    it->_y-=itMove->second._distance; break;
                }

                // Add this board to the front of the list...
                solution.push_front(copyBlocks(blocks));
                board = renderBlocks(blocks);
                itMove = previousMoves.find(board);
            }
            // Now that we have the full list, emit it in order
            for(auto& blocks: solution) {
                printBoard(blocks);
                cout << "Press ENTER for next move\n";
                cin.get();
            }
            cout << "Run free, prisoner, run! :-)\n";
            exit(0);
        }

        // Nope, the prisoner is still trapped.
        //
        // Add all potential states arrising from immediate
        // possible moves to the end of the queue.
        for(auto& block: blocks) {

#define COMMON_BODY(direction) \
    auto copiedBlocks = copyBlocks(blocks);                         \
    auto candidateBoard = renderBlocks(copiedBlocks);               \
    if (visited.find(candidateBoard) == visited.end()) {            \
        /* Add to the end of the queue for further study */         \
        queue.push_back(DepthAndStateTuple(level+1, copiedBlocks)); \
        /* Store board and move, so we can backtrack later */       \
        previousMoves.insert(                                       \
            pair<Board,Move>(                                       \
                candidateBoard,                                     \
                Move(block._id, Move::direction, distance)));       \
    }

            if (block._isHorizontal) {
                // Can the block move to the left?
                int blockStartingX = block._x;
                for(int distance=1; distance<SIZE; distance++) {
                    int testX = blockStartingX-distance;
                    if (testX>=0 && empty==board(block._y, testX)) {
                        block._x = testX;
                        COMMON_BODY(left)
                    } else
                        break;
                }
                // Can the block move to the right?
                for(int distance=1; distance<SIZE; distance++) {
                    int testX = blockStartingX+distance-1+block._length;
                    if (testX<SIZE && empty==board(block._y, testX)) {
                        block._x = blockStartingX+distance;
                        COMMON_BODY(right)
                    } else
                        break;
                }
                block._x = blockStartingX;
            } else {
                // Can the block move up?
                int blockStartingY = block._y;
                for(int distance=1; distance<SIZE; distance++) {
                    int testY = blockStartingY-distance;
                    if (testY>=0 && empty==board(testY, block._x)) {
                        block._y = testY;
                        COMMON_BODY(up)
                    } else
                        break;
                }
                // Can the block move down?
                for(int distance=1; distance<SIZE; distance++) {
                    int testY = blockStartingY+distance-1+block._length;
                    if (testY<SIZE && empty==board(testY, block._x)) {
                        block._y = blockStartingY+distance;
                        COMMON_BODY(down)
                    } else
                        break;
                }
                block._y = blockStartingY;
            }
        }
        // and go recheck the queue, from the top!
    }
}

void DetectTileBodies()
{
    // This function looks at the center pixel of each tile,
    // and guesses what TileKind it is.
    //
    // (Heuristics on the snapshots taken from my iPhone)
    //
    cout << "Detecting tile bodies...\n";
    for(int y=0; y<SIZE; y++) {
        for(int x=0; x<SIZE; x++) {
            unsigned line   = 145 + y*50;
            unsigned column =  34 + x*50;
            // The red channel, surprisingly, was not necessary
            //unsigned char r = g_image[line][column][0];
            unsigned char g = g_image[line][column][1];
            unsigned char b = g_image[line][column][2];
            if (b > 30)
                g_tiles[y][x] = empty;
            else if (g < 30)
                g_tiles[y][x] = prisoner;
            else
                g_tiles[y][x] = block;
        }
    }
}

void DetectTopAndBottomTileBorders()
{
    cout << "Detecting top and bottom tile borders...\n\n";
    for(int y=0; y<SIZE; y++) {
        for(int x=0; x<SIZE; x++) {
            unsigned line    = 145 + y*50;
            unsigned column  =  34 + x*50;
            unsigned ytop    = line - 23;
            unsigned ybottom = line + 23;

            unsigned char r = g_image[ytop][column][0];
            unsigned char g = g_image[ytop][column][1];
            //unsigned char b = g_image[ytop][column][2];
            if      (r > 200 && g > 160) g_borders[y*2][x] = white;
            else if (r < 40 && g < 30)   g_borders[y*2][x] = black;
            else                         g_borders[y*2][x] = notBorder;

            r = g_image[ybottom][column][0];
            g = g_image[ybottom][column][1];
            //b = g_image[ybottom][column][2];
            if      (r > 200 && g > 160) g_borders[y*2+1][x] = white;
            else if (r < 40 && g < 30)   g_borders[y*2+1][x] = black;
            else                         g_borders[y*2+1][x] = notBorder;
        }
    }
}

int main()
{
    ifstream rgbDataFileStream;
    rgbDataFileStream.open("data.rgb", ios::in | ios::binary);
    if (!rgbDataFileStream.is_open()) {
        cerr << "Convert your iPhone snapshot to 'data.rgb' ";
        cerr << "with ImageMagick:\n\n";
        cerr << "\tbash$ convert IMG_0354.PNG data.rgb\n\n";
        exit(1);
    }
    rgbDataFileStream.read(
        reinterpret_cast<char*>(&g_image[0][0][0]),
        480*320*3);
    if (rgbDataFileStream.fail() || rgbDataFileStream.eof()) {
        cerr << "Failed to read 480x320x3 bytes from 'data.rgb'...\n\n";
        exit(1);
    }
    rgbDataFileStream.close();
    DetectTileBodies();
    DetectTopAndBottomTileBorders();
    list<Block> blocks =
        ScanBodiesAndBordersAndEmitStartingBlockPositions();
    SolveBoard(blocks);
}
