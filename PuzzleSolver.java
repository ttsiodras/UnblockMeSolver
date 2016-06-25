package com.benderapps.engy.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import com.benderapps.engy.actors.Block;
import com.benderapps.engy.world.World;

/*
 * Created by Carles Mallafre on 25/06/2016, Java version of UnblockMeSolver library: https://github.com/ttsiodras/UnblockMeSolver
 * 
 */

public class PuzzleSolver {
	static int blockID = 0;
	private static final int SIZE = 6;
	private World world;

	private enum TileKind {
		empty, block, prisoner
	}

	public enum Direction {
		left, right, up, down;
	}

	public class BlockSolver {
		// static int blockID;
		public int id;
		public int x, y;
		public boolean isHoritzontal;
		public TileKind kind;
		public int length;
		public Move nextSolutionMovement;

		int hash() {
			return id | y << 8 | x << 16;
		}

		public BlockSolver(int y, int x, boolean isHoritzontal, TileKind kind,
				int length, Move move) {
			this.id = blockID++;
			this.y = y;
			this.x = x;
			this.isHoritzontal = isHoritzontal;
			this.kind = kind;
			this.length = length;
			this.nextSolutionMovement = move;
		}
		
		public BlockSolver() {
		}

		public BlockSolver clone() {
			BlockSolver bs = new BlockSolver();
			bs.id = this.id;
			bs.x = this.x;
			bs.y = this.y;
			bs.isHoritzontal = this.isHoritzontal;
			bs.kind = this.kind;
			bs.length = this.length;
			bs.nextSolutionMovement = this.nextSolutionMovement;
			return bs;
		}

	}

	private class Board {
		TileKind data[] = new TileKind[SIZE * SIZE];
		int hashes[] = new int[SIZE * SIZE / 2];

		public void setData(int y, int x, TileKind kind) {
			data[y * SIZE + x] = kind;
		}

		public boolean isPositionEmpty(int y, int x) {
			return data[y * SIZE + x] == null;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			Board other = (Board) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (!Arrays.equals(data, other.data))
				return false;
			if (!Arrays.equals(hashes, other.hashes))
				return false;
			return true;
		}
		
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + Arrays.hashCode(data);
			result = prime * result + Arrays.hashCode(hashes);
			return result;
		}

		private PuzzleSolver getOuterType() {
			return PuzzleSolver.this;
		}
	}

	private class BoardAndLevel {
		@Override
		public int hashCode() {
			final int prime = 31;
			int result = 1;
			result = prime * result + getOuterType().hashCode();
			result = prime * result + ((board == null) ? 0 : board.hashCode());
			result = prime * result + level;
			return result;
		}

		@Override
		public boolean equals(Object obj) {
			if (this == obj)
				return true;
			if (obj == null)
				return false;
			if (getClass() != obj.getClass())
				return false;
			BoardAndLevel other = (BoardAndLevel) obj;
			if (!getOuterType().equals(other.getOuterType()))
				return false;
			if (board == null) {
				if (other.board != null)
					return false;
			} else if (!board.equals(other.board))
				return false;
			if (level != other.level)
				return false;
			return true;
		}

		private Board board;
		private int level;

		public BoardAndLevel(Board b, int lvl) {
			this.board = b;
			this.level = lvl;
		}

		private PuzzleSolver getOuterType() {
			return PuzzleSolver.this;
		}
		
	}

	private class DepthAndMoveAndState {
		private int depth;
		private Move move;
		private ArrayList<BlockSolver> blocks;

		public DepthAndMoveAndState(int d, Move m, ArrayList<BlockSolver> l) {
			this.depth = d;
			this.move = m;
			this.blocks = l;
		}
	}

	public class Move {
		int blockId;
		public int distance;
		public Direction move;

		public Move(int blockID, Direction d, int steps) {
			this.blockId = blockID;
			this.distance = steps;
			this.move = d;
		}
	}

	public PuzzleSolver(World world) {
		this.world = world;
	}

	public ArrayList<ArrayList<BlockSolver>> solvePuzzle() {
		// returned array with movements to solve the puzzle
		ArrayList<ArrayList<BlockSolver>> solution = new ArrayList<ArrayList<BlockSolver>>();

		ArrayList<BlockSolver> blocks_world = getBlocksFromWorld();

		// We need to store the last move that got us to a specific
		// board state - that way we can backtrack from a final board
		// state to the list of moves we used to achieve it.
		BoardAndLevel boardAndLevel;
		HashMap<BoardAndLevel, Move> previousMoves = new HashMap<BoardAndLevel, Move>();

		// Start by storing a "sentinel" value, for the initial board
		// state - we used no Move to achieve it, so store a block id
		// of -1 to mark it:
		int oldLevel = 0;
		boardAndLevel = new BoardAndLevel(renderBlocks(blocks_world), oldLevel);
		previousMoves.put(boardAndLevel, new Move(-1, Direction.left, 1));

		// We must not revisit board states we have already examined,
		// so we need a 'visited' set:
		Set<Board> visited = new HashSet<Board>();

		// Now, to implement Breadth First Search, all we need is a Queue
		// storing the states we need to investigate - so it needs to
		// be a list of board states... We'll also be maintaining
		// the depth we traversed to reach this board state, and the
		// move to perform - so we end up with a tuple of
		// int (depth), Move, list of blocks (state).
		LinkedList<DepthAndMoveAndState> queue = new LinkedList<DepthAndMoveAndState>();

		// Start with our initial board state, and playedMoveDepth set to 1
		queue.add(new DepthAndMoveAndState(1, new Move(-1, Direction.left, 0),
				blocks_world));

		while (!queue.isEmpty()) {
			// Extract first element of the queue
			DepthAndMoveAndState qtop = queue.pop();
			int level = qtop.depth;
			Move move = qtop.move;
			ArrayList<BlockSolver> blocks = qtop.blocks;

			// Report depth increase when it happens
			if (level > oldLevel) {
				oldLevel = level;
			}

			// Create a Board for fast 2D access to tile state
			Board board = renderBlocks(blocks);

			// Have we seen this board before?
			if (visited.contains(board)) {
				// Yep - skip it
				continue;
			}

			// No, we haven't - store it so we avoid re-doing
			// the following work again in the future...
			visited.add(board);

			/* Store board and move, so we can backtrack later */
			previousMoves.put(new BoardAndLevel(board, oldLevel), move);

			// Check if this board state is a winning state:
			// Find prisoner block...
			BlockSolver it = null;
			for (BlockSolver block : blocks) {
				if (block.kind == TileKind.prisoner) {
					it = block;
					break;
				}
			}

			// Can he escape? Check to his right!
			boolean allClear = true;
			for (int x = it.x + it.length; x < SIZE; x++) {
				allClear = allClear && board.isPositionEmpty(it.y, x);
				if (!allClear)
					break;
			}
			if (allClear) {
				// To print the Moves we used in normal order, we will
				// backtrack through the board states to print
				// the Move we used at each one...
				solution.add(0, copyBlocks(blocks));

				Move itMove = previousMoves
						.get(new BoardAndLevel(board, level));
				while (itMove != null) {
					if (itMove.blockId == -1) {
						// Sentinel - reached starting board
						break;
					}
					// Find the block we moved, and move it
					// (in reverse direction - we are going back)
					BlockSolver it2 = null;
					for (BlockSolver block : blocks) {
						if (block.id == itMove.blockId) {
							it2 = block;
							break;
						}
					}

					if (it2 == null)
						break;

					//store the kind of movement
					it2.nextSolutionMovement = itMove;
					
					switch (itMove.move) {
					case left:
						it2.x += itMove.distance;
						break;
					case right:
						it2.x -= itMove.distance;
						break;
					case up:
						it2.y += itMove.distance;
						break;
					case down:
						it2.y -= itMove.distance;
						break;
					}

					// Add this board to the front of the list...
					solution.add(0, copyBlocks(blocks));
					board = renderBlocks(blocks);
					level--;
					itMove = previousMoves.get(new BoardAndLevel(board, level));
				}
				return solution;
			}

			// Nope, the prisoner is still trapped.
			// Add all potential states arrising from immediate
			// possible moves to the end of the queue.
			for (BlockSolver block : blocks) {
				if (block.isHoritzontal) {
					// Can the block move to the left?
					int blockStartingX = block.x;
					for (int distance = 1; distance < SIZE; distance++) {
						int testX = blockStartingX - distance;
						if (testX >= 0 && board.isPositionEmpty(block.y, testX)) {
							block.x = testX;
							common_body(Direction.left, blocks, visited, queue,
									level, block, distance);
						} else {
							break;
						}
					}
					// Can the block move to the right?
					for (int distance = 1; distance < SIZE; distance++) {
						int testX = blockStartingX + distance - 1
								+ block.length;
						if (testX < SIZE
								&& board.isPositionEmpty(block.y, testX)) {
							block.x = blockStartingX + distance;
							common_body(Direction.right, blocks, visited,
									queue, level, block, distance);
						} else
							break;
					}
					block.x = blockStartingX;
				} else {
					// Can the block move up?
					int blockStartingY = block.y;
					for (int distance = 1; distance < SIZE; distance++) {
						int testY = blockStartingY - distance;
						if (testY >= 0 && board.isPositionEmpty(testY, block.x)) {
							block.y = testY;
							common_body(Direction.up, blocks, visited, queue,
									level, block, distance);
						} else
							break;
					}
					// Can the block move down?
					for (int distance = 1; distance < SIZE; distance++) {
						int testY = blockStartingY + distance - 1
								+ block.length;
						if (testY < SIZE
								&& board.isPositionEmpty(testY, block.x)) {
							block.y = blockStartingY + distance;
							common_body(Direction.down, blocks, visited, queue,
									level, block, distance);
						} else
							break;
					}
					block.y = blockStartingY;
				}
			}
			// and go recheck the queue, from the top!
		}
		return solution;
	}

	private ArrayList<BlockSolver> copyBlocks(ArrayList<BlockSolver> blocks) {
		ArrayList<BlockSolver> clonedList = new ArrayList<BlockSolver>(
				blocks.size());
		for (BlockSolver block : blocks) {
			clonedList.add(block.clone());
		}
		return clonedList;
	}

	private void common_body(Direction direction,
			ArrayList<BlockSolver> blocks, Set<Board> visited,
			LinkedList<DepthAndMoveAndState> queue, int level,
			BlockSolver block, int distance) {
		ArrayList<BlockSolver> copiedBlocks = copyBlocks(blocks);
		Board candidateBoard = renderBlocks(copiedBlocks);
		if (!visited.contains(candidateBoard)) {
			/* Add to the end of the queue for further study */
			queue.addLast(new DepthAndMoveAndState(level + 1, new Move(
					block.id, direction, distance), copiedBlocks));
		}
	}

	private ArrayList<BlockSolver> getBlocksFromWorld() {
		ArrayList<BlockSolver> blocks = new ArrayList<BlockSolver>();
		for (int i = 0; i < world.blocks.getChildren().size; i++) {
			Block block = (Block) world.blocks.getChildren().get(i);
			BlockSolver blockSolver = new BlockSolver(
					block.getFieldPosition().get(0)[1],
					block.getFieldPosition().get(0)[0],
					(block.getType() != Block.BIG_VERTICAL && block.getType() != Block.SMALL_VERTICAL),
					(block.getType() != Block.MASTER ? TileKind.block
							: TileKind.prisoner),
					(block.getType() == Block.BIG_HORITZONTAL || block
							.getType() == Block.BIG_VERTICAL) ? 3 : 2, null);
			blocks.add(blockSolver);
		}
		return blocks;
	}

	private Board renderBlocks(ArrayList<BlockSolver> blocks) {
		int idx = 0;
		Board tmp = new Board();
		for (BlockSolver p : blocks) {
			if (p.isHoritzontal) {
				for (int i = 0; i < p.length; i++) {
					tmp.setData(p.y, p.x + i, p.kind);
				}
			} else {
				for (int i = 0; i < p.length; i++) {
					tmp.setData(p.y + i, p.x, p.kind);
				}
			}
			assert idx < SIZE * SIZE / 2;
			tmp.hashes[idx++] = p.hash();
		}
		return tmp;
	}
}
